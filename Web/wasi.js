/*
  Deskew WebUI
  Minimal WASI (wasi_snapshot_preview1) host for running deskew.wasm in a browser
  or in Node. Files live in memory, one preopened directory "/" holds them all.
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
*/

const ERRNO = {
  SUCCESS: 0, BADF: 8, EXIST: 20, INVAL: 28, ISDIR: 31, NOENT: 44,
  NOSYS: 52, NOTDIR: 54, NOTEMPTY: 55, SPIPE: 70,
};

const FILETYPE = { CHARACTER_DEVICE: 2, DIRECTORY: 3, REGULAR_FILE: 4 };

const OFLAGS = { CREAT: 1, DIRECTORY: 2, EXCL: 4, TRUNC: 8 };
const FDFLAGS_APPEND = 1;
const WHENCE = { SET: 0, CUR: 1, END: 2 };

const ALL_RIGHTS = 0x1fffffffn;
const PREOPEN_FD = 3;

export class WasiExit extends Error {
  constructor(code) {
    super(`exit ${code}`);
    this.code = code;
  }
}

// One file: bytes up to size are the content, the rest is spare capacity
class MemFile {
  constructor(data) {
    this.data = data ? new Uint8Array(data) : new Uint8Array(0);
    this.size = this.data.length;
  }

  ensure(capacity) {
    if (capacity <= this.data.length) return;
    const grown = new Uint8Array(Math.max(capacity, this.data.length * 2, 4096));
    grown.set(this.data.subarray(0, this.size));
    this.data = grown;
  }

  contents() {
    return this.data.slice(0, this.size);
  }
}

function normalizePath(path) {
  const out = [];
  for (const part of path.split('/')) {
    if (part === '' || part === '.') continue;
    if (part === '..') out.pop();
    else out.push(part);
  }
  return '/' + out.join('/');
}

function parentOf(path) {
  const i = path.lastIndexOf('/');
  return i <= 0 ? '/' : path.substring(0, i);
}

/**
  Runs one WASI command module.
  files: { '/path': Uint8Array } put into the file system before start.
  Returns { exitCode, stdout, stderr, files } where files are all regular files after the run.
*/
export function runWasi(module, { args = [], env = {}, files = {}, onStdout, onStderr } = {}) {
  const fsFiles = new Map();
  const fsDirs = new Set(['/']);

  const addDirs = (path) => {
    let dir = parentOf(path);
    while (!fsDirs.has(dir)) {
      fsDirs.add(dir);
      dir = parentOf(dir);
    }
  };

  for (const [path, data] of Object.entries(files)) {
    const p = normalizePath(path);
    if (p.endsWith('/') || data === null) {
      fsDirs.add(p);
      addDirs(p);
    } else {
      fsFiles.set(p, new MemFile(data));
      addDirs(p);
    }
  }

  const decoder = new TextDecoder();
  const encoder = new TextEncoder();
  let stdout = '';
  let stderr = '';

  const fds = new Map();
  fds.set(0, { kind: 'stdin' });
  fds.set(1, { kind: 'stdout' });
  fds.set(2, { kind: 'stderr' });
  fds.set(PREOPEN_FD, { kind: 'dir', path: '/', preopen: true });
  let nextFd = PREOPEN_FD + 1;

  const argStrings = ['deskew', ...args].map((s) => encoder.encode(s + '\0'));
  const envStrings = Object.entries(env).map(([k, v]) => encoder.encode(`${k}=${v}\0`));

  let instance = null;
  const mem = () => new DataView(instance.exports.memory.buffer);
  const bytes = () => new Uint8Array(instance.exports.memory.buffer);
  const readStr = (ptr, len) => decoder.decode(bytes().slice(ptr, ptr + len));

  const resolve = (dirFd, pathPtr, pathLen) => {
    const dir = fds.get(dirFd);
    if (!dir || dir.kind !== 'dir') return null;
    const rel = readStr(pathPtr, pathLen);
    return normalizePath(rel.startsWith('/') ? rel : dir.path + '/' + rel);
  };

  const writeFilestat = (buf, type, size) => {
    const v = mem();
    for (let i = 0; i < 64; i += 8) v.setBigUint64(buf + i, 0n, true);
    v.setUint8(buf + 16, type);
    v.setBigUint64(buf + 24, 1n, true);
    v.setBigUint64(buf + 32, BigInt(size), true);
    const now = BigInt(Date.now()) * 1000000n;
    v.setBigUint64(buf + 40, now, true);
    v.setBigUint64(buf + 48, now, true);
    v.setBigUint64(buf + 56, now, true);
  };

  const iovecs = (iovs, count) => {
    const v = mem();
    const list = [];
    for (let i = 0; i < count; i++) {
      list.push([v.getUint32(iovs + i * 8, true), v.getUint32(iovs + i * 8 + 4, true)]);
    }
    return list;
  };

  const sizesGet = (strings) => (countPtr, sizePtr) => {
    const v = mem();
    v.setUint32(countPtr, strings.length, true);
    v.setUint32(sizePtr, strings.reduce((n, s) => n + s.length, 0), true);
    return ERRNO.SUCCESS;
  };

  const stringsGet = (strings) => (ptrsPtr, bufPtr) => {
    const v = mem();
    const b = bytes();
    for (const s of strings) {
      v.setUint32(ptrsPtr, bufPtr, true);
      b.set(s, bufPtr);
      ptrsPtr += 4;
      bufPtr += s.length;
    }
    return ERRNO.SUCCESS;
  };

  const doWrite = (fdObj, chunks, position) => {
    let total = 0;
    if (fdObj.kind === 'stdout' || fdObj.kind === 'stderr') {
      for (const [ptr, len] of chunks) {
        const text = decoder.decode(bytes().slice(ptr, ptr + len));
        if (fdObj.kind === 'stdout') {
          stdout += text;
          onStdout?.(text);
        } else {
          stderr += text;
          onStderr?.(text);
        }
        total += len;
      }
      return total;
    }
    const file = fdObj.file;
    let pos = position ?? (fdObj.append ? file.size : fdObj.pos);
    for (const [ptr, len] of chunks) {
      file.ensure(pos + len);
      file.data.set(bytes().subarray(ptr, ptr + len), pos);
      pos += len;
      total += len;
      if (pos > file.size) file.size = pos;
    }
    if (position === undefined) fdObj.pos = pos;
    return total;
  };

  const doRead = (fdObj, chunks, position) => {
    const file = fdObj.file;
    let pos = position ?? fdObj.pos;
    let total = 0;
    for (const [ptr, len] of chunks) {
      const n = Math.max(0, Math.min(len, file.size - pos));
      bytes().set(file.data.subarray(pos, pos + n), ptr);
      pos += n;
      total += n;
      if (n < len) break;
    }
    if (position === undefined) fdObj.pos = pos;
    return total;
  };

  const wasi = {
    args_sizes_get: sizesGet(argStrings),
    args_get: stringsGet(argStrings),
    environ_sizes_get: sizesGet(envStrings),
    environ_get: stringsGet(envStrings),

    clock_res_get(id, resPtr) {
      mem().setBigUint64(resPtr, 1000n, true);
      return ERRNO.SUCCESS;
    },

    clock_time_get(id, precision, timePtr) {
      // 0 = realtime, others are monotonic/cputime
      const ns = id === 0
        ? BigInt(Date.now()) * 1000000n
        : BigInt(Math.round(performance.now() * 1000000));
      mem().setBigUint64(timePtr, ns, true);
      return ERRNO.SUCCESS;
    },

    random_get(buf, len) {
      crypto.getRandomValues(bytes().subarray(buf, buf + len));
      return ERRNO.SUCCESS;
    },

    proc_exit(code) {
      throw new WasiExit(code);
    },

    sched_yield: () => ERRNO.SUCCESS,

    fd_prestat_get(fd, buf) {
      const f = fds.get(fd);
      if (!f || !f.preopen) return ERRNO.BADF;
      const v = mem();
      v.setUint8(buf, 0);
      v.setUint32(buf + 4, encoder.encode(f.path).length, true);
      return ERRNO.SUCCESS;
    },

    fd_prestat_dir_name(fd, ptr, len) {
      const f = fds.get(fd);
      if (!f || !f.preopen) return ERRNO.BADF;
      bytes().set(encoder.encode(f.path).subarray(0, len), ptr);
      return ERRNO.SUCCESS;
    },

    fd_fdstat_get(fd, buf) {
      const f = fds.get(fd);
      if (!f) return ERRNO.BADF;
      const v = mem();
      const type = f.kind === 'dir' ? FILETYPE.DIRECTORY
        : f.kind === 'file' ? FILETYPE.REGULAR_FILE : FILETYPE.CHARACTER_DEVICE;
      v.setUint8(buf, type);
      v.setUint16(buf + 2, f.append ? FDFLAGS_APPEND : 0, true);
      v.setBigUint64(buf + 8, ALL_RIGHTS, true);
      v.setBigUint64(buf + 16, ALL_RIGHTS, true);
      return ERRNO.SUCCESS;
    },

    fd_fdstat_set_flags: () => ERRNO.SUCCESS,
    fd_advise: () => ERRNO.SUCCESS,
    fd_sync: () => ERRNO.SUCCESS,
    fd_datasync: () => ERRNO.SUCCESS,

    fd_filestat_get(fd, buf) {
      const f = fds.get(fd);
      if (!f) return ERRNO.BADF;
      if (f.kind === 'dir') writeFilestat(buf, FILETYPE.DIRECTORY, 0);
      else if (f.kind === 'file') writeFilestat(buf, FILETYPE.REGULAR_FILE, f.file.size);
      else writeFilestat(buf, FILETYPE.CHARACTER_DEVICE, 0);
      return ERRNO.SUCCESS;
    },

    fd_filestat_set_size(fd, size) {
      const f = fds.get(fd);
      if (!f || f.kind !== 'file') return ERRNO.BADF;
      const n = Number(size);
      f.file.ensure(n);
      if (n > f.file.size) f.file.data.fill(0, f.file.size, n);
      f.file.size = n;
      return ERRNO.SUCCESS;
    },

    fd_filestat_set_times: () => ERRNO.SUCCESS,
    fd_allocate: () => ERRNO.SUCCESS,

    fd_close(fd) {
      if (!fds.has(fd)) return ERRNO.BADF;
      if (fd > PREOPEN_FD) fds.delete(fd);
      return ERRNO.SUCCESS;
    },

    fd_write(fd, iovs, iovsLen, nPtr) {
      const f = fds.get(fd);
      if (!f || f.kind === 'dir' || f.kind === 'stdin') return ERRNO.BADF;
      mem().setUint32(nPtr, doWrite(f, iovecs(iovs, iovsLen)), true);
      return ERRNO.SUCCESS;
    },

    fd_pwrite(fd, iovs, iovsLen, offset, nPtr) {
      const f = fds.get(fd);
      if (!f || f.kind !== 'file') return ERRNO.BADF;
      mem().setUint32(nPtr, doWrite(f, iovecs(iovs, iovsLen), Number(offset)), true);
      return ERRNO.SUCCESS;
    },

    fd_read(fd, iovs, iovsLen, nPtr) {
      const f = fds.get(fd);
      if (!f) return ERRNO.BADF;
      if (f.kind === 'stdin') {
        mem().setUint32(nPtr, 0, true);
        return ERRNO.SUCCESS;
      }
      if (f.kind !== 'file') return ERRNO.BADF;
      mem().setUint32(nPtr, doRead(f, iovecs(iovs, iovsLen)), true);
      return ERRNO.SUCCESS;
    },

    fd_pread(fd, iovs, iovsLen, offset, nPtr) {
      const f = fds.get(fd);
      if (!f || f.kind !== 'file') return ERRNO.BADF;
      mem().setUint32(nPtr, doRead(f, iovecs(iovs, iovsLen), Number(offset)), true);
      return ERRNO.SUCCESS;
    },

    fd_seek(fd, offset, whence, newOffsetPtr) {
      const f = fds.get(fd);
      if (!f) return ERRNO.BADF;
      if (f.kind !== 'file') return ERRNO.SPIPE;
      const base = whence === WHENCE.SET ? 0 : whence === WHENCE.CUR ? f.pos : f.file.size;
      const pos = base + Number(offset);
      if (pos < 0) return ERRNO.INVAL;
      f.pos = pos;
      mem().setBigUint64(newOffsetPtr, BigInt(pos), true);
      return ERRNO.SUCCESS;
    },

    fd_tell(fd, offsetPtr) {
      const f = fds.get(fd);
      if (!f || f.kind !== 'file') return ERRNO.BADF;
      mem().setBigUint64(offsetPtr, BigInt(f.pos), true);
      return ERRNO.SUCCESS;
    },

    fd_readdir(fd, buf, bufLen, cookie, usedPtr) {
      const f = fds.get(fd);
      if (!f || f.kind !== 'dir') return ERRNO.BADF;
      const prefix = f.path === '/' ? '/' : f.path + '/';
      const names = [];
      for (const d of fsDirs) {
        if (d !== f.path && d.startsWith(prefix) && !d.substring(prefix.length).includes('/')) {
          names.push([d.substring(prefix.length), FILETYPE.DIRECTORY]);
        }
      }
      for (const p of fsFiles.keys()) {
        if (p.startsWith(prefix) && !p.substring(prefix.length).includes('/')) {
          names.push([p.substring(prefix.length), FILETYPE.REGULAR_FILE]);
        }
      }
      // dirent: d_next u64, d_ino u64, d_namlen u32, d_type u8, then name
      const out = [];
      names.forEach(([name, type], i) => {
        if (BigInt(i) < cookie) return;
        const nameBytes = encoder.encode(name);
        const entry = new Uint8Array(24 + nameBytes.length);
        const ev = new DataView(entry.buffer);
        ev.setBigUint64(0, BigInt(i + 1), true);
        ev.setBigUint64(8, BigInt(i + 1), true);
        ev.setUint32(16, nameBytes.length, true);
        ev.setUint8(20, type);
        entry.set(nameBytes, 24);
        out.push(entry);
      });
      let used = 0;
      const b = bytes();
      for (const entry of out) {
        const n = Math.min(entry.length, bufLen - used);
        b.set(entry.subarray(0, n), buf + used);
        used += n;
        if (used >= bufLen) break;
      }
      mem().setUint32(usedPtr, used, true);
      return ERRNO.SUCCESS;
    },

    path_open(dirFd, dirFlags, pathPtr, pathLen, oflags, rightsBase, rightsInh, fdFlags, fdPtr) {
      const path = resolve(dirFd, pathPtr, pathLen);
      if (path === null) return ERRNO.BADF;

      if (fsDirs.has(path)) {
        if (oflags & (OFLAGS.CREAT | OFLAGS.EXCL)) return ERRNO.EXIST;
        const fd = nextFd++;
        fds.set(fd, { kind: 'dir', path });
        mem().setUint32(fdPtr, fd, true);
        return ERRNO.SUCCESS;
      }
      if (oflags & OFLAGS.DIRECTORY) return fsFiles.has(path) ? ERRNO.NOTDIR : ERRNO.NOENT;

      let file = fsFiles.get(path);
      if (file) {
        if ((oflags & OFLAGS.CREAT) && (oflags & OFLAGS.EXCL)) return ERRNO.EXIST;
        if (oflags & OFLAGS.TRUNC) file.size = 0;
      } else {
        if (!(oflags & OFLAGS.CREAT)) return ERRNO.NOENT;
        if (!fsDirs.has(parentOf(path))) return ERRNO.NOENT;
        file = new MemFile();
        fsFiles.set(path, file);
      }

      const fd = nextFd++;
      fds.set(fd, { kind: 'file', path, file, pos: 0, append: (fdFlags & FDFLAGS_APPEND) !== 0 });
      mem().setUint32(fdPtr, fd, true);
      return ERRNO.SUCCESS;
    },

    path_filestat_get(dirFd, flags, pathPtr, pathLen, buf) {
      const path = resolve(dirFd, pathPtr, pathLen);
      if (path === null) return ERRNO.BADF;
      if (fsDirs.has(path)) writeFilestat(buf, FILETYPE.DIRECTORY, 0);
      else if (fsFiles.has(path)) writeFilestat(buf, FILETYPE.REGULAR_FILE, fsFiles.get(path).size);
      else return ERRNO.NOENT;
      return ERRNO.SUCCESS;
    },

    path_filestat_set_times: () => ERRNO.SUCCESS,

    path_create_directory(dirFd, pathPtr, pathLen) {
      const path = resolve(dirFd, pathPtr, pathLen);
      if (path === null) return ERRNO.BADF;
      if (fsDirs.has(path) || fsFiles.has(path)) return ERRNO.EXIST;
      if (!fsDirs.has(parentOf(path))) return ERRNO.NOENT;
      fsDirs.add(path);
      return ERRNO.SUCCESS;
    },

    path_remove_directory(dirFd, pathPtr, pathLen) {
      const path = resolve(dirFd, pathPtr, pathLen);
      if (path === null) return ERRNO.BADF;
      if (!fsDirs.has(path)) return fsFiles.has(path) ? ERRNO.NOTDIR : ERRNO.NOENT;
      const prefix = path + '/';
      for (const p of [...fsFiles.keys(), ...fsDirs]) {
        if (p.startsWith(prefix)) return ERRNO.NOTEMPTY;
      }
      fsDirs.delete(path);
      return ERRNO.SUCCESS;
    },

    path_unlink_file(dirFd, pathPtr, pathLen) {
      const path = resolve(dirFd, pathPtr, pathLen);
      if (path === null) return ERRNO.BADF;
      if (fsDirs.has(path)) return ERRNO.ISDIR;
      if (!fsFiles.delete(path)) return ERRNO.NOENT;
      return ERRNO.SUCCESS;
    },

    path_rename(oldFd, oldPtr, oldLen, newFd, newPtr, newLen) {
      const from = resolve(oldFd, oldPtr, oldLen);
      const to = resolve(newFd, newPtr, newLen);
      if (from === null || to === null) return ERRNO.BADF;
      const file = fsFiles.get(from);
      if (!file) return ERRNO.NOENT;
      fsFiles.delete(from);
      fsFiles.set(to, file);
      return ERRNO.SUCCESS;
    },
  };

  // Anything the RTL imports but is not needed for Deskew answers "not supported"
  const wasiImports = new Proxy(wasi, {
    get: (target, name) => target[name] ?? (() => ERRNO.NOSYS),
  });

  const imports = {};
  for (const imp of WebAssembly.Module.imports(module)) {
    if (imp.kind !== 'function') continue;
    imports[imp.module] ??= {};
    imports[imp.module][imp.name] = imp.module === 'wasi_snapshot_preview1'
      ? wasiImports[imp.name]
      : () => { throw new Error(`Unexpected import ${imp.module}.${imp.name}`); };
  }

  instance = new WebAssembly.Instance(module, imports);

  let exitCode = 0;
  try {
    instance.exports._start();
  } catch (e) {
    if (e instanceof WasiExit) exitCode = e.code;
    else throw e;
  }

  const outFiles = {};
  for (const [path, file] of fsFiles) outFiles[path] = file.contents();

  return { exitCode, stdout, stderr, files: outFiles };
}
