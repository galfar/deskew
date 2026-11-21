// Runs Web/deskew.wasm under the web GUI's in-memory WASI host in Node.
// Usage: node Tests/web/test-wasi.mjs [path/to/deskew.wasm]
import { readFileSync, readdirSync, statSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { runWasi } from '../../Web/wasi.js';

const root = join(dirname(fileURLToPath(import.meta.url)), '../..');
const wasmPath = process.argv[2] ?? join(root, 'Web/deskew.wasm');
const module = new WebAssembly.Module(readFileSync(wasmPath));

let failures = 0;
function check(name, cond, detail = '') {
  console.log(`${cond ? 'ok  ' : 'FAIL'} ${name}${detail ? ' - ' + detail : ''}`);
  if (!cond) failures++;
}

// Results the native builds give, checked by Tests/cli/check_cli.py too
const expected = JSON.parse(readFileSync(join(root, 'Tests/expected-detection.json'), 'utf8')).auto;

function run(args, files) {
  return runWasi(module, { args, files: { ...files, '/out/': null } });
}

// Detection on every test image, as the page does it
// Files only, local copies can have folders with more images in TestImages/
const testImages = readdirSync(join(root, 'TestImages')).filter((n) => statSync(join(root, 'TestImages', n)).isFile());
for (const name of testImages.sort()) {
  const ext = name.split('.').pop();
  const input = `/in/input.${ext}`;
  const t = performance.now();
  const r = run(['-g', 'd', '-s', 'jw', '-o', '/out/', '-r', 'auto', input],
    { [input]: readFileSync(join(root, 'TestImages', name)) });
  const ms = Math.round(performance.now() - t);
  let report = null;
  try { report = JSON.parse(r.stdout); } catch { /* reported below */ }
  const det = report?.detection;
  const work = Object.keys(r.files).find((p) => p.endsWith('.work.png'));
  check(`detect ${name}`, r.exitCode === 0 && det && work,
    det ? `${det.skewAngle} deg, ${det.outcome}, ${ms} ms` : `exit ${r.exitCode} ${r.stdout}${r.stderr}`.slice(0, 300));
  const exp = expected[name];
  if (det && exp) {
    const same = Math.abs(det.skewAngle - exp.skewAngle) <= 1e-6 && det.outcome === exp.outcome &&
      det.threshold === exp.threshold && JSON.stringify(det.contentRect) === JSON.stringify(exp.contentRect);
    check(`same as native ${name}`, same, same ? '' :
      `${det.skewAngle} ${det.outcome} ${det.threshold} ${JSON.stringify(det.contentRect)}, native: ` +
      `${exp.skewAngle} ${exp.outcome} ${exp.threshold} ${JSON.stringify(exp.contentRect)}`);
  } else if (det) {
    check(`same as native ${name}`, false, 'not in Tests/expected-detection.json');
  }
}

// Full run with output, PNG and JPEG
for (const [ext, extra] of [['png', []], ['jpg', ['-c', 'j85']], ['tif', []]]) {
  const r = run(['-s', 'j', '-b', 'FFFFFF', '-o', `/out/output.${ext}`, ...extra, '/in/input.png'],
    { '/in/input.png': readFileSync(join(root, 'TestImages/2.png')) });
  const out = r.files[`/out/output.${ext}`];
  check(`deskew to ${ext}`, r.exitCode === 0 && out?.length > 1000, out ? `${out.length} bytes` : r.stdout.slice(0, 300));
}

// Conversion only (used for the view of formats the browser cannot show)
{
  const r = run(['-l', '90', '-o', '/out/view.png', '/in/input.tif'],
    { '/in/input.tif': readFileSync(join(root, 'TestImages/1-lzw.tif')) });
  const out = r.files['/out/view.png'];
  check('convert TIFF to PNG', out?.length > 1000 && out[1] === 0x50, out ? `${out.length} bytes` : r.stdout.slice(0, 300));
}

// Corrupt input must be reported, not trap (a PNG header with garbage after it,
// a truncated PNG would load partially as in the native build)
{
  const bytes = new Uint8Array(5000).map((_, i) => (i * 7919) & 0xff);
  bytes.set([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]);
  let r;
  try {
    r = run(['-s', 'j', '-o', '/out/output.png', '/in/input.png'], { '/in/input.png': bytes });
  } catch (e) {
    check('corrupt PNG reported', false, `trapped: ${e}`);
  }
  if (r) {
    let report = null;
    try { report = JSON.parse(r.stdout); } catch { /* reported below */ }
    check('corrupt PNG reported', r.exitCode !== 0 && report?.error, `exit ${r.exitCode}, error: ${report?.error ?? r.stdout.slice(0, 200)}`);
  }
}

// Bad parameters
{
  const r = run(['-s', 'j', '-a', 'abc', '/in/input.png'], { '/in/input.png': new Uint8Array(10) });
  let report = null;
  try { report = JSON.parse(r.stdout); } catch { /* reported below */ }
  check('bad parameter reported', r.exitCode !== 0 && report?.error, report?.error ?? r.stdout.slice(0, 200));
}

console.log(failures ? `${failures} failed` : 'all passed');
process.exit(failures ? 1 : 0);
