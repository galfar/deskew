/*
  Deskew Web GUI
  Web Worker running deskew.wasm, so the page stays responsive during detection
  and rotation. Every request is a new instance of the CLI with its own memory.
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
*/

import { runWasi } from './wasi.js';

let modulePromise = null;

function getModule() {
  modulePromise ??= (async () => {
    const url = new URL('deskew.wasm', import.meta.url);
    const response = await fetch(url);
    if (!response.ok) throw new Error(`Could not load ${url.pathname} (HTTP ${response.status})`);
    return WebAssembly.compileStreaming
      ? WebAssembly.compileStreaming(response).catch(async () =>
          WebAssembly.compile(await (await fetch(url)).arrayBuffer()))
      : WebAssembly.compile(await response.arrayBuffer());
  })();
  return modulePromise;
}

self.onmessage = async (event) => {
  const { id, args, files } = event.data;
  try {
    const module = await getModule();
    const started = performance.now();
    const result = runWasi(module, { args, files });
    result.timeMs = performance.now() - started;

    // Input files are not interesting for the page, send back only the new ones
    for (const path of Object.keys(files)) delete result.files[path];
    const transfer = Object.values(result.files).map((data) => data.buffer);
    self.postMessage({ id, result }, transfer);
  } catch (e) {
    self.postMessage({ id, error: String(e?.stack || e) });
  }
};

// Start compiling right away, the first run is then faster
getModule().then(
  () => self.postMessage({ ready: true }),
  (e) => self.postMessage({ ready: false, error: String(e) }),
);
