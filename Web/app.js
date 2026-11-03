/*
  Deskew Web GUI
  Page logic: builds Deskew command line arguments from the toggles, runs the
  CLI in a worker, shows the image with detection overlay and a rotated preview.
  The real rotation and encoding runs only when the output is downloaded.
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
*/

const $ = (id) => document.getElementById(id);

// Longest side of the canvas: previews of 600 dpi scans do not need all their pixels
const MAX_VIEW_SIZE = 2400;
// Default of the CLI (-l): smaller angles are not worth rotating
const SKIP_ANGLE = 0.01;
// Option controls that change detection, others change only the output
const DETECTION_INPUTS = ['maxAngle', 'thresholdAuto', 'thresholdLevel', 'contentArea',
  'margins', 'marginsUnit', 'angleStep', 'extraParams'];
const OUTPUT_INPUTS = ['background', 'filter', 'crop', 'force', 'format', 'jpegQuality'];

const state = {
  file: null,        // { name, bytes }
  bitmap: null,      // decoded input for the view
  report: null,      // JSON report of the last detection
  workBitmap: null,  // work image of the last detection
  view: 'original',
  detectTimeMs: 0,
  detectSeq: 0,
};

// ---- Worker ----

const worker = new Worker(new URL('worker.js', import.meta.url), { type: 'module' });
const pending = new Map();
let nextRequestId = 1;

worker.onmessage = (event) => {
  const msg = event.data;
  if ('ready' in msg) {
    $('engineStatus').textContent = msg.ready ? 'Engine ready' : `Engine failed to load: ${msg.error}`;
    $('engineStatus').classList.toggle('error', !msg.ready);
    return;
  }
  const request = pending.get(msg.id);
  pending.delete(msg.id);
  if (msg.error) request.reject(new Error(msg.error));
  else request.resolve(msg.result);
};

function runDeskew(args, files) {
  const id = nextRequestId++;
  return new Promise((resolve, reject) => {
    pending.set(id, { resolve, reject });
    worker.postMessage({ id, args, files });
  });
}

// ---- Arguments ----

function splitParams(text) {
  // Simple shell-like split with "quoted parts"
  const out = [];
  const re = /"([^"]*)"|(\S+)/g;
  let m;
  while ((m = re.exec(text)) !== null) out.push(m[1] ?? m[2]);
  return out;
}

function detectionArgs() {
  const args = ['-a', $('maxAngle').value, '-d', $('angleStep').value];
  args.push('-t', $('thresholdAuto').checked ? 'a' : $('thresholdLevel').value);
  const area = $('contentArea').value;
  if (area === 'auto') args.push('-r', 'auto');
  if (area === 'margins') args.push('-m', `${$('margins').value.replace(/\s+/g, '')},${$('marginsUnit').value}`);
  return args.concat(splitParams($('extraParams').value));
}

function outputArgs() {
  const args = ['-b', $('background').value.substring(1).toUpperCase(), '-q', $('filter').value];
  const flags = ($('crop').checked ? 'c' : '') + ($('force').checked ? 'f' : '');
  if (flags) args.push('-g', flags);
  if (outputExt() === 'jpg') args.push('-c', `j${$('jpegQuality').value}`);
  return args;
}

function inputExt() {
  const m = /\.([^.]+)$/.exec(state.file.name);
  return m ? m[1].toLowerCase() : '';
}

function outputExt() {
  const fmt = $('format').value;
  return fmt === 'same' ? inputExt() : fmt;
}

function baseName(name) {
  return name.replace(/\.[^.]+$/, '');
}

// Keeps names simple for the WASI file system, the original name is used for the download
function inputPath() {
  return `/in/input.${inputExt() || 'img'}`;
}

// ---- Runs ----

async function detect() {
  if (!state.file) return;
  const seq = ++state.detectSeq;
  setBusy(true);
  try {
    const args = ['-g', 'd', '-s', 'jw', '-o', '/out/', ...detectionArgs(), inputPath()];
    const result = await runDeskew(args, { [inputPath()]: state.file.bytes, '/out/': null });
    if (seq !== state.detectSeq) return;  // newer detection already started
    showLog(args, result);

    let report = null;
    try {
      report = JSON.parse(result.stdout);
    } catch {
      showError(`Unexpected output from Deskew (exit code ${result.exitCode}), see the output below.`);
    }
    state.report = report;
    if (report?.error) showError(report.error);

    const workPath = Object.keys(result.files).find((p) => p.endsWith('.work.png'));
    state.workBitmap = workPath ? await decode(result.files[workPath]) : null;

    // Formats the browser cannot show (TIFF, TGA, PNM) get a PNG copy from Deskew itself
    if (!state.bitmap && report && !report.error) state.bitmap = await deskewDecode();

    state.detectTimeMs = result.timeMs;
    if (report && !report.error) showReport(report, result.timeMs);
    updateTabs();
    draw();
  } catch (e) {
    if (seq === state.detectSeq) showError(e.message);
  } finally {
    if (seq === state.detectSeq) setBusy(false);
  }
}

async function deskewDecode() {
  // Skip angle at its maximum: Deskew only converts the input to PNG
  const args = ['-l', '90', '-o', '/out/view.png', inputPath()];
  const result = await runDeskew(args, { [inputPath()]: state.file.bytes, '/out/': null });
  const png = result.files['/out/view.png'];
  return png ? decode(png) : null;
}

async function download() {
  if (!state.file) return;
  setBusy(true);
  $('downloadBtn').disabled = true;
  try {
    const ext = outputExt();
    const outPath = `/out/output.${ext}`;
    const args = ['-s', 'j', '-o', outPath, ...outputArgs(), ...detectionArgs(), inputPath()];
    const result = await runDeskew(args, { [inputPath()]: state.file.bytes, '/out/': null });
    showLog(args, result);

    const data = result.files[outPath];
    if (!data) {
      let message = `Deskew did not write the output (exit code ${result.exitCode}).`;
      try {
        message = JSON.parse(result.stdout).error ?? message;
      } catch { /* keep the generic message */ }
      showError(message);
      return;
    }
    const blob = new Blob([data], { type: mimeType(ext) });
    const a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = `deskewed-${baseName(state.file.name)}.${ext}`;
    document.body.appendChild(a);
    a.click();
    a.remove();
    setTimeout(() => URL.revokeObjectURL(a.href), 10000);
  } catch (e) {
    showError(e.message);
  } finally {
    setBusy(false);
    $('downloadBtn').disabled = !state.file;
  }
}

function mimeType(ext) {
  return {
    png: 'image/png', jpg: 'image/jpeg', jpeg: 'image/jpeg', tif: 'image/tiff',
    tiff: 'image/tiff', bmp: 'image/bmp',
  }[ext] ?? 'application/octet-stream';
}

async function decode(bytes) {
  try {
    return await createImageBitmap(new Blob([bytes]));
  } catch {
    return null;
  }
}

// ---- Drawing ----

function willRotate(report) {
  const det = report?.detection;
  if (!det) return false;
  if ($('force').checked) return Math.abs(det.skewAngle) > 0;
  return det.reliable && Math.abs(det.skewAngle) >= SKIP_ANGLE;
}

function draw() {
  const canvas = $('canvas');
  const ctx = canvas.getContext('2d');
  const view = state.view;

  if (view === 'work' && state.workBitmap) {
    drawFitted(canvas, ctx, state.workBitmap);
    return;
  }

  const bmp = state.bitmap;
  if (!bmp) {
    canvas.width = canvas.height = 0;
    return;
  }

  if (view === 'preview') {
    drawPreview(canvas, ctx, bmp);
    return;
  }

  const scale = drawFitted(canvas, ctx, bmp);
  const det = state.report?.detection;
  if (!det) return;

  // Input image coordinates: detection area in red, found content in green, as in the work image
  ctx.lineWidth = Math.max(2, canvas.width / 250);
  if (det.detectionRect) strokeRect(ctx, det.detectionRect, scale, 'rgba(220, 38, 38, 0.9)', [8, 6]);
  if (det.contentRect) strokeRect(ctx, det.contentRect, scale, 'rgba(22, 163, 74, 0.95)', []);
}

function drawFitted(canvas, ctx, bmp) {
  const scale = Math.min(1, MAX_VIEW_SIZE / Math.max(bmp.width, bmp.height));
  canvas.width = Math.round(bmp.width * scale);
  canvas.height = Math.round(bmp.height * scale);
  ctx.imageSmoothingQuality = 'high';
  ctx.drawImage(bmp, 0, 0, canvas.width, canvas.height);
  return scale;
}

function strokeRect(ctx, [l, t, r, b], scale, color, dash) {
  ctx.save();
  ctx.strokeStyle = color;
  ctx.setLineDash(dash);
  ctx.strokeRect(l * scale, t * scale, (r - l) * scale, (b - t) * scale);
  ctx.restore();
}

function drawPreview(canvas, ctx, bmp) {
  const det = state.report?.detection;
  const angle = det && willRotate(state.report) ? det.skewAngle : 0;
  // Positive skew means content turned clockwise, Deskew rotates counterclockwise to fix it
  const a = (-angle * Math.PI) / 180;
  const w = bmp.width;
  const h = bmp.height;
  let outW = w;
  let outH = h;
  if (!$('crop').checked) {
    outW = Math.abs(w * Math.cos(a)) + Math.abs(h * Math.sin(a));
    outH = Math.abs(w * Math.sin(a)) + Math.abs(h * Math.cos(a));
  }
  const scale = Math.min(1, MAX_VIEW_SIZE / Math.max(outW, outH));
  canvas.width = Math.round(outW * scale);
  canvas.height = Math.round(outH * scale);
  ctx.fillStyle = $('background').value;
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  ctx.imageSmoothingQuality = 'high';
  ctx.save();
  ctx.translate(canvas.width / 2, canvas.height / 2);
  ctx.rotate(a);
  ctx.scale(scale, scale);
  ctx.drawImage(bmp, -w / 2, -h / 2);
  ctx.restore();
}

// ---- UI state ----

function setBusy(busy) {
  $('busy').hidden = !busy;
}

function showError(message) {
  $('result').innerHTML = '';
  const span = document.createElement('span');
  span.className = 'bad';
  span.textContent = message;
  $('result').appendChild(span);
}

function showReport(report, timeMs) {
  const det = report.detection;
  const el = $('result');
  el.innerHTML = '';
  if (!det) {
    el.textContent = 'No detection result.';
    return;
  }
  const outcomeClass = det.reliable ? 'ok' : det.outcome === 'blank' ? 'muted' : 'warn';
  const parts = [
    ['angle', `${det.skewAngle.toFixed(2)}°`],
    [outcomeClass, ` ${det.outcome}`],
    ['muted', `, confidence ${(det.confidence * 100).toFixed(0)} %`],
    ['muted', ` · ${report.input.width} × ${report.input.height} ${report.input.format}`],
    ['muted', ` · ${Math.round(timeMs)} ms`],
  ];
  if (!willRotate(report)) parts.push(['muted', ' · output will not be rotated']);
  for (const [cls, text] of parts) {
    const span = document.createElement('span');
    span.className = cls;
    span.textContent = text;
    el.appendChild(span);
  }
}

function showLog(args, result) {
  const cmd = ['deskew', ...args.map((a) => (/\s/.test(a) ? `"${a}"` : a))].join(' ');
  $('log').textContent = `$ ${cmd}\n${result.stdout}${result.stderr}\n[exit code ${result.exitCode}]`;
}

function updateTabs() {
  for (const btn of $('tabs').querySelectorAll('button')) {
    btn.classList.toggle('active', btn.dataset.view === state.view);
    btn.disabled = btn.dataset.view === 'work' ? !state.workBitmap : !state.bitmap;
  }
}

function updateVisibility() {
  $('thresholdLevel').disabled = $('thresholdAuto').checked;
  $('marginsRow').hidden = $('contentArea').value !== 'margins';
  $('jpegRow').hidden = !state.file ? $('format').value !== 'jpg' : outputExt() !== 'jpg';
}

async function openFile(file) {
  const bytes = new Uint8Array(await file.arrayBuffer());
  state.file = { name: file.name, bytes };
  state.report = null;
  state.workBitmap = null;
  state.bitmap = await decode(bytes);
  $('fileName').textContent = `${file.name} (${formatSize(bytes.length)})`;
  $('downloadBtn').disabled = false;
  $('result').textContent = 'Detecting skew...';
  updateVisibility();
  updateTabs();
  draw();
  detect();
}

function formatSize(n) {
  return n < 1024 * 1024 ? `${Math.round(n / 1024)} kB` : `${(n / 1024 / 1024).toFixed(1)} MB`;
}

// ---- Wiring ----

let detectTimer = 0;
function scheduleDetect() {
  clearTimeout(detectTimer);
  detectTimer = setTimeout(detect, 300);
}

for (const id of DETECTION_INPUTS) {
  $(id).addEventListener('input', () => {
    updateVisibility();
    scheduleDetect();
  });
}
for (const id of OUTPUT_INPUTS) {
  $(id).addEventListener('input', () => {
    updateVisibility();
    if (state.report?.detection) showReport(state.report, state.detectTimeMs);
    draw();
  });
}

$('fileInput').addEventListener('change', (e) => {
  if (e.target.files[0]) openFile(e.target.files[0]);
});

const dropZone = $('dropZone');
for (const target of [dropZone, $('canvasWrap')]) {
  target.addEventListener('dragover', (e) => {
    e.preventDefault();
    dropZone.classList.add('over');
  });
  target.addEventListener('dragleave', () => dropZone.classList.remove('over'));
  target.addEventListener('drop', (e) => {
    e.preventDefault();
    dropZone.classList.remove('over');
    if (e.dataTransfer.files[0]) openFile(e.dataTransfer.files[0]);
  });
}

$('tabs').addEventListener('click', (e) => {
  const view = e.target.dataset?.view;
  if (!view || e.target.disabled) return;
  state.view = view;
  updateTabs();
  draw();
});

$('downloadBtn').addEventListener('click', download);

updateVisibility();
updateTabs();
