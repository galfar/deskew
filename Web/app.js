/*
  Deskew WebUI
  Page logic: builds Deskew command line arguments from the toggles, runs the
  CLI in a worker, shows the image with detection overlay and a rotated preview.
  The real rotation and encoding runs only when the output is downloaded.
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
*/

import { WEBUI_BUILD } from './version.js';

const $ = (id) => document.getElementById(id);

// Default of the CLI (-l): smaller angles are not worth rotating
const SKIP_ANGLE = 0.01;
// Option controls that change detection, others change only the output
const DETECTION_INPUTS = ['maxAngle', 'thresholdAuto', 'thresholdLevel', 'contentArea',
  'margins', 'marginsUnit', 'angleStep', 'extraParams'];
const OUTPUT_INPUTS = ['background', 'filter', 'crop', 'force', 'format', 'jpegQuality'];

const state = {
  file: null,        // { name, bytes } as opened
  input: null,       // { bytes, ext, convertedFrom, kind } what Deskew gets, a browser decoded copy if it cannot read the file
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
    if (msg.ready) showEngineVersion();
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

function fileExt(name) {
  const m = /\.([^.]+)$/.exec(name);
  return m ? m[1].toLowerCase() : '';
}

// Formats Deskew can write, from the Output line of deskew -h, null until the engine answers
let outputFormats = null;
// Extensions listed under another name in deskew -h (Imaging format masks)
const EXT_FORMATS = {
  jpg: 'JPEG', jpeg: 'JPEG', jfif: 'JPEG', jpe: 'JPEG', jif: 'JPEG', tif: 'TIFF', tiff: 'TIFF', dib: 'BMP',
};

function canDeskewWrite(ext) {
  return !outputFormats || outputFormats.has(EXT_FORMATS[ext] ?? ext.toUpperCase());
}

// "Same as input" falls back to PNG (lossless) when Deskew cannot write the input format,
// e.g. AVIF converted by the browser or WebP which Deskew reads but does not write
function sameExt() {
  const ext = fileExt(state.file.name);
  return !state.input?.convertedFrom && ext && canDeskewWrite(ext) ? ext : 'png';
}

function outputExt() {
  const fmt = $('format').value;
  return fmt === 'same' ? sameExt() : fmt;
}

function baseName(name) {
  return name.replace(/\.[^.]+$/, '');
}

// Keeps names simple for the WASI file system, the original name is used for the download
function inputPath() {
  return `/in/input.${state.input.ext || 'img'}`;
}

function inputFiles() {
  return { [inputPath()]: state.input.bytes, '/out/': null };
}

// Name of the opened file's format for messages, e.g. AVIF
function formatLabel() {
  return (fileExt(state.file.name) || state.file.type.replace(/^image\//, '') || 'this format').toUpperCase();
}

// ---- Runs ----

async function detect() {
  if (!state.file) return;
  const seq = ++state.detectSeq;
  const file = state.file;
  setBusy(true);
  try {
    let run = await runDetection();
    if (seq !== state.detectSeq) return;  // newer detection already started

    // Deskew cannot read the file (AVIF, HEIC, ...), the browser can: give Deskew a lossless copy
    if (inputUnreadable(run.report) && state.bitmap && !state.input.convertedFrom) {
      const copy = await encodeForDeskew(state.bitmap);
      if (seq !== state.detectSeq || state.file !== file) return;
      if (copy) {
        state.input = { ...copy, convertedFrom: formatLabel() };
        updateFormatOptions();
        updateVisibility();
        run = await runDetection();
        if (seq !== state.detectSeq) return;
      }
    }

    const { args, result, report } = run;
    showLog(args, result);
    if (!report) showError(`Unexpected output from Deskew (exit code ${result.exitCode}), see the output below.`);
    state.report = report;
    if (report?.error) {
      const browserToo = inputUnreadable(report) && !state.bitmap;
      showError(browserToo ? `${report.error} The browser cannot decode ${formatLabel()} either.` : report.error);
    }

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

async function runDetection() {
  const args = ['-g', 'd', '-s', 'jw', '-o', '/out/', ...detectionArgs(), inputPath()];
  const result = await runDeskew(args, inputFiles());
  let report = null;
  try {
    report = JSON.parse(result.stdout);
  } catch { /* reported by the caller */ }
  return { args, result, report };
}

// Parameters were fine but the input was not loaded: unsupported format or a variant
// of a supported one the codec cannot read
function inputUnreadable(report) {
  return Boolean(report?.error && report.input && report.input.width == null);
}

// Lossless copy of what the browser decoded, 8 bits per channel, metadata like DPI is not kept.
// Canvas pixels are always RGBA, so opaque images go as PGM (gray) or PPM (color) to keep
// Deskew's memory use and the output as small as for the original, PNG only with transparency.
async function encodeForDeskew(bitmap) {
  try {
    const { width, height } = bitmap;
    const canvas = new OffscreenCanvas(width, height);
    const ctx = canvas.getContext('2d', { willReadFrequently: true });
    ctx.drawImage(bitmap, 0, 0);
    const rgba = ctx.getImageData(0, 0, width, height).data;
    let opaque = true;
    let gray = true;
    for (let i = 0; i < rgba.length && opaque; i += 4) {
      opaque = rgba[i + 3] === 255;
      gray &&= rgba[i] === rgba[i + 1] && rgba[i] === rgba[i + 2];
    }
    if (!opaque) {
      const blob = await canvas.convertToBlob({ type: 'image/png' });
      return { bytes: new Uint8Array(await blob.arrayBuffer()), ext: 'png', kind: 'PNG' };
    }
    const header = new TextEncoder().encode(`${gray ? 'P5' : 'P6'}\n${width} ${height}\n255\n`);
    const channels = gray ? 1 : 3;
    const bytes = new Uint8Array(header.length + width * height * channels);
    bytes.set(header);
    for (let i = 0, o = header.length; i < rgba.length; i += 4) {
      bytes[o++] = rgba[i];
      if (!gray) {
        bytes[o++] = rgba[i + 1];
        bytes[o++] = rgba[i + 2];
      }
    }
    return gray ? { bytes, ext: 'pgm', kind: 'grayscale' } : { bytes, ext: 'ppm', kind: 'RGB' };
  } catch {
    return null;  // too big for a canvas, for example
  }
}

async function deskewDecode() {
  // Skip angle at its maximum: Deskew only converts the input to PNG
  const args = ['-l', '90', '-o', '/out/view.png', inputPath()];
  const result = await runDeskew(args, inputFiles());
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
    const result = await runDeskew(args, inputFiles());
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

// Zoom and pan: content (image) coordinates are mapped to the canvas by a scale
// and an offset in CSS pixels. "Fit" mode follows the size of the view.
const viewport = { zoom: 1, panX: 0, panY: 0, fit: true, contentW: 0, contentH: 0 };
const MIN_ZOOM = 0.02;
const MAX_ZOOM = 32;

// Size of what the current view shows, in image pixels
function contentSize() {
  if (state.view === 'work' && state.workBitmap) return [state.workBitmap.width, state.workBitmap.height];
  const bmp = state.bitmap;
  if (!bmp) return [0, 0];
  if (state.view !== 'preview' || $('crop').checked) return [bmp.width, bmp.height];
  const a = previewAngleRad();
  return [
    Math.abs(bmp.width * Math.cos(a)) + Math.abs(bmp.height * Math.sin(a)),
    Math.abs(bmp.width * Math.sin(a)) + Math.abs(bmp.height * Math.cos(a)),
  ];
}

function previewAngleRad() {
  const det = state.report?.detection;
  const angle = det && willRotate(state.report) ? det.skewAngle : 0;
  // Positive skew means content turned clockwise, Deskew rotates counterclockwise to fix it
  return (-angle * Math.PI) / 180;
}

function viewSize() {
  const wrap = $('canvasWrap');
  return [wrap.clientWidth, wrap.clientHeight];
}

function fitZoom() {
  const [vw, vh] = viewSize();
  const { contentW: cw, contentH: ch } = viewport;
  if (!cw || !ch) return 1;
  return Math.min((vw - 16) / cw, (vh - 16) / ch);
}

function setZoom(zoom, anchorX, anchorY) {
  const [vw, vh] = viewSize();
  anchorX ??= vw / 2;
  anchorY ??= vh / 2;
  zoom = Math.min(MAX_ZOOM, Math.max(MIN_ZOOM, zoom));
  // Keep the image point under the anchor where it is
  const ix = (anchorX - viewport.panX) / viewport.zoom;
  const iy = (anchorY - viewport.panY) / viewport.zoom;
  viewport.zoom = zoom;
  viewport.panX = anchorX - ix * zoom;
  viewport.panY = anchorY - iy * zoom;
  viewport.fit = false;
  render();
}

function zoomToFit() {
  viewport.fit = true;
  render();
}

function center() {
  const [vw, vh] = viewSize();
  viewport.panX = (vw - viewport.contentW * viewport.zoom) / 2;
  viewport.panY = (vh - viewport.contentH * viewport.zoom) / 2;
}

// Called when the content changes (new image, view, detection, output options)
function draw() {
  const [cw, ch] = contentSize();
  const changed = cw !== viewport.contentW || ch !== viewport.contentH;
  const oldCx = viewport.contentW / 2;
  const oldCy = viewport.contentH / 2;
  viewport.contentW = cw;
  viewport.contentH = ch;
  if (changed && !viewport.fit) {
    // Same zoom, keep the content centered where it was (rotation changes the size a bit)
    viewport.panX += (oldCx - cw / 2) * viewport.zoom;
    viewport.panY += (oldCy - ch / 2) * viewport.zoom;
  }
  render();
}

function render() {
  const canvas = $('canvas');
  const ctx = canvas.getContext('2d');
  const [vw, vh] = viewSize();
  const dpr = window.devicePixelRatio || 1;
  canvas.width = Math.round(vw * dpr);
  canvas.height = Math.round(vh * dpr);
  canvas.dataset.content = `${Math.round(viewport.contentW)}x${Math.round(viewport.contentH)}`;

  ctx.setTransform(1, 0, 0, 1, 0, 0);
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  if (viewport.contentW && viewport.fit) {
    viewport.zoom = fitZoom();
    center();
  }
  updateZoomLabel();
  if (!viewport.contentW) return;
  const z = viewport.zoom;
  ctx.setTransform(dpr * z, 0, 0, dpr * z, dpr * viewport.panX, dpr * viewport.panY);
  // Pixels are worth seeing sharp when zoomed in
  ctx.imageSmoothingEnabled = z < 2;
  ctx.imageSmoothingQuality = 'high';

  if (state.view === 'work' && state.workBitmap) {
    ctx.drawImage(state.workBitmap, 0, 0);
  } else if (state.view === 'preview') {
    drawPreview(ctx, state.bitmap);
  } else {
    drawOriginal(ctx, state.bitmap, z);
  }
}

function drawOriginal(ctx, bmp, zoom) {
  ctx.drawImage(bmp, 0, 0);
  const det = state.report?.detection;
  if (!det) return;
  // Detection area in red, found content in green, as in the work image; 2 screen pixels wide
  ctx.lineWidth = 2 / zoom;
  if (det.detectionRect) strokeRect(ctx, det.detectionRect, 'rgba(220, 38, 38, 0.9)', [8 / zoom, 6 / zoom]);
  if (det.contentRect) strokeRect(ctx, det.contentRect, 'rgba(22, 163, 74, 0.95)', []);
}

function strokeRect(ctx, [l, t, r, b], color, dash) {
  ctx.save();
  ctx.strokeStyle = color;
  ctx.setLineDash(dash);
  ctx.strokeRect(l, t, r - l, b - t);
  ctx.restore();
}

function drawPreview(ctx, bmp) {
  const { contentW: cw, contentH: ch } = viewport;
  ctx.save();
  ctx.beginPath();
  ctx.rect(0, 0, cw, ch);
  ctx.clip();
  ctx.fillStyle = $('background').value;
  ctx.fillRect(0, 0, cw, ch);
  ctx.translate(cw / 2, ch / 2);
  ctx.rotate(previewAngleRad());
  ctx.drawImage(bmp, -bmp.width / 2, -bmp.height / 2);
  ctx.restore();
}

function updateZoomLabel() {
  $('zoomLabel').textContent = viewport.contentW ? `${Math.round(viewport.zoom * 100)} %` : '';
  $('zoomFit').classList.toggle('active', viewport.fit);
}

function setupZoomAndPan() {
  const wrap = $('canvasWrap');

  wrap.addEventListener('wheel', (e) => {
    if (!viewport.contentW) return;
    e.preventDefault();
    const rect = wrap.getBoundingClientRect();
    // Trackpads send small deltas, mouse wheels big ones
    const factor = Math.exp(-Math.max(-100, Math.min(100, e.deltaY)) * 0.0025);
    setZoom(viewport.zoom * factor, e.clientX - rect.left, e.clientY - rect.top);
  }, { passive: false });

  // Drag with mouse or one finger, pinch with two fingers
  const pointers = new Map();
  let pinch = null;
  wrap.addEventListener('pointerdown', (e) => {
    if (!viewport.contentW) return;
    wrap.setPointerCapture(e.pointerId);
    pointers.set(e.pointerId, { x: e.clientX, y: e.clientY });
    wrap.classList.add('dragging');
    if (pointers.size === 2) {
      const [p1, p2] = [...pointers.values()];
      pinch = { dist: Math.hypot(p1.x - p2.x, p1.y - p2.y), zoom: viewport.zoom };
    }
  });
  wrap.addEventListener('pointermove', (e) => {
    const last = pointers.get(e.pointerId);
    if (!last) return;
    const now = { x: e.clientX, y: e.clientY };
    pointers.set(e.pointerId, now);
    if (pointers.size === 2 && pinch) {
      const [p1, p2] = [...pointers.values()];
      const rect = wrap.getBoundingClientRect();
      const dist = Math.hypot(p1.x - p2.x, p1.y - p2.y);
      setZoom(pinch.zoom * dist / pinch.dist, (p1.x + p2.x) / 2 - rect.left, (p1.y + p2.y) / 2 - rect.top);
    } else if (pointers.size === 1) {
      viewport.panX += now.x - last.x;
      viewport.panY += now.y - last.y;
      viewport.fit = false;
      render();
    }
  });
  const release = (e) => {
    pointers.delete(e.pointerId);
    if (pointers.size < 2) pinch = null;
    if (pointers.size === 0) wrap.classList.remove('dragging');
  };
  wrap.addEventListener('pointerup', release);
  wrap.addEventListener('pointercancel', release);

  // Double click: fit <-> 100 % at the clicked point
  wrap.addEventListener('dblclick', (e) => {
    if (!viewport.contentW) return;
    const rect = wrap.getBoundingClientRect();
    if (viewport.fit) setZoom(1, e.clientX - rect.left, e.clientY - rect.top);
    else zoomToFit();
  });

  $('zoomIn').addEventListener('click', () => setZoom(viewport.zoom * 1.5));
  $('zoomOut').addEventListener('click', () => setZoom(viewport.zoom / 1.5));
  $('zoomFit').addEventListener('click', zoomToFit);
  $('zoom100').addEventListener('click', () => setZoom(1));

  new ResizeObserver(() => render()).observe(wrap);
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

// Outcomes of the JSON report as shown on the page, with the reason as a tooltip
const OUTCOMES = {
  detected: ['Detected', ''],
  noContent: ['No content', 'No content to detect the skew from (blank page?)'],
  lowConfidence: ['Low confidence', 'No dominant direction of text lines found within the max angle'],
  atRangeLimit: ['At max angle limit', 'Skew seems to be larger than the max angle, try raising it'],
};

function showReport(report, timeMs) {
  const det = report.detection;
  const el = $('result');
  el.innerHTML = '';
  if (!det) {
    el.textContent = 'No detection result.';
    return;
  }
  const outcomeClass = det.reliable ? 'ok' : 'warn';
  const [outcomeText, outcomeReason] = OUTCOMES[det.outcome] ?? [det.outcome, ''];
  const parts = [
    ['angle', `${det.skewAngle.toFixed(2)}°`],
    [outcomeClass, ` ${outcomeText}`, outcomeReason],
    ['muted', `, confidence ${(det.confidence * 100).toFixed(0)} %`],
    ['muted', ` · ${report.input.width} × ${report.input.height} ${report.input.format}`],
    ['muted', ` · ${Math.round(timeMs)} ms`],
  ];
  if (!willRotate(report)) parts.push(['muted', ' · output will not be rotated']);
  const from = state.input?.convertedFrom;
  if (from) {
    parts.push(['note', ` · ${from} decoded by the browser`,
      `Deskew cannot read ${from}, so the browser decoded it and gave Deskew a lossless ` +
      `${state.input.kind} copy (8 bits per channel, metadata like DPI is not kept). ` +
      `Deskew cannot write ${from} either, "Same as input" saves PNG.`]);
  }
  for (const [cls, text, title] of parts) {
    const span = document.createElement('span');
    span.className = cls;
    span.textContent = text;
    if (title) span.title = title;
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

// "Same as input" says what it actually means for the opened file
function updateFormatOptions() {
  const option = $('format').querySelector('option[value=same]');
  if (!state.file) {
    option.textContent = 'Same as input';
    return;
  }
  const ext = sameExt();
  const fallback = ext === 'png' && fileExt(state.file.name) !== 'png';
  option.textContent = `Same as input (${fallback ? `PNG, no ${formatLabel()} output` : (EXT_FORMATS[ext] ?? ext.toUpperCase())})`;
}

function updateVisibility() {
  $('thresholdLevel').disabled = $('thresholdAuto').checked;
  $('marginsRow').hidden = $('contentArea').value !== 'margins';
  $('jpegRow').hidden = !state.file ? $('format').value !== 'jpg' : outputExt() !== 'jpg';
}

async function openFile(file) {
  const bytes = new Uint8Array(await file.arrayBuffer());
  state.file = { name: file.name, type: file.type, bytes };
  state.input = { bytes, ext: fileExt(file.name), convertedFrom: null };
  state.report = null;
  state.workBitmap = null;
  state.bitmap = await decode(bytes);
  viewport.fit = true;
  $('fileName').textContent = `${file.name} (${formatSize(bytes.length)})`;
  $('downloadBtn').disabled = false;
  $('result').textContent = 'Detecting skew...';
  updateFormatOptions();
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
// Max angle slider and number box show the same value, the box allows values off the slider steps
$('maxAngleSlider').addEventListener('input', () => {
  $('maxAngle').value = $('maxAngleSlider').value;
  scheduleDetect();
});
$('maxAngle').addEventListener('input', () => {
  const value = parseFloat($('maxAngle').value);
  if (!Number.isNaN(value)) $('maxAngleSlider').value = value;
});

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

// Usage text of the engine (deskew -h), run once and shared by the status line and help
let usagePromise = null;
function getUsage() {
  usagePromise ??= runDeskew(['-h'], {}).then((result) => (result.stdout + result.stderr).trim());
  usagePromise.catch(() => { usagePromise = null; });  // try again next time
  return usagePromise;
}

// The first line of -h is the title, e.g. "Deskew 1.33 (2025-06-02, ...) by Marek Mauder"
async function showEngineVersion() {
  try {
    const usage = await getUsage();
    const outputs = /Output:\s*(.*)/.exec(usage)?.[1];
    if (outputs) outputFormats = new Set(outputs.split(',').map((s) => s.trim().toUpperCase()));
    if (state.file) {
      updateFormatOptions();
      updateVisibility();
    }
    const title = usage.split('\n')[0].trim();
    const version = /^Deskew\s+(\S+)/.exec(title)?.[1];
    if (!version) return;
    $('engineStatus').textContent = `Engine ready: Deskew ${version}`;
    $('engineStatus').title = title;
  } catch {
    // Status stays "Engine ready"
  }
}

// WebUI build: version.js is stamped when the site is assembled (CI), "dev" otherwise
function showWebuiVersion() {
  const { date, commit } = WEBUI_BUILD;
  const el = $('webuiVersion');
  if (!date) {
    el.textContent = 'development build';
    return;
  }
  el.textContent = `build ${date}`;
  if (commit) {
    const link = document.createElement('a');
    link.href = `https://github.com/galfar/deskew/commit/${commit}`;
    link.textContent = commit.slice(0, 7);
    el.append(' (', link, ')');
  }
}

// Help: formats and options come from the engine itself, so they stay in sync with the CLI
$('helpBtn').addEventListener('click', async () => {
  $('helpDialog').showModal();
  try {
    const text = await getUsage();
    $('formatsIn').textContent = /Input:\s*(.*)/.exec(text)?.[1] ?? '?';
    $('formatsOut').textContent = /Output:\s*(.*)/.exec(text)?.[1] ?? '?';
    $('usageText').textContent = text;
  } catch (e) {
    $('formatsIn').textContent = $('formatsOut').textContent = `Engine not available: ${e.message}`;
  }
});
// Click on the backdrop closes the help
$('helpDialog').addEventListener('click', (e) => {
  if (e.target === $('helpDialog')) $('helpDialog').close();
});

setupZoomAndPan();
setupTheme();
showWebuiVersion();

// Dark / light switch: follows the system until used, the choice is remembered in this browser
function setupTheme() {
  const root = document.documentElement;
  const systemDark = window.matchMedia('(prefers-color-scheme: dark)');
  const isDark = () => (root.dataset.theme ? root.dataset.theme === 'dark' : systemDark.matches);
  const update = () => {
    // Icon shows what a click switches to
    $('themeBtn').textContent = isDark() ? '☀' : '☾';
    $('themeBtn').title = isDark() ? 'Switch to light mode' : 'Switch to dark mode';
  };
  try {
    const saved = localStorage.getItem('deskew-theme');
    if (saved === 'dark' || saved === 'light') root.dataset.theme = saved;
  } catch { /* storage not available, follow the system */ }
  $('themeBtn').addEventListener('click', () => {
    root.dataset.theme = isDark() ? 'light' : 'dark';
    try {
      localStorage.setItem('deskew-theme', root.dataset.theme);
    } catch { /* not remembered then */ }
    update();
  });
  systemDark.addEventListener('change', update);
  update();
}

updateVisibility();
updateTabs();
