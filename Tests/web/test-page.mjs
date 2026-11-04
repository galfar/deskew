// Drives the web GUI in headless Chromium: open images, check the detection
// result, switch views, download the output.
// Usage: node Tests/web/test-page.mjs [screenshot-dir]
import { createServer } from 'node:http';
import { readFile, writeFile } from 'node:fs/promises';
import { dirname, join, extname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createRequire } from 'node:module';

const require = createRequire(import.meta.url);
const { chromium } = require('playwright');

const root = join(dirname(fileURLToPath(import.meta.url)), '../..');
const webDir = join(root, 'Web');
const shotDir = process.argv[2];
const types = { '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css', '.wasm': 'application/wasm' };

const server = createServer(async (req, res) => {
  try {
    let path = new URL(req.url, 'http://x').pathname;
    if (path === '/') path = '/index.html';
    const data = await readFile(join(webDir, path));
    res.writeHead(200, { 'Content-Type': types[extname(path)] ?? 'application/octet-stream' });
    res.end(data);
  } catch {
    res.writeHead(404);
    res.end();
  }
}).listen(0);
const url = `http://localhost:${server.address().port}/`;

let failures = 0;
function check(name, cond, detail = '') {
  console.log(`${cond ? 'ok  ' : 'FAIL'} ${name}${detail ? ' - ' + detail : ''}`);
  if (!cond) failures++;
}

const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1400, height: 900 }, acceptDownloads: true });
const errors = [];
page.on('pageerror', (e) => errors.push(String(e)));
page.on('console', (m) => m.type() === 'error' && errors.push(m.text()));

await page.goto(url);
await page.waitForSelector('#engineStatus:text("Engine ready")', { timeout: 30000 });
check('engine loads', true);
await page.waitForSelector('#engineStatus:text("Deskew 1.")', { timeout: 30000 });
check('engine version in status', true, await page.textContent('#engineStatus'));
const webuiVersion = await page.textContent('#webuiVersion');
check('WebUI version in footer', /development build|build \d{4}-\d\d-\d\d/.test(webuiVersion), webuiVersion);

async function openImage(name) {
  await page.$eval('#result', (el) => { el.innerHTML = ''; });
  await page.setInputFiles('#fileInput', join(root, 'TestImages', name));
  await page.waitForSelector('#result .angle', { timeout: 60000 });
  await page.waitForSelector('#busy', { state: 'hidden', timeout: 60000 });
  return page.textContent('#result');
}

const r2 = await openImage('2.png');
check('2.png detected', r2.includes('-2.19°') && r2.includes('detected'), r2);
if (shotDir) await page.screenshot({ path: join(shotDir, 'original.png') });

await page.click('[data-view=preview]');
if (shotDir) await page.screenshot({ path: join(shotDir, 'preview.png') });
await page.click('[data-view=work]');
const workSize = await page.$eval('#canvas', (c) => c.dataset.content);
check('work image shown', workSize === '1241x1748', `content ${workSize}`);
if (shotDir) await page.screenshot({ path: join(shotDir, 'work.png') });
await page.click('[data-view=original]');

// Zoom and pan
const fitLabel = await page.textContent('#zoomLabel');
const box = await page.locator('#canvasWrap').boundingBox();
await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
await page.mouse.wheel(0, -300);
const zoomedLabel = await page.textContent('#zoomLabel');
check('wheel zooms in', parseInt(zoomedLabel) > parseInt(fitLabel), `${fitLabel} -> ${zoomedLabel}`);
await page.mouse.down();
await page.mouse.move(box.x + box.width / 2 + 100, box.y + box.height / 2 + 50);
await page.mouse.up();
if (shotDir) await page.screenshot({ path: join(shotDir, 'zoomed.png') });
await page.click('#zoom100');
check('100 % button', (await page.textContent('#zoomLabel')) === '100 %');
await page.click('#zoomFit');
check('fit button', (await page.textContent('#zoomLabel')) === fitLabel, await page.textContent('#zoomLabel'));

// Dark / light switch
const bgBefore = await page.evaluate(() => getComputedStyle(document.body).backgroundColor);
await page.click('#themeBtn');
const bgAfter = await page.evaluate(() => getComputedStyle(document.body).backgroundColor);
check('theme switch', bgBefore !== bgAfter, `${bgBefore} -> ${bgAfter}`);
if (shotDir) await page.screenshot({ path: join(shotDir, 'dark.png') });
await page.click('#themeBtn');

// Help with formats from the engine
await page.click('#helpBtn');
await page.waitForFunction(() => document.querySelector('#formatsIn').textContent.includes('TIFF'), null, { timeout: 30000 });
const formatsIn = await page.textContent('#formatsIn');
check('help lists formats', formatsIn.includes('PNG') && formatsIn.includes('WEBP'), formatsIn);
if (shotDir) await page.screenshot({ path: join(shotDir, 'help.png') });
await page.keyboard.press('Escape');

// A detection option triggers detection again
await page.fill('#maxAngle', '1');
await page.waitForFunction(() => !document.querySelector('#result').textContent.includes('-2.19°'), null, { timeout: 30000 });
await page.waitForSelector('#busy', { state: 'hidden' });
const r2b = await page.textContent('#result');
check('max angle 1 changes the outcome', /atRangeLimit|lowConfidence|will not be rotated/.test(r2b), r2b);
await page.fill('#maxAngle', '10');
await page.waitForFunction(() => document.querySelector('#result').textContent.includes('-2.19°'), null, { timeout: 30000 });

// Download
await page.selectOption('#format', 'jpg');
const [download] = await Promise.all([page.waitForEvent('download', { timeout: 60000 }), page.click('#downloadBtn')]);
const outPath = await download.path();
const out = await readFile(outPath);
check('download JPEG', download.suggestedFilename() === 'deskewed-2.jpg' && out[0] === 0xff && out[1] === 0xd8,
  `${download.suggestedFilename()}, ${out.length} bytes`);

// TIFF: the browser cannot decode it, Deskew converts it for the view
const rt = await openImage('1-g4.tif');
await page.waitForFunction(() => document.querySelector('#canvas').dataset.content === '2076x3086', null, { timeout: 30000 });
check('TIFF detected and shown', rt.includes('-1.35°'), rt);
if (shotDir) await page.screenshot({ path: join(shotDir, 'tiff.png') });

// Blank page
const rb = await openImage('blank.png');
check('blank page', rb.includes('noContent') && rb.includes('will not be rotated'), rb);

check('no page errors', errors.length === 0, errors.join('; '));

await browser.close();
server.close();
console.log(failures ? `${failures} failed` : 'all passed');
process.exit(failures ? 1 : 0);
