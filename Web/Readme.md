Deskew WebUI
===========================

Live version: <https://galfar.github.io/deskew-webui>

A static web page that runs the Deskew command line tool compiled to WebAssembly
(`wasm32-wasip1`) in the browser. Images are processed locally and never leave the
user's machine, so the page can be hosted anywhere static files can be, GitHub Pages included.

How it works
------------------------

- `deskew.wasm` is the unchanged CLI (`deskew.lpr`) built for WASI.
- `wasi.js` is a small in-memory WASI host: the input image is put into a virtual file
  system, the CLI is started with ordinary command line arguments and the output files
  and standard output are read back afterwards.
- `worker.js` runs the CLI in a Web Worker so the page stays responsive. Every run is a
  new instance of the module (compiled once).
- `app.js` builds the arguments from the toggles (as `Gui/options.pas` does for the
  desktop GUI) and reads the JSON report (`-s j`).

Detection runs in detect only mode (`-g d -s jw`) whenever a detection option changes;
the page shows the angle, outcome and confidence, the detection area and found content
over the image, and the work image. The deskewed preview is a canvas rotation of the
decoded input, so output toggles (background, crop, force rotation) are instant. The real
rotation and encoding run only for "Deskew & download".

All three views (original with overlay, deskewed preview, work image) can be zoomed with the
mouse wheel or a pinch and panned by dragging; they are drawn from the full resolution image.
The help button lists the supported formats and all options, taken from `deskew -h` of the engine.
The engine version (also from `deskew -h`) is shown in the header as soon as the engine loads.
The footer shows the WebUI build: `version.js` marks a development build, CI overwrites it with
the build date and commit when it assembles the site.

Formats the browser cannot display (TIFF, TGA, PNM) are converted to PNG by Deskew
itself for the view.

Building
------------------------

Needs FPC trunk with the `wasm32-wasip1` target and `wasm-ld` (FPC 3.2 has no WebAssembly
support). `Scripts/compile-wasm.sh` builds `Web/deskew.wasm` with the options of `Scripts/compile.sh`
plus `-Twasip1 -Pwasm32 -CTbfexceptions`; FPC can be given in the `FPC` environment variable.

Branchful exceptions (`-CTbfexceptions`) do not need the wasm exception handling proposal in the
browser. Errors in the codecs are caught and reported as in the native build, with an `error`
member in the JSON report and exit code 1.

Tested with FPC 3.3.1-20578-gad4da509ba (FPCupDeluxe, 2026-07-16) and wasm-ld 16. The binary is
about 4.8 MB and not committed (`Web/.gitignore`), the page needs it next to `index.html`.

Running locally
------------------------

The page uses a module worker and `fetch`, so it must be served over HTTP, opening
`index.html` as a file does not work:

    cd Web
    python3 -m http.server 8000

Then open <http://localhost:8000>.

`Tests/web/test-wasi.mjs` runs `deskew.wasm` under the same WASI host in Node, and
`Tests/web/test-page.mjs` drives the page in a headless browser with Playwright.

Limits
------------------------

- WebAssembly memory is 32-bit, so the huge image cases (hundreds of megapixels) do not fit.
- TIFF, PNG, JPEG, BMP, TGA, PNM and WebP input work (Imaging's own Pascal codecs), WebP output
  does not (it would need libwebp).
- Each detection decodes the input again, large scans take a moment per toggle change.
  Moving to a library build with the image kept in memory is the next step if that is
  too slow.
