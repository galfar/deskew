# Using Deskew

This is the reference for the command line tool: all the options, what the detection reports,
the JSON report for scripts, large images and things good to know. Run `deskew -h` for a short
version of the options.
Examples for common tasks are in the [Readme](../Readme.md#examples).

```
deskew [options] input
```

- [How options are written](#how-options-are-written)
- [Input and output](#input-and-output): [`-o`](#-o-output-file), [`-b`](#-b-background-color), [`-q`](#-q-resampling-filter), [`-f`](#-f-output-pixel-format), [`-c`](#-c-output-file-format-specs), [`-p`](#-p-print-resolution)
- [Skew detection](#skew-detection): [`-a`](#-a-max-angle), [`-d`](#-d-angle-step), [`-l`](#-l-skip-angle), [`-t`](#-t-threshold), [`-r`](#-r-content-rectangle), [`-m`](#-m-margins)
- [Flags](#flags): [`-g`](#-g-operational-flags), [`-s`](#-s-info-flags), [`-h`](#-h-help)
- [What the detection reports](#what-the-detection-reports)
- [JSON report](#json-report)
- [Exit code](#exit-code)
- [Large images](#large-images)
- [PDF, multi-page TIFF and OCR](#pdf-multi-page-tiff-and-ocr)
- [Good to know](#good-to-know)

## How options are written

- Every option is followed by its value as a separate argument: `-a 5`, `-g d`, `-s sp`.
  Joined forms like `-a5` or `-gd` are not accepted.
- Option names are case sensitive, values are not.
- Numbers use decimal point (`-a 2.5`) no matter what the system locale is.
- Values that are lists use commas: `-c j85,tlzw`, `-m 10,20,mm`. Do not put spaces
  inside unless the whole value is quoted.
- There is exactly one input file. The order of the options and the input file does not matter.
- The same option can be given more than once, the last value wins
  (flags of `-g` and `-s` add up, `-c` specs for one file format cannot be repeated).

## Input and output

### Input file

Any image in one of the supported file formats: BMP, JPEG, PNG, GIF, QOI, TGA, PBM/PGM/PPM,
TIFF and WebP. The list is also printed at the end of the help (`deskew -h`).
WebP can be written only when the libwebp library is found (see [Good to know](#good-to-know)).

Only the first page of multi-page files is processed, see [PDF, multi-page TIFF and OCR](#pdf-multi-page-tiff-and-ocr)
for how to deskew all the pages.

### `-o` output file

File to save the result to, or a folder. The file format is given by the extension of the file.

- Default: `deskewed-<input name>.png` in the folder of the input file.
- **Folder**: a path that ends with `/` (or `\` on Windows) or a folder that exists. The output gets
  the name of the input file there: `deskew -o out/ scans/page.tif` writes `out/page.tif`.
  Input formats that cannot be written (WebP without libwebp) are saved as PNG.
- Missing folders of the output path are created.
- The input file cannot be overwritten: output that is the input file (in any spelling of its path),
  or the folder of the input file given as the output folder, is refused. Detect only mode (`-g d`)
  writes no image and accepts it.
- When nothing was changed in the image (the page is not skewed, or the rotation was skipped)
  and the output has the same extension as the input, the input file is copied as it is.
  It is not compressed again and compression options are not applied.

### `-b` background color

Color of the empty areas that appear around the rotated page. Hexadecimal, without any prefix.

| Form | Meaning | Example |
|---|---|---|
| `LL` | gray level | `-b ff` white, `-b 80` gray |
| `RRGGBB` | red, green, blue | `-b ffffff` white, `-b fff8e0` cream |
| `AARRGGBB` | opacity + color, `00` is fully transparent | `-b 00000000` transparent |

- Default: `000000` (black).
- Color with opacity below `ff` makes the output 32-bit with alpha channel.
  Use a file format that can store it (PNG, TIFF, ...).
- Color that is not gray makes the output of a grayscale image 24-bit RGB.

### `-q` resampling filter

Filter used when the image is rotated.

| Value | |
|---|---|
| `nearest` | fastest, no new colors are created, edges get jagged |
| `linear` | bilinear, fast and good for most scans (default) |
| `cubic` | bicubic (Catmull-Rom), sharper, slower |
| `lanczos` | sharpest, slowest |

### `-f` output pixel format

Forces the pixel format of the output image: `b1` (1-bit black and white), `g8` (8-bit grayscale),
`rgb24`, `rgba32`.

By default the output keeps the format of the input where it can. Rotation itself works with 8-bit
grayscale, 24-bit RGB and 32-bit RGBA, other formats are converted to the closest of these.
In particular **1-bit input becomes 8-bit grayscale**: rotation with filtering creates gray pixels
along the edges and turning them back to black and white would damage the text. The price is a larger file.
Use `-f b1` if you want 1-bit output anyway.

### `-c` output file format specs

Settings of the output file format, compression so far. One or more specs separated by commas.
They only apply when the output is in the given file format.

| Spec | Applies to | |
|---|---|---|
| `jNN` | JPEG, TIFF with JPEG compression | quality 1 to 100 (best), default is 90 |
| `tnone` | TIFF | no compression |
| `tlzw` | TIFF | LZW (default) |
| `trle` | TIFF | PackBits RLE |
| `tdeflate` | TIFF | Deflate/ZIP |
| `tjpeg` | TIFF | JPEG, lossy |
| `tg4` | TIFF | CCITT Group 4 fax, output is converted to 1-bit |
| `tinput` | TIFF | same compression as the input TIFF, including lossy JPEG |
| `tinput-lossless` | TIFF | same as the input TIFF, but LZW instead of JPEG |

Examples: `-c j85`, `-c tdeflate`, `-c j95,tjpeg`.

- 1-bit TIFFs are written with Group 4 compression unless `tnone` is given.
- `tinput` falls back to the default when the input is not a TIFF or its compression cannot be used
  for writing. `tinput` is not the default because the rotated image would be compressed
  with JPEG again when the input was, losing some quality each time.

### `-p` print resolution

Resolution of the image in DPI (dots per inch), whole number. It is used instead of the resolution
stored in the input file, or when the file has none:

- for content rectangle and margins given in `mm`, `cm` or `in`,
- it is written to the output file.

## Skew detection

Deskew looks for the lines of text. The image is converted to grayscale, pixels darker than
the [threshold](#-t-threshold) are taken as text and a Hough transform finds the direction in which
they line up the best, within the [max angle](#-a-max-angle) on both sides of horizontal.

### `-a` max angle

The largest skew angle that is looked for, in degrees, in both directions. Default: `10`,
allowed: more than 0, up to 90.

Larger range takes proportionally more time and memory. A page skewed more than the max angle
is not fixed: the detection ends as not reliable and the image is left as it is
(see [What the detection reports](#what-the-detection-reports)).

### `-d` angle step

Step between the tested angles in degrees. Default: `0.1`, allowed: 0.01 to 5.

Smaller step is more precise and slower. The reported angle is finer than the step because
it is an average of several of the strongest lines.

### `-l` skip angle

Pages skewed by less than this angle (in degrees) are not rotated. Default: `0.01`, allowed: 0 to 90.

Rotation always blurs the image a bit and makes it larger, so there is no point in rotating by a few
hundredths of a degree. `-l 0` rotates whenever any skew is found, and so does forced rotation (`-g f`).

### `-t` threshold

Tells which pixels are text and which are background: `a` for automatic (default) or a level from 1 to 255.
Pixels darker than the level are text.

Automatic threshold uses Otsu's method on the area where the skew is detected. It works well for
ordinary black on white pages. Set the level yourself for faint text, colored paper or when the
automatic one picks up the paper texture. `-s w` saves the thresholded image so you can see the result.

### `-r` content rectangle

Limits the skew detection to a part of the page. Useful when there is something outside of the text that
confuses the detection: pictures, borders, punch holes, edges of a book. The whole image is still rotated.

```
-r left,top,right,bottom[,unit]
-r auto
```

- The four numbers are positions of the edges measured from the top left corner of the image
  (not width and height).
- Units: `px` pixels (default), `%` of the image width and height, `mm`, `cm`, `in`.
  The last three need to know the resolution of the image, see [`-p`](#-p-print-resolution).
  If the file has no resolution and `-p` is not given, Deskew stops with an error.
- `auto` finds the content area of each image automatically: dark areas touching the edges of the image
  (scanner bed around the page, shadows) and stray dots are left out and the detection is limited
  to the area where the rest is. It works well for pages with white margins and for a page lying
  on a dark background. It does not help much with book scans where the edges of the pages and
  the opposite page are inside the picture; use margins or an explicit rectangle for those.

Examples: `-r 200,300,2200,3200`, `-r 10,10,90,90,%`, `-r 1,1.5,7.5,10,in`, `-r auto`.

Default is the whole image. `-r` and `-m` cannot be used together.

### `-m` margins

Another way to limit the detection: parts along the edges of the page that are left out.

```
-m all[,unit]
-m horizontal,vertical[,unit]
-m left,top,right,bottom[,unit]
```

Units are the same as for [`-r`](#-r-content-rectangle).

Examples: `-m 100` (100 pixels on each side), `-m 5,%`, `-m 15,10,%` (15 % left and right, 10 % top and bottom),
`-m 20,10,10,15,mm`.

## Flags

### `-g` operational flags

Any combination of these letters:

| Flag | |
|---|---|
| `c` | crop to input size: the output has the same size as the input, corners of the rotated page are cut off instead of making the image larger |
| `d` | detect only: the skew angle is printed, no output file is written |
| `f` | force rotation: always rotate by the detected angle, also when it is not reliable or smaller than the [skip angle](#-l-skip-angle) |

Example: `-g cd`.

### `-s` info flags

More information printed to the output. Any combination of these letters:

| Flag | |
|---|---|
| `s` | skew detection stats |
| `p` | program parameters as they were understood |
| `t` | time taken by each step |
| `l` | libraries used for zlib, JPEG, TIFF and WebP: shared libraries found on the system, or the code built in (see [Good to know](#good-to-know)) |
| `w` | save the work image: black and white image that the detection sees, with a red frame inside the area where the skew was detected and a green one around the found content. It is saved next to the output as `<output file name>.work.png` (always PNG), also in detect only mode |
| `j` | [JSON report](#json-report) to standard output; the text output goes to standard error then |
| `r` | [JSON report](#json-report) to a file: name of the output file with `.json` added |

### `-h` help

Prints the usage and the list of supported file formats. `--help` and `-?` do the same.

## What the detection reports

```
Calculating skew angle using threshold 201...
Skew angle found [deg]: -1.210
Detection confidence: 100%
```

**Skew angle** is positive when the content is turned clockwise (text lines go down to the right).
The image is then rotated counterclockwise by this angle. For example ImageMagick's `-rotate`
takes the opposite sign for the same correction.

**Confidence** says how much the found angle stands out from all the other tested angles, from 0 to 100 %.
Clean text pages get 100 %. It is a relative measure, not a probability: pages with little text,
handwriting, drawings or comic pages get lower values even when the angle is right.
Below 30 % the angle is not considered reliable.

The detection can end in these ways:

| Outcome | What happened | What Deskew does |
|---|---|---|
| detected | lines have a clear direction | rotates the image (unless the angle is below the [skip angle](#-l-skip-angle)) |
| no content | blank page, nothing but a dark border or a few stray dots | nothing, the image is saved as it is |
| low confidence | there is something on the page but without a dominant direction within the max angle: noise, photo, or a page skewed much more than the max angle | nothing, the image is saved as it is |
| at range limit | lines get stronger towards the max angle, the real skew is probably just beyond it | nothing, the image is saved as it is; try a larger `-a` |

In the last three cases Deskew prints why the angle is not reliable:

```
No content found in the image using threshold 151, skew detection skipped.
Skew angle found [deg]: 0.000
Detection confidence: 0%
Skew angle is not reliable: no content to detect the skew from (blank page?).
Skipping deskewing step, use "-g f" to rotate the image anyway.
```

The run still ends with success and the output file is written, so batches go on.
With `-g f` the image is rotated by whatever angle was found (the skip angle is ignored then too).

With `-s s` there is more:

```
Skew detection stats:
  content area:       [0,104,1241,1619]
  pixel count:               2,168,027
  tested pixels:                59,171
  accumulator size:          1,195,200
  accumulated counts:       11,834,200
  best count:                      357
  peak angle:                   -6.200
  lines at peak:                    20
  sharpness:                     0.559
  confidence:                    1.000
  outcome:                    detected
```

- *content area*: where the content of the page was found, `none` for a blank page.
  This is the area used with `-r auto`.
- *tested pixels*: pixels on the bottom edge of something dark, these vote for the lines.
- *best count*: number of pixels on the strongest line.
- *peak angle*: tested angle with the best score (each angle gets a score from all of its lines).
- *lines at peak*: how many of the 20 strongest lines agree with the peak angle. The resulting angle is their average.
  When the strongest lines go elsewhere (they are page edges or borders, not text) the peak angle itself is used.
- *sharpness*: how much the peak stands out, confidence is derived from it.

## JSON report

Scripts do not have to read the text output. `-s j` and `-s r` give the same information
as a JSON document:

```
deskew -s j -o out/page.tif page.tif > report.json
deskew -s r -o out/page.tif page.tif
```

- **`-s j`** writes the report to standard output. Nothing else is written there then:
  the usual text goes to standard error, so `deskew -s j ... | jq .` works and the progress can still
  be seen in the terminal.
- **`-s r`** writes the report to a file next to the output image. Its name is the name of the output
  file with `.json` added: `out/page.tif.json` here, `deskewed-page.png.json` when no `-o` is given.
  It is the same in detect only mode (`-g d`) where no output image is written: the report is named
  after the output that would be written. Standard output has the usual text.
- Both can be used together (`-s jr`), the file and the standard output then have the same content.
- The report always has everything. The other info flags (`s`, `p`, `t`) only control the text output.

```json
{
  "deskew": {
    "version": "1.35",
    "target": "x64 Windows, FPC",
    "libraries": {
      "zlib": { "backend": "dynlib", "version": "1.3.1" },
      "jpeg": { "backend": "pascal", "version": null },
      "tiff": { "backend": "pastiff", "version": null },
      "webp": { "backend": "pascal", "version": null }
    }
  },
  "input": {
    "file": "C:\\Scans\\page.tif",
    "width": 2076,
    "height": 3086,
    "format": "Binary",
    "dpiX": 96,
    "dpiY": 96
  },
  "parameters": {
    "maxAngle": 10,
    "angleStep": 0.1,
    "skipAngle": 0.01,
    "thresholding": "auto",
    "thresholdLevel": null,
    "contentArea": "page",
    "contentAreaValues": null,
    "contentAreaUnit": "px",
    "dpiOverride": null,
    "resamplingFilter": "linear",
    "backgroundColor": "FFFFFFFF",
    "outputFormat": null,
    "jpegQuality": null,
    "tiffCompression": null,
    "detectOnly": false,
    "forceRotation": false,
    "cropToInput": false
  },
  "detection": {
    "skewAngle": -1.375,
    "confidence": 1,
    "outcome": "detected",
    "reliable": true,
    "threshold": 1,
    "contentRect": [117, 59, 1949, 2999],
    "detectionRect": [0, 0, 2076, 3086],
    "peakAngle": -1.4,
    "sharpness": 0.787,
    "linesAtPeak": 20,
    "bestCount": 624,
    "pixelCount": 6404460,
    "testedPixels": 76981,
    "accumulatorSize": 2064400,
    "accumulatedCounts": 15396200
  },
  "rotated": true,
  "output": {
    "file": "C:\\Scans\\out\\page.tif",
    "written": "saved",
    "width": 2151,
    "height": 3136,
    "format": "Gray8",
    "tiffCompression": null
  },
  "workImageFile": null,
  "timingsMs": {
    "load": 4.331,
    "grayConversion": 2.084,
    "thresholding": 15.744,
    "contentDetection": 3.593,
    "skewDetection": 107.613,
    "workImage": null,
    "formatConversion": 6.120,
    "rotation": 210.744,
    "save": 31.242,
    "total": 408.205
  },
  "error": null
}
```

| Member | |
|---|---|
| `deskew.version`, `deskew.target` | version and build of the tool, as in the first line of the text output |
| `deskew.libraries` | what reads and writes the files, as `-s l` shows it: `backend` and `version` (`null` when not known) for `zlib` (`dynlib` for the shared library, or the built in `bundled`, `fpc`, `delphi`), `jpeg` (`turbojpeg` or `pascal`), `tiff` (`libtiff` or `pastiff`) and `webp` (`libwebp` or `pascal`) |
| `input.file` | full path of the input file |
| `input.width`, `height`, `format` | size in pixels and pixel format of the loaded image |
| `input.dpiX`, `dpiY` | print resolution used for the image: from the file, or the one given with `-p`. `null` when it is not known |
| `parameters` | options of the run including the defaults. `thresholdLevel` is the value of `-t` (`null` with automatic threshold), `contentArea` is `page`, `rect`, `margins` or `auto` and `contentAreaValues` has the numbers of `-r` or `-m` in `contentAreaUnit`. Members for options that were not given and have no fixed default are `null` |
| `detection.skewAngle` | detected skew angle in degrees, [positive when the content is turned clockwise](#what-the-detection-reports) |
| `detection.confidence` | 0 to 1 (the text output shows it in percents) |
| `detection.outcome` | `detected`, `noContent`, `lowConfidence` or `atRangeLimit`, see [the outcomes](#what-the-detection-reports) |
| `detection.reliable` | `true` for `detected`. The image is rotated only then, unless the rotation is forced |
| `detection.threshold` | threshold level the detection used: the automatically found one or the one given with `-t` |
| `detection.contentRect` | content area found on the page as `[left, top, right, bottom]` in pixels, `null` for a blank page |
| `detection.detectionRect` | area where the skew was detected: whole page, the area given with `-r` or `-m`, or the content area with `-r auto` |
| `detection.peakAngle` ... `accumulatedCounts` | detection stats, the same numbers as `-s s` prints |
| `rotated` | `true` when the image was rotated. It is `false` in detect only mode, when the angle is not reliable or when it is below the skip angle |
| `output.file` | full path of the output file, `null` in detect only mode |
| `output.written` | `saved`: the image was written in the output file format. `copied`: nothing was changed and the input file was copied as it is. `none`: no output (detect only mode, or an error) |
| `output.width`, `height`, `format` | size and pixel format of the written image, `null` when nothing was written |
| `output.tiffCompression` | TIFF compression asked from the TIFF writer when a TIFF was saved: the scheme of `-c t...`, with `input` and `input-lossless` replaced by the scheme found in the input (e.g. `g4`, `lzw`). `null` when the writer's default is used (LZW, CCITT G4 for 1-bit images) or no TIFF was saved. The writer still switches 1-bit images to G4 and uses LZW when the scheme does not fit the image (e.g. JPEG for 1-bit). `parameters.tiffCompression` is the option as given |
| `workImageFile` | full path of the work image saved with `-s w`, `null` when none was saved |
| `timingsMs` | time of the steps in milliseconds, `null` for steps that did not run: `load`, `grayConversion` (working copy of the image for the detection), `thresholding` (automatic threshold; with `-r auto` both runs, for the page and for the found content), `contentDetection`, `skewDetection`, `workImage` (`-s w`), `formatConversion` (pixel format changes of the image before the rotation and for the output, e.g. `-f`), `rotation`, `save`. `total` is the time of the whole run, including what is not listed (parsing, the report itself) |
| `error` | `null`, or the error message when the run failed |

What can be counted on:

- The document is plain ASCII, other characters in file names are written as `\uXXXX` escapes.
  Numbers use decimal point.
- New members can be added in future versions. Existing ones keep their names and meaning.
- **Errors** are reported the same way: `error` has the message and the [exit code](#exit-code) is 1.
  Parts that Deskew did not get to are left out or `null`: there is no `detection` when the input could not
  be loaded, and only `deskew` and `error` when the parameters were wrong. The report file of `-s r` is
  written on errors too when the name of the output file is known.
- `-h` shows the usual help, also with `-s j`.

Examples:

```
# bash: skew angle of a page
deskew -s j -g d page.png 2>/dev/null | jq .detection.skewAngle

# bash: list the pages of a batch that were not rotated because the detection was not reliable
for f in scans/*.png; do
  deskew -s j -o "out/$(basename "$f")" "$f" 2>/dev/null | jq -r 'select(.detection.reliable == false) | .input.file'
done
```

```powershell
# PowerShell
$report = .\deskew -s j -g d page.png 2>$null | ConvertFrom-Json
$report.detection.skewAngle
```

```python
# Python
import json, subprocess
run = subprocess.run(["deskew", "-s", "j", "-o", "out.png", "in.png"], capture_output=True)
report = json.loads(run.stdout)
if report["error"]:
    print("failed:", report["error"])
elif not report["detection"]["reliable"]:
    print("not rotated:", report["detection"]["outcome"])
```

## Exit code

- `0`: success. Also when the rotation was skipped because the page is not skewed or the angle is not reliable.
- `1`: error: invalid parameters, file cannot be read or written, unsupported file format, out of memory.
  The reason is printed as the last thing (and it is in the `error` member of the [JSON report](#json-report)).

## Large images

Deskew was tested with images of 28000 x 26000, 40000 x 36000 and 48000 x 44800 pixels in 24-bit RGB
(2, 4 and 6 GiB of pixel data, the last one has more than 2^31 pixels) coming from QOI, PNG and
BigTIFF files, in 64-bit builds.

- **Memory**: a full run needs about 2 times the size of the uncompressed image
  (the original and the rotated result, the grayscale working image is freed before the rotation),
  that is about 8.3 GB for a 4 GiB image. Detect only run (`-g d`) needs about 1.4 times the size.
- **Time** for the 4 GiB image on a desktop PC: loading 6 to 12 s, detection 5 s,
  rotation 70 s with the linear filter and 3 minutes with Lanczos.
- **Input and output formats**: Deskew can work with TIFF, PNG, JPEG, QOI, and BMP files over 4 GB. Exact limits depend on file format but mainly on the available memory.
- **TIFF**: files over 4 GB need BigTIFF. It is read in all builds and written automatically when the classic TIFF is not enough.
- 32-bit builds cannot work with images anywhere near these sizes.

## PDF, multi-page TIFF and OCR

Here are a few examples of how you can use Deskew in conjunction with other tools. 

Deskew works with one image at a time and PDFs are not supported at all.
Scanned PDFs and multi-page TIFFs are split into pages first,
the pages are deskewed, and joined again. These recipes use common free tools (Debian/Ubuntu package names
in parentheses; most of them are available for macOS and Windows as well).

### Scanned PDF

With [poppler](https://poppler.freedesktop.org/) (`poppler-utils`) and [img2pdf](https://gitlab.mister-muffin.de/josch/img2pdf) (`img2pdf`):

```bash
mkdir pages out
pdfimages -list scan.pdf                  # x-ppi column: resolution of the scanned pages
pdftoppm -r 300 -png scan.pdf pages/page  # pages/page-1.png, page-2.png, ... at 300 DPI
for f in pages/*.png; do deskew -b ffffff -o out/ "$f"; done
img2pdf out/*.png -o scan-deskewed.pdf
```

- Give `pdftoppm -r` the resolution of the scan (`pdfimages -list` tells it): a higher one only makes
  the files bigger. With 10 or more pages the numbers get leading zeros, so the order stays right.
- img2pdf puts the images into the PDF as they are (no recompression) and gets the size of the page from
  their resolution, which Deskew carries over. Rotated pages are a bit bigger than the original ones
  (the corners are added), `-g c` keeps the size of the input.
- 1-bit scans: `-f b1 -b ff` instead of `-b ffffff` keeps the pages 1-bit and the PDF small.
  Color scans: `pdftoppm -jpeg` and `deskew -c j85` make JPEG pages, img2pdf embeds them unchanged.
- `pdfimages -png scan.pdf pages/page` extracts the scanned images with their exact pixels instead of
  rendering the pages, but they have no resolution stored: give it to Deskew with `-p` (e.g. `-p 300`,
  the value from `pdfimages -list`), otherwise the pages come out much larger in the PDF. This works when
  each page is a single image.

### Multi-page TIFF

With the libtiff tools (`libtiff-tools`):

```bash
mkdir pages out
tiffsplit scan.tif pages/page-            # pages/page-aaa.tif, page-aab.tif, ...
for f in pages/*.tif; do deskew -f b1 -b ff -o out/ "$f"; done
tiffcp out/*.tif scan-deskewed.tif
```

- `-f b1 -b ff` is for black and white pages (they are saved with Group 4 fax compression).
  For gray or color pages leave it out and use `-b ffffff -c tinput-lossless` to keep the compression of the input.
- tiffcp copies the pages as they are, with their own compression and resolution.
- With ImageMagick: `magick scan.tif pages/page-%03d.tif` to split and `magick out/*.tif scan-deskewed.tif`
  to join (ImageMagick 6: `convert`).

### OCR

Text recognition works better on straight pages. With [Tesseract](https://github.com/tesseract-ocr/tesseract) (`tesseract-ocr`),
after the pages are deskewed as above:

```bash
tesseract out/page-1.png out/page-1       # plain text in out/page-1.txt
ls out/*.png > pages.txt
tesseract pages.txt scan-ocr pdf          # searchable PDF, one page per image
```

Tesseract uses the resolution of the images, so keep it right (`-p` when the input has none).
[OCRmyPDF](https://ocrmypdf.readthedocs.io/) has its own `--deskew`; to use Deskew instead, make the PDF
with img2pdf as above and run `ocrmypdf scan-deskewed.pdf scan-ocr.pdf`.

## Good to know

- **Libraries**: no libraries are needed, all the file formats are read and written by code built
  into Deskew. When the system has zlib, libjpeg-turbo (2.0 or newer, TurboJPEG API), libtiff or libwebp,
  they are used instead (on Windows: `zlib1.dll`, `turbojpeg.dll`, `libtiff.dll` or `libwebp.dll` next to
  `deskew.exe` or in `PATH`). `-s l` shows what is used, the JSON report has it in `deskew.libraries`.
  Saving WebP needs libwebp.
- **Output location**: without `-o` the output goes next to the input file, not to the current folder.
- **1-bit images** report `using threshold 1` with automatic threshold. That is right: they only have black (0) and white (255) pixels.
- **File size after deskewing 1-bit scans**: the output is 8-bit grayscale by default and it is much larger.
  `-f b1` keeps it 1-bit (use it with TIFF or PNG output), `-q nearest` keeps the pixels black and white
  in the 8-bit output so it compresses much better.
- **Blank pages with paper texture**: automatic threshold can split the texture of the paper into
  "text" and "background". The page is then recognized as low confidence and left alone, but the detection
  takes much longer than usual. An explicit threshold (`-t 128`) avoids it.
- **Speed**: the time of the detection depends mostly on the number of dark pixels and on the max angle
  and angle step. Dense pages in high resolution can take several seconds. Larger angle step (`-d 0.2`) or
  smaller max angle (`-a 5`) helps.
- **Skew larger than the max angle** is not fixed and its angle is not reported. A page skewed
  just beyond the max angle ends as `atRangeLimit`, one skewed much more as `lowConfidence`
  (the same as noise). Raise `-a` when pages can be skewed that much.
- **`-g f` on a blank page** rotates by a meaningless angle. Forcing is meant for pages where you know
  better than the detection.
- **Metadata**: print resolution (DPI) is carried over to JPEG, PNG and TIFF output. 
  Other metadata (EXIF, PNG text chunks, ICC profiles) is not carried over.
- **Piping the output** into a program that stops reading early (`deskew ... | head -3`) can end with an
  I/O error message, the image was processed anyway.
- **Reading the results in scripts**: use the [JSON report](#json-report). The text output is meant
  for people; the line `Skew angle found [deg]: X` stays as it is for the existing scripts.
