#!/usr/bin/env python3
"""End-to-end checks of the Deskew command line tool.

Runs the built executable over TestImages/ and checks its results through the
JSON report (-s j) and the files written:
  - skew angle, outcome and threshold found for each test image (with default
    options and with -r auto) must be the same as in expected-detection.json,
  - decisions of the tool: input copied as is when nothing changes, output
    pixel format, size with and without cropping, detect only mode, work image,
    TIFF compression and resolution of the output,
  - failures (bad parameters, missing or unreadable input, missing DPI...) must
    end with a non-zero exit code and an error in the report.

Unlike the smoke scripts in Bin/ this checks what the tool did, not only that
it did not fail. Only the Python standard library is needed.

Usage: python3 Tests/cli/check_cli.py <deskew-executable> [--update]

--update writes the detection results of the given executable into
expected-detection.json instead of comparing with it. Use it only after an
intended change of the detection, and check the differences in git diff.
"""
import filecmp
import json
import os
import shutil
import struct
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
IMAGES = os.path.join(ROOT, 'TestImages')
EXPECTED = os.path.join(os.path.dirname(HERE), 'expected-detection.json')

# Option sets for the detection checks, also used by Tests/web/test-wasi.mjs
DETECTION_SETS = {'default': [], 'auto': ['-r', 'auto']}
ANGLE_TOLERANCE = 1e-6

# TIFF compression tag values
TIFF_LZW, TIFF_G4, TIFF_JPEG, TIFF_DEFLATE, TIFF_PACKBITS = 5, 4, 7, 8, 32773

failures = []
checks = 0


def check(cond, what):
    global checks
    checks += 1
    if not cond:
        failures.append(what)
        print('  FAIL:', what)
    return cond


def image(name):
    return os.path.join(IMAGES, name)


def test_images():
    return sorted(f for f in os.listdir(IMAGES) if os.path.isfile(image(f)))


class Run:
    """One run of the tool with -s j added, report parsed from standard output."""

    def __init__(self, exe, args):
        self.args = args
        p = subprocess.run([exe, '-s', 'j'] + args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        self.code = p.returncode
        self.stderr = p.stderr.decode('utf-8', errors='replace')
        try:
            self.report = json.loads(p.stdout.decode('ascii'))
        except Exception as e:
            self.report = None
            self.parse_error = '%s, stdout: %r' % (e, p.stdout[:300])

    @property
    def label(self):
        return ' '.join(os.path.basename(a.rstrip('/' + os.sep)) or a if os.sep in a or '/' in a else a
                        for a in self.args)

    def ok(self):
        """Checks that the run succeeded and the report was read."""
        if not check(self.report is not None, '%s: report is not JSON (%s)' %
                     (self.label, getattr(self, 'parse_error', ''))):
            return False
        return check(self.code == 0 and self.report.get('error') is None,
                     '%s: exit code %d, error: %s %s' %
                     (self.label, self.code, self.report.get('error'), self.stderr[-300:]))

    def get(self, path):
        value = self.report
        for key in path.split('.'):
            value = value.get(key) if isinstance(value, dict) else None
        return value

    def expect(self, path, expected):
        actual = self.get(path)
        check(actual == expected, '%s: %s is %r, expected %r' % (self.label, path, actual, expected))


def tiff_tags(path):
    """Tag values of the first IFD of a TIFF file: lists of ints, rationals as floats."""
    data = open(path, 'rb').read()
    e = '<' if data[:2] == b'II' else '>'
    ifd = struct.unpack(e + 'I', data[4:8])[0]
    count = struct.unpack(e + 'H', data[ifd:ifd + 2])[0]
    sizes = {1: 1, 3: 2, 4: 4, 5: 8}
    tags = {}
    for i in range(count):
        entry = ifd + 2 + i * 12
        tag, typ, cnt, val = struct.unpack(e + 'HHII', data[entry:entry + 12])
        if typ not in sizes:
            continue
        size = sizes[typ] * cnt
        raw = data[entry + 8:entry + 12] if size <= 4 else data[val:val + size]
        if typ == 1:
            tags[tag] = list(raw[:cnt])
        elif typ == 3:
            tags[tag] = list(struct.unpack(e + 'H' * cnt, raw[:2 * cnt]))
        elif typ == 4:
            tags[tag] = list(struct.unpack(e + 'I' * cnt, raw[:4 * cnt]))
        else:
            nums = struct.unpack(e + 'I' * (2 * cnt), raw[:8 * cnt])
            tags[tag] = [nums[k] / nums[k + 1] if nums[k + 1] else 0.0 for k in range(0, 2 * cnt, 2)]
    return tags


# ---------------------------------------------------------------- detection

def detection_results(exe):
    results = {}
    for set_name, options in DETECTION_SETS.items():
        results[set_name] = {}
        for name in test_images():
            r = Run(exe, options + ['-g', 'd', image(name)])
            if not r.ok():
                continue
            entry = {'skewAngle': r.get('detection.skewAngle'),
                     'outcome': r.get('detection.outcome'),
                     'threshold': r.get('detection.threshold')}
            if set_name == 'auto':
                entry['contentRect'] = r.get('detection.contentRect')
            results[set_name][name] = entry
    return results


def check_detection(exe):
    print('Detection results of the test images')
    with open(EXPECTED) as f:
        expected = json.load(f)
    actual = detection_results(exe)

    for set_name, options in DETECTION_SETS.items():
        exp_set, act_set = expected.get(set_name, {}), actual.get(set_name, {})
        for name in test_images():
            label = '%s %s' % (name, ' '.join(options) or '(default options)')
            exp, act = exp_set.get(name), act_set.get(name)
            if not check(exp is not None, label + ': no expected result, run with --update and check git diff'):
                continue
            if act is None:
                continue  # failed run, already reported
            angle_ok = (act['skewAngle'] is not None and
                        abs(act['skewAngle'] - exp['skewAngle']) <= ANGLE_TOLERANCE)
            check(angle_ok, '%s: skew angle %s, expected %s' % (label, act['skewAngle'], exp['skewAngle']))
            for key in exp:
                if key != 'skewAngle':
                    check(act.get(key) == exp[key], '%s: %s is %r, expected %r' % (label, key, act.get(key), exp[key]))
        for name in exp_set:
            check(name in act_set or name not in test_images(),
                  '%s: expected result for an image that is not in TestImages/' % name)


def update_expected(exe):
    results = detection_results(exe)
    if failures:
        print('Not updated, some runs failed')
        return
    with open(EXPECTED, 'w', newline='\n') as f:
        f.write(json.dumps(results, indent=2) + '\n')
    print('Written', EXPECTED)


# ---------------------------------------------------------------- decisions

def check_copy_when_unchanged(exe, tmp):
    print('Input copied as is when nothing changes')
    out = os.path.join(tmp, 'skip.jpg')
    r = Run(exe, ['-a', '5', '-l', '2', '-o', out, image('F1550.jpg')])
    if r.ok():
        r.expect('rotated', False)
        r.expect('output.written', 'copied')
        check(filecmp.cmp(out, image('F1550.jpg'), shallow=False), 'skip angle: output differs from the input')

    out = os.path.join(tmp, 'blank.png')
    r = Run(exe, ['-o', out, image('blank.png')])
    if r.ok():
        r.expect('detection.outcome', 'noContent')
        r.expect('detection.reliable', False)
        r.expect('rotated', False)
        r.expect('output.written', 'copied')
        check(filecmp.cmp(out, image('blank.png'), shallow=False), 'blank page: output differs from the input')

    out = os.path.join(tmp, 'blank-forced.png')
    r = Run(exe, ['-g', 'f', '-o', out, image('blank.png')])
    if r.ok():
        r.expect('rotated', True)
        r.expect('output.written', 'saved')

    # Conversion only: no rotation but another file format
    out = os.path.join(tmp, 'convert.png')
    r = Run(exe, ['-l', '90', '-o', out, image('1-lzw.tif')])
    if r.ok():
        r.expect('rotated', False)
        r.expect('output.written', 'saved')
        r.expect('output.format', 'Binary')
        check(open(out, 'rb').read(8) == b'\x89PNG\r\n\x1a\n', 'conversion: output is not PNG')

    out = os.path.join(tmp, 'same.tif')
    r = Run(exe, ['-l', '90', '-o', out, image('1-lzw.tif')])
    if r.ok():
        r.expect('output.written', 'copied')
        check(filecmp.cmp(out, image('1-lzw.tif'), shallow=False), 'same format: output differs from the input')


def check_output_formats(exe, tmp):
    print('Output pixel formats')
    cases = [
        # (input, options, expected output format)
        ('2.png', [], 'Gray8'),
        ('2.png', ['-b', 'FF0000'], 'R8G8B8'),       # gray image, color background
        ('2.png', ['-b', '80FFFFFF'], 'A8R8G8B8'),   # background with alpha
        ('2.png', ['-f', 'b1'], 'Binary'),
        ('1-lzw.tif', [], 'Gray8'),                  # 1-bit input is not saved as 1-bit
        ('1-lzw.tif', ['-f', 'b1'], 'Binary'),
        ('5.png', [], 'Gray8'),                      # gray palette
        ('6.png', [], 'R8G8B8'),                     # color palette
        ('6.png', ['-f', 'g8', '-b', '77'], 'Gray8'),
        ('6.png', ['-f', 'rgba32', '-b', '40ff00ff'], 'A8R8G8B8'),
        ('F1550.jpg', [], 'R8G8B8'),
    ]
    for i, (name, options, fmt) in enumerate(cases):
        out = os.path.join(tmp, 'format%d.png' % i)
        r = Run(exe, options + ['-o', out, image(name)])
        if r.ok():
            r.expect('output.written', 'saved')
            r.expect('output.format', fmt)
            check(os.path.isfile(out), '%s: no output file' % r.label)


def check_output_size(exe, tmp):
    print('Output size with and without cropping')
    r = Run(exe, ['-o', os.path.join(tmp, 'size.jpg'), image('F1550.jpg')])
    if r.ok():
        r.expect('rotated', True)
        check(r.get('output.width') > r.get('input.width') and r.get('output.height') > r.get('input.height'),
              '%s: rotated output is not larger than the input' % r.label)

    r = Run(exe, ['-g', 'c', '-o', os.path.join(tmp, 'crop.jpg'), image('F1550.jpg')])
    if r.ok():
        r.expect('rotated', True)
        r.expect('output.width', r.get('input.width'))
        r.expect('output.height', r.get('input.height'))


def check_detect_only(exe, tmp):
    print('Detect only mode, work image and report file')
    out = os.path.join(tmp, 'detect.png')
    r = Run(exe, ['-g', 'd', '-o', out, image('5.png')])
    if r.ok():
        r.expect('rotated', False)
        r.expect('output.written', 'none')
        r.expect('output.tiffCompression', None)

    r = Run(exe, ['-g', 'd', '-c', 'tinput', '-o', os.path.join(tmp, 'detect.tif'), image('1-g4.tif')])
    if r.ok():
        r.expect('parameters.tiffCompression', 'input')
        r.expect('output.tiffCompression', None)
        check(not os.path.exists(out), 'detect only: output file was written')

    folder = os.path.join(tmp, 'reports') + os.sep
    r = Run(exe, ['-g', 'd', '-s', 'rw', '-o', folder, image('3.png')])
    if r.ok():
        work = os.path.join(folder, '3.work.png')
        check(os.path.isfile(work), 'detect only: work image %s not written' % work)
        report_file = os.path.join(folder, '3.png.json')
        if check(os.path.isfile(report_file), 'detect only: report file %s not written' % report_file):
            with open(report_file) as f:
                file_report = json.load(f)
            # Timings differ between the two runs
            for rep in (file_report, r.report):
                rep.pop('timingsMs', None)
            check(file_report == r.report, 'report file differs from the report on standard output')


def check_tiff_output(exe, tmp):
    print('TIFF compression and resolution')
    cases = [
        # (input, options, expected compression, expected output format,
        #  parameters.tiffCompression (as given), output.tiffCompression (asked from the writer))
        ('1-lzw.tif', [], TIFF_LZW, 'Gray8', None, None),               # default compression
        ('1-lzw.tif', ['-f', 'b1'], TIFF_G4, 'Binary', None, None),      # 1-bit is always G4
        ('1-lzw.tif', ['-c', 'tdeflate'], TIFF_DEFLATE, 'Gray8', 'deflate', 'deflate'),
        ('1-lzw.tif', ['-c', 'trle'], TIFF_PACKBITS, 'Gray8', 'rle', 'rle'),
        ('1-lzw.tif', ['-b', 'DD', '-c', 'j95,tjpeg'], TIFF_JPEG, 'Gray8', 'jpeg', 'jpeg'),
        ('1-g4.tif', ['-t', '128', '-c', 'tinput'], TIFF_G4, 'Binary', 'input', 'g4'),
        ('tiff-jpeg.tif', ['-c', 'tinput'], TIFF_JPEG, 'R8G8B8', 'input', 'jpeg'),
        ('tiff-jpeg.tif', ['-c', 'tinput-lossless'], TIFF_LZW, 'R8G8B8', 'input-lossless', 'lzw'),
        ('2.png', ['-c', 'tinput'], TIFF_LZW, 'Gray8', 'input', None),    # not a TIFF input: default
    ]
    for i, (name, options, compression, fmt, given, asked) in enumerate(cases):
        out = os.path.join(tmp, 'tiff%d.tif' % i)
        r = Run(exe, options + ['-o', out, image(name)])
        if not r.ok():
            continue
        r.expect('output.format', fmt)
        r.expect('parameters.tiffCompression', given)
        r.expect('output.tiffCompression', asked)
        tags = tiff_tags(out)
        check(tags.get(259) == [compression], '%s: TIFF compression %s, expected %d' %
              (r.label, tags.get(259), compression))
        dpi = r.get('input.dpiX')
        if dpi is not None:
            res = tags.get(282, [0.0])[0]
            check(abs(res - dpi) < 0.5, '%s: output resolution %s, input has %s DPI' % (r.label, res, dpi))


def check_physical_units(exe, tmp):
    print('Margins in physical units')
    r = Run(exe, ['-g', 'd', '-m', '10,mm', image('3.png')])  # DPI from the image
    if r.ok():
        rect = r.get('detection.detectionRect')
        # 10 mm at 150 DPI is 59 px
        check(rect is not None and abs(rect[0] - 59) <= 1, '%s: detection rect %s' % (r.label, rect))

    r = Run(exe, ['-g', 'd', '-p', '300', '-m', '10,mm', image('2.png')])  # DPI given by the user
    if r.ok():
        rect = r.get('detection.detectionRect')
        check(rect is not None and abs(rect[0] - 118) <= 1, '%s: detection rect %s' % (r.label, rect))


def check_failures(exe, tmp):
    print('Failures')
    corrupt = os.path.join(tmp, 'corrupt.png')
    with open(corrupt, 'wb') as f:
        f.write(b'\x89PNG\r\n\x1a\n' + bytes((i * 7919) & 0xFF for i in range(5000)))
    text = os.path.join(tmp, 'text.png')
    with open(text, 'w') as f:
        f.write('not an image\n')
    out = os.path.join(tmp, 'failed.png')
    # A copy, a broken build must not overwrite the test image
    same = os.path.join(tmp, 'same.png')
    shutil.copyfile(image('2.png'), same)

    cases = [
        ('missing input', ['-o', out, image('does-not-exist.png')]),
        ('corrupt PNG', ['-o', out, corrupt]),
        ('text file', ['-o', out, text]),
        ('physical units without DPI', ['-m', '10,mm', '-o', out, image('2.png')]),
        ('unknown output format', ['-o', os.path.join(tmp, 'failed.xyz'), image('2.png')]),
        ('output is the input', ['-o', same, same]),
        ('bad parameter value', ['-a', 'abc', '-o', out, image('2.png')]),
        ('unknown parameter', ['-z', '1', '-o', out, image('2.png')]),
        ('no input', ['-o', out]),
    ]
    for what, args in cases:
        r = Run(exe, args)
        if not check(r.report is not None, '%s: report is not JSON (%s)' % (what, getattr(r, 'parse_error', ''))):
            continue
        check(r.code != 0, '%s: exit code 0' % what)
        check(bool(r.report.get('error')), '%s: no error in the report' % what)
        check(not os.path.exists(out) and not os.path.exists(os.path.join(tmp, 'failed.xyz')),
              '%s: output file was written' % what)
    check(filecmp.cmp(same, image('2.png'), shallow=False), 'output is the input: input file changed')


def main():
    args = [a for a in sys.argv[1:] if not a.startswith('--')]
    if len(args) != 1:
        print(__doc__)
        return 2
    exe = os.path.abspath(args[0])
    if not os.path.isfile(exe) and os.path.isfile(exe + '.exe'):
        exe += '.exe'
    if not os.path.isfile(exe):
        print('Executable not found:', exe)
        return 2
    print('Using', exe)

    if '--update' in sys.argv:
        update_expected(exe)
        return 1 if failures else 0

    tmp = tempfile.mkdtemp(prefix='deskew-check-')
    try:
        check_detection(exe)
        check_copy_when_unchanged(exe, tmp)
        check_output_formats(exe, tmp)
        check_output_size(exe, tmp)
        check_detect_only(exe, tmp)
        check_tiff_output(exe, tmp)
        check_physical_units(exe, tmp)
        check_failures(exe, tmp)
    finally:
        shutil.rmtree(tmp, ignore_errors=True)

    print()
    if failures:
        print('%d of %d checks FAILED' % (len(failures), checks))
        return 1
    print('All %d checks passed' % checks)
    return 0


if __name__ == '__main__':
    sys.exit(main())
