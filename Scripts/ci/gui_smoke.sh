#!/bin/bash
# Smoke test of the GUI builds: starts each GUI executable given, waits until its
# main window is shown, saves a screenshot of the screen and stops the GUI again
# (killed, so no settings are saved). Fails when a GUI ends or shows no window
# within the time limit. Catches startup crashes and missing shared libraries
# (e.g. libQt6Pas), not problems inside the GUI.
#
# Linux: needs an X display (run under xvfb-run), xdotool and ImageMagick (import).
#        A window manager (openbox) is started when found, Qt dialogs are not
#        painted under bare Xvfb.
# macOS: needs a logged in desktop session (CI runners have one) and swiftc to count
#        the windows of the process. Screenshot is skipped if not permitted.
#
# Usage: Scripts/ci/gui_smoke.sh <output-dir> <gui-executable>...
set -u

if [ $# -lt 2 ]; then
    echo "Usage: $0 <output-dir> <gui-executable>..."
    exit 2
fi

OUT_DIR=$1
shift
mkdir -p "$OUT_DIR"
TIMEOUT=30
OS=$(uname -s)

if [ "$OS" == "Darwin" ]; then
    # Number of on screen windows of a process (no permission needed for that)
    WINCOUNT="$OUT_DIR/wincount"
    cat > "$OUT_DIR/wincount.swift" <<'EOF'
import CoreGraphics
let pid = Int(CommandLine.arguments[1])!
let windows = CGWindowListCopyWindowInfo([.optionOnScreenOnly], kCGNullWindowID) as? [[String: Any]] ?? []
print(windows.filter { ($0[kCGWindowOwnerPID as String] as? Int) == pid }.count)
EOF
    swiftc -O -o "$WINCOUNT" "$OUT_DIR/wincount.swift" || exit 1
    rm "$OUT_DIR/wincount.swift"
elif [ -n "${DISPLAY:-}" ] && command -v openbox > /dev/null; then
    openbox > "$OUT_DIR/openbox.log" 2>&1 &
    WM_PID=$!
    sleep 1
fi

has_window() {
    if [ "$OS" == "Darwin" ]; then
        [ "$("$WINCOUNT" "$1" 2> /dev/null || echo 0)" -gt 0 ]
    else
        [ -n "$(xdotool search --onlyvisible --name '^Deskew GUI' 2> /dev/null)" ]
    fi
}

take_screenshot() {
    if [ "$OS" == "Darwin" ]; then
        screencapture -x "$1" 2> /dev/null || echo "  (no screenshot: not permitted)"
    else
        import -window root "$1"
    fi
}

FAILED=0
for GUI in "$@"; do
    NAME=$(basename "$GUI")
    LOG="$OUT_DIR/$NAME.log"
    echo "Starting $GUI"
    "$GUI" > "$LOG" 2>&1 &
    PID=$!

    SHOWN=0
    for ((I = 0; I < TIMEOUT * 2; I++)); do
        sleep 0.5
        if ! kill -0 $PID 2> /dev/null; then
            break
        fi
        if has_window $PID; then
            SHOWN=1
            break
        fi
    done

    if [ $SHOWN == 1 ]; then
        # Let it paint
        sleep 2
        take_screenshot "$OUT_DIR/$NAME.png"
        echo "  OK, main window shown"
    elif kill -0 $PID 2> /dev/null; then
        take_screenshot "$OUT_DIR/$NAME-timeout.png"
        echo "  FAILED: no window within $TIMEOUT s"
        FAILED=1
    else
        wait $PID
        echo "  FAILED: ended with exit code $? before showing a window"
        FAILED=1
    fi

    if kill -0 $PID 2> /dev/null; then
        kill $PID
        wait $PID 2> /dev/null
    fi
    if [ $SHOWN == 0 ] && [ -s "$LOG" ]; then
        echo "  Output:"
        sed 's/^/    /' "$LOG"
    fi
done

if [ -n "${WM_PID:-}" ]; then
    kill $WM_PID 2> /dev/null
fi
rm -f "${WINCOUNT:-}"
exit $FAILED
