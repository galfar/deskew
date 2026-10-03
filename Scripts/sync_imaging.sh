#!/usr/bin/env bash
#
# Keeps Imaging/ (bundled Vampyre Imaging Library) in sync with the imaginglib repository
# https://github.com/galfar/imaginglib. Imaging/ holds an unmodified copy of the units Deskew
# needs, taken from a single imaginglib commit, which is recorded in Imaging/IMAGING_SYNC.txt.
# Do not edit files in Imaging/ by hand; Deskew-specific settings belong in ImagingUserOptions.inc.
#
# Usage:
#   sync_imaging.sh update <ref> [--repo <path>]
#   sync_imaging.sh check [--repo <path>]
#
# Commands:
#   update <ref>   Replace Imaging/ with the files from <ref> and record the commit.
#                  <ref> is anything Git can fetch from imaginglib: a branch (develop),
#                  a tag, or a commit hash.
#   check          Verify Imaging/ is identical to the commit recorded in IMAGING_SYNC.txt
#                  (exit code 1 and a diff if not). Used by CI.
#
# Options:
#   --repo <path>  Take files from a local imaginglib clone instead of downloading from GitHub,
#                  e.g. to try Imaging changes that are not pushed yet, or to work offline.
#                  <ref> is then resolved in that clone (e.g. origin/develop, or a local branch).
#
# Updating Imaging to the latest develop:
#   Scripts/sync_imaging.sh update develop
#   Build and run tests, then commit Imaging/ (with IMAGING_SYNC.txt) in one commit.
#
set -euo pipefail

REPO_URL="https://github.com/galfar/imaginglib"
ROOT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)"
DEST_DIR="$ROOT_DIR/Imaging"
SYNC_FILE="IMAGING_SYNC.txt"

usage() {
  sed -n '2,/^set -euo/p' "${BASH_SOURCE[0]}" | grep '^#' | sed 's/^# \{0,1\}//'
  exit 2
}

COMMAND="${1:-}"
[[ "$COMMAND" == "update" || "$COMMAND" == "check" ]] || usage
shift

REF=""
LOCAL_REPO=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --repo)
      [[ $# -ge 2 ]] || usage
      LOCAL_REPO="$2"
      shift 2 ;;
    -*)
      usage ;;
    *)
      [[ -z "$REF" ]] || usage
      REF="$1"
      shift ;;
  esac
done

if [[ "$COMMAND" == "update" ]]; then
  [[ -n "$REF" ]] || usage
else
  [[ -z "$REF" ]] || usage
  [[ -f "$DEST_DIR/$SYNC_FILE" ]] || { echo "ERROR: $DEST_DIR/$SYNC_FILE not found" >&2; exit 1; }
  REF="$(sed -n 's/^commit: *//p' "$DEST_DIR/$SYNC_FILE")"
  [[ -n "$REF" ]] || { echo "ERROR: no commit recorded in $SYNC_FILE" >&2; exit 1; }
fi

TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

# Repository to read the files from: a local clone, or a shallow fetch of just <ref> from GitHub
if [[ -n "$LOCAL_REPO" ]]; then
  SRC_REPO="$LOCAL_REPO"
  COMMIT="$(git -C "$SRC_REPO" rev-parse --verify "$REF^{commit}")"
else
  SRC_REPO="$TMP_DIR/repo"
  git init -q "$SRC_REPO"
  echo "Fetching '$REF' from $REPO_URL ..."
  git -C "$SRC_REPO" fetch -q --depth 1 "$REPO_URL" "$REF"
  COMMIT="$(git -C "$SRC_REPO" rev-parse --verify "FETCH_HEAD^{commit}")"
fi

SRC="$TMP_DIR/src"
OUT="$TMP_DIR/Imaging"
mkdir -p "$SRC" "$OUT/Libs" "$OUT/LibTiff/Compiled"

# Exact file contents as stored in the repository (line endings, BOMs)
git -C "$SRC_REPO" archive "$COMMIT" Source Extensions/LibTiff \
  Extensions/ImagingExtFileFormats.pas Extensions/ImagingTiff.pas Extensions/ImagingPsd.pas \
  | tar -x -C "$SRC"

# Core units Deskew does not use (keep in line with ImagingUserOptions.inc):
#   ImagingComponents.pas - LCL/VCL components
#   ImagingRadiance.pas   - Radiance HDR format (DONT_LINK_RADHDR)
SKIP_CORE_UNITS=" ImagingComponents.pas ImagingRadiance.pas "

for F in "$SRC"/Source/*.pas "$SRC"/Source/*.inc; do
  [[ "$SKIP_CORE_UNITS" == *" $(basename "$F") "* ]] && continue
  cp "$F" "$OUT/"
done
cp "$SRC"/Source/Libs/* "$OUT/Libs/"

# Extension formats Deskew links: TIFF and PSD
cp "$SRC"/Extensions/ImagingExtFileFormats.pas "$SRC"/Extensions/ImagingTiff.pas \
   "$SRC"/Extensions/ImagingPsd.pas "$OUT/"
cp "$SRC"/Extensions/LibTiff/*.pas "$OUT/LibTiff/"
cp "$SRC"/Extensions/LibTiff/Compiled/* "$OUT/LibTiff/Compiled/"

printf 'Synced by Scripts/sync_imaging.sh, do not edit files in this folder.\nrepo: %s\ncommit: %s\n' \
  "$REPO_URL" "$COMMIT" > "$OUT/$SYNC_FILE"

if [[ "$COMMAND" == "check" ]]; then
  if diff -r "$OUT" "$DEST_DIR"; then
    echo "Imaging/ matches imaginglib commit $COMMIT"
  else
    echo "ERROR: Imaging/ differs from imaginglib commit $COMMIT (see diff above)" >&2
    exit 1
  fi
else
  rm -rf "$DEST_DIR"
  mv "$OUT" "$DEST_DIR"
  echo "Imaging/ synced from imaginglib commit $COMMIT"
fi
