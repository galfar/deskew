#!/bin/bash
set -eu
# enable /** globbing
shopt -s globstar

echo "Deleting ugly files..."

ROOTDIR="$(dirname "$0")/.."

PATTERNS="*.ppu *.dcu *.o link????.res ppas.sh *.rsj"

delInTree() {
  for PAT in $2; do
    find $1 -mindepth 1 -iname "$PAT" -type f | xargs rm -f
  done
}

delInTree "$ROOTDIR" "$PATTERNS"