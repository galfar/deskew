#!/bin/bash
# Sets DESKEW_RELEASE_SUFFIX for the release archive names of a CI run
# (Scripts/get_versions.sh appends it to the version: RELEASE_VERSION):
#   v* tag:      empty, archives are deskew-<version>-<target>.zip. The tag must be
#                the version in the sources (vX.YY = SAppVersion), the run fails otherwise.
#   other runs:  dev-<date of the commit, YYYYMMDD>, archives are
#                deskew-<version>-dev-<date>-<target>.zip, so they cannot be mistaken
#                for the release of that version
#
# Written to $GITHUB_ENV for the following steps of the job (and printed).
# Local runs of the release scripts do not use it: no suffix there.
set -eu

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/Scripts/get_versions.sh"

REF="${GITHUB_REF:-}"
case "$REF" in
  refs/tags/v*)
    TAG="${REF#refs/tags/}"
    if [ "$TAG" != "v$CLI_VERSION" ]; then
      echo "ERROR: tag $TAG does not match the version in MainUnit.pas ($CLI_VERSION)" >&2
      exit 1
    fi
    SUFFIX="" ;;
  *)
    SUFFIX="dev-$(git -C "$ROOT" log -1 --format=%cd --date=format:%Y%m%d)" ;;
esac

echo "DESKEW_RELEASE_SUFFIX=$SUFFIX"
if [ -n "${GITHUB_ENV:-}" ]; then
  echo "DESKEW_RELEASE_SUFFIX=$SUFFIX" >> "$GITHUB_ENV"
fi
