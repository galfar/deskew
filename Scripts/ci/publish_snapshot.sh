#!/bin/bash
# Publishes the development archives of a CI run as a rolling GitHub pre-release
# (default tag "snapshot"): the tag is moved to the built commit, the release is
# created the first time and its text updated, new archives are uploaded and the
# old ones deleted afterwards (the release is never empty).
#
# Usage: Scripts/ci/publish_snapshot.sh <tag> <folder with the archives>
#
# Needs gh with GH_TOKEN (contents: write), GH_REPO, GITHUB_SHA, GITHUB_REF and a
# checkout whose git can push tags. The archives must be the dev ones
# (deskew-<version>-dev-<date>-<target>.zip, Scripts/ci/release_suffix.sh), one
# for each target in SNAPSHOT_TARGETS, otherwise nothing is published.
set -eu

TAG="${1:?tag}"
DIR="${2:?folder with the archives}"
TARGETS="${SNAPSHOT_TARGETS:-win-x64 win-x86 linux-x64 linux-arm64 macos-arm64 macos-x64 web}"

: "${GH_REPO:?}" "${GITHUB_SHA:?}" "${GITHUB_REF:?}"

case "$TAG" in
  v[0-9]*) echo "ERROR: $TAG looks like a release tag" >&2; exit 1 ;;
esac
# The real snapshot comes from master only, tests use another tag
if [ "$TAG" = "snapshot" ] && [ "$GITHUB_REF" != "refs/heads/master" ]; then
  echo "ERROR: tag snapshot is published from master only (this is $GITHUB_REF)" >&2
  exit 1
fi

# --- Archives: exactly one per target, all of the same dev version.
# A downloaded artifact can get .zip appended to its name (deskew-...zip.zip): renamed back.
find "$DIR" -type f -name 'deskew-*.zip.zip' | while read -r f; do mv "$f" "${f%.zip}"; done
FILES=()
VERSION=
for t in $TARGETS; do
  found=( $(find "$DIR" -type f -name "deskew-*-dev-*-$t.zip") )
  if [ "${#found[@]}" -ne 1 ]; then
    echo "ERROR: expected one dev archive for $t, found ${#found[@]}: ${found[*]:-}" >&2
    exit 1
  fi
  f="${found[0]}"
  name="$(basename "$f" .zip)"
  v="${name#deskew-}"; v="${v%-$t}"
  if [ -n "$VERSION" ] && [ "$v" != "$VERSION" ]; then
    echo "ERROR: archives of different versions: $VERSION and $v" >&2
    exit 1
  fi
  VERSION="$v"
  FILES+=("$f")
done
echo "Snapshot $VERSION of $GITHUB_SHA:"
printf '  %s\n' "${FILES[@]}"

# --- Tag
git tag -f "$TAG" "$GITHUB_SHA"
git push -f origin "refs/tags/$TAG"

# --- Release text
NOTES="$(mktemp)"
SHORT="${GITHUB_SHA:0:10}"
cat > "$NOTES" <<EOF
**Development build, not a release.** Archives of the latest successful CI build of \`master\`,
tested by CI but not as much as the releases. For the latest release see [Releases](https://github.com/$GH_REPO/releases/latest).

- Version: $VERSION
- Commit: [$SHORT](https://github.com/$GH_REPO/commit/$GITHUB_SHA)
- Built: $(date -u '+%Y-%m-%d %H:%M UTC')

What changed since the last release: [Docs/History.md](https://github.com/$GH_REPO/blob/$GITHUB_SHA/Docs/History.md) and the [commits](https://github.com/$GH_REPO/commits/$GITHUB_SHA).

The archives are the same as in the releases: one per platform with the command line tool, the GUI, documentation and examples, 
and the WebUI archive (\`-web\`).
EOF

TITLE="Development snapshot"
if gh release view "$TAG" > /dev/null 2>&1; then
  gh release edit "$TAG" --title "$TITLE" --notes-file "$NOTES" --prerelease
else
  gh release create "$TAG" --title "$TITLE" --notes-file "$NOTES" --prerelease --latest=false --verify-tag
fi

# --- Archives: upload the new ones, then delete what is not among them
gh release upload "$TAG" "${FILES[@]}" --clobber
NEW_NAMES=" $(for f in "${FILES[@]}"; do basename "$f"; done | tr '\n' ' ') "
gh release view "$TAG" --json assets --jq '.assets[].name' | while read -r asset; do
  case "$NEW_NAMES" in
    *" $asset "*) ;;
    *) echo "Deleting old $asset"; gh release delete-asset "$TAG" "$asset" -y ;;
  esac
done

echo "Published https://github.com/$GH_REPO/releases/tag/$TAG"
