#!/usr/bin/env bash
set -euo pipefail

FRAME_REPO="$(cd "$(dirname "$0")/.." && pwd)"
FRAME_TMP="$(mktemp -d "${TMPDIR:-/tmp}/frame4s-rehearsal.XXXXXX")"
trap 'rm -rf "$FRAME_TMP"' EXIT

mkdir -p "$FRAME_TMP/modules/frame" "$FRAME_TMP/modules/frame-fs2" "$FRAME_TMP/project"
cp -R "$FRAME_REPO/modules/frame/shared" "$FRAME_REPO/modules/frame/jvm" \
  "$FRAME_REPO/modules/frame/js" "$FRAME_TMP/modules/frame/"
cp -R "$FRAME_REPO/modules/frame-fs2/shared" "$FRAME_REPO/modules/frame-fs2/jvm" \
  "$FRAME_REPO/modules/frame-fs2/js" "$FRAME_TMP/modules/frame-fs2/"
cp "$FRAME_REPO/project/build.properties" "$FRAME_REPO/project/plugins.sbt" \
  "$FRAME_TMP/project/"

find "$FRAME_TMP/modules" -type f -name '*.scala' -print0 |
  xargs -0 perl -pi -e 's/scalafim\.frame/frame4s/g; s/private\[frame\]/private[frame4s]/g'

if rg --pcre2 -n 'scalafim\.(?!frame)' "$FRAME_TMP/modules" --glob '*.scala'; then
  echo "Frame extraction failed: an internal ScalaFIM dependency remains." >&2
  exit 1
fi

cp "$FRAME_REPO/tools/frame-standalone-build.sbt" "$FRAME_TMP/build.sbt"

(
  cd "$FRAME_TMP"
  sbt \
    'frameJVM/test' \
    'frameJS/test' \
    'frameFs2JVM/test' \
    'frameFs2JS/test'
)
