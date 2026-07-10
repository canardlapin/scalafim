#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
out_dir="${1:-$repo_root/target/graphics-gallery}"

cd "$repo_root"
sbt --error 'graphicsSvgJVM / Test / compile' 'designJVM / Compile / compile'

classpaths="$(sbt --error 'export graphicsSvgJVM / Test / fullClasspath' 'export designJVM / Compile / fullClasspath')"
graphics_classpath="$(printf '%s\n' "$classpaths" | sed -n '1p')"
design_classpath="$(printf '%s\n' "$classpaths" | sed -n '2p')"

runner_args=("$out_dir")
if [[ "${SCALAFIM_GALLERY_REQUIRE_DESIGN:-0}" == "1" ]]; then
  runner_args+=("--require-design")
fi

java -cp "$graphics_classpath:$design_classpath" \
  scalafim.graphics.svg.GalleryRender "${runner_args[@]}"
