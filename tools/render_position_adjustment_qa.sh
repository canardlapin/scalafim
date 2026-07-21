#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
out_dir="${1:-$repo_root/target/graphics-position-qa}"

cd "$repo_root"
Rscript tools/r-parity/render_graphics_position_reference.R "$out_dir/ggplot2"
sbt --error "graphicsJava2dJVM / Test / runMain scalafim.graphics.java2d.PositionVisualQa $out_dir"

printf 'visual QA: %s\n' "$out_dir/index.html"
