#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
out_dir="${1:-$repo_root/target/design-gallery}"

cd "$repo_root"
sbt --error "designJVM / Test / runMain scalafim.fmri.design.DesignGalleryRender $out_dir"

printf 'design gallery: %s\n' "$out_dir/index.html"
