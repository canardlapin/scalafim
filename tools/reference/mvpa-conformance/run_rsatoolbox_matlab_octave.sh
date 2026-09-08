#!/usr/bin/env bash
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
octave_bin="${OCTAVE_BIN:-octave}"

if [[ -z "${RSA_TOOLBOX_MATLAB_ROOT:-}" ]]; then
  echo "RSA_TOOLBOX_MATLAB_ROOT must name the pinned rsatoolbox_matlab checkout" >&2
  exit 2
fi

fixture="$(mktemp "${TMPDIR:-/tmp}/scalafim-mvpa-fixture.XXXXXX.json")"
trap 'rm -f "$fixture"' EXIT
python3 "$here/fixture.py" >"$fixture"

export SCALAFIM_MVPA_FIXTURE="$fixture"
export SCALAFIM_MVPA_MATLAB_RUNNER="$here/rsatoolbox_matlab_reference.m"

"$octave_bin" --quiet --eval \
  "function import(varargin); endfunction; run(getenv('SCALAFIM_MVPA_MATLAB_RUNNER'));"
