#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo_root"

export LC_ALL=C
export LANG=C
export TZ=UTC

case "${1:-}" in
  "")
    target=all
    ;;
  --r-only)
    target=r
    ;;
  --python-only)
    target=python
    ;;
  *)
    echo "usage: $0 [--r-only|--python-only]" >&2
    exit 64
    ;;
esac

# Live regeneration must see the locked packages installed in this environment.
# Dependency-free PR checks invoke check_receipts.py with -S separately.
python3 tools/r-parity/check_receipts.py --regenerate "$target"
python3 -S tools/scenarios/validate_manifest.py docs/scenarios/manifest.json
