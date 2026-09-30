#!/usr/bin/env bash
# Writer acceptance (STP P5.03): render ScalaFIM's writer goldens, have native tools read them, record a receipt.
# Re-run whenever a transform writer changes; WriterGoldensSuite fails until goldens and receipt are refreshed.
set -euo pipefail
cd "$(dirname "$0")/../.."
GOLDENS=modules/transform/shared/src/test/resources/scalafim/transform/oracle/writer_goldens
sbt -batch "transformJVM/Test/runMain scalafim.transform.WriterGoldens $GOLDENS"
uv run --with SimpleITK==2.5.6 --with nibabel==5.4.2 --with fslpy==3.29.1 --with nitransforms==25.1.0 --with h5py==3.16.0 --with numpy \
  python tools/transform/writer_acceptance.py "$GOLDENS"
