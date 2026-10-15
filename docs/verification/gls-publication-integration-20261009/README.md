# GLS publication integration — 2026-10-09

Remote main and the local validated PHRF/GLS work merged cleanly. The merge
preserves both histories and includes no manual source conflict resolution.

Both JVM and Scala.js HRF, design, model and fit suites passed, along with the
27-test GLS factor pilot on each platform. All 384 pilot fit outputs/platform
reproduce the committed follow-up study within 1e-9 scaled error, with identical
rejection, coverage and residual-df decisions. Exact observed error, test
commands, parent revisions and validated code-tree hash are in checks.json.

The Scala.js fit link exhausted the accumulated 3g sbt heap after earlier
suites. Only the integration build processes were stopped. Fit and the pilot
then passed in separate fresh 6g servers; completed gates were retained.
This infrastructure recovery did not change source or scientific criteria.

These are software/integration regression checks. The original and longer-run
voxelwise scientific failures remain binding in their existing receipts.
