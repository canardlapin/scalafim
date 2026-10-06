# Prepared final qualification workflow

Preparation only; final frozen LWU and seven precision files are now copied
byte-identically from ROOT. `final-inputs.json` binds their hashes and the published
Gale URI. No commit or publication has been performed by this task.

The final candidate must use published Gale
`b56a9dd0b8ad479621a3594880f90c9add8c2824` through the ordinary committed URI.
No overlay, sibling checkout or provider system-property override is accepted.

Two native Ubuntu 24.04 x64 jobs qualify JVM and Scala.js separately, each with
fresh sbt processes for budget/kernel tests, the unchanged compact cohort,
exactly the original LWU accuracy test and full fit tests. Node is pinned to
24.21.0. The historical Temurin 17.0.20+1 is attempted first; any fallback to the
declared JDK 17 baseline is recorded in host evidence. Each job is bounded at
45 minutes; the inherited LWU suite timeout remains 60 minutes, with no separate
15-minute wall timeout truncating that scientific gate.

The LWU selector inherits the original test and asserts its exact name/count.
It retains 100 samples at SNR 1.0 and 0.5, seeds 111 and 112, oracle grid
26 x 11 x 9 and the original latency, FWHM and amplitude assertions. It runs
through a temporary scoped Test/unmanagedSources setting, preserving original
library test source and excluding the broad throughput spike. LWU admission
below 95% remains the original reported caveat; no new admission claim is made.

Every stage log and exit code is retained, including failures. These prepared
files have passed shell/Python syntax and workflow parsing checks; scientific
tests and hosted publication have not been performed by this preparation task.
