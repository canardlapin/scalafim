# Controlled Linux baseline and observations

[Run 37505102238](https://github.com/canardlapin/scalafim/actions/runs/37505102238)
ran the unchanged compact-condition suite from commit 37a84e99 in separate JVM
and Scala.js processes, then ran observation copies with the same assertions.

| Suite | JVM | Scala.js |
| --- | --- | --- |
| Unmodified baseline | 24/24 admitted; exit 0 | 21/24 admitted; exit 1 |
| Observation capture | 24/24 admitted; exit 0 | 21/24 admitted; exit 1 |

The admission requirement remains at least 22/24. Accuracy, stationarity and
work limits are unchanged. The workflow collects failed tests with
`continue-on-error`; its successful conclusion does **not** mean scientific
qualification passed. Production decoder code is unchanged by this capture.

Actual host: Ubuntu 24.04.5 x86_64, Temurin 17.0.20.1+1, Node 24.21.0,
V8 13.6.233.17-node.53 and glibc 2.39. The requested historical JDK version
resolved to this newer patch binary; the retained `java -version` fingerprint
is authoritative.

[First run 37503330869](https://github.com/canardlapin/scalafim/actions/runs/37503330869)
captured the observations, but its baseline step failed because shallow checkout
lacked commit 37a84e99. The corrected workflow fetches history. Both runs'
114 JVM and 185 Scala.js observation records match exactly. This agreement
does not prove observations harmless across all runtimes: an earlier Darwin
capture changed admission counts. Baseline and diagnostic results remain distinct.

The archive retains both runs, raw scientific exit codes and logs, source hashes,
actual hosts, workflow, observation sources and the original baseline suite.
`receipt.json` seals its digest. Independent residual sign analysis and any
production repair require separate qualification.

Verify without network access: `python3 -S verify.py`.
