# Local commit qualification — 10 September 2026

An isolated candidate based on ScalaFIM
`99c04035660dfcb5108289ec83367d8fc83d5da5` contains exactly the 67 source/build/test
paths recorded in [the continuation manifest](continuation-source-manifest.json).
Their hashes (including recorded deletions) were checked against the working tree
before staging. No local provider override was used. Full raw command output is
retained in [the compressed receipt](commit-checks.json.gz).

`scalafimCompileAll` passed without warnings. Bounded, separate sbt invocations
passed these tests on both platforms:

| Module | JVM | Scala.js |
|---|---:|---:|
| fit | 315 | 304 |
| estimates | 10 | 10 |
| estimates-io | 9 | 3 |
| group | 49 | 48 |
| fit-estimates | 11 | 8 |
| Total | 394 | 373 |

This is full cross-platform compilation plus focused tests, not the entire
repository test suite. Newer concurrent profile-fit work is outside the tested
base and the scoped commit. In particular, the additive `b312115` output-request
commit landed while these checks ran; its two files are not part of this change.

The earlier [64 MiB heap and relocation proof](continuation-heap-and-relocation.json)
is retained, not rerun at this checkpoint. It checked a 128 MiB numerical payload
with an independent all-values Python read and a separate fitter-free reader.
The exact estimate implementation source still matches that evidence manifest.
This does not qualify the full covariance/cohort access or crash-durability matrix.

See [the plan assessment](../../plans/estimate-set-implementation.md#commit-checkpoint-assessment--10-september-2026)
for stage status and the next milestone. The persisted schema remains explicitly
a development profile; this commit does not declare V0 conformance or completion
of the exporter/application replacement.
