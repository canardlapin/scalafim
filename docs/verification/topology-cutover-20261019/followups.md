# Eco follow-ups, 2026-10-19

The topology cutover at `56ca75257` now passes against published mesh4s
`54b7278` without a source override: surface JVM 320 passed; JS 277 passed,
2 existing opt-in skips. Runtime property checks and staged dependency paths
confirm provider resolution. The earlier Opus approval is bound to that
candidate; it is not a review of the subsequent commits below.

`6860aef0b` replaces materialized neighbor collections in component traversal
and harmonic lens updates with mesh4s iteration, reusing callbacks. Provider
neighbor rows are sorted, preserving floating-point summation order. Point-map
placement uses `withCoordinates`, retaining the exact topology/vertex owner.
An independent union-find oracle covers all 128 active masks on a fragmented
mesh. Full surface and surface-view suites passed on both platforms: 1,111
passed, 2 existing skips. Existing coordinate-owner, pointwise bridge, frozen
surface-chain, persistence and harmonic reveal checks remain in those suites.
No real-scale traversal allocation or speed bound is claimed.

`ab800a166` removes 497 lines: unused ScalaFIM HalfFlow local-statistics and
validity copies, their three unit tests, and unused searchlight policy enums
and their unused error branch. A pre-deletion source-reference audit found
all references confined to the four changed files; none remain afterwards.
The HalfFlow implementations remain in reframe4s. The removed APIs have no
production consumers; there is no algorithm substitution. The neuroim2 parity
APIs designated for neuroim4s, active axis/dimension types, and historical
qualification receipts remain untouched.

After deletion, image JVM 352 tests and JS 322 tests passed; repository-wide
`scalafimCompileAll` passed on both platforms without warnings. The six fewer
image test executions are the removed API's three tests on each platform;
no surviving assertion, tolerance or fixture was weakened. The follow-up test
and compile logs emitted no warnings. The published-pin startup separately
reported existing linops4s build-definition and Intaglio unused-key warnings.

Exact SHAs, commands, test counts and log digests are in
[followups.json](followups.json). Raw logs, the pre-deletion audit and the
verified build-input manifest are retained in the common Git directory under
`verification/topology-cutover-20261019/`. The warm server was shut down after
the gates. ScalaFIM remains unpublished; hosted CI and native requalification
were not run.

Topology acceptance is complete in Mote; publication is separate under the
epic contract. U3 still requires generic geodesic/sphere/index extraction and
consumer deletion/edge-weight ordering work. The neuroim provider task still
requires image4s components, locus4s neighborhoods, Gale operations and the
remaining caller cleanup. These broader tasks and simplification A/B remain
unfinished.
