# Numerical ownership continuation

Mote: `bd-01KXYJPWYV08VFWRMCYYDPZCAV`.
ScalaFIM base: `62312f5d` (includes the landed RSA packet).
Worktree: `/private/tmp/scalafim-numerical-ownership-20261003`.
Gale provider issue: `bd-01M427PJCGSXP2VCCE97EEMT5T`.
Gale candidate base: `18d24dbb5056122032b0278f8bad557a9bb1cf23`.

This continuation removes the remaining polynomial QR and profile Cholesky
implementations from ScalaFIM in a local candidate. Generic factorization and
execution workspace ownership belong to Gale; ScalaFIM retains polynomial
conventions, profile derivatives, curvature interpretation and failure policy.
The provider capability is not yet published. The HRF representation migration
and its consumer and measurement gates remain open.

## Polynomial QR

`ParametricBasis.Poly.fit` uses Gale unpivoted QR and applies Q to the signed
diagonal of R. Multiplying by that signed diagonal preserves the R polynomial
basis convention despite a factorization's arbitrary column signs. Pivoting is
disabled because the columns are ordered polynomial powers. The existing R
4.5.1 fixture checks basis values, alpha and norm2; additional tests check
orthonormality, training prediction, representable unit changes and insufficient
unique inputs. No conditioning or allocation improvement is claimed.

## Profile Cholesky

The proposed Gale `DenseCholeskyWorkspace` owns reusable factor and RHS scratch.
It uses the same lower-triangle kernel as immutable Gale Cholesky. Its array
span API is an execution interface, not a second public matrix representation.
Caller storage is read completely before output is committed. Failure leaves
caller arrays unchanged, upper triangles are ignored, and overlapping inputs
are supported. Finite lower factors and positive diagonal entries are required
for a solve. Invalid spans, non-finite data and non-finite computed solutions
are refused explicitly. The shared kernel also tightens immutable Cholesky's
handling of non-finite pivots and arithmetic overflow.

`ProfileReduction`, `GramConditionObjective`, `CompactConditionObjective`,
`ShapeDecoder` and `TrialBandedObjective` use this capability. Cached-node
scoring reuses a solution array and passes the stored factor's offset rather
than allocating factor and RHS copies. Positive-definiteness classification
uses singleton cases, including the ordinary indefinite-curvature path.
Refused inputs may allocate typed Gale errors. `CurvatureStatus.NumericalFailure`
distinguishes a non-finite solve/reduction from a non-positive Gram matrix;
failed reductions clear all output arrays and set energy to positive infinity.

Provider tests use an independently specified exact integer factor, solution
and RHS, at scales 1e-150, 1 and 1e150. They also cover offsets, ignored upper
garbage, overlap, transactional failures, reuse, empty shapes, invalid spans,
capacity bounds, tolerance and overflow. The profile finite-difference oracle
now uses Gale pivoted LU rather than the Cholesky implementation under test.
An explicit invalid-input/recovery case exercises output clearing.

A read-only review found no blocker in the workspace ownership, span/alias
handling, cached factor offsets or profile failure paths. That review did not
execute tests or establish allocation measurements. A JVM per-thread allocation
probe is included for successful positive and indefinite reductions; its scope
does not cover decoder construction, failed inputs, all node-scoring paths or JS.

## HRF S0 and remaining stages

New shared characterization tests freeze current Mat/Vec aliasing, mutable data,
array-identity equality and detached-copy behavior, plus an existing R HRF
fixture. These describe the old representation, not desired ownership semantics.
A bounded Scala.js direct-convolution timing harness checks a stable checksum.
No production HRF representation or external consumer has changed.

The [v4 contract](../plans/hrf-matvec-contract-v4.md) remains authoritative for
staging: S0 measurements precede representation changes; S4 uses the fixed
paired non-inferiority protocol; S5 requires staged Eidolon and PLS Neuro
consumer evidence. Initial host checks failed the load <=2 requirement; the
prospective host-specific amendment described below permits <=5 on buc-gw01.
No valid baseline or HRF performance admission is claimed while the run is active.
ScalaFIM remains a general library: consumer applications are compatibility
evidence, not owners of generic APIs or scientific semantics.

## Verification status

Polynomial commit `9b380f9c` passed `designJVM/test` and `designJS/test`, each
with 269 tests and no compiler warnings, on Homebrew JDK 25.0.1 and sbt 1.11.7.
The compressed raw log, exit status and source hashes are retained under
[evidence](evidence/numerical-ownership-continuation/). This owning-module
qualification does not establish any performance improvement.

Gale candidate `3172e80b9512196df156738ba2183ebf17dea074` passed `coreJVM/test`
(666 tests), `coreJS/test` (656), `lawsJVM/test` (54), `lawsJS/test` (54), and
`scalafmtCheckAll`, with no compiler warnings, on Temurin JDK 21.0.12.1+1.
All eight new workspace tests passed on both platforms. Its exact Git bundle,
compressed patch, source hashes and raw log are retained in the same evidence
directory. The earlier JDK 21 attempt found a syntax error in the new test;
the final run follows its correction and supersedes that failed attempt.

HRF characterization commit `61df8a46` passed `hrfJVM/test` and `hrfJS/test`,
261 tests each, on JDK 21. The initial JS attempt failed on the new timing
harness's lambda syntax; the corrected run passed and also compiled the JMH
benchmarks. Both attempts are retained. The JS timing harness completed with
checksum `9411.314245468566`; its single busy-host timing is diagnostic only.
Profile commit `70b22667` passed `fitJVM/test` (392 tests) and `fitJS/test`
(381 tests) against Gale `3172e80`. The allocation probe used 100,000 warmup
reductions and three blocks of 100,000 calls per curvature case. Positive
curvature measured 0.004, 0 and 0 B/reduction; indefinite curvature measured
0, 0 and 0. Both descriptive medians are zero. The first block's 400 total
bytes are retained rather than rounded away. This is calling-thread JVM
allocation evidence for this successful reduction fixture, not a timing or
whole-decoder allocation claim. `scalafimCompileAll` passed across JVM and JS
without compiler warnings against the same local provider.

The initial consumer attempt completed the JS smoke, then exhausted the 3 GB
heap during build reload before any fit tests ran. Its log is retained. The
successful fit gates started a fresh JDK 21 server with 5 GB and the explicit
provider property set at process startup. The profile candidate requires
`-Dscalafim.gale.build` pointing at the isolated Gale clone
until the upstream change is published and the pinned revision is updated.

Gale's sbt-git plugin could not load a linked worktree (`NoWorkTreeException`),
so its candidate is in the isolated normal clone
`/private/tmp/gale-dense-cholesky-candidate-20261004`. Tests use Temurin JDK
21.0.12.1+1, downloaded from the official release with its SHA-256 checked.

The user subsequently authorized `buc-gw01` over SSH. An isolated JDK 21 was
installed there after a transferred archive checksum check. Its mechanical
preflight refused load above 2.0; no benchmark was launched. The portable JMH
package is source-bound to `61df8a46`, contains 27 classpath entries and has
archive SHA-256 `829b2476f1ae8f10a08bb4c7c1c06c415a879db0c0f89f3c307d3c8e6b4d4542`.
See the retained preflight receipt, package manifest receipt, runner and remote
plan. Staging a runnable package does not satisfy S0 measurement or S4 admission.
The remote archive and each package file passed their hash checks, and JMH's
benchmark listing ran successfully. Runner review identified per-interval CPU
accounting and child-process cleanup corrections before any timing launch.
The corrected runner passed fractional CPU-time parsing and a synthetic
TERM-resistant child cleanup check. It uses an anchored process group and
per-interval CPU deltas; unknown competing Java/Node arrivals are conservative
refusals. The runner itself never marks a result admitted without output review.

Before any timing was collected, the user explicitly approved a host-specific
start/end load limit of 5 for 10-core `buc-gw01`. The retained amendment records
that change; all other host checks and the 1.05 margin remain unchanged. An
initial load-5 attempt refused load 5.2949 without starting JMH. A bounded
readiness wait then launched `s0-baseline-20261004-02` once, at load 4.9741.
Runner PID was 70392. Results and validity review are pending; no baseline or
non-inferiority conclusion follows from launch.

## Upstream integration candidate

Gale `6198ad2b62fd6dc290cc39fe4611b8311e741d58` applies the provider change
to upstream main `1b018d4dd082b786123b65e02cd5b81e61968e21` in the isolated
normal clone `/private/tmp/gale-dense-cholesky-integration-20261004`. It passes
689 JVM core tests, 679 JS core tests, 54 laws on each platform and
`scalafmtCheckAll`, without compiler warnings. Its core production trees are
byte-identical to consumer-tested `3172e80`; the newer base adds upstream
test/documentation/CI changes. The exact consumer run used `3172e80`, not a
published `6198ad2` dependency. Both candidate bundles and raw gate logs are
retained. The upstream candidate has not been published; ScalaFIM's normal pin
remains unchanged, so the profile commit still needs the local Gale override.

The [external consumer inventory](hrf-consumer-migration-inventory.md) records
actual Eidolon and PLS Neuro uses and proposed replacements. It is a read-only
inventory, not consumer adoption evidence. S1-S5 remain staged work.
