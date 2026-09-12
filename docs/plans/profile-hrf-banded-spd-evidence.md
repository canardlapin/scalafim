# Banded SPD provider evidence (PHRF-06)

Date: 2026-09-12. Scope: the upstream Gale factor/solve capability required by
[TrialBanded](profile-hrf-work-items-v2.md#phrf-07-implement-the-trialbanded-backend-on-shared-gram-blocks).
This is provider qualification, not trial-estimator or calibration evidence.

## Provider contract

Gale now supplies `BandedCholesky`, an immutable `ExactSolveFactor` constructed
from packed lower SPD bands. `bands(i,d) = A(i,i-d)` places the diagonal in
column zero. Factorization stores `N (b+1)` doubles and uses `O(N b²)` work;
each RHS solve uses `O(N b)` work. The coefficient matrix is never expanded
to dense square storage by the factor/solve kernels.

The API includes pure vector/matrix solves, in-place `MutableDVec` and
`DMatBuilder` solves, transpose and triangular solves, log determinant, and
conditioning diagnostics. A consuming builder constructor transfers storage
without copying. Nonfinite input, nonpositive pivots and shape failures are
explicit. The conditioning diagnostic is an absolute-triangular-comparison
upper bound, potentially conservative or infinite; it is not a calibrated
admission criterion. Pivot ratio is labeled separately.

The implementation lives entirely in Gale. The prepared ScalaFIM
`BandedSpdProviderSuite` is retained as
[an apply-ready patch](profile-hrf-banded-spd-consumer.patch) while the source
dependency graph is aligned. The normal build keeps its existing Gale pin.
No private factorization, trial estimator, or new scientific admission rule
is introduced here.

## Provenance and integration

The previous ScalaFIM pin is `83cac90a678d1b8a31c590e0c1b8fc8bf3427161`.
Development started from verified Gale main
`288596690a4027157b40d5ffbf36e7d24d98e009`, preserving the independently active
`release/0.1-stabilization` branch. This main revision also carries upstream
QR/workspace improvements, numerical-policy tests and portable sparse
Cholesky that were newer than ScalaFIM's old pin. Consumer regression gates
therefore cover existing condition and fitting paths as well as the new seam.

Local-override development used `/private/tmp/gale-phr06-checkout` and an
isolated ScalaFIM checkout at `fd62ea7aabb25ccd415faacfedb7243152861502`.
Unrelated active HRF/SPMG corrections in the shared workspace were excluded.
An initial concurrent build invalidated Gale JS output and produced stale
TASTy warnings in the consumer. Those results were discarded; provider and
consumer builds were then run sequentially, with affected output cleaned.

## Numerical evidence

`BandedSpdProviderSuite` builds overlapping finite-support trial columns at
12 and 40 trials, spacings 2 and 5, and ridge penalties `1e-6`, `0.3`, `1e3`.
It checks packed versus dense Cholesky solutions for three RHS columns,
independent dense residuals, profile quadratic energy and log determinants.
It also checks factor reuse through the generic capability, in-place builder
solves, and preservation of pure inputs. The fixture's finite support is a
provider test geometry; it makes no Cascade34 truncation claim.

Gale's own suites add analytic Laplacian factors, inverse and determinant;
known-factor reconstruction; scaling, reversal and superposition laws;
independent Breeze/LAPACK parity; ownership and strided-input behavior;
finite/shape/pivot failures; explicit conditioning overflow; and a
100000-row, bandwidth-2 solve retaining 300000 factor entries.

## Validation and throughput

The provider candidate is committed as
`2a6734cfce7686c7c5a9d66cadf679f47fdf805b`, retained on
`feat/packed-banded-spd` in the original Gale repository and in the isolated
standalone checkout. The original Gale release checkout remains clean.

Gale qualification: 652 JVM core tests, 642 JS core tests, 54 laws per
platform, 84 Breeze parity tests, both optimized JS links, formatting and
documentation generation passed. The full ScalaFIM compile passed without
compiler warnings, and the banded provider, profile-reduction, condition-fit
and compact-runtime suites passed 8 tests on each platform with the explicit
local override. Broader consumer regressions also passed:

| Suite | JVM | JS |
| --- | ---: | ---: |
| `design` | 238 | 238 |
| `fit` | 318 | 307 |
| `firstLevelLaws` | 51 | 8 focused tests |

The full first-level laws suite was run on JVM; JS ran the four focused
provider/profile suites above. The existing condition milestone's unmet LWU
low-SNR admission remains unchanged.

JMH on the Apple M3 Max (JDK 25.0.1, one thread/fork, three 500 ms warmups,
five 500 ms measurements, GC profiler, 256–512 MiB heap):

| N | b | Factor | Factor + conditioning | One RHS in place | Four RHS in place |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 300 | 64 | 0.404 ms | 0.437 ms | 0.074 ms | 0.148 ms |
| 1200 | 64 | 1.785 ms | 1.972 ms | 0.321 ms | 0.697 ms |
| 4800 | 64 | 7.294 ms | 8.019 ms | 1.314 ms | 2.971 ms |

At fixed bandwidth 64, the 1200→4800 row increase costs 4.09× for both
factorization and one-RHS solve. At N=1200, doubling bandwidth 128→256 costs
4.15× for factorization and 1.88× for solve, consistent with the quadratic
and linear work counts. In-place allocation is approximately 16–57 B/op,
with no N- or Nb-sized numerical scratch. Timings include RHS reset work;
pure factor timings include copying and matrix-norm calculation. The largest
four-RHS result has a wide 99.9% interval (about ±0.957 ms), retained without
trimming. Raw JMH samples, allocation receipts and commands are committed in
Gale under `benchmarks/results/2026-09-12-banded-cholesky*.json` and
`docs/banded-cholesky-evidence.md`.

## Publication boundary

[Gale PR #11](https://github.com/canardlapin/gale/pull/11) merged as
`099832ff15c8a4a8fcf3398c7b779fb4bbc12434`, verified against live main.
All 14 [CI jobs](https://github.com/canardlapin/gale/actions/runs/34695481082)
passed on the candidate, including the JDK 21 / Node 22 numerical lanes,
Breeze interop, documentation, optimized links and experimental WebAssembly.
The separate Cursor approval and security automation checks also passed.
Bugbot reported a usage limit and is not counted as review evidence.

The merged revision was then tested as an immutable ScalaFIM source pin,
without a local override. sbt fetched and compiled the exact `099832ff`
checkout, but `scalafimCompileAll` failed at `mvpaFitJVM / update` with a Gale
version conflict: the sibling source builds still supply
`1.0.0-SNAPSHOT`, while Multivar requests `1.0.0-83cac90a678d`.
The affected test commands following that compile did not run.

Inspection of the effective fit classpath showed both the new Gale source
directory and the older `83cac90a` source directory. Live `image4s`, `graph4s`,
`reframe4s` and `multivar` still pin that older revision. The root's existing
foreign-project `version` settings did not determine the effective versions:
sbt reported `0.1.0+99-099832ff-SNAPSHOT` for the new source project and
`1.0.0-SNAPSHOT` for the older one. Therefore the earlier local-override
results are useful regression observations, not proof of a single-provider
consumer build.

The attempted ScalaFIM pin was restored to
`83cac90a678d1b8a31c590e0c1b8fc8bf3427161`; the consumer suite remains a patch.
PHRF-06 is **not closed**. Remaining work is coordinated sibling source-pin
and artifact-coordinate alignment, verification that the consumer classpath
contains one Gale implementation, and the immutable pin update followed by
full compile and affected JVM/JS tests. The dependency conflict was not
suppressed and no compatibility rule was relaxed.

PHRF-07 owns Gram-block preparation and the complete trial backend. Its B0
runtime checkpoint, followed by PHRF-14 calibration, remains outstanding.
