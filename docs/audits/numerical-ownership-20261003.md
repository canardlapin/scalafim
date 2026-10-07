# Numerical ownership audit — 2026-10-03

Mote: `bd-01KXYJPWYV08VFWRMCYYDPZCAV`.
Base: `fa6bd6a9f7d773144885c57849cd298521c1772c` (local main).
Candidate branch: `work/numerical-ownership-20261003`.
Gale: `18d24dbb5056122032b0278f8bad557a9bb1cf23`.

This packet repairs the remaining RSA generic solver and records the actual
HRF migration boundary. It does **not** complete the parent migration ticket.
HRF representations, polynomial QR, and profile-HRF Cholesky remain. No
whole-repository duplicate-free or performance-improvement claim is made.
ScalaFIM remains a general library; consumer source is compatibility evidence.

## Implementation and caller inventory

Paths below are relative to `modules/`, with the usual `shared/src/main/scala`
segment omitted. This is an implementation-level audit of the named targets
and additional factorization matches, not proof from a name-only scan.

| Target | Implementation and callers | Disposition |
| --- | --- | --- |
| `image.DMat` | No production definition found. `image/.../SampleSpaces.scala` obtains its inverse from `GeometryAffine.inverse`. | Historical target absent; no new image matrix migration. |
| `hrf/.../hrf/linalg/Mat.scala`, `Vec.scala` | Public array-backed case classes; public `data` and `unsafe`; matrix/vector construction, row/column copies, concatenation, elementwise operations. HRF evaluation, regressors, design builders and compatibility exports consume these. | Generic representation belongs in Gale. Staged migration remains outstanding. |
| `hrf/.../hrf/package.scala` | Wildcard exports `Mat` and `Vec`; overlaps Gale's `Vec` vocabulary. | Remove only through the ownership/import migration. |
| `hrf/.../hrf/linalg/Complex.scala`, `Fft.scala` | Private HRF complex values and split-array FFT used for convolution. FFT source records an earlier allocation rationale; this audit does not reproduce its historical timings. | Explicitly retained by ticket/contract pending a justified upstream FFT capability. |
| `design/.../design/linalg/Qr.scala` | Converts arrays through Gale builders, calls Gale `.qr`, delegates `applyQ`/`applyQT`; exposes rank/permutation/diagonal-R condition policy. | Legitimate adapter, not a private factorization. Diagonal-R ratio is explicitly a proxy, not a 2-norm condition number. Allocation remains unbenchmarked. |
| `motion/.../motion/MotionEstimator.scala: solveNormal6` | Jacobi-scaled six-parameter normal system, Gale Cholesky with relative pivot policy, then unscaling and finite checks. | Private elimination already removed. Prior receipt: [motion scale-invariance](../verification/motion-scale-invariant-solve-20260930.md). Existing 97 JVM / 84 JS results are historical, not rerun here. No retained specialized factorization or allocation advantage is claimed. |
| `mvpa/.../mvpa/Rsa.scala` | `PartialPearson` aligns labeled controls, centers outcomes/controls, then originally formed two normal-equation systems and used private Gauss–Jordan with absolute `1e-12` pivot cutoff. | Replaced here with one Gale column-pivoted QR and two right-hand-side projections. |
| `mvpa/.../mvpa/FeatureModel.scala: solve` | Standardized ridge matrix construction followed by Gale Cholesky/solve. | Legitimate domain preparation and Gale adapter. |
| `design/.../design/basis/Basis.scala: Poly.householderQrReduced` | Complete local Householder QR; `Poly.fit` uses reduced Q and signed diagonal R to reproduce R polynomial basis/coefficient conventions. | Still a generic factorization. Requires a separate bounded replacement through unpivoted Gale QR, preserving `Q*diag(R)`, prediction coefficients, R parity and rank behavior. No measured retention justification found. |
| `fit/.../fit/profile/ProfileReduction.scala: SmallCholesky` | General in-place Cholesky and triangular solves, also called by `GramCondition`, `ShapeDecoder`, `CompactCondition`, `TrialBanded`, and tests. `c`/`d` are runtime dimensions; “tiny” is not an enforced fixed-size invariant. | Still a generic factorization. Allocation-sensitive profile loops need a measured Gale workspace/capability migration. No comparative measurement found that justifies retaining a separate solver family. |
| `connectivity/.../Numerics.scala` | SPD inverse via Gale Cholesky/solve, eigenvectors via `gale.spectral.Eigen`; owns symmetrization/jitter policy. | Adapter/policy, not duplicate decomposition. Numerical policy itself was not requalified here. |
| `fit/.../CoefficientMatrixStorage.scala`, `FixedEffects.scala` | Inverse methods delegate to Gale Cholesky and solve against identity. | No local elimination family. Existing tolerance policies are outside this bounded repair. |
| `hrf/.../Basis.scala: BasisTransform.inverse` | Identity, diagonal reciprocals and permutation inversion only. | Domain transform algebra, not a general matrix inverse. |

UMVPA-M2.07 (`bd-01M2BNFWEJFFZYMGJB3Z41RGKP`) is closed. Its consumer
migration did not remove `Rsa.solveLinearSystem`; both this base and the
inspected `origin/main` still contained it. The old HRF draft's assumption
that M2.07 would repair it is therefore superseded by this packet.

## RSA contract and independent evidence

For centered control columns C, partial Pearson is the cosine between the
orthogonal residuals of the centered observed and model RDM vectors. Scaling
any column by a nonzero constant preserves the control span; positive scaling
of either outcome preserves the score and negative scaling flips its sign.

The implementation first divides each finite vector by its maximum absolute
entry, centers it, and normalizes its Euclidean norm. This prevents overflow
and underflow caused solely by units. A single column-pivoted Gale QR of the
normalized controls determines rank. All requested controls must be independent:
no silently dropped controls or pseudoinverse policy is introduced. Absolute
QR diagonal cutoff `1e-12` now acts on unit-norm columns and is independent of
their physical units. It is a numerical policy, not a universal identifiability
threshold.

Both normalized outcomes are transformed together by Q transpose. Coordinates
after the control rank are the orthogonal residuals in another orthonormal
basis; their dot product and norms give the same partial correlation without
forming normal equations or solving for coefficients. Relative residual norm
at most `1e-12` is explicitly refused, including exactly explained outcomes
whose floating-point residuals are roundoff. Constant controls give a rank
error; constant outcomes give a zero-residual error. Final finite correlation
is bounded to [-1,1] for endpoint roundoff.

Independent fixture: four disjoint mean-zero contrasts a,b,u,v, each with
squared norm 2. Observed = `2a-3b+2u+v`, model = `-a+4b+u+3v`; controlling
for a,b leaves `2u+v` and `u+3v`. Their dot product is 10 and squared norms
are 10 and 20, giving exactly `1/sqrt(2)`. Tests also use nonsingular mixtures,
reordered controls, mixed scales from `1e-150` to `1e150`, outcome offsets/signs,
duplicates, constants, unresolved directions and nonfinite values.

The conditioning witness uses controls a and `a + 2^-30 b`. Their design
condition number is approximately `2^31`, while the normal matrix condition
is approximately `2^62`. IEEE Double rounds `2 + 2*(2^-30)^2` to 2, so the old
computed Gram matrix has determinant zero even though the controls are
independent. The new fixture's analytic score remains `1/sqrt(2)`; tolerance
`2e-6` allows conditioning-amplified roundoff. Ordinary fixtures use `1e-12`.
This is numerical conditioning evidence, not a speed or allocation reduction.

## HRF ownership and compatibility

The recovered [v4 migration contract](../plans/hrf-matvec-contract-v4.md) is
stored verbatim, SHA-256
`dbc827aa9114c98b380e5e7138b6b14df3e4250fac25de981951ddf930f7d96e`.
It originated in the 2026-10-01 session scratchpad at
`/private/tmp/claude-502/-Users-bbuchsbaum-code-scala-scalafim/35f4d01b-4044-469b-be6b-0fb1ad772510/scratchpad/backlog/hrf-matvec-contract-v4.md`,
linked by Fray threads 77/114. Its historic counts and status are preserved as
provenance, not asserted current. Mote's 2026-09-30 decision records approval
of D1–D5; the text still says draft, and v4's later performance protocol has no
completed measurement attached.

Decisions: DMat/DVec only, no aliases; immutable publication via Gale builders
or defensive copying, never caller-owned array transfer; explicit domain value
equality with signed-zero and NaN policy; no matrix re-exports; private
`evaluateInto` for hot kernels; cbind upstream as DMat.hcat; staged S0–S5 with
per-slice ownership/review. JS timing is evidence, not an admission gate.
Pin-gated external breaking changes carry replacement examples, and S5 waits
for Eidolon and PLS Neuro to pass S3. These decisions do not authorize an
upstream publication.

Current Mat/Vec ownership is still mutable: `unsafe` aliases caller arrays,
`.data` returns those arrays, and case-class equality compares array identities.
`Vec.toArray`, `Mat.row`, `Mat.col` and `Mat.updated` copy, but do not protect the
original matrix's storage. Model's canonical `matrixValues` is already DMat;
its `matrix: Mat` compatibility export is detached. Fit retains repeated
copy adapters (`MatrixAdapters`, `ObservedFamilyCertification`,
`StructuralHypotheses`). A targeted codec scan in hrf/design/model/fit found no
Mat/Vec-specific codec; this is not a blanket serialization-compatibility claim.

[Exact importer lists](numerical-ownership-importers-20261003.txt) record an
explicit-import lexical scan, excluding same-package and fully-qualified uses:

| Snapshot | Main imports | Test imports | Total |
| --- | ---: | ---: | ---: |
| Base fa6bd6a9 | 46 | 75 | 121 |
| Pending SF candidate 8ac435387fb4477643f050ad1725fe75374d6e42 | 56 | 90 | 146 |

The SF candidate is a distinct unlanded branch, not part of this base. Its new
HRF/design/model callers must be included when the migration is rebased.

External source snapshots (read-only local checkouts, not a consumer build):
Eidolon `83c2a3a1864ce805d6d4c7c83a4ecee796314449` pins ScalaFIM
`8e38b9a72932cf645abd123175f819f0ebee63f8` in `project/BuildVersions.scala`.
`HrfEvaluationKernel` returns Mat; `HrfEvaluationModule` uses Mat in artifact
and view adapters. These become DMat signatures and explicit Gale imports.
PLS Neuro `8919882867a6b5c9d3bdd9d284e5cf30b1a51800` has three main importers
(`ContrastReadout`, `ModelDraft`, `ResponseBasisPane`) and 25 test importers.
Its `project/Providers.scala` requires an explicit prepared provider directory,
so no single active ScalaFIM pin can be inferred from the consumer HEAD.
Neither snapshot establishes the S5 consumer admission gate. Before S1, re-run
an exact `unsafe`/`data` migration inventory at each chosen consumer revision
and declared provider closure, including test probes; replace raw reads with
indexing/copy APIs and construction with Gale-owned builders. No consumer code
was edited as part of this general-library packet.

## Verification and remaining gates

Final candidate: `mvpaJVM/test` and `mvpaJS/test` passed **128 tests each**
(exit 0), including ten new projection tests, existing RSA parity, and operator
RSA consumer tests. The complete final log contains no compiler warnings.
`git diff --check` passes. These are owning-module gates, not a whole-repository
compile or HRF migration qualification. No production public signature changed.
A separate read-only reviewer accepted the residual-projection mathematics,
normalized rank policy and squared residual threshold. The reviewer did not
rerun tests; platform evidence comes from the logged runs above. Review covered
RSA source SHA-256 `b64c24acfd1bd2b3e3e54507af523e6c65514fff67235a75f27901d2611a8ec0`.
The first candidate JVM run passed 126 tests before the two final boundary
tests were added; the final run supersedes that test count.
Allocation probe is bounded JVM per-thread allocated
bytes for full scoring calls at 10/66/190 distances with 2/4/8 controls. It checks
the known `1/sqrt(2)` score, uses 10,000 warmup calls and three blocks of 5,000
measured calls. It is not a timing benchmark, JMH comparison, or portable
allocation claim. The run used Homebrew OpenJDK 25.0.1 on macOS arm64.
Measurements include label alignment, construction and projection in the full
scorer, and count managed allocations on the calling thread only.

| Distances / controls | Old median B/score | Candidate median B/score | Ratio |
| --- | ---: | ---: | ---: |
| 10 / 2 | 12,994.816 | 12,491.3632 | 0.961 |
| 66 / 4 | 69,664 | 74,600 | 1.071 |
| 190 / 8 | 297,640 | 323,984 | 1.089 |

These are three-block descriptive medians in one warmed resident JVM, candidate
then baseline, not randomized timing/non-inferiority estimates. Larger cases
allocate more with QR: the numerical repair has a measured allocation cost.
No claim of a general allocation reduction or throughput improvement is made.
The HRF 5% timing gate does not apply to this separate RSA repair.

The exact baseline RSA source (`fa6bd6a9`, SHA-256
`3f889fdc6bfd057bba97d93e04e79e6dd03b1aa6bd590fa7a63d5103a1503192`)
was temporarily restored for an actual regression/allocation comparison, then
the candidate was restored automatically. Against the then-nine-test fixture,
the baseline failed three tests (unit scaling, near-collinear full rank, and
near-zero residual policy) and passed six. The saved fixture distinguishes this
run from the final ten-test suite, which adds common extreme outcome scaling.
Gzip-compressed raw logs (preserving terminal bytes and whitespace), exit-code
sidecars, the baseline fixture and the initial failed-test
receipt are committed under [evidence](evidence/numerical-ownership-20261003/).

The initial focused test command was queued before the implementation edit.
Although its historical log name is `red.log`, it tests the candidate: it must
not be reported as a pre-fix failed regression run.

HRF S0/S4 timing cannot presently satisfy the v4 host-validity rule: on
2026-10-03 at 13:07:09 UTC, one-minute load was **11.08**, versus the required
maximum **2.0** (earlier snapshot 20.87). No valid timing block was started;
there is no non-inferiority result. Do not weaken the threshold or stop other
sessions to create a quiet host. S0 requires characterization and a valid
baseline before fixing the S4 sample size; S4 has one frozen analysis and
all benchmarks must pass the 5% margin. S5 additionally requires recorded
consumer admission. HRF production representations are unchanged in this packet.

Next actions on the still-open parent: land the reviewed RSA packet; execute
HRF S0 on a host meeting the fixed protocol; stage S1–S5 with upstream hcat and
consumer receipts; replace or independently justify Poly QR and SmallCholesky
with appropriate parity/conditioning/allocation evidence. A file deletion or
Gale call alone does not satisfy those remaining gates.
