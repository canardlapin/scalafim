# HRF Mat/Vec ownership and export contract (DRAFT v4, bd-01KXYJPWYV08VFWRMCYYDPZCAV)

> v4 (2026-10-01) answers exec-orchestrator's sequential-error objection (Fray #114, seq 780):
> - one frozen final analysis, with no interim looks or extensions;
> - objective host validity, fixed before any outcome is read;
> - joint non-inferiority, with every benchmark required to pass.
>
> v3 (2026-09-30) fixes the performance rule per exec-orchestrator (Fray #77, seq 769). Pass means u ≤ m, fail means l > m, and anything else is inconclusive. Blocks are resampled in pairs, the sample size is fixed with a bounded number of extensions, and the margin does not depend on observed noise.
> v2 (2026-09-30) applies exec-orchestrator's critique of v1 (sha256 765ef95402fa81cd…, Fray #77, seq 754):
> - ownership that Scala cannot enforce is no longer claimed;
> - equality is now an explicit value policy;
> - S1 is described as the source-incompatible change it is;
> - the deprecation policy is an explicit choice;
> - the performance gate is a pre-registered non-inferiority rule.
>
> Gale facts were refreshed to the integration pin `da38f8c4`. That pin is `18d24dbb` plus one commit (banded SPD log-determinant jets), which touches none of the `DMat`/`DVec` surfaces cited here. The inventory in §1 is historical, taken at `7989608d`; it is re-counted at the target before S1.

Status: draft for owner review. Nothing below is implemented. Scope is the HRF `Mat`/`Vec` representation
only; motion `solve6` is done (docs/verification/motion-scale-invariant-solve-20260930.md), MVPA
`Rsa.solveLinearSystem` is deferred to UMVPA-M2.07 (bd-01M2BNFWEJFFZYMGJB3Z41RGKP). Paths below are under `modules/`.

## 1. Inventory (as of 7989608d + working tree)

**Definitions.** `hrf/shared/.../hrf/linalg/`:
- `Mat.scala:5` `final case class Mat private (rows, cols, data: Array[Double])`, row-major. `data` is a public
  mutable array. `Mat.unsafe` (`:75`) is public and zero-copy, so any module can alias or mutate a buffer. Ops:
  `apply(r,c)` `:11`, `updated` `:13` (full clone), `row` `:18` / `col` `:22` (copies to `Vec`), cbind `++` `:30`,
  `map` `:42`, `zeros` `:51`, `eye` `:53`, `fromRows` `:61`. No products, solves or decompositions.
- `Vec.scala:5` `final case class Vec private (data: Array[Double])` (public `data`). `toArray` `:8` clones. Ops:
  `map`/`zipMap`/`+`/`-`/`*k`/`dot`/`maxAbs`; public `unsafe` `:58`.
- Case-class equality on an `Array` field is **reference** equality. `hashCode` is identity-based, and `toString`
  prints `[D@…`. This contaminates every case class that holds a `Mat`: `design/.../basis/Basis.scala:146,163,402,429,532,567`
  (`Ident`/`Poly`/`Standardized`/`Scale`/`RobustScale`/`BSpline`) and `design/.../event/EventTerm.scala:13`
  (`TermDesignMatrix`). Nothing in-repo serializes `Mat`: no codecs were found. Artifacts go through `DMat`/`Vector`.
- `Complex.scala:3` and `Fft.scala:13` are already `private[hrf]`. `Fft.scala:3-12` documents a measured
  allocation rationale for its split-array layout (127 ms vs 3.6 ms). Gale has no FFT. `gale.spectral.Complex` is a
  boundary-only value with no arithmetic tier. **Retain both, per the ticket.**
- `hrf/package.scala:10` `export scalafim.fmri.hrf.linalg.{Mat, Vec}` means `import scalafim.fmri.hrf.*` brings in
  `Mat`/`Vec`. `gale.linalg` also defines `Vec` (trait and object; `Vec.scala:7,300` at the pin), so a file that
  wildcard-imports both packages has an ambiguous `Vec`.

**Build facts.** `build.sbt:344-363`: `hrf` does **not** depend on Gale. It declares `spire` and JVM-only `JTransforms`,
but no hrf source uses either (a separate hygiene item). `hrf` has no `jvm`/`js` source dirs. hrf/design/model/fit
use `strictFirstLevelCompilerSettings` (`build.sbt:185-190`, `-Werror`). Combined with `-deprecation`, that makes
any `@deprecated` use in those modules a compile error. The version is `0.1.0-SNAPSHOT` and there is no MiMa.
A JMH project already exists (`hrfBenchJVM`, `build.sbt:565`: `RegressorConvolutionBenchmark`, `DenseDriveBenchmark`,
`BasisResponseBenchmark`). There is no JS benchmark.

**Importers.** 126 files: 52 main, 74 test. Counted as explicit `hrf.linalg`, `hrf.*` wildcard, or selective import.
- hrf (31): these files produce the data. `Hrf.scala:51-95` makes `Vec` the public per-lag result type (`apply`/`at`/`evaluateInSupport`);
  `eval` `:61-75` allocates one `Vec` per lag, then copies. Hot kernels in `regressor/Regressor.scala:391,529` read
  `hrf(lag).data` per lag and write into `Array[Double]`. They publish zero-copy through `Mat.unsafe` (`:442,589,633`);
  `Fft.convolveReal` is at `:618`. `Penalty.scala:52` mutates `Mat.eye(nb).data` in place. `Toeplitz`, `Deriv`,
  `Decorators`, `Basis`, `LwuBasis`, `Evaluate`, `Design`, `RegressorSet` (cbind reduce at `:16`), and `compat/RNames` all
  construct via `unsafe`/`zeros`.
- design (43; 26 main): only construction and indexing. `Mat.unsafe` wraps freshly built arrays
  (e.g. `formula/EventModelBuilder.scala:1577,1643,1657,1673,1800,1886,2191,2696`, `contrast/ContrastSpec.scala:131`,
  `basis/Basis.scala` ×16, `baseline/BaselineModel.scala` ×13). There is heavy `.data` flat indexing and some cbind.
  All QR goes through the Gale adapter `design/linalg/Qr.scala` (already assessed as legitimate).
- model (5): `model/DesignBlock.scala:33` executes on `matrixValues: DMat`. `matrix: Mat` at `:41-44` is a
  "detached compatibility export" (a copy). `DesignSchema.scala:740,995` also stores `DMat`, with `exportMatrix` →
  `Mat` at `:1132`. **The design/model boundary is already half-migrated: Mat is the legacy view, DMat the owned value.**
- fit (38; 4 main): these files convert only, `Mat` → `DMat`, by copying. There are three duplicate adapters:
  `fit/MatrixAdapters.scala:18,26,40` (`fromHrfMatrix*`), `fit/profile/ObservedFamilyCertification.scala:139`
  (`toDMat`), and `fit/StructuralHypotheses.scala` (`fromRows`, `.data`).
- first-level-laws (5), hrf-laws (2; `hrf-laws/.../HrfLaws.scala` is main code: `.data` ×9, `unsafe` ×4), fit-estimates (2).
- **External** (they pin ScalaFIM revisions): eidolon has 2 main files. `HrfEvaluationKernel.scala:8,12` uses
  `type Output = Mat`, and `HrfEvaluationModule.scala` is the other. PLSNeuro has 3 main files
  (`glm-review/ContrastReadout.scala`, `ModelDraft.scala`, `glm-review-javafx/ResponseBasisPane.scala`) and about 25 test
  probes. Vendored `scalafim-*` copies under PLSNeuro closures are not consumers.

**Generic vs domain.** Everything in `Mat`/`Vec` is generic storage and elementwise work, so it belongs in Gale.
Nothing in them is scientific policy. The domain content lives in the callers: lag axes, basis identity, penalty
order, contrast semantics, design column schema. Adjacent finding, out of scope here and to be filed separately:
`design/.../basis/Basis.scala:259` `householderQrReduced` is a private QR family, used by `Poly.fit` (`:204`) for R `poly()` parity.

**Gale at the pin `18d24dbb`.** `DMat`/`DVec` are immutable strided views (`Matrix.scala:40`, `Vec.scala:23`) with
private constructors. The raw-array wrap `fromArrayRowMajor` (`:920`) is `private[gale]`. The public owned-write path
is `DMatBuilder.writeLinear`/`update` followed by `result()`, which transfers ownership without a copy
(`MutableMatrix.scala:15`). Every builder write checks that the builder is open and the index is in bounds. On JS,
storage is `Float64Array` (`core/js/.../PlatformArrays.scala:7`), so **any** `Array[Double]` → `DMat` hop copies on JS.
Gale already covers `zeros`, `eye`, `tabulate`, `gatherRows`/`gatherColumns`, `row`/`col` views, `+`, `-`, `*`,
`copyRowMajorTo`, and the solves and factorizations. Gale has no cbind: fit carries its own `MatrixAdapters.bindColumns`.

## 2. Contract

**C1 End-state (recommended).** Gale `DMat`/`DVec` become the *only* general dense matrix and vector representations
in ScalaFIM public APIs. `scalafim.fmri.hrf.linalg.{Mat, Vec}` are deleted, not aliased. Domain meaning goes into
typed records only where a real invariant exists: basis samples carry a lag axis and basis ids, penalties are
square and symmetric positive semidefinite, and the design already has `DesignBlock`/`DesignSchema`. None of
these records exposes a mutable buffer.
- *Rejected: an opaque alias `HrfMat = DMat`.* It adds a name that carries no invariant and keeps two vocabularies alive.
- *Rejected: hardening `Mat` in place.* That keeps the duplicate representation and the three copy adapters in fit.
- **[OWNER DECISION D1]** Confirm C1, or choose "harden `Mat` in place" (stages S0-S1 only).

**C2 Ownership.** Public values own their storage immutably, following Gale's rule that no public API creates a
mutable alias. Hot kernels may use private `Array[Double]` scratch. They publish either through `DMatBuilder`, or
through one final copy into a builder when the kernel's own work dominates that copy (convolution is O(n·m)).
`private[hrf]`/`private[design]` helpers own this adaptation, one per module. No `unsafe` constructor crosses a
module boundary.
- A public zero-copy `ownRowMajor(Array[Double])` cannot establish exclusive ownership in Scala, because the caller can
  keep the array and mutate it later. It would reintroduce exactly the mutable alias this contract removes, so it is
  **not** part of the contract, and a measured copy cost does not justify it.
  - The defaults are `DMatBuilder` or a defensive copy.
  - The only acceptable zero-copy transfer is one that Gale enforces itself, for example a builder that writes into
    storage it allocated and seals on `result()`.
- **[OWNER DECISION D2]** If S4 shows a builder or copy regression beyond the §3 margin, should we ask Gale upstream for
  an *enforceable* transfer capability (builder-owned storage), rather than weaken ownership? Recommendation: yes, and
  only in that case. Never take a caller-supplied array.

**C3 Exports.** The wildcard `scalafim.fmri.hrf.*` stops exporting matrix types. hrf re-exports no Gale names, so
callers import `gale.linalg.{DMat, DVec}` explicitly. That removes the `Vec` collision. `Complex`/`Fft` stay `private[hrf]`.

**C4 Equality and inspection.** No case class relies on a matrix field's default equality. Where equality matters (the
`ParametricBasis` cases, `TermDesignMatrix`), it follows one declared *value* policy.
- **Policy.** Values are equal iff their dimensions are equal and every element is equal under
  `numericEqual(a, b) = (a == b) || (a.isNaN && b.isNaN)`.
  - `+0.0` and `-0.0` are therefore equal, which matches scientific meaning for design and basis values.
  - Any NaN equals any NaN. NaN payloads carry no meaning here.
  - `hashCode` is computed over canonicalised elements (`-0.0 -> 0.0`, every NaN mapped to a single canonical NaN) and
    the dimensions, so it is consistent with `equals`.
  - A module that needs a different semantic declares it on that type, for example exact bitwise identity for
    provenance. It is never imposed globally.
- **Fingerprints are not equality.** A fingerprint such as `DesignFingerprint` may be used as a fast inequality
  pre-check. Equal fingerprints must still be confirmed by the value comparison, because collisions are not ruled out.
- **Required laws**, on JVM and JS:
  - reflexivity and symmetry;
  - `equals` implies equal `hashCode`;
  - independently allocated buffers with equal values compare equal;
  - different dimensions with equal flattened data compare unequal;
  - signed-zero and NaN cases behave as specified;
  - a fingerprint collision is simulated by forcing equal fingerprints over different values, and the values must
    still compare unequal.

**C5 Hot-path discipline.** The `Regressor` convolution kernels, `Fft`, `Hrf.eval`, and the design basis and
baseline builders keep allocation-free inner loops. Per-lag `Hrf.apply` stays a public convenience that returns
`DVec`. The kernels move to a private `evaluateInto(lag, dest: Array[Double], offset)` so they no longer allocate one object per lag.

## 3. Staged migration

Every slice is one PR. Each slice must pass `sbt scalafimCompileAll` (the lint gate, with `-Werror`) and the
touched modules' `*JVM/test` and `*JS/test` suites, run in bounded batches per AGENTS.md.
- **S0 Characterize (tests only).** Pin current behavior before any change:
  - golden fixtures for the `Hrf.eval`/`Regressor` outputs and the design matrices already covered by R-parity
    suites (`RegressorParitySuite`, `LibraryParitySuite`, `HrfRParitySuite`, `RParityCorpusSuite`);
  - an aliasing test that documents the current `Mat.data` mutation hazard;
  - JMH baselines for the three `hrfBenchJVM` benchmarks;
  - a new, small JS timing harness for regressor convolution. **[OWNER DECISION D3]** Is a JS perf gate required, or only JVM JMH?
- **S1 Seal.** Make `Mat.unsafe`/`Vec.unsafe` `private[scalafim]` and `data` `private[scalafim]`, adding read-only
  `copyRowMajorTo`/`toArray`. Fix `Penalty.scala:52` so it no longer mutates the result of `eye`.
  - Numeric results are unchanged, but this is **source-incompatible for external callers**: any consumer that uses
    `unsafe` or `data` stops compiling when it bumps its pin. The slice records that consequence explicitly.
  - `private[scalafim]` still leaves every in-repo module with a mutable-alias capability. S1 narrows the exposure but
    does not remove it; only S5 does.
  - Pinned external consumers (eidolon, PLSNeuro) need no simultaneous deployment. Before any of them bumps past S1,
    S1 ships a concrete migration inventory of every external use of `unsafe` and `data`, found by grep at their
    pinned revisions, with a replacement example for each.
  - S1 can still ship independently even if D1 is declined.
- **S2 Gale edge.** Add `galeCore` to `hrf` in `build.sbt` (acyclic, since Gale is standalone). Add one
  `private[scalafim]` adapter pair `Mat <-> DMat` in hrf. Delete the duplicates in `fit/MatrixAdapters.scala:18-55` and
  `ObservedFamilyCertification.scala:139` in favor of it. **[OWNER DECISION D4]** Should cbind move upstream
  (`DMat.hcat`) or stay as one ScalaFIM helper? Recommendation: upstream, because it is generic.
- **S3 Consumers first, leaves inward.** Change the public signatures to `DMat` in this order: fit (4 main files,
  plus tests, which are mostly fixture construction); then model (`DesignBlock.matrix` is removed, since `matrixValues`
  becomes canonical); then design, split across 2-3 PRs (contrast → basis/baseline → event/formula); then hrf-laws
  and first-level-laws. Each PR converts at the boundary it moves, keeps outputs value-identical (bitwise where no
  arithmetic changed), and adds the C4 equality laws for the case classes it touches.
- **S4 hrf core.** Switch `Hrf.eval`, `Regressor`, `RegressorSet`, `Toeplitz`, `Penalty`, `Basis`, and `Deriv` to
  `DMat`/`DVec`, and add the `evaluateInto` kernel (C5).
- **Performance decision rule**, fixed before any S4 measurement:
  - **Setup.** Baseline and candidate are matched builds on the same JDK and flags, measured on a quiet host. Load
    average and competing processes are recorded for every run.
  - **Design.** Interleaved A/B blocks. Each block is one baseline fork and one candidate fork, with the same warmup
    and the same inputs as S0, run back to back in randomized order.
  - **Primary metric.** The ratio `r = candidate / baseline` of median time per op, per benchmark.
  - **Secondary metric.** Allocation per op (`-prof gc`, B/op), which must not increase for `evaluateInto` paths.
  - **Interval.** A two-sided 95% paired bootstrap CI `[l, u]` for `r`, resampling whole A/B blocks so that each
    resample keeps the pairing.
  - **Margin.** `m = 1.05`, a practical acceptable regression chosen for the use case, independent of observed noise.
    High S0 variance does not widen `m`. Instead it sets the sample size, or it triggers an explicit prospective owner
    decision on cost.
  - **Sample size and sequential policy: one prospectively frozen final analysis.** No interim looks and no extensions.
    - `n` blocks is fixed before S4 from the S0 fork-to-fork variance, so that at `r = 1` the expected half-width of the
      95% CI is at most 0.02. It is frozen in the S4 receipt before any S4 block runs.
    - There is exactly one look, after all `n` valid blocks.
      - Nothing depends on intermediate results, so the ordinary 95% interval keeps its nominal level, and there are no
        extensions to pool with or replace earlier blocks.
      - An inconclusive final result is reported as inconclusive and escalated to the owner. Any further measurement is
        a new, separately pre-registered study that does not reuse these blocks.
  - **Host validity.** These criteria are fixed before any outcome is read and checked mechanically for every block:
    - 1-minute load average ≤ 2.0 at block start and end;
    - no other JVM or `node` process above 10% CPU during the block (1 Hz sampling);
    - no thermal-throttle or power-source change reported;
    - identical JDK, flags and commit for both arms.

    A block that fails any check is invalid. It is retained and reported, excluded from the analysis, and replaced by
    a new block appended at the end. Replacement depends only on these host criteria, never on timing results. At most
    `n/2` replacements are allowed; beyond that the study is reported as not measurable on this host.
  - **Multiple benchmarks (joint non-inferiority).** Every S0 benchmark must pass individually (`u ≤ m`).
    - Admission is the intersection of the per-benchmark tests (an intersection–union test). Its level is at most the
      per-test level, so passing needs no multiplicity adjustment.
    - Any single Fail fails the slice.
    - Any Inconclusive without a Fail makes the slice inconclusive.
  - **Verdict per benchmark.**
    - Pass if `u ≤ m`.
    - Fail if `l > m`.
    - Inconclusive otherwise (`l ≤ m < u`): reported and escalated to the owner, never waived.
  - **Reporting.** Every attempt is retained and reported, including noisy, invalidated and failed runs.
- **JS measurement.** One bounded timing harness for regressor convolution (S0), with the same ratio and margin
  reported as evidence. Whether it gates is D3. This authorises no broader benchmark campaign.
- **S5 Remove.** Delete `linalg/Mat.scala` and `Vec.scala` and the `package.scala:10` export. Add `compileErrors`
  negative tests: `Mat` and `Vec` do not resolve from `hrf.*`, and `hrf.*` together with `gale.linalg.*` is unambiguous.
  Update `modules/hrf/README.md` and the design, model, and fit READMEs.

## 4. Compatibility and deprecation

Deprecation is compatible with `-Werror`. The build can suppress or scope specific deprecation warnings (for example
`-Wconf` per origin), so a deprecation cycle is a policy choice, not a technical impossibility. The chosen policy
(see D5) is **no deprecation cycle inside the repo**. Each slice migrates every in-repo caller of the surface it changes, atomically.
External consumers (eidolon, PLSNeuro) pin revisions, so they break only when their pin is bumped. Each breaking
slice (S1, S3, S5) records its breaking change and the one-line replacement in the PR body and module README.
A `private[scalafim]` bridge is **not** an external compatibility mechanism, because external consumers cannot
call it.
**[OWNER DECISION D5]** Choose one external policy:
- (a) Recommended: pin-gated breaking changes with the migration inventory and replacement examples from S1. S5 waits
  until eidolon and PLSNeuro have bumped past S3.
- (b) One release of public `@deprecated` forwarding bridges (`Mat.toDMat`, and so on), with `-Wconf` scoped to those
  forwarders.

## 5. Test and parity obligations (per slice)

- Numerical: existing R-parity fixtures unchanged, with explicit `assertEqualsDouble` tolerances. Paths without new
  arithmetic stay bitwise-identical.
- Shape and ownership: `DMatBuilder` misuse (a second `result()`), empty 0×k matrices, and the invariant that
  results never alias caller inputs.
- Platform: shared-suite placement on both JVM and JS. The JS `Float64Array` copy is exercised by at least one
  round-trip test per adapter.
- Performance: JVM JMH before and after for S4 (and for S3 design builders if they touch hot loops). Allocation
  profiling (`-prof gc`) for `evaluateInto`.
- Scenario harness: every touched scenario still returns a clean `Pass` (docs/plans/scenario-parity-harness.md).

## 6. Non-goals

- Replacing `Fft`/`Complex`: retained until Gale gains a justified cross-platform complex/FFT tier.
- The `Poly` Householder QR (separate mote), MVPA RSA solver (UMVPA-M2.07), and the motion/reframe4s migration.
- Reviving `scalafim.linalg`, or adding eigen/SVD/inverse helpers to domain modules.
- Redesigning design-matrix semantics or column schemas.
- Claiming a performance improvement from the type swap itself. S4 must *measure* one.
- Removing the unused `spire`/`JTransforms` deps. That is worthwhile, but it is a separate hygiene PR.
