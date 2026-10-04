# UMVPA M3.09 resource admission and cost probes

Source base: `47b5a851f970d27bb952668713fdaa3de3e7c847`, isolated branch
`work/umvpa-finish-20261001`. Mote packet:
`bd-01M2BNGMPD64GHF944S9FKME7S`. Final source identity is attached in Mote.

## Implemented boundary

`ResourceAdmission` uses BigInt shape/byte arithmetic before machine-width
conversion. It costs resident source/buffers, worker scratch, shared storage,
retained output and owned worker allocations. Strict whole-numeric admission
refuses unknown costs. Owned-numeric admission preserves excluded unknowns;
neither mode measures process RSS. Whole-numeric bounds are conditional on
supplied provider declarations and method-owned allocation formulas and
exclude objects, allocator/GC overhead and other processes.

Method-supplied equivalent routes retain the exact scientific PlanId through
`ExecutionPlan.admit`; a changed scientific binding is refused. Resource
selection can choose an admitted small dense route, but never changes the
population, metric or randomization count. Generic parallel costs scale
worker allocations and require a declared supported worker count. The
current observation-product seam refuses more than one worker.

`ObservationProduct` acquires only its own provider scope and expires raw
forward/adjoint and marked-fit access before closing it. It preserves the
original nominal coordinates, values identity and origins. Owned copying
streams one basis column at a time, never a neural-square identity, and
preflights Int capacity and explicit copy policy. Actual original-source
materialization reads/copy cells are separate from subsequent owned-product
applications. Counter receipts retain attempts and successful returned cells,
including failure paths; owned primitive copy-array counts are explicit.
These are adapter counters, not measurements of all backend allocations.

The vector-level adapter cannot certify arbitrary caller matrix input/output
widths. Those remain an explicit unknown unless an enclosing method admission
supplies their cost. The two-stage fitter supplies its fixed width bound.

Every `TwoStagePatternFit.fit` path performs resource admission before reader
acquisition. It combines source, response and numerical backend declarations
with covariance/residual allocation formulas, peak structured workspace and
retained pilot/final artifacts/history. Sequential workspace is charged once;
fit work accumulates. Full `StructuredPatternOptimizer.admit` is shared by
execution and the enclosing fitter, so deterministic geometry, axis, lineage,
replay, dimension, workspace and operator-column refusals precede dense source
preparation. Final noise-rank precision shapes are checked separately before
access, including `(p+h)*r`.

Direct cost probes bind actual evidence/lineage identities to immutable
`EvidenceExposure`. Unsupported assurance, unrelated evidence, unknown
external training footprint and materialized-route probes refuse before
callback execution. Cells and marked-fit counts are limited before the next
operation. `ObservationCostProbe` preserves the request, fit budget, source
identity, work delta and elapsed nanoseconds for successes, failures and
refusals. Timing excludes acquisition/preparation and is one sample, not a
universal latency bound or model-quality result. Dense-copy preparation must
be exposure-accounted by its owning caller; this direct-only probe does not
represent an earlier materialization as untouched access.

## Independent evidence and repairs

A read-only reviewer identified unrestricted output-width, per-worker scratch,
late structured refusal and mismatched/overstated probe exposure defects.
All were repaired, along with the final augmented precision shape overflow.
The reviewer approved the final source conditional on platform gates.

Tests check exact live-byte threshold arithmetic; unknown/overflow/unsupported
worker and source-copy refusals; actual dense/direct products; acquisition and
expiry; real failed-provider attempts; and pre-operation cell/fit limits.
The two-stage dense and direct fits preserve factors, covariance and framed
residual identity. Independent source counters match direct receipt columns;
the dense route makes exactly three original forward and zero adjoint reads
before fitting only the owned product. Declared provider/application costs
sum independently to 5,080 bytes beyond owned allocations. This validates
accounting plumbing, not independently measured provider memory.

The metadata-only overflow fixture has `n=20001`, `p=q=r=40000`, `h=20000`:
its other bounded dense shapes fit Int, but final precision needs
2,400,000,000 cells. It refuses without creating these matrices. A poison
source with a too-small structured column budget has zero acquisitions/reads.

Earlier failed drafts and targeted test failures remain in raw logs; they are
not treated as accepted evidence. Final qualification uses only the frozen
source gates below.

## Final verification

Final combined verification: gate90 passed 324 mvpa + 45 mvpa-fit + 107
mvpa-dataset + 20 mvpa-spatial JVM tests, plus 3 workflow and 5 atlas example
tests (504 total). Gate91 passed the same four Scala.js module counts, plus
2 workflow example tests (498 total). Combined: 1,002 tests.
`scalafimCompileAll` passed both platforms without warnings. Full raw logs
contain no warning/error lines. The 181 source/fixture entries in
`gate90-sources.json` remained unchanged across both gates. `git diff --check`
passed.

Raw log SHA-256:

- gate90: `eea466e8660f97e813458dbe13de0760d1712bf446492e46aa59868194d5d3f2`
- gate91: `27e3f972ba4bf867b380b2532657f8dd751ccbfa9b6072cb27ab8ad60b7fd653`

Evidence directory: `/private/tmp/scalafim-umvpa-finish-evidence-20261001`.

This packet establishes local resource admission, equivalent-route fixtures,
and instrumented scoped probe behavior. It does not qualify whole-brain
reference-workload latency, process peak-memory limits, statistical calibration,
full release, push or merge.
