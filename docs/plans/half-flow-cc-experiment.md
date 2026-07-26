# HalfFlow-CC falsification protocol

- Epic: `bd-01KY514GPNG0P1W5AJ1TW96CEP`
- Gate: `bd-01KY517059SYQ3429D6G4WSNH5` (G0)
- Machine freeze:
  [`../benchmarks/receipts/half-flow-cc-g0-control-2026-07-22.json`](../benchmarks/receipts/half-flow-cc-g0-control-2026-07-22.json)
- Result schema:
  [`../benchmarks/schemas/half-flow-cc-result-v1.schema.json`](../benchmarks/schemas/half-flow-cc-result-v1.schema.json)

HalfFlow-CC is a falsifiable successor candidate, not a silent retuning of the
current engine. The exact-affine HalfFlow-LM sources and sub18 result are frozen
as historical control `E0`. The fair A-E experiment reruns the old algorithm as
lane E through the same native-input and supplied-affine harness as every other
lane.

## Claims this experiment may make

The experiment may determine whether the observed accuracy gap is primarily
caused by the metric, the hard sampled-inverse gate, midpoint action, or the
combined current implementation. It may select, reject, or narrow HalfFlow-CC.

One sub18 pair may validate plumbing and reproduce a regression. It cannot
establish anatomical accuracy, statistical parity, or a generally applicable
speed ratio. Architecture selection requires at least five labeled development
pairs; an ANTs-parity claim requires a larger predeclared cohort and paired
statistical criterion.

## Frozen historical control E0

`E0` is the retained exact-affine, multi-window, multi-channel 3x3 LM engine
captured in `half-flow-factored-affine-p7-2026-07-22.json`. Its selected source
hashes still match the live shared checkout. Its sub18 result is diagnostic:

- Dice: 0.9325895815;
- global NCC: 0.9177628055;
- local NCC: 0.5452558344;
- gradient NCC: 0.2808158787;
- nonlinear RMS: 0.697649 mm;
- zero sampled folds;
- forward/backward inverse maxima: 0.404245/0.367892 mm;
- 24 accepted of 58 attempted steps; and
- 23.943 seconds end to end on the captured JVM run.

E0 estimated its own robust affine and pre-resampled the moving image onto the
fixed lattice before nonlinear optimization. It is therefore a historical
regression target, not a fair A-E observation.

## Shared inputs and affine

The frozen sub18 plumbing case uses the four immutable NIfTI inputs recorded in
the G0 receipt. The fixed mask is optimization support. The moving mask is
evaluation-only and MUST NOT be available to the objective, support builder,
proposal generator, or acceptance logic.

The common supplied-affine artifact is the hashed ITK 3D affine emitted by the
captured ANTs run. This choice is intentionally conservative: it may favor the
ANTs initialization, but every nonlinear lane receives exactly the same map.
Before the artifact is admitted, its ITK/LPS-to-ScalaFIM adapter must match
`antsApplyTransforms` on physical landmarks and a resampled-image oracle. An
unverified matrix conversion is not a supplied affine.

All A-E lanes:

- read independent fixed and moving native pyramids;
- receive the same verified supplied affine;
- use the fixed mask as optimization support;
- exclude the moving mask and labels from optimization;
- use accepted-step budgets 10, 8, and 6 at shrink 4, 2, and 1;
- write large outputs to isolated work directories and commit only hashes and
  compact receipts; and
- emit the v1 result schema, including unavailable measurements with a reason.

The existing Docker comparator remains useful but is labeled
`linux/amd64-under-Apple-Silicon-emulation`. Its 49.915-second result is not a
native-hardware speed baseline. The fair nonlinear comparator is a new
SyN-only fixed-mask profile initialized from the same verified affine; the
existing full rigid-affine-SyN result remains historical comparator `ANTS0`.

## Falsification lanes

| Lane | Metric and derivative | Pointwise step | Action | Geometry/controller policy |
| --- | --- | --- | --- | --- |
| A | true squared neighborhood CC, adjoint boxes | rank one | midpoint | whole-grid Gaussian, separated controls |
| B | lane A | rank one | midpoint | lane A plus old accumulated-inverse hard rejection |
| C | true squared neighborhood CC, adjoint boxes | rank one | one direction | otherwise lane A |
| D | standardized-center surrogate | rank one | midpoint | otherwise lane A |
| E | frozen multi-window standardized features | multi-channel 3x3 | midpoint | current masked Sobolev and trust/inverse policy |

Lane A versus B isolates inverse strangulation. A versus C tests midpoint
action. A versus D isolates the objective. A versus E compares the complete
smaller candidate with the retained algorithm. A true-CC 3x3 lane is forbidden
until a residual factorization is specified and independently differentiated.

## Evidence and troubleshooting contract

Every run records these groups even when a value is unavailable:

1. provenance: repository/source hashes, toolchain, host/runtime class, thread
   count, seed, and contention classification;
2. inputs: hashes and roles for images, optimization support, and
   evaluation-only labels;
3. affine: artifact identity, conversion status, and consumers;
4. configuration: levels, metric, step, geometry, flow, controller caps, and
   accepted/attempted budgets;
5. objective and anatomy: loss, global/local CC, label Dice/surface metrics,
   and TRE where available;
6. support: active fraction, edge-band active fraction, and support/variance
   diagnostics;
7. topology and inverse: determinant distribution, fold counts, export error
   percentiles/maxima, and typed export failure;
8. controller history: damping, step scale, squarings, retries, and rejection
   counts by reason; and
9. performance: phase times, total time, allocations when measurable, peak
   RSS, and artifact hashes.

Interpret failures by group. Metric-value or derivative failures stop before
optimization. Jacobian failures modify geometric step scale. Paired-flow
accuracy modifies squaring depth. Inverse-cache drift requests repair. Export
inversion either meets its configured tolerance or returns a typed failure.
None of these numerical events is allowed to masquerade as an LM damping
observation.

Validate the frozen protocol from the repository root:

```sh
python tools/registration/validate_halfflow_cc_protocol.py
```

The default historical check validates the JSON Schema and a conforming minimal
result, the recorded source and protocol hash inventories, frozen external
inputs, lane identities, dependency-sensitive fairness rules, and the
evaluation-mask firewall. Its in-memory adversarial checks also prove that a
moving-mask leak and a lane A/B metric drift are rejected. Use
`--live-sources` as an explicit drift audit when the current checkout is
expected to match the frozen source and protocol hashes.
