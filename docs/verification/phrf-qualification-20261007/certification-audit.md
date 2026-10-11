# PHRF-11 certification feasibility audit

This audit retains the PHRF-11 contract. `CertifiedOriginalEquations` must
continue to refuse: neither a small prepared-basis residual nor a validated
spectrum of a different finite matrix supplies the missing certificate.

## Existing capabilities and missing evidence

| Requirement | Existing implementation | Gap |
| --- | --- | --- |
| Actual selected shape and physical axes | `ProfileTrialReference`, `ProfileTrialAxis`, public execution declaration | Preserve these bindings through every future enclosure. |
| Directional predictor plus one correction | `TrialConditionalSolve` with work receipts and local cubic laws | An eight-corner bank does not control error over the full chart. The new noiseless controls exhibit order-one amplitude errors. |
| Exact conditional readout | Explicit `ExactShape`; independent augmented QR agrees | It factors at the selected shape and solves the prepared model. It is not the bounded corrected path or an original-family certificate. |
| Spectral validation | Pinned Gale `ValidatedSvd.enclosure`, `MatrixEnclosure`, `RealInterval` | Full/economy dense validation is an oracle capability. An admitted, bounded preparation-time enclosure for each production reference neighborhood is still needed. |
| Finite normal-equation certificate | Gale `FiniteSymmetricSolveBounds` and outward interval arithmetic | The Gershgorin margin requires strict diagonal dominance. General penalized trial/nuisance systems need not satisfy it; positivity cannot be inferred from a successful factorization alone. |
| Kernel basis representation | `KernelBasisCertificate` held-out value and derivative maxima | These are sampled evidence, not uniform cell bounds. |
| Tail | `tailRelativeEnergyEither` finite-extent quadrature diagnostic | Explicitly documented as not a truncation guarantee. A 48-second basis has measurable observation error for slow Cascade34 tails. |
| Sampling and convolution | EventTerm microtime convolution, fixed precision, original event identities | Enclose sampling/interpolation/roundoff under the declared target equations, including duration and onset conventions. |
| Whitening | Whole-column recurrence preserves AR/MA tails | Bound amplification and recurrence arithmetic; do not substitute unwhitened kernel error. |
| Query and normalization | Signed queries, native nuisance units, transported lambda | Turn coefficient/energy bounds into absolute query bounds, including cancellation and normalization error. Float32 conversion remains separate. |

The Gale provider is **not missing a generic eigensolver or SVD**. Adding another
solver in ScalaFIM would not repair these gaps. The new audit actually invokes
Gale's validated SVD on each finite original-observation augmented matrix. Its
strictly positive lower singular-value bounds establish conditioning of that
specific finite array only. Treating its computed entries as exact does not
enclose the preceding family evaluation, convolution or whitening.

## Required mathematical composition

Let `G` and `b` be the original target normal equations, and let `Ghat`, `bhat`
be the prepared equations at the same selected shape and physical geometry.
For candidate `a`, the original residual obeys

```
||b - G a|| <= ||bhat - Ghat a|| + epsilon_b + epsilon_G ||a||,
```

provided the first norm includes arithmetic error and `epsilon_b`, `epsilon_G`
are genuine upper bounds on the corresponding differences. If a certified
lower spectral bound `mu > 0` applies to `G`, then

```
||a - a_original|| <= residual_upper / mu
|q' (a - a_original)| <= ||q|| residual_upper / mu.
```

Alternatively the reference-energy argument in the plan uses
`G >= (1-eta) G0`, with `eta < 1`, and one additional reference inverse
application for `r' G0^-1 r`. Certification must count that inverse, matrix
actions, arithmetic enclosures and scratch. Dense per-voxel covariance
preparation cannot be hidden inside this step.

For an observation design enclosure `A = Ahat + Delta`,
`||Delta|| <= epsilon_A` implies
`||A'A - Ahat'Ahat|| <= 2 ||Ahat|| epsilon_A + epsilon_A^2`.
The enclosure of `Delta` must cover **all** family/basis, interpolation,
observation-tail and whitening effects. Nuisance and penalty conventions must
stay identical. Signed-query bounds use absolute errors even when the query
nearly cancels; no relative-only certificate is admissible.

All displayed inequalities are mathematical requirements, not claims that the
current floating-point code has certified their premises.

## Executed diagnostic and disposition

`TrialQualificationAudit` compares the prepared basis, the direct family
truncated at 48 seconds, and direct family convolution through the complete
600-second acquisition window. Events are impulses on the existing 0.1-second
grid, with fixed AR(1)=0.3 whitening and six nuisance columns. Its full-window
oracle remains a **sampled finite observation model**, not a continuous-time
integration certificate. Longer runs, nonzero durations, other families and
run-boundary cases require their own enclosures.

Four interior shapes are tested with signed condition amplitudes, a matched
zero-noise control, heterogeneous trial amplitudes, and an AR noise ratio of
0.5. Both exact and corrected readouts are audited against independent dense
augmented least squares at the **returned** shape. Every refusal is retained.
The same prepared model and decoder are used for both readout modes.

The results disprove admission of the current corrected eight-node path over
this chart. They also show that excellent agreement with prepared equations
does not certify the original family. PHRF-11 remains in review, with no
change to public certificate admission, tolerances, decoder budgets or
scientific acceptance thresholds.

The next implementation must provide an observation-error envelope and a
reference-neighborhood bound, with explicit refusal outside the neighborhood.
Increasing reference density has a shared-memory cost; increasing the horizon
has bandwidth and preparation costs. Exact readout remains an explicit option
whose work must be measured. None of these tradeoffs can be declared resolved
by this exploratory audit.
