# Trial constrained log-determinant jet (2026-09-30)

Mote: `bd-01M3RB3GQ2RBTTDB652CFZ1AJN`, a PHRF-11 ML prerequisite. Base: `041514a6`
(committed AR and native prerequisites plus Stage 1). Gale comes from the accepted
local override `-Dscalafim.gale.build=/private/tmp/scalafim-execution-20260929/gale-logdet`
(base `18d24dbb` plus the accepted `BandedLogDetJet` source). These tests are
local-provider evidence, not hosted-pin qualification.

## Scope

`TrialConstrainedLogDetJet.evaluate(input, policy)` returns the value, gradient
and full Hessian (actual second derivatives) of

    D = log|A| - (N-C) log lambda + log|M'A⁻¹M| - log|M'M|,   A = X'X + lambda I,

which equals `log|K|` for `K = I + X P Xᵀ / lambda` with the unprojected whitened
trial design.

## Factor/band identity (repair of independent review of 28382eae)

The first candidate accepted any factor whose size and bandwidth matched,
together with a separately supplied A band. A residual check cannot prove that
they belong together: a close same-shape SPD band passes it. The input now
accepts no factor and no A band. Its only source is `TrialAcceptedTrialBand`,
a stamped bundle with a private constructor. `TrialAcceptedTrialBand.factorize`
(or `atPreparationLambda`):

- validates dimension, coordinates, lambda, band shape and finite active entries
  (a validation refusal is charged as `constructionRefusals`, with no factor
  attempt);
- copies the active lower band into canonical owned storage, zeroing padding,
  and freezes it;
- factors exactly that frozen copy with Gale `factorLower`, charged as
  `factorAttempts`/`factorFailures` in a construction receipt separate from
  evaluation;
- binds preparation, coordinates, lambda, the frozen band and the factor.

`TrialDeterminantInput(accepted, first, second)` validates only the derivative
bands. Evaluation reuses the bundle's factor and frozen band and performs no
further N-sized factorisation. The residual check remains an empirical
solve-quality admission and is not used as identity evidence.

## Evaluation

With the accepted factor of the bundle's own band:

- `U = A⁻¹M`, `U_p = -A⁻¹A_pU`, `U_pq = -A⁻¹(A_pqU + A_pU_q + A_qU_p)`; the
  same-axis case carries two `A_pU_p` terms.
- Gale `BandedCholesky.logDetJet` for `A`, and for the small `B = M'U`, whose
  lower triangle is packed as a full band.
- ScalaFIM's row-packed second jets (00, 01, 02, 11, 12, 22) are remapped by
  explicit `(p, q)` into Gale's column packing (00, 01, 11, 02, 12, 22).

It performs no N-sized factorisation, builds no dense N x N or T x T matrix, uses
no domain inverse or trace helper, and applies no jitter, clipping, floors or
clamps. Every banded solve is admitted by the max-column normalized backward
residual against the original `A` bands, including lambda, with tolerance
`1e-10`. That is an empirical solve-residual check, not a forward-error or
scientific certificate. Refusals are typed and carry the work already charged.

This helper does not enable public ML. The criterion adapter, the owner's
coherent reference seam, executor wiring and the early ML refusal all stay with
their owners.

## Evidence

The shared `TrialConstrainedLogDetJetSuite` (12 tests) runs on JVM and JS.

- The oracle is independent: whitened time-domain X from the design source and
  an explicit two-run AR(1) recurrence, `K = I + (XP)(XP)ᵀ/lambda`, and `log|K|`
  from Gale's dense Cholesky. Centered finite differences of that dense value
  give every gradient and Hessian entry at h = 2e-3 and 1e-3.
- Off-node Gaussian 2D and Cascade34 3D fixtures have unequal counts, a
  singleton condition, nuisance and two runs. D agrees with the dense value to
  about 1e-15. Finite-difference errors are about 1e-6 at h = 2e-3 and fall
  about 4x on halving (asserted to fall by more than 1.67x).
- In 3D, `|H02 - H11|` is about 0.022, more than 1000x the oracle error, so an
  unremapped packing cannot pass.
- The value matches the accepted scalar `logDetAtNode` at a prepared node.
- D, g and H are unchanged by the nuisance design, by coincident trials plus a
  complete-record permutation, and by the scaling `X* = sX` with
  `lambda* = s² lambda`. With N = C (P = 0), D, g and H vanish.
- The predeclared lambda values 1e-6, 1 and 1e4 are all admitted, with maximum
  residual about 1e-16, and match the dense value (the worst relative difference
  is about 1e-11, at lambda = 1e-6).
- The work receipt is exact: `1 + d + d(d+1)/2` banded solves, C RHS columns
  each, a residual check per solve, the stated band-product columns, one small
  factor, two log-determinant jets, and `(d+4)·N·C` scratch doubles.
- Construction refusals (invalid lambda, dimension, coordinate, band shape,
  nonfinite active value entry) carry receipt (1, 1, 0, 0, 0). A non-SPD band
  charges and fails its single factor attempt, (1, 0, 1, 1, 0). An admitted
  bundle retains N·(b+1) doubles. Derivative count, shape and nonfinite active
  entries are refused before evaluation; NaN padding is ignored.
- Identity controls:
  - Type level: the pre-repair constructor shape
    `TrialDeterminantInput(prep, at, lambda, factor, value, first, second)` and
    `new TrialAcceptedTrialBand(...)` do not compile (munit `compileErrors`).
  - Executable: A2 = A1 + 1e-13 I passes the old hybrid residual check
    (U from A1's factor against A2) below 1e-10. The stamped bundle for A2 holds
    the factor of its own frozen band.
  - Copy: source NaN padding is not retained, active entries match bit for bit,
    a closed source builder refuses writes, and repeated evaluation is
    identical.
- Planted bugs (12 tests): copying Gale's packing without the remap fails 2;
  dropping one same-axis `A_pU_p` term fails 3; retaining the caller's source
  matrix instead of the frozen copy fails 1; a public bundle constructor fails 1.
  The last is caught only when the suite is recompiled, because the check is a
  compile-time assertion and the incremental compiler does not track a
  string-literal reference. Its run therefore uses `fitJVM/Test/clean`, and so
  does the final gate.

Logs, metadata and source hashes are in the Mote handoff.
