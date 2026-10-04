# Whitening transpose (2026-09-29)

Mote: `bd-01M3R659ZJRDY3JTCCHR9JKJ78`, a PHRF-11 prerequisite for the conditional
operator adjoint on the original selected response. Base: integration `1d2bdf5d`.

## Change

`WhiteningTransform.transposeMatrix(plan, input): Either[ArError, DMat]` applies
`Wᵀ`, the adjoint of the existing linear map `WhiteningTransform.matrix(plan, _)`.
It is not inverse whitening.

The forward recurrence is `e_t = s_t (y_t - Σ φ_l y_{t-l} - Σ θ_l e_{t-l})`,
restricted to one segment, where `s_t` is the first-sample scale at a segment
start and `1` elsewhere. The adjoint handles each segment independently and each
column separately. It seeds `ē` from the input and walks `t` downward:

```
z = s_t ē_t;  ȳ_t += z;  ȳ_{t-l} -= φ_l z;  ē_{t-l} -= θ_l z   (lags within the segment)
```

The scale multiplies the fully accumulated `ē_t`, before MA propagation. Rows and
per-segment scales are validated with the forward map's typed errors, before any
output is written. A zero precomputed scale gives a singular forward map, whose
adjoint is still valid. Scratch is one array of the longest segment length; the
input is read through its logical strides and never modified. The forward code
is unchanged.

## Evidence

Shared `WhiteningTransposeSuite` (7 tests), run on JVM and JS together with the
existing `WhiteningPlanSuite` (12 tests):

- The oracle is an independent dense `W = (I + S B)⁻¹ S A`, built from the
  defining equations and inverted with Gale's dense solve. It is first checked
  against the production forward map.
- `transposeMatrix` equals `denseWᵀ z`, and the dot-product law holds against the
  production forward map. The cases are IID, AR(1) exact, AR(3), MA(2), ARMA(2,1)
  with scale 0.6, ARMA(1,2) with scale 0, and a by-run AR(1)/ARMA pair. They cover
  global and by-run scopes, two runs, and a censoring reset inside a run.
- Zero scale: the start rows of dense `W` are zero, and the adjoint of indicator
  inputs is zero.
- Strided inputs (transposed storage and a sliced view padded with NaN) give
  bit-identical results. The input is unchanged, and zero-column input is
  supported.
- Typed refusals match the forward map for row mismatch and zero rows. Plans
  refuse a negative precomputed scale and a mismatched by-run coefficient count.
- Planted bugs: scaling the raw input before MA accumulation, and dropping the
  reverse MA propagation, each fail 3 of the 7 new tests.

Logs, metadata and source hashes are in the Mote handoff. No consumer
conditional operator, ML, or scientific qualification is claimed.
