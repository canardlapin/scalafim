# Draft issues for github.com/bbuchsbaum/fmrihrf

Found while building an R-parity corpus for the Scala port (scalafim `modules/hrf`).
All numbers reproduced against installed fmrihrf 0.1.x on R 4.5.2.

---

## Issue 1 — `evaluate.HRF` and `evaluate.Reg` use different quadrature, so duration amplitude differs by ~1/precision

`evaluate.HRF` integrates the boxcar with trapezoid weights via
`.block_offsets_weights()` (R/utils-internal.R), so the result **converges** as
`precision -> 0`. The regressor path sums the microtime samples **unweighted**,
so it **diverges** as `precision -> 0`.

```r
library(fmrihrf)
g <- seq(0, 40, by = 2)
for (p in c(1.0, 0.5, 0.1, 0.05))
  cat(sprintf("evaluate.HRF  p=%.2f peak=%.5g\n", p,
      max(abs(evaluate(HRF_SPMG1, g - 10, duration = 4, precision = p)))))
#> evaluate.HRF  p=1.00 peak=5.8737
#> evaluate.HRF  p=0.50 peak=5.9199
#> evaluate.HRF  p=0.10 peak=5.9346
#> evaluate.HRF  p=0.05 peak=5.9350     <- converges

gg <- seq(0, 80, by = 2)
r <- regressor(onsets = c(10, 30), hrf = HRF_SPMG1, duration = 4)
for (p in c(0.5, 0.33, 0.1))
  cat(sprintf("evaluate.Reg  p=%.2f peak=%.5g\n", p,
      max(abs(evaluate(r, gg, precision = p)))))
#> evaluate.Reg  p=0.50 peak=13.078
#> evaluate.Reg  p=0.33 peak=18.977
#> evaluate.Reg  p=0.10 peak=60.584     <- diverges, ~1/precision
```

So the same `duration = 4` means two different things depending on which object
you evaluate, and for regressors the amplitude of every epoch-design column is a
function of the `precision` argument. At the default `precision = 0.33` an epoch
regressor is inflated ~3.2x relative to the integral.

Suggested fix: apply `.block_offsets_weights()` in the regressor path too, so
both paths compute `∫₀^d h(t-u) du`.

---

## Issue 2 — `summate = FALSE` and the two boxcar conventions

Related to #1. In `evaluate.HRF`, `summate = FALSE` divides by `sum(weights)`,
i.e. it produces the **duration-averaged** (unit-mass) box rather than the
unit-height one. That is a genuinely different and useful model, but the name
`summate` describes an implementation detail rather than the modelling choice,
and the regressor path's `summate = FALSE` takes a different route entirely.

Suggested fix: expose the two conventions as named constructors (unit-height vs
unit-mass boxcar) rather than a boolean, and make both paths agree.

---

## Issue 3 — `hrf_fourier` / `hrf_sine` error on scalar `t`

```r
hrf_fourier(-1, span = 24, nbasis = 4)
#> Error in basis[!in_support, ] <- 0 :
#>   incorrect number of subscripts on matrix
```

`vapply(..., numeric(length(t)))` returns a plain vector when `length(t) == 1`,
so the matrix-style subscript in `basis[!in_support, ] <- 0`
(R/hrf-functions.R:79, and the same pattern at :225) fails. Vector `t` works.

Suggested fix: `basis <- as.matrix(basis)` before masking, or
`drop = FALSE` handling.

---

## Issue 4 — `daguerre_basis` normalizes against the caller's `t` grid, so values depend on the query

R/hrf-functions.R:452-459 rescales each column by `max(abs(basis[,i]))` computed
over whatever `t` was passed in. The kernel therefore returns different values
for different query grids:

```r
f <- fmrihrf:::daguerre_basis
f(0, n_basis = 3, scale = 4)[1]                      #> 1
f(seq(-2, 32, by = 0.5), n_basis = 3, scale = 4)[5]  #> 0.7788  (this is t = 0)
```

Including negative lags inflates the divisor (`exp(-x/2)` grows as `t` goes
negative), shrinking every reported value. Two analyses on the same data with
different sampling grids get different Daguerre regressors.

Suggested fix: normalize against a fixed reference grid determined at
construction (e.g. `seq(0, span, length.out = n)`), not against the argument.

---

## Issue 5 — `HRF_BSPLINE` knots are quantiles of the caller's time vector, so it is not a function of `t`

`hrf_bspline()` (R/hrf-functions.R:65-77) passes explicit knots derived from the
span — `quantile(seq(0, span), ...)`, i.e. `(8, 16)` for span 24.

`hrf_bspline_generator()` (R/hrf.R:782-783), which is what builds `HRF_BSPLINE`,
calls `splines::bs()` with **no `knots =` argument**:

```r
splines::bs(t[valid_t_idx], df = effective_nbasis, degree = degree,
            Boundary.knots = c(0, span), intercept = FALSE)
```

so `bs()` falls back to `knots <- quantile(x[!outside], c(1/3, 2/3))` over
*whatever `t` was supplied*. The two definitions therefore disagree, and the
generator one is grid-dependent:

```r
# regressor fine grid: seq(0, 24, by = 0.33) ends at 23.76, not 24
# -> interior knots (7.92, 15.84) instead of (8, 16); ~1% column difference
evaluate(HRF_BSPLINE, 0.33)
#> knots collapse onto the single supplied point
#> (0, 0.98625, 0.01375, 0, 0)
```

Consequences:
- the same experiment analysed at two TRs gets two different B-spline bases;
- `evaluate(HRF_BSPLINE, t)` for a scalar or short `t` is degenerate;
- R's own `method = "loop"` and `method = "conv"` disagree for this basis,
  because `eval_loop` (R/evaluate-helpers.R:283-296) evaluates on per-event
  relative grids and so derives yet another knot set.

`hrf_tent_generator` (R/hrf.R:829-833) routes through `hrf_bspline()` with
explicit knots and is unaffected.

Suggested fix: pass explicit `knots =` in `hrf_bspline_generator`, matching
`hrf_bspline()`, so the basis is fixed at construction.

---

## Issue 6 (lower priority) — several kernels respond before the event

`HRF_GAUSSIAN`, `hrf_mexhat`, `hrf_inv_logit` and `hrf_lwu` are non-zero at
negative lag, while `HRF_SPMG1`, `HRF_GAMMA`, `HRF_FIR`, `HRF_BSPLINE` and
`hrf_fourier` correctly return 0:

```
            t=-10      t=-5       t=-1       t=-0.1
gaussian    2.5e-15    5.4e-08    4.4e-04    1.9e-03
inv_logit   1.1e-07    1.7e-05    9.1e-04    2.2e-03
mexhat      4.0e-13    4.0e-06    1.2e-02    4.0e-02
lwu         3.6e-07    5.5e-05    1.6e-02    4.4e-02
```

In the regressor path negative lags are masked, so ordinary design matrices are
unaffected. But `gen_hrf(..., width =)` samples `h(t - offset)` unguarded, so
blocking one of these kernels mixes in pre-onset values. `hrf_mexhat` at 4% of
peak at lag -0.1 is not negligible.

Suggested fix: apply the same `in_support` mask these already have for
fourier/sine, or gate on `t >= 0` in the shape functions.
