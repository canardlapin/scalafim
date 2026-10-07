# Endpoint-anchored B-spline parity

Issue: `bd-01M43X88PVTG0A28RYC3X1TEJS`. Base: local main
`92033261fd82eb003b556c2b4a7af2c87551996d`.

## Decision and API

Add `Hrfs.BsplineConvention.EndpointAnchored` for fmrihrf commit
`18d418f26dd4547ca21ce11d12ea9e5e91a80b87`. Keep the default `LegacyR`
frozen to fmrihrf 0.4.0 and keep `Complete` for bases that must contain
constants. This resolves the drift through an explicit convention choice.

```scala
val hrf = Hrfs.bspline(nBasis = 5, span = 24.5.s, degree = 3,
  convention = Hrfs.BsplineConvention.EndpointAnchored)
```

The new convention constructs the complete clamped basis of width `N + 2`
and removes the first and last columns. It returns exactly `N` columns,
with knots uniform over the actual span and zero values at both endpoints
and outside support. It requires a positive finite span, `degree >= 1`, and
`N >= max(1, degree - 1)`. The positive-degree requirement follows R's
`splines::bs`, which rejects degree zero. Counts that overflow `N + 2` are
rejected. No column normalization is introduced. Constants cannot be
represented by this convention.

The descriptor uses the new parameter tag `endpoint-anchored` within
`hrf-descriptor/v2`. Existing legacy and complete identities are unchanged;
the identity suite pins literal parameter encodings for all three conventions.
The descriptor's basis count and piecewise-polynomial integration breaks
follow the new convention. Existing HrfSpec/formula shortcuts continue to
select the legacy default; direct `Hrfs.bspline` and `HrfFunctions.bsplineBasis`
calls select the new convention explicitly.

## Independent evidence

Run from the repository root:

```sh
Rscript docs/verification/hrf-endpoint-anchored-bspline-20261005/generate_fixture.R
```

The generator retrieves `R/hrf-functions.R` from the exact upstream commit,
checks source blob `69a6c8476de9b87e01bca6183eae32d639935c2c`, and evaluates
only its `hrf_bspline` definition with R `splines`. It archives that parsed
definition as `upstream_hrf_bspline.R`. Neither installed fmrihrf nor checkout
HEAD selects the reference behavior. Recorded versions: R 4.5.1 and splines
4.5.1. The generated fixture has nine configurations, 129 rows, and 899
values: degrees 1, 2, 3, 4, 8, and 12; minimum widths and interior knots;
integer and fractional spans down to 0.5; knot positions, midpoints,
off-grid points, endpoints, and out-of-support values. The generator also
checks upstream rejection of zero degree and undersized bases.

`EndpointAnchoredBsplineSuite` compares both the raw evaluator and the public
HRF against every fixture value at `1e-12`. Serialization uses 13 significant
digits, with absolute values below `1e-14` rounded to zero; times are
canonicalized before R evaluation. For values in `[0,1]`, the serialization
rounding bound is `5e-14`; the comparison tolerance also allows evaluation
roundoff across JVM and JS. Additional checks use the two analytic cubic
Bernstein polynomials, exact endpoint/support zeros, constructor rejections,
and analytic whole-window means for a fractional span.

`PrimitiveSuite` extends its independent Bernstein antiderivative checks to
the retained interior columns of minimum-width endpoint-anchored bases,
including partial windows and degrees through 32. Whole-support knot-integral
checks exercise interior knots and support clipping through degree 32 for
all three conventions. The old boundary-column tail checks remain applicable
to legacy and complete modes, which retain that column.

## Legacy fixture protection

The existing parity corpus, including `bspline_impulse`, remains unchanged.
`generate_fmrihrf_r_parity_fixtures.R` now checks the package version and
released legacy fractional-span/minimum-width goldens before generating any
output. This catches a newer checkout that still reports version 0.4.0.

Executed the full generator against an immutable archive of upstream
`e89ecc37c543afc0f9f2e3531cc3d56e67af5c06` (the parent of `18d418f`):
exit 0, generated 15 kernels, nine duration cases, and five regressor cases
to a temporary file. Against checkout `40c21257c0da333e651338ac258cfceec64656bf`:
exit 1 with the explicit frozen-fixture diagnostic, with no output file
created. R emitted locale and installed-testthat version warnings, and the
expected legacy minimum-width warning; these did not affect the checks.
Raw logs: `/private/tmp/scalafim-bspline-drift-20261005-r/`.

## Scala gates

Executed on the source changes in this checkout (uncommitted), based on
`92033261`. The resident sbt server reported sbt 1.11.7 and Homebrew Java
25.0.1; compilation uses the repository's Java 17 release target.

| Gate | Result |
|---|---|
| `hrfJVM/test` | 292 passed, 0 failed |
| `hrfJS/test` | 292 passed, 0 failed |
| `hrfLawsJVM/test` | 82 passed, 0 failed |
| `hrfLawsJS/test` | 82 passed, 0 failed |
| `designJVM/test` | 445 passed, 0 failed |
| `designJS/test` | 444 passed, 0 failed |
| `modelJVM/test` | 53 passed, 0 failed |
| `modelJS/test` | 53 passed, 0 failed |
| `scalafimCompileAll` | Exit 0, no source warnings or errors |

The full test logs contain no source warnings or errors. The first sandboxed
build attempt failed at the sbt cache lock, before compilation; the authorized
retry passed. Raw logs and command/exit metadata:
`/private/tmp/scalafim-bspline-drift-20261005-hrf-authorized.log` and
`/private/tmp/scalafim-bspline-drift-20261005-consumers.log`.
A SHA-256 source manifest is retained at
`/private/tmp/scalafim-bspline-drift-20261005-verified-source.json`.
No publication or complete repository test-suite qualification is claimed.
