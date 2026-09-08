# Inspect a response basis

`ResponseBasisReview` evaluates native HRF coordinates for model review.
`ResponseBasisGraphics` turns that review into an Intaglio plot without loading
signal data or requiring a platform renderer. Use these APIs to show a researcher
what their canonical, derivative or FIR components mean before fitting.

The following example reviews SPMG3 and its mean response between four and eight
seconds. Display samples and exact readout weights are separate products.

```scala
import scalafim.fmri.hrf.*
import scalafim.fmri.design.{ResponseBasisReview, ResponseBasisGraphics}

val preview = ResponseBasisReview.make(
  Hrfs.SPMG3,
  functional = Some(ResponseFunctional.WindowMean(4.s, 8.s))
).fold(error => throw IllegalArgumentException(error.message), identity)

val plot = ResponseBasisGraphics.plot(preview, width = 800, height = 300)
  .fold(error => throw IllegalArgumentException(error.message), identity)
val scene = plot.scene
assert(scene.semantics.plots.nonEmpty)
assert(preview.basisIds.size == 3)
assert(preview.readout.exists(_.weights.size == 3))
```

Present `Either` failures in an editor's validation state; the example throws
only to keep a small executable demonstration readable. Custom kernels without
an exact integration primitive cannot supply exact window readouts. Their error
is retained rather than silently replacing the readout with display-grid
quadrature. Exceptions from custom kernel evaluation become `KernelEvaluation`.

Every curve retains its native basis identity, role and unnormalized values.
Its horizontal coordinate is elapsed response time in seconds, relative to an
event. This is a basis preview; the fitted design also depends on events,
convolution, run sampling and column scaling. Derivative coefficients are shape
coordinates, not additional condition amplitudes. Use the fit module's native
structural response hypotheses to read out a fitted response and propagate its
covariance; the preview does not estimate signal or uncertainty.

FIR curves retain physical interval steps, including when the display grid is
too coarse to sample a bin's interior. A wide basis can be shown a few
coordinates at a time:

```scala
import scalafim.fmri.hrf.*
import scalafim.fmri.design.{ResponseBasisReview, ResponseBasisGraphics}

val fir = ResponseBasisReview.make(
  Hrfs.fir(nBasis = 18, span = 36.s),
  Some(ResponseFunctional.WindowIntegral(4.s, 8.s)),
  samples = 2
).fold(error => throw IllegalArgumentException(error.message), identity)
val firstPage = ResponseBasisGraphics.plotSelected(fir, fir.basisIds.take(6))
  .fold(error => throw IllegalArgumentException(error.message), identity)
assert(fir.readout.exists(_.weights.size == 18))
assert(firstPage.scene.semantics.plots.nonEmpty)
```

Selecting curves changes the display only. Readout weights still use the full
basis axis: point and window-mean outputs have response-value units; window
integrals have response-times-seconds units. Gray vertical lines mark the
requested point or window boundaries. Plot and scene descriptions retain basis
identities, native values, physical times and the exact functional receipt for
accessible views and semantic exports.

The preview defaults to 257 samples and a budget of 200,000 sampled values.
The plot accepts at most 12 selected curves by default and rejects empty,
duplicate or unknown selections. Explicit budgets can be changed by a host;
they are resource and display choices, not changes to the model or estimator.
Render at the actual target dimensions through an Intaglio backend.
