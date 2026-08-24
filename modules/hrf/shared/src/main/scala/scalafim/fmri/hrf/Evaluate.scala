package scalafim.fmri.hrf

import scalafim.fmri.hrf.{Hrf, Seconds, s}
import scalafim.fmri.hrf.linalg.Mat

object Evaluate:

  /** Render a kernel on a lag axis — `grid` is displacement from onset, not
    * clock time. See [[Lag]].
    */
  def apply(
      hrf: Hrf,
      grid: Seq[Lag],
      amplitude: Double = 1.0,
      duration: Seconds = 0.0.s,
      precision: Seconds = 0.2.s,
      summate: Boolean = true,
      normalize: Boolean = false,
      integration: Integration = Integration.Exact
  ): Mat =
    require(grid.nonEmpty, "`grid` must be non-empty")
    require(grid.forall(_.value.isFinite), "`grid` must be finite")
    require(precision.value > 0.0, "`precision` must be > 0")

    // `normalize` peak-scales the *kernel*, over its own `[0, span]`, before
    // evaluation. It used to scale the output by the maximum observed on the
    // supplied `grid`, which made the answer depend on which time points the
    // caller happened to ask for: the same SPMG1 read 0.206 at t=2 on one grid
    // and 1.000 on another that merely omitted the peak.
    val kernel =
      if normalize then HrfCombinators.normalize(hrf)(precision) else hrf
    val nb = kernel.nbasis

    // `summate` selects the box convention, not a summation strategy:
    // true  -> unit-height box, mass grows with duration (R `summate = TRUE`);
    // false -> unit-mass box, the duration-average (R `summate = FALSE`).
    val pulse =
      Pulse
        .fromSummate(duration, summate)
        .fold(err => throw new IllegalArgumentException(err.message), identity)

    val data = new Array[Double](grid.size * nb)
    var gi = 0
    while gi < grid.size do
      val v = PulseResponse.at(pulse, kernel, grid(gi), precision, integration).data
      var j = 0
      while j < nb do
        data(gi * nb + j) = amplitude * v(j)
        j += 1
      gi += 1
    Mat.unsafe(grid.size, nb, data)

  def doubles(
      hrf: Hrf,
      grid: Seq[Double],
      amplitude: Double = 1.0,
      duration: Double = 0.0,
      precision: Double = 0.2,
      summate: Boolean = true,
      normalize: Boolean = false,
      integration: Integration = Integration.Exact
  ): Mat =
    apply(hrf, grid.map(Lag(_)), amplitude, duration.s, precision.s, summate, normalize, integration)
