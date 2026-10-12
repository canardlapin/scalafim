package scalafim.phrfcmp.ingest

/** Owner/harness preparation only. The fitters still receive truth-free FitInputs. */
object NoDeviationHarness:
  def inputs(bound: BoundDataset): FitInputs =
    require(bound.manifest.rootKind == RootKind.Harness && bound.manifest.cell.kind == CellKind.Trial)
    val fit = bound.fit
    val signal = bound.truth.signal
    val mean = bound.truth.signalConditionMean.getOrElse(throw new IllegalArgumentException("trial condition mean absent"))
    val y = Matrix.of(fit.y.rows, fit.y.cols, Array.tabulate(fit.y.data.length)(i =>
      fit.y.data(i) - signal.data(i) + mean.data(i))).fold(e => throw new IllegalArgumentException(e.message), identity)
    fit.copy(y = y)
