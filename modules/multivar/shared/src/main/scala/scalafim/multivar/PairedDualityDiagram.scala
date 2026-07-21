package scalafim.multivar

/** A row-aligned pair of duality diagrams over a shared sample space.
  *
  * This is the common data geometry for paired latent analyses such as PLSC,
  * regularized CCA, and reduced-rank regression. The value is deliberately only
  * geometry: method-specific operators live above it.
  */
final case class PairedDualityDiagram private (
    x: DualityDiagram,
    y: DualityDiagram,
    sampleSpace: MvSpace
):
  def rows: Int =
    sampleSpace.size

  def xCols: Int =
    x.cols

  def yCols: Int =
    y.cols

  /** The single row metric shared by both diagrams.
    *
    * Construction requires `x.rowMetric.sameValues(y.rowMetric)`, so the x diagram's
    * metric is the canonical instance for every paired row-space operation.
    */
  def rowMetric: MetricSpec =
    x.rowMetric

object PairedDualityDiagram:
  @deprecated(
    "Equal row counts do not establish entity identity; use SameEntityEvidence.toLegacyPair or Unsafe.pairedDiagramFromArrays",
    "0.1.0"
  )
  def from(
      x: MatrixView,
      y: MatrixView,
      rowMetric: Option[MetricSpec] = None,
      xColumnMetric: Option[MetricSpec] = None,
      yColumnMetric: Option[MetricSpec] = None,
      sampleSpace: Option[MvSpace] = None,
      xSpace: Option[MvSpace] = None,
      ySpace: Option[MvSpace] = None
  ): Either[MultivarError, PairedDualityDiagram] =
    fromPositionalUnsafe(x, y, rowMetric, xColumnMetric, yColumnMetric, sampleSpace, xSpace, ySpace)

  private[multivar] def fromPositionalUnsafe(
      x: MatrixView,
      y: MatrixView,
      rowMetric: Option[MetricSpec] = None,
      xColumnMetric: Option[MetricSpec] = None,
      yColumnMetric: Option[MetricSpec] = None,
      sampleSpace: Option[MvSpace] = None,
      xSpace: Option[MvSpace] = None,
      ySpace: Option[MvSpace] = None
  ): Either[MultivarError, PairedDualityDiagram] =
    for
      _ <- requireEqualRows(x.rows, y.rows)
      resolvedSampleSpace <- resolveSampleSpace(x.rows, sampleSpace, rowMetric.flatMap(_.space))
      resolvedXSpace <- resolveObservedSpace("paired.x", x.cols, xSpace)
      resolvedYSpace <- resolveObservedSpace("paired.y", y.cols, ySpace)
      xDiagram <- DualityDiagram.from(
        x,
        rowMetric = rowMetric,
        columnMetric = xColumnMetric,
        rowSpace = Some(resolvedSampleSpace),
        columnSpace = Some(resolvedXSpace)
      )
      yDiagram <- DualityDiagram.from(
        y,
        rowMetric = rowMetric,
        columnMetric = yColumnMetric,
        rowSpace = Some(resolvedSampleSpace),
        columnSpace = Some(resolvedYSpace)
      )
      paired <- fromDiagrams(xDiagram, yDiagram, Some(resolvedSampleSpace))
    yield paired

  def fromDiagrams(
      x: DualityDiagram,
      y: DualityDiagram,
      sampleSpace: Option[MvSpace] = None
  ): Either[MultivarError, PairedDualityDiagram] =
    for
      _ <- requireEqualRows(x.rows, y.rows)
      resolvedSampleSpace <- sampleSpace match
        case Some(value) =>
          requireSampleSpace(value, x.rows).flatMap { _ =>
            requireSpace("x row", x.rowSpace, value).flatMap(_ => requireSpace("y row", y.rowSpace, value))
          }.map(_ => value)
        case None =>
          requireSameSampleSpace(x.rowSpace, y.rowSpace).map(_ => x.rowSpace)
      _ <- requireObservedSpace("x column", x.columnSpace)
      _ <- requireObservedSpace("y column", y.columnSpace)
      _ <-
        if x.columnSpace == y.columnSpace then
          Left(MultivarError.MatrixShapeMismatch("paired diagrams require distinct observed x and y spaces"))
        else Right(())
      _ <- requireMetricSpace("x row metric", x.rowMetric, resolvedSampleSpace)
      _ <- requireMetricSpace("y row metric", y.rowMetric, resolvedSampleSpace)
      _ <-
        if x.rowMetric.sameValues(y.rowMetric) then Right(())
        else
          Left(
            MultivarError.MetricMismatch(
              "paired diagrams must share one row metric, but the x and y row metrics differ in value"
            )
          )
    yield PairedDualityDiagram(x, y, resolvedSampleSpace)

  private def requireEqualRows(xRows: Int, yRows: Int): Either[MultivarError, Unit] =
    if xRows == yRows then Right(())
    else Left(MultivarError.MatrixShapeMismatch(s"paired diagrams expected equal rows, got $xRows and $yRows"))

  private def resolveSampleSpace(
      rows: Int,
      explicit: Option[MvSpace],
      metricSpace: Option[MvSpace]
  ): Either[MultivarError, MvSpace] =
    explicit match
      case Some(space) =>
        for
          _ <- requireSampleSpace(space, rows)
          _ <- metricSpace match
            case Some(tag) => requireSpace("row metric", tag, space)
            case None      => Right(())
        yield space
      case None =>
        metricSpace match
          case Some(space) =>
            requireSampleSpace(space, rows).map(_ => space)
          case None =>
            MvSpace.of("paired.samples", SpaceRole.Samples, rows)

  private def resolveObservedSpace(
      fallbackId: String,
      cols: Int,
      explicit: Option[MvSpace]
  ): Either[MultivarError, MvSpace] =
    explicit match
      case Some(space) =>
        for
          _ <- requireObservedSpace("observed", space)
          _ <- requireSpaceSize("observed", space, cols)
        yield space
      case None =>
        MvSpace.of(fallbackId, SpaceRole.Observed, cols)

  private def requireSameSampleSpace(left: MvSpace, right: MvSpace): Either[MultivarError, Unit] =
    for
      _ <- requireSampleSpace(left, left.size)
      _ <- requireSampleSpace(right, right.size)
      _ <- requireSpace("y row", right, left)
    yield ()

  private def requireSampleSpace(space: MvSpace, rows: Int): Either[MultivarError, Unit] =
    if space.role != SpaceRole.Samples then
      Left(MultivarError.MatrixShapeMismatch(s"paired sample space '${space.id.value}' must have role ${SpaceRole.Samples.label}"))
    else requireSpaceSize("sample", space, rows)

  private def requireObservedSpace(kind: String, space: MvSpace): Either[MultivarError, Unit] =
    if space.role == SpaceRole.Observed then Right(())
    else Left(MultivarError.MatrixShapeMismatch(s"$kind space '${space.id.value}' must have role ${SpaceRole.Observed.label}"))

  private def requireSpace(kind: String, actual: MvSpace, expected: MvSpace): Either[MultivarError, Unit] =
    if actual == expected then Right(())
    else
      Left(
        MultivarError.MatrixShapeMismatch(
          s"$kind space '${actual.id.value}' does not match shared sample space '${expected.id.value}'"
        )
      )

  private def requireSpaceSize(kind: String, space: MvSpace, size: Int): Either[MultivarError, Unit] =
    if space.size == size then Right(())
    else Left(MultivarError.MatrixShapeMismatch(s"$kind space '${space.id.value}' has size ${space.size} but expected $size"))

  private def requireMetricSpace(kind: String, metric: MetricSpec, sampleSpace: MvSpace): Either[MultivarError, Unit] =
    if metric.dim != sampleSpace.size then Left(MultivarError.MetricShapeMismatch(IndexAxis.Row, sampleSpace.size, metric.dim))
    else
      metric.space match
        case Some(space) => requireSpace(kind, space, sampleSpace)
        case None        => Right(())
