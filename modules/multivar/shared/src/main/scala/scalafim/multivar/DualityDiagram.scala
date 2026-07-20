package scalafim.multivar

import gale.linalg.DMat

/** A metric-weighted data table `(X, D, Q)` in the duality-diagram sense.
  *
  * `table` is `X`, `rowMetric` is `D`, and `columnMetric` is `Q`. The value is
  * intentionally small: construction checks the table, spaces, and metrics once,
  * then exposes the canonical row/column operators used by generalized PCA and
  * related multivariate methods.
  */
final case class DualityDiagram private (
    table: MatrixView,
    rowSpace: MvSpace,
    columnSpace: MvSpace,
    rowMetric: MvMetric,
    columnMetric: MvMetric
):
  def rows: Int =
    table.rows

  def cols: Int =
    table.cols

  /** `X' D X`, the column-side Gram form induced by the row metric. */
  def rowGram(policy: StoragePolicy = StoragePolicy.AllowDense): Either[MultivarError, DMat] =
    DualityKernels.rowGram(this, policy)

  /** `X Q X'`, the row-side Gram form induced by the column metric. */
  def columnGram(policy: StoragePolicy = StoragePolicy.AllowDense): Either[MultivarError, DMat] =
    DualityKernels.colGram(this, policy)

  /** `X Q X' D`, the row-space endomorphism of the duality diagram. */
  def rowOperator(policy: StoragePolicy = StoragePolicy.AllowDense): Either[MultivarError, DMat] =
    columnGram(policy).flatMap(DualityKernels.multiplyMetricRight(_, rowMetric))

  /** `X' D X Q`, the column-space endomorphism of the duality diagram. */
  def columnOperator(policy: StoragePolicy = StoragePolicy.AllowDense): Either[MultivarError, DMat] =
    rowGram(policy).flatMap(DualityKernels.multiplyMetricRight(_, columnMetric))

  /** `tr(X' D X Q)`, often called total inertia in the duality-diagram literature. */
  def totalInertia(policy: StoragePolicy = StoragePolicy.AllowDense): Either[MultivarError, Double] =
    DualityKernels.totalVariance(this, policy)

  def totalVariance(policy: StoragePolicy = StoragePolicy.AllowDense): Either[MultivarError, Double] =
    totalInertia(policy)

  /** Swap row and column geometry through a lazy transposed view; materialization is
    * deferred until a later operation explicitly requests dense storage.
    */
  def transpose(): Either[MultivarError, DualityDiagram] =
    DualityDiagram.from(
      table.transposeView,
      rowMetric = Some(columnMetric),
      columnMetric = Some(rowMetric),
      rowSpace = Some(columnSpace),
      columnSpace = Some(rowSpace)
    )

object DualityDiagram:
  def from(
      table: MatrixView,
      rowMetric: Option[MvMetric] = None,
      columnMetric: Option[MvMetric] = None,
      rowSpace: Option[MvSpace] = None,
      columnSpace: Option[MvSpace] = None
  ): Either[MultivarError, DualityDiagram] =
    for
      _ <- requirePositiveAxis(IndexAxis.Row, table.rows)
      _ <- requirePositiveAxis(IndexAxis.Column, table.cols)
      resolvedRowSpace <- resolveSpace(
        axis = IndexAxis.Row,
        explicit = rowSpace,
        metricSpace = rowMetric.flatMap(_.space),
        fallbackId = "duality.rows",
        fallbackRole = SpaceRole.Samples,
        size = table.rows
      )
      resolvedColumnSpace <- resolveSpace(
        axis = IndexAxis.Column,
        explicit = columnSpace,
        metricSpace = columnMetric.flatMap(_.space),
        fallbackId = "duality.columns",
        fallbackRole = SpaceRole.Observed,
        size = table.cols
      )
      resolvedRowMetric <- resolveMetric(IndexAxis.Row, table.rows, rowMetric, resolvedRowSpace)
      resolvedColumnMetric <- resolveMetric(IndexAxis.Column, table.cols, columnMetric, resolvedColumnSpace)
    yield DualityDiagram(table, resolvedRowSpace, resolvedColumnSpace, resolvedRowMetric, resolvedColumnMetric)

  private def requirePositiveAxis(axis: IndexAxis, size: Int): Either[MultivarError, Unit] =
    if size > 0 then Right(())
    else Left(MultivarError.InvalidDimension(s"duality diagram ${axis.label} count", size))

  private def resolveSpace(
      axis: IndexAxis,
      explicit: Option[MvSpace],
      metricSpace: Option[MvSpace],
      fallbackId: String,
      fallbackRole: SpaceRole,
      size: Int
  ): Either[MultivarError, MvSpace] =
    explicit match
      case Some(space) =>
        for
          _ <- requireSpaceSize(axis, space, size)
          _ <- metricSpace match
            case Some(tag) if tag != space =>
              Left(
                MultivarError.MatrixShapeMismatch(
                  s"${axis.label} metric space '${tag.id.value}' does not match diagram ${axis.label} space '${space.id.value}'"
                )
              )
            case _ =>
              Right(())
        yield space
      case None =>
        metricSpace match
          case Some(space) =>
            requireSpaceSize(axis, space, size).map(_ => space)
          case None =>
            MvSpace.of(fallbackId, fallbackRole, size)

  private def resolveMetric(
      axis: IndexAxis,
      size: Int,
      metric: Option[MvMetric],
      space: MvSpace
  ): Either[MultivarError, MvMetric] =
    metric match
      case Some(value) if value.dim != size =>
        Left(MultivarError.MetricShapeMismatch(axis, size, value.dim))
      case Some(value) =>
        value.space match
          case Some(tag) if tag != space =>
            Left(
              MultivarError.MatrixShapeMismatch(
                s"${axis.label} metric space '${tag.id.value}' does not match diagram ${axis.label} space '${space.id.value}'"
              )
            )
          case _ =>
            Right(value)
      case None =>
        Right(MvMetric.unsafeIdentity(size, Some(space)))

  private def requireSpaceSize(axis: IndexAxis, space: MvSpace, size: Int): Either[MultivarError, Unit] =
    if space.size == size then Right(())
    else
      Left(
        MultivarError.MatrixShapeMismatch(
          s"diagram ${axis.label} space '${space.id.value}' has size ${space.size} but table ${axis.label} count is $size"
        )
      )
