package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

enum CpcaBlock:
  case GxH
  case G0xH
  case GxH0
  case G0xH0

  def label: String =
    this match
      case GxH   => "GxH"
      case G0xH  => "G0xH"
      case GxH0  => "GxH0"
      case G0xH0 => "G0xH0"

  private[multivar] def rowMode: CpcaSubspaceMode =
    this match
      case GxH | GxH0   => CpcaSubspaceMode.Project
      case G0xH | G0xH0 => CpcaSubspaceMode.Residual

  private[multivar] def columnMode: CpcaSubspaceMode =
    this match
      case GxH | G0xH   => CpcaSubspaceMode.Project
      case GxH0 | G0xH0 => CpcaSubspaceMode.Residual

object CpcaBlock:
  val all: Vector[CpcaBlock] =
    Vector(GxH, G0xH, GxH0, G0xH0)

  private[multivar] def validateRequested(blocks: Vector[CpcaBlock]): Either[MultivarError, Unit] =
    if blocks.isEmpty then Left(MultivarError.InvalidBlockPartition("CPCA block request must select at least one block"))
    else validateDistinct(blocks)

  private[multivar] def validateDistinct(blocks: Vector[CpcaBlock]): Either[MultivarError, Unit] =
    val seen = scala.collection.mutable.HashSet.empty[CpcaBlock]
    var i = 0
    var error = Option.empty[MultivarError]
    while i < blocks.length && error.isEmpty do
      val block = blocks(i)
      if seen.contains(block) then error = Some(MultivarError.InvalidBlockPartition(s"CPCA block '${block.label}' requested more than once"))
      else seen += block
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(())

final case class CpcaBlockRequest private (
    blocks: Vector[CpcaBlock],
    rankByBlock: Map[CpcaBlock, ComponentCount],
    defaultComponents: Option[ComponentCount]
):
  require(blocks.nonEmpty, "CPCA block request must be non-empty")

  def requestedComponents(block: CpcaBlock): Option[ComponentCount] =
    rankByBlock.get(block).orElse(defaultComponents)

  def requestedComponentUpperBound: Option[ComponentCount] =
    val values = blocks.flatMap(requestedComponents)
    if values.isEmpty then None
    else Some(ComponentCount.unsafe(values.map(_.value).max))

  def requestedComponentSummary: String =
    blocks
      .map { block =>
        val value = requestedComponents(block).map(_.value.toString).getOrElse("full")
        s"${block.label}:$value"
      }
      .mkString(",")

  def validateAgainst(sampleCount: Int, featureCount: Int): Either[MultivarError, Unit] =
    val broadLimit = Math.min(sampleCount, featureCount)
    val allRanks = defaultComponents.toVector ++ rankByBlock.values
    allRanks.find(_.value > broadLimit) match
      case Some(value) => Left(MultivarError.InvalidComponentRequest(value.value, broadLimit))
      case None        => Right(())

object CpcaBlockRequest:
  val default: CpcaBlockRequest =
    unsafe()

  def from(
      blocks: Iterable[CpcaBlock] = Vector(CpcaBlock.GxH),
      rankByBlock: Map[CpcaBlock, ComponentCount] = Map.empty,
      defaultComponents: Option[ComponentCount] = None
  ): Either[MultivarError, CpcaBlockRequest] =
    val selected = blocks.toVector
    for
      _ <- CpcaBlock.validateRequested(selected)
      _ <- validateRankKeys(selected, rankByBlock)
    yield CpcaBlockRequest(selected, rankByBlock, defaultComponents)

  def unsafe(
      blocks: Iterable[CpcaBlock] = Vector(CpcaBlock.GxH),
      rankByBlock: Map[CpcaBlock, ComponentCount] = Map.empty,
      defaultComponents: Option[ComponentCount] = None
  ): CpcaBlockRequest =
    from(blocks, rankByBlock, defaultComponents).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def validateRankKeys(
      blocks: Vector[CpcaBlock],
      rankByBlock: Map[CpcaBlock, ComponentCount]
  ): Either[MultivarError, Unit] =
    val requested = blocks.toSet
    rankByBlock.keys.find(block => !requested.contains(block)) match
      case Some(block) =>
        Left(MultivarError.InvalidBlockPartition(s"rank requested for CPCA block '${block.label}' that is not selected"))
      case None =>
        Right(())

private[multivar] enum CpcaSubspaceMode:
  case Project
  case Residual

enum CpcaConstraint:
  case Identity
  case Zero
  case Basis(design: DMat, keepDesign: Boolean = true)

  def basisRows: Option[Int] =
    this match
      case Basis(design, _) => Some(design.rows)
      case _                => None

  def validate(axis: IndexAxis, expectedRows: Int): Either[MultivarError, Unit] =
    this match
      case Identity | Zero =>
        if expectedRows > 0 then Right(())
        else Left(MultivarError.InvalidDimension(s"CPCA ${axis.label} constraint rows", expectedRows))
      case Basis(design, _) =>
        if design.rows != expectedRows then
          Left(
            MultivarError.MatrixShapeMismatch(
              s"CPCA ${axis.label} constraint design has ${design.rows} rows but expected $expectedRows"
            )
          )
        else MatrixOps.checkFinite(s"CPCA ${axis.label} constraint design", design)

final case class CpcaEstimatorSpec(
    blocks: Vector[CpcaBlock] = Vector(CpcaBlock.GxH),
    rankByBlock: Map[CpcaBlock, ComponentCount] = Map.empty,
    defaultComponents: Option[ComponentCount] = None,
    rowMetric: Option[MetricSpec] = None,
    columnMetric: Option[MetricSpec] = None,
    rowConstraint: CpcaConstraint = CpcaConstraint.Identity,
    columnConstraint: CpcaConstraint = CpcaConstraint.Identity,
    storagePolicy: StoragePolicy = StoragePolicy.AllowDense,
    rankTolerance: Double = 1e-12
):
  def blockRequest: Either[MultivarError, CpcaBlockRequest] =
    CpcaBlockRequest.from(blocks, rankByBlock, defaultComponents)

  def requestedComponents(block: CpcaBlock): Option[ComponentCount] =
    blockRequest.toOption.flatMap(_.requestedComponents(block))

  def requestedComponentUpperBound: Option[ComponentCount] =
    blockRequest.toOption.flatMap(_.requestedComponentUpperBound)

  def requestedComponentSummary: String =
    blockRequest.map(_.requestedComponentSummary).getOrElse("invalid")

  def validate(sampleCount: Int, featureCount: Int): Either[MultivarError, Unit] =
    for
      request <- blockRequest
      _ <- RowGeometryOps.requireTolerance("CPCA rank tolerance", rankTolerance)
      _ <- validateMetric(IndexAxis.Row, sampleCount, rowMetric)
      _ <- validateMetric(IndexAxis.Feature, featureCount, columnMetric)
      _ <- rowConstraint.validate(IndexAxis.Row, sampleCount)
      _ <- columnConstraint.validate(IndexAxis.Feature, featureCount)
      _ <- validateRequestedRanks(request, sampleCount, featureCount)
    yield ()

  private def validateRequestedRanks(
      request: CpcaBlockRequest,
      sampleCount: Int,
      featureCount: Int
  ): Either[MultivarError, Unit] =
    request.validateAgainst(sampleCount, featureCount).flatMap { _ =>
      var i = 0
      var error = Option.empty[MultivarError]
      while i < request.blocks.length && error.isEmpty do
        val block = request.blocks(i)
        request.requestedComponents(block) match
          case Some(value) =>
            staticBlockRankLimit(block, sampleCount, featureCount) match
              case Some(limit) if value.value > limit =>
                error = Some(MultivarError.InvalidComponentRequest(value.value, limit))
              case _ =>
          case None =>
        i += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(())
    }

  private def staticBlockRankLimit(
      block: CpcaBlock,
      sampleCount: Int,
      featureCount: Int
  ): Option[Int] =
    val rowLimit = staticConstraintRank(rowConstraint, sampleCount).map { rank =>
      block.rowMode match
        case CpcaSubspaceMode.Project  => rank
        case CpcaSubspaceMode.Residual => sampleCount - rank
    }
    val columnLimit = staticConstraintRank(columnConstraint, featureCount).map { rank =>
      block.columnMode match
        case CpcaSubspaceMode.Project  => rank
        case CpcaSubspaceMode.Residual => featureCount - rank
    }
    (rowLimit, columnLimit) match
      case (Some(row), Some(column)) => Some(Math.min(row, column))
      case _                         => None

  private def staticConstraintRank(spec: CpcaConstraint, size: Int): Option[Int] =
    spec match
      case CpcaConstraint.Identity => Some(size)
      case CpcaConstraint.Zero     => Some(0)
      case CpcaConstraint.Basis(_, _) =>
        None

  private def validateMetric(
      axis: IndexAxis,
      expected: Int,
      metric: Option[MetricSpec]
  ): Either[MultivarError, Unit] =
    metric match
      case Some(value) if value.dim != expected =>
        Left(MultivarError.MetricShapeMismatch(axis, expected, value.dim))
      case _ =>
        Right(())

final case class CpcaBlockInertia(block: CpcaBlock, ss: Double, prop: Double):
  require(ss >= -1e-8, "block sum-of-squares must be non-negative up to roundoff")
  require(prop >= -1e-8, "block proportion must be non-negative up to roundoff")

final case class CpcaPartition(totalSS: Double, blocks: Vector[CpcaBlockInertia]):
  require(totalSS >= -1e-8, "total sum-of-squares must be non-negative up to roundoff")
  require(
    blocks.length == CpcaBlock.all.length && blocks.map(_.block).toSet == CpcaBlock.all.toSet,
    "CPCA partition must contain each of the four blocks exactly once"
  )

  def inertia(block: CpcaBlock): Option[CpcaBlockInertia] =
    blocks.find(_.block == block)

final case class CpcaBlockResult private[multivar] (
    block: CpcaBlock,
    singularValues: DVec,
    uStar: DMat,
    vStar: DMat,
    u: DMat,
    v: DMat,
    rowCoordinates: Option[DMat],
    columnCoordinates: Option[DMat],
    ss: Double
):
  require(uStar.cols == singularValues.length, "left whitened vectors must match singular values")
  require(vStar.cols == singularValues.length, "right whitened vectors must match singular values")
  require(u.cols == singularValues.length, "left metric vectors must match singular values")
  require(v.cols == singularValues.length, "right metric vectors must match singular values")

  def d: DVec =
    singularValues

  def rank: Int =
    singularValues.length

  def scores: DMat =
    MatrixOps.scaleColumns(u, singularValues)

  def reconstructWhitened(components: Option[ComponentCount] = None): Either[MultivarError, DMat] =
    reconstruct(uStar, vStar, components)

  def reconstructOriginal(components: Option[ComponentCount] = None): Either[MultivarError, DMat] =
    reconstruct(u, v, components)

  private def reconstruct(
      left: DMat,
      right: DMat,
      components: Option[ComponentCount]
  ): Either[MultivarError, DMat] =
    val k = components.map(_.value).getOrElse(rank)
    if k > rank then Left(MultivarError.InvalidComponentRequest(k, rank))
    else if k == 0 then Right(DMat.zeros(left.rows, right.rows))
    else
      val scaled = MatrixOps.scaleColumns(MatrixOps.takeColumns(left, k), MatrixOps.takeVector(singularValues, k))
      Right(GaleNumerics.multiply(scaled, MatrixOps.takeColumns(right, k).transpose))

private[multivar] object CpcaMath:
  def add(left: DMat, right: DMat): DMat =
    require(left.rows == right.rows && left.cols == right.cols, "matrix shapes must match")
    val out = left.copyData
    val rightData = right.copyData
    var i = 0
    while i < out.length do
      out(i) += rightData(i)
      i += 1
    GaleNumerics.matrixFromRowMajor(left.rows, left.cols, out)

  def frobeniusNorm2(matrix: DMat): Double =
    val data = matrix.copyData
    var acc = 0.0
    var i = 0
    while i < data.length do
      val value = data(i)
      acc += value * value
      i += 1
    acc

  def sumSquares(values: DVec): Double =
    var acc = 0.0
    var i = 0
    while i < values.length do
      acc += values(i) * values(i)
      i += 1
    acc
