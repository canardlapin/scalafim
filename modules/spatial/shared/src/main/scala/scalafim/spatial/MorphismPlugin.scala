package scalafim.spatial

import gale.linalg.DMat

import scala.util.hashing.MurmurHash3

enum StageSemantics:
  case CoordinatePullback, ValueTransform, FusionBarrier

opaque type MorphismPluginId = String

object MorphismPluginId:
  def apply(value: String): Either[SpatialError, MorphismPluginId] =
    val normalized = value.trim
    if normalized.isEmpty then Left(SpatialError.EmptyIdentifier("morphism plugin"))
    else Right(normalized)

  private[spatial] def unsafe(value: String): MorphismPluginId =
    value

  extension (id: MorphismPluginId)
    def value: String =
      id

enum ValueTransform:
  case ObservationLinear(matrix: DMat)
  case RowLinear(matrix: DMat)
  case PointwiseAffine(scale: Double, offset: Double)

  this match
    case ObservationLinear(matrix) =>
      require(matrix.rows == matrix.cols, "observation transform must be square")
      require(ValueTransform.isFinite(matrix), "observation transform must be finite")
    case RowLinear(matrix) =>
      require(ValueTransform.isFinite(matrix), "row transform must be finite")
    case PointwiseAffine(scale, offset) =>
      require(scale.isFinite && offset.isFinite, "pointwise affine values must be finite")

  def fingerprint: String =
    this match
      case ObservationLinear(matrix) => ValueTransform.matrixFingerprint("observation-linear-v1", matrix)
      case RowLinear(matrix) => ValueTransform.matrixFingerprint("row-linear-v1", matrix)
      case PointwiseAffine(scale, offset) =>
        s"pointwise-affine-v1:${java.lang.Double.toHexString(scale)}:${java.lang.Double.toHexString(offset)}"

object ValueTransform:
  def observationLinear(matrix: DMat): Either[SpatialError, ValueTransform] =
    if matrix.rows != matrix.cols then
      Left(SpatialError.UnsupportedPluginComposition(s"observation matrix must be square, got ${matrix.rows}x${matrix.cols}"))
    else if !isFinite(matrix) then
      Left(SpatialError.UnsupportedPluginComposition("observation matrix contains non-finite values"))
    else Right(ValueTransform.ObservationLinear(matrix))

  def rowLinear(matrix: DMat): Either[SpatialError, ValueTransform] =
    if !isFinite(matrix) then Left(SpatialError.UnsupportedPluginComposition("row matrix contains non-finite values"))
    else Right(ValueTransform.RowLinear(matrix))

  def pointwiseAffine(scale: Double, offset: Double): Either[SpatialError, ValueTransform] =
    if !scale.isFinite || !offset.isFinite then
      Left(SpatialError.UnsupportedPluginComposition("pointwise affine values must be finite"))
    else Right(ValueTransform.PointwiseAffine(scale, offset))

  private[spatial] def isFinite(matrix: DMat): Boolean =
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        if !matrix(row, col).isFinite then return false
        col += 1
      row += 1
    true

  private def matrixFingerprint(prefix: String, matrix: DMat): String =
    var hash = MurmurHash3.stringHash(s"$prefix|${matrix.rows}x${matrix.cols}")
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        val bits = java.lang.Double.doubleToLongBits(matrix(row, col))
        hash = MurmurHash3.mix(hash, (bits ^ (bits >>> 32)).toInt)
        col += 1
      row += 1
    s"$prefix:${java.lang.Integer.toHexString(MurmurHash3.finalizeHash(hash, matrix.rows * matrix.cols))}"

final case class MorphismPlugin private (
  id: MorphismPluginId,
  semantics: StageSemantics,
  transform: ValueTransform
):
  require(semantics != StageSemantics.CoordinatePullback, "coordinate pullbacks use CoordinateMap")

  def fingerprint: String =
    s"plugin-v1:${id.value}:$semantics:${transform.fingerprint}"

object MorphismPlugin:
  def functional(id: MorphismPluginId, matrix: DMat): Either[SpatialError, MorphismPlugin] =
    ValueTransform.observationLinear(matrix).map { transform =>
      new MorphismPlugin(id, StageSemantics.ValueTransform, transform)
    }

  def filter(id: MorphismPluginId, matrix: DMat): Either[SpatialError, MorphismPlugin] =
    ValueTransform.rowLinear(matrix).map { transform =>
      new MorphismPlugin(id, StageSemantics.ValueTransform, transform)
    }

  def hybrid(id: MorphismPluginId, matrix: DMat): Either[SpatialError, MorphismPlugin] =
    ValueTransform.rowLinear(matrix).map { transform =>
      new MorphismPlugin(id, StageSemantics.ValueTransform, transform)
    }

  def pointwiseBarrier(
    id: MorphismPluginId,
    scale: Double,
    offset: Double
  ): Either[SpatialError, MorphismPlugin] =
    ValueTransform.pointwiseAffine(scale, offset).map { transform =>
      new MorphismPlugin(id, StageSemantics.FusionBarrier, transform)
    }

  private[spatial] def supports(kind: MorphismKind, plugin: MorphismPlugin): Boolean =
    (kind, plugin.transform) match
      case (MorphismKind.Functional, ValueTransform.ObservationLinear(_)) => true
      case (MorphismKind.Filter, ValueTransform.RowLinear(_)) => true
      case (MorphismKind.Filter, ValueTransform.PointwiseAffine(_, _)) => true
      case (MorphismKind.Hybrid, ValueTransform.RowLinear(_)) => true
      case _ => false

  private[spatial] def validateDomains(
    morphism: Morphism,
    source: Domain,
    target: Domain
  ): Either[SpatialError, Unit] =
    morphism.plugin match
      case None => Right(())
      case Some(plugin) =>
        plugin.transform match
          case ValueTransform.ObservationLinear(_) =>
            if source.nElements != target.nElements then
              Left(
                SpatialError.InvalidMorphismPlugin(
                  morphism.id,
                  s"observation transforms preserve sample count, got ${source.nElements}->${target.nElements}"
                )
              )
            else Right(())
          case ValueTransform.RowLinear(matrix) =>
            if matrix.rows != target.nElements || matrix.cols != source.nElements then
              Left(
                SpatialError.InvalidMorphismPlugin(
                  morphism.id,
                  s"row matrix is ${matrix.rows}x${matrix.cols}, expected ${target.nElements}x${source.nElements}"
                )
              )
            else Right(())
          case ValueTransform.PointwiseAffine(_, _) =>
            if source.nElements != target.nElements then
              Left(
                SpatialError.InvalidMorphismPlugin(
                  morphism.id,
                  s"pointwise transforms preserve sample count, got ${source.nElements}->${target.nElements}"
                )
              )
            else Right(())
