package scalafim.surface.view

import scalafim.image.SampleSpaces.*

import intaglio.*
import scalafim.image.*
import scalafim.surface.*

opaque type SurfaceMinimumSamples = Int

object SurfaceMinimumSamples:
  def make(value: Int): Either[SurfaceViewError, SurfaceMinimumSamples] =
    if value > 0 then Right(value)
    else Left(SurfaceViewError.InvalidProjection(s"minimum sample count must be positive; got $value"))

  def unsafe(value: Int): SurfaceMinimumSamples =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (count: SurfaceMinimumSamples)
    def value: Int = count

sealed trait SurfaceProjectionFill

object SurfaceProjectionFill:
  case object NaN extends SurfaceProjectionFill
  final case class Constant private[view] (value: Double) extends SurfaceProjectionFill

  def constant(value: Double): Either[SurfaceViewError, SurfaceProjectionFill] =
    if value.isFinite then Right(Constant(value))
    else Left(SurfaceViewError.InvalidProjection(s"fill value must be finite; got $value"))

final case class SurfaceProjectionPolicy(
  minimumSamples: SurfaceMinimumSamples = SurfaceMinimumSamples.unsafe(1),
  fill: SurfaceProjectionFill = SurfaceProjectionFill.NaN
)

/** Sample accounting is the sampler's observed [[SurfaceSampleTally]]:
  * `acceptedSamples` counts finite values only, and `rejectedSamples` counts
  * samples outside the volume, excluded by the mask, or non-finite.
  * `qualifiedVertices` follows the per-vertex `sampleCounts`, which include
  * non-finite samples, so a qualified vertex can still carry a NaN value.
  */
final case class SurfaceProjectionReceipt(
  vertices: Int,
  tally: SurfaceSampleTally,
  qualifiedVertices: Int,
  sourceVolumeValues: Long,
  sourceBytes: Long,
  materializedBytes: Long,
  elapsedNanos: Long,
  path: SurfaceSamplingPath,
  reducer: SurfaceSampleAggregation
):
  def requestedSamples: Long =
    tally.requested

  def acceptedSamples: Long =
    tally.accepted

  def rejectedSamples: Long =
    tally.rejected

final case class SurfaceProjectionResult(
  values: SurfaceField[Double],
  sampleCounts: SurfaceField[Int],
  quality: SurfaceField[Boolean],
  receipt: SurfaceProjectionReceipt
)

object SurfaceVolumeProjection:
  def materialize(
    morphism: VolToSurfMorphism,
    volume: SomeScalarVolume[Double],
    policy: SurfaceProjectionPolicy = SurfaceProjectionPolicy(),
    mask: Option[SomeMaskVolume] = None
  ): SurfaceProjectionResult =
    val started = System.nanoTime()
    val sampled = morphism.sample(volume, mask)
    val vertexCount = sampled.values.geometry.vertexCount
    val values = new Array[Double](vertexCount)
    val counts = new Array[Int](vertexCount)
    val quality = new Array[Boolean](vertexCount)
    var qualified = 0
    var vertex = 0
    while vertex < vertexCount do
      val id = VertexId.unsafe(vertex)
      val count = sampled.sampleCounts.valueAt(id).getOrElse(0)
      val observed = sampled.values.valueAt(id).getOrElse(Double.NaN)
      val keep = count >= policy.minimumSamples.value
      counts(vertex) = count
      quality(vertex) = keep
      if keep then
        values(vertex) = observed
        qualified += 1
      else
        values(vertex) = policy.fill match
          case SurfaceProjectionFill.NaN => Double.NaN
          case SurfaceProjectionFill.Constant(value) => value
      vertex += 1
    val volumeValues = volume.space.spatialDims.iterator.map(_.toLong).product
    SurfaceProjectionResult(
      SurfaceField.full(sampled.values.geometry, values.toIndexedSeq, sampled.values.label),
      SurfaceField.full(sampled.values.geometry, counts.toIndexedSeq, sampled.sampleCounts.label),
      SurfaceField.full(sampled.values.geometry, quality.toIndexedSeq, "surface-projection-quality"),
      SurfaceProjectionReceipt(
        vertexCount,
        sampled.tally,
        qualified,
        volumeValues,
        volumeValues * 8L,
        vertexCount.toLong * (8L + 4L + 1L),
        System.nanoTime() - started,
        morphism.plan.path,
        morphism.plan.aggregation
      )
    )

  def scalarLayer(
    result: SurfaceProjectionResult,
    id: SurfaceLayerId,
    surface: SurfaceId,
    displayGeometry: SurfaceGeometry,
    colorizer: Colorizer[Double],
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal
  ): Either[SurfaceViewError, SurfaceLayer] =
    if !result.values.geometry.hasSameMeshDomain(displayGeometry) then
      Left(SurfaceViewError.IncompatibleLayerDomain(id, surface))
    else
      val values = new Array[Double](displayGeometry.vertexCount)
      var vertex = 0
      while vertex < values.length do
        result.values.valueAt(VertexId.unsafe(vertex)) match
          case Some(value) => values(vertex) = value
          case None => return Left(SurfaceViewError.InvalidDataLength(displayGeometry.vertexCount, result.values.size))
        vertex += 1
      SurfaceLayer.scalar(id, surface, result.values.geometry, values, colorizer, opacity = opacity, blendMode = blendMode)
