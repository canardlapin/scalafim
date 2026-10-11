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

/** How a projection classified its vertices. Every vertex is exactly one of:
  *
  *  - `qualified`: at least `minimumSamples` finite samples;
  *  - `nonFiniteOnly`: no finite sample but at least one non-finite one. A
  *    non-finite value is not an observation, so such a vertex never qualifies,
  *    and it is tallied here rather than hidden among the unsupported vertices;
  *  - `insufficient`: every other vertex, with fewer finite samples than the
  *    minimum (including none at all).
  */
final case class SurfaceVertexTally(
  vertices: Int,
  qualified: Int,
  nonFiniteOnly: Int,
  insufficient: Int
):
  require(
    vertices >= 0 && qualified >= 0 && nonFiniteOnly >= 0 && insufficient >= 0,
    "vertex tallies must be non-negative"
  )
  require(
    qualified + nonFiniteOnly + insufficient == vertices,
    "every vertex must be qualified, non-finite only, or insufficient"
  )

  def unqualified: Int =
    nonFiniteOnly + insufficient

/** Sample accounting is the sampler's observed [[SurfaceSampleTally]]:
  * `acceptedSamples` counts finite values only, and `rejectedSamples` counts
  * samples outside the volume, excluded by the mask, or non-finite.
  * Vertex qualification counts finite samples only (see [[SurfaceVertexTally]]).
  */
final case class SurfaceProjectionReceipt(
  vertexTally: SurfaceVertexTally,
  tally: SurfaceSampleTally,
  sourceVolumeValues: Long,
  sourceBytes: Long,
  materializedBytes: Long,
  elapsedNanos: Long,
  path: SurfaceSamplingPath,
  reducer: SurfaceSampleAggregation
):
  def vertices: Int =
    vertexTally.vertices

  def qualifiedVertices: Int =
    vertexTally.qualified

  def nonFiniteOnlyVertices: Int =
    vertexTally.nonFiniteOnly

  def requestedSamples: Long =
    tally.requested

  def acceptedSamples: Long =
    tally.accepted

  def rejectedSamples: Long =
    tally.rejected

/** `sampleCounts` holds finite samples per vertex and `nonFiniteCounts` the
  * non-finite in-mask samples; `quality` marks the qualified vertices.
  */
final case class SurfaceProjectionResult(
  values: SurfaceField[Double],
  sampleCounts: SurfaceField[Int],
  nonFiniteCounts: SurfaceField[Int],
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
    val nonFiniteCounts = new Array[Int](vertexCount)
    val quality = new Array[Boolean](vertexCount)
    var qualified = 0
    var nonFiniteOnly = 0
    var vertex = 0
    while vertex < vertexCount do
      val id = VertexId.unsafe(vertex)
      val count = sampled.sampleCounts.valueAt(id).getOrElse(0)
      val nonFinite = sampled.nonFiniteCounts.valueAt(id).getOrElse(0)
      val observed = sampled.values.valueAt(id).getOrElse(Double.NaN)
      val keep = count >= policy.minimumSamples.value
      counts(vertex) = count
      nonFiniteCounts(vertex) = nonFinite
      quality(vertex) = keep
      if keep then
        values(vertex) = observed
        qualified += 1
      else
        if count == 0 && nonFinite > 0 then nonFiniteOnly += 1
        values(vertex) = policy.fill match
          case SurfaceProjectionFill.NaN => Double.NaN
          case SurfaceProjectionFill.Constant(value) => value
      vertex += 1
    val volumeValues = volume.space.spatialDims.iterator.map(_.toLong).product
    SurfaceProjectionResult(
      SurfaceField.full(sampled.values.geometry, values.toIndexedSeq, sampled.values.label),
      SurfaceField.full(sampled.values.geometry, counts.toIndexedSeq, sampled.sampleCounts.label),
      SurfaceField.full(sampled.values.geometry, nonFiniteCounts.toIndexedSeq, sampled.nonFiniteCounts.label),
      SurfaceField.full(sampled.values.geometry, quality.toIndexedSeq, "surface-projection-quality"),
      SurfaceProjectionReceipt(
        SurfaceVertexTally(vertexCount, qualified, nonFiniteOnly, vertexCount - qualified - nonFiniteOnly),
        sampled.tally,
        volumeValues,
        volumeValues * 8L,
        vertexCount.toLong * (8L + 4L + 4L + 1L),
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
