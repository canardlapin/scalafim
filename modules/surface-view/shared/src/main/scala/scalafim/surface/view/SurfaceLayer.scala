package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

enum SurfaceLayerKind:
  case Scalar, Labels, Mask, PackedRgba

sealed trait SurfaceLayer:
  def id: SurfaceLayerId
  def surfaceId: SurfaceId
  def geometry: SurfaceGeometry
  def frameCount: Int
  def opacity: DisplayOpacity
  def blendMode: DisplayBlendMode
  def kind: SurfaceLayerKind

  private[view] def writeColors(
    timepoint: Int,
    presentation: SurfaceLayerPresentation,
    target: Array[Int]
  ): Unit

  private[view] def describe(vertex: Int, timepoint: Int): String

  final def supportsWindow: Boolean = kind == SurfaceLayerKind.Scalar
  final def supportsThreshold: Boolean = kind == SurfaceLayerKind.Scalar

  final def isCompatibleWith(surface: SurfaceGeometry): Boolean =
    geometry.hemisphere == surface.hemisphere && geometry.mesh.hasSameTopology(surface.mesh)

object SurfaceLayer:
  /** Conventional sulcal/gyral underlay encoding. Curvature remains an
    * ordinary `SurfaceField[Double]`; this helper only fixes the neutral gray
    * presentation and verifies that folded and display geometries share exact
    * ordered topology.
    */
  object CurvatureUnderlay:
    val Window: DisplayWindow = DisplayWindow.unsafe(-1.0, 1.0)
    val Ramp: ColorRamp = ColorRamp(
      Rgba32.unsafe(48, 48, 48),
      Rgba32.unsafe(208, 208, 208)
    )

  private final class ScalarLayer(
    val id: SurfaceLayerId,
    val surfaceId: SurfaceId,
    val geometry: SurfaceGeometry,
    val frameCount: Int,
    val opacity: DisplayOpacity,
    val blendMode: DisplayBlendMode,
    values: Array[Double],
    colorizer: Colorizer[Double]
  ) extends SurfaceLayer:
    val kind: SurfaceLayerKind = SurfaceLayerKind.Scalar

    private[view] def writeColors(timepoint: Int, presentation: SurfaceLayerPresentation, target: Array[Int]): Unit =
      val effectiveWindow = presentation.window.flatMap(colorizer.withWindow).getOrElse(colorizer)
      val effective = presentation.threshold.flatMap(effectiveWindow.withThreshold).getOrElse(effectiveWindow)
      val frame = if frameCount == 1 then 0 else timepoint
      val offset = frame * geometry.vertexCount
      var vertex = 0
      while vertex < target.length do
        target(vertex) = effective.color(values(offset + vertex)).toPackedInt
        vertex += 1

    private[view] def describe(vertex: Int, timepoint: Int): String =
      val frame = if frameCount == 1 then 0 else timepoint
      values(frame * geometry.vertexCount + vertex).toString

  private final class LabelLayer(
    val id: SurfaceLayerId,
    val surfaceId: SurfaceId,
    val geometry: SurfaceGeometry,
    val frameCount: Int,
    val opacity: DisplayOpacity,
    val blendMode: DisplayBlendMode,
    values: Array[Int],
    colorizer: Colorizer[Int]
  ) extends SurfaceLayer:
    val kind: SurfaceLayerKind = SurfaceLayerKind.Labels

    private[view] def writeColors(timepoint: Int, presentation: SurfaceLayerPresentation, target: Array[Int]): Unit =
      val frame = if frameCount == 1 then 0 else timepoint
      val offset = frame * geometry.vertexCount
      var vertex = 0
      while vertex < target.length do
        target(vertex) = colorizer.color(values(offset + vertex)).toPackedInt
        vertex += 1

    private[view] def describe(vertex: Int, timepoint: Int): String =
      val frame = if frameCount == 1 then 0 else timepoint
      values(frame * geometry.vertexCount + vertex).toString

  private final class MaskLayer(
    val id: SurfaceLayerId,
    val surfaceId: SurfaceId,
    val geometry: SurfaceGeometry,
    val frameCount: Int,
    val opacity: DisplayOpacity,
    val blendMode: DisplayBlendMode,
    values: Array[Boolean],
    colorizer: Colorizer[Boolean]
  ) extends SurfaceLayer:
    val kind: SurfaceLayerKind = SurfaceLayerKind.Mask

    private[view] def writeColors(timepoint: Int, presentation: SurfaceLayerPresentation, target: Array[Int]): Unit =
      val frame = if frameCount == 1 then 0 else timepoint
      val offset = frame * geometry.vertexCount
      var vertex = 0
      while vertex < target.length do
        target(vertex) = colorizer.color(values(offset + vertex)).toPackedInt
        vertex += 1

    private[view] def describe(vertex: Int, timepoint: Int): String =
      val frame = if frameCount == 1 then 0 else timepoint
      values(frame * geometry.vertexCount + vertex).toString

  private final class PackedLayer(
    val id: SurfaceLayerId,
    val surfaceId: SurfaceId,
    val geometry: SurfaceGeometry,
    val frameCount: Int,
    val opacity: DisplayOpacity,
    val blendMode: DisplayBlendMode,
    values: Array[Int]
  ) extends SurfaceLayer:
    val kind: SurfaceLayerKind = SurfaceLayerKind.PackedRgba

    private[view] def writeColors(timepoint: Int, presentation: SurfaceLayerPresentation, target: Array[Int]): Unit =
      val frame = if frameCount == 1 then 0 else timepoint
      val offset = frame * geometry.vertexCount
      var vertex = 0
      while vertex < target.length do
        target(vertex) = values(offset + vertex)
        vertex += 1

    private[view] def describe(vertex: Int, timepoint: Int): String =
      val frame = if frameCount == 1 then 0 else timepoint
      val packed = values(frame * geometry.vertexCount + vertex)
      f"0x$packed%08x"

  private def expectedLength(geometry: SurfaceGeometry, frameCount: Int): Either[SurfaceViewError, Int] =
    if frameCount <= 0 then Left(SurfaceViewError.InvalidFrameCount(frameCount))
    else
      val count = geometry.vertexCount.toLong * frameCount.toLong
      if count > Int.MaxValue.toLong then Left(SurfaceViewError.InvalidDataLength(Int.MaxValue, Int.MaxValue))
      else Right(count.toInt)

  private def validateLength(geometry: SurfaceGeometry, frameCount: Int, actual: Int): Either[SurfaceViewError, Unit] =
    expectedLength(geometry, frameCount).flatMap: expected =>
      if expected == actual then Right(()) else Left(SurfaceViewError.InvalidDataLength(expected, actual))

  def scalar(
    id: SurfaceLayerId,
    surfaceId: SurfaceId,
    geometry: SurfaceGeometry,
    values: Array[Double],
    colorizer: Colorizer[Double],
    frameCount: Int = 1,
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal
  ): Either[SurfaceViewError, SurfaceLayer] =
    validateLength(geometry, frameCount, values.length).map: _ =>
      new ScalarLayer(id, surfaceId, geometry, frameCount, opacity, blendMode, values.clone(), colorizer)

  def curvatureUnderlay(
    id: SurfaceLayerId,
    surfaceId: SurfaceId,
    curvature: SurfaceField[Double],
    displayGeometry: SurfaceGeometry,
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal
  ): Either[SurfaceViewError, SurfaceLayer] =
    if !curvature.geometry.hasSameMeshDomain(displayGeometry) then
      Left(SurfaceViewError.IncompatibleLayerDomain(id, surfaceId))
    else if curvature.size != displayGeometry.vertexCount then
      Left(SurfaceViewError.InvalidDataLength(displayGeometry.vertexCount, curvature.size))
    else
      val values = new Array[Double](displayGeometry.vertexCount)
      var vertex = 0
      while vertex < values.length do
        curvature.valueAt(VertexId(vertex)) match
          case Some(value) => values(vertex) = value
          case None => return Left(SurfaceViewError.InvalidDataLength(displayGeometry.vertexCount, curvature.size))
        vertex += 1
      scalar(
        id,
        surfaceId,
        curvature.geometry,
        values,
        ScalarColorizer(CurvatureUnderlay.Window, CurvatureUnderlay.Ramp),
        opacity = opacity,
        blendMode = blendMode
      )

  def labels(
    id: SurfaceLayerId,
    surfaceId: SurfaceId,
    geometry: SurfaceGeometry,
    values: Array[Int],
    colorizer: Colorizer[Int],
    frameCount: Int = 1,
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal
  ): Either[SurfaceViewError, SurfaceLayer] =
    validateLength(geometry, frameCount, values.length).map: _ =>
      new LabelLayer(id, surfaceId, geometry, frameCount, opacity, blendMode, values.clone(), colorizer)

  def mask(
    id: SurfaceLayerId,
    surfaceId: SurfaceId,
    geometry: SurfaceGeometry,
    values: Array[Boolean],
    colorizer: Colorizer[Boolean],
    frameCount: Int = 1,
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal
  ): Either[SurfaceViewError, SurfaceLayer] =
    validateLength(geometry, frameCount, values.length).map: _ =>
      new MaskLayer(id, surfaceId, geometry, frameCount, opacity, blendMode, values.clone(), colorizer)

  def packedRgba(
    id: SurfaceLayerId,
    surfaceId: SurfaceId,
    geometry: SurfaceGeometry,
    values: IndexedSeq[Rgba32],
    frameCount: Int = 1,
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal
  ): Either[SurfaceViewError, SurfaceLayer] =
    validateLength(geometry, frameCount, values.length).map: _ =>
      val packed = new Array[Int](values.length)
      var index = 0
      while index < values.length do
        packed(index) = values(index).toPackedInt
        index += 1
      new PackedLayer(id, surfaceId, geometry, frameCount, opacity, blendMode, packed)
