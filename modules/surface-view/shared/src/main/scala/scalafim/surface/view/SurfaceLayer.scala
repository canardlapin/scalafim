package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

enum SurfaceLayerKind:
  case Scalar, Labels, Mask, PackedRgba

enum SurfaceSampleAssociation:
  case Vertex, Face

/** Existing constructors retain mapped-color interpolation unless explicitly changed. */
enum SurfaceVertexInterpolation:
  case Color, NearestSample

  def policy: SurfaceMapInterpolation = this match
    case Color => SurfaceMapInterpolation.VertexColor
    case NearestSample => SurfaceMapInterpolation.NearestVertex

enum SurfaceMapInterpolation:
  case VertexColor, FaceConstant, NearestVertex, VertexScalar

sealed trait SurfaceLayer:
  def id: SurfaceLayerId
  def surfaceId: SurfaceId
  def geometry: SurfaceGeometry
  def frameCount: Int
  def opacity: DisplayOpacity
  def blendMode: DisplayBlendMode
  def kind: SurfaceLayerKind
  def association: SurfaceSampleAssociation = SurfaceSampleAssociation.Vertex
  def interpolation: SurfaceMapInterpolation = association match
    case SurfaceSampleAssociation.Vertex => SurfaceMapInterpolation.VertexColor
    case SurfaceSampleAssociation.Face => SurfaceMapInterpolation.FaceConstant
  final def sampleCount: Int = association match
    case SurfaceSampleAssociation.Vertex => geometry.vertexCount
    case SurfaceSampleAssociation.Face => geometry.faceCount

  private[view] def writeColors(
    timepoint: Int,
    presentation: SurfaceLayerPresentation,
    target: Array[Int]
  ): Unit

  private[view] def scalarSamples(timepoint: Int): Option[Array[Double]] = None

  private[view] def describe(vertex: Int, timepoint: Int): String

  def supportsWindow: Boolean = false
  def supportsThreshold: Boolean = false
  def scalarMapping: Option[ScalarMapping] = None
  /** Inspectable category palette; arbitrary callbacks remain opaque. */
  def labelMapping: Option[LabelColorizer] = None

  final def effectiveScalarMapping(presentation: SurfaceLayerPresentation): Either[SurfaceViewError, Option[ScalarMapping]] =
    scalarMapping match
      case None => Right(None)
      case Some(mapping) => mapping.resolve(presentation.window, presentation.threshold)
        .left.map(error => SurfaceViewError.InvalidScalarMapping(id, error)).map(Some.apply)

  final def isCompatibleWith(surface: SurfaceGeometry): Boolean =
    geometry.hemisphere == surface.hemisphere && geometry.mesh.hasSameTopology(surface.mesh)

object SurfaceLayer:
  private final class FaceLayer[A](
    val id: SurfaceLayerId,
    val surfaceId: SurfaceId,
    field: SurfaceFaceField[A],
    val kind: SurfaceLayerKind,
    colorizer: Colorizer[A],
    val opacity: DisplayOpacity,
    val blendMode: DisplayBlendMode
  ) extends SurfaceLayer:
    val geometry: SurfaceGeometry = field.geometry
    val frameCount: Int = field.frameCount
    override val association: SurfaceSampleAssociation = SurfaceSampleAssociation.Face
    override val labelMapping: Option[LabelColorizer] = colorizer match
      case palette: LabelColorizer if kind == SurfaceLayerKind.Labels => Some(palette)
      case _ => None

    private[view] def writeColors(timepoint: Int, presentation: SurfaceLayerPresentation, target: Array[Int]): Unit =
      val windowed = presentation.window.flatMap(colorizer.withWindow).getOrElse(colorizer)
      val effective = presentation.threshold.flatMap(windowed.withThreshold).getOrElse(windowed)
      val frame = if frameCount == 1 then 0 else timepoint
      var face = 0
      while face < target.length do
        target(face) = effective.color(field.valueAtUnsafe(face, frame)).toPackedInt
        face += 1

    private[view] def describe(face: Int, timepoint: Int): String =
      field.valueAtUnsafe(face, if frameCount == 1 then 0 else timepoint).toString

  def faceScalar(
    id: SurfaceLayerId,
    surfaceId: SurfaceId,
    field: SurfaceFaceField[Double],
    colorizer: Colorizer[Double],
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal
  ): SurfaceLayer = new ScalarLayer(id, surfaceId, field.geometry, field.frameCount, opacity, blendMode,
    ScalarSamples.Face(field), colorizer, SurfaceMapInterpolation.FaceConstant)

  def faceLabels(
    id: SurfaceLayerId,
    surfaceId: SurfaceId,
    field: SurfaceFaceField[Int],
    colorizer: Colorizer[Int],
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal
  ): SurfaceLayer = new FaceLayer(id, surfaceId, field, SurfaceLayerKind.Labels, colorizer, opacity, blendMode)

  def faceMask(
    id: SurfaceLayerId,
    surfaceId: SurfaceId,
    field: SurfaceFaceField[Boolean],
    colorizer: Colorizer[Boolean],
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal
  ): SurfaceLayer = new FaceLayer(id, surfaceId, field, SurfaceLayerKind.Mask, colorizer, opacity, blendMode)

  def facePackedRgba(
    id: SurfaceLayerId,
    surfaceId: SurfaceId,
    field: SurfaceFaceField[Rgba32],
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal
  ): SurfaceLayer =
    val colorizer = new Colorizer[Rgba32]:
      def color(value: Rgba32): Rgba32 = value
    new FaceLayer(id, surfaceId, field, SurfaceLayerKind.PackedRgba, colorizer, opacity, blendMode)

  /** Sign of the source measurement at sulcal vertices. FreeSurfer sulc/curv
    * use PositiveIsSulcal; other sources must declare their own convention.
    */
  enum SulcalPolarity:
    case PositiveIsSulcal, NegativeIsSulcal

  /** Legacy negative-dark sulcal/gyral underlay encoding. Curvature remains an
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

  private enum ScalarSamples:
    case Vertex(values: Array[Double])
    case Face(field: SurfaceFaceField[Double])

    def value(index: Int, frame: Int, count: Int): Double = this match
      case Vertex(values) => values(frame * count + index)
      case Face(field) => field.valueAtUnsafe(index, frame)

  private final class ScalarLayer(
    val id: SurfaceLayerId,
    val surfaceId: SurfaceId,
    val geometry: SurfaceGeometry,
    val frameCount: Int,
    val opacity: DisplayOpacity,
    val blendMode: DisplayBlendMode,
    samples: ScalarSamples,
    colorizer: Colorizer[Double],
    override val interpolation: SurfaceMapInterpolation
  ) extends SurfaceLayer:
    val kind: SurfaceLayerKind = SurfaceLayerKind.Scalar
    override val association: SurfaceSampleAssociation = samples match
      case ScalarSamples.Vertex(_) => SurfaceSampleAssociation.Vertex
      case ScalarSamples.Face(_) => SurfaceSampleAssociation.Face
    override val scalarMapping: Option[ScalarMapping] = ScalarMapping.inspect(colorizer)
    override def supportsWindow: Boolean = scalarMapping.nonEmpty || colorizer.supportsWindow
    override def supportsThreshold: Boolean = scalarMapping.nonEmpty || colorizer.supportsThreshold

    override private[view] def scalarSamples(timepoint: Int): Option[Array[Double]] =
      if interpolation != SurfaceMapInterpolation.VertexScalar then None
      else
        val frame = if frameCount == 1 then 0 else timepoint
        Some(Array.tabulate(sampleCount)(index => samples.value(index, frame, sampleCount)))

    private[view] def writeColors(timepoint: Int, presentation: SurfaceLayerPresentation, target: Array[Int]): Unit =
      val effective = effectiveScalarMapping(presentation).toOption.get match
        case Some(mapping) => mapping.colorizer
        case None =>
          val windowed = presentation.window.flatMap(colorizer.withWindow).getOrElse(colorizer)
          presentation.threshold.flatMap(windowed.withThreshold).getOrElse(windowed)
      val frame = if frameCount == 1 then 0 else timepoint
      var vertex = 0
      while vertex < target.length do
        target(vertex) = effective.color(samples.value(vertex, frame, sampleCount)).toPackedInt
        vertex += 1

    private[view] def describe(vertex: Int, timepoint: Int): String =
      val frame = if frameCount == 1 then 0 else timepoint
      samples.value(vertex, frame, sampleCount).toString

  private final class LabelLayer(
    val id: SurfaceLayerId,
    val surfaceId: SurfaceId,
    val geometry: SurfaceGeometry,
    val frameCount: Int,
    val opacity: DisplayOpacity,
    val blendMode: DisplayBlendMode,
    values: Array[Int],
    colorizer: Colorizer[Int],
    override val interpolation: SurfaceMapInterpolation
  ) extends SurfaceLayer:
    val kind: SurfaceLayerKind = SurfaceLayerKind.Labels
    override val labelMapping: Option[LabelColorizer] = colorizer match
      case palette: LabelColorizer => Some(palette)
      case _ => None

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
    colorizer: Colorizer[Boolean],
    override val interpolation: SurfaceMapInterpolation
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
    values: Array[Int],
    override val interpolation: SurfaceMapInterpolation
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
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal,
    interpolation: SurfaceVertexInterpolation = SurfaceVertexInterpolation.Color
  ): Either[SurfaceViewError, SurfaceLayer] =
    validateLength(geometry, frameCount, values.length).map: _ =>
      new ScalarLayer(id, surfaceId, geometry, frameCount, opacity, blendMode, ScalarSamples.Vertex(values.clone()), colorizer, interpolation.policy)

  /** Interpolate raw vertex samples before mapping. Only inspectable scalar mappings
    * can enter this path; categorical constructors retain their existing policies.
    */
  def interpolatedScalar(
    id: SurfaceLayerId,
    surfaceId: SurfaceId,
    geometry: SurfaceGeometry,
    values: Array[Double],
    mapping: ScalarMapping,
    frameCount: Int = 1,
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal
  ): Either[SurfaceViewError, SurfaceLayer] =
    validateLength(geometry, frameCount, values.length).map: _ =>
      new ScalarLayer(id, surfaceId, geometry, frameCount, opacity, blendMode,
        ScalarSamples.Vertex(values.clone()), mapping.colorizer, SurfaceMapInterpolation.VertexScalar)

  def curvatureUnderlay(
    id: SurfaceLayerId,
    surfaceId: SurfaceId,
    curvature: SurfaceField[Double],
    displayGeometry: SurfaceGeometry,
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal,
    polarity: SulcalPolarity = SulcalPolarity.NegativeIsSulcal,
    window: DisplayWindow = CurvatureUnderlay.Window,
    sulcalColor: Rgba32 = Rgba32.unsafe(48, 48, 48),
    gyralColor: Rgba32 = Rgba32.unsafe(208, 208, 208)
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
        ScalarColorizer(window, polarity match
          case SulcalPolarity.PositiveIsSulcal => ColorRamp(gyralColor, sulcalColor)
          case SulcalPolarity.NegativeIsSulcal => ColorRamp(sulcalColor, gyralColor)),
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
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal,
    interpolation: SurfaceVertexInterpolation = SurfaceVertexInterpolation.Color
  ): Either[SurfaceViewError, SurfaceLayer] =
    validateLength(geometry, frameCount, values.length).map: _ =>
      new LabelLayer(id, surfaceId, geometry, frameCount, opacity, blendMode, values.clone(), colorizer, interpolation.policy)

  def mask(
    id: SurfaceLayerId,
    surfaceId: SurfaceId,
    geometry: SurfaceGeometry,
    values: Array[Boolean],
    colorizer: Colorizer[Boolean],
    frameCount: Int = 1,
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal,
    interpolation: SurfaceVertexInterpolation = SurfaceVertexInterpolation.Color
  ): Either[SurfaceViewError, SurfaceLayer] =
    validateLength(geometry, frameCount, values.length).map: _ =>
      new MaskLayer(id, surfaceId, geometry, frameCount, opacity, blendMode, values.clone(), colorizer, interpolation.policy)

  def packedRgba(
    id: SurfaceLayerId,
    surfaceId: SurfaceId,
    geometry: SurfaceGeometry,
    values: IndexedSeq[Rgba32],
    frameCount: Int = 1,
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal,
    interpolation: SurfaceVertexInterpolation = SurfaceVertexInterpolation.Color
  ): Either[SurfaceViewError, SurfaceLayer] =
    validateLength(geometry, frameCount, values.length).map: _ =>
      val packed = new Array[Int](values.length)
      var index = 0
      while index < values.length do
        packed(index) = values(index).toPackedInt
        index += 1
      new PackedLayer(id, surfaceId, geometry, frameCount, opacity, blendMode, packed, interpolation.policy)
