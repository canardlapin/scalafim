package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

opaque type SurfaceContentDigest = String

object SurfaceContentDigest:
  def make(value: String): Either[SurfaceSceneError, SurfaceContentDigest] =
    val normalized = value.trim.toLowerCase
    if normalized.length == 64 && normalized.forall(character => character.isDigit || character >= 'a' && character <= 'f') then
      Right(normalized)
    else Left(SurfaceSceneError.InvalidDigest(value))

  def unsafe(value: String): SurfaceContentDigest =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (digest: SurfaceContentDigest)
    def value: String = digest

opaque type SurfaceAssetUri = String

object SurfaceAssetUri:
  def make(value: String): Either[SurfaceSceneError, SurfaceAssetUri] =
    val normalized = value.trim
    if normalized.nonEmpty then Right(normalized)
    else Left(SurfaceSceneError.BlankAssetUri)

  def unsafe(value: String): SurfaceAssetUri =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (uri: SurfaceAssetUri)
    def value: String = uri

final case class SurfaceExternalReference(uri: SurfaceAssetUri, sha256: SurfaceContentDigest)

final case class SurfaceSceneBindings(
  surfaces: Map[SurfaceId, SurfaceExternalReference],
  layers: Map[SurfaceLayerId, SurfaceExternalReference]
)

final case class SurfaceSceneAsset(
  id: SurfaceId,
  reference: SurfaceExternalReference,
  hemisphere: CorticalHemisphere,
  kind: SurfaceKind,
  vertexCount: Int,
  faceCount: Int,
  topologyIdentity: String
)

final case class SurfaceSceneLayer(
  id: SurfaceLayerId,
  surface: SurfaceId,
  reference: SurfaceExternalReference,
  encoding: SurfaceLayerKind,
  frameCount: Int,
  blendMode: DisplayBlendMode
)

final case class SurfaceSceneLayerState(
  id: SurfaceLayerId,
  visible: Boolean,
  opacity: DisplayOpacity,
  window: Option[DisplayWindow],
  threshold: Option[DisplayThreshold]
)

final case class SurfaceProvenance private (
  producer: String,
  softwareVersion: String,
  createdAt: String,
  entries: Vector[(String, String)]
)

object SurfaceProvenance:
  def make(
    producer: String,
    softwareVersion: String,
    createdAt: String,
    entries: Iterable[(String, String)] = Vector.empty
  ): Either[SurfaceSceneError, SurfaceProvenance] =
    val normalizedProducer = producer.trim
    val normalizedVersion = softwareVersion.trim
    val normalizedCreatedAt = createdAt.trim
    val normalizedEntries = entries.iterator.map: (key, value) =>
      key.trim -> value.trim
    .toVector.sortBy(_._1)
    if normalizedProducer.isEmpty then Left(SurfaceSceneError.InvalidProvenance("producer must be non-empty"))
    else if normalizedVersion.isEmpty then Left(SurfaceSceneError.InvalidProvenance("software version must be non-empty"))
    else if normalizedCreatedAt.isEmpty then Left(SurfaceSceneError.InvalidProvenance("creation time must be non-empty"))
    else if normalizedEntries.exists((key, _) => key.isEmpty) then
      Left(SurfaceSceneError.InvalidProvenance("entry keys must be non-empty"))
    else if normalizedEntries.exists((_, value) => value.isEmpty) then
      Left(SurfaceSceneError.InvalidProvenance("entry values must be non-empty"))
    else if normalizedEntries.map(_._1).distinct.length != normalizedEntries.length then
      Left(SurfaceSceneError.InvalidProvenance("entry keys must be unique"))
    else Right(new SurfaceProvenance(normalizedProducer, normalizedVersion, normalizedCreatedAt, normalizedEntries))

enum SurfaceDocumentRevision:
  case V1

  def value: Int =
    this match
      case V1 => 1

enum SurfaceUnknownFieldPolicy:
  case Reject, Ignore

final case class SurfaceSceneReadPolicy(
  unknownFields: SurfaceUnknownFieldPolicy = SurfaceUnknownFieldPolicy.Reject
)

final case class SurfaceSceneDocument private (
  revision: SurfaceDocumentRevision,
  assets: Vector[SurfaceSceneAsset],
  layers: Vector[SurfaceSceneLayer],
  layout: SurfaceLayout,
  camera: SurfaceCamera,
  lighting: SurfaceLighting,
  clipping: SurfaceClipping,
  timepoint: Int,
  selection: Option[SurfaceSelection],
  layerStates: Vector[SurfaceSceneLayerState],
  requiredFeatures: Set[SurfaceBackendFeature],
  provenance: SurfaceProvenance
):
  def admit(capabilities: SurfaceBackendCapabilities): Either[SurfaceSceneError, Unit] =
    val missing = requiredFeatures.filterNot(capabilities.supports).toVector.sortBy(SurfaceSceneNames.feature)
    if missing.isEmpty then Right(())
    else Left(SurfaceSceneError.MissingCapabilities(missing))

  def restore(
    model: SurfaceViewerModel,
    resolved: SurfaceSceneBindings
  ): Either[SurfaceSceneError, SurfaceViewerState] =
    for
      _ <- SurfaceSceneDocument.validateBindings(this, resolved)
      _ <- SurfaceSceneDocument.validateModel(this, model)
      restored <- SurfaceSceneDocument.restoreState(this, model)
    yield restored

object SurfaceSceneDocument:
  val CurrentRevision: SurfaceDocumentRevision = SurfaceDocumentRevision.V1

  def capture(
    model: SurfaceViewerModel,
    state: SurfaceViewerState,
    bindings: SurfaceSceneBindings,
    provenance: SurfaceProvenance,
    requiredFeatures: Set[SurfaceBackendFeature] = Set.empty
  ): Either[SurfaceSceneError, SurfaceSceneDocument] =
    for
      _ <- validateBindingKeys(model, bindings)
      assets <- traverse(model.surfaces): asset =>
        bindings.surfaces.get(asset.id) match
          case None => Left(SurfaceSceneError.MissingSurfaceBinding(asset.id))
          case Some(reference) => Right(SurfaceSceneAsset(
            asset.id,
            reference,
            asset.domain.hemisphere,
            asset.geometry.kind,
            asset.domain.vertexCount,
            asset.domain.faceCount,
            asset.domain.topology.stableKey
          ))
      layers <- traverse(model.layers): layer =>
        bindings.layers.get(layer.id) match
          case None => Left(SurfaceSceneError.MissingLayerBinding(layer.id))
          case Some(reference) => Right(SurfaceSceneLayer(
            layer.id,
            layer.surfaceId,
            reference,
            layer.kind,
            layer.frameCount,
            layer.blendMode
          ))
      states <- traverse(state.layerOrder): id =>
        state.presentations.get(id) match
          case None => Left(SurfaceSceneError.InvalidDocument(s"missing presentation for layer '${id.value}'"))
          case Some(value) => Right(SurfaceSceneLayerState(
            value.id,
            value.visible,
            value.opacity,
            value.window,
            value.threshold
          ))
      result <- make(
        CurrentRevision,
        assets,
        layers,
        state.layout,
        state.camera,
        state.lighting,
        state.clipping,
        state.timepoint,
        state.selection,
        states,
        requiredFeatures,
        provenance
      )
    yield result

  def make(
    revision: SurfaceDocumentRevision,
    assets: Vector[SurfaceSceneAsset],
    layers: Vector[SurfaceSceneLayer],
    layout: SurfaceLayout,
    camera: SurfaceCamera,
    lighting: SurfaceLighting,
    clipping: SurfaceClipping,
    timepoint: Int,
    selection: Option[SurfaceSelection],
    layerStates: Vector[SurfaceSceneLayerState],
    requiredFeatures: Set[SurfaceBackendFeature],
    provenance: SurfaceProvenance
  ): Either[SurfaceSceneError, SurfaceSceneDocument] =
    if assets.isEmpty then Left(SurfaceSceneError.InvalidDocument("at least one surface asset is required"))
    else if assets.map(_.id).distinct.length != assets.length then
      Left(SurfaceSceneError.InvalidDocument("surface ids must be unique"))
    else if layers.map(_.id).distinct.length != layers.length then
      Left(SurfaceSceneError.InvalidDocument("layer ids must be unique"))
    else if layerStates.map(_.id).distinct.length != layerStates.length then
      Left(SurfaceSceneError.InvalidDocument("layer state ids must be unique"))
    else if layerStates.map(_.id).toSet != layers.map(_.id).toSet then
      Left(SurfaceSceneError.InvalidDocument("layer state ids must exactly match layer ids"))
    else if layers.exists(layer => !assets.exists(_.id == layer.surface)) then
      Left(SurfaceSceneError.InvalidDocument("every layer must reference a declared surface"))
    else
      val frameCounts = layers.iterator.map(_.frameCount).filter(_ > 1).toVector.distinct
      val frameCount = frameCounts.headOption.getOrElse(1)
      val invalidLayout = layout match
        case SurfaceLayout.Single(surface) => !assets.exists(_.id == surface)
        case SurfaceLayout.Bilateral(left, right, _) =>
          assets.find(_.id == left).forall(_.hemisphere != CorticalHemisphere.Left) ||
            assets.find(_.id == right).forall(_.hemisphere != CorticalHemisphere.Right)
      val invalidSelection = selection.exists: selected =>
        assets.find(_.id == selected.surface).forall(asset => selected.vertex.index >= asset.vertexCount)
      val invalidPresentation = layerStates.exists: state =>
        layers.find(_.id == state.id).exists: layer =>
          layer.encoding != SurfaceLayerKind.Scalar && (state.window.nonEmpty || state.threshold.nonEmpty)
      if frameCounts.length > 1 then Left(SurfaceSceneError.InvalidDocument("dynamic layer frame counts differ"))
      else if timepoint < 0 || timepoint >= frameCount then
        Left(SurfaceSceneError.InvalidDocument(s"timepoint $timepoint is outside 0..${frameCount - 1}"))
      else if invalidLayout then Left(SurfaceSceneError.InvalidDocument("layout does not match declared surface identities"))
      else if invalidSelection then Left(SurfaceSceneError.InvalidDocument("selection is outside its declared surface"))
      else if invalidPresentation then Left(SurfaceSceneError.InvalidDocument("only scalar layers may carry windows or thresholds"))
      else Right(new SurfaceSceneDocument(
      revision,
      assets,
      layers,
      layout,
      camera,
      lighting,
      clipping,
      timepoint,
      selection,
      layerStates,
      requiredFeatures,
      provenance
      ))

  private def validateBindingKeys(
    model: SurfaceViewerModel,
    bindings: SurfaceSceneBindings
  ): Either[SurfaceSceneError, Unit] =
    val surfaceIds = model.surfaces.map(_.id).toSet
    val layerIds = model.layers.map(_.id).toSet
    val extraSurfaces = bindings.surfaces.keySet -- surfaceIds
    val extraLayers = bindings.layers.keySet -- layerIds
    if extraSurfaces.nonEmpty then Left(SurfaceSceneError.UnexpectedSurfaceBinding(extraSurfaces.toVector.sortBy(_.value).head))
    else if extraLayers.nonEmpty then Left(SurfaceSceneError.UnexpectedLayerBinding(extraLayers.toVector.sortBy(_.value).head))
    else Right(())

  private def validateBindings(
    document: SurfaceSceneDocument,
    resolved: SurfaceSceneBindings
  ): Either[SurfaceSceneError, Unit] =
    val expectedSurfaces = document.assets.map(_.id).toSet
    val expectedLayers = document.layers.map(_.id).toSet
    val extraSurfaces = resolved.surfaces.keySet -- expectedSurfaces
    val extraLayers = resolved.layers.keySet -- expectedLayers
    if extraSurfaces.nonEmpty then return Left(SurfaceSceneError.UnexpectedSurfaceBinding(extraSurfaces.toVector.sortBy(_.value).head))
    if extraLayers.nonEmpty then return Left(SurfaceSceneError.UnexpectedLayerBinding(extraLayers.toVector.sortBy(_.value).head))
    var index = 0
    while index < document.assets.length do
      val expected = document.assets(index)
      resolved.surfaces.get(expected.id) match
        case None => return Left(SurfaceSceneError.MissingSurfaceBinding(expected.id))
        case Some(actual) if actual != expected.reference =>
          return Left(SurfaceSceneError.AssetReferenceMismatch(expected.id.value))
        case _ => ()
      index += 1
    index = 0
    while index < document.layers.length do
      val expected = document.layers(index)
      resolved.layers.get(expected.id) match
        case None => return Left(SurfaceSceneError.MissingLayerBinding(expected.id))
        case Some(actual) if actual != expected.reference =>
          return Left(SurfaceSceneError.AssetReferenceMismatch(expected.id.value))
        case _ => ()
      index += 1
    Right(())

  private def validateModel(
    document: SurfaceSceneDocument,
    model: SurfaceViewerModel
  ): Either[SurfaceSceneError, Unit] =
    if document.assets.map(_.id).toSet != model.surfaces.map(_.id).toSet then
      Left(SurfaceSceneError.ModelMismatch("surface ids differ"))
    else if document.layers.map(_.id).toSet != model.layers.map(_.id).toSet then
      Left(SurfaceSceneError.ModelMismatch("layer ids differ"))
    else
      var index = 0
      while index < document.assets.length do
        val expected = document.assets(index)
        model.surface(expected.id) match
          case None => return Left(SurfaceSceneError.ModelMismatch(s"surface '${expected.id.value}' is missing"))
          case Some(actual) =>
            val exact =
              actual.domain.hemisphere == expected.hemisphere &&
                actual.geometry.kind == expected.kind &&
                actual.domain.vertexCount == expected.vertexCount &&
                actual.domain.faceCount == expected.faceCount &&
                actual.domain.topology.stableKey == expected.topologyIdentity
            if !exact then return Left(SurfaceSceneError.ModelMismatch(s"surface '${expected.id.value}' identity differs"))
        index += 1
      index = 0
      while index < document.layers.length do
        val expected = document.layers(index)
        model.layer(expected.id) match
          case None => return Left(SurfaceSceneError.ModelMismatch(s"layer '${expected.id.value}' is missing"))
          case Some(actual) =>
            val exact =
              actual.surfaceId == expected.surface &&
                actual.kind == expected.encoding &&
                actual.frameCount == expected.frameCount &&
                actual.blendMode == expected.blendMode
            if !exact then return Left(SurfaceSceneError.ModelMismatch(s"layer '${expected.id.value}' identity differs"))
        index += 1
      Right(())

  private def restoreState(
    document: SurfaceSceneDocument,
    model: SurfaceViewerModel
  ): Either[SurfaceSceneError, SurfaceViewerState] =
    val actions = Vector.newBuilder[SurfaceViewerAction]
    actions += SurfaceViewerAction.SetLayout(document.layout)
    actions += SurfaceViewerAction.SetViewpoint(document.camera.viewpoint)
    actions += SurfaceViewerAction.SetProjection(document.camera.projection)
    actions += SurfaceViewerAction.SetZoom(document.camera.zoom)
    actions += SurfaceViewerAction.SetPan(document.camera.panX, document.camera.panY)
    actions += SurfaceViewerAction.SetOrbit(document.camera.orbit)
    actions += SurfaceViewerAction.SetLighting(document.lighting)
    actions += SurfaceViewerAction.SetClipping(document.clipping)
    actions += SurfaceViewerAction.SetTimepoint(document.timepoint)
    document.selection.foreach(selection => actions += SurfaceViewerAction.Select(selection.surface, selection.vertex))
    document.layerStates.zipWithIndex.foreach: (layer, index) =>
      actions += SurfaceViewerAction.SetLayerVisible(layer.id, layer.visible)
      actions += SurfaceViewerAction.SetLayerOpacity(layer.id, layer.opacity)
      layer.window.foreach(value => actions += SurfaceViewerAction.SetLayerWindow(layer.id, value))
      layer.threshold.foreach(value => actions += SurfaceViewerAction.SetLayerThreshold(layer.id, value))
      actions += SurfaceViewerAction.MoveLayer(layer.id, index)
    var state = SurfaceViewerState.initial(model)
    val values = actions.result()
    var index = 0
    while index < values.length do
      SurfaceViewer.reduce(model, state, values(index)) match
        case Left(error) => return Left(SurfaceSceneError.InvalidState(error))
        case Right(next) => state = next
      index += 1
    Right(state)

  private def traverse[A, B](values: Vector[A])(f: A => Either[SurfaceSceneError, B]): Either[SurfaceSceneError, Vector[B]] =
    val result = Vector.newBuilder[B]
    var index = 0
    while index < values.length do
      f(values(index)) match
        case Left(error) => return Left(error)
        case Right(value) => result += value
      index += 1
    Right(result.result())

enum SurfaceSceneError:
  case BlankAssetUri
  case InvalidDigest(value: String)
  case InvalidProvenance(reason: String)
  case MissingSurfaceBinding(id: SurfaceId)
  case MissingLayerBinding(id: SurfaceLayerId)
  case UnexpectedSurfaceBinding(id: SurfaceId)
  case UnexpectedLayerBinding(id: SurfaceLayerId)
  case AssetReferenceMismatch(id: String)
  case ModelMismatch(reason: String)
  case MissingCapabilities(features: Vector[SurfaceBackendFeature])
  case UnsupportedRevision(value: Int)
  case UnknownField(path: String, field: String)
  case InvalidJson(path: String, reason: String)
  case InvalidDocument(reason: String)
  case InvalidState(error: SurfaceViewError)

  def message: String =
    this match
      case BlankAssetUri => "surface asset URI must be non-empty"
      case InvalidDigest(value) => s"surface content digest must contain exactly 64 hexadecimal characters; got '$value'"
      case InvalidProvenance(reason) => s"invalid surface provenance: $reason"
      case MissingSurfaceBinding(id) => s"surface '${id.value}' has no external asset binding"
      case MissingLayerBinding(id) => s"surface layer '${id.value}' has no external asset binding"
      case UnexpectedSurfaceBinding(id) => s"external binding references unknown surface '${id.value}'"
      case UnexpectedLayerBinding(id) => s"external binding references unknown layer '${id.value}'"
      case AssetReferenceMismatch(id) => s"resolved asset reference for '$id' does not match the document"
      case ModelMismatch(reason) => s"surface scene model mismatch: $reason"
      case MissingCapabilities(features) =>
        s"surface backend lacks required features: ${features.map(SurfaceSceneNames.feature).mkString(", ")}"
      case UnsupportedRevision(value) => s"surface scene document revision $value is unsupported"
      case UnknownField(path, field) => s"unknown field '$field' at $path"
      case InvalidJson(path, reason) => s"invalid surface scene JSON at $path: $reason"
      case InvalidDocument(reason) => s"invalid surface scene document: $reason"
      case InvalidState(error) => s"surface scene state is invalid: ${error.message}"

private[view] object SurfaceSceneNames:
  def feature(value: SurfaceBackendFeature): String =
    value match
      case SurfaceBackendFeature.DeterministicPixels => "deterministic-pixels"
      case SurfaceBackendFeature.HardwareAcceleration => "hardware-acceleration"
      case SurfaceBackendFeature.DepthBuffer => "depth-buffer"
      case SurfaceBackendFeature.BackFaceCulling => "back-face-culling"
      case SurfaceBackendFeature.Lighting => "lighting"
      case SurfaceBackendFeature.WorldClipping => "world-clipping"
      case SurfaceBackendFeature.BilateralViewports => "bilateral-viewports"
      case SurfaceBackendFeature.NativePicking => "native-picking"
      case SurfaceBackendFeature.HighResolutionSnapshot => "high-resolution-snapshot"
      case SurfaceBackendFeature.GpuVolumeProjection => "gpu-volume-projection"
