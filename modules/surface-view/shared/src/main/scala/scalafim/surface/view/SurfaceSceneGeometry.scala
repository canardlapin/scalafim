package scalafim.surface.view

import scalafim.surface.*

/** A reference to an admitted geometry family and its native presentation, never copied meshes.
  * As with the scene's other external references, the resolver must verify bytes before supplying
  * bindings. A label or topology match alone is not proof of geometry content identity.
  */
final case class SurfaceSceneGeometry(
  surface: SurfaceId,
  variants: Map[SurfaceKind, SurfaceExternalReference],
  presentation: SurfaceGeometryPresentation
):
  private[view] def valid: Boolean = variants.nonEmpty && (presentation match
    case SurfaceGeometryPresentation.Fixed(kind) => variants.contains(kind)
    case SurfaceGeometryPresentation.Morphing(from, to, fraction) =>
      from != to && fraction.value < 1.0 && variants.contains(from) && variants.contains(to)
    case _: SurfaceGeometryPresentation.RevealLens => false)

  private[view] def actions: Vector[SurfaceViewerAction] = presentation match
    case SurfaceGeometryPresentation.Fixed(kind) => Vector(SurfaceViewerAction.SetGeometryState(surface, kind))
    case SurfaceGeometryPresentation.Morphing(from, to, fraction) => Vector(
      SurfaceViewerAction.SetGeometryState(surface, from),
      SurfaceViewerAction.BeginGeometryMorph(surface, to),
      SurfaceViewerAction.SetGeometryMorphFraction(surface, fraction))
    case _: SurfaceGeometryPresentation.RevealLens => Vector.empty // rejected before restore

object SurfaceSceneGeometry:
  private def bindingsFor(asset: SurfaceAsset, bindings: SurfaceSceneBindings)
      : Either[SurfaceSceneError, Map[SurfaceKind, SurfaceExternalReference]] =
    val references = bindings.geometries.get(asset.id).orElse(
      Option.when(asset.geometries.surfaces.size == 1)(bindings.surfaces.get(asset.id).map(
        reference => Map(asset.geometry.kind -> reference))).flatten)
    references.filter(_.keySet == asset.geometries.surfaces.keySet).toRight(
      SurfaceSceneError.ModelMismatch(s"geometry family '${asset.id.value}' needs verified references for every variant"))

  private[view] def capture(model: SurfaceViewerModel, state: SurfaceViewerState, bindings: SurfaceSceneBindings)
      : Either[SurfaceSceneError, Vector[SurfaceSceneGeometry]] =
    if !bindings.geometries.keySet.subsetOf(model.surfaces.map(_.id).toSet) then
      Left(SurfaceSceneError.ModelMismatch("geometry bindings contain an unknown surface"))
    else model.surfaces.foldLeft[Either[SurfaceSceneError, Vector[SurfaceSceneGeometry]]](Right(Vector.empty)):
      (prior, asset) => for
        values <- prior
        references <- bindingsFor(asset, bindings)
        presentation <- state.geometryPresentations.get(asset.id).toRight(
          SurfaceSceneError.InvalidDocument(s"missing geometry presentation for '${asset.id.value}'"))
        geometry = SurfaceSceneGeometry(asset.id, references, presentation)
        _ <- Either.cond(geometry.valid, (), SurfaceSceneError.InvalidDocument(
          "scene serialization supports Fixed and Morphing geometry; RevealLens requires a deformation asset codec"))
        _ <- asset.resolve(presentation).left.map(SurfaceSceneError.InvalidState.apply)
      yield values :+ geometry

  private[view] def validateResolved(states: Vector[SurfaceSceneGeometry], model: SurfaceViewerModel,
      bindings: SurfaceSceneBindings): Either[SurfaceSceneError, Unit] =
    states.foldLeft[Either[SurfaceSceneError, Unit]](Right(())):
      (prior, saved) =>
        for
          _ <- prior
          asset <- model.surface(saved.surface).toRight(SurfaceSceneError.ModelMismatch("geometry surface is missing"))
          actual <- bindingsFor(asset, bindings)
          _ <- Either.cond(actual == saved.variants, (),
            SurfaceSceneError.AssetReferenceMismatch(s"geometry-family:${saved.surface.value}"))
          _ <- asset.resolve(saved.presentation).left.map(SurfaceSceneError.InvalidState.apply)
        yield ()

/** Kept separate from the main codec so the new revision does not alter legacy JSON shapes. */
private[view] object SurfaceSceneGeometryCodec:
  def encode(value: SurfaceSceneGeometry): SceneJson =
    val presentation = value.presentation match
      case SurfaceGeometryPresentation.Fixed(kind) => SceneJson.obj(
        "kind" -> SceneJson.Str("fixed"), "geometry" -> SceneJson.Str(kind.label))
      case SurfaceGeometryPresentation.Morphing(from, to, fraction) => SceneJson.obj(
        "kind" -> SceneJson.Str("morphing"), "from" -> SceneJson.Str(from.label), "to" -> SceneJson.Str(to.label),
        "fraction" -> SceneJson.Num(fraction.value.toString))
      case _: SurfaceGeometryPresentation.RevealLens =>
        throw new IllegalArgumentException("RevealLens is not a serializable geometry presentation")
    SceneJson.obj("surface" -> SceneJson.Str(value.surface.value), "presentation" -> presentation,
      "variants" -> SceneJson.Arr(value.variants.toVector.sortBy(_._1.label).map: (kind, reference) =>
        SceneJson.obj("kind" -> SceneJson.Str(kind.label), "uri" -> SceneJson.Str(reference.uri.value),
          "sha256" -> SceneJson.Str(reference.sha256.value))))

  def decode(value: SceneJson, index: Int, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, SurfaceSceneGeometry] =
    val path = s"$$.geometryStates[$index]"
    def kind(raw: String): Either[SurfaceSceneError, SurfaceKind] =
      val parsed = SurfaceKind.fromString(raw)
      Either.cond(parsed.label == raw, parsed, SurfaceSceneError.InvalidJson(path, s"noncanonical geometry kind: $raw"))
    for
      obj <- SceneJsonRead.obj(value, path, Set("surface", "variants", "presentation"), policy)
      id <- SceneJsonRead.string(obj, "surface", path).flatMap(SurfaceId.make(_).left.map(SurfaceSceneError.InvalidState.apply))
      array <- SceneJsonRead.array(obj, "variants", path)
      variants <- array.foldLeft[Either[SurfaceSceneError, Vector[(SurfaceKind, SurfaceExternalReference)]]](Right(Vector.empty)):
        (prior, entry) => for
          values <- prior
          ref <- SceneJsonRead.obj(entry, path, Set("kind", "uri", "sha256"), policy)
          k <- SceneJsonRead.string(ref, "kind", path).flatMap(kind)
          uri <- SceneJsonRead.string(ref, "uri", path).flatMap(SurfaceAssetUri.make)
          sha <- SceneJsonRead.string(ref, "sha256", path).flatMap(SurfaceContentDigest.make)
        yield values :+ (k -> SurfaceExternalReference(uri, sha))
      _ <- Either.cond(variants.map(_._1).distinct.size == variants.size, (), SurfaceSceneError.InvalidJson(path, "duplicate geometry variant"))
      raw <- SceneJsonRead.value(obj, "presentation", path)
      tagObject <- SceneJsonRead.obj(raw, path, Set("kind", "geometry", "from", "to", "fraction"), policy)
      tag <- SceneJsonRead.string(tagObject, "kind", path)
      presentation <- tag match
        case "fixed" =>
          for
            fields <- SceneJsonRead.obj(raw, path, Set("kind", "geometry"), policy)
            geometry <- SceneJsonRead.string(fields, "geometry", path).flatMap(kind)
          yield SurfaceGeometryPresentation.Fixed(geometry)
        case "morphing" =>
          for
            fields <- SceneJsonRead.obj(raw, path, Set("kind", "from", "to", "fraction"), policy)
            from <- SceneJsonRead.string(fields, "from", path).flatMap(kind)
            to <- SceneJsonRead.string(fields, "to", path).flatMap(kind)
            number <- SceneJsonRead.double(fields, "fraction", path)
            fraction <- SurfaceMorphFraction.make(number).left.map(SurfaceSceneError.InvalidState.apply)
          yield SurfaceGeometryPresentation.Morphing(from, to, fraction)
        case other => Left(SurfaceSceneError.InvalidJson(path, s"unsupported geometry presentation: $other"))
      result = SurfaceSceneGeometry(id, variants.toMap, presentation)
      _ <- Either.cond(result.valid, (), SurfaceSceneError.InvalidJson(path, "presentation does not belong to its geometry family"))
    yield result
