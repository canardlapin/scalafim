package scalafim.surface.view

import intaglio.*

/** A saved request plus the identity that restoration must reproduce. */
final case class SurfaceSceneLegend(request: SurfaceLegendRequest, canonicalKey: String):
  def layerIds: Vector[SurfaceLayerId] = request match
    case SurfaceLegendRequest.Continuous(layers, _, _, _, _) => layers.ids
    case SurfaceLegendRequest.Split(layers, _, _, _, _) => layers.ids
    case SurfaceLegendRequest.Categorical(layers, _, _, _) => layers.ids
    case SurfaceLegendRequest.Manual(_, _) => Vector.empty

private[view] object SurfaceSceneLegendCodec:
  def encode(saved: SurfaceSceneLegend): SceneJson =
    def common(kind: String, title: LegendTitle): Vector[(String, SceneJson)] = Vector(
      "kind" -> SceneJson.Str(kind), "quantity" -> SceneJson.Str(title.quantity),
      "units" -> title.units.fold[SceneJson](SceneJson.Null)(SceneJson.Str.apply),
      "canonicalKey" -> SceneJson.Str(saved.canonicalKey))
    def layers(value: SurfaceLegendLayers): (String, SceneJson) =
      "layers" -> SceneJson.Arr(value.ids.map(id => SceneJson.Str(id.value)))
    def scalar(kind: String, ids: SurfaceLegendLayers, title: LegendTitle, ticks: Vector[AxisTick], hidden: Boolean, invalid: Boolean): SceneJson =
      SceneJson.Obj(common(kind, title) ++ Vector(layers(ids),
        "ticks" -> SceneJson.Arr(ticks.sortBy(_.value).map(t => SceneJson.obj(
          "value" -> SceneJson.Num(t.value.toString), "label" -> SceneJson.Str(t.label)))),
        "showHidden" -> SceneJson.Bool(hidden), "showInvalid" -> SceneJson.Bool(invalid)))
    saved.request match
      case SurfaceLegendRequest.Continuous(ids, title, ticks, hidden, invalid) => scalar("continuous", ids, title, ticks, hidden, invalid)
      case SurfaceLegendRequest.Split(ids, title, ticks, hidden, invalid) => scalar("split", ids, title, ticks, hidden, invalid)
      case SurfaceLegendRequest.Categorical(ids, title, labels, fallback) =>
        SceneJson.Obj(common("categorical", title) ++ Vector(layers(ids),
          "labels" -> SceneJson.Arr(labels.toVector.sortBy(_._1).map((id, label) => SceneJson.obj(
            "id" -> SceneJson.Num(id.toString), "label" -> SceneJson.Str(label.trim)))),
          "fallbackLabel" -> SceneJson.Str(fallback.trim)))
      case SurfaceLegendRequest.Manual(title, entries) =>
        SceneJson.Obj(common("manual", title) :+ ("entries" -> SceneJson.Arr(entries.map(entry => SceneJson.obj(
          "label" -> SceneJson.Str(entry.label), "rgba" -> SceneJson.Num(entry.color.toPackedInt.toString))))))

  def decode(value: SceneJson, path: String, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, SurfaceSceneLegend] =
    for
      obj <- SceneJsonRead.obj(value, path, Set.empty, policy, allowAny = true)
      kind <- SceneJsonRead.string(obj, "kind", s"$path.kind")
      fields <- kind match
        case "continuous" | "split" => Right(Set("layers", "ticks", "showHidden", "showInvalid"))
        case "categorical" => Right(Set("layers", "labels", "fallbackLabel"))
        case "manual" => Right(Set("entries"))
        case _ => Left(SurfaceSceneError.InvalidJson(s"$path.kind", s"unknown legend kind '$kind'"))
      _ <- SceneJsonRead.checkFields(obj, path, fields ++ Set("kind", "quantity", "units", "canonicalKey"), policy)
      quantity <- SceneJsonRead.string(obj, "quantity", s"$path.quantity")
      unitsJson <- SceneJsonRead.value(obj, "units", s"$path.units")
      units <- unitsJson match
        case SceneJson.Null => Right(None)
        case other => SceneJsonRead.stringValue(other, s"$path.units").map(Some.apply)
      title <- LegendTitle.make(quantity, units).left.map(error => SurfaceSceneError.InvalidJson(path, error.message))
      key <- SceneJsonRead.string(obj, "canonicalKey", s"$path.canonicalKey")
      _ <- if key.trim.nonEmpty then Right(()) else Left(SurfaceSceneError.InvalidJson(path, "legend identity must be nonempty"))
      request <- if kind == "manual" then manual(obj, title, path, policy)
        else
          for
            idsJson <- SceneJsonRead.array(obj, "layers", s"$path.layers")
            ids <- traverse(idsJson, s"$path.layers"): (entry, location) =>
              SceneJsonRead.stringValue(entry, location).flatMap(raw =>
                SurfaceLayerId.make(raw).left.map(error => SurfaceSceneError.InvalidJson(location, error.message)))
            layers <- SurfaceLegendLayers.make(ids).left.map(error => SurfaceSceneError.InvalidJson(path, error.message))
            result <- if kind == "categorical" then categorical(obj, layers, title, path, policy)
              else scalar(obj, kind, layers, title, path, policy)
          yield result
    yield SurfaceSceneLegend(request, key)

  private def scalar(obj: SceneJson.Obj, kind: String, layers: SurfaceLegendLayers, title: LegendTitle,
    path: String, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, SurfaceLegendRequest] =
    for
      ticksJson <- SceneJsonRead.array(obj, "ticks", s"$path.ticks")
      ticks <- traverse(ticksJson, s"$path.ticks"): (value, location) =>
        for
          tick <- SceneJsonRead.obj(value, location, Set("value", "label"), policy)
          number <- SceneJsonRead.double(tick, "value", s"$location.value")
          label <- SceneJsonRead.string(tick, "label", s"$location.label")
          _ <- if label.trim.nonEmpty then Right(()) else Left(SurfaceSceneError.InvalidJson(location, "tick label must be nonempty"))
        yield AxisTick.unsafe(number, label)
      _ <- if ticks.map(_.value).distinct.length == ticks.length then Right(())
        else Left(SurfaceSceneError.InvalidJson(path, "tick values must be unique"))
      hidden <- SceneJsonRead.bool(obj, "showHidden", s"$path.showHidden")
      invalid <- SceneJsonRead.bool(obj, "showInvalid", s"$path.showInvalid")
    yield if kind == "split" then SurfaceLegendRequest.Split(layers, title, ticks, hidden, invalid)
      else SurfaceLegendRequest.Continuous(layers, title, ticks, hidden, invalid)

  private def categorical(obj: SceneJson.Obj, layers: SurfaceLegendLayers, title: LegendTitle,
    path: String, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, SurfaceLegendRequest] =
    for
      labelsJson <- SceneJsonRead.array(obj, "labels", s"$path.labels")
      labels <- traverse(labelsJson, s"$path.labels"): (value, location) =>
        for
          entry <- SceneJsonRead.obj(value, location, Set("id", "label"), policy)
          id <- SceneJsonRead.int(entry, "id", s"$location.id")
          label <- SceneJsonRead.string(entry, "label", s"$location.label")
        yield id -> label.trim
      fallback <- SceneJsonRead.string(obj, "fallbackLabel", s"$path.fallbackLabel")
      _ <- if labels.isEmpty || labels.map(_._1).distinct.length != labels.length ||
          labels.exists(_._2.isEmpty) || fallback.trim.isEmpty then
        Left(SurfaceSceneError.InvalidJson(path, "category keys must be unique and labels nonempty")) else Right(())
    yield SurfaceLegendRequest.Categorical(layers, title, labels.toMap, fallback.trim)

  private def manual(obj: SceneJson.Obj, title: LegendTitle, path: String,
    policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, SurfaceLegendRequest] =
    for
      entriesJson <- SceneJsonRead.array(obj, "entries", s"$path.entries")
      entries <- traverse(entriesJson, s"$path.entries"): (value, location) =>
        for
          entry <- SceneJsonRead.obj(value, location, Set("label", "rgba"), policy)
          label <- SceneJsonRead.string(entry, "label", s"$location.label")
          rgba <- SceneJsonRead.int(entry, "rgba", s"$location.rgba")
          swatch <- SurfaceLegendItem.make(label, Rgba32.fromPackedInt(rgba))
            .left.map(error => SurfaceSceneError.InvalidJson(location, error.message))
        yield swatch
      _ <- if entries.nonEmpty then Right(()) else Left(SurfaceSceneError.InvalidJson(path, "manual legend needs entries"))
    yield SurfaceLegendRequest.Manual(title, entries)

  private def traverse[A](values: Vector[SceneJson], path: String)(f: (SceneJson, String) => Either[SurfaceSceneError, A])
      : Either[SurfaceSceneError, Vector[A]] =
    values.zipWithIndex.foldLeft[Either[SurfaceSceneError, Vector[A]]](Right(Vector.empty)):
      case (result, (value, index)) => for
        previous <- result
        next <- f(value, s"$path[$index]")
      yield previous :+ next
