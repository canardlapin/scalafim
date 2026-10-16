package scalafim.surface.view

import intaglio.*

/** A saved request plus the identity that restoration must reproduce. */
final case class SurfaceSceneLegend(request: SurfaceLegendRequest, canonicalKey: String):
  def layerIds: Vector[SurfaceLayerId] = request match
    case SurfaceLegendRequest.Bivariate(layers, _, _, _, _, _, _) => layers.ids
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
      case SurfaceLegendRequest.Bivariate(ids, effectTitle, confidenceTitle, effectTicks, confidenceTicks, width, height) =>
        SceneJson.Obj(Vector(
          "kind" -> SceneJson.Str("bivariate"),
          "layers" -> SceneJson.Arr(ids.ids.map(id => SceneJson.Str(id.value))),
          "effectQuantity" -> SceneJson.Str(effectTitle.quantity),
          "effectUnits" -> effectTitle.units.fold[SceneJson](SceneJson.Null)(SceneJson.Str.apply),
          "confidenceQuantity" -> SceneJson.Str(confidenceTitle.quantity),
          "confidenceUnits" -> confidenceTitle.units.fold[SceneJson](SceneJson.Null)(SceneJson.Str.apply),
          "effectTicks" -> ticksJson(effectTicks),
          "confidenceTicks" -> ticksJson(confidenceTicks),
          "width" -> SceneJson.Num(width.toString),
          "height" -> SceneJson.Num(height.toString),
          "canonicalKey" -> SceneJson.Str(saved.canonicalKey)))
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
        case "bivariate" => Right(Set("layers", "effectQuantity", "effectUnits", "confidenceQuantity", "confidenceUnits", "effectTicks", "confidenceTicks", "width", "height"))
        case "continuous" | "split" => Right(Set("layers", "ticks", "showHidden", "showInvalid"))
        case "categorical" => Right(Set("layers", "labels", "fallbackLabel"))
        case "manual" => Right(Set("entries"))
        case _ => Left(SurfaceSceneError.InvalidJson(s"$path.kind", s"unknown legend kind '$kind'"))
      common = if kind == "bivariate" then Set("kind", "canonicalKey") else Set("kind", "quantity", "units", "canonicalKey")
      _ <- SceneJsonRead.checkFields(obj, path, fields ++ common, policy)
      key <- SceneJsonRead.string(obj, "canonicalKey", s"$path.canonicalKey")
      _ <- if key.trim.nonEmpty then Right(()) else Left(SurfaceSceneError.InvalidJson(path, "legend identity must be nonempty"))
      request <- if kind == "bivariate" then bivariate(obj, path, policy)
        else
          for
            legendTitle <- title(obj, "quantity", "units", path)
            result <- if kind == "manual" then manual(obj, legendTitle, path, policy)
              else
                for
                  layers <- layerIds(obj, path)
                  value <- if kind == "categorical" then categorical(obj, layers, legendTitle, path, policy)
                    else scalar(obj, kind, layers, legendTitle, path, policy)
                yield value
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

  private def bivariate(obj: SceneJson.Obj, path: String, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, SurfaceLegendRequest] =
    for
      layers <- layerIds(obj, path)
      effectTitle <- title(obj, "effectQuantity", "effectUnits", path)
      confidenceTitle <- title(obj, "confidenceQuantity", "confidenceUnits", path)
      effectTicks <- ticks(obj, "effectTicks", path, policy)
      confidenceTicks <- ticks(obj, "confidenceTicks", path, policy)
      width <- SceneJsonRead.int(obj, "width", s"$path.width")
      height <- SceneJsonRead.int(obj, "height", s"$path.height")
    yield SurfaceLegendRequest.Bivariate(layers, effectTitle, confidenceTitle, effectTicks, confidenceTicks, width, height)

  private def layerIds(obj: SceneJson.Obj, path: String): Either[SurfaceSceneError, SurfaceLegendLayers] =
    for
      idsJson <- SceneJsonRead.array(obj, "layers", s"$path.layers")
      ids <- traverse(idsJson, s"$path.layers"): (entry, location) =>
        SceneJsonRead.stringValue(entry, location).flatMap(raw =>
          SurfaceLayerId.make(raw).left.map(error => SurfaceSceneError.InvalidJson(location, error.message)))
      layers <- SurfaceLegendLayers.make(ids).left.map(error => SurfaceSceneError.InvalidJson(path, error.message))
    yield layers

  private def title(obj: SceneJson.Obj, quantityField: String, unitsField: String, path: String): Either[SurfaceSceneError, LegendTitle] =
    for
      quantity <- SceneJsonRead.string(obj, quantityField, s"$path.$quantityField")
      unitsJson <- SceneJsonRead.value(obj, unitsField, s"$path.$unitsField")
      units <- unitsJson match
        case SceneJson.Null => Right(None)
        case other => SceneJsonRead.stringValue(other, s"$path.$unitsField").map(Some.apply)
      result <- LegendTitle.make(quantity, units).left.map(error => SurfaceSceneError.InvalidJson(path, error.message))
    yield result

  private def ticks(obj: SceneJson.Obj, field: String, path: String, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, Vector[AxisTick]] =
    val tickPath = s"$path.$field"
    for
      values <- SceneJsonRead.array(obj, field, tickPath)
      result <- traverse(values, tickPath): (value, location) =>
        for
          tick <- SceneJsonRead.obj(value, location, Set("value", "label"), policy)
          number <- SceneJsonRead.double(tick, "value", s"$location.value")
          label <- SceneJsonRead.string(tick, "label", s"$location.label")
          _ <- if label.trim.nonEmpty then Right(()) else Left(SurfaceSceneError.InvalidJson(location, "tick label must be nonempty"))
        yield AxisTick.unsafe(number, label)
      _ <- if result.nonEmpty && result.map(_.value).distinct.length == result.length then Right(())
        else Left(SurfaceSceneError.InvalidJson(tickPath, "tick values must be nonempty and unique"))
    yield result

  private def ticksJson(values: Vector[AxisTick]): SceneJson =
    SceneJson.Arr(values.map(t => SceneJson.obj("value" -> SceneJson.Num(t.value.toString), "label" -> SceneJson.Str(t.label))))

  private def traverse[A](values: Vector[SceneJson], path: String)(f: (SceneJson, String) => Either[SurfaceSceneError, A])
      : Either[SurfaceSceneError, Vector[A]] =
    values.zipWithIndex.foldLeft[Either[SurfaceSceneError, Vector[A]]](Right(Vector.empty)):
      case (result, (value, index)) => for
        previous <- result
        next <- f(value, s"$path[$index]")
      yield previous :+ next
