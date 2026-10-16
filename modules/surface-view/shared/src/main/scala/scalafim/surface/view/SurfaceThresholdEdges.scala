package scalafim.surface.view

import intaglio.*

/** Original-vertex overlay inputs for the explicit SurfViewJS threshold policy.
  * Signs encode visibility, not scalar distance: equality and invalid samples
  * are hidden. The overlay changes RGB before lighting and preserves base alpha.
  */
/** Raw packets are validated at rendering boundaries so direct callers receive
  * typed failures rather than constructor exceptions or native buffer faults.
  */
final case class SurfaceThresholdEdgePacket(signs: FloatBufferView, colors: IntBufferView)

object SurfaceThresholdEdgeValidation:
  def validatePacket(mesh: SurfaceMeshPacket, layer: SurfaceLayerPacket): Either[String, Unit] =
    layer.thresholdEdge match
      case None => Right(())
      case Some(edge) =>
        val samples = mesh.sampleNormals.getOrElse(mesh.normals).length / 3
        if layer.interpolation != SurfaceMapInterpolation.VertexColor then
          Left("threshold edges require vertex-color interpolation")
        else if edge.signs.length != samples || edge.colors.length != samples then
          Left("threshold edge inputs must cover every original vertex")
        else
          var sample = 0
          var error: Option[String] = None
          while sample < samples && error.isEmpty do
            val sign = edge.signs(sample)
            if !sign.isFinite || (sign != -1f && sign != 1f) then
              error = Some("threshold edge visibility signs must be finite -1 or 1")
            sample += 1
          error match
            case Some(reason) => Left(reason)
            case None => Right(())

  def validate(plan: SurfaceRenderPlan): Either[String, Unit] =
    if !plan.layers.exists(_.thresholdEdge.nonEmpty) then Right(())
    else if plan.colorPipeline != SurfaceColorPipeline.DisplaySrgbLinearLight then
      Left("threshold edges require the linear-light color pipeline")
    else
      var error: Option[String] = None
      var index = 0
      while index < plan.layers.length && error.isEmpty do
        val layer = plan.layers(index)
        layer.thresholdEdge.foreach: edge =>
          if error.isEmpty then
            val prefix = s"threshold edge layer '${layer.layer.value}'"
            plan.meshes.find(_.surface == layer.surface) match
              case None => error = Some(s"$prefix has no matching mesh")
              case Some(mesh) =>
                validatePacket(mesh, layer) match
                  case Left(reason) => error = Some(s"$prefix $reason")
                  case Right(_) => ()
                if error.isEmpty && !plan.fragmentSurfaces(layer.surface) then
                  error = Some(s"$prefix requires fragment surface semantics")
                if error.isEmpty && index != plan.layers.lastIndexWhere(_.surface == layer.surface) then
                  error = Some(s"$prefix must be topmost")
                if error.isEmpty && layer.blendMode != DisplayBlendMode.Normal then
                  error = Some(s"$prefix must use normal blending")
                if error.isEmpty && plan.layers.exists(candidate =>
                  candidate.surface == layer.surface && candidate.interpolation != SurfaceMapInterpolation.VertexColor) then
                  error = Some(s"$prefix requires a vertex-color stack")
        index += 1
      error match
        case Some(reason) => Left(reason)
        case None => Right(())

private[view] object SurfaceThresholdEdges:
  def interval(mapping: ScalarMapping): Option[ScalarInterval] = mapping.visibility match
    case ScalarVisibility.Outside(value) if value.endpoints == ScalarEndpointInclusion.Both && value.upper > value.lower => Some(value)
    case _ => None

  def packet(values: Array[Double], mapping: ScalarMapping): Option[SurfaceThresholdEdgePacket] =
    interval(mapping).map: bounds =>
      val signs = new Array[Float](values.length)
      val colors = new Array[Int](values.length)
      val middle = bounds.lower * .5 + bounds.upper * .5
      // Scale before subtracting so finite, opposite-sign endpoints cannot
      // overflow and turn a valid overlay sample into the invalid color.
      val nudge = math.max(math.max(math.abs(mapping.scale.window.upper * 1e-6 - mapping.scale.window.lower * 1e-6),
        bounds.upper * 1e-6 - bounds.lower * 1e-6), 1e-18)
      val above = math.min(Double.MaxValue, bounds.upper + nudge)
      val below = math.max(-Double.MaxValue, bounds.lower - nudge)
      val unmasked = mapping.copy(visibility = ScalarVisibility.All)
      var vertex = 0
      while vertex < values.length do
        val value = values(vertex)
        signs(vertex) = if value.isFinite && (value < bounds.lower || value > bounds.upper) then 1f else -1f
        colors(vertex) = if !value.isFinite then 0 else
          val shown = if value >= middle then math.max(value, above) else math.min(value, below)
          unmasked.color(shown).toPackedInt
        vertex += 1
      SurfaceThresholdEdgePacket(new FloatBufferView(signs), new IntBufferView(colors))
