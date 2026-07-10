package scalafim.graphics.svg

import scalafim.graphics.*

/** The SVG backend's adoption of the shared renderer conformance contract:
  * every case must render successfully, deterministically, keep its named
  * markers, and emit numeric-only geometry.
  */
class SvgConformanceSuite extends munit.FunSuite:

  private object SvgHarness extends RendererHarness[String]:
    private val options = SvgOptions.unsafe(width = 240, height = 160)

    override def render(scene: Scene): Either[String, String] =
      SvgRenderer.render(scene, options).map(_.value).left.map(_.message)

    override def containsMarker(out: String, name: GraphicsName): Boolean =
      out.contains(s"""data-name="${name.value}"""")

    override def satisfies(out: String, requirement: RenderRequirement): Boolean =
      requirement match
        case RenderRequirement.Primitive(name, kind) =>
          namedLines(out, name).exists { line =>
            val prefix = kind match
              case RenderPrimitiveKind.Disc      => "<circle"
              case RenderPrimitiveKind.Polyline  => "<polyline"
              case RenderPrimitiveKind.Polygon   => "<polygon"
              case RenderPrimitiveKind.Rectangle => "<rect"
              case RenderPrimitiveKind.Text      => "<text"
              case RenderPrimitiveKind.Image     => "<image"
            line.startsWith(prefix)
          }
        case RenderRequirement.Group(name, clipped, rotated) =>
          namedLines(out, name).exists { line =>
            line.startsWith("<g") &&
            line.contains(" clip-path=") == clipped &&
            line.contains(" transform=\"rotate(") == rotated
          }
        case RenderRequirement.Style(name, stroke, fill, lineWidth, lineType, alpha) =>
          namedLines(out, name).exists { line =>
            !line.startsWith("<g") &&
            hasPaint(line, "stroke", stroke) &&
            hasPaint(line, "fill", fill) &&
            line.contains(s""" stroke-width="${number(lineWidth)}"""") &&
            hasLineType(line, lineType) &&
            hasOpacity(line, alpha)
          }
        case RenderRequirement.Text(name, horizontal, vertical, rotated) =>
          namedLines(out, name).exists { line =>
            line.startsWith("<text") &&
            line.contains(s""" text-anchor="${textAnchor(horizontal)}"""") &&
            line.contains(s""" dominant-baseline="${textBaseline(vertical)}"""") &&
            line.contains(" transform=\"rotate(") == rotated
          }
        case RenderRequirement.Image(name, dimensions, interpolation, alpha) =>
          namedLines(out, name).exists { line =>
            val rendering = interpolation match
              case RasterInterpolation.Nearest => "pixelated"
              case RasterInterpolation.Smooth  => "auto"
            line.startsWith("<image") &&
            line.contains(s""" data-pixel-width="${dimensions.width}"""") &&
            line.contains(s""" data-pixel-height="${dimensions.height}"""") &&
            line.contains(s""" image-rendering="$rendering"""") &&
            hasOpacity(line, alpha)
          }

    override def validate(out: String): Option[String] =
      if out.contains("calc(") then Some("output contains CSS calc expressions")
      else if out.contains("%") then Some("output contains percentage lengths")
      else if out.contains("NaN") then Some("output contains NaN coordinates")
      else if !out.startsWith("<svg xmlns=") then Some("output is not an SVG document")
      else None

    private def namedLines(out: String, name: GraphicsName): Vector[String] =
      val marker = s"""data-name="${name.value}"""
      out.linesIterator.map(_.trim).filter(_.contains(marker)).toVector

    private def hasPaint(line: String, attribute: String, paint: Option[Rgba]): Boolean =
      paint match
        case Some(color) =>
          line.contains(s""" $attribute="${hex(color)}"""") &&
            (color.alpha == 1.0 || line.contains(s""" $attribute-opacity="${number(color.alpha)}""""))
        case None =>
          line.contains(s""" $attribute="none"""")

    private def hasLineType(line: String, lineType: LineType): Boolean =
      lineType match
        case LineType.Solid  => !line.contains(" stroke-dasharray=")
        case LineType.Dashed => line.contains(""" stroke-dasharray="6 4"""")
        case LineType.Dotted => line.contains(""" stroke-dasharray="1 3"""")

    private def hasOpacity(line: String, alpha: Double): Boolean =
      if alpha == 1.0 then !line.contains(" opacity=")
      else line.contains(s""" opacity="${number(alpha)}"""")

    private def textAnchor(value: HJust): String =
      value match
        case HJust.Left   => "start"
        case HJust.Center => "middle"
        case HJust.Right  => "end"

    private def textBaseline(value: VJust): String =
      value match
        case VJust.Bottom => "text-after-edge"
        case VJust.Center => "middle"
        case VJust.Top    => "text-before-edge"

    private def hex(color: Rgba): String =
      def channel(value: Int): String =
        val encoded = value.toHexString
        if encoded.length == 1 then "0" + encoded else encoded
      "#" + channel(color.red) + channel(color.green) + channel(color.blue)

    private def number(value: Double): String =
      if value == value.toLong.toDouble then value.toLong.toString else value.toString

  test("the SVG backend passes the renderer conformance contract") {
    val violations = RendererConformance.check(SvgHarness).fold(e => fail(e.message), identity)
    assertEquals(violations, Vector.empty)
  }

  test("conformance groups can run independently for focused debugging") {
    val primitives = RendererConformance.group(ConformanceGroup.Primitive).fold(e => fail(e.message), identity)
    primitives.foreach { conformanceCase =>
      val rendered = SvgHarness.render(conformanceCase.scene)
      assert(rendered.isRight, s"case '${conformanceCase.name.value}' failed: $rendered")
    }
  }
