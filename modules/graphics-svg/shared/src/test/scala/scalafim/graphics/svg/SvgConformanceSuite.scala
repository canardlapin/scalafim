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

    override def validate(out: String): Option[String] =
      if out.contains("calc(") then Some("output contains CSS calc expressions")
      else if out.contains("%") then Some("output contains percentage lengths")
      else if out.contains("NaN") then Some("output contains NaN coordinates")
      else if !out.startsWith("<svg xmlns=") then Some("output is not an SVG document")
      else None

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
