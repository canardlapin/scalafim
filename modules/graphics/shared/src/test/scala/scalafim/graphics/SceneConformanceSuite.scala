package scalafim.graphics

/** Runs the renderer conformance contract against the shared device lowering,
  * which acts as the reference backend: every case must resolve to numeric
  * device primitives deterministically with all named markers intact.
  */
class SceneConformanceSuite extends munit.FunSuite:

  private object DeviceHarness extends RendererHarness[DeviceScene]:
    private val device = DeviceContext.unsafe(240.0, 160.0)

    override def render(scene: Scene): Either[String, DeviceScene] =
      DeviceScene.fromScene(scene, device).left.map(_.message)

    override def containsMarker(out: DeviceScene, name: GraphicsName): Boolean =
      out.elements.exists(containsName(_, name))

    override def validate(out: DeviceScene): Option[String] =
      firstNonFinite(out.elements)

    private def containsName(element: DeviceElement, name: GraphicsName): Boolean =
      element match
        case DeviceElement.Mark(primitive) =>
          primitiveName(primitive).contains(name)
        case DeviceElement.Group(groupName, _, _, children) =>
          groupName.contains(name) || children.exists(containsName(_, name))

    private def primitiveName(primitive: DevicePrimitive): Option[GraphicsName] =
      primitive match
        case DevicePrimitive.Disc(_, _, _, _, name)          => name
        case DevicePrimitive.Polyline(_, _, _, name)         => name
        case DevicePrimitive.RectShape(_, _, _, _, _, name)  => name
        case DevicePrimitive.TextRun(_, _, _, _, _, _, _, _, _, name) => name

    private def firstNonFinite(elements: Vector[DeviceElement]): Option[String] =
      elements.iterator.map(nonFinite).collectFirst { case Some(problem) => problem }

    private def nonFinite(element: DeviceElement): Option[String] =
      element match
        case DeviceElement.Mark(primitive) =>
          val values = primitive match
            case DevicePrimitive.Disc(cx, cy, r, _, _)              => Vector(cx, cy, r)
            case DevicePrimitive.Polyline(points, _, _, _)          => points.flatMap(p => Vector(p.x, p.y))
            case DevicePrimitive.RectShape(x, y, w, h, _, _)        => Vector(x, y, w, h)
            case DevicePrimitive.TextRun(_, x, y, _, _, rot, fs, _, _, _) => Vector(x, y, rot, fs)
          if values.forall(_.isFinite) then None
          else Some(s"non-finite device coordinate in $primitive")
        case DeviceElement.Group(_, _, _, children) =>
          firstNonFinite(children)

  test("conformance cases cover all behavior groups") {
    val cases = RendererConformance.cases.fold(e => fail(e.message), identity)
    val groups = cases.map(_.group).toSet
    assertEquals(
      groups,
      Set[ConformanceGroup](
        ConformanceGroup.Primitive,
        ConformanceGroup.Layout,
        ConformanceGroup.Guide,
        ConformanceGroup.CompiledPlot
      )
    )
    assert(cases.forall(!_.scene.isEmpty))
    assert(cases.forall(_.markers.nonEmpty))
    assertEquals(cases.map(_.name.value).distinct.length, cases.length)
  }

  test("group selection filters cases by behavior family") {
    val primitives = RendererConformance.group(ConformanceGroup.Primitive).fold(e => fail(e.message), identity)
    assert(primitives.nonEmpty)
    assert(primitives.forall(_.group == ConformanceGroup.Primitive))
  }

  test("the device lowering passes the full conformance contract") {
    val violations = RendererConformance.check(DeviceHarness).fold(e => fail(e.message), identity)
    assertEquals(violations, Vector.empty)
  }

  test("the explicit scaled-plot fixture frames every point disc inside its clip") {
    val scene = RendererConformance.scaledPlotCase.fold(e => fail(e.message), identity).scene
    val device = DeviceScene
      .fromScene(scene, DeviceContext.unsafe(640.0, 480.0))
      .fold(e => fail(e.message), identity)
    val (clip, discs) = device.elements.collectFirst {
      case DeviceElement.Group(name, Some(clip), _, children)
          if name.exists(_.value == "plot-panel") =>
        val discs = children.collect {
          case DeviceElement.Mark(disc: DevicePrimitive.Disc) => disc
        }
        (clip, discs)
    }.getOrElse(fail("missing scaled-plot panel"))

    assertEquals(discs.length, 3)
    discs.foreach { disc =>
      assert(disc.centerX - disc.radius >= clip.x)
      assert(disc.centerX + disc.radius <= clip.x + clip.width)
      assert(disc.centerY - disc.radius >= clip.y)
      assert(disc.centerY + disc.radius <= clip.y + clip.height)
    }
  }

  test("the checker reports missing markers and failures as violations") {
    object BlindHarness extends RendererHarness[String]:
      override def render(scene: Scene): Either[String, String] = Right("")
      override def containsMarker(out: String, name: GraphicsName): Boolean = false
    val violations = RendererConformance.check(BlindHarness).fold(e => fail(e.message), identity)
    assert(violations.nonEmpty)
    assert(violations.forall(_.problem.startsWith("missing marker")))
  }
