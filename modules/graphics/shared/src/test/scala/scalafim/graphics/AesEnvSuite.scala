package scalafim.graphics

class AesEnvSuite extends munit.FunSuite:
  private final case class Row(x: Double, y: Double, condition: String)

  private def colorScale: DiscreteScale[Rgba] =
    DiscreteScale(
      "condition-color",
      DiscreteDomain.ordered(Vector("A", "B")).fold(e => fail(e.message), identity),
      DiscretePalette.valuesUnsafe(Vector(Rgba.unsafe(10, 20, 30), Rgba.unsafe(40, 50, 60)))
    ).fold(e => fail(e.message), identity)

  private def xScale: ContinuousScale[Double] =
    ContinuousScale
      .train("x-position", Vector(0.0, 1.0, 2.0), Palette.numeric)
      .fold(e => fail(e.message), identity)

  test("AesSpec.env normalizes exactly the bound aesthetics in declaration order") {
    val spec = AesSpec
      .empty[Row]
      .withPosition(_.x, _.y)
      .withColor(Rgba.Black)
    val env = spec.env
    assertEquals(env.bound, Vector[Aesthetic[?]](Aesthetic.X, Aesthetic.Y, Aesthetic.Color))
    assert(env.get(Aesthetic.X).nonEmpty)
    assert(env.get(Aesthetic.Fill).isEmpty)
  }

  test("env round-trips through AesSpec.fromEnv") {
    val spec = AesSpec
      .empty[Row]
      .withPosition(_.x, _.y)
      .withAlpha(0.5)
      .withLabel(_.condition)
    val restored = AesSpec.fromEnv(spec.env)
    assertEquals(restored.env.bound, spec.env.bound)
    val row = Row(1.0, 2.0, "A")
    assertEquals(restored.alpha.flatMap(_.map(row)), Some(0.5))
    assertEquals(restored.label.flatMap(_.map(row)), Some("A"))
  }

  test("typed lookup returns values at the aesthetic's own type") {
    val env = AesSpec.empty[Row].withColor(Rgba.Black).env
    val color: Option[AesValue[Row, Rgba]] = env.get(Aesthetic.Color)
    assertEquals(color.flatMap(_.map(Row(0.0, 0.0, "A"))), Some(Rgba.Black))
  }

  test("bind registers a scaled binding once and rejects a duplicate") {
    val binding = ScaleBinding[Row, String, Rgba](Aesthetic.Color, _.condition, colorScale)
    val bound = AesEnv.empty[Row].bind(binding)
    assert(bound.isRight)
    val again = bound.flatMap(_.bind(binding))
    assertEquals(again, Left(GraphicsError.DuplicateScale("color")))
  }

  test("bind allows replacing an unscaled binding with a scaled one") {
    val env = AesSpec.empty[Row].withColor(Rgba.Black).env
    val binding = ScaleBinding[Row, String, Rgba](Aesthetic.Color, _.condition, colorScale)
    val bound = env.bind(binding)
    assert(bound.exists(_.get(Aesthetic.Color).exists(_.isScaled)))
  }

  test("inherit prefers scaled local, then scaled parent, then local, then parent") {
    val row = Row(1.0, 2.0, "A")
    val scaledColor = ScaleBinding[Row, String, Rgba](Aesthetic.Color, _.condition, colorScale).toAesValue
    val localDirect = AesSpec.empty[Row].withColor(Rgba.White).env
    val parentScaled = AesEnv.empty[Row].updated(Aesthetic.Color, scaledColor)

    val merged = localDirect.inherit(parentScaled)
    assert(merged.get(Aesthetic.Color).exists(_.isScaled), "scaled parent overrides direct local")

    val parentDirect = AesSpec.empty[Row].withColor(Rgba.Black).env
    val localWins = localDirect.inherit(parentDirect)
    assertEquals(localWins.get(Aesthetic.Color).flatMap(_.map(row)), Some(Rgba.White))

    val parentOnly = AesEnv.empty[Row].inherit(parentDirect)
    assertEquals(parentOnly.get(Aesthetic.Color).flatMap(_.map(row)), Some(Rgba.Black))
  }

  test("inherit through AesSpec matches env semantics") {
    val plotMapping = AesSpec.empty[Row].withPosition(_.x, _.y).withColor(Rgba.Black)
    val layerMapping = AesSpec.empty[Row].withColor(Rgba.White)
    val inherited = layerMapping.inherit(plotMapping)
    val row = Row(0.0, 0.0, "A")
    assertEquals(inherited.color.flatMap(_.map(row)), Some(Rgba.White))
    assert(inherited.x.nonEmpty && inherited.y.nonEmpty)
  }

  test("ScaleRegistry registers each scaled binding exactly once, in declaration order") {
    val env = AesEnv
      .empty[Row]
      .bind(ScaleBinding[Row, String, Rgba](Aesthetic.Color, _.condition, colorScale))
      .flatMap(_.bind(ScaleBinding[Row, Double, Double](Aesthetic.X, _.x, xScale)))
      .fold(e => fail(e.message), identity)
    val registry = ScaleRegistry.fromEnv(env)
    assertEquals(registry.entries.map(_.aesthetic), Vector[Aesthetic[?]](Aesthetic.X, Aesthetic.Color))
    val declarations = registry.declarations(3)
    assertEquals(declarations.map(_.aesthetic), Vector("x", "color"))
    assertEquals(declarations.map(_.layerIndex), Vector(3, 3))
    assertEquals(declarations.map(_.kind), Vector(ScaleKind.Continuous, ScaleKind.Discrete))
    val trained = registry.trained
    assertEquals(trained.map(_.descriptor.kind), Vector(ScaleKind.Continuous, ScaleKind.Discrete))
  }

  test("registry lookup by aesthetic finds the registered scale") {
    val env = AesEnv
      .empty[Row]
      .bind(ScaleBinding[Row, String, Rgba](Aesthetic.Color, _.condition, colorScale))
      .fold(e => fail(e.message), identity)
    val registry = ScaleRegistry.fromEnv(env)
    assert(registry.forAesthetic(Aesthetic.Color).nonEmpty)
    assert(registry.forAesthetic(Aesthetic.Fill).isEmpty)
  }
