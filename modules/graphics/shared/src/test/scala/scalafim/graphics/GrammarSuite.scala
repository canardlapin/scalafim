package scalafim.graphics

class GrammarSuite extends munit.FunSuite:

  private final case class Observation(time: Double, value: Double, condition: String)

  private val data =
    Vector(
      Observation(0.0, 1.0, "A"),
      Observation(1.0, 2.0, "A"),
      Observation(2.0, 3.0, "B")
    )

  test("typed layers carry required position aesthetics by construction") {
    val layer = Layer.point[Observation](_.time, _.value)
    val mapping = layer.effectiveMapping(AesSpec.empty)

    assertEquals(layer.geom, Geom.Point)
    assert(mapping.position.nonEmpty)
    assertEquals(mapping.position.get(data.head), (0.0, 1.0))
  }

  test("plot validates required aesthetics after layer mapping inheritance") {
    val plotMapping =
      AesSpec.empty[Observation].withPosition(_.time, _.value)

    val inheritedPoint =
      Layer
        .fromMapping[Observation](Geom.Point, AesSpec.empty[Observation])
        .toOption
        .get

    val missingPoint =
      Plot(data).addLayer(inheritedPoint)

    val inheritedPlot =
      Plot(data)
        .withMapping(plotMapping)
        .addLayer(inheritedPoint)

    val missingTextLabel =
      Layer.fromMapping[Observation](
        Geom.Text,
        AesSpec.empty[Observation].withPosition(_.time, _.value)
      ).flatMap(layer => Plot(data).addLayer(layer))

    assertEquals(missingPoint.left.toOption, Some(GraphicsError.MissingAesthetic("point", "x")))
    assert(inheritedPlot.isRight)
    assertEquals(missingTextLabel.left.toOption, Some(GraphicsError.MissingAesthetic("text", "label")))
  }

  test("plot layers inherit plot data and mappings without mutating either") {
    val plotMapping =
      AesSpec.empty[Observation]
        .withPosition(_.time, _.value)
        .withGroup(_.condition)

    val layer =
      Layer.fromMapping[Observation](
        Geom.Line,
        AesSpec.empty[Observation].withPosition(_.time, _.value)
      ).toOption.get

    val plot =
      Plot(data)
        .withMapping(plotMapping)
        .addLayer(layer)
        .toOption
        .get

    assertEquals(plot.layerData(layer), data)
    assertEquals(plot.layerMapping(layer).group.get(data.last), "B")
    assertEquals(plot.layers, Vector(layer))
  }

  test("plot rejects duplicate scale bindings for the same aesthetic") {
    val scale =
      ContinuousScale
        .train("x", data.map(_.time), Palette.numeric)
        .toOption
        .get

    val binding = ScaleBinding[Observation, Double, Double](Aesthetic.X, _.time, scale)
    val plot = Plot(data).withScale(binding).toOption.get

    assertEquals(plot.withScale(binding).left.toOption, Some(GraphicsError.DuplicateScale("x")))
  }

  test("scale bindings carry the row extractor used to map an aesthetic") {
    val domain = DiscreteDomain.ordered(Vector("A", "B")).toOption.get
    val palette = DiscretePalette.valuesUnsafe(Vector(Rgba.Black, Rgba.White))
    val scale = DiscreteScale("condition", domain, palette).toOption.get
    val binding = ScaleBinding[Observation, String, Rgba](Aesthetic.Color, _.condition, scale)

    assertEquals(binding.map(data.head), Some(Rgba.Black))
    assertEquals(binding.map(data.last), Some(Rgba.White))
  }
