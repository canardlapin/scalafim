package scalafim.surface.view.javafx

/** Native array behavior, including mutations by callers holding the public TriangleMesh. */
class JavaFxPreparedColorSuite extends munit.FunSuite:
  test("unchanged coordinates avoid uploads but an external same-size mutation is repaired"):
    val values = javafx.collections.FXCollections.observableFloatArray(0.0f, 0.25f, 0.5f, 1.0f)
    val coordinates = new JavaFxTextureCoordinates(values)
    val expected = Array(0.0f, 0.25f, 0.5f, 1.0f)
    assertEquals(coordinates.install(expected), 0L)
    values.set(2, 0.75f)
    assertEquals(coordinates.install(expected), 16L)
    assertEquals(values.toArray(Array.emptyFloatArray).toVector, expected.toVector)
    assertEquals(coordinates.install(expected.clone()), 0L)

  test("growth shrink and empty arrays preserve native capacity and byte receipts"):
    val values = javafx.collections.FXCollections.observableFloatArray()
    val coordinates = new JavaFxTextureCoordinates(values)
    for expected <- Vector(Array(0.25f), Array(0.0f, 0.5f, 1.0f), Array(0.75f), Array.emptyFloatArray) do
      assertEquals(coordinates.install(expected), expected.length.toLong * 4L)
      assertEquals(values.toArray(Array.emptyFloatArray).toVector, expected.toVector)
      assertEquals(coordinates.install(expected), 0L)
    values.setAll(Array(1.0f, 0.5f), 0, 2)
    assertEquals(coordinates.install(Array(0.25f)), 4L)
    assertEquals(values.size(), 1)

  test("bulk comparison preserves primitive equality including signed zero and nonfinite values"):
    val values = javafx.collections.FXCollections.observableFloatArray(-0.0f, Float.PositiveInfinity)
    val coordinates = new JavaFxTextureCoordinates(values)
    assertEquals(coordinates.install(Array(0.0f, Float.PositiveInfinity)), 0L)
    assertEquals(java.lang.Float.floatToRawIntBits(values.get(0)), java.lang.Float.floatToRawIntBits(-0.0f))
    assertEquals(coordinates.install(Array(Float.NaN)), 4L)
    assertEquals(coordinates.install(Array(Float.NaN)), 4L)
