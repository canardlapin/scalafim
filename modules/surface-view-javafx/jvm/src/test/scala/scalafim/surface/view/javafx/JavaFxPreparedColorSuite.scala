package scalafim.surface.view.javafx

/** Native array behavior, including mutations by callers holding the public TriangleMesh. */
class JavaFxPreparedColorSuite extends munit.FunSuite:
  test("unchanged coordinates avoid uploads but an external same-size mutation is repaired"):
    val values = _root_.javafx.collections.FXCollections.observableFloatArray(0.0f, 0.25f, 0.5f, 1.0f)
    val coordinates = new JavaFxTextureCoordinates(values)
    val expected = Array(0.0f, 0.25f, 0.5f, 1.0f)
    assertEquals(coordinates.install(expected), 0L)
    values.set(2, 0.75f)
    assertEquals(coordinates.install(expected), 16L)
    assertEquals(values.toArray(Array.emptyFloatArray).toVector, expected.toVector)
    assertEquals(coordinates.install(expected.clone()), 0L)

  test("revision-bound worker comparison skips only an unchanged native payload"):
    val values = _root_.javafx.collections.FXCollections.observableFloatArray(0.0f, 0.25f, 0.5f, 1.0f)
    val coordinates = new JavaFxTextureCoordinates(values)
    val workerInput = Array(0.0f, 0.25f, 0.5f, 1.0f)
    val update = JavaFxPreparedCoordinateUpdate.owned(coordinates.capture(), workerInput)
    workerInput(1) = 0.75f
    assertEquals(coordinates.installPrepared(update.coordinates, update), 0L)
    assertEquals(values.toArray(Array.emptyFloatArray).toVector, Vector(0.0f, 0.25f, 0.5f, 1.0f))
    values.set(2, 0.75f)
    assertEquals(coordinates.installPrepared(update.coordinates, update), 16L)
    assertEquals(values.toArray(Array.emptyFloatArray).toVector, update.coordinates.toVector)

  test("captures are cached per revision and cannot authorize another native coordinate buffer"):
    val first = _root_.javafx.collections.FXCollections.observableFloatArray(0.0f, 0.25f)
    val firstCoordinates = new JavaFxTextureCoordinates(first)
    val snapshot = firstCoordinates.capture()
    assert(firstCoordinates.capture() eq snapshot)
    first.set(1, 0.5f)
    val changed = firstCoordinates.capture()
    assert(!(changed eq snapshot))
    assert(snapshot.samePrimitiveValues(Array(0.0f, 0.25f)))
    assert(changed.samePrimitiveValues(Array(0.0f, 0.5f)))

    val second = _root_.javafx.collections.FXCollections.observableFloatArray(0.75f, 1.0f)
    val secondCoordinates = new JavaFxTextureCoordinates(second)
    val update = JavaFxPreparedCoordinateUpdate.owned(snapshot, Array(0.0f, 0.25f))
    assertEquals(secondCoordinates.installPrepared(update.coordinates, update), 8L)
    assertEquals(second.toArray(Array.emptyFloatArray).toVector, update.coordinates.toVector)

  test("revision and exact payload guards fall back for resized or substituted coordinates"):
    val values = _root_.javafx.collections.FXCollections.observableFloatArray(0.0f, 0.25f)
    val coordinates = new JavaFxTextureCoordinates(values)
    val update = JavaFxPreparedCoordinateUpdate.owned(coordinates.capture(), Array(0.0f, 0.25f))
    values.setAll(Array(0.0f, 0.25f, 0.5f), 0, 3)
    assertEquals(coordinates.installPrepared(update.coordinates, update), 8L)
    val substitute = Array(0.5f, 0.75f)
    assertEquals(coordinates.installPrepared(substitute, update), 8L)
    assertEquals(values.toArray(Array.emptyFloatArray).toVector, substitute.toVector)

  test("growth shrink and empty arrays preserve native capacity and byte receipts"):
    val values = _root_.javafx.collections.FXCollections.observableFloatArray()
    val coordinates = new JavaFxTextureCoordinates(values)
    for expected <- Vector(Array(0.25f), Array(0.0f, 0.5f, 1.0f), Array(0.75f), Array.emptyFloatArray) do
      assertEquals(coordinates.install(expected), expected.length.toLong * 4L)
      assertEquals(values.toArray(Array.emptyFloatArray).toVector, expected.toVector)
      assertEquals(coordinates.install(expected), 0L)
    values.setAll(Array(1.0f, 0.5f), 0, 2)
    assertEquals(coordinates.install(Array(0.25f)), 4L)
    assertEquals(values.size(), 1)

  test("worker comparison preserves primitive equality including signed zero and nonfinite values"):
    val values = _root_.javafx.collections.FXCollections.observableFloatArray(-0.0f, Float.PositiveInfinity)
    val coordinates = new JavaFxTextureCoordinates(values)
    val signedZero = JavaFxPreparedCoordinateUpdate.owned(coordinates.capture(), Array(0.0f, Float.PositiveInfinity))
    assertEquals(coordinates.installPrepared(signedZero.coordinates, signedZero), 0L)
    assertEquals(java.lang.Float.floatToRawIntBits(values.get(0)), java.lang.Float.floatToRawIntBits(-0.0f))
    val nan = JavaFxPreparedCoordinateUpdate.owned(coordinates.capture(), Array(Float.NaN))
    assertEquals(coordinates.installPrepared(nan.coordinates, nan), 4L)
    assertEquals(coordinates.installPrepared(nan.coordinates, nan), 4L)
