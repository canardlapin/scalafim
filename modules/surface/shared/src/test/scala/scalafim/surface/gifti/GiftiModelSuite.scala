package scalafim.surface.gifti

class GiftiModelSuite extends munit.FunSuite:

  test("typed attributes normalize known GIFTI codes"):
    assertEquals(GiftiIntent.fromAttribute("NIFTI_INTENT_POINTSET"), GiftiIntent.PointSet)
    assertEquals(GiftiIntent.fromAttribute("NIFTI_INTENT_TRIANGLE"), GiftiIntent.Triangle)
    assertEquals(GiftiEncoding.fromAttribute(Some("GZipBase64Binary")), GiftiEncoding.GZipBase64Binary)
    assertEquals(GiftiEncoding.fromAttribute(None), GiftiEncoding.Ascii)
    assertEquals(GiftiDataType.fromAttribute("NIFTI_TYPE_FLOAT32"), GiftiDataType.Float32)
    assertEquals(GiftiEndian.fromAttribute(Some("BigEndian")), GiftiEndian.Big)
    assertEquals(GiftiArrayOrder.fromAttribute(None), GiftiArrayOrder.RowMajor)

  test("unknown attributes preserve their source text"):
    assertEquals(GiftiIntent.fromAttribute("NIFTI_INTENT_MY_CUSTOM"), GiftiIntent.Other("NIFTI_INTENT_MY_CUSTOM"))
    assertEquals(GiftiEncoding.fromAttribute(Some("CustomEncoding")), GiftiEncoding.Other("CustomEncoding"))
    assertEquals(GiftiDataType.fromAttribute("NIFTI_TYPE_COMPLEX64"), GiftiDataType.Other("NIFTI_TYPE_COMPLEX64"))
    assertEquals(GiftiEndian.fromAttribute(Some("MiddleEndian")), GiftiEndian.Other("MiddleEndian"))
    assertEquals(GiftiArrayOrder.fromAttribute(Some("CustomOrder")), GiftiArrayOrder.Other("CustomOrder"))

  test("data arrays expose shape and intent contracts"):
    val array =
      GiftiDataArray(
        intent = GiftiIntent.PointSet,
        dataType = GiftiDataType.Float32,
        encoding = GiftiEncoding.Ascii,
        endian = GiftiEndian.Little,
        arrayOrder = GiftiArrayOrder.RowMajor,
        dims = Vector(4, 3),
        metadata = Map.empty,
        transforms = Vector.empty,
        dataText = "0 0 0"
      )

    assertEquals(array.rank, 2)
    assertEquals(array.rows, 4)
    assertEquals(array.columns, 3)
    assertEquals(array.elementCount, 12)
    assert(array.isPointSet)
    assert(array.isTriple)

  test("label color conversion keeps GIFTI 0-1 color values explicit"):
    val label = GiftiLabel(2, "Positive", red = Some(1.0), green = Some(0.5), blue = Some(0.0), alpha = Some(1.0))
    assertEquals(label.colorHex, Some("#ff8000"))

  test("documents provide typed array lookup and reject duplicate labels"):
    val pointset =
      GiftiDataArray(
        intent = GiftiIntent.PointSet,
        dataType = GiftiDataType.Float32,
        encoding = GiftiEncoding.Ascii,
        endian = GiftiEndian.Little,
        arrayOrder = GiftiArrayOrder.RowMajor,
        dims = Vector(1, 3),
        metadata = Map.empty,
        transforms = Vector.empty,
        dataText = "0 0 0"
      )
    val doc = GiftiDocument(Map("Version" -> "1.0"), Map.empty, Vector(GiftiLabel(1, "A")), Vector(pointset))

    assertEquals(doc.pointSet, Some(pointset))
    assertEquals(doc.triangles, None)

    interceptMessage[IllegalArgumentException]("requirement failed: GIFTI label keys must be unique"):
      GiftiDocument(Map.empty, Map.empty, Vector(GiftiLabel(1, "A"), GiftiLabel(1, "B")), Vector(pointset))
