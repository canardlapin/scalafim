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

  test("payload vectors and matrices expose typed shape semantics"):
    val vectorArray =
      GiftiDataArray(
        intent = GiftiIntent.Label,
        dataType = GiftiDataType.Int32,
        encoding = GiftiEncoding.Ascii,
        endian = GiftiEndian.Little,
        arrayOrder = GiftiArrayOrder.RowMajor,
        dims = Vector(3),
        metadata = Map.empty,
        transforms = Vector.empty,
        dataText = "1 2 3"
      )
    val vector = GiftiPayload.from(vectorArray, Vector(1, 2, 3)).toOption.get

    assertEquals(GiftiPayload.requireVector(vector).toOption.get(1), 2)

  test("payload matrices make row-major and column-major order explicit"):
    val rowMajor =
      GiftiDataArray(
        intent = GiftiIntent.PointSet,
        dataType = GiftiDataType.Float32,
        encoding = GiftiEncoding.Ascii,
        endian = GiftiEndian.Little,
        arrayOrder = GiftiArrayOrder.RowMajor,
        dims = Vector(2, 3),
        metadata = Map.empty,
        transforms = Vector.empty,
        dataText = "1 2 3 4 5 6"
      )
    val columnMajor = rowMajor.copy(arrayOrder = GiftiArrayOrder.ColumnMajor)

    val rowMatrix = GiftiPayload.matrix(rowMajor, Vector(1, 2, 3, 4, 5, 6)).toOption.get
    val columnMatrix = GiftiPayload.matrix(columnMajor, Vector(1, 4, 2, 5, 3, 6)).toOption.get

    assertEquals(rowMatrix.row(1), Vector(4, 5, 6))
    assertEquals(columnMatrix.row(1), Vector(4, 5, 6))
    assertEquals(columnMatrix.rowMajorValues, Vector(1, 2, 3, 4, 5, 6))

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
