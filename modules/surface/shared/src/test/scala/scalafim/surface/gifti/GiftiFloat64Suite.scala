package scalafim.surface.gifti

class GiftiFloat64Suite extends munit.FunSuite:
  private def array(data: String = "1 2 3") =
    GiftiXmlParser.parseString(GiftiFloat64Fixtures.xml("ASCII", "LittleEndian", data))
      .fold(error => fail(error.message), identity).dataArrays.head

  test("Float64 width checks reject overflow before allocation"):
    assertEquals(GiftiPayloadDecoder.expectedByteCount(array()), Right(24))
    assert(GiftiPayloadDecoder.expectedByteCount(array().copy(dims = Vector(Int.MaxValue))).isLeft)
    assert(GiftiPayloadDecoder.expectedByteCount(array().copy(dims = Vector(65536, 65536))).isLeft)

  test("Float64 binary decoding rejects short, overlong and misaligned payloads"):
    for n <- Vector(0, 16, 23, 25, 32) do
      val result = GiftiPayloadDecoder.doublePayload(array(), GiftiPayloadDecoder.EncodedData.Binary(new Array[Byte](n)))
      assert(result.isLeft, s"accepted $n bytes")

  test("Float64 ASCII rejects malformed data and integer/byte coercion even for integral values"):
    val ascii = GiftiPayloadDecoder.EncodedData.Ascii
    for text <- Vector("1 nope 3", "1 2", "1 2 3 4") do
      assert(GiftiPayloadDecoder.doublePayload(array(text), ascii).isLeft)
    assertEquals(GiftiPayloadDecoder.intPayload(array(), ascii), Left(GiftiError.UnsupportedDataType(GiftiDataType.Float64)))
    assertEquals(GiftiPayloadDecoder.bytePayload(array(), ascii), Left(GiftiError.UnsupportedDataType(GiftiDataType.Float64)))
