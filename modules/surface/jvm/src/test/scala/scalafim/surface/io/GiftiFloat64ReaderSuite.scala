package scalafim.surface.io

import scalafim.surface.gifti.*

class GiftiFloat64ReaderSuite extends munit.FunSuite:
  for ((encoding, endian, data), index) <- GiftiFloat64Fixtures.cases.zipWithIndex do
    test(s"Float64 public reader preserves precision: $encoding $endian $index"):
      val document = GiftiReader.parseString(GiftiFloat64Fixtures.xml(encoding, endian, data))
        .fold(error => fail(error.message), identity)
      val array = document.dataArrays.head
      val payload = GiftiReader.doublePayload(array).fold(error => fail(error.message), identity)
      payload.values.zip(GiftiFloat64Fixtures.expected).foreach((actual, expected) =>
        assertEqualsDouble(actual, expected, 0.0))
      assertEquals(GiftiReader.intPayload(array), Left(GiftiError.UnsupportedDataType(GiftiDataType.Float64)))
