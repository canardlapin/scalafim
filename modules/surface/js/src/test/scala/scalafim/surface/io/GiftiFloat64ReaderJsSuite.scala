package scalafim.surface.io

import scalafim.surface.gifti.*
import scala.concurrent.ExecutionContext.Implicits.global

class GiftiFloat64ReaderJsSuite extends munit.FunSuite:
  for ((encoding, endian, data), index) <- GiftiFloat64Fixtures.cases.zipWithIndex do
    test(s"Float64 public reader preserves precision: $encoding $endian $index"):
      val document = GiftiReader.parseString(GiftiFloat64Fixtures.xml(encoding, endian, data))
        .fold(error => fail(error.message), identity)
      val array = document.dataArrays.head
      GiftiReader.doublePayload(array).flatMap { result =>
        val payload = result.fold(error => fail(error.message), identity)
        payload.values.zip(GiftiFloat64Fixtures.expected).foreach((actual, expected) =>
          assertEqualsDouble(actual, expected, 0.0))
        GiftiReader.intPayload(array).map(result =>
          assertEquals(result, Left(GiftiError.UnsupportedDataType(GiftiDataType.Float64))))
      }
