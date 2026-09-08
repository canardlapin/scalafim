package scalafim.surface.gifti

import scalafim.surface.Hemisphere

class GiftiHemisphereSuite extends munit.FunSuite:
  private def document(root: Option[String], point: Option[String]): GiftiDocument =
    def metadata(value: Option[String]): Map[String, String] = value.map("AnatomicalStructurePrimary" -> _).toMap
    val points = GiftiDataArray(GiftiIntent.PointSet, GiftiDataType.Float32, GiftiEncoding.Ascii,
      GiftiEndian.Little, GiftiArrayOrder.RowMajor, Vector(1, 3), metadata(point), Vector.empty, "0 0 0")
    GiftiDocument(Map.empty, metadata(root), Vector.empty, Vector(points))

  test("pointset and document cortical declarations resolve without a filename"):
    assertEquals(GiftiSurfaceCodec.inferredHemisphere(document(None, Some("CortexLeft")), Hemisphere.Unknown), Right(Hemisphere.Left))
    assertEquals(GiftiSurfaceCodec.inferredHemisphere(document(Some("CortexRight"), None), Hemisphere.Unknown), Right(Hemisphere.Right))
    assertEquals(GiftiSurfaceCodec.inferredHemisphere(document(Some("CortexLeft"), Some("CortexLeft")), Hemisphere.Left), Right(Hemisphere.Left))

  test("missing declarations preserve filename fallback or unknown"):
    assertEquals(GiftiSurfaceCodec.inferredHemisphere(document(None, None), Hemisphere.Right), Right(Hemisphere.Right))
    assertEquals(GiftiSurfaceCodec.inferredHemisphere(document(None, None), Hemisphere.Unknown), Right(Hemisphere.Unknown))

  test("contradictory declarations and filename hints are rejected"):
    assert(GiftiSurfaceCodec.inferredHemisphere(document(Some("CortexLeft"), Some("CortexRight")), Hemisphere.Unknown).isLeft)
    assert(GiftiSurfaceCodec.inferredHemisphere(document(None, Some("CortexLeft")), Hemisphere.Right).isLeft)

  test("noncortical and unrecognized anatomy is not relabeled from a filename"):
    assertEquals(GiftiSurfaceCodec.inferredHemisphere(document(Some("Cerebellum"), None), Hemisphere.Left), Right(Hemisphere.Unknown))
    assertEquals(GiftiSurfaceCodec.inferredHemisphere(document(Some("unrecognized"), None), Hemisphere.Left), Right(Hemisphere.Unknown))
