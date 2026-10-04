package scalafim.surface.gifti

import scalafim.surface.*
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/** Same public-reader contract executed through each platform's actual decoder. */
abstract class GiftiPlacementReaderChecks extends munit.FunSuite:
  def decode(document: GiftiDocument, selection: GiftiTransformSelection): Future[Either[GiftiError, GiftiDecodedSurface]]
  def convenience(document: GiftiDocument): Future[Either[GiftiError, SurfaceGeometry]]

  for fixture <- GiftiPlacementFixtures.cases do
    test(fixture.name):
      val document = GiftiXmlParser.parseString(GiftiPlacementFixtures.xml(fixture.transforms)).toOption.get
      decode(document, fixture.selection).map { result =>
        fixture.expected match
          case None => assert(result.isLeft, s"unexpected placement: $result")
          case Some(expected) =>
            val decoded = result.fold(error => fail(error.message), identity)
            assertEquals(decoded.placement, expected)
            assertEquals(decoded.sourceTransforms, fixture.transforms)
            val point = decoded.geometry.mesh.vertex(VertexId(0))
            assertEqualsDouble(point.x, 1.0000000000000002, 0.0)
            assertEqualsDouble(point.y, 2.0, 0.0)
            assertEqualsDouble(point.z, 3.0, 0.0)
            val world = decoded.geometry.surfaceToWorld(Vector(point.x, point.y, point.z)).toOption.get
            expected match
              case GiftiPlacement.NativeCoordinates =>
                assertEquals(world, Vector(point.x, point.y, point.z))
              case GiftiPlacement.Transformed(original, _) =>
                assertEqualsDouble(world(0), original.matrixData(3) + point.x, 0.0)
                assertEqualsDouble(world(1), 24.0, 0.0)
                assertEqualsDouble(world(2), 39.0, 0.0)
      }

  test("geometry-only convenience API refuses ambiguous and unknown placement"):
    val cases = Vector(Vector(GiftiPlacementFixtures.mni, GiftiPlacementFixtures.scanner), Vector(GiftiPlacementFixtures.unknown))
    Future.sequence(cases.map { transforms =>
      val document = GiftiXmlParser.parseString(GiftiPlacementFixtures.xml(transforms)).toOption.get
      convenience(document).map(result => assert(result.isLeft))
    }).map(_ => ())
