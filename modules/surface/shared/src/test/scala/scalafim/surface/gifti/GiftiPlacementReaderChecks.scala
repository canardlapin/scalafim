package scalafim.surface.gifti

import scalafim.surface.*
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/** Same public-reader placement contract executed through each platform's actual decoder. */
abstract class GiftiPlacementReaderChecks extends munit.FunSuite:
  import GiftiPlacementFixtures.Expect

  def decode(document: GiftiDocument, selection: GiftiTransformSelection): Future[Either[GiftiError, DeclaredGiftiSurface]]
  def convenience(document: GiftiDocument): Future[Either[GiftiError, SurfaceGeometry]]

  private def parse(transforms: Vector[GiftiTransform]): GiftiDocument =
    GiftiXmlParser.parseString(GiftiPlacementFixtures.xml(transforms)).fold(error => fail(error.message), identity)

  for fixture <- GiftiPlacementFixtures.cases do
    test(fixture.name):
      decode(parse(fixture.transforms), fixture.selection).map { result =>
        fixture.expected match
          case Expect.Refused => assert(result.isLeft, s"unexpected placement: $result")
          case expected =>
            val decoded = result.fold(error => fail(error.message), identity)
            val systems = fixture.transforms.map(GiftiCoordinateSystem.from)
            assertEquals(decoded.coordinates.coordinateSystems, systems)
            val point = decoded.geometry.mesh.vertex(VertexId(0))
            assertEqualsDouble(point.x, 1.0000000000000002, 0.0)
            assertEqualsDouble(point.y, 2.0, 0.0)
            assertEqualsDouble(point.z, 3.0, 0.0)
            val world = decoded.geometry.surfaceToWorld(Vector(point.x, point.y, point.z)).fold(e => fail(e.toString), identity)
            expected match
              case Expect.Native =>
                assertEquals(decoded.placement, GiftiPlacement.NativeCoordinates)
                assertEquals(world, Vector(point.x, point.y, point.z))
              case Expect.Placed(index, target) =>
                assertEquals(decoded.placement, GiftiPlacement.Transformed(index, systems(index), target))
                val original = fixture.transforms(index)
                if GiftiPlacementFixtures.isIdentity(original) then
                  assertEquals(world, Vector(point.x, point.y, point.z))
                else
                  assertEqualsDouble(world(0), original.matrixData(3) + point.x, 0.0)
                  assertEqualsDouble(world(1), 24.0, 0.0)
                  assertEqualsDouble(world(2), 39.0, 0.0)
              case Expect.Refused => fail("unreachable")
      }

  test("geometry-only convenience API refuses ambiguous and unknown non-identity placement"):
    val refused = Vector(
      Vector(GiftiPlacementFixtures.mni, GiftiPlacementFixtures.scanner),
      Vector(GiftiPlacementFixtures.unknown),
      Vector(GiftiPlacementFixtures.identityUnknown, GiftiPlacementFixtures.mni)
    )
    Future.sequence(refused.map(transforms => convenience(parse(transforms)).map(result => assert(result.isLeft, result.toString))))
      .map(_ => ())

  test("geometry-only convenience API keeps identity-only declarations native"):
    val pair = Vector(GiftiPlacementFixtures.blankIdentity, GiftiPlacementFixtures.talairachIdentity)
    convenience(parse(pair)).map { result =>
      val geometry = result.fold(error => fail(error.message), identity)
      assertEquals(geometry.surfaceToWorld(Vector(1.0, 2.0, 3.0)), Right(Vector(1.0, 2.0, 3.0)))
    }
