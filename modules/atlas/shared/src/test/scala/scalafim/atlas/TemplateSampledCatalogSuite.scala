package scalafim.atlas

import scalafim.spatial.{DomainKind, SamplingGeometry}

class TemplateSampledCatalogSuite extends munit.FunSuite:
  test("all published template surface densities normalize as surface spaces while catalog domains remain unsampled"):
    val spaces = Vector(
      SpaceId.FsAverage5,
      SpaceId.FsAverage6,
      SpaceId.FsAverage,
      SpaceId.FsLR32k,
      SpaceId.FsLR59k,
      SpaceId.FsLR164k
    )
    assertEquals(spaces.map(SpaceId.kind), Vector.fill(6)(SpaceKindTag.Surface))
    spaces.foreach { space =>
      assertEquals(SpaceId.asSurface(SpaceId.normalize(space.value)), Right(space))
      TemplateCatalog.standard.domain(space) match
        case Right(domain) =>
          assertEquals(domain.kind, DomainKind.Surface)
          domain.geometry match
            case SamplingGeometry.Unsampled(DomainKind.Surface, _) => ()
            case other => fail(s"catalog domain for ${space.value} became executable: $other")
        case Left(error) => fail(error.message)
    }
