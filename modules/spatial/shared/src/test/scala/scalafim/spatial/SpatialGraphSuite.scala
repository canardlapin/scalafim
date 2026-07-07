package scalafim.spatial

import scalafim.image.{DMat, NeuroSpace}
import scalafim.surface.{Hemisphere, SurfaceGeometry, SurfaceKind, TriangleMesh}

class SpatialGraphSuite extends munit.FunSuite:

  private def value[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def domain(name: String, voxels: Int = 1): Domain =
    val id = value(DomainId(name))
    val subject = value(SubjectId("sub-01"))
    val modality = value(Modality(name))
    val geometry = value(SamplingGeometry.volume(NeuroSpace(Vector(voxels, 1, 1), trans = Some(DMat.eye(4)))))
    value(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def surfaceDomain(name: String): Domain =
    val id = value(DomainId(name))
    val subject = value(SubjectId("sub-01"))
    val mesh =
      TriangleMesh.fromRows(
        vertices = Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0)
        ),
        faces = Vector((0, 1, 2))
      )
    val surface = SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.Midthickness)
    val geometry = value(SamplingGeometry.surface(surface))
    value(Domain.build(id, SpaceRef.Surface(subject, Hemisphere.Left, SurfaceKind.Midthickness), geometry))

  private def morphism(
    idValue: String,
    source: Domain,
    target: Domain,
    cost: Double,
    tag: RouteTag = RouteTag.Anatomical,
    inverse: Inverse = Inverse.None,
    kind: MorphismKind = MorphismKind.Affine3D
  ): Morphism =
    value(
      Morphism.build(
        id = value(MorphismId(idValue)),
        source = source.id,
        target = target.id,
        kind = kind,
        routeTag = tag,
        cost = cost,
        inverse = inverse
      )
    )

  test("graph rejects duplicate domains and morphisms"):
    val epi = domain("epi")
    val t1 = domain("t1")
    val epiToT1 = morphism("epi-to-t1", epi, t1, 1.0)

    assertEquals(SpatialGraph.build(Vector(epi, epi)).left.toOption, Some(SpatialError.DuplicateDomain(epi.id)))

    val graph = value(SpatialGraph.build(Vector(epi, t1), Vector(epiToT1)))
    assertEquals(graph.add(epiToT1).left.toOption, Some(SpatialError.DuplicateMorphism(epiToT1.id)))

  test("shortest routing chooses the lowest cost path"):
    val epi = domain("epi")
    val t1 = domain("t1")
    val mni = domain("mni")
    val direct = morphism("epi-to-mni-direct", epi, mni, 5.0)
    val first = morphism("epi-to-t1", epi, t1, 1.0)
    val second = morphism("t1-to-mni", t1, mni, 1.0)
    val graph = value(SpatialGraph.build(Vector(epi, t1, mni), Vector(direct, first, second)))

    val path = value(graph.path(epi.id, mni.id))
    assertEquals(path.ids.map(_.value), Vector("epi-to-t1", "t1-to-mni"))
    assertEqualsDouble(path.cost, 2.0, 1e-12)

  test("route policies filter anatomical and functional edges"):
    val epi = domain("epi")
    val t1 = domain("t1")
    val latent = domain("latent")
    val anatomical = morphism("epi-to-t1", epi, t1, 1.0, RouteTag.Anatomical)
    val functional = morphism("epi-to-latent", epi, latent, 1.0, RouteTag.Functional, kind = MorphismKind.Functional)
    val graph = value(SpatialGraph.build(Vector(epi, t1, latent), Vector(anatomical, functional)))

    val anatomicalPath = value(graph.path(epi.id, t1.id, RoutingPolicy.Anatomical))
    assertEquals(anatomicalPath.ids.map(_.value), Vector("epi-to-t1"))

    val functionalPath = value(graph.path(epi.id, latent.id, RoutingPolicy.Functional))
    assertEquals(functionalPath.ids.map(_.value), Vector("epi-to-latent"))

    assertEquals(graph.path(epi.id, latent.id, RoutingPolicy.Anatomical).left.toOption, Some(SpatialError.NoPath(epi.id, latent.id)))

  test("inverse routing uses only geometric inverses"):
    val epi = domain("epi")
    val t1 = domain("t1")
    val surface = surfaceDomain("surface")
    val affine = morphism("epi-to-t1", epi, t1, 1.0, inverse = Inverse.Exact("analytic"))
    val volToSurf =
      morphism(
        "t1-to-surface",
        t1,
        surface,
        2.0,
        inverse = Inverse.AdjointOnly,
        kind = MorphismKind.VolumeToSurface
      )
    val graph = value(SpatialGraph.build(Vector(epi, t1, surface), Vector(affine, volToSurf)))

    assertEquals(graph.path(t1.id, epi.id).left.toOption, Some(SpatialError.NoPath(t1.id, epi.id)))

    val reverse = value(graph.path(t1.id, epi.id, allowInverses = true))
    assertEquals(reverse.ids.map(_.value), Vector("epi-to-t1:inverse"))
    assert(reverse.usedInverses)

    val noGeometricReverse = graph.path(surface.id, t1.id, allowInverses = true)
    assertEquals(noGeometricReverse.left.toOption, Some(SpatialError.NoPath(surface.id, t1.id)))

  test("graph rejects morphisms whose kind is incompatible with domain kinds"):
    val source = domain("source")
    val target = domain("target")
    val bad =
      morphism(
        "bad-volume-to-surface",
        source,
        target,
        1.0,
        inverse = Inverse.AdjointOnly,
        kind = MorphismKind.VolumeToSurface
      )

    val result = SpatialGraph.build(Vector(source, target), Vector(bad))

    assertEquals(
      result.left.toOption,
      Some(SpatialError.IncompatibleMorphismKind(MorphismKind.VolumeToSurface, source.id, DomainKind.Volume, target.id, DomainKind.Volume))
    )

  test("provided inverse quality increases reverse edge cost"):
    val source = domain("source")
    val target = domain("target")
    val warp = morphism("source-to-target", source, target, 2.0, inverse = Inverse.Provided("file", 0.75))
    val graph = value(SpatialGraph.build(Vector(source, target), Vector(warp)))

    val reverse = value(graph.path(target.id, source.id, allowInverses = true))
    assertEqualsDouble(reverse.cost, 2.25, 1e-12)
    assertEqualsDouble(reverse.pathQuality, 0.75, 1e-12)
