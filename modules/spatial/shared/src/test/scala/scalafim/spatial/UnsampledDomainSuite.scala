package scalafim.spatial

import image4s.geometry.{Affine, D3, Frame}
import reframe4s.lie.FramedAffine
import scalafim.image.SpatialPoint
import scalafim.image.world.{FrameCatalog, TemplateName, WorldSpace}
import scalafim.transform.{TransformProvenance, WorldTransform}

class UnsampledDomainSuite extends munit.FunSuite:

  private def value[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def frame(name: String): Frame[D3] =
    FrameCatalog.frame(WorldSpace.Template(TemplateName.unsafe(name)))

  private def template(name: String, kind: DomainKind, world: Frame[D3]): Domain =
    val templateKind = if kind == DomainKind.Surface then TemplateKind.Surface else TemplateKind.Volume
    value(
      Domain.build(
        value(DomainId(name)),
        SpaceRef.Template(TemplateName.unsafe(name), None, templateKind),
        value(SamplingGeometry.unsampled(kind, world))
      )
    )

  private def translation(x: Double, y: Double, z: Double): Affine[D3] =
    ProviderAffines.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, x),
        Vector(0.0, 1.0, 0.0, y),
        Vector(0.0, 0.0, 1.0, z),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private val aFrame = frame("unsampled-a")
  private val bFrame = frame("unsampled-b")
  private val cFrame = frame("unsampled-c")
  private val a = template("unsampled-a", DomainKind.Volume, aFrame)
  private val b = template("unsampled-b", DomainKind.Volume, bFrame)
  private val c = template("unsampled-c", DomainKind.Volume, cFrame)

  test("unsampled world domains have no elements and only volume or surface kinds"):
    assertEquals(a.nElements, 0)
    assertEquals(a.kind, DomainKind.Volume)
    assert(SamplingGeometry.unsampled(DomainKind.Latent, aFrame).isLeft)
    assert(SamplingGeometry.unsampled(DomainKind.Hybrid, aFrame).isLeft)

  test("provider maps join unsampled domains and carry points both ways along a route"):
    // pullbacks: b -> a subtracts (1, 0, 0); c -> b subtracts (0, 2, 0); so the forward route a -> c adds (1, 2, 0)
    val ab = value(Morphism.between(value(MorphismId("a-b")), a, b, MorphismKind.Affine3D, RouteTag.Anatomical, inverse = Inverse.Exact("affine"), coordinateMap = value(CoordinateMap.affine(a, b, translation(-1.0, 0.0, 0.0)))))
    val bc = value(Morphism.between(value(MorphismId("b-c")), b, c, MorphismKind.Affine3D, RouteTag.Anatomical, inverse = Inverse.Exact("affine"), coordinateMap = value(CoordinateMap.affine(b, c, translation(0.0, -2.0, 0.0)))))
    val graph = value(SpatialGraph.build(Vector(a, b, c), Vector(ab, bc)))
    val route = value(graph.path(a.id, c.id))

    assertEquals(value(route.push(SpatialPoint.Origin)), SpatialPoint(1.0, 2.0, 0.0))
    assertEquals(value(route.pullback(SpatialPoint(1.0, 2.0, 0.0))), SpatialPoint.Origin)

    val reverse = value(graph.path(c.id, a.id, allowInverses = true))
    assertEquals(value(reverse.push(SpatialPoint(1.0, 2.0, 0.0))), SpatialPoint.Origin)

  test("maps on foreign frames and operators onto unsampled domains are rejected"):
    val foreign = template("unsampled-foreign", DomainKind.Volume, frame("unsampled-foreign"))
    val sameIdentityOtherOwner = template("unsampled-a-copy", DomainKind.Volume, frame("unsampled-a"))
    val map = value(CoordinateMap.affine(sameIdentityOtherOwner, foreign, translation(0.0, 0.0, 0.0)))
    val misplaced = value(Morphism.build(value(MorphismId("misplaced")), a.id, foreign.id, MorphismKind.Affine3D, RouteTag.Anatomical, coordinateMap = map))
    assert(SpatialGraph.build(Vector(a, foreign), Vector(misplaced)).isLeft)

    val identityGraph = value(SpatialGraph.build(Vector(a)))
    assert(OperatorCompiler.compile(identityGraph, CompileRequest(a.id, a.id)).isLeft)

  test("routes through steps without provider maps cannot carry points"):
    val planned = value(Morphism.between(value(MorphismId("planned")), a, b, MorphismKind.Warp3D, RouteTag.Anatomical))
    val graph = value(SpatialGraph.build(Vector(a, b), Vector(planned)))
    val route = value(graph.path(a.id, b.id))
    assertEquals(route.push(SpatialPoint.Origin).left.toOption, Some(SpatialError.MissingCoordinateMap(planned.id)))
    assertEquals(route.pullback(SpatialPoint.Origin).left.toOption, Some(SpatialError.MissingCoordinateMap(planned.id)))

  test("surface-to-volume morphisms join surface sources to volume targets only"):
    val surface = template("unsampled-surface", DomainKind.Surface, frame("unsampled-surface"))
    val fill = value(Morphism.between(value(MorphismId("ribbon-fill")), surface, a, MorphismKind.SurfaceToVolume, RouteTag.Anatomical))
    assertEquals(fill.kind, MorphismKind.SurfaceToVolume)
    assert(Morphism.between(value(MorphismId("wrong-way")), a, surface, MorphismKind.SurfaceToVolume, RouteTag.Anatomical).isLeft)

  test("world transforms become provider maps with their exact affine inverse"):
    // WorldTransform a -> b: pullback b -> a subtracts (3, 0, 0)
    val pullback = FramedAffine.betweenFrames[Frame[D3], Frame[D3], D3](bFrame, aFrame)(translation(-3.0, 0.0, 0.0))
    val transform = WorldTransform.Linear(pullback, TransformProvenance.constructed("test"))
    val map = value(CoordinateMap.fromWorldTransform(transform, "test"))
    val morphism = value(Morphism.between(value(MorphismId("world-a-b")), a, b, MorphismKind.Affine3D, RouteTag.Anatomical, inverse = Inverse.Exact("affine"), coordinateMap = map))
    val route = value(value(SpatialGraph.build(Vector(a, b), Vector(morphism))).path(a.id, b.id))
    assertEquals(value(route.push(SpatialPoint.Origin)), SpatialPoint(3.0, 0.0, 0.0))
    assertEquals(value(map.transform(SpatialPoint(3.0, 0.0, 0.0))), SpatialPoint.Origin)
