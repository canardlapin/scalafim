package scalafim.spatial

import scalafim.image.DMat
import scalafim.image.NeuroSpace

class GraphDifferentialSuite extends munit.FunSuite:
  test("graph-delegated routing matches an independent exhaustive path oracle"):
    val source = domain("source")
    val intermediate = domain("intermediate")
    val target = domain("target")
    val morphisms = Vector(
      morphism("direct", source, target, 7.0),
      morphism("first", source, intermediate, 1.25),
      morphism("second", intermediate, target, 2.5)
    )
    val spatial = value(SpatialGraph.build(Vector(source, intermediate, target), morphisms))

    assertEquivalent(spatial, source.id, target.id, RoutingPolicy.Shortest, allowInverses = false)

  test("generic route adapters preserve policy filtering and geometric inverses"):
    val source = domain("source")
    val anatomical = domain("anatomical")
    val functional = domain("functional")
    val morphisms = Vector(
      morphism("anatomical-route", source, anatomical, 1.0, RouteTag.Anatomical, Inverse.Exact("analytic")),
      morphism("functional-route", source, functional, 0.5, RouteTag.Functional, Inverse.None)
    )
    val spatial = value(SpatialGraph.build(Vector(source, anatomical, functional), morphisms))

    assertEquivalent(spatial, source.id, anatomical.id, RoutingPolicy.Anatomical, allowInverses = false)
    assertEquivalent(spatial, source.id, functional.id, RoutingPolicy.Functional, allowInverses = false)
    assertEquivalent(spatial, anatomical.id, source.id, RoutingPolicy.Anatomical, allowInverses = true)
    assert(spatial.path(functional.id, source.id, RoutingPolicy.Functional, allowInverses = true).isLeft)
    assertEquals(oraclePath(spatial, functional.id, source.id, RoutingPolicy.Functional, allowInverses = true), None)

  test("parallel spatial morphisms collapse explicitly to the cheapest eligible simple edge"):
    val source = domain("source")
    val target = domain("target")
    val expensive = morphism("expensive", source, target, 4.0)
    val cheap = morphism("cheap", source, target, 1.0)
    val spatial = value(SpatialGraph.build(Vector(source, target), Vector(expensive, cheap)))

    val domainPath = value(spatial.path(source.id, target.id))
    val oracle = oraclePath(spatial, source.id, target.id, RoutingPolicy.Shortest, allowInverses = false).get

    assertEquals(domainPath.ids.map(_.value), Vector("cheap"))
    assertEquals(oracle.map(_.id.value), Vector("cheap"))
    assertEqualsDouble(oracle.map(_.cost).sum, domainPath.cost, 1e-12)

    val firstEqual = morphism("first-equal", source, target, 1.0)
    val secondEqual = morphism("second-equal", source, target, 1.0)
    val equalCost = value(SpatialGraph.build(Vector(source, target), Vector(firstEqual, secondEqual)))
    assertEquals(value(equalCost.path(source.id, target.id)).ids.map(_.value), Vector("first-equal"))

  test("equal-cost route ties are deterministic by ordered domain basis"):
    val source = domain("source")
    val b = domain("b")
    val c = domain("c")
    val target = domain("target")
    val spatial = value(
      SpatialGraph.build(
        Vector(source, c, b, target),
        Vector(
          morphism("source-c", source, c, 1.0),
          morphism("c-target", c, target, 1.0),
          morphism("source-b", source, b, 1.0),
          morphism("b-target", b, target, 1.0)
        )
      )
    )

    val first = value(spatial.path(source.id, target.id))
    val second = value(spatial.path(source.id, target.id))

    assertEquals(first.ids.map(_.value), Vector("source-b", "b-target"))
    assertEquals(second.ids, first.ids)

  test("spatial identity paths remain a domain wrapper responsibility"):
    val source = domain("source")
    val spatial = value(SpatialGraph.build(Vector(source)))
    val domainPath = value(spatial.path(source.id, source.id))

    assertEquals(domainPath.ids.map(_.value), Vector("identity:source"))
    assertEqualsDouble(domainPath.cost, 0.0, 1e-12)

  private def assertEquivalent(
      spatial: SpatialGraph,
      source: DomainId,
      target: DomainId,
      policy: RoutingPolicy,
      allowInverses: Boolean
  ): Unit =
    val domainPath = value(spatial.path(source, target, policy, allowInverses))
    val oracle = oraclePath(spatial, source, target, policy, allowInverses).get

    assertEquals(domainPath.ids, oracle.map(_.id))
    assertEqualsDouble(domainPath.cost, oracle.map(_.cost).sum, 1e-12)
    assertEquals(domainPath.usedInverses, oracle.exists(_.isInverted))

  private def oraclePath(
      spatial: SpatialGraph,
      source: DomainId,
      target: DomainId,
      policy: RoutingPolicy,
      allowInverses: Boolean
  ): Option[Vector[Morphism]] =
    val forward = spatial.morphisms.filter(allowed(_, policy))
    val inverse =
      if !allowInverses then Vector.empty
      else
        forward.flatMap: morphism =>
          if !morphism.inverse.isGeometric then None
          else
            morphism.reversed.toOption.filter: reversed =>
              Morphism.validateDomains(reversed, spatial.domains(reversed.source), spatial.domains(reversed.target)).isRight
    val edges = (forward ++ inverse)
      .filter(morphism => morphism.source != morphism.target)
      .zipWithIndex
    val adjacency = edges.groupBy(_._1.source).view.mapValues(_.sortBy(_._2)).toMap
    val paths = Vector.newBuilder[(Vector[Morphism], Vector[Int])]

    def visit(current: DomainId, visited: Set[DomainId], path: Vector[Morphism], orders: Vector[Int]): Unit =
      if current == target then paths += path -> orders
      else
        adjacency.getOrElse(current, Vector.empty).foreach: (morphism, order) =>
          if !visited(morphism.target) then
            visit(morphism.target, visited + morphism.target, path :+ morphism, orders :+ order)

    visit(source, Set(source), Vector.empty, Vector.empty)
    paths.result().minByOption: (path, orders) =>
      (
        path.map(_.cost).sum,
        path.map(_.target.value).mkString("\u0000"),
        orders.mkString(",")
      )
    .map(_._1)

  private def allowed(morphism: Morphism, policy: RoutingPolicy): Boolean =
    policy match
      case RoutingPolicy.Shortest => true
      case RoutingPolicy.Anatomical =>
        morphism.routeTag == RouteTag.Identity || morphism.routeTag == RouteTag.Anatomical
      case RoutingPolicy.Functional =>
        morphism.routeTag == RouteTag.Identity || morphism.routeTag == RouteTag.Functional

  private def value[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def domain(name: String): Domain =
    val id = value(DomainId(name))
    val subject = value(SubjectId("sub-01"))
    val modality = value(Modality(name))
    val geometry = value(SamplingGeometry.volume(NeuroSpace(Vector(1, 1, 1), trans = Some(DMat.eye(4)))))
    value(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def morphism(
      name: String,
      source: Domain,
      target: Domain,
      cost: Double,
      tag: RouteTag = RouteTag.Anatomical,
      inverse: Inverse = Inverse.None
  ): Morphism =
    value(
      Morphism.build(
        value(MorphismId(name)),
        source.id,
        target.id,
        MorphismKind.Affine3D,
        tag,
        cost,
        inverse
      )
    )
