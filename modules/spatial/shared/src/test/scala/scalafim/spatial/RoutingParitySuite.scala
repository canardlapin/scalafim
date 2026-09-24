package scalafim.spatial

import scalafim.image.world.SubjectId

import scalafim.image.SampleSpaces

/** Routing parity with neurofunctor `find_path_with_inverses`, `inspect_path` and `all_paths`.
  *
  * The expected rows come from `tools/r-parity/generate_neurofunctor_routing_fixtures.R`, run against the installed
  * neurofunctor 0.1.0 (source checkout 363e48f72874df882154b018990ef926d0a53dda). Edge names match the generator:
  * e1 A->B affine cost 1, e2 B->C warp cost 1 inverse quality 0.2, e3 D->C warp cost 0.5 quality 0.9, e4 A->D affine
  * cost 2, e5 E->A warp cost 0.3 quality 0.5. neurofunctor affines have exact inverses; warps with an inverse asset
  * have provided ones.
  */
class RoutingParitySuite extends munit.FunSuite:

  private final case class Expected(
    source: String,
    target: String,
    penalty: Double,
    edges: Vector[String],
    usedInverses: Boolean,
    totalCost: Double
  )

  // source,target,penalty,edges,used_inverses,total_cost (neurofunctor output, verbatim)
  private val oracle =
    Vector(
      "A,C,1.0,e1|e2,FALSE,2.000000",
      "C,A,1.0,e3:inverse|e4:inverse,TRUE,2.600000",
      "C,A,10.0,e3:inverse|e4:inverse,TRUE,3.500000",
      "A,E,1.0,e5:inverse,TRUE,0.800000",
      "C,B,1.0,e2:inverse,TRUE,1.800000",
      "C,B,0.0,e2:inverse,TRUE,1.000000",
      "D,B,1.0,e3|e2:inverse,TRUE,2.300000",
      "B,E,1.0,e1:inverse|e5:inverse,TRUE,1.800000"
    ).map { row =>
      val cells = row.split(",", -1).toVector
      Expected(cells(0), cells(1), cells(2).toDouble, cells(3).split("\\|").toVector, cells(4) == "TRUE", cells(5).toDouble)
    }

  private def value[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def domain(name: String): Domain =
    val subject = value(SubjectId("sub-01").asSpatial)
    val geometry = value(SamplingGeometry.volume(SampleSpaces(Vector(2, 2, 2), affine = Some(ProviderAffines.identity))))
    value(Domain.build(value(DomainId(name)), SpaceRef.Volume(subject, None, value(Modality(name))), geometry))

  private val domains = Vector("A", "B", "C", "D", "E").map(name => name -> domain(name)).toMap

  private def edge(name: String, source: String, target: String, kind: MorphismKind, cost: Double, inverse: Inverse): Morphism =
    value(
      Morphism.build(
        value(MorphismId(name)),
        domains(source).id,
        domains(target).id,
        kind,
        RouteTag.Anatomical,
        cost,
        inverse
      )
    )

  private val graph =
    value(
      SpatialGraph.build(
        Vector("A", "B", "C", "D", "E").map(domains),
        Vector(
          edge("e1", "A", "B", MorphismKind.Affine3D, 1.0, Inverse.Exact("analytic")),
          edge("e2", "B", "C", MorphismKind.Warp3D, 1.0, Inverse.Provided("file", 0.2)),
          edge("e3", "D", "C", MorphismKind.Warp3D, 0.5, Inverse.Provided("file", 0.9)),
          edge("e4", "A", "D", MorphismKind.Affine3D, 2.0, Inverse.Exact("analytic")),
          edge("e5", "E", "A", MorphismKind.Warp3D, 0.3, Inverse.Provided("file", 0.5))
        )
      )
    )

  oracle.foreach { expected =>
    test(s"find_path_with_inverses ${expected.source}->${expected.target} penalty ${expected.penalty}"):
      val penalty = value(InversePenalty(expected.penalty))
      val path = value(graph.path(domains(expected.source).id, domains(expected.target).id, allowInverses = true, inversePenalty = penalty))
      assertEquals(path.ids.map(_.value), expected.edges)
      assertEquals(path.usedInverses, expected.usedInverses)
      assertEqualsDouble(path.cost, expected.totalCost, 1e-9)
      assertEquals(path.invertedSteps.map(_.value), expected.edges.filter(_.endsWith(":inverse")))
  }

  test("forward routes win even when an inverse route would be cheaper"):
    val a = domain("fa")
    val b = domain("fb")
    val expensive = edge2("expensive", a, b, 10.0, Inverse.Exact("analytic"))
    val reverse = edge2("cheap-reverse", b, a, 0.1, Inverse.Exact("analytic"))
    val local = value(SpatialGraph.build(Vector(a, b), Vector(expensive, reverse)))

    val path = value(local.path(a.id, b.id, allowInverses = true))
    assertEquals(path.ids.map(_.value), Vector("expensive"))
    assert(!path.usedInverses)

  test("inverse penalty must be finite and non-negative"):
    assertEquals(InversePenalty(-1.0).left.toOption, Some(SpatialError.InvalidInversePenalty(-1.0)))
    assert(InversePenalty(Double.NaN).isLeft)
    assertEqualsDouble(InversePenalty.default.value, 1.0, 0.0)

  test("inspectPath reports steps, routed costs and inverse use"):
    val inspection = value(graph.inspectPath(domains("C").id, domains("A").id, allowInverses = true))
    assertEquals(inspection.steps.map(_.morphism.value), Vector("e3:inverse", "e4:inverse"))
    assertEquals(inspection.steps.map(_.inverted), Vector(true, true))
    assertEquals(inspection.steps.map(_.source.value), Vector("C", "D"))
    assertEquals(inspection.steps.map(_.target.value), Vector("D", "A"))
    assertEqualsDouble(inspection.steps.head.cost, 0.6, 1e-12)
    assertEqualsDouble(inspection.totalCost, 2.6, 1e-12)
    assert(inspection.usedInverses)
    val rendered = inspection.render
    assert(rendered.contains("Step 1: Warp3D (inverse) e3:inverse"), clue = rendered)
    assert(rendered.contains("Total steps: 2"), clue = rendered)

    val forwardOnly = graph.inspectPath(domains("C").id, domains("A").id)
    assertEquals(forwardOnly.left.toOption, Some(SpatialError.NoPath(domains("C").id, domains("A").id)))

  test("allPaths lists simple forward routes cheapest first and honours the limit"):
    val paths = value(graph.allPaths(domains("A").id, domains("C").id))
    assertEquals(paths.map(_.ids.map(_.value)), Vector(Vector("e1", "e2"), Vector("e4", "e3")))
    assertEquals(paths.map(_.cost), Vector(2.0, 2.5))
    assert(paths.forall(!_.usedInverses))

    val limited = value(graph.allPaths(domains("A").id, domains("C").id, maxPaths = 1))
    assertEquals(limited.map(_.ids.map(_.value)), Vector(Vector("e1", "e2")))

    assertEquals(graph.allPaths(domains("A").id, domains("C").id, maxPaths = 0).left.toOption, Some(SpatialError.InvalidPathLimit(0)))
    assertEquals(value(graph.allPaths(domains("C").id, domains("A").id)), Vector.empty)

  test("allPaths can enumerate inverse alternatives and keeps parallel edges distinct"):
    val routes = value(graph.allPaths(domains("C").id, domains("A").id, allowInverses = true))
    assertEquals(
      routes.map(_.ids.map(_.value)),
      Vector(Vector("e3:inverse", "e4:inverse"), Vector("e2:inverse", "e1:inverse"))
    )
    assert(routes.forall(_.usedInverses))

    val a = domain("pa")
    val b = domain("pb")
    val parallel =
      value(
        SpatialGraph.build(
          Vector(a, b),
          Vector(edge2("first", a, b, 2.0, Inverse.None), edge2("second", a, b, 1.0, Inverse.None))
        )
      )
    assertEquals(value(parallel.allPaths(a.id, b.id)).map(_.ids.map(_.value)), Vector(Vector("second"), Vector("first")))

  private def edge2(name: String, source: Domain, target: Domain, cost: Double, inverse: Inverse): Morphism =
    value(Morphism.build(value(MorphismId(name)), source.id, target.id, MorphismKind.Affine3D, RouteTag.Anatomical, cost, inverse))
