package scalafim.spatial

import scalafim.image.{SampleSpaces, DMat, SomeSampleSpace}
import scalafim.image.SampleSpaces.*

class FieldRuntimeSuite extends munit.FunSuite:

  private def value[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def linValue[A](result: Either[LinearMapError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.getMessage)

  private def domain(name: String, dims: Vector[Int] = Vector(3, 1, 1)): Domain =
    val id = value(DomainId(name))
    val subject = value(SubjectId("sub-01"))
    val modality = value(Modality(name))
    val geometry = value(SamplingGeometry.volume(SampleSpaces(dims, trans = Some(DMat.eye(4)))))
    value(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def translation(x: Double, y: Double, z: Double): DMat =
    DMat.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, x),
        Vector(0.0, 1.0, 0.0, y),
        Vector(0.0, 0.0, 1.0, z),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def affine(idValue: String, source: Domain, target: Domain, matrix: DMat): Morphism =
    value(
      Morphism.build(
        id = value(MorphismId(idValue)),
        source = source.id,
        target = target.id,
        kind = MorphismKind.Affine3D,
        routeTag = RouteTag.Anatomical,
        cost = 1.0,
        inverse = Inverse.Exact("analytic"),
        coordinateMap = value(CoordinateMap.affine3D(matrix))
      )
    )

  private def graph(domains: Vector[Domain], morphisms: Vector[Morphism]): SpatialGraph =
    value(SpatialGraph.build(domains, morphisms))

  private def compile(g: SpatialGraph, request: CompileRequest): SpatialOperator =
    value(OperatorCompiler.compile(g, request))

  test("field views are inspectable before materialization"):
    val source = domain("source")
    val target = domain("target")
    val op = compile(
      graph(Vector(source, target), Vector(affine("source-to-target", source, target, translation(1.0, 0.0, 0.0)))),
      CompileRequest(source.id, target.id, sampling = SamplingPolicy.Nearest)
    )
    val field = Field.fromMatrix(source.id, DoubleMatrix.fromRows(Vector(Vector(10.0), Vector(20.0), Vector(30.0))), "bold")
    val cache = InMemoryOperatorCache.empty
    val runtime = CachedFieldRuntime(cache)

    val view = value(runtime.view(field, op))

    assert(view.isView)
    assertEquals(view.rootId, field.rootId)
    assertEquals(view.domain, target.id)
    assertEquals(view.root, source.id)
    assertEquals(view.sampleCount, op.rows)
    assertEquals(view.observations, 1)
    assertEquals(view.pending.length, 1)
    assertEquals(view.legacyExecution.operators, view.pending)
    assertEquals(view.plan.intent.root, source.id)
    assertEquals(view.plan.intent.target, target.id)
    assertEquals(view.plan.steps.length, 1)
    view.plan.steps.head match
      case ViewPlanStep.Reexpress(_, _, _, _, _, _, ViewStepOrigin.LegacyCompiled(path, compiler)) =>
        assertEquals(path, op.provenance.path)
        assertEquals(compiler, op.provenance.compiler)
      case other =>
        fail(s"expected an explicit legacy compatibility step, got $other")
    assertEquals(view.provenance.transforms.map(_.compiler), Vector("affine-pullback-fused-v1"))
    assertEquals(cache.size, 1)

  test("field runtime materializes by applying cached operators"):
    val source = domain("source")
    val target = domain("target")
    val op = compile(
      graph(Vector(source, target), Vector(affine("source-to-target", source, target, translation(1.0, 0.0, 0.0)))),
      CompileRequest(source.id, target.id, sampling = SamplingPolicy.Nearest)
    )
    val field = Field.fromMatrix(source.id, DoubleMatrix.fromRows(Vector(Vector(10.0), Vector(20.0), Vector(30.0))), "bold")
    val runtime = CachedFieldRuntime(InMemoryOperatorCache.empty)
    val view = value(runtime.view(field, op))

    val data = value(runtime.data(view))

    assertEquals(data.toRows, Vector(Vector(20.0), Vector(30.0), Vector(0.0)))
    assertEquals(linValue(op.forward(field.data.materialized.get)).toRows, data.toRows)

  test("runtime supports chained lazy views"):
    val source = domain("source", Vector(4, 1, 1))
    val mid = domain("mid", Vector(4, 1, 1))
    val target = domain("target", Vector(2, 1, 1))
    val first = compile(
      graph(Vector(source, mid), Vector(affine("source-to-mid", source, mid, translation(1.0, 0.0, 0.0)))),
      CompileRequest(source.id, mid.id, sampling = SamplingPolicy.Nearest)
    )
    val second = compile(
      graph(Vector(mid, target), Vector(affine("mid-to-target", mid, target, translation(1.0, 0.0, 0.0)))),
      CompileRequest(mid.id, target.id, sampling = SamplingPolicy.Nearest)
    )
    val field = Field.fromMatrix(source.id, DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0), Vector(4.0))), "bold")
    val runtime = CachedFieldRuntime(InMemoryOperatorCache.empty)

    val midView = value(runtime.view(field, first))
    val targetView = value(runtime.view(midView, second))
    val data = value(runtime.data(targetView))

    assertEquals(targetView.pending.length, 2)
    assertEquals(targetView.plan.steps.length, 2)
    assertEquals(targetView.plan.rootId, field.rootId)
    assertEquals(targetView.plan.intent.root, source.id)
    assertEquals(targetView.plan.intent.target, target.id)
    assertEquals(targetView.provenance.transforms.map(_.source), Vector(source.id, mid.id))
    assertEquals(data.toRows, Vector(Vector(3.0), Vector(4.0)))

  test("field roots have distinct identities even when domain and label match"):
    val source = domain("source")
    val data = DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0)))
    val first = Field.fromMatrix(source.id, data, "bold")
    val second = Field.fromMatrix(source.id, data, "bold")
    val explicitId =
      FieldRootId("dataset:sub-01:run-01") match
        case Right(value) => value
        case Left(error) => fail(error.message)
    val explicit = Field.fromMatrix(explicitId, source.id, data, "bold")

    assertNotEquals(first.rootId, second.rootId)
    assertEquals(explicit.rootId, explicitId)
    assert(first.plan.isRoot)
    assert(second.plan.isRoot)

  test("cache misses are explicit and caches are not ambient registries"):
    val source = domain("source")
    val target = domain("target")
    val op = compile(
      graph(Vector(source, target), Vector(affine("source-to-target", source, target, DMat.eye(4)))),
      CompileRequest(source.id, target.id, sampling = SamplingPolicy.Nearest)
    )
    val field = Field.fromMatrix(source.id, DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0))), "bold")
    val runtimeWithCache = CachedFieldRuntime(InMemoryOperatorCache.empty)
    val view = value(runtimeWithCache.view(field, op))
    val runtimeWithoutCache = CachedFieldRuntime(InMemoryOperatorCache.empty)

    val result = runtimeWithoutCache.data(view)

    assertEquals(result.left.toOption, Some(SpatialError.OperatorCacheMiss(view.pending.head.label)))

  test("field runtime rejects mismatched operator domains and shapes"):
    val source = domain("source")
    val other = domain("other")
    val target = domain("target")
    val op = compile(
      graph(Vector(other, target), Vector(affine("other-to-target", other, target, DMat.eye(4)))),
      CompileRequest(other.id, target.id, sampling = SamplingPolicy.Nearest)
    )
    val field = Field.fromMatrix(source.id, DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0))), "bold")
    val runtime = CachedFieldRuntime(InMemoryOperatorCache.empty)

    val result = runtime.view(field, op)

    assertEquals(result.left.toOption, Some(SpatialError.FieldDomainMismatch(source.id, other.id)))

  test("source-backed field refs are inspectable but not materialized"):
    val source = domain("source")
    val sourceId = value(FieldSourceId("archive-run"))
    val descriptor = value(
      FieldSourceDescriptor.make(
        sourceId,
        value(FieldSourceRevision("revision-1")),
        "archive-run",
        source.id,
        source.geometry,
        rows = 3,
        observations = 2
      )
    )
    val field = value(
      Field.fromSource(
        source,
        UnavailableFieldSource(descriptor, "fixture has no backing reader")
      )
    )
    val runtime = CachedFieldRuntime(InMemoryOperatorCache.empty)

    assertEquals(field.describe.label, "archive-run")
    assert(!field.data.isMaterialized)
    assertEquals(runtime.data(field).left.toOption, Some(SpatialError.FieldDataUnavailable("archive-run")))
