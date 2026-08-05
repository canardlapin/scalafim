package scalafim.spatial

import scalafim.image.{DMat, NeuroSpace}

class LazyFieldRuntimeSuite extends munit.FunSuite:

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def apiValue[A](result: Either[FieldApiError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def volumeDomain(name: String): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val modality = spatialValue(Modality(name))
    val geometry = spatialValue(
      SamplingGeometry.volume(NeuroSpace(Vector(4, 1, 1), trans = Some(DMat.eye(4))))
    )
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def translation(x: Double): DMat =
    DMat.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, x),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def affine(name: String, source: Domain, target: Domain, x: Double): Morphism =
    spatialValue(
      Morphism.build(
        id = spatialValue(MorphismId(name)),
        source = source.id,
        target = target.id,
        kind = MorphismKind.Affine3D,
        routeTag = RouteTag.Anatomical,
        inverse = Inverse.Exact("analytic"),
        coordinateMap = spatialValue(CoordinateMap.affine3D(translation(x)))
      )
    )

  private def rootData(offset: Double = 0.0): DoubleMatrix =
    DoubleMatrix.fromRows(
      Vector(
        Vector(offset + 0.0, offset + 10.0, offset + 20.0),
        Vector(offset + 1.0, offset + 11.0, offset + 21.0),
        Vector(offset + 2.0, offset + 12.0, offset + 22.0),
        Vector(offset + 3.0, offset + 13.0, offset + 23.0)
      )
    )

  test("value compiles and executes one root-first operator, then memoizes the result"):
    val root = volumeDomain("root")
    val mid = volumeDomain("mid")
    val target = volumeDomain("target")
    given SpatialGraph = spatialValue(
      SpatialGraph.build(
        Vector(root, mid, target),
        Vector(affine("root-mid", root, mid, 0.5), affine("mid-target", mid, target, 0.5))
      )
    )
    val backend = CountingBackend(PullbackProgramCompiler.volumeAffine)
    val planCache = InMemoryEvaluationPlanCache.empty
    val resultCache = InMemoryFieldResultCache.empty
    val runtime = LazyFieldRuntime(summon[SpatialGraph], backend, planCache = planCache, resultCache = resultCache)
    given FieldRuntime = runtime
    val field = Field.fromMatrix(root.id, rootData(), "bold")
    val view = apiValue(
      field
        .to(mid)
        .flatMap(_.to(target))
        .flatMap(_.rows(0, 2))
        .flatMap(_.timeBlock(1, 2))
    )

    val first = apiValue(view.value)
    val second = apiValue(view.value)

    assertEquals(first.toRows, Vector(Vector(11.0, 21.0), Vector(13.0, 23.0)))
    assertEquals(second.toRows, first.toRows)
    assertEquals(backend.compileCalls, 1)
    assertEquals(planCache.size, 1)
    assertEquals(resultCache.size, 1)
    assertEquals(runtime.stats.compilations, 1L)
    assertEquals(runtime.stats.executions, 1L)
    assertEquals(runtime.stats.resultCacheHits, 1L)
    val trace = runtime.lastTrace.getOrElse(fail("expected an evaluation trace"))
    assertEquals(trace.resultCache, CacheDisposition.Hit)
    assertEquals(trace.valueResamplingPasses, 1)
    assertEquals(trace.support.sourceRows, Vector(1, 3))

  test("demand, policy, and root revision are independent cache dimensions"):
    val root = volumeDomain("root")
    val target = volumeDomain("target")
    given SpatialGraph = spatialValue(
      SpatialGraph.build(Vector(root, target), Vector(affine("root-target", root, target, 0.0)))
    )
    val planCache = InMemoryEvaluationPlanCache.empty
    val resultCache = InMemoryFieldResultCache.empty
    val runtime = LazyFieldRuntime(summon[SpatialGraph], planCache = planCache, resultCache = resultCache)
    given FieldRuntime = runtime
    val explicitId = spatialValue(FieldRootId("shared-id").left.map(error => SpatialError.OperatorAssemblyFailed(error.message)))
    val firstRoot = Field.fromMatrix(explicitId, root.id, rootData(), "first")
    val secondRoot = Field.fromMatrix(explicitId, root.id, rootData(100.0), "second")

    val rowZero = apiValue(firstRoot.to(target, sampling = SamplingPolicy.Nearest).flatMap(_.rows(0)))
    val rowOne = apiValue(firstRoot.to(target, sampling = SamplingPolicy.Nearest).flatMap(_.rows(1)))
    val trilinear = apiValue(firstRoot.to(target, sampling = SamplingPolicy.Trilinear).flatMap(_.rows(0)))
    val revised = apiValue(secondRoot.to(target, sampling = SamplingPolicy.Nearest).flatMap(_.rows(0)))

    assertEquals(apiValue(rowZero.value).toRows, Vector(Vector(0.0, 10.0, 20.0)))
    assertEquals(apiValue(rowOne.value).toRows, Vector(Vector(1.0, 11.0, 21.0)))
    assertEquals(apiValue(trilinear.value).toRows, Vector(Vector(0.0, 10.0, 20.0)))
    assertEquals(apiValue(revised.value).toRows, Vector(Vector(100.0, 110.0, 120.0)))
    assertEquals(firstRoot.rootId, secondRoot.rootId)
    assertNotEquals(firstRoot.rootRevision, secondRoot.rootRevision)
    assertEquals(planCache.size, 4)
    assertEquals(resultCache.size, 4)
    assertEquals(resultCache.keys.map(_.demand).distinct.length, 2)
    assertEquals(resultCache.keys.map(_.sampling).distinct.length, 2)
    assertEquals(resultCache.keys.map(_.rootRevision).distinct.length, 2)

  test("failed compilation does not poison plan or result caches"):
    val root = volumeDomain("root")
    val target = volumeDomain("target")
    given SpatialGraph = spatialValue(
      SpatialGraph.build(Vector(root, target), Vector(affine("root-target", root, target, 0.0)))
    )
    val backend = FlakyBackend(PullbackProgramCompiler.volumeAffine)
    val planCache = InMemoryEvaluationPlanCache.empty
    val resultCache = InMemoryFieldResultCache.empty
    val runtime = LazyFieldRuntime(summon[SpatialGraph], backend, planCache = planCache, resultCache = resultCache)
    given FieldRuntime = runtime
    val view = apiValue(Field.fromMatrix(root.id, rootData(), "bold").to(target))

    assertEquals(
      view.value.left.toOption,
      Some(FieldApiError.Spatial(SpatialError.OperatorAssemblyFailed("forced compiler failure")))
    )
    assertEquals(planCache.size, 0)
    assertEquals(resultCache.size, 0)

    assertEquals(apiValue(view.value).rows, target.nElements)
    assertEquals(backend.compileCalls, 2)
    assertEquals(planCache.size, 1)
    assertEquals(resultCache.size, 1)

  test("materialize creates a fresh current-domain root for descendants"):
    val root = volumeDomain("root")
    val target = volumeDomain("target")
    val next = volumeDomain("next")
    given SpatialGraph = spatialValue(
      SpatialGraph.build(
        Vector(root, target, next),
        Vector(affine("root-target", root, target, 1.0), affine("target-next", target, next, 1.0))
      )
    )
    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    given FieldRuntime = runtime
    val original = Field.fromMatrix(root.id, rootData(), "bold")
    val targetView = apiValue(original.to(target))
    val materialized = apiValue(targetView.materialize)
    val descendant = apiValue(materialized.to(next))
    val descendantValues = apiValue(descendant.value)

    assert(materialized.isMaterializedRoot)
    assertEquals(materialized.root, target.id)
    assertEquals(materialized.domain, target.id)
    assertEquals(materialized.plan.steps, Vector.empty)
    assertNotEquals(materialized.rootId, original.rootId)
    assertNotEquals(materialized.rootRevision, original.rootRevision)
    assertEquals(descendant.root, target.id)
    assertEquals(
      descendantValues.toRows,
      Vector(
        Vector(2.0, 12.0, 22.0),
        Vector(3.0, 13.0, 23.0),
        Vector(0.0, 0.0, 0.0),
        Vector(0.0, 0.0, 0.0)
      )
    )
    assertEquals(runtime.stats.materializations, 1L)

  test("identity row demand is inspectable as zero spatial resampling passes"):
    val root = volumeDomain("root")
    given SpatialGraph = spatialValue(SpatialGraph.build(Vector(root), Vector.empty))
    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    given FieldRuntime = runtime
    val selected = apiValue(Field.fromMatrix(root.id, rootData(), "bold").rows(3, 1))

    assertEquals(apiValue(selected.value).toRows, Vector(Vector(3.0, 13.0, 23.0), Vector(1.0, 11.0, 21.0)))
    assertEquals(runtime.lastTrace.map(_.valueResamplingPasses), Some(0))

private final class CountingBackend(delegate: PullbackProgramCompiler) extends PullbackProgramCompiler:
  var compileCalls: Int = 0

  override def id: PullbackBackendId =
    delegate.id

  override def compile(program: PullbackProgram): Either[SpatialError, SpatialOperator] =
    compileCalls += 1
    delegate.compile(program)

private final class FlakyBackend(delegate: PullbackProgramCompiler) extends PullbackProgramCompiler:
  var compileCalls: Int = 0

  override def id: PullbackBackendId =
    delegate.id

  override def compile(program: PullbackProgram): Either[SpatialError, SpatialOperator] =
    compileCalls += 1
    if compileCalls == 1 then Left(SpatialError.OperatorAssemblyFailed("forced compiler failure"))
    else delegate.compile(program)
