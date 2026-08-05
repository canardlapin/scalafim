package scalafim.spatial

import scalafim.image.{DMat, NeuroSpace}

class FieldExplanationSuite extends munit.FunSuite:

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def apiValue[A](result: Either[FieldApiError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def policyValue(result: Either[FieldQcPolicyError, FieldQcPolicy]): FieldQcPolicy =
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

  private def pointwiseBarrier(name: String, source: Domain, target: Domain): Morphism =
    val pluginId = spatialValue(MorphismPluginId(s"$name-plugin"))
    val plugin = spatialValue(MorphismPlugin.pointwiseBarrier(pluginId, scale = 2.0, offset = 1.0))
    spatialValue(
      Morphism.build(
        id = spatialValue(MorphismId(name)),
        source = source.id,
        target = target.id,
        kind = MorphismKind.Filter,
        routeTag = RouteTag.Functional,
        plugin = Some(plugin)
      )
    )

  private def rootData: DoubleMatrix =
    DoubleMatrix.fromRows(
      Vector(
        Vector(0.0, 10.0, 20.0),
        Vector(1.0, 11.0, 21.0),
        Vector(2.0, 12.0, 22.0),
        Vector(3.0, 13.0, 23.0)
      )
    )

  test("descriptive explanation is deterministic and performs no planning or evaluation"):
    val root = volumeDomain("root")
    val target = volumeDomain("target")
    given SpatialGraph = spatialValue(
      SpatialGraph.build(Vector(root, target), Vector(affine("root-target", root, target, 0.0)))
    )
    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    val field = Field.fromMatrix(root.id, rootData, "bold")
    val first = apiValue(field.to(target).flatMap(_.rows(3, 1)).flatMap(_.timeBlock(1, 2)))
    val equivalent = apiValue(field.to(target).flatMap(_.rows(3, 1)).flatMap(_.timeBlock(1, 2)))

    val explained = first.explain

    assertEquals(explained, equivalent.explain)
    assertEquals(explained.phase, FieldExplanationPhase.Descriptive)
    assertEquals(explained.rootId, field.rootId)
    assertEquals(explained.rootDomain, root.id)
    assertEquals(explained.currentDomain, target.id)
    assertEquals(explained.currentShape, FieldShape(2, 2))
    assertEquals(explained.demand.targetRows, Vector(3, 1))
    assertEquals(explained.demand.observationIndices, Vector(1, 2))
    assertEquals(explained.execution, None)
    assertEquals(runtime.explain(first), explained)
    assertEquals(runtime.stats, LazyRuntimeStats(0L, 0L, 0L, 0L, 0L))

  test("explicit planning explains normalization, compilers, fusion, support, cache, and QC without evaluation"):
    val root = volumeDomain("root")
    val mid = volumeDomain("mid")
    val target = volumeDomain("target")
    given SpatialGraph = spatialValue(
      SpatialGraph.build(
        Vector(root, mid, target),
        Vector(affine("root-mid", root, mid, 0.5), affine("mid-target", mid, target, 0.5))
      )
    )
    val runtime = LazyFieldRuntime(summon[SpatialGraph], backend = PullbackProgramCompiler.volumeAffine)
    val view = apiValue(Field.fromMatrix(root.id, rootData, "bold").to(mid).flatMap(_.to(target)).flatMap(_.rows(0, 2)))

    val planned = spatialValue(runtime.plan(view))
    val execution = planned.execution.getOrElse(fail("expected planned execution details"))

    assertEquals(planned.phase, FieldExplanationPhase.Planned)
    assertEquals(execution.normalizedRoute.map(_.morphism.value), Vector("root-mid", "mid-target"))
    assertEquals(execution.normalizedRoute.map(_.inverse.quality), Vector(1.0, 1.0))
    assertEquals(execution.compilerTrace.map(_.value), Vector("affine-pullback-v1", "affine-pullback-v1"))
    assertEquals(execution.stages.map(_.semantics), Vector.fill(2)(StageSemantics.CoordinatePullback))
    assertEquals(execution.sourceSupport.sourceRows, Vector(1, 3))
    assertEquals(execution.sourceSupport.precision, SupportPrecision.Exact)
    assertEquals(execution.fusion, FusionExplanation(2, Vector.empty, 1))
    assertEquals(execution.qc.coverage.targetRows, Vector(0, 2))
    assertEquals(execution.planCache, CacheDisposition.Miss)
    assertEquals(execution.resultCache, CacheDisposition.NotConsulted)
    assertEquals(execution.backend.value, "volume-affine-program-v1")
    assert(execution.cacheKey.label.nonEmpty)
    assertEquals(runtime.explain(view), planned)
    assertEquals(runtime.stats.compilations, 1L)
    assertEquals(runtime.stats.executions, 0L)

  test("evaluated explanation is retained per field and materialization records full lineage"):
    val root = volumeDomain("root")
    val target = volumeDomain("target")
    given SpatialGraph = spatialValue(
      SpatialGraph.build(Vector(root, target), Vector(affine("root-target", root, target, 1.0)))
    )
    val runtime = LazyFieldRuntime(summon[SpatialGraph], backend = PullbackProgramCompiler.volumeAffine)
    given FieldRuntime = runtime
    val field = Field.fromMatrix(root.id, rootData, "bold")
    val first = apiValue(field.to(target).flatMap(_.rows(0, 2)))
    val second = apiValue(field.to(target).flatMap(_.rows(1, 3)))

    val materialized = apiValue(first.materialize)
    apiValue(second.value)

    val firstExplanation = runtime.explain(first)
    val execution = firstExplanation.execution.getOrElse(fail("expected evaluation details"))
    val lineage = materialized.explain.lineage.lastOption.getOrElse(fail("expected materialization lineage"))

    assertEquals(firstExplanation.phase, FieldExplanationPhase.Evaluated)
    assertEquals(execution.resultCache, CacheDisposition.Miss)
    assertEquals(execution.fusion.valueResamplingPasses, 1)
    assertEquals(execution.qc.coverage.targetRows, Vector(0, 2))
    assertEquals(lineage.sourceRootId, field.rootId)
    assertEquals(lineage.sourceRootRevision, field.rootRevision)
    assertEquals(lineage.sourceDomain, root.id)
    assertEquals(lineage.targetDomain, target.id)
    assertEquals(lineage.demand.targetRows, Vector(0, 2))
    assertEquals(lineage.execution, Some(execution))
    assertEquals(materialized.provenance.root, target.id)

  test("plugin fusion barriers remain explicit in planned stage order"):
    val root = volumeDomain("root")
    val target = volumeDomain("target")
    given SpatialGraph = spatialValue(
      SpatialGraph.build(Vector(root, target), Vector(pointwiseBarrier("threshold", root, target)))
    )
    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    val view = apiValue(Field.fromMatrix(root.id, rootData, "bold").to(target))

    val planned = spatialValue(runtime.plan(view))
    val execution = planned.execution.getOrElse(fail("expected plugin plan"))

    assertEquals(execution.normalizedRoute.map(_.compiler.value), Vector("filter-value-plugin-v1"))
    assertEquals(execution.stages.map(_.semantics), Vector(StageSemantics.FusionBarrier))
    assertEquals(execution.fusion, FusionExplanation(0, Vector(spatialValue(MorphismId("threshold"))), 0))

  test("coverage and path-quality QC failures remain typed"):
    val root = volumeDomain("root")
    val target = volumeDomain("target")
    given SpatialGraph = spatialValue(
      SpatialGraph.build(Vector(root, target), Vector(affine("root-target", root, target, 10.0)))
    )
    val runtime = LazyFieldRuntime(summon[SpatialGraph], backend = PullbackProgramCompiler.volumeAffine)
    val view = apiValue(Field.fromMatrix(root.id, rootData, "bold").to(target).flatMap(_.rows(0)))
    val planned = spatialValue(runtime.plan(view))
    val qc = planned.execution.getOrElse(fail("expected planned QC")).qc
    val coveragePolicy = policyValue(FieldQcPolicy.build(minimumCoverage = 1.0))
    val qualityPolicy = policyValue(FieldQcPolicy.build(minimumPathQuality = 1.0))

    assertEquals(
      qc.validate(coveragePolicy),
      Left(FieldQcFailure.CoverageBelowMinimum(0.0, 1.0))
    )
    val lowerQuality = qc.copy(pathQuality = 0.5)
    assertEquals(
      lowerQuality.validate(qualityPolicy),
      Left(FieldQcFailure.PathQualityBelowMinimum(0.5, 1.0))
    )
    assertEquals(
      FieldQcPolicy.build(minimumCoverage = 1.5).left.toOption,
      Some(FieldQcPolicyError.InvalidMinimumCoverage(1.5))
    )
