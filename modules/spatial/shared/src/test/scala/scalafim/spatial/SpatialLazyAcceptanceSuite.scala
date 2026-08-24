package scalafim.spatial

import ravel.NDArray as RavelArray
import scalafim.image.{GridSpec, Resample, SampleSpaces, SpatialPullbacks}
import scalafim.image.SampleSpaces.*
import scalafim.surface.*

enum ScenarioStatus:
  case Pass, Fail

final case class ScenarioObservation(name: String, passed: Boolean, detail: String):
  require(name.trim.nonEmpty, "scenario observation name must be non-empty")
  require(detail.trim.nonEmpty, "scenario observation detail must be non-empty")

final case class ScenarioResult(id: String, observations: Vector[ScenarioObservation]):
  require(id.trim.nonEmpty, "scenario id must be non-empty")
  require(observations.nonEmpty, "scenario must contain at least one observation")

  def status: ScenarioStatus =
    if observations.forall(_.passed) then ScenarioStatus.Pass else ScenarioStatus.Fail

  def ciPass: Boolean =
    status == ScenarioStatus.Pass

  def render: String =
    val checks = observations.map { observation =>
      s"  ${observation.name}: ${observation.detail} pass=${observation.passed}"
    }
    (s"scenario=$id status=$status" +: checks).mkString("\n")

object SpatialScenario:
  def fact(name: String, passed: Boolean, detail: String): ScenarioObservation =
    ScenarioObservation(name, passed, detail)

  def vector(
    name: String,
    actual: Vector[Double],
    expected: Vector[Double],
    tolerance: Double
  ): ScenarioObservation =
    val maximumError =
      if actual.length != expected.length then Double.PositiveInfinity
      else
        var index = 0
        var maximum = 0.0
        while index < actual.length do
          maximum = math.max(maximum, math.abs(actual(index) - expected(index)))
          index += 1
        maximum
    fact(
      name,
      maximumError <= tolerance,
      s"actual=$actual expected=$expected maxAbsError=$maximumError tolerance=$tolerance"
    )

/**
 * Frozen with R 4.5.1 `stats::approx` from the neurotransform pullback
 * convention: target -> dense map -> affine -> root interpolation. The
 * sequential values deliberately differ, so this fixture detects a second
 * value interpolation as well as a direction reversal.
 */
private object NeurotransformPullbackOracleV1:
  val targetX: Vector[Double] = Vector(0.0, 1.0, 2.0)
  val warpedX: Vector[Double] = Vector(0.0, 0.5, 1.5)
  val rootX: Vector[Double] = Vector(0.5, 1.0, 2.0)
  val direct: Vector[Double] = Vector(4.0, 8.0, 0.0)
  val sequential: Vector[Double] = Vector(4.0, 4.0, 3.0)

class SpatialLazyAcceptanceSuite extends munit.FunSuite:

  private val volumeSpace = SampleSpaces(Vector(6, 1, 1), affine = Some(ProviderAffines.identity))

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def apiValue[A](result: Either[FieldApiError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def volumeDomain(name: String): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-acceptance"))
    val modality = spatialValue(Modality(name))
    val geometry = spatialValue(SamplingGeometry.volume(volumeSpace))
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def surfaceDomain(name: String, geometry: SurfaceGeometry): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-acceptance"))
    val sampled = spatialValue(SamplingGeometry.surface(geometry))
    spatialValue(Domain.build(id, SpaceRef.Surface(subject, geometry.hemisphere, geometry.kind), sampled))

  private def volumeGrid(domain: Domain): GridSpec =
    domain.geometry match
      case SamplingGeometry.Volume(space, _) => GridSpec.fromSpace(space)
      case _ => fail(s"domain ${domain.id.value} is not volumetric")

  private def surfaceAt(xs: Vector[Double], kind: SurfaceKind): SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(xs(0), 0.0, 0.0),
          Vector(xs(1), 0.0, 0.0),
          Vector(xs(2), 0.0, 0.0)
        ),
        Vector((0, 1, 2))
      ),
      Hemisphere.Left,
      kind
    )

  private def affine(name: String, source: Domain, target: Domain, x: Double): Morphism =
    val matrix =
      ProviderAffines.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0, x),
          Vector(0.0, 1.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    spatialValue(
      Morphism.between(
        spatialValue(MorphismId(name)),
        source,
        target,
        MorphismKind.Affine3D,
        RouteTag.Anatomical,
        inverse = Inverse.Exact("analytic"),
        coordinateMap = spatialValue(CoordinateMap.affine(source, target, matrix))
      )
    )

  private def warp(name: String, source: Domain, target: Domain): Morphism =
    val sourceGrid = volumeGrid(source)
    val targetGrid = volumeGrid(target)
    val sourceX = Vector(0.0, 0.5, 1.5, 3.0, 4.0, 5.0)
    val data =
      RavelArray.tabulate[Double](
        targetGrid.shape.x,
        targetGrid.shape.y,
        targetGrid.shape.z,
        3
      ) { (x, y, z, component) =>
        component match
          case 0 => sourceX(x)
          case 1 => y.toDouble
          case _ => z.toDouble
      }
    val pullback =
      SpatialPullbacks
        .coordinates(
          sourceGrid,
          targetGrid,
          data,
          method = Resample.Method.Linear
        )
        .fold(error => fail(error.message), identity)
    spatialValue(
      Morphism.between(
        spatialValue(MorphismId(name)),
        source,
        target,
        MorphismKind.Warp3D,
        RouteTag.Anatomical,
        coordinateMap = spatialValue(CoordinateMap.dense(pullback))
      )
    )

  private def volumeToSurface(
    name: String,
    source: Domain,
    target: Domain,
    geometry: SurfaceGeometry
  ): Morphism =
    val pial = surfaceAt(Vector(0.0, 1.0, 2.0), SurfaceKind.Pial)
    val plan = VolumeSurfaceSamplingPlan(SurfaceGeometryPair(geometry, pial), SurfaceSamplingPath.White)
    spatialValue(
      Morphism.between(
        spatialValue(MorphismId(name)),
        source,
        target,
        MorphismKind.VolumeToSurface,
        RouteTag.Anatomical,
        inverse = Inverse.AdjointOnly,
        coordinateMap = CoordinateMap.volumeSamples(plan)
      )
    )

  private def rootValues: DoubleMatrix =
    val base = Vector(0.0, 8.0, 0.0, 4.0, 0.0, 12.0)
    DoubleMatrix.fromRows(base.map(value => Vector(value, value + 100.0, value + 200.0)))

  test("affine, nonlinear, and surface pull-through scenario returns one clean verdict"):
    val result = runScenario()
    assert(result.ciPass, result.render)
    assertEquals(result.status, ScenarioStatus.Pass)

  test("frozen R oracle distinguishes root-first interpolation from sequential interpolation"):
    assertNotEquals(NeurotransformPullbackOracleV1.direct, NeurotransformPullbackOracleV1.sequential)
    assertEquals(NeurotransformPullbackOracleV1.rootX, Vector(0.5, 1.0, 2.0))

  private def runScenario(): ScenarioResult =
    val root = volumeDomain("native")
    val anatomical = volumeDomain("anatomical")
    val warped = volumeDomain("warped")
    val white = surfaceAt(NeurotransformPullbackOracleV1.targetX, SurfaceKind.White)
    val surface = surfaceDomain("left-white", white)
    given SpatialGraph = spatialValue(
      SpatialGraph.build(
        Vector(root, anatomical, warped, surface),
        Vector(
          affine("native-anatomical", root, anatomical, 0.5),
          warp("anatomical-warped", anatomical, warped),
          volumeToSurface("warped-surface", warped, surface, white)
        )
      )
    )
    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    given FieldRuntime = runtime
    val field = Field.fromMatrix(root.id, rootValues, "bold")
    val view = apiValue(
      field
        .to(anatomical)
        .flatMap(_.to(warped))
        .flatMap(_.to(surface))
        .flatMap(_.vertices(2, 0))
        .flatMap(_.timeBlock(1, 2))
    )
    val before = view.explain
    val statsBefore = runtime.stats
    val first = apiValue(view.value)
    val repeated = apiValue(view.value)
    val materialized = apiValue(view.materialize)
    val after = runtime.explain(view)
    val execution = after.execution.getOrElse(fail("scenario execution trace missing"))
    val lineage = materialized.explain.lineage.lastOption.getOrElse(fail("scenario lineage missing"))
    val expectedSelected = Vector(100.0, 200.0, 104.0, 204.0)

    ScenarioResult(
      "spatial.lazy.affine-warp-surface.v1",
      Vector(
        SpatialScenario.fact(
          "description is pure",
          before.phase == FieldExplanationPhase.Descriptive && statsBefore == LazyRuntimeStats(0L, 0L, 0L, 0L, 0L),
          s"before=${before.phase} statsBefore=$statsBefore"
        ),
        SpatialScenario.vector("R pullback oracle", first.copyData.toVector, expectedSelected, 1e-12),
        SpatialScenario.vector("repeated evaluation", repeated.copyData.toVector, expectedSelected, 1e-12),
        SpatialScenario.vector("materialized values", materialized.data.materialized.get.copyData.toVector, expectedSelected, 1e-12),
        SpatialScenario.fact(
          "one spatial resampling",
          execution.fusion.valueResamplingPasses == 1,
          s"passes=${execution.fusion.valueResamplingPasses} stages=${execution.stages.map(_.semantics)}"
        ),
        SpatialScenario.fact(
          "root-first route",
          execution.normalizedRoute.map(_.morphism.value) ==
            Vector("native-anatomical", "anatomical-warped", "warped-surface"),
          s"route=${execution.normalizedRoute.map(_.morphism.value)}"
        ),
        SpatialScenario.fact(
          "narrow source support",
          execution.sourceSupport.sourceRows == Vector(0, 1, 2),
          s"support=${execution.sourceSupport.sourceRows}"
        ),
        SpatialScenario.fact(
          "result reuse",
          runtime.stats.executions == 1L && runtime.stats.resultCacheHits == 2L,
          s"stats=${runtime.stats}"
        ),
        SpatialScenario.fact(
          "materialization lineage",
          lineage.sourceRootId == field.rootId && lineage.execution.nonEmpty,
          s"source=${lineage.sourceRootId.value} execution=${lineage.execution.map(_.phase)}"
        )
      )
    )
