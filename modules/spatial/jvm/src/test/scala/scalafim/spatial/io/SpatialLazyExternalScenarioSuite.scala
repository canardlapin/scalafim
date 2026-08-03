package scalafim.spatial.io

import scalafim.image.io.Nifti
import scalafim.image.{Axis, DMat, PrimitiveBuffers, NeuroSpace, NeuroVec}
import scalafim.spatial.*

import java.nio.file.{Files, Path}

class SpatialLazyExternalScenarioSuite extends munit.FunSuite:

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def apiValue[A](result: Either[FieldApiError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def volumeDomain(name: String, space: NeuroSpace): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-external"))
    val modality = spatialValue(Modality(name))
    val geometry = spatialValue(SamplingGeometry.volume(space))
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def affine(name: String, source: Domain, target: Domain, x: Double): Morphism =
    val matrix =
      DMat.fromRows(
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
        coordinateMap = spatialValue(CoordinateMap.affine3D(matrix))
      )
    )

  private def withNiftiPath[A](body: Path => A): A =
    val directory = Files.createTempDirectory("scalafim-spatial-external-")
    val path = directory.resolve("bold.nii")
    try body(path)
    finally
      Files.deleteIfExists(path)
      Files.deleteIfExists(directory)

  test("external NIfTI pull-through scenario returns one clean verdict"):
    val result = withNiftiPath(runScenario)
    assert(result.ciPass, result.render)
    assertEquals(result.status, ScenarioStatus.Pass)

  private def runScenario(path: Path): ScenarioResult =
    val space = NeuroSpace(Vector(6, 1, 1), trans = Some(DMat.eye(4)))
    val root = volumeDomain("native", space)
    val target = volumeDomain("target", space)
    val source = spatialValue(NiftiFieldSource.prepare(path, root, observations = 3, label = "bold"))
    val field = spatialValue(Field.fromSource(root, source))
    given SpatialGraph = spatialValue(
      SpatialGraph.build(Vector(root, target), Vector(affine("native-target", root, target, 1.0)))
    )
    val view = apiValue(field.to(target).flatMap(_.rows(0, 2)).flatMap(_.timeBlock(1, 2)))
    val before = view.explain
    val sourceBefore = source.stats

    val values =
      PrimitiveBuffers.fromArray(
        Array(
          0.0, 1.0, 2.0, 3.0, 4.0, 5.0,
          10.0, 11.0, 12.0, 13.0, 14.0, 15.0,
          20.0, 21.0, 22.0, 23.0, 24.0, 25.0
        )
      )
    Nifti.writeVec(path, NeuroVec.fromLinear(values, space.addDim(3, Some(Axis.Time)), "bold"))

    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    given FieldRuntime = runtime
    val first = apiValue(view.value)
    val repeated = apiValue(view.value)
    val materialized = apiValue(view.materialize)
    val after = runtime.explain(view)
    val execution = after.execution.getOrElse(fail("external scenario execution trace missing"))
    val stats = source.stats
    val expected = Vector(11.0, 21.0, 13.0, 23.0)
    val fullReadBytes = root.nElements.toLong * 3L * java.lang.Double.BYTES.toLong

    ScenarioResult(
      "spatial.lazy.external-nifti.v1",
      Vector(
        SpatialScenario.fact(
          "construction and explanation perform no IO",
          before.phase == FieldExplanationPhase.Descriptive && sourceBefore == NiftiFieldSourceStats(0L, 0L, 0L, 0L, 0L, 0L),
          s"phase=${before.phase} sourceStats=$sourceBefore"
        ),
        SpatialScenario.vector("external values", first.copyData.toVector, expected, 1e-12),
        SpatialScenario.vector("external repeat", repeated.copyData.toVector, expected, 1e-12),
        SpatialScenario.vector(
          "external materialization",
          materialized.data.materialized.get.copyData.toVector,
          expected,
          1e-12
        ),
        SpatialScenario.fact(
          "exact source support",
          execution.sourceSupport.sourceRows == Vector(1, 3) && execution.sourceSupport.precision == SupportPrecision.Exact,
          s"support=${execution.sourceSupport}"
        ),
        SpatialScenario.fact(
          "narrow physical read",
          stats.readCalls == 1L && stats.bytesRead == 32L && stats.bytesRead < fullReadBytes,
          s"bytesRead=${stats.bytesRead} fullReadBytes=$fullReadBytes readCalls=${stats.readCalls}"
        ),
        SpatialScenario.fact(
          "compiled and executed once",
          runtime.stats.compilations == 1L && runtime.stats.executions == 1L && runtime.stats.resultCacheHits == 2L,
          s"runtimeStats=${runtime.stats}"
        ),
        SpatialScenario.fact(
          "channel closes after demand",
          Files.deleteIfExists(path),
          s"path=$path"
        )
      )
    )
