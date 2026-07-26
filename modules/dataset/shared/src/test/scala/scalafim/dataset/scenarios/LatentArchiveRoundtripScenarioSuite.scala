package scalafim.dataset.scenarios

import scalafim.archive.RunLabel
import scalafim.dataset.*
import scalafim.image.NeuroSpace
import scalafim.latent.{DctNorm, LatentArchiveCodec}

class LatentArchiveRoundtripScenarioSuite extends munit.FunSuite:
  test("dataset latent archive roundtrip scenario receipt passes") {
    val result = runScenario()
    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val scenarioId = "dataset.latent-archive-roundtrip.v1"
    val rows =
      Vector(
        Vector(1.0, 2.0, 3.0, 4.0),
        Vector(2.0, 3.0, 5.0, 7.0),
        Vector(3.0, 5.0, 8.0, 11.0),
        Vector(5.0, 8.0, 13.0, 17.0)
      )
    val space = NeuroSpace(Vector(2, 2, 1))
    val archive =
      LatentArchiveCodec
        .toTemporalDctArchive(
          data = GaleTestData.matrixFromRows(rows),
          space = space,
          components = rows.length,
          norm = DctNorm.Ortho,
          center = true,
          ridge = 0.0,
          metadata = Map("scenario" -> scenarioId)
        )
        .fold(err => fail(err.message), identity)
    val backend =
      LatentArchiveDatasetBackend
        .make(
          id = DatasetId("scenario-latent-archive"),
          archive = archive,
          run = RunLabel.indexed(0),
          metadata = DatasetMetadata(Map("scenario" -> scenarioId, "storage" -> "lna-temporal-dct"))
        )
        .fold(err => fail(err.message), identity)
    val selection =
      DataSelection(
        time = TimepointSelection.indices(3, 1),
        voxels = VoxelSelection.indices(2, 0)
      )
    val series =
      backend
        .readEither(selection)
        .fold(err => fail(err.message), identity)
    val expected =
      Vector(
        Vector(rows(3)(2), rows(3)(0)),
        Vector(rows(1)(2), rows(1)(0))
      )

    ScenarioHarness.result(
      scenarioId,
      Vector(
        ScenarioHarness.fact("series.timepoints", series.timepoints == Vector(3, 1), s"actual=${series.timepoints}"),
        ScenarioHarness.fact("series.voxels", series.voxelIndices == Vector(2, 0), s"actual=${series.voxelIndices}"),
        ScenarioHarness.fact("shape.timepoints", backend.shape.timepoints == rows.length, s"actual=${backend.shape.timepoints} expected=${rows.length}"),
        ScenarioHarness.fact("shape.spatial_dims", backend.shape.spatialDims == Vector(2, 2, 1), s"actual=${backend.shape.spatialDims}"),
        ScenarioHarness.fact("metadata.scenario", series.metadata.get("scenario").contains(scenarioId), s"metadata=${series.metadata.values}"),
        ScenarioHarness.finite("series.values", series.data.data.toIndexedSeq)
      ) ++
        ScenarioHarness.matrix("series.selected", series.data, expected, ScenarioTolerance.absolute(1e-10))
    )
