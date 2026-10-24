package scalafim.fmri.model

import gale.linalg.Matrix
import neuroim4s.image.SampleSpaces
import scalafim.dataset.{DatasetEventRow, DatasetEvents, DatasetFieldId, DatasetId, DatasetValue, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.hrf.design.SamplingFrame

class ModelIdentityMigrationSuite extends munit.FunSuite:
  private def dataset(events: DatasetEvents): FmriDataset =
    FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        DatasetId("model-identity-migration"),
        Matrix.dense(4, 1, Vector(1.0, 3.0, 5.0, 7.0)),
        SampleSpaces(Vector(1, 1, 1))
      ),
      samplingFrame = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0)),
      events = events
    ).dataset

  private def decodedBuild(text: String): ModelBuildSpec =
    ModelBuildSpecJsonCodec.decode(text).fold(error => fail(error.message), identity)

  private def decodedDocument(text: String): ModelDocument =
    ModelDocumentJsonCodec.decode(text).fold(error => fail(error.message), identity)

  test("saved model-build v1 rebuilds ordinary source-event identity with the current fingerprint"):
    val v1 =
      """{"schema":"scalafim.model-build","version":1,"formula":"onset ~ hrf(condition)","blockColumn":null,"durationColumn":null,""" +
      """"baseline":{"kind":"constant"},"baselineDegree":1,"intercept":"Global","strategy":"ols","defaultHrf":"SPMG1",""" +
      """"precision":0.3,"dropEmpty":true,"summate":true,"strict":false,"contrastSets":{},"nuisance":null,"factorLevels":[],""" +
      """"emptyCellPolicy":"UseDropEmptyFlag","missingValuePolicy":{"kind":"zero-contribution"},"degenerateModulatorPolicy":"RetainAndReport","orthogonalization":[]}"""
    val spec = decodedBuild(v1)
    assertEquals(ModelBuildSpecJsonCodec.encode(spec), Right(v1))

    val events = DatasetEvents(
      Vector(
        Map("onset" -> "0", "condition" -> "face"),
        Map("onset" -> "1", "condition" -> "house"),
        Map("onset" -> "2", "condition" -> "face"),
        Map("onset" -> "3", "condition" -> "house")
      )
    )
    val model = FmriModelBuilder.buildModelEither(dataset(events), spec).fold(error => fail(error.message), identity)

    assertEquals(model.eventModel.designSchema.audit.sourceEvents.map(_.sourceRow), Vector(0, 1, 2, 3))
    assert(model.eventModel.designSchema.fingerprint.value.startsWith("design-schema/v3:"), model.eventModel.designSchema.fingerprint.value)

  test("saved model-document v1 preserves derived missing-row provenance in the current fingerprint"):
    val v1 =
      """{"schema":"scalafim.model-document","version":1,"formula":"onset ~ hrf(reward)",""" +
      """"derived":{"missingRows":"drop","columns":["reward: number = (gain * 2)"]},""" +
      """"build":{"blockColumn":null,"durationColumn":null,"baseline":{"kind":"constant"},"baselineDegree":1,"intercept":"Global",""" +
      """"strategy":{"kind":"ols","controls":{"volumeWeighting":{"kind":"disabled"},"nuisanceProjection":{"kind":"disabled"},"missingData":"Error"}},""" +
      """"defaultHrf":"SPMG1","precision":0.3,"dropEmpty":true,"summate":true,"strict":false,"contrastSets":{},"nuisance":null,"factorLevels":[],""" +
      """"emptyCellPolicy":"UseDropEmptyFlag","missingValuePolicy":{"kind":"zero-contribution"},"degenerateModulatorPolicy":"RetainAndReport","orthogonalization":[]},""" +
      """"confounds":null,"runCombination":{"type":"fixed-effects"}}"""
    val document = decodedDocument(v1)
    assertEquals(ModelDocumentJsonCodec.encode(document), Right(v1))

    val events = DatasetEvents.fromTypedRows(
      Vector(0.0, 1.0, 2.0, 3.0).zip(Vector(1.0, Double.NaN, 3.0, 4.0)).map: (onset, gain) =>
        DatasetEventRow.unsafe(
          Map(
            DatasetFieldId("onset") -> DatasetValue.Number(onset),
            DatasetFieldId("gain") -> DatasetValue.Number(gain)
          )
        )
    ).fold(error => fail(error.message), identity)
    val model = FmriModelBuilder.buildModelEither(dataset(events), document.build).fold(error => fail(error.message), identity)

    assertEquals(model.eventModel.designSchema.audit.sourceEvents.map(_.sourceRow), Vector(0, 2, 3))
    assert(model.eventModel.designSchema.fingerprint.value.startsWith("design-schema/v3:"), model.eventModel.designSchema.fingerprint.value)
