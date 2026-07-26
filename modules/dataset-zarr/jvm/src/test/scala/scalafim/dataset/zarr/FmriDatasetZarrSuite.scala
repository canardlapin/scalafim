package scalafim.dataset.zarr

import java.nio.file.Files
import scalafim.dataset.*
import scalafim.zarr.ReadLimits

class FmriDatasetZarrSuite extends munit.FunSuite:

  test("openZarr composes canonical storage into a source-blind FmriDataset") {
    val root = Files.createTempDirectory("scalafim-open-zarr")
    val fixture = NiftiRoundTripFixture.create(root)
    val limits = ReadLimits(maxObjects = 8)

    val dataset =
      FmriDataset
        .openZarr(
          fixture.revision,
          RunId("run-01"),
          ZarrDatasetOpenOptions(
            metadata = DatasetMetadata(Map("study" -> "demo")),
            readLimits = limits
          )
        )
        .fold(error => fail(error.message), identity)

    assertEquals(dataset.timeAxis.runIds.map(_.value), Vector("run-01"))
    assertEquals(dataset.samplingFrame.tr.map(_.value), Vector(1.5))
    assertEquals(dataset.samplingFrame.startTime.map(_.value), Vector(0.0))
    assertEquals(dataset.metadata.get("study"), Some("demo"))

    val provenance =
      dataset.metadata.provenance.collect:
        case value: NeuroArchiveZarrProvenance => value
      .getOrElse(fail("expected NeuroArchive Zarr provenance"))
    assertEquals(provenance.source, "neuroarchive-zarr")
    assertEquals(provenance.acquisitionId, fixture.imported.manifest.acquisitionId.value)
    assertEquals(provenance.payloadId, fixture.imported.manifest.payloadId.value)
    assertEquals(provenance.contentRevision, fixture.imported.publication.contentRevision.value)
    assertEquals(provenance.logicalPayloadHash, fixture.imported.publication.logicalPayloadHash.value)

    val series =
      dataset
        .seriesEither(
          DataSelection(
            TimepointSelection.indices(1, 0),
            VoxelSelection.indices(11, 0, 5)
          )
        )
        .fold(error => fail(error.message), identity)
    assertEquals(
      series.data.toRows,
      Vector(
        Vector(3.75, 1.0, 2.25),
        Vector(0.75, -2.0, -0.75)
      )
    )
    assertEquals(series.metadata.provenance, dataset.metadata.provenance)

    val source =
      dataset.backend match
        case backend: ResponseBlockDatasetBackend =>
          backend.source match
            case zarr: ZarrResponseBlockSource => zarr
            case other => fail(s"expected Zarr response source, found $other")
        case other =>
          fail(s"expected response-block dataset backend, found $other")
    assertEquals(source.readLimits, limits)
  }

  test("openZarr folds store opening failures into DatasetError") {
    val missing = Files.createTempDirectory("scalafim-open-zarr-missing").resolve("absent")

    val result = FmriDataset.openZarr(missing, RunId("run-01"))

    assert(result.left.exists:
      case DatasetError.StorageFailure(_) => true
      case _                              => false
    )
  }
