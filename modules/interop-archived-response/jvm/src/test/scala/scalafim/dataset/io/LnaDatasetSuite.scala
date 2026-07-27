package scalafim.dataset.io

import gale.linalg.{DMat as GaleDMat, DVec}
import scalafim.archive.RunLabel
import scalafim.archive.io.{JhdfSharedBasisStore, LnaHdf5Store}
import scalafim.archive.lna.{LnaPipeline, QuantParams, SharedBasisArtifact, SharedBasisId, SharedBasisMask}
import scalafim.dataset.{
  DataSelection,
  DatasetError,
  DatasetEvents,
  FmriDataset,
  GaleTestData,
  TimepointSelection,
  VoxelSelection
}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.{DMat, Mask, NeuroSpace}
import scalafim.latent.{
  BoldZipCoarseBasis,
  BoldZipDetailBasis,
  BoldZipPayload,
  BoldZipResidualEvent,
  BoldZipSpatialBasis,
  BoldZipTextureEntry,
  DctNorm,
  LegacyLatentArchiveCodec,
  LatentSelection,
  TransportLatentResponse
}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class LnaDatasetSuite extends munit.FunSuite:
  private val space = NeuroSpace(Vector(2, 2, 1))
  private val data =
    DMat.fromRows(
      Vector(
        Vector(0.0, 1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0, 7.0),
        Vector(8.0, 9.0, 10.0, 11.0)
      )
    )

  test("LnaDataset discovers subjects, metadata, participants, and shared basis registry") {
    withFixture { root =>
      val dataset = LnaDataset.open(root).fold(err => fail(err.message), identity)

      assertEquals(dataset.subjects.fold(err => fail(err.message), identity), Vector("sub-01", "sub-02"))

      val description =
        dataset
          .datasetDescription
          .fold(err => fail(err.message), identity)
          .getOrElse(fail("missing dataset description"))
      assertEquals(description.fields("Name").asString, Some("Demo LNA Derivative"))

      val participants =
        dataset
          .participants
          .fold(err => fail(err.message), identity)
          .getOrElse(fail("missing participants table"))
      assertEquals(participants.nrows, 2)
      assertEquals(participants.column("participant_id").fold(err => fail(err.message), identity).flatten, Vector("sub-01", "sub-02"))

      val registry =
        dataset
          .sharedBasisRegistry
          .fold(err => fail(err.message), identity)
      val id = SharedBasisId.unsafe("demo_basis")
      assert(registry.get(id).isDefined)
      assertEquals(registry.aliasesFor(registry.get(id).get.checksum).map(_.value), Vector("demo_basis"))
    }
  }

  test("LnaDataset filters LNA files by subject session task and space") {
    withFixture { root =>
      val dataset = LnaDataset.unsafe(root)

      val rest =
        dataset
          .findLnaFiles(
            LnaDatasetQuery.fromLabels(
              subject = LnaSubjectLabel.unsafe("01"),
              task = Some(LnaTaskLabel.unsafe("rest")),
              space = Some(LnaSpaceLabel.unsafe("MNI"))
            )
          )
          .fold(err => fail(err.message), identity)
      assertEquals(rest.length, 1)
      assert(rest.head.getFileName.toString.contains("task-rest"))

      val nback =
        dataset
          .findLnaFiles(LnaDatasetQuery(subject = "sub-01", session = Some("02"), task = Some("nback")))
          .fold(err => fail(err.message), identity)
      assertEquals(nback.length, 1)
      assert(nback.head.toString.contains("ses-02"))

      val none =
        dataset
          .findLnaFiles(LnaDatasetQuery(subject = "sub-01", task = Some("faces")))
          .fold(err => fail(err.message), identity)
      assertEquals(none, Vector.empty)

      assert(LnaDatasetQuery.fromStrings(subject = "../sub-01").isLeft)
      assert(LnaDatasetQuery.fromStrings(subject = "sub-01", task = Some("../rest")).isLeft)
      assert(LnaDatasetQuery.fromStrings(subject = "sub-01", run = Some("../01")).isLeft)
      assert(LnaDatasetQuery.fromStrings(subject = "sub-01", acq = Some("../hi")).isLeft)
      assert(LnaDatasetQuery.fromStrings(subject = "sub-01", desc = Some("../clean")).isLeft)
    }
  }

  test("LnaDataset matches parsed run acquisition and description entities exactly") {
    val root = Files.createTempDirectory("scalafim-lna-entity-query-")
    try
      writeArchive(root.resolve("sub-09/func/sub-09_task-rest_run-01_acq-hi_desc-denoised_space-MNI_bold.lna.h5"), data)
      writeArchive(root.resolve("sub-09/func/sub-09_task-rest_run-010_acq-hi_desc-denoised_space-MNI_bold.lna.h5"), data)
      writeArchive(root.resolve("sub-09/func/sub-09_task-rest_run-01_acq-high_desc-denoised_space-MNI_bold.lna.h5"), data)
      writeArchive(root.resolve("sub-09/func/sub-09_task-rest_run-01_acq-hi_desc-denoisedExtra_space-MNI_bold.lna.h5"), data)
      writeArchive(root.resolve("sub-09/func/sub-09_task-resting_run-01_acq-hi_desc-denoised_space-MNI_bold.lna.h5"), data)

      val dataset = LnaDataset.unsafe(root)
      val exact =
        dataset
          .findLnaFiles(
            LnaDatasetQuery(
              subject = "09",
              task = Some("rest"),
              space = Some("MNI"),
              run = Some("01"),
              acq = Some("hi"),
              desc = Some("denoised")
            )
          )
          .fold(err => fail(err.message), identity)

      assertEquals(exact.length, 1)
      assertEquals(exact.head.getFileName.toString, "sub-09_task-rest_run-01_acq-hi_desc-denoised_space-MNI_bold.lna.h5")

      val runOne =
        dataset
          .findLnaFiles(LnaDatasetQuery(subject = "09", task = Some("rest"), space = Some("MNI"), run = Some("1")))
          .fold(err => fail(err.message), identity)
      assertEquals(runOne, Vector.empty)

      val ambiguous =
        dataset.resolveLnaFile(LnaDatasetQuery(subject = "09", task = Some("rest"), space = Some("MNI"), run = Some("01")))

      ambiguous match
        case Left(LnaDatasetLookupError.Ambiguous(query, matches)) =>
          assertEquals(query.run.map(_.value), Some("01"))
          assertEquals(matches.length, 3)
        case other =>
          fail(s"expected ambiguous lookup, got $other")
    finally deleteTree(root)
  }

  test("LnaDataset resolves session run queries to real archive-backed datasets") {
    val root = Files.createTempDirectory("scalafim-lna-session-run-query-")
    try
      writeArchive(root.resolve("sub-10/ses-01/func/sub-10_ses-01_task-rest_run-01_space-MNI_bold.lna.h5"), data)
      writeArchive(root.resolve("sub-10/ses-01/func/sub-10_ses-01_task-rest_run-02_space-MNI_bold.lna.h5"), data)

      val dataset = LnaDataset.unsafe(root)
      val run2 =
        dataset
          .readSubject(
            LnaDatasetQuery(
              subject = "10",
              session = Some("01"),
              task = Some("rest"),
              space = Some("MNI"),
              run = Some("02")
            )
          )
          .fold(err => fail(err.message), identity)

      assertEquals(
        run2.metadata.get("lna.archive_relative_path"),
        Some("sub-10/ses-01/func/sub-10_ses-01_task-rest_run-02_space-MNI_bold.lna.h5")
      )
      assertEquals(run2.shape.timepoints, 3)

      val ambiguous =
        dataset.resolveLnaFile(LnaDatasetQuery(subject = "10", session = Some("01"), task = Some("rest"), space = Some("MNI")))
      ambiguous match
        case Left(LnaDatasetLookupError.Ambiguous(query, matches)) =>
          assertEquals(query.session.map(_.value), Some("ses-01"))
          assertEquals(matches.length, 2)
        case other =>
          fail(s"expected ambiguous lookup, got $other")
    finally deleteTree(root)
  }

  test("LnaDataset reports malformed LNA filename entities during discovery") {
    val root = Files.createTempDirectory("scalafim-lna-malformed-query-")
    try
      val malformed = root.resolve("sub-11/func/sub-11_task-rest_task-other_space-MNI_bold.lna.h5")
      Files.createDirectories(malformed.getParent)
      Files.writeString(malformed, "not an archive")

      val dataset = LnaDataset.unsafe(root)
      val failed =
        dataset
          .findLnaFiles(LnaDatasetQuery(subject = "11", task = Some("rest")))
          .left
          .toOption
          .getOrElse(fail("expected malformed filename error"))
      assert(failed.message.contains("duplicate entity 'task'"))

      dataset.resolveLnaFile(LnaDatasetQuery(subject = "11", task = Some("rest"))) match
        case Left(LnaDatasetLookupError.ScanFailed(error)) =>
          assert(error.message.contains("duplicate entity 'task'"))
        case other =>
          fail(s"expected scan failure, got $other")
    finally deleteTree(root)
  }

  test("LnaDataset loads one archive as a LatentArchiveDatasetBackend") {
    withFixture { root =>
      val dataset = LnaDataset.unsafe(root)
      val backend =
        dataset
          .readSubject(LnaDatasetQuery(subject = "sub-01", task = Some("rest"), space = Some("MNI")))
          .fold(err => fail(err.message), identity)

      assertEquals(backend.shape.timepoints, 3)
      assertEquals(backend.shape.spatialSize, 4)
      assertEquals(backend.metadata.get("lna.archive_relative_path"), Some("sub-01/func/sub-01_task-rest_space-MNI_bold.lna.h5"))

      val relativeBackend =
        dataset
          .backendFor(root.relativize(root.resolve("sub-01/func/sub-01_task-rest_space-MNI_bold.lna.h5")), backend.id)
          .fold(err => fail(err.message), identity)
      assertEquals(relativeBackend.shape, backend.shape)

      val series =
        backend.readEither(
          DataSelection(
            time = TimepointSelection.indices(0, 2),
            voxels = VoxelSelection.indices(1, 3)
          )
        ).fold(err => fail(err.message), identity)

      val expected = Vector(Vector(1.0, 3.0), Vector(9.0, 11.0))
      series.data.toRows.zip(expected).foreach { case (actualRow, expectedRow) =>
        actualRow.zip(expectedRow).foreach { case (actual, expectedValue) =>
          assert(math.abs(actual - expectedValue) < 2e-4)
        }
      }
    }
  }

  test("LnaDataset materializes shared-basis archives through the dataset root registry") {
    val root = Files.createTempDirectory("scalafim-lna-dataset-shared-basis-")
    try
      Files.writeString(root.resolve("dataset_description.json"), """{"Name":"Shared Basis LNA Derivative"}""")
      val basis =
        SharedBasisArtifact(
          loadings = DMat.eye(4),
          mask = SharedBasisMask(Vector(2, 2, 1), Vector(true, true, true, true)),
          kind = "identity",
          params = Map("source" -> "dataset-suite")
        )
      val basisId = SharedBasisId.unsafe("identity_basis")
      JhdfSharedBasisStore
        .writeContentAddressed(root.resolve("bases"), basis, basisId = Some(basisId), created = "2026-07-06T21:30:00Z")
        .fold(err => fail(err.message), identity)

      val archivePath = root.resolve("sub-03/func/sub-03_task-shared_space-MNI_bold.lna.h5")
      val archive =
        LnaPipeline
          .sharedBasisEmbedArchive(data, space, basis, basisId)
          .fold(err => fail(err.message), identity)
      Option(archivePath.getParent).foreach(Files.createDirectories(_))
      LnaHdf5Store.default.write(archivePath, archive).fold(err => fail(err.message), identity)

      val dataset = LnaDataset.unsafe(root)
      val backend =
        dataset
          .readSubjectMaterialized(LnaDatasetQuery(subject = "03", task = Some("shared"), space = Some("MNI")))
          .fold(err => fail(err.message), identity)

      assertEquals(backend.shape.timepoints, data.rows)
      assertEquals(backend.shape.spatialSize, data.cols)
      assertEquals(backend.data, data)
      assertEquals(backend.metadata.get("lna.archive_relative_path"), Some("sub-03/func/sub-03_task-shared_space-MNI_bold.lna.h5"))
    finally deleteTree(root)
  }

  test("LnaDataset reads temporal latent archives as selection-aware latent backends") {
    val root = Files.createTempDirectory("scalafim-lna-dataset-temporal-latent-")
    try
      Files.writeString(root.resolve("dataset_description.json"), """{"Name":"Temporal Latent LNA Derivative"}""")
      val archivePath = root.resolve("sub-04/func/sub-04_task-dct_space-MNI_bold.lna.h5")
      val archive =
        LegacyLatentArchiveCodec
          .toTemporalDctArchive(
            data = GaleTestData.matrixFromRows(data.toRows),
            space = space,
            components = data.rows,
            norm = DctNorm.Ortho,
            center = true
          )
          .fold(err => fail(err.message), identity)
      Option(archivePath.getParent).foreach(Files.createDirectories(_))
      LnaHdf5Store.default.write(archivePath, archive).fold(err => fail(err.message), identity)

      val dataset = LnaDataset.unsafe(root)
      val backend =
        dataset
          .readSubjectLatent(LnaDatasetQuery(subject = "04", task = Some("dct"), space = Some("MNI")))
          .fold(err => fail(err.message), identity)
      val series =
        backend.read(
          DataSelection(
            time = TimepointSelection.indices(2, 0),
            voxels = VoxelSelection.indices(3, 1)
          )
        )

      assertEquals(backend.shape.timepoints, data.rows)
      assertEquals(backend.shape.spatialSize, data.cols)
      assertRowsClose(series.data.toRows, Vector(Vector(11.0, 9.0), Vector(3.0, 1.0)), 1e-10)
    finally deleteTree(root)
  }

  test("LnaDataset reads transport latent archives as selection-aware latent backends") {
    val root = Files.createTempDirectory("scalafim-lna-dataset-transport-latent-")
    try
      Files.writeString(root.resolve("dataset_description.json"), """{"Name":"Transport Latent LNA Derivative"}""")
      val decoder =
        GaleTestData.csrFromTriplets(
          rows = 4,
          cols = 2,
          rowIndices = Array(0, 1, 2, 2, 3, 3),
          colIndices = Array(0, 1, 0, 1, 0, 1),
          values = Array(1.0, 1.0, 1.0, 1.0, 2.0, -1.0)
        )
      val response =
        TransportLatentResponse
          .withIdentityTransform(
            coefficientsAnalysis = GaleTestData.matrixFromRows(
              Vector(
                Vector(1.0, 2.0),
                Vector(3.0, 4.0)
              )
            ),
            nativeDecoder = decoder,
            offset = Some(DVec.fromSeq(Vector(10.0, 20.0, 30.0, 40.0))),
            label = "transport-dataset"
          )
          .fold(err => fail(err.message), identity)
      val archive =
        LegacyLatentArchiveCodec
          .toTransportArchive(response, space)
          .fold(err => fail(err.message), identity)
      val archivePath = root.resolve("sub-06/func/sub-06_task-transport_space-MNI_bold.lna.h5")
      Option(archivePath.getParent).foreach(Files.createDirectories(_))
      LnaHdf5Store.default.write(archivePath, archive).fold(err => fail(err.message), identity)

      val dataset = LnaDataset.unsafe(root)
      val backend =
        dataset
          .readSubjectLatent(LnaDatasetQuery(subject = "06", task = Some("transport"), space = Some("MNI")))
          .fold(err => fail(err.message), identity)
      val series =
        backend.read(
          DataSelection(
            time = TimepointSelection.indices(1, 0),
            voxels = VoxelSelection.indices(3, 1)
          )
        )
      val expected =
        response
          .reconstruct(LatentSelection(timepoints = Some(Vector(1, 0)), samples = Some(Vector(3, 1))))
          .fold(err => fail(err.message), identity)

      assertEquals(backend.shape.timepoints, 2)
      assertEquals(backend.shape.spatialSize, 4)
      assertEquals(backend.response.metadata("family"), "transport")
      assertRowsClose(series.data.toRows, GaleTestData.toRows(expected), 1e-12)
    finally deleteTree(root)
  }

  test("LnaDataset reads BOLDZip latent archives as selection-aware latent backends") {
    val root = Files.createTempDirectory("scalafim-lna-dataset-boldzip-latent-")
    val boldZipSpace = NeuroSpace(Vector(3, 1, 1))
    try
      Files.writeString(root.resolve("dataset_description.json"), """{"Name":"BOLDZip Latent LNA Derivative"}""")
      val spatialBasis =
        BoldZipSpatialBasis(
          sampleCount = 3,
          coarse = BoldZipCoarseBasis.MatrixBasis(GaleTestData.matrixFromRows(Vector(Vector(1.0), Vector(0.0), Vector(1.0)))),
          detail = BoldZipDetailBasis.IdentitySamples,
          label = "identity-detail"
        ).fold(err => fail(err.message), identity)
      val response =
        BoldZipPayload(
          temporalBasis = GaleDMat.eye(4),
          carrierTheta = GaleTestData.matrixFromRows(
            Vector(
              Vector(1.0, 2.0, 3.0, 4.0),
              Vector(10.0, 20.0, 30.0, 40.0)
            )
          ),
          carrierLoadings = GaleTestData.matrixFromRows(Vector(Vector(2.0, 1.0))),
          spatialBasis = spatialBasis,
          texture = Vector(
            BoldZipTextureEntry.unsafe(atom = 0, carrier = 0, amplitude = 0.5),
            BoldZipTextureEntry.unsafe(atom = 1, carrier = 1, amplitude = 1.0, lag = 1)
          ),
          events = Vector(BoldZipResidualEvent.unsafe(atom = 2, frame = 2, amplitude = 3.0)),
          offset = Some(DVec.fromSeq(Vector(10.0, 20.0, 30.0))),
          label = "boldzip-dataset"
        ).fold(err => fail(err.message), identity)
      val archive =
        LegacyLatentArchiveCodec
          .toBoldZipArchive(response, boldZipSpace)
          .fold(err => fail(err.message), identity)
      val archivePath = root.resolve("sub-07/func/sub-07_task-boldzip_space-MNI_bold.lna.h5")
      Option(archivePath.getParent).foreach(Files.createDirectories(_))
      LnaHdf5Store.default.write(archivePath, archive).fold(err => fail(err.message), identity)

      val dataset = LnaDataset.unsafe(root)
      val backend =
        dataset
          .readSubjectLatent(LnaDatasetQuery(subject = "07", task = Some("boldzip"), space = Some("MNI")))
          .fold(err => fail(err.message), identity)
      val series =
        backend.read(
          DataSelection(
            time = TimepointSelection.indices(3, 1),
            voxels = VoxelSelection.indices(2, 0)
          )
        )
      val expected =
        response
          .reconstruct(LatentSelection(timepoints = Some(Vector(3, 1)), samples = Some(Vector(2, 0))))
          .fold(err => fail(err.message), identity)

      assertEquals(backend.shape.timepoints, 4)
      assertEquals(backend.shape.spatialSize, 3)
      assertEquals(backend.response.metadata("family"), "boldzip_sr")
      assertRowsClose(series.data.toRows, GaleTestData.toRows(expected), 1e-12)
    finally deleteTree(root)
  }

  test("LnaDataset reads sparse shared-basis archives as mask-aware latent backends") {
    val root = Files.createTempDirectory("scalafim-lna-dataset-shared-sparse-")
    try
      Files.writeString(root.resolve("dataset_description.json"), """{"Name":"Sparse Shared Basis LNA Derivative"}""")
      val basis =
        SharedBasisArtifact(
          loadings = DMat.fromRows(
            Vector(
              Vector(1.0, 0.0),
              Vector(1.0, 1.0),
              Vector(0.0, 1.0)
            )
          ),
          mask = SharedBasisMask(Vector(2, 2, 1), Vector(true, false, true, true)),
          kind = "nonorthogonal",
          params = Map("source" -> "dataset-suite")
        )
      val basisId = SharedBasisId.unsafe("sparse_nonorthogonal_basis")
      JhdfSharedBasisStore
        .writeContentAddressed(root.resolve("bases"), basis, basisId = Some(basisId), created = "2026-07-06T23:00:00Z")
        .fold(err => fail(err.message), identity)

      val activeData =
        GaleTestData.matrixFromRows(
          Vector(
            Vector(11.0, 1.0, 7.0),
            Vector(13.0, 0.0, 4.0),
            Vector(8.0, -3.5, 5.5)
          )
        )
      val archive =
        LegacyLatentArchiveCodec
          .toSharedBasisArchive(
            data = activeData,
            space = space,
            basis = basis,
            basisId = basisId,
            center = true
          )
          .fold(err => fail(err.message), identity)
      val archivePath = root.resolve("sub-05/func/sub-05_task-shared_run-01_space-MNI_bold.lna.h5")
      Option(archivePath.getParent).foreach(Files.createDirectories(_))
      LnaHdf5Store.default.write(archivePath, archive).fold(err => fail(err.message), identity)

      val dataset = LnaDataset.unsafe(root)
      val backend =
        dataset
          .readSubjectLatent(LnaDatasetQuery(subject = "05", task = Some("shared"), space = Some("MNI")))
          .fold(err => fail(err.message), identity)
      val maskIndices = Mask.indices(backend.mask)
      val series =
        backend.read(
          DataSelection(
            time = TimepointSelection.indices(2, 0),
            voxels = VoxelSelection.indices(3, 0)
          )
        )

      assertEquals(Vector.tabulate(maskIndices.length)(maskIndices(_)), Vector(0, 2, 3))
      assertEquals(backend.shape.spatialSize, 4)
      assertEquals(backend.response.metadata("family"), "shared_basis")
      assertRowsClose(series.data.toRows, Vector(Vector(5.5, 8.0), Vector(7.0, 11.0)), 1e-10)
      interceptMessage[IllegalArgumentException](
        "voxel 1 is outside the readable sample mask"
      ) {
        backend.read(DataSelection(voxels = VoxelSelection.indices(1)))
      }

      val query =
        LnaDatasetQuery(
          subject = "05",
          task = Some("shared"),
          space = Some("MNI"),
          run = Some("01")
        )
      val timing =
        SamplingFrame
          .regular(tr = 1.0, nScans = activeData.rows)
          .fold(error => fail(error.message), identity)
      val opened =
        FmriDataset
          .openLna(root, query, timing)
          .fold(error => fail(error.message), identity)
      val provenance =
        opened.metadata.provenance.collect:
          case value: LnaDatasetProvenance => value
        .getOrElse(fail("expected LNA provenance"))
      assertEquals(provenance.codecFamily, "shared-basis")
      assertEquals(provenance.externalBasis, Some(basisId.value))
      assertEquals(provenance.readMode, LnaDatasetReadMode.SelectionAware)
    finally deleteTree(root)
  }

  test("LnaDataset reports ambiguous subject reads") {
    withFixture { root =>
      val dataset = LnaDataset.unsafe(root)
      val failed = dataset.readSubject(LnaDatasetQuery(subject = "sub-01"))
      assert(failed.isLeft)
      assert(failed.left.toOption.exists(_.message.contains("multiple LNA files match")))
    }
  }

  test("openLna requires timing, derives the external run, and preserves fallback provenance") {
    val root = Files.createTempDirectory("scalafim-open-lna-")
    try
      val archivePath =
        root.resolve("sub-12/func/sub-12_task-rest_run-01_space-MNI_bold.lna.h5")
      writeArchive(archivePath, data)
      val query =
        LnaDatasetQuery(
          subject = "12",
          task = Some("rest"),
          space = Some("MNI"),
          run = Some("01")
        )
      val timing =
        SamplingFrame
          .regular(tr = 0.8, nScans = data.rows)
          .fold(error => fail(error.message), identity)
      val dataset =
        FmriDataset
          .openLna(root, query, timing)
          .fold(error => fail(error.message), identity)

      assertEquals(dataset.timeAxis.runIds.map(_.value), Vector("run-01"))
      val provenance =
        dataset.metadata.provenance.collect:
          case value: LnaDatasetProvenance => value
        .getOrElse(fail("expected LNA provenance"))
      assertEquals(provenance.archivePath, "sub-12/func/sub-12_task-rest_run-01_space-MNI_bold.lna.h5")
      assertEquals(provenance.archiveRun.value, "run-01")
      assertEquals(provenance.codecFamily, "lna-pipeline")
      assertEquals(provenance.readMode, LnaDatasetReadMode.WholeRunFallback)

      val series =
        dataset
          .seriesEither(
            DataSelection(
              time = TimepointSelection.indices(2, 0),
              voxels = VoxelSelection.indices(3, 1)
            )
          )
          .fold(error => fail(error.message), identity)
      assertRowsClose(series.data.toRows, Vector(Vector(11.0, 9.0), Vector(3.0, 1.0)), 2e-4)
      assertEquals(series.metadata.provenance, dataset.metadata.provenance)

      val missingRun =
        FmriDataset.openLna(
          root,
          query,
          timing,
          RunLabel("not-present"),
          DatasetEvents.Empty
        )
      assert(missingRun.left.exists(_.message.contains("run 'not-present' not found")))
    finally deleteTree(root)
  }

  test("LNA internal run selection rejects ambiguity instead of choosing run zero") {
    val archive =
      LnaPipeline
        .quantArchive(data, space, params = QuantParams(bits = 16))
        .fold(err => fail(err.message), identity)
    val first = archive.manifest.runs.head
    val second = first.copy(label = RunLabel("run-02"))
    val multiRun = archive.copy(
      manifest = archive.manifest.copy(runs = Vector(first, second))
    )
    val query = LnaDatasetQuery(subject = "12", task = Some("rest"))

    val selected = selectArchiveRun(multiRun, requested = None, query)
    val exact = selectArchiveRun(multiRun, requested = Some(second.label), query)

    assert(selected.left.exists:
      case DatasetError.AmbiguousDatasetRun(_, 2) => true
      case _                                      => false
    )
    assertEquals(exact, Right(second.label))
  }

  private def withFixture[A](f: Path => A): A =
    val root = Files.createTempDirectory("scalafim-lna-dataset-")
    try
      Files.writeString(root.resolve("dataset_description.json"), """{"Name":"Demo LNA Derivative","BIDSVersion":"1.9.0"}""")
      Files.writeString(root.resolve("participants.tsv"), "participant_id\tage\nsub-01\t31\nsub-02\t29\n")
      writeBasis(root.resolve("bases"))
      writeArchive(root.resolve("sub-01/func/sub-01_task-rest_space-MNI_bold.lna.h5"), data)
      writeArchive(root.resolve("sub-01/ses-02/func/sub-01_ses-02_task-nback_space-T1w_bold.lna.h5"), data)
      writeArchive(root.resolve("sub-02/func/sub-02_task-rest_space-MNI_bold.lna.h5"), data)
      f(root)
    finally deleteTree(root)

  private def writeArchive(path: Path, matrix: DMat): Unit =
    val archive =
      LnaPipeline
        .quantArchive(matrix, space, params = QuantParams(bits = 16))
        .fold(err => fail(err.message), identity)
    Option(path.getParent).foreach(Files.createDirectories(_))
    LnaHdf5Store.default.write(path, archive).fold(err => fail(err.message), identity)

  private def writeBasis(dir: Path): Unit =
    val artifact =
      SharedBasisArtifact(
        loadings = DMat.fromRows(
          Vector(
            Vector(1.0, 0.0),
            Vector(0.5, 0.5),
            Vector(0.0, 1.0),
            Vector(-0.5, 0.25)
          )
        ),
        mask = SharedBasisMask(Vector(2, 2, 1), Vector(true, true, true, true)),
        kind = "demo",
        params = Map("source" -> "fixture")
      )
    JhdfSharedBasisStore
      .writeContentAddressed(dir, artifact, basisId = Some(SharedBasisId.unsafe("demo_basis")), created = "2026-07-06T19:45:00Z")
      .fold(err => fail(err.message), identity)

  private def deleteTree(path: Path): Unit =
    if Files.exists(path) then
      val files = Files.walk(path)
      try files.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists)
      finally files.close()

  private def assertRowsClose(
      actual: Vector[Vector[Double]],
      expected: Vector[Vector[Double]],
      tol: Double
  ): Unit =
    assertEquals(actual.length, expected.length)
    assertEquals(if actual.isEmpty then 0 else actual.head.length, if expected.isEmpty then 0 else expected.head.length)
    actual.zip(expected).foreach { case (actualRow, expectedRow) =>
      actualRow.zip(expectedRow).foreach { case (actualValue, expectedValue) =>
        assertEqualsDouble(actualValue, expectedValue, tol)
      }
    }
