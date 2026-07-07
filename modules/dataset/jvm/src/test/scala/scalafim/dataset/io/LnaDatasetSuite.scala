package scalafim.dataset.io

import scalafim.archive.io.{JhdfSharedBasisStore, LnaHdf5Store}
import scalafim.archive.lna.{LnaPipeline, QuantParams, SharedBasisArtifact, SharedBasisId, SharedBasisMask}
import scalafim.dataset.{DataSelection, TimepointSelection, VoxelSelection}
import scalafim.image.{DMat, Mask, NeuroSpace}
import scalafim.latent.{
  BoldZipCoarseBasis,
  BoldZipDetailBasis,
  BoldZipPayload,
  BoldZipResidualEvent,
  BoldZipSpatialBasis,
  BoldZipTextureEntry,
  DctNorm,
  LatentArchiveCodec,
  LatentSelection,
  TransportLatentResponse
}
import scalafim.linalg.{CsrMatrix, DoubleMatrix, DoubleVector, LinearMapError}

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
    }
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
        LatentArchiveCodec
          .toTemporalDctArchive(
            data = DoubleMatrix.fromRows(data.toRows),
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
        mapValue(
          CsrMatrix.fromTriplets(
            rows = 4,
            cols = 2,
            rowIndices = Array(0, 1, 2, 2, 3, 3),
            colIndices = Array(0, 1, 0, 1, 0, 1),
            values = Array(1.0, 1.0, 1.0, 1.0, 2.0, -1.0)
          )
        )
      val response =
        TransportLatentResponse
          .withIdentityTransform(
            coefficientsAnalysis = DoubleMatrix.fromRows(
              Vector(
                Vector(1.0, 2.0),
                Vector(3.0, 4.0)
              )
            ),
            nativeDecoder = decoder,
            offset = Some(DoubleVector.fromSeq(Vector(10.0, 20.0, 30.0, 40.0))),
            label = "transport-dataset"
          )
          .fold(err => fail(err.message), identity)
      val archive =
        LatentArchiveCodec
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
      assertRowsClose(series.data.toRows, expected.toRows, 1e-12)
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
          coarse = BoldZipCoarseBasis.MatrixBasis(DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(0.0), Vector(1.0)))),
          detail = BoldZipDetailBasis.IdentitySamples,
          label = "identity-detail"
        ).fold(err => fail(err.message), identity)
      val response =
        BoldZipPayload(
          temporalBasis = DoubleMatrix.eye(4),
          carrierTheta = DoubleMatrix.fromRows(
            Vector(
              Vector(1.0, 2.0, 3.0, 4.0),
              Vector(10.0, 20.0, 30.0, 40.0)
            )
          ),
          carrierLoadings = DoubleMatrix.fromRows(Vector(Vector(2.0, 1.0))),
          spatialBasis = spatialBasis,
          texture = Vector(
            BoldZipTextureEntry.unsafe(atom = 0, carrier = 0, amplitude = 0.5),
            BoldZipTextureEntry.unsafe(atom = 1, carrier = 1, amplitude = 1.0, lag = 1)
          ),
          events = Vector(BoldZipResidualEvent.unsafe(atom = 2, frame = 2, amplitude = 3.0)),
          offset = Some(DoubleVector.fromSeq(Vector(10.0, 20.0, 30.0))),
          label = "boldzip-dataset"
        ).fold(err => fail(err.message), identity)
      val archive =
        LatentArchiveCodec
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
      assertRowsClose(series.data.toRows, expected.toRows, 1e-12)
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
        DoubleMatrix.fromRows(
          Vector(
            Vector(11.0, 1.0, 7.0),
            Vector(13.0, 0.0, 4.0),
            Vector(8.0, -3.5, 5.5)
          )
        )
      val archive =
        LatentArchiveCodec
          .toSharedBasisArchive(
            data = activeData,
            space = space,
            basis = basis,
            basisId = basisId,
            center = true
          )
          .fold(err => fail(err.message), identity)
      val archivePath = root.resolve("sub-05/func/sub-05_task-shared_space-MNI_bold.lna.h5")
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
      interceptMessage[IllegalArgumentException]("voxel 1 is outside the latent mask") {
        backend.read(DataSelection(voxels = VoxelSelection.indices(1)))
      }
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

  private def mapValue[A](result: Either[LinearMapError, A]): A =
    result match
      case Right(value) => value
      case Left(error)  => fail(error.message)
