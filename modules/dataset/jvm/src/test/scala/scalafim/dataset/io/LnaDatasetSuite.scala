package scalafim.dataset.io

import scalafim.archive.io.{JhdfSharedBasisStore, LnaHdf5Store}
import scalafim.archive.lna.{LnaPipeline, QuantParams, SharedBasisArtifact, SharedBasisId, SharedBasisMask}
import scalafim.dataset.{DataSelection, IndexSelection}
import scalafim.image.{DMat, NeuroSpace}

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
          .findLnaFiles(LnaDatasetQuery(subject = "01", task = Some("rest"), space = Some("MNI")))
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

      assert(dataset.findLnaFiles(LnaDatasetQuery(subject = "../sub-01")).isLeft)
      assert(dataset.findLnaFiles(LnaDatasetQuery(subject = "sub-01", task = Some("../rest"))).isLeft)
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
        backend.read(
          DataSelection(
            time = IndexSelection.indices(0, 2),
            voxels = IndexSelection.indices(1, 3)
          )
        )

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
