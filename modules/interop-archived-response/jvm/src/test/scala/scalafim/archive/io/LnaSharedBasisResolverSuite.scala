package scalafim.archive.io

import scalafim.image.SampleSpaces

import scalafim.archive.lna.*
import gale.linalg.DMat
import scalafim.archive.lna.GaleArchiveTestData
import scalafim.image.SomeSampleSpace

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class LnaSharedBasisResolverSuite extends munit.FunSuite:
  private val space = SampleSpaces(Vector(2, 2, 1))
  private val data =
    GaleArchiveTestData.matrixFromRows(
      Vector(
        Vector(0.0, 1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0, 7.0),
        Vector(8.0, 9.0, 10.0, 11.0)
      )
    )

  private val basisId = SharedBasisId.unsafe("identity_basis")

  private val identityBasis =
    SharedBasisArtifact(
      loadings = DMat.eye(4),
      mask = SharedBasisMask(Vector(2, 2, 1), Vector(true, true, true, true)),
      kind = "identity",
      params = Map("source" -> "resolver-suite")
    )

  private val nonorthogonalSpace = SampleSpaces(Vector(3, 1, 1))
  private val nonorthogonalBasisId = SharedBasisId.unsafe("nonorthogonal_basis")
  private val nonorthogonalBasis =
    SharedBasisArtifact(
      loadings =
        GaleArchiveTestData.matrixFromRows(
          Vector(
            Vector(1.0, 0.0),
            Vector(1.0, 1.0),
            Vector(0.0, 1.0)
          )
        ),
      mask = SharedBasisMask(Vector(3), Vector(true, true, true)),
      kind = "nonorthogonal",
      params = Map("source" -> "resolver-suite")
    )

  test("resolves a shared basis through the dataset root registry and reconstructs") {
    val root = Files.createTempDirectory("scalafim-lna-resolver-registry-")
    try
      JhdfSharedBasisStore
        .writeContentAddressed(root.resolve("bases"), identityBasis, basisId = Some(basisId), created = "2026-07-06T21:10:00Z")
        .fold(err => fail(err.message), identity)
      val archivePath = root.resolve("sub-01/func/sub-01_task-shared_space-MNI_bold.lna.h5")
      writeSharedBasisArchive(archivePath, identityBasis, locator = None)

      val reconstructed =
        LnaSharedBasisResolver
          .readAndReconstruct(archivePath, datasetRoot = Some(root))
          .fold(err => fail(err.message), identity)

      assertEquals(GaleArchiveTestData.toRows(reconstructed), GaleArchiveTestData.toRows(data))
    finally deleteTree(root)
  }

  test("resolves an archive-relative shared basis locator") {
    val root = Files.createTempDirectory("scalafim-lna-resolver-locator-")
    try
      val archivePath = root.resolve("sub-01/func/sub-01_task-shared_space-MNI_bold.lna.h5")
      val basisDir = archivePath.getParent.resolve("bases")
      val basisPath = basisDir.resolve(identityBasis.contentAddressedFilename)
      JhdfSharedBasisStore.write(basisPath, identityBasis).fold(err => fail(err.message), identity)

      writeSharedBasisArchive(
        archivePath,
        identityBasis,
        locator = Some(SharedBasisLocator.unsafe(s"bases/${identityBasis.contentAddressedFilename}"))
      )

      val reconstructed =
        LnaSharedBasisResolver
          .readAndReconstruct(archivePath)
          .fold(err => fail(err.message), identity)

      assertEquals(GaleArchiveTestData.toRows(reconstructed), GaleArchiveTestData.toRows(data))
    finally deleteTree(root)
  }

  test("fails when a registry alias has the wrong checksum") {
    val root = Files.createTempDirectory("scalafim-lna-resolver-wrong-checksum-")
    try
      val staleBasis =
        identityBasis.copy(kind = "stale-identity")
      JhdfSharedBasisStore
        .writeContentAddressed(root.resolve("bases"), staleBasis, basisId = Some(basisId), created = "2026-07-06T21:15:00Z")
        .fold(err => fail(err.message), identity)
      val archivePath = root.resolve("sub-01/func/sub-01_task-shared_space-MNI_bold.lna.h5")
      writeSharedBasisArchive(archivePath, identityBasis, locator = None)

      val failed =
        LnaSharedBasisResolver.readAndReconstruct(archivePath, datasetRoot = Some(root))

      assert(failed.isLeft)
      assert(failed.left.toOption.exists(_.message.contains("has checksum")))
    finally deleteTree(root)
  }

  test("fails clearly when no shared basis artifact can be found") {
    val root = Files.createTempDirectory("scalafim-lna-resolver-missing-")
    try
      val archivePath = root.resolve("sub-01/func/sub-01_task-shared_space-MNI_bold.lna.h5")
      writeSharedBasisArchive(archivePath, identityBasis, locator = None)

      val failed =
        LnaSharedBasisResolver.readAndReconstruct(archivePath, datasetRoot = Some(root))

      assert(failed.isLeft)
      assert(failed.left.toOption.exists(_.message.contains("was not found")))
    finally deleteTree(root)
  }

  test("adds stored voxel offsets when reconstructing shared-basis coefficients") {
    val root = Files.createTempDirectory("scalafim-lna-resolver-offset-")
    try
      JhdfSharedBasisStore
        .writeContentAddressed(root.resolve("bases"), nonorthogonalBasis, basisId = Some(nonorthogonalBasisId), created = "2026-07-06T22:30:00Z")
        .fold(err => fail(err.message), identity)
      val coefficients =
        GaleArchiveTestData.matrixFromRows(
          Vector(
            Vector(1.0, 2.0),
            Vector(3.0, -1.0),
            Vector(-2.0, 0.5)
          )
        )
      val offset = Vector(10.0, -2.0, 5.0)
      val expected =
        GaleArchiveTestData.matrixFromRows(
          Vector.tabulate(coefficients.rows) { row =>
            Vector.tabulate(nonorthogonalBasis.nVoxels) { voxel =>
              var sum = offset(voxel)
              var atom = 0
              while atom < nonorthogonalBasis.nAtoms do
                sum += coefficients(row, atom) * nonorthogonalBasis.loadings(voxel, atom)
                atom += 1
              sum
            }
          }
        )
      val archive =
        LnaPipeline
          .sharedBasisEmbedArchiveFromCoefficients(
            coefficients = coefficients,
            space = nonorthogonalSpace,
            basis = nonorthogonalBasis,
            basisId = nonorthogonalBasisId,
            offset = Some(offset)
          )
          .fold(err => fail(err.message), identity)
      val archivePath = root.resolve("sub-01/func/sub-01_task-shared-offset_space-MNI_bold.lna.h5")
      Option(archivePath.getParent).foreach(Files.createDirectories(_))
      LnaHdf5Store.default.write(archivePath, archive).fold(err => fail(err.message), identity)

      val reconstructed =
        LnaSharedBasisResolver
          .readAndReconstruct(archivePath, datasetRoot = Some(root))
          .fold(err => fail(err.message), identity)

      assertEquals(GaleArchiveTestData.toRows(reconstructed), GaleArchiveTestData.toRows(expected))
    finally deleteTree(root)
  }

  test("expands sparse shared-basis masks when materializing coefficients") {
    val root = Files.createTempDirectory("scalafim-lna-resolver-sparse-mask-")
    try
      val sparseBasis =
        nonorthogonalBasis.copy(
          mask = SharedBasisMask(Vector(2, 2, 1), Vector(true, false, true, true))
        )
      JhdfSharedBasisStore
        .writeContentAddressed(root.resolve("bases"), sparseBasis, basisId = Some(nonorthogonalBasisId), created = "2026-07-06T22:45:00Z")
        .fold(err => fail(err.message), identity)
      val coefficients =
        GaleArchiveTestData.matrixFromRows(
          Vector(
            Vector(1.0, 2.0),
            Vector(3.0, -1.0)
          )
        )
      val offset = Vector(10.0, -2.0, 5.0)
      val archive =
        LnaPipeline
          .sharedBasisEmbedArchiveFromCoefficients(
            coefficients = coefficients,
            space = space,
            basis = sparseBasis,
            basisId = nonorthogonalBasisId,
            offset = Some(offset)
          )
          .fold(err => fail(err.message), identity)
      val archivePath = root.resolve("sub-01/func/sub-01_task-shared-sparse_space-MNI_bold.lna.h5")
      Option(archivePath.getParent).foreach(Files.createDirectories(_))
      LnaHdf5Store.default.write(archivePath, archive).fold(err => fail(err.message), identity)

      val reconstructed =
        LnaSharedBasisResolver
          .readAndReconstruct(archivePath, datasetRoot = Some(root))
          .fold(err => fail(err.message), identity)

      assertEquals(
        GaleArchiveTestData.toRows(reconstructed),
        Vector(
          Vector(11.0, 0.0, 1.0, 7.0),
          Vector(13.0, 0.0, 0.0, 4.0)
        )
      )
    finally deleteTree(root)
  }

  private def writeSharedBasisArchive(
      path: Path,
      basis: SharedBasisArtifact,
      locator: Option[SharedBasisLocator]
  ): Unit =
    val archive =
      LnaPipeline
        .sharedBasisEmbedArchive(data, space, basis, basisId, locator = locator)
        .fold(err => fail(err.message), identity)
    Option(path.getParent).foreach(Files.createDirectories(_))
    LnaHdf5Store.default.write(path, archive).fold(err => fail(err.message), identity)

  private def deleteTree(path: Path): Unit =
    if Files.exists(path) then
      val files = Files.walk(path)
      try files.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists)
      finally files.close()
