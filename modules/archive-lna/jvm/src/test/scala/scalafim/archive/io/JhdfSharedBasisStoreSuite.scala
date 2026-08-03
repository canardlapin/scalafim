package scalafim.archive.io

import io.jhdf.HdfFile
import scalafim.archive.lna.*
import scalafim.image.DMat

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class JhdfSharedBasisStoreSuite extends munit.FunSuite:
  private val mask =
    SharedBasisMask(
      dims = Vector(2, 2, 1),
      values = Vector(true, true, true, true)
    )

  private val loadings =
    DMat.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.5, 0.5),
        Vector(0.0, 1.0),
        Vector(-0.5, 0.25)
      )
    )

  private val artifact =
    SharedBasisArtifact(
      loadings = loadings,
      mask = mask,
      kind = "slepian",
      params = Map("space" -> "MNI152NLin2009cAsym", "radius" -> "8")
    )

  test("jHDF shared basis store writes content-addressed artifacts and registry aliases") {
    val dir = Files.createTempDirectory("scalafim-shared-basis-")
    try
      val id = SharedBasisId.unsafe("schaefer400_slepian-k8")
      val result =
        JhdfSharedBasisStore
          .writeContentAddressed(dir, artifact, basisId = Some(id), created = "2026-07-06T19:30:00Z")
          .fold(err => fail(err.message), identity)

      assertEquals(result.file.getFileName.toString, artifact.contentAddressedFilename)
      assert(Files.exists(result.file))
      assert(Files.exists(dir.resolve("registry.json")))

      val loaded =
        JhdfSharedBasisStore
          .read(result.file)
          .fold(err => fail(err.message), identity)

      assertEquals(loaded.loadings, artifact.loadings)
      assertEquals(loaded.mask, artifact.mask)
      assertEquals(loaded.kind, artifact.kind)
      assertEquals(loaded.params, artifact.params)
      assertEquals(loaded.created, Some("2026-07-06T19:30:00Z"))
      assertEquals(loaded.checksum, artifact.checksum)

      val registry =
        JhdfSharedBasisStore
          .readRegistry(dir.resolve("registry.json"))
          .fold(err => fail(err.message), identity)
      val entry = registry.get(id).getOrElse(fail("missing registry entry"))
      assertEquals(entry.checksum, artifact.checksum)
      assertEquals(entry.filename, artifact.contentAddressedFilename)
      assertEquals(entry.kind, "slepian")
      assertEquals(entry.nAtoms, 2)
      assertEquals(entry.nVoxels, 4)

      val hdf = new HdfFile(result.file)
      try
        val meta = hdf.getByPath("/meta")
        assertEquals(meta.getAttribute("basis_kind").getData, "slepian")
        assertEquals(meta.getAttribute("checksum").getData, artifact.checksum.value)
        assertEquals(hdf.getDatasetByPath("/loadings").getDimensions().toVector, Vector(4, 2))
        assertEquals(hdf.getDatasetByPath("/mask").getDimensions().toVector, Vector(2, 2, 1))
      finally hdf.close()
    finally deleteTree(dir)
  }

  test("jHDF shared basis store rejects checksum mismatches") {
    val file = Files.createTempFile("scalafim-shared-basis-corrupt-", ".lna_basis.h5")
    try
      writeCorruptFixture(file, artifact)
      val failed = JhdfSharedBasisStore.read(file)
      assert(failed.isLeft)
      assert(failed.left.toOption.exists(_.message.contains("checksum mismatch")))
    finally Files.deleteIfExists(file)
  }

  test("jHDF shared basis direct write preserves absent created timestamp") {
    val file = Files.createTempFile("scalafim-shared-basis-direct-", ".lna_basis.h5")
    try
      JhdfSharedBasisStore.write(file, artifact).fold(err => fail(err.message), identity)
      val loaded = JhdfSharedBasisStore.read(file).fold(err => fail(err.message), identity)
      assertEquals(loaded.created, None)
      assertEquals(loaded.checksum, artifact.checksum)
    finally Files.deleteIfExists(file)
  }

  private def writeCorruptFixture(path: Path, source: SharedBasisArtifact): Unit =
    val changed =
      DMat.fromRows(
        Vector(
          Vector(2.0, 0.0),
          Vector(0.5, 0.5),
          Vector(0.0, 1.0),
          Vector(-0.5, 0.25)
        )
      )
    val hdf = HdfFile.write(path)
    try
      hdf.putAttribute("scalafim_lna_basis_storage", "scalafim-lna-basis-hdf5-0")
      val meta = hdf.putGroup("meta")
      meta.putAttribute("basis_kind", source.kind)
      meta.putAttribute("checksum", source.checksum.value)
      meta.putAttribute("checksum_algorithm", SharedBasisArtifact.ChecksumAlgorithm)
      meta.putAttribute("mask_checksum", source.maskChecksum.value)
      meta.putAttribute("mask_checksum_algorithm", SharedBasisArtifact.MaskChecksumAlgorithm)
      meta.putAttribute("n_atoms", source.nAtoms)
      meta.putAttribute("n_voxels", source.nVoxels)
      meta.putAttribute("created", "2026-07-06T19:30:00Z")
      meta.putAttribute("loadings_storage", "dense")
      hdf.putDataset("loadings", Array.tabulate(changed.rows, changed.cols)((r, c) => changed(r, c)))
      hdf.putDataset("mask", Array.tabulate(2, 2, 1)((i, j, k) => if source.mask.values((i * 2 + j) + k) then 1 else 0))
      hdf.putDataset("params", SharedBasisParamsCodec.render(source.params))
    finally hdf.close()

  private def deleteTree(path: Path): Unit =
    if Files.exists(path) then
      val files = Files.walk(path)
      try files.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists)
      finally files.close()
