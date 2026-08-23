package scalafim.dataset.zarr

import java.nio.file.Files
import scalafim.archive.zarr.{AcquisitionTiming, CanonicalChunkProfile, NeuroArchiveZarr, TimeUnits}
import bids4s.io.BidsProjectLoader
import scalafim.dataset.*
import scalafim.fmri.fit.{FitChunkPlan, FitChunkingStrategy}
import scalafim.image.io.Nifti
import zarr4s.{IndexLocation, JvmCodecRuntime, JvmFileStore, JvmGzip, PhysicalLayout, Shape}

class NiftiZarrBridgeSuite extends munit.FunSuite:
  private def assertRegularSeconds(
      timing: AcquisitionTiming,
      expectedStep: Double,
      expectedCount: Long
  ): Unit = timing match
    case AcquisitionTiming.Regular(origin, step, count, TimeUnits.Second) =>
      assertEqualsDouble(origin, 0.0, 0.0)
      assertEqualsDouble(step, expectedStep, 1e-12)
      assertEquals(count, expectedCount)
    case found => fail(s"expected regular timing in seconds, found $found")

  test("raw int16 NIfTI imports, serves ordered calibrated blocks, and exports BIDS"):
    val root = Files.createTempDirectory("scalafim-nifti-zarr-roundtrip")
    val fixture = NiftiRoundTripFixture.create(root)
    assertEquals(fixture.imported.descriptor.dataType.name, "int16")
    assertEquals(fixture.imported.manifest.calibration.scale, 0.25)
    assertEquals(fixture.imported.manifest.calibration.offset, -2.0)
    assertEquals(fixture.imported.manifest.timing.sampleCount, 2L)
    assertRegularSeconds(fixture.imported.manifest.timing, 1.5, 2L)

    val source = ZarrResponseBlockSource.open(fixture.opened)
      .fold(error => fail(error.message), identity)
    val block = source.readBlock(DataSelection(
      TimepointSelection.indices(1, 0),
      VoxelSelection.indices(11, 0, 5)
    )).fold(error => fail(error.message), identity)
    assertEquals(block.timepoints, Vector(1, 0))
    assertEquals(block.voxelIndices, Vector(11, 0, 5))
    assertEquals(block.data.toRows, Vector(
      Vector(3.75, 1.0, 2.25),
      Vector(0.75, -2.0, -0.75)
    ))

    val exported = fixture.exportRoot.resolve(NiftiRoundTripFixture.relativePath)
    val header = Nifti.readHeader(exported)
    assertEquals(header.datatype, 4)
    assertEquals(header.bitpix, 16)
    assertEquals(header.dims, Vector(3, 2, 2, 2))
    assertEquals(header.slope, 0.25)
    assertEquals(header.intercept, -2.0)
    assertEquals(header.sformCode, 1)
    assertEquals(Nifti.readVec(exported).valueAtCanonicalOrdinal(23), 3.75)
    BidsProjectLoader.loadStrict(fixture.exportRoot).fold(error => fail(error.message), _ => ())

  test("importer rejects unsupported scalar types before publishing"):
    val root = Files.createTempDirectory("scalafim-nifti-zarr-unsupported")
    val source = NiftiRoundTripFixture.writeSource(root.resolve("source.nii"))
    val bytes = Files.readAllBytes(source)
    val buffer = java.nio.ByteBuffer.wrap(bytes).order(java.nio.ByteOrder.LITTLE_ENDIAN)
    buffer.putShort(70, 512.toShort)
    buffer.putShort(72, 16.toShort)
    Files.write(source, bytes)
    val chunk = zarr4s.Shape(1L, 1L, 2L, 2L).fold(error => fail(error.message), identity)
    val target = root.resolve("revision.zarr")
    assert(NiftiCanonicalImporter.publish(
      source,
      NiftiRoundTripFixture.relativePath,
      target,
      chunk
    ).isLeft)
    assert(!Files.exists(target))

  test("importer normalizes NIfTI millisecond and microsecond timing to seconds"):
    val cases = Vector(
      ("millisecond", 1500.0f, 18.toByte),
      ("microsecond", 1500000.0f, 26.toByte)
    )
    cases.foreach: (label, repetitionTime, xyztUnits) =>
      val root = Files.createTempDirectory(s"scalafim-nifti-zarr-$label-tr")
      val source = NiftiRoundTripFixture.writeSource(
        root.resolve("source.nii"),
        repetitionTime = repetitionTime,
        xyztUnits = xyztUnits
      )
      val chunk = Shape(1L, 1L, 2L, 2L).fold(error => fail(error.message), identity)
      val imported = NiftiCanonicalImporter.publish(
        source,
        NiftiRoundTripFixture.relativePath,
        root.resolve("revision.zarr"),
        chunk
      ).fold(error => fail(error.message), identity)
      assertRegularSeconds(imported.manifest.timing, 1.5, 2L)

  test("importer refuses frequency-domain NIfTI units for BOLD timing"):
    val root = Files.createTempDirectory("scalafim-nifti-zarr-frequency-units")
    val source = NiftiRoundTripFixture.writeSource(
      root.resolve("source.nii"),
      repetitionTime = 2.0f,
      xyztUnits = 34.toByte
    )
    val chunk = Shape(1L, 1L, 2L, 2L).fold(error => fail(error.message), identity)
    val target = root.resolve("revision.zarr")
    val result = NiftiCanonicalImporter.publish(
      source,
      NiftiRoundTripFixture.relativePath,
      target,
      chunk
    )
    assert(result.isLeft)
    assert(!Files.exists(target))

  test("measured sharded import path publishes start-indexed canonical BOLD"):
    val root = Files.createTempDirectory("scalafim-nifti-zarr-sharded")
    val source = NiftiRoundTripFixture.writeSource(root.resolve("source.nii"))
    val inner = Shape(1L, 1L, 2L, 2L).fold(error => fail(error.message), identity)
    val shard = Shape(2L, 2L, 2L, 4L).fold(error => fail(error.message), identity)
    val profile = CanonicalChunkProfile("test-balanced", inner, shard)
      .fold(error => fail(error.message), identity)
    val target = root.resolve("revision.zarr")
    val imported = NiftiCanonicalImporter.publishSharded(
      source,
      NiftiRoundTripFixture.relativePath,
      target,
      profile
    ).fold(error => fail(error.message), identity)
    imported.descriptor.layout match
      case PhysicalLayout.Sharded(_, _, _, IndexLocation.Start, outer) => assert(outer.isEmpty)
      case other => fail(s"expected start-indexed sharded layout, found $other")

    val store = JvmFileStore.open(target).fold(fail(_), identity)
    val opened = NeuroArchiveZarr.openCanonical(store, runtime = JvmCodecRuntime.portable)
      .fold(error => fail(error.message), identity)
    val block = ZarrResponseBlockSource.open(opened)
      .fold(error => fail(error.message), identity)
      .readBlock(DataSelection(
        TimepointSelection.indices(1, 0),
        VoxelSelection.indices(11, 0, 5)
      )).fold(error => fail(error.message), identity)
    assertEquals(block.data.toRows, Vector(
      Vector(3.75, 1.0, 2.25),
      Vector(0.75, -2.0, -0.75)
    ))

  test("fit chunk selections execute through Zarr without materializing the whole run"):
    val root = Files.createTempDirectory("scalafim-zarr-fit-chunks")
    val fixture = NiftiRoundTripFixture.create(root)
    val source = ZarrResponseBlockSource.open(fixture.opened)
      .fold(error => fail(error.message), identity)
    val selection = DataSelection(
      TimepointSelection.indices(1, 0),
      VoxelSelection.indices(11, 0, 5)
    ).resolveEither(source.shape, source.voxelDomain)
      .fold(error => fail(error.message), identity)
    val chunks = FitChunkPlan.fromResolvedSelection(
      selection,
      FitChunkingStrategy.unsafeByVoxelCount(2)
    ).fold(error => fail(error.message), identity)
    assertEquals(chunks.indexed.map(_.voxelIndices).toVector, Vector(Vector(11, 0), Vector(5)))
    val blocks = chunks.indexed.map(chunk =>
      source.readBlock(chunk.selection).fold(error => fail(error.message), identity)
    )
    assertEquals(blocks.map(_.nVoxels).toVector, Vector(2, 1))
    assertEquals(blocks.map(_.timepoints).toVector, Vector(Vector(1, 0), Vector(1, 0)))
