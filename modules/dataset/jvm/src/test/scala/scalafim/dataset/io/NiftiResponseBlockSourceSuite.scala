package scalafim.dataset.io

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import munit.FunSuite
import scalafim.dataset.{
  DataSelection,
  DatasetResponseSource,
  DatasetResponseSchema,
  TimepointSelection,
  VoxelSelection
}
import scalafim.image.{Axis, PrimitiveBuffers, NeuroSpace, NeuroVec}
import scalafim.image.io.Nifti
import scalafim.response.*

import java.nio.file.{Files, Path}
import java.nio.ByteOrder
import java.util.zip.GZIPOutputStream
import scala.jdk.CollectionConverters.*

class NiftiResponseBlockSourceSuite extends FunSuite:
  test("uncompressed NIfTI source reads only the requested ordered block") {
    withFixture { root =>
      val path = writeSeries(root.resolve("bold.nii"))
      val source = NiftiResponseBlockSource.open(path).toOption.get
      val selection = DataSelection(
        time = TimepointSelection.indices(2, 0),
        voxels = VoxelSelection.indices(3, 1)
      )

      val block = source.readBlock(selection).toOption.get

      assertEquals(source.shape.timepoints, 3)
      assertEquals(source.shape.spatialSize, 4)
      assertEquals(block.timepoints, Vector(2, 0))
      assertEquals(block.voxelIndices, Vector(3, 1))
      assertMatrixEquals(block.data, Vector(Vector(11.0, 9.0), Vector(3.0, 1.0)))
      assertEquals(block.data.rows * block.data.cols, 4)
      assert(!source.wasStaged)
    }
  }

  test("response-kernel adapter preserves uncompressed NIfTI values and order") {
    withFixture { root =>
      val source =
        NiftiResponseBlockSource
          .open(writeSeries(root.resolve("adapter-bold.nii")))
          .toOption
          .get
      val schemaId = ResponseSchemaId.unsafe("nifti-adapter")
      val time =
        TimeDomain
          .regular(
            DomainId.unsafe[TimeAxis]("nifti-adapter:time"),
            origin = 0.5,
            interval = 1.0,
            count = source.shape.timepoints,
            UnitId.unsafe("second")
          )
          .toOption
          .get
      val schema =
        DatasetResponseSchema
          .fromBlockSource(
            source,
            schemaId,
            time,
            UnitId.unsafe("scanner-unit")
          )
          .toOption
          .get
      val adapted =
        DatasetResponseSource
          .fromBlockSource[IO](
            source,
            schema,
            SourceId.unsafe("nifti-adapter-source")
          )
          .toOption
          .get
      val times =
        OrderedIndices
          .fromInts(schema.time.id, schema.time.count, Vector(2, 0))
          .toOption
          .get
      val samples =
        OrderedIndices
          .fromInts(schema.samples.id, schema.samples.count, Vector(3, 1))
          .toOption
          .get
      val requested =
        ResolvedResponseSelection
          .make(schema, times, samples)
          .toOption
          .get
      val result =
        adapted
          .read(requested)
          .value
          .unsafeRunSync()
          .toOption
          .get

      assertEquals(result.block.selection, requested)
      assertEquals(
        result.block.rowMajorCopy.toSeq.toVector,
        Vector(11.0, 9.0, 3.0, 1.0)
      )
    }
  }

  test("compressed NIfTI requires explicit reusable staging") {
    withFixture { root =>
      val raw = writeSeries(root.resolve("bold.nii"))
      val compressed = gzip(raw, root.resolve("bold.nii.gz"))
      val cache = NiftiStagingCache.unsafe(root.resolve("cache"))

      assert(NiftiResponseBlockSource.open(compressed).isLeft)

      val first = cache.stage(compressed).toOption.get
      val second = cache.stage(compressed).toOption.get
      val source = NiftiResponseBlockSource.open(compressed, staging = Some(cache)).toOption.get
      val block = source.readBlock(
        DataSelection(
          time = TimepointSelection.indices(1),
          voxels = VoxelSelection.indices(0, 2)
        )
      ).toOption.get

      assertEquals(first, second)
      assertEquals(source.dataPath, first)
      assert(source.wasStaged)
      assertMatrixEquals(block.data, Vector(Vector(4.0, 6.0)))
      val stagedFiles = Files.list(cache.root)
      try assertEquals(stagedFiles.iterator().asScala.count(_.toString.endsWith(".nii")), 1)
      finally stagedFiles.close()
    }
  }

  test("failed compressed staging leaves no partial finalized artifact") {
    withFixture { root =>
      val compressed = root.resolve("broken.nii.gz")
      Files.write(compressed, Array[Byte](1, 2, 3, 4))
      val cache = NiftiStagingCache.unsafe(root.resolve("cache"))

      assert(cache.stage(compressed).isLeft)

      val files = Files.list(cache.root)
      try assertEquals(files.iterator().asScala.toVector, Vector.empty)
      finally files.close()
    }
  }

  test("raw byte fixtures cover every supported NIfTI scalar decoder") {
    val cases = Vector(
      ("uint8", 2, 8, Vector(0.0, 255.0, 7.0, 128.0), 0.0),
      ("int16", 4, 16, Vector(-32768.0, -2.0, 7.0, 32767.0), 0.0),
      ("int32", 8, 32, Vector(-100000.0, -2.0, 7.0, 100000.0), 0.0),
      ("float32", 16, 32, Vector(-1.25, 2.5, 7.75, 100.125), 1e-6),
      ("float64", 64, 64, Vector(-1.25, 2.5, 7.75, 100.125), 1e-12)
    )

    withFixture { root =>
      cases.foreach { case (label, datatype, bitpix, raw, tolerance) =>
        val path = NiftiByteFixture.write(root.resolve(s"$label.nii"), datatype, bitpix, raw)
        val source = NiftiResponseBlockSource.open(path).toOption.get
        val block = source.readBlock(
          DataSelection(
            time = TimepointSelection.indices(1, 0),
            voxels = VoxelSelection.indices(1, 0)
          )
        ).toOption.get

        assertMatrixEquals(
          block.data,
          Vector(Vector(raw(3), raw(2)), Vector(raw(1), raw(0))),
          tolerance
        )
      }
    }
  }

  test("big-endian payloads apply slope and intercept while zero slope means identity") {
    withFixture { root =>
      val bigEndian = NiftiByteFixture.write(
        root.resolve("big-endian-scaled.nii"),
        datatype = 4,
        bitpix = 16,
        rawValues = Vector(1.0, -2.0, 300.0, -400.0),
        byteOrder = ByteOrder.BIG_ENDIAN,
        slope = 2.5f,
        intercept = -3.0f,
        voxOffset = 368
      )
      val scaled = NiftiResponseBlockSource.open(bigEndian).toOption.get.readBlock().toOption.get
      assertMatrixEquals(scaled.data, Vector(Vector(-0.5, -8.0), Vector(747.0, -1003.0)))

      val zeroSlope = NiftiByteFixture.write(
        root.resolve("zero-slope.nii"),
        datatype = 8,
        bitpix = 32,
        rawValues = Vector(1.0, 2.0, 3.0, 4.0),
        slope = 0.0f,
        intercept = 5.0f
      )
      val identityScaled = NiftiResponseBlockSource.open(zeroSlope).toOption.get.readBlock().toOption.get
      assertMatrixEquals(identityScaled.data, Vector(Vector(6.0, 7.0), Vector(8.0, 9.0)))
    }
  }

  test("truncated payloads and unsupported datatype contracts fail explicitly") {
    withFixture { root =>
      val truncated = NiftiByteFixture.write(
        root.resolve("truncated.nii"),
        datatype = 64,
        bitpix = 64,
        rawValues = Vector(1.0, 2.0, 3.0, 4.0)
      )
      Files.write(truncated, Files.readAllBytes(truncated).dropRight(1))
      val truncatedRead = NiftiResponseBlockSource.open(truncated).toOption.get.readBlock()
      assert(truncatedRead.left.toOption.exists(_.message.contains("unexpected EOF")))

      val unsupported = NiftiByteFixture.write(
        root.resolve("unsupported.nii"),
        datatype = 512,
        bitpix = 16,
        rawValues = Vector.empty
      )
      assert(NiftiResponseBlockSource.open(unsupported).left.toOption.exists(_.message.contains("unsupported NIfTI datatype")))

      val mismatched = NiftiByteFixture.write(
        root.resolve("mismatched.nii"),
        datatype = 64,
        bitpix = 32,
        rawValues = Vector.empty
      )
      assert(
        NiftiResponseBlockSource
          .open(mismatched)
          .left
          .toOption
          .exists(_.message.contains("datatype 64 with 32 bits"))
      )
    }
  }

  private def writeSeries(path: Path): Path =
    val values = PrimitiveBuffers.fromArray(
      Array(
        0.0, 4.0, 8.0,
        1.0, 5.0, 9.0,
        2.0, 6.0, 10.0,
        3.0, 7.0, 11.0
      )
    )
    val space = NeuroSpace(Vector(2, 2, 1)).addDim(3, Some(Axis.Time))
    Nifti.writeVec(path, NeuroVec.copyFromCanonicalArray(values, space, "bold"))

  private def gzip(source: Path, target: Path): Path =
    val input = Files.newInputStream(source)
    val output = new GZIPOutputStream(Files.newOutputStream(target))
    try input.transferTo(output)
    finally
      output.close()
      input.close()
    target

  private def assertMatrixEquals(
      actual: scalafim.image.DMat,
      expected: Vector[Vector[Double]],
      tolerance: Double = 1e-12
  ): Unit =
    assertEquals(actual.rows, expected.length)
    assertEquals(actual.cols, expected.head.length)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        val value = actual(row, column)
        assert(value.isFinite)
        assertEqualsDouble(value, expected(row)(column), tolerance)
        column += 1
      row += 1

  private def withFixture[A](body: Path => A): A =
    val root = Files.createTempDirectory("scalafim-nifti-block-")
    try body(root)
    finally
      val files = Files.walk(root)
      try files.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists)
      finally files.close()
