package scalafim.spatial.io

import image4s.geometry.GeometryError
import scalafim.image.io.Nifti
import scalafim.image.{SampleSpaceError, SampleSpaces, PrimitiveBuffers, SomeSampleSpace, SomeScalarSeries}
import scalafim.spatial.{ProviderAffines, ProviderAxes}
import scalafim.image.SampleSpaces.*
import scalafim.spatial.*

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption.APPEND
import java.nio.file.{Files, Path}

class NiftiFieldSourceSuite extends munit.FunSuite:

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def apiValue[A](result: Either[FieldApiError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def volumeDomain(name: String, space: SomeSampleSpace): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val modality = spatialValue(Modality(name))
    val geometry = spatialValue(SamplingGeometry.volume(space))
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def withNiftiPath[A](body: Path => A): A =
    val directory = Files.createTempDirectory("scalafim-nifti-field-source-")
    val path = directory.resolve("fixture.nii")
    try body(path)
    finally
      Files.deleteIfExists(path)
      Files.deleteIfExists(directory)

  test("lazy runtime reads one exact NIfTI support block and closes its channel"):
    withNiftiPath { path =>
      val spatial = SampleSpaces(Vector(4, 1, 1), affine = Some(ProviderAffines.identity))
      val domain = volumeDomain("root", spatial)
      val source = spatialValue(NiftiFieldSource.prepare(path, domain, observations = 3, label = "bold"))
      val field = spatialValue(Field.fromSource(domain, source))
      given SpatialGraph = spatialValue(SpatialGraph.build(Vector(domain), Vector.empty))
      val view = field.rows(3, 1).flatMap(_.timeBlock(1, 2)) match
        case Right(value) => value
        case Left(error) => fail(error.message)

      assertEquals(source.stats, NiftiFieldSourceStats(0L, 0L, 0L, 0L, 0L, 0L))

      val values = PrimitiveBuffers.fromArray(Array(0.0, 10.0, 20.0, 1.0, 11.0, 21.0, 2.0, 12.0, 22.0, 3.0, 13.0, 23.0))
      Nifti
        .writeSeries(path, SomeScalarSeries.unsafeCopyFromCanonicalArray(values, spatial.addDim(ProviderAxes.time(3)), "bold"))
        .fold(error => fail(error.message), _ => ())
      val runtime = LazyFieldRuntime(summon[SpatialGraph])
      given FieldRuntime = runtime

      val first = apiValue(view.value)
      val second = apiValue(view.value)

      assertEquals(first.toRows, Vector(Vector(13.0, 23.0), Vector(11.0, 21.0)))
      assertEquals(second.toRows, first.toRows)
      assertEquals(
        source.stats,
        NiftiFieldSourceStats(
          validationCalls = 2L,
          readCalls = 1L,
          headerReads = 1L,
          channelOpens = 1L,
          readWindows = 4L,
          bytesRead = 32L
        )
      )
      assert(Files.deleteIfExists(path), "the data channel must be closed when evaluation returns")
    }

  test("big-endian int16 fixtures preserve byte order, scaling, and request order"):
    withNiftiPath { path =>
      writeBigEndianInt16(path)
      val spatial = SampleSpaces(Vector(4, 1, 1), affine = Some(ProviderAffines.identity))
      val domain = volumeDomain("root", spatial)
      val source = spatialValue(NiftiFieldSource.prepare(path, domain, observations = 2))
      val request = spatialValue(
        FieldSourceRequest.make(source.descriptor, sourceRows = Vector(3, 1), observations = Vector(1))
      )

      assertEquals(source.validate(), Right(()))
      val block = spatialValue(source.read(request))

      assertEquals(block.data.toRows, Vector(Vector(17.0), Vector(13.0)))
      assertEquals(source.stats.headerReads, 1L)
      assertEquals(source.stats.channelOpens, 1L)
      assertEquals(source.stats.readWindows, 2L)
      assertEquals(source.stats.bytesRead, 4L)
    }

  test("terminal validation reports unavailable, stale, and geometry-mismatched NIfTI roots"):
    withNiftiPath { path =>
      val spatial = SampleSpaces(Vector(4, 1, 1), affine = Some(ProviderAffines.identity))
      val domain = volumeDomain("root", spatial)
      val source = spatialValue(NiftiFieldSource.prepare(path, domain, observations = 1))
      val field = spatialValue(Field.fromSource(domain, source))
      given SpatialGraph = spatialValue(SpatialGraph.build(Vector(domain), Vector.empty))
      val runtime = LazyFieldRuntime(summon[SpatialGraph])
      given FieldRuntime = runtime

      val unavailable = field.value.left.toOption
      assert(unavailable.exists {
        case FieldApiError.Spatial(SpatialError.FieldSourceUnavailable(id, _)) => id == source.descriptor.id
        case _ => false
      })

      val values = PrimitiveBuffers.fromArray(Array(1.0, 2.0, 3.0, 4.0))
      Nifti
        .writeSeries(path, SomeScalarSeries.unsafeCopyFromCanonicalArray(values, spatial.addDim(ProviderAxes.time(1))))
        .fold(error => fail(error.message), _ => ())
      assertEquals(apiValue(field.value).toRows, Vector(Vector(1.0), Vector(2.0), Vector(3.0), Vector(4.0)))
      Files.write(path, Array(0.toByte), APPEND)

      val stale = field.value.left.toOption
      assert(stale.exists {
        case FieldApiError.Spatial(SpatialError.FieldSourceStale(id, _, _)) => id == source.descriptor.id
        case _ => false
      })
    }

    withNiftiPath { path =>
      val identity = SampleSpaces(Vector(4, 1, 1), affine = Some(ProviderAffines.identity))
      val translated = SampleSpaces(
        Vector(4, 1, 1),
        affine = Some(
          ProviderAffines.fromRows(
            Vector(
              Vector(1.0, 0.0, 0.0, 5.0),
              Vector(0.0, 1.0, 0.0, 0.0),
              Vector(0.0, 0.0, 1.0, 0.0),
              Vector(0.0, 0.0, 0.0, 1.0)
            )
          )
        )
      )
      Nifti
        .writeSeries(
          path,
          SomeScalarSeries.unsafeCopyFromCanonicalArray(
            PrimitiveBuffers.fromArray(Array(1.0, 2.0, 3.0, 4.0)),
            identity.addDim(ProviderAxes.time(1))
          )
        )
        .fold(error => fail(error.message), _ => ())
      val domain = volumeDomain("translated", translated)
      val source = spatialValue(NiftiFieldSource.prepare(path, domain, observations = 1))

      assertEquals(
        source.validate().left.toOption,
        Some(
          SpatialError.FieldSourceGridMismatch(
            source.descriptor.id,
            GeometryError.GridsNotCongruent(0.0)
          )
        )
      )
      assertEquals(source.stats.channelOpens, 0L)
    }

  test("NIfTI validation preserves an exact D2 sample-space admission failure"):
    withNiftiPath { path =>
      writeTwoDimensionalFloat32(path)
      val domain = volumeDomain("d2-header", SampleSpaces(Vector(2, 2, 1)))
      val source = spatialValue(NiftiFieldSource.prepare(path, domain, observations = 1))
      val cause =
        SampleSpaceError.ExpectedDimensionality(
          "D3 sample space",
          expected = 3,
          actual = 2
        )

      assertEquals(
        source.validate().left.toOption,
        Some(SpatialError.FieldSourceSampleSpaceAdmission(source.descriptor.id, cause))
      )
      assertEquals(source.stats.channelOpens, 0L)
    }

  private def writeTwoDimensionalFloat32(path: Path): Unit =
    val values = Array(1.0f, 2.0f, 3.0f, 4.0f)
    val bytes = Array.ofDim[Byte](352 + values.length * 4)
    val buffer = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
    buffer.putInt(0, 348)
    buffer.putShort(40, 2.toShort)
    buffer.putShort(42, 2.toShort)
    buffer.putShort(44, 2.toShort)
    buffer.putShort(70, 16.toShort)
    buffer.putShort(72, 32.toShort)
    buffer.putFloat(76, 1.0f)
    buffer.putFloat(80, 1.0f)
    buffer.putFloat(84, 1.0f)
    buffer.putFloat(108, 352.0f)
    buffer.putShort(254, 1.toShort)
    buffer.putFloat(280, 1.0f)
    buffer.putFloat(300, 1.0f)
    buffer.putFloat(320, 1.0f)
    val magic = "n+1".getBytes(StandardCharsets.US_ASCII)
    buffer.put(344, magic(0))
    buffer.put(345, magic(1))
    buffer.put(346, magic(2))
    var i = 0
    while i < values.length do
      buffer.putFloat(352 + i * 4, values(i))
      i += 1
    Files.write(path, bytes)

  private def writeBigEndianInt16(path: Path): Unit =
    val values = Array[Short](1, 2, 3, 4, 5, 6, 7, 8)
    val bytes = Array.ofDim[Byte](352 + values.length * 2)
    val buffer = ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN)
    buffer.putInt(0, 348)
    buffer.putShort(40, 4.toShort)
    buffer.putShort(42, 4.toShort)
    buffer.putShort(44, 1.toShort)
    buffer.putShort(46, 1.toShort)
    buffer.putShort(48, 2.toShort)
    buffer.putShort(70, 4.toShort)
    buffer.putShort(72, 16.toShort)
    buffer.putFloat(76, 1.0f)
    buffer.putFloat(80, 1.0f)
    buffer.putFloat(84, 1.0f)
    buffer.putFloat(88, 1.0f)
    buffer.putFloat(92, 1.0f)
    buffer.putFloat(108, 352.0f)
    buffer.putFloat(112, 2.0f)
    buffer.putFloat(116, 1.0f)
    buffer.putShort(254, 1.toShort)
    buffer.putFloat(280, 1.0f)
    buffer.putFloat(300, 1.0f)
    buffer.putFloat(320, 1.0f)
    val magic = "n+1".getBytes(StandardCharsets.US_ASCII)
    buffer.put(344, magic(0))
    buffer.put(345, magic(1))
    buffer.put(346, magic(2))
    var i = 0
    while i < values.length do
      buffer.putShort(352 + i * 2, values(i))
      i += 1
    Files.write(path, bytes)
