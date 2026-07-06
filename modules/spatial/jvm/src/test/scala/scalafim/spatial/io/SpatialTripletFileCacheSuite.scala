package scalafim.spatial.io

import scalafim.image.{DMat, NeuroSpace}
import scalafim.linalg.{CsrMatrix, LinearMapError, SparseTriplets}
import scalafim.spatial.*

import java.io.DataOutputStream
import java.nio.file.{Files, Path}

class SpatialTripletFileCacheSuite extends munit.FunSuite:

  private def value[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def linValue[A](result: Either[LinearMapError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def ioValue[A](result: Either[SpatialIoError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def domain(name: String, dims: Vector[Int] = Vector(2, 1, 1)): Domain =
    val id = value(DomainId(name))
    val subject = value(SubjectId("sub-01"))
    val modality = value(Modality(name))
    val geometry = value(SamplingGeometry.volume(NeuroSpace(dims, trans = Some(DMat.eye(4)))))
    value(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def assertSameTriplets(actual: SparseTriplets, expected: SparseTriplets): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    assertEquals(actual.rowIndices.toVector, expected.rowIndices.toVector)
    assertEquals(actual.colIndices.toVector, expected.colIndices.toVector)
    val actualValues = actual.values
    val expectedValues = expected.values
    assertEquals(actualValues.length, expectedValues.length)
    var i = 0
    while i < actualValues.length do
      assertEquals(
        java.lang.Double.doubleToRawLongBits(actualValues(i)),
        java.lang.Double.doubleToRawLongBits(expectedValues(i))
      )
      i += 1

  private def withTempFile[A](name: String)(f: Path => A): A =
    val dir = Files.createTempDirectory("scalafim-spatial-cache-")
    val path = dir.resolve(name)
    try f(path)
    finally
      Files.deleteIfExists(path)
      Files.deleteIfExists(dir)

  test("writeTriplets roundtrips cache key, provenance, and sparse values losslessly"):
    val source = value(DomainId("func"))
    val target = value(DomainId("mni"))
    val pathIds = Vector(value(MorphismId("func-to-t1w")), value(MorphismId("t1w-to-mni")))
    val provenance =
      OperatorProvenance(
        path = pathIds,
        routing = RoutingPolicy.Anatomical,
        sampling = SamplingPolicy.Trilinear,
        roi = Some(Vector(3, 1)),
        allowInverses = true,
        compiler = "volume-affine-v1"
      )
    val key =
      OperatorCacheKey(
        source = source,
        target = target,
        path = provenance.path,
        routing = provenance.routing,
        sampling = provenance.sampling,
        roi = provenance.roi,
        allowInverses = provenance.allowInverses,
        compiler = provenance.compiler,
        rows = 2,
        cols = 4
      )
    val triplets =
      linValue(
        SparseTriplets(
          rows = 2,
          cols = 4,
          rowIndices = Array(0, 0, 1),
          colIndices = Array(1, 3, 0),
          values = Array(math.Pi, -0.0, 0.125)
        )
      )

    withTempFile("operator.sftc") { path =>
      ioValue(SpatialTripletFileCache.writeTriplets(path, key, provenance, triplets))
      val cached = ioValue(SpatialTripletFileCache.read(path))

      assertEquals(cached.key, key)
      assertEquals(cached.provenance, provenance)
      assertSameTriplets(cached.triplets, triplets)
    }

  test("write persists compiled CSR spatial operators"):
    val epi = domain("epi", Vector(2, 2, 1))
    val graph = value(SpatialGraph.build(Vector(epi)))
    val operator =
      value(
        OperatorCompiler.compile(
          graph,
          CompileRequest(epi.id, epi.id, sampling = SamplingPolicy.Nearest, roi = Some(Vector(3, 1)))
        )
      )
    val expected =
      operator.map match
        case csr: CsrMatrix => csr.toTriplets
        case other => fail(s"expected CsrMatrix, got ${other.getClass.getName}")

    withTempFile("compiled.sftc") { path =>
      ioValue(SpatialTripletFileCache.write(path, operator))
      val cached = ioValue(SpatialTripletFileCache.read(path))

      assertEquals(cached.key, OperatorCacheKey.from(operator))
      assertEquals(cached.provenance, operator.provenance)
      assertSameTriplets(cached.triplets, expected)
    }

  test("read rejects cache files with the wrong magic header"):
    withTempFile("bad.sftc") { path =>
      val out = new DataOutputStream(Files.newOutputStream(path))
      try
        out.writeUTF("not-scalafim")
        out.writeInt(1)
      finally out.close()

      SpatialTripletFileCache.read(path).left.toOption match
        case Some(SpatialIoError.InvalidCacheHeader(_, reason)) =>
          assert(reason.contains("expected scalafim-spatial-triplets"))
        case other =>
          fail(s"expected invalid cache header, got $other")
    }
