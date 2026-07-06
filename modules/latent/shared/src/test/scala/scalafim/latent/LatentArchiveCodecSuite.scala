package scalafim.latent

import scalafim.archive.lna.{DatasetRole, LnaPipeline, Payload, SharedBasisArtifact, SharedBasisId, SharedBasisMask, TransformKind, TransformParams}
import scalafim.image.{DMat, NeuroSpace}
import scalafim.linalg.{DoubleMatrix, DoubleVector}

class LatentArchiveCodecSuite extends munit.FunSuite:
  private val basis =
    DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.5, 1.0),
        Vector(0.0, 2.0)
      )
    )

  private val loadings =
    DoubleMatrix.fromRows(
      Vector(
        Vector(10.0, 1.0),
        Vector(20.0, 2.0),
        Vector(30.0, 3.0),
        Vector(40.0, 4.0)
      )
    )

  private val offset =
    DoubleVector.fromSeq(Vector(1.0, 2.0, 3.0, 4.0))

  test("explicit latent responses roundtrip through LNA archives") {
    val source =
      ExplicitLatentResponse(
        basis = basis,
        loadings = loadings,
        offset = Some(offset),
        sourceDomain = DomainId.unsafe("latent.coefficients.demo"),
        targetDomain = DomainId.unsafe("voxels.demo"),
        label = "demo-latent",
        metadata = Map("basis" -> "provided", "subject" -> "sub-01")
      ).fold(err => fail(err.message), identity)

    val archive =
      LatentArchiveCodec
        .toArchive(source, NeuroSpace(Vector(2, 2, 1)))
        .fold(err => fail(err.message), identity)

    val decoded =
      LatentArchiveCodec
        .fromArchive(archive)
        .fold(err => fail(err.message), identity)

    assertEquals(decoded.sourceDomain.value, "latent.coefficients.demo")
    assertEquals(decoded.targetDomain.value, "voxels.demo")
    assertEquals(decoded.label, "demo-latent")
    assertEquals(decoded.metadata, Map("basis" -> "provided", "subject" -> "sub-01"))
    assertEquals(decoded.basis.toRows, basis.toRows)
    assertEquals(decoded.loadings.toRows, loadings.toRows)
    assertEquals(decoded.offset.map(_.toVector), Some(offset.toVector))

    val selected =
      decoded
        .reconstruct(LatentSelection(timepoints = Some(Vector(2, 0)), samples = Some(Vector(3, 1))))
        .fold(err => fail(err.message), identity)

    assertEquals(selected.toRows, Vector(Vector(12.0, 6.0), Vector(44.0, 22.0)))
  }

  test("temporal DCT archives preserve typed DCT params and reconstruct full-rank data") {
    val data =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 2.0, 3.0, 4.0),
          Vector(2.0, 3.0, 5.0, 7.0),
          Vector(3.0, 5.0, 8.0, 11.0),
          Vector(5.0, 8.0, 13.0, 17.0)
        )
      )

    val archive =
      LatentArchiveCodec
        .toTemporalDctArchive(
          data = data,
          space = NeuroSpace(Vector(2, 2, 1)),
          components = data.rows,
          norm = DctNorm.Ortho,
          center = true,
          ridge = 0.0
        )
        .fold(err => fail(err.message), identity)

    assertEquals(archive.manifest.transforms.map(_.kind), Vector(TransformKind.Temporal, TransformKind.Embed))
    archive.manifest.transforms.head.params match
      case TransformParams.TemporalDct(params) =>
        assertEquals(params.components, data.rows)
        assertEquals(params.center, true)
      case other =>
        fail(s"expected temporal DCT params, found $other")

    val decoded =
      LatentArchiveCodec
        .fromArchive(archive)
        .fold(err => fail(err.message), identity)
    val reconstructed =
      decoded
        .reconstruct()
        .fold(err => fail(err.message), identity)
    val pipelineReconstructed =
      LnaPipeline
        .reconstruct(archive)
        .fold(err => fail(err.message), identity)

    assertEquals(decoded.metadata("family"), "time_dct")
    assertEquals(decoded.metadata("basis"), "dct")
    assertEquals(decoded.metadata("components"), "4")
    assertEquals(decoded.metadata("center"), "true")
    assertMatrixEquals(reconstructed, data.toRows, 1e-12)
    assertRowsEqual(pipelineReconstructed.toRows, data.toRows, 1e-12)
  }

  test("shared-basis archives are encoded through the latent Gram solver and store offsets") {
    val sharedLoadings =
      DMat.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(0.0, 1.0)
        )
      )
    val sharedBasis =
      SharedBasisArtifact(
        loadings = sharedLoadings,
        mask = SharedBasisMask(Vector(3), Vector(true, true, true)),
        kind = "nonorthogonal",
        params = Map("source" -> "codec-suite")
      )
    val basisId = SharedBasisId.unsafe("codec_nonorthogonal_basis")
    val expectedCoefficients =
      Vector(
        Vector(1.0, 2.0),
        Vector(3.0, -1.0),
        Vector(-2.0, 0.5)
      )
    val data =
      DoubleMatrix.fromRows(
        expectedCoefficients.map { row =>
          Vector.tabulate(sharedLoadings.rows) { voxel =>
            var sum = Vector(10.0, -2.0, 5.0)(voxel)
            var atom = 0
            while atom < sharedLoadings.cols do
              sum += row(atom) * sharedLoadings(voxel, atom)
              atom += 1
            sum
          }
        }
      )

    val archive =
      LatentArchiveCodec
        .toSharedBasisArchive(
          data = data,
          space = NeuroSpace(Vector(3, 1, 1)),
          basis = sharedBasis,
          basisId = basisId,
          center = true
        )
        .fold(err => fail(err.message), identity)

    val descriptor = archive.manifest.transforms.head
    assertEquals(descriptor.kind, TransformKind.Embed)
    assert(descriptor.datasets.exists(_.role == DatasetRole.Offset))
    descriptor.params match
      case params: TransformParams.SharedBasisEmbed =>
        assertEquals(params.basis.basisId, basisId)
        assert(params.centerDataWith.nonEmpty)
        assertEquals(params.metadata("center"), "true")
      case other =>
        fail(s"expected shared-basis embed params, found $other")

    val offsetPath = descriptor.datasets.find(_.role == DatasetRole.Offset).get.path
    val storedOffset =
      archive.payload(offsetPath) match
        case Some(Payload.DoubleVector(values, _)) => values
        case other => fail(s"expected stored offset vector, found $other")

    val encoded =
      SharedBasisEncoder
        .encode(data, sharedBasis, basisId, center = true)
        .fold(err => fail(err.message), identity)
    val coeffPath = descriptor.datasets.find(_.role == DatasetRole.Coefficients).get.path
    val storedCoefficients =
      archive.payload(coeffPath) match
        case Some(Payload.DoubleMatrix(values, _)) => values
        case other => fail(s"expected stored coefficient matrix, found $other")

    assertRowsEqual(Vector(storedOffset), Vector(encoded.offset.get.toVector), 1e-12)
    assertRowsEqual(storedCoefficients.toRows, encoded.coefficients.toRows, 1e-12)
    assert(!rowsClose(storedCoefficients.toRows, rawProjection(data.toRows, storedOffset, sharedLoadings), 1e-8))
  }

  private def assertMatrixEquals(
      actual: DoubleMatrix,
      expected: Vector[Vector[Double]],
      tol: Double
  ): Unit =
    assertRowsEqual(actual.toRows, expected, tol)

  private def assertRowsEqual(
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

  private def rawProjection(
      rows: Vector[Vector[Double]],
      offset: Vector[Double],
      loadings: DMat
  ): Vector[Vector[Double]] =
    rows.map { row =>
      Vector.tabulate(loadings.cols) { atom =>
        var sum = 0.0
        var voxel = 0
        while voxel < loadings.rows do
          sum += (row(voxel) - offset(voxel)) * loadings(voxel, atom)
          voxel += 1
        sum
      }
    }

  private def rowsClose(
      actual: Vector[Vector[Double]],
      expected: Vector[Vector[Double]],
      tol: Double
  ): Boolean =
    actual.length == expected.length &&
      actual.zip(expected).forall { case (actualRow, expectedRow) =>
        actualRow.length == expectedRow.length &&
          actualRow.zip(expectedRow).forall { case (actualValue, expectedValue) =>
            math.abs(actualValue - expectedValue) <= tol
          }
      }
