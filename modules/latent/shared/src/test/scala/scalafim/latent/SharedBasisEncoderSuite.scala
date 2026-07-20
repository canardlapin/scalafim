package scalafim.latent

import scalafim.archive.lna.{SharedBasisArtifact, SharedBasisId, SharedBasisMask}
import scalafim.image.DMat
import scalafim.linalg.DoubleMatrix

class SharedBasisEncoderSuite extends munit.FunSuite:
  private val loadings =
    DMat.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 1.0),
        Vector(0.0, 1.0)
      )
    )

  private val artifact =
    SharedBasisArtifact(
      loadings = loadings,
      mask = SharedBasisMask(Vector(3), Vector(true, true, true)),
      kind = "nonorthogonal",
      params = Map("source" -> "suite")
    )

  private val basisId = SharedBasisId.unsafe("nonorthogonal_basis")

  private val coefficients =
    Vector(
      Vector(1.0, 2.0),
      Vector(3.0, -1.0),
      Vector(-2.0, 0.5)
    )

  private val offset = Vector(10.0, -2.0, 5.0)

  test("projects shared-basis data with a Gram solve rather than raw loading multiplication") {
    val data = denseFrom(coefficients, offset)
    val encoding =
      SharedBasisEncoder
        .encode(data, artifact, basisId, center = true)
        .fold(err => fail(err.message), identity)

    val reconstructed =
      encoding.response
        .reconstruct()
        .fold(err => fail(err.message), identity)

    assertRowsEqual(reconstructed.toRows, data.toRows, 1e-12)
    assertRowsEqual(Vector(encoding.offset.get.toVector), Vector(columnMeans(data.toRows)), 1e-12)
    assert(!rowsClose(encoding.coefficients.toRows, rawProjection(data.toRows, columnMeans(data.toRows)), 1e-8))
    assertEquals(encoding.response.metadata("family"), "shared_basis")
    assertEquals(encoding.response.metadata("basis.id"), basisId.value)
    assertEquals(encoding.response.metadata("center"), "true")
    assert(encoding.response.decodeSemantics.coefficientDecodeIsLinearOnly)
    assert(encoding.response.decodeSemantics.reconstructionIncludes(LatentMaterializationTerm.SampleOffset))
  }

  test("recovers exact coefficients for non-centered data in the shared basis span") {
    val data = denseFrom(coefficients, Vector.fill(3)(0.0))
    val encoding =
      SharedBasisEncoder
        .encode(data, artifact, basisId, center = false)
        .fold(err => fail(err.message), identity)

    assertEquals(encoding.offset, None)
    assertRowsEqual(encoding.coefficients.toRows, coefficients, 1e-12)
    assert(encoding.response.decodeSemantics.coefficientDecodeIsLinearOnly)
    assert(!encoding.response.decodeSemantics.reconstructionIncludes(LatentMaterializationTerm.SampleOffset))
  }

  test("rejects basis/data voxel mismatches") {
    val data =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 2.0),
          Vector(3.0, 4.0)
        )
      )

    val failed =
      SharedBasisEncoder.encode(data, artifact, basisId)

    assert(failed.isLeft)
    assert(failed.left.toOption.exists(_.message.contains("shared basis voxels")))
  }

  private def denseFrom(
      coefficients: Vector[Vector[Double]],
      offset: Vector[Double]
  ): DoubleMatrix =
    DoubleMatrix.fromRows(
      coefficients.map { row =>
        Vector.tabulate(loadings.rows) { voxel =>
          var sum = offset(voxel)
          var atom = 0
          while atom < loadings.cols do
            sum += row(atom) * loadings(voxel, atom)
            atom += 1
          sum
        }
      }
    )

  private def rawProjection(
      data: Vector[Vector[Double]],
      center: Vector[Double]
  ): Vector[Vector[Double]] =
    data.map { row =>
      Vector.tabulate(loadings.cols) { atom =>
        var sum = 0.0
        var voxel = 0
        while voxel < loadings.rows do
          sum += (row(voxel) - center(voxel)) * loadings(voxel, atom)
          voxel += 1
        sum
      }
    }

  private def columnMeans(rows: Vector[Vector[Double]]): Vector[Double] =
    Vector.tabulate(rows.head.length) { col =>
      rows.map(_(col)).sum / rows.length.toDouble
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
