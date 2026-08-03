package scalafim.latent

import scalafim.image.NeuroSpace
import gale.linalg.DMat

class BoldZipEncoderSuite extends munit.FunSuite:

  test("identity-detail encoder exactly reconstructs deterministic data and archives") {
    val data =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(1.0, -2.0, 0.5),
          Vector(3.0, 0.25, -1.5),
          Vector(-0.75, 2.5, 4.0),
          Vector(0.0, -3.0, 1.25)
        )
      )
    val encoding =
      BoldZipEncoder
        .identityDetail(data, metadata = Map("case" -> "identity-detail"))
        .fold(err => fail(err.message), identity)
    val payload = encoding.payload

    assertEquals(payload.shape.timepoints, data.rows)
    assertEquals(payload.shape.samples, data.cols)
    assertEquals(payload.shape.coefficients, data.cols)
    assertEquals(payload.texture.length, data.cols)
    assertEquals(payload.events.length, 0)
    assertEquals(payload.metadata("encoder"), "identity_detail")
    assertEquals(payload.metadata("case"), "identity-detail")
    assertEqualsDouble(encoding.quality.maxAbsError, 0.0, 1e-12)
    assertEqualsDouble(encoding.quality.relativeFrobeniusError, 0.0, 1e-12)
    assertRowsClose(payload.reconstruct().fold(err => fail(err.message), identity).toRows, data.toRows, 1e-12)
    assertRowsClose(payload.decodeCoefficients(payload.coefTime.transpose).fold(err => fail(err.message), identity).toRows, data.transpose.toRows, 1e-12)

    val archive =
      BoldZipLatentArchiveCodec
        .toArchive(payload, NeuroSpace(Vector(data.cols, 1, 1)))
        .fold(err => fail(err.message), identity)
    val decoded =
      LatentArchiveRegistry.standard
        .fromArchive(archive)
        .fold(err => fail(err.message), identity) match
        case LatentArchiveResponse.BoldZip(response) => response
        case other => fail(s"expected BOLDZip archive response, found $other")
    assertRowsClose(decoded.reconstruct().fold(err => fail(err.message), identity).toRows, data.toRows, 1e-12)
  }

  test("centered identity-detail encoder stores offsets but preserves reconstruction") {
    val data =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(10.0, 2.0),
          Vector(12.0, 4.0),
          Vector(14.0, 6.0)
        )
      )
    val encoding =
      BoldZipEncoder
        .identityDetail(data, center = true, metadata = Map("case" -> "centered"))
        .fold(err => fail(err.message), identity)
    val payload = encoding.payload

    assert(payload.offset.nonEmpty)
    assertEquals(payload.offset.map(_.toVector), Some(Vector(12.0, 4.0)))
    assert(payload.decodeSemantics.reconstructionIncludes(LatentMaterializationTerm.SampleOffset))
    assert(!payload.decodeSemantics.coefficientDecodeIncludes(LatentMaterializationTerm.SampleOffset))
    assertRowsClose(payload.reconstruct().fold(err => fail(err.message), identity).toRows, data.toRows, 1e-12)
    assertEqualsDouble(encoding.quality.maxAbsError, 0.0, 1e-12)
  }

  test("random synthetic identity-detail encodings roundtrip exactly") {
    val rng = Lcg(0xB01d5eedL)
    Vector.tabulate(32)(identity).foreach { caseIndex =>
      val rows = 2 + (caseIndex % 5)
      val cols = 1 + (caseIndex % 4)
      val data =
        LatentNumerics.matrixFromRows(
          Vector.tabulate(rows) { _ =>
            Vector.tabulate(cols) { _ =>
              rng.nextDouble(-5.0, 5.0)
            }
          }
        )
      val encoding =
        BoldZipEncoder
          .identityDetail(data, center = caseIndex % 2 == 0, metadata = Map("case" -> s"synthetic-$caseIndex"))
          .fold(err => fail(err.message), identity)
      val reconstructed =
        encoding.payload.reconstruct().fold(err => fail(err.message), identity)

      assertRowsClose(reconstructed.toRows, data.toRows, 1e-10)
      assertEqualsDouble(encoding.quality.maxAbsError, 0.0, 1e-10)
      assertEqualsDouble(encoding.quality.relativeFrobeniusError, 0.0, 1e-10)
      assertEquals(encoding.payload.metadata("case"), s"synthetic-$caseIndex")
    }
  }

  test("identity-detail encoder rejects invalid data and metadata") {
    assert(BoldZipEncoder.identityDetail(DMat.zeros(0, 2)).isLeft)
    assert(BoldZipEncoder.identityDetail(DMat.zeros(2, 0)).isLeft)
    val nonFinite =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(1.0, Double.NaN),
          Vector(2.0, 3.0)
        )
      )
    assert(BoldZipEncoder.identityDetail(nonFinite).isLeft)
    assert(BoldZipEncoder.identityDetail(DMat.eye(2), metadata = Map("" -> "bad")).isLeft)
  }

  private final class Lcg private (private var state: Long):
    def nextDouble(min: Double, max: Double): Double =
      state = state * 6364136223846793005L + 1442695040888963407L
      val bits = (state >>> 11).toDouble / (1L << 53).toDouble
      min + bits * (max - min)

  private object Lcg:
    def apply(seed: Long): Lcg =
      new Lcg(seed)

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
