package scalafim.latent

import scalafim.archive.lna.{SharedBasisArtifact, SharedBasisId, SharedBasisMask}
import scalafim.image.{DMat, NeuroSpace}
import scalafim.linalg.DoubleMatrix

class LatentRoundtripMatrixSuite extends munit.FunSuite:

  test("temporal and shared-spatial archive paths roundtrip separable fixtures") {
    val timepoints = 4
    val components = 2
    val space = NeuroSpace(Vector(3, 1, 1))
    val core =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, -2.0),
          Vector(0.5, 3.0)
        )
      )
    val spatialLoadings =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(0.0, 2.0)
        )
      )
    val sharedBasis =
      SharedBasisArtifact(
        loadings = DMat.fromRows(spatialLoadings.toRows),
        mask = SharedBasisMask(Vector(3), Vector(true, true, true)),
        kind = "roundtrip-matrix",
        params = Map("suite" -> "LatentRoundtripMatrixSuite")
      )

    temporalCases(timepoints, components).foreach { temporalCase =>
      val data = separableData(temporalCase.basis, core, spatialLoadings)
      val temporalArchive =
        LatentEncoder
          .toArchive(data, space, temporalCase.spec)
          .fold(err => fail(err.message), identity)
      val temporalDecoded =
        LatentArchiveCodec
          .fromArchive(temporalArchive)
          .fold(err => fail(err.message), identity)
      val temporalResponse =
        temporalDecoded.capability match
          case LatentResponseCapability.Response(response) =>
            response
          case other =>
            fail(s"${temporalCase.id} did not decode to a latent response: $other")

      assertEquals(temporalDecoded.kind, temporalCase.archiveKind)
      assertEquals(temporalResponse.metadata("case"), temporalCase.id)
      assertResponseRoundtrip(s"${temporalCase.id}:temporal", temporalResponse, data)

      val sharedSpec =
        LatentEncodingSpec
          .sharedBasis(
            basis = sharedBasis,
            basisId = SharedBasisId.unsafe(s"roundtrip_${temporalCase.id}"),
            center = false,
            metadata = Map("case" -> temporalCase.id)
          )
          .fold(err => fail(err.message), identity)
      val sharedArchive =
        LatentEncoder
          .toArchive(data, space, sharedSpec)
          .fold(err => fail(err.message), identity)
      val materializedSpatial =
        LatentArchiveCodec
          .fromArchive(sharedArchive)
          .fold(err => fail(err.message), identity) match
          case LatentArchiveResponse.SharedBasis(response) =>
            response.materialize(sharedBasis, Some(space)).fold(err => fail(err.message), identity)
          case other =>
            fail(s"${temporalCase.id} expected shared-basis archive variant, found $other")

      assertResponseRoundtrip(s"${temporalCase.id}:shared-spatial", materializedSpatial, data)
      assertRowsEqual(
        temporalResponse.reconstruct().fold(err => fail(err.message), identity).toRows,
        materializedSpatial.reconstruct().fold(err => fail(err.message), identity).toRows,
        1e-10
      )
    }
  }

  private final case class TemporalCase(
      id: String,
      basis: DoubleMatrix,
      spec: LatentEncodingSpec,
      archiveKind: LatentArchiveKind
  )

  private def temporalCases(
      timepoints: Int,
      components: Int
  ): Vector[TemporalCase] =
    val providedBasis =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(0.0, 1.0),
          Vector(-1.0, 1.0)
        )
      )
    val dctBasis =
      DctBasis.build(timepoints, components, DctNorm.Ortho).fold(err => fail(err.message), identity)
    val haarBasis =
      HaarBasis.build(timepoints, components).fold(err => fail(err.message), identity)

    Vector(
      TemporalCase(
        id = "provided",
        basis = providedBasis,
        spec = LatentEncodingSpec
          .providedBasis(
            basis = providedBasis,
            center = false,
            metadata = Map("case" -> "provided")
          )
          .fold(err => fail(err.message), identity),
        archiveKind = LatentArchiveKind.Explicit
      ),
      TemporalCase(
        id = "dct",
        basis = dctBasis,
        spec = LatentEncodingSpec
          .dct(
            timepoints = timepoints,
            components = components,
            norm = DctNorm.Ortho,
            center = false,
            metadata = Map("case" -> "dct")
          )
          .fold(err => fail(err.message), identity),
        archiveKind = LatentArchiveKind.TemporalDct
      ),
      TemporalCase(
        id = "haar",
        basis = haarBasis,
        spec = LatentEncodingSpec
          .haar(
            timepoints = timepoints,
            components = components,
            center = false,
            metadata = Map("case" -> "haar")
          )
          .fold(err => fail(err.message), identity),
        archiveKind = LatentArchiveKind.TemporalHaar
      )
    )

  private def separableData(
      temporalBasis: DoubleMatrix,
      core: DoubleMatrix,
      spatialLoadings: DoubleMatrix
  ): DoubleMatrix =
    DoubleMatrix.multiply(
      DoubleMatrix.multiply(temporalBasis, core),
      spatialLoadings.transpose
    )

  private def assertResponseRoundtrip(
      label: String,
      response: LatentResponse,
      expected: DoubleMatrix
  ): Unit =
    val selection =
      LatentSelection(
        timepoints = Some(Vector(3, 0)),
        samples = Some(Vector(2, 0))
      )
    val reconstructed =
      response.reconstruct().fold(err => fail(s"$label full reconstruction failed: ${err.message}"), identity)
    val selected =
      response.reconstruct(selection).fold(err => fail(s"$label selection failed: ${err.message}"), identity)
    val decoded =
      response
        .decodeCoefficients(response.coefTime.transpose)
        .fold(err => fail(s"$label coefficient decode failed: ${err.message}"), identity)

    assertRowsEqual(reconstructed.toRows, expected.toRows, 1e-10)
    assertRowsEqual(selected.toRows, selectRows(expected, Vector(3, 0), Vector(2, 0)), 1e-10)
    assertRowsEqual(decoded.toRows, expected.transpose.toRows, 1e-10)

  private def selectRows(
      matrix: DoubleMatrix,
      rows: Vector[Int],
      cols: Vector[Int]
  ): Vector[Vector[Double]] =
    rows.map { row =>
      cols.map(col => matrix(row, col))
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
