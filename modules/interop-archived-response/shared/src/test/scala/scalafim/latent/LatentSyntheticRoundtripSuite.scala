package scalafim.latent

import scalafim.image.SampleSpaces
import scalafim.image.SampleSpaces.spatialDims

import scalafim.archive.lna.{SharedBasisArtifact, SharedBasisId, SharedBasisMask}
import scalafim.archive.lna.GaleArchiveTestData
import scalafim.image.SomeSampleSpace
import gale.linalg.{DMat, DVec}

class LatentSyntheticRoundtripSuite extends munit.FunSuite:

  test("synthetic temporal encoders roundtrip full, selected, and coefficient decodes") {
    val timepoints = 8
    val components = 4
    val samples = 5
    val rng = Lcg(0x51a7e57L)

    for
      center <- Vector(false, true)
      temporal <- temporalCases(timepoints, components, center)
      caseIndex <- 0 until 12
    do
      val sampleLoadings =
        syntheticMatrix(samples, components, rng, scale = 1.5 + caseIndex.toDouble / 10.0)
      val data =
        LatentNumerics.multiply(temporal.basis, sampleLoadings.transpose)
      val response =
        LatentEncoder
          .encodeResponse(data, temporal.spec)
          .fold(err => fail(s"${temporal.id} encode failed: ${err.message}"), identity)
      val expectedOffset =
        Option.when(center)(columnMeans(data))

      assertEquals(response.metadata("case"), temporal.id)
      assertEquals(response.metadata("center"), center.toString)
      if !center then
        assertRowsEqual(response.loadings.toRows, sampleLoadings.toRows, 1e-8)
      assertResponseRoundtrip(s"${temporal.id}:$center:$caseIndex", response, data, expectedOffset, 1e-8)
  }

  test("synthetic shared and radial spatial encoders roundtrip centered sparse data") {
    val rng = Lcg(0x5a11ad1a1L)

    for caseIndex <- 0 until 16 do
      val timepoints = 5 + caseIndex % 4
      val shared =
        sharedSpatialCase(caseIndex, timepoints, rng)
      val radial =
        radialSpatialCase(caseIndex, timepoints, rng)

      Vector(shared, radial).foreach { synthetic =>
        val result =
          LatentEncoder
            .encode(synthetic.data, synthetic.spec)
            .fold(err => fail(s"${synthetic.id} encode failed: ${err.message}"), identity)
        val response = result.response
        val expectedOffset =
          Some(columnMeans(synthetic.data))

        assertEquals(response.metadata("case"), synthetic.id)
        assertEquals(response.metadata("center"), "true")
        assertResponseRoundtrip(synthetic.id, response, synthetic.data, expectedOffset, 1e-8)

        synthetic.radialSelection.foreach { selection =>
          result match
            case LatentEncodingResult.RadialBasis(encoding) =>
              val selected =
                encoding
                  .decode(selection)
                  .fold(err => fail(s"${synthetic.id} radial selection failed: ${err.message}"), identity)
              assertRowsEqual(
                selected.toRows,
                selectRows(synthetic.data, synthetic.selectedTimepoints, synthetic.selectedSamples),
                1e-8
              )
            case other =>
              fail(s"${synthetic.id} expected radial encoding, found $other")
        }
      }
  }

  private final case class TemporalCase(
      id: String,
      basis: DMat,
      spec: LatentEncodingSpec
  )

  private final case class SpatialCase(
      id: String,
      data: DMat,
      spec: LatentEncodingSpec,
      selectedTimepoints: Vector[Int] = Vector.empty,
      selectedSamples: Vector[Int] = Vector.empty,
      radialSelection: Option[RadialDecodeSelection] = None
  )

  private final class Lcg private (private var state: Long):
    def nextDouble(min: Double, max: Double): Double =
      min + unit() * (max - min)

    private def unit(): Double =
      ((nextLong() >>> 11).toDouble / 9007199254740992.0)

    private def nextLong(): Long =
      state = state * 6364136223846793005L + 1442695040888963407L
      state

  private object Lcg:
    def apply(seed: Long): Lcg =
      new Lcg(seed)

  private def temporalCases(
      timepoints: Int,
      components: Int,
      center: Boolean
  ): Vector[TemporalCase] =
    val provided =
      providedTemporalBasis(timepoints)
    val dct =
      DctBasis
        .build(timepoints, components, DctNorm.Ortho)
        .fold(err => fail(err.message), identity)
    val haar =
      HaarBasis
        .build(timepoints, components)
        .fold(err => fail(err.message), identity)

    Vector(
      TemporalCase(
        id = "synthetic-provided-temporal",
        basis = provided,
        spec = LatentEncodingSpec
          .providedBasis(
            basis = provided,
            center = center,
            metadata = Map("case" -> "synthetic-provided-temporal", "center" -> center.toString)
          )
          .fold(err => fail(err.message), identity)
      ),
      TemporalCase(
        id = "synthetic-dct-temporal",
        basis = dct,
        spec = LatentEncodingSpec
          .dct(
            timepoints = timepoints,
            components = components,
            norm = DctNorm.Ortho,
            center = center,
            metadata = Map("case" -> "synthetic-dct-temporal")
          )
          .fold(err => fail(err.message), identity)
      ),
      TemporalCase(
        id = "synthetic-haar-temporal",
        basis = haar,
        spec = LatentEncodingSpec
          .haar(
            timepoints = timepoints,
            components = components,
            center = center,
            metadata = Map("case" -> "synthetic-haar-temporal")
          )
          .fold(err => fail(err.message), identity)
      )
    )

  private def sharedSpatialCase(
      caseIndex: Int,
      timepoints: Int,
      rng: Lcg
  ): SpatialCase =
    val loadings =
      GaleArchiveTestData.matrixFromRows(
        Vector(
          Vector(1.0, 0.25, -0.2),
          Vector(0.4, 1.0, 0.3),
          Vector(-0.3, 0.2, 1.0),
          Vector(0.8, -0.5, 0.6)
        )
      )
    val artifact =
      SharedBasisArtifact(
        loadings = loadings,
        mask = SharedBasisMask(Vector(6), Vector(true, false, true, true, false, true)),
        kind = "synthetic-shared",
        params = Map("suite" -> "LatentSyntheticRoundtripSuite")
      )
    val coefficients =
      syntheticMatrix(timepoints, loadings.cols, rng, scale = 1.2 + caseIndex.toDouble / 20.0)
    val offset =
      syntheticOffset(loadings.rows, rng)
    val data =
      spatialDataFrom(loadings, coefficients, offset)
    val id =
      s"synthetic-shared-spatial-$caseIndex"
    val spec =
      LatentEncodingSpec
        .sharedBasis(
          basis = artifact,
          basisId = SharedBasisId.unsafe(s"synthetic_shared_$caseIndex"),
          center = true,
          metadata = Map("case" -> id)
        )
        .fold(err => fail(err.message), identity)

    SpatialCase(id, data, spec)

  private def radialSpatialCase(
      caseIndex: Int,
      timepoints: Int,
      rng: Lcg
  ): SpatialCase =
    val space =
      SampleSpaces(Vector(5, 1, 1))
    val active =
      Vector(4, 0, 3, 1)
    val spec =
      RadialBasisSpec(sigma0 = 1.0, levels = 0, seed = 100L + caseIndex.toLong)
        .fold(err => fail(err.message), identity)
    val radial =
      RadialBasis
        .fromSpec(space, active, spec)
        .fold(err => fail(err.message), identity)
    val coefficients =
      syntheticMatrix(timepoints, radial.nAtoms, rng, scale = 1.0 + caseIndex.toDouble / 25.0)
    val offset =
      DVec.fromSeq(syntheticOffset(radial.nVoxels, rng))
    val data =
      radialDataFrom(radial.loadings, coefficients, offset.toVector)
    val id =
      s"synthetic-radial-spatial-$caseIndex"
    val encodingSpec =
      LatentEncodingSpec
        .radialBasis(
          radialBasis = radial,
          maskDims = space.spatialDims,
          basisId = SharedBasisId.unsafe(s"synthetic_radial_$caseIndex"),
          center = true,
          metadata = Map("case" -> id)
        )
        .fold(err => fail(err.message), identity)
    val selection =
      RadialDecodeSelection
        .checked(timepoints = Some(Vector(timepoints - 1, 1)), activeVoxels = Some(Vector(3, 0)))
        .fold(err => fail(err.message), identity)

    SpatialCase(
      id = id,
      data = data,
      spec = encodingSpec,
      selectedTimepoints = Vector(timepoints - 1, 1),
      selectedSamples = Vector(3, 0),
      radialSelection = Some(selection)
    )

  private def providedTemporalBasis(timepoints: Int): DMat =
    LatentNumerics.matrixFromRows(
      Vector.tabulate(timepoints) { time =>
        val x = (time.toDouble - (timepoints.toDouble - 1.0) / 2.0) / timepoints.toDouble
        val alt = if time % 2 == 0 then 0.5 else -0.5
        Vector(1.0, x, x * x, alt)
      }
    )

  private def syntheticMatrix(
      rows: Int,
      cols: Int,
      rng: Lcg,
      scale: Double
  ): DMat =
    LatentNumerics.matrixFromRows(
      Vector.tabulate(rows) { row =>
        Vector.tabulate(cols) { col =>
          val drift = (row.toDouble - col.toDouble) / (rows.toDouble + cols.toDouble)
          nonTiny(rng.nextDouble(-scale, scale) + drift)
        }
      }
    )

  private def syntheticOffset(
      length: Int,
      rng: Lcg
  ): Vector[Double] =
    Vector.tabulate(length) { index =>
      nonTiny(rng.nextDouble(-0.8, 0.8) + index.toDouble / 10.0)
    }

  private def spatialDataFrom(
      loadings: DMat,
      coefficients: DMat,
      offset: Vector[Double]
  ): DMat =
    LatentNumerics.matrixFromRows(
      Vector.tabulate(coefficients.rows) { time =>
        Vector.tabulate(loadings.rows) { sample =>
          var sum = offset(sample)
          var atom = 0
          while atom < loadings.cols do
            sum += coefficients(time, atom) * loadings(sample, atom)
            atom += 1
          sum
        }
      }
    )

  private def radialDataFrom(
      loadings: DMat,
      coefficients: DMat,
      offset: Vector[Double]
  ): DMat =
    LatentNumerics.matrixFromRows(
      Vector.tabulate(coefficients.rows) { time =>
        Vector.tabulate(loadings.rows) { sample =>
          var sum = offset(sample)
          var atom = 0
          while atom < loadings.cols do
            sum += coefficients(time, atom) * loadings(sample, atom)
            atom += 1
          sum
        }
      }
    )

  private def assertResponseRoundtrip(
      label: String,
      response: ExplicitLatentResponse,
      expected: DMat,
      expectedOffset: Option[Vector[Double]],
      tol: Double
  ): Unit =
    val selection =
      LatentSelection(
        timepoints = Some(Vector(expected.rows - 1, 0)),
        samples = Some(Vector(expected.cols - 1, 0))
      )
    val reconstructed =
      response
        .reconstruct()
        .fold(err => fail(s"$label reconstruction failed: ${err.message}"), identity)
    val selected =
      response
        .reconstruct(selection)
        .fold(err => fail(s"$label selected reconstruction failed: ${err.message}"), identity)
    val decoded =
      response
        .decodeCoefficients(response.coefTime.transpose)
        .fold(err => fail(s"$label coefficient decode failed: ${err.message}"), identity)
    val decodedWithOffset =
      addSampleOffset(decoded, response.offset)

    assertEquals(response.shape.timepoints, expected.rows)
    assertEquals(response.shape.samples, expected.cols)
    assert(response.decodeSemantics.coefficientDecodeIsLinearOnly)
    assertEquals(response.decodeSemantics.reconstructionIncludes(LatentMaterializationTerm.SampleOffset), expectedOffset.nonEmpty)
    assertOffset(label, response.offset, expectedOffset, 1e-10)
    assertFinite(label, reconstructed)
    assertFinite(label, selected)
    assertFinite(label, decodedWithOffset)
    assertRowsEqual(reconstructed.toRows, expected.toRows, tol)
    assertRowsEqual(selected.toRows, selectRows(expected, Vector(expected.rows - 1, 0), Vector(expected.cols - 1, 0)), tol)
    assertRowsEqual(decodedWithOffset.toRows, expected.transpose.toRows, tol)

  private def addSampleOffset(
      decoded: DMat,
      offset: Option[DVec]
  ): DMat =
    offset match
      case None =>
        decoded
      case Some(values) =>
        LatentNumerics.matrixFromRows(
          Vector.tabulate(decoded.rows) { row =>
            Vector.tabulate(decoded.cols) { col =>
              decoded(row, col) + values(row)
            }
          }
        )

  private def columnMeans(matrix: DMat): Vector[Double] =
    Vector.tabulate(matrix.cols) { col =>
      var sum = 0.0
      var row = 0
      while row < matrix.rows do
        sum += matrix(row, col)
        row += 1
      sum / matrix.rows.toDouble
    }

  private def assertOffset(
      label: String,
      actual: Option[DVec],
      expected: Option[Vector[Double]],
      tol: Double
  ): Unit =
    (actual, expected) match
      case (None, None) =>
        ()
      case (Some(actualValues), Some(expectedValues)) =>
        assertRowsEqual(Vector(actualValues.toVector), Vector(expectedValues), tol)
      case _ =>
        fail(s"$label offset mismatch: actual=${actual.map(_.toVector)}, expected=$expected")

  private def selectRows(
      matrix: DMat,
      rows: Vector[Int],
      cols: Vector[Int]
  ): Vector[Vector[Double]] =
    rows.map { row =>
      cols.map(col => matrix(row, col))
    }

  private def nonTiny(value: Double): Double =
    if math.abs(value) < 0.05 then value + (if value < 0.0 then -0.05 else 0.05)
    else value

  private def assertFinite(
      label: String,
      matrix: DMat
  ): Unit =
    val data = matrix.copyData
    var i = 0
    while i < data.length do
      assert(data(i).isFinite, clue = s"$label non-finite value at $i")
      i += 1

  private def assertRowsEqual(
      actual: Vector[Vector[Double]],
      expected: Vector[Vector[Double]],
      tol: Double
  ): Unit =
    assertEquals(actual.length, expected.length)
    assertEquals(if actual.isEmpty then 0 else actual.head.length, if expected.isEmpty then 0 else expected.head.length)
    actual.zip(expected).foreach { case (actualRow, expectedRow) =>
      assertEquals(actualRow.length, expectedRow.length)
      actualRow.zip(expectedRow).foreach { case (actualValue, expectedValue) =>
        assertEqualsDouble(actualValue, expectedValue, tol)
      }
    }
