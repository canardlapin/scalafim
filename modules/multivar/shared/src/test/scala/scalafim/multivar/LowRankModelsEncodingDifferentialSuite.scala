package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

class LowRankModelsEncodingDifferentialSuite extends munit.FunSuite:
  private val valueTolerance = 8e-7

  test("frozen-decoder convex encodings match LowRankModels.jl reference solutions"):
    assertEquals(
      LowRankModelsEncodingOracleFixtureData.schemaVersion,
      "scalafim.lowrankmodels.encoding-oracle.v1"
    )
    assertEquals(
      LowRankModelsEncodingOracleFixtureData.provenance,
      LowRankModelsOracleFixtureData.provenance
    )
    assertEquals(
      LowRankModelsEncodingOracleFixtureData.cases.map(_.id).toSet,
      Set("quadratic-dense-ridge", "quadratic-sparse-l1", "logistic-sparse-ridge")
    )
    LowRankModelsEncodingOracleFixtureData.cases.foreach(checkCase)

  test("external encoding objective trajectories are monotone and stationary"):
    LowRankModelsEncodingOracleFixtureData.cases.foreach: fixture =>
      assertEquals(fixture.objectiveTrajectory.length, fixture.iterations + 1, fixture.id)
      fixture.objectiveTrajectory.sliding(2).foreach:
        case Vector(previous, next) =>
          assert(
            next <= previous + 1e-12 * Math.max(1.0, Math.abs(previous)),
            s"${fixture.id} external objective increased from $previous to $next"
          )
        case _ => ()
      assertEqualsDouble(fixture.objectiveTrajectory.last, fixture.totalObjective, 1e-12)
      assert(fixture.proxGradientResidual <= 1e-9, s"${fixture.id} external residual")

  private def checkCase(fixture: LowRankModelsEncodingOracleCase): Unit =
    val rows = space(s"${fixture.id}-new-row", 1)
    val features = space(s"${fixture.id}-features", fixture.data.length)
    val latent = space(s"${fixture.id}-latent", fixture.rank)
    val (domain, loss) = fixture.loss match
      case "quadratic" => FeatureDomain.Real -> EntryLoss.Quadratic
      case "logistic" => FeatureDomain.Binary -> EntryLoss.Logistic
      case other => fail(s"${fixture.id} unsupported encoding loss $other")
    val specifications = Vector.tabulate(fixture.data.length): index =>
      acceptedObservation(
        GlrmFeatureSpec.from(
          GlrmFeatureId.unsafe(s"${fixture.id}-feature-$index"),
          domain,
          loss
        )
      )
    val layout = acceptedObservation(
      GlrmFeatureLayout.from(features.evidence, specifications, id(s"${fixture.id}-layout"))
    )
    val decoder = acceptedObservation(
      FeatureDecoder.from(
        layout,
        latent.evidence,
        matrix(fixture.decoder).t,
        id(s"${fixture.id}-decoder")
      )
    )
    val encoder = acceptedEncoding(
      FittedLatentEncoder.from(decoder, Vector(rowPenalty(fixture)))
    )
    val observations =
      if fixture.observed.forall(identity) then
        acceptedObservation(
          ObservationPattern.from(
            rows.evidence,
            features.evidence,
            fixture.data.map(ObservationCell.Observed(_)),
            id(s"${fixture.id}-dense-observations")
          )
        )
      else
        val entries = fixture.data.indices.collect:
          case feature if fixture.observed(feature) =>
            acceptedObservation(
              SparsePointObservation.from(feature, fixture.data.length, fixture.data(feature))
            )
        .toVector
        acceptedObservation(
          ObservationPattern.singleRowSparse(
            rows.evidence,
            features.evidence,
            entries,
            ObservationReason.unsafe("withheld by LowRankModels encoding fixture"),
            id(s"${fixture.id}-sparse-observations")
          )
        )
    val result = acceptedEncoding(encoder.encode(observations, Some(vector(fixture.initial))))

    assertVectorClose(result.code.values, fixture.solution, valueTolerance, s"${fixture.id} code")
    assertEquals(
      result.support.features.indices,
      fixture.observed.indices.filter(fixture.observed).toVector,
      fixture.id
    )
    assertEquals(result.support.observedCount, fixture.observed.count(identity), fixture.id)
    assertEqualsDouble(result.objective.observedEntryLoss, fixture.observedEntryLoss, valueTolerance)
    assertEqualsDouble(result.objective.rowPenalty, fixture.rowPenalty, valueTolerance)
    assertEqualsDouble(result.objective.total, fixture.totalObjective, valueTolerance)
    assert(result.certificate.proxGradientResidual <= 8e-7, s"${fixture.id} ScalaFIM residual")
    assert(result.achievement.isInstanceOf[LatentEncodingAchievement.EpsilonStationary])
    assert(result.uniqueness.isInstanceOf[LatentCodeUniqueness.UniqueByStrongConvexity])

    result.decoded.zipWithIndex.foreach:
      case (feature, index) =>
        feature.prediction match
          case DecodedPrediction.Point(value) =>
            assertEquals(fixture.loss, "quadratic")
            assertEqualsDouble(value, fixture.decoded(index), valueTolerance)
          case DecodedPrediction.Binary(probability) =>
            assertEquals(fixture.loss, "logistic")
            assertEqualsDouble(probability.value, fixture.decoded(index), valueTolerance)
          case other => fail(s"${fixture.id} unexpected decoded prediction $other")

    val scalaNatural = fixture.decoder.map(row => dot(fixture.solution, row))
    assertVectorClose(scalaNatural, fixture.naturalParameters, 2e-11, s"${fixture.id} external natural")

  private def rowPenalty(fixture: LowRankModelsEncodingOracleCase): GlrmFactorPenaltyTerm =
    val functional = fixture.penalty.kind match
      case LowRankModelsOraclePenaltyKind.L1 => GlrmFactorPenalty.ElementwiseL1
      case LowRankModelsOraclePenaltyKind.SquaredFrobenius => GlrmFactorPenalty.SquaredFrobenius
      case LowRankModelsOraclePenaltyKind.NoPenalty => fail(s"${fixture.id} requires a row penalty")
    GlrmFactorPenaltyTerm(
      GlrmFactorTarget.RowCodes,
      functional,
      PenaltyWeight.unsafe(fixture.penalty.scalaWeight),
      id(s"${fixture.id}-row-penalty")
    )

  private def dot(left: Vector[Double], right: Vector[Double]): Double =
    left.zip(right).map(_ * _).sum

  private def assertVectorClose(
      actual: DVec,
      expected: Vector[Double],
      tolerance: Double,
      label: String
  ): Unit =
    assertEquals(actual.length, expected.length, label)
    var index = 0
    while index < expected.length do
      assertEqualsDouble(actual(index), expected(index), tolerance)
      index += 1

  private def assertVectorClose(
      actual: Vector[Double],
      expected: Vector[Double],
      tolerance: Double,
      label: String
  ): Unit =
    assertEquals(actual.length, expected.length, label)
    actual.indices.foreach(index => assertEqualsDouble(actual(index), expected(index), tolerance))

  private def matrix(values: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(values)

  private def vector(values: Vector[Double]): DVec =
    GaleNumerics.vectorFromArray(values.toArray)

  private def space(name: String, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(name), SpaceRole.Observed, Dimension.unsafe(dimension)))

  private def id(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))

  private def acceptedObservation[A](value: Either[GeneralizedLowRankError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedEncoding[A](value: Either[LatentEncodingError, A]): A =
    value.fold(error => fail(error.message), identity)
