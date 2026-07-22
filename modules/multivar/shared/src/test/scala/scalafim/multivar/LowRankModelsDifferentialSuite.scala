package scalafim.multivar

import gale.linalg.DMat

class LowRankModelsDifferentialSuite extends munit.FunSuite:
  private val tolerance = 1e-12

  test("fixture provenance pins the reviewed LowRankModels.jl implementation"):
    assertEquals(LowRankModelsOracleFixtureData.schemaVersion, "scalafim.lowrankmodels.oracle.v1")
    assertEquals(
      LowRankModelsOracleFixtureData.provenance.repository,
      "https://github.com/madeleineudell/LowRankModels.jl.git"
    )
    assertEquals(
      LowRankModelsOracleFixtureData.provenance.commit,
      "a18f0df45f1a6ce37634bf4e347062b6090397eb"
    )
    assertEquals(LowRankModelsOracleFixtureData.provenance.juliaVersion, "1.6.7")
    assertEquals(LowRankModelsOracleFixtureData.provenance.packageVersion, "1.1.1")
    assertEquals(
      LowRankModelsOracleFixtureData.cases.map(_.id).toSet,
      Set(
        "quadratic-masked-rank2",
        "logistic-masked-rank2",
        "poisson-masked-rank2",
        "categorical-masked-rank2"
      )
    )

  test("scalar GLRM objectives, gradients, and decoders match LowRankModels.jl"):
    val cases = LowRankModelsOracleFixtureData.cases.collect:
      case fixture: LowRankModelsScalarOracleCase => fixture
    assertEquals(cases.length, 3)
    cases.foreach(checkScalarCase)

  test("categorical GLRM objective, softmax gauge, gradients, and mask match LowRankModels.jl"):
    val fixture = LowRankModelsOracleFixtureData.cases.collectFirst:
      case value: LowRankModelsCategoricalOracleCase => value
    fixture match
      case None => fail("missing categorical LowRankModels.jl fixture")
      case Some(value) => checkCategoricalCase(value)

  private def checkScalarCase(fixture: LowRankModelsScalarOracleCase): Unit =
    val rows = space(s"${fixture.id}-rows", fixture.data.length)
    val features = space(s"${fixture.id}-features", fixture.data.head.length)
    val latent = space(s"${fixture.id}-latent", fixture.rank)
    val (domain, loss) = fixture.loss match
      case "quadratic" => FeatureDomain.Real -> EntryLoss.Quadratic
      case "logistic" => FeatureDomain.Binary -> EntryLoss.Logistic
      case "poisson" => FeatureDomain.Count -> EntryLoss.Poisson
      case other => fail(s"unsupported scalar fixture loss $other")
    val specifications = Vector.tabulate(features.evidence.dimension): index =>
      accepted(
        GlrmFeatureSpec.from(
          GlrmFeatureId.unsafe(s"${fixture.id}-feature-$index"),
          domain,
          loss
        )
      )
    val layout = accepted(
      GlrmFeatureLayout.from(features.evidence, specifications, id(s"${fixture.id}-layout"))
    )
    val pattern = accepted(
      ObservationPattern.from(
        rows.evidence,
        features.evidence,
        scalarCells(fixture),
        id(s"${fixture.id}-observations")
      )
    )
    val program = accepted(
      GeneralizedLowRankProgram.from(
        pattern,
        layout,
        penalties(fixture),
        MissingnessStatement.Unspecified,
        GlrmPredictionTarget.ReconstructObserved
      )
    )
    val factors = factorsFor(fixture, rows, features, latent)(layout)
    val objective = accepted(program.evaluate(factors))
    val scalaResponseConstant =
      if fixture.loss == "poisson" then exactPoissonResponseConstant(fixture)
      else 0.0

    assertClose(
      objective.observedEntryLoss - scalaResponseConstant,
      fixture.objective.observedEntryLoss - fixture.objective.responseConstant,
      s"${fixture.id} common observed loss"
    )
    assertClose(objective.rowPenalty, fixture.objective.rowPenalty, s"${fixture.id} row penalty")
    assertClose(objective.decoderPenalty, fixture.objective.decoderPenalty, s"${fixture.id} decoder penalty")
    assertClose(
      objective.total - scalaResponseConstant,
      fixture.objective.total - fixture.objective.responseConstant,
      s"${fixture.id} common total"
    )

    var row = 0
    while row < fixture.data.length do
      var feature = 0
      while feature < fixture.data(row).length do
        val natural = factors.decoder.natural(factors.rowCodes.values, row, feature)
        assertClose(natural(0), fixture.naturalParameters(row)(feature), s"${fixture.id} natural[$row,$feature]")
        fixture.naturalGradients(row)(feature) match
          case None => assert(!fixture.observed(row)(feature), s"${fixture.id} unexpected missing gradient")
          case Some(expected) =>
            assert(fixture.observed(row)(feature), s"${fixture.id} observed gradient mask")
            val specification = specifications(feature)
            val embedding = accepted(FeatureEmbedding.from(specification.id, domain, row, fixture.data(row)(feature)))
            val actual = accepted(loss.naturalGradient(specification.id, embedding, natural))
            assertClose(actual(0), expected, s"${fixture.id} gradient[$row,$feature]")
        accepted(factors.decoded(row, feature)) match
          case DecodedPrediction.Point(value) =>
            assertEquals(fixture.loss, "quadratic")
            assertClose(value, fixture.decoded(row)(feature), s"${fixture.id} decoded[$row,$feature]")
          case DecodedPrediction.Binary(probability) =>
            assertEquals(fixture.loss, "logistic")
            assertClose(probability.value, fixture.decoded(row)(feature), s"${fixture.id} decoded[$row,$feature]")
          case DecodedPrediction.ExpectedCount(mean) =>
            assertEquals(fixture.loss, "poisson")
            assertClose(mean, fixture.decoded(row)(feature), s"${fixture.id} decoded[$row,$feature]")
          case other => fail(s"${fixture.id} unexpected scalar decoder $other")
        feature += 1
      row += 1

  private def checkCategoricalCase(fixture: LowRankModelsCategoricalOracleCase): Unit =
    val rows = space(s"${fixture.id}-rows", fixture.data.length)
    val features = space(s"${fixture.id}-features", 1)
    val latent = space(s"${fixture.id}-latent", fixture.rank)
    val levels = CategoryLevels.unsafe(Vector.tabulate(fixture.levels)(index => s"level-$index"))
    val domain = FeatureDomain.Categorical(levels)
    val loss = EntryLoss.CategoricalCrossEntropy
    val featureId = GlrmFeatureId.unsafe(s"${fixture.id}-feature")
    val specification = accepted(GlrmFeatureSpec.from(featureId, domain, loss))
    val layout = accepted(
      GlrmFeatureLayout.from(features.evidence, Vector(specification), id(s"${fixture.id}-layout"))
    )
    val cells = fixture.data.indices.toVector.map: row =>
      if fixture.observed(row) then ObservationCell.Observed(fixture.data(row).toDouble)
      else ObservationCell.Missing(ObservationReason.unsafe("withheld by LowRankModels fixture"))
    val pattern = accepted(
      ObservationPattern.from(rows.evidence, features.evidence, cells, id(s"${fixture.id}-observations"))
    )
    val program = accepted(
      GeneralizedLowRankProgram.from(
        pattern,
        layout,
        penalties(fixture),
        MissingnessStatement.Unspecified,
        GlrmPredictionTarget.ReconstructObserved
      )
    )
    val factors = factorsFor(fixture, rows, features, latent)(layout)
    val objective = accepted(program.evaluate(factors))
    assertClose(objective.observedEntryLoss, fixture.objective.observedEntryLoss, s"${fixture.id} observed loss")
    assertClose(objective.rowPenalty, fixture.objective.rowPenalty, s"${fixture.id} row penalty")
    assertClose(objective.decoderPenalty, fixture.objective.decoderPenalty, s"${fixture.id} decoder penalty")
    assertClose(objective.total, fixture.objective.total, s"${fixture.id} total")

    var row = 0
    while row < fixture.data.length do
      val natural = factors.decoder.natural(factors.rowCodes.values, row, 0)
      assertVectorClose(natural, fixture.naturalParameters(row), s"${fixture.id} natural[$row]")
      if fixture.observed(row) then
        val embedding = accepted(FeatureEmbedding.from(featureId, domain, row, fixture.data(row).toDouble))
        val gradient = accepted(loss.naturalGradient(featureId, embedding, natural))
        val expected = fixture.naturalGradients(row).map:
          case Some(value) => value
          case None => fail(s"${fixture.id} observed row $row has null gradient")
        assertVectorClose(gradient, expected, s"${fixture.id} gradient[$row]")
      else assert(fixture.naturalGradients(row).forall(_.isEmpty), s"${fixture.id} missing-row gradient")
      accepted(factors.decoded(row, 0)) match
        case DecodedPrediction.Categorical(actualLevels, probabilities) =>
          assertEquals(actualLevels.values, levels.values)
          assertVectorClose(
            probabilities.map(_.value),
            fixture.decoded(row),
            s"${fixture.id} decoded[$row]"
          )
        case other => fail(s"${fixture.id} unexpected categorical decoder $other")
      row += 1

  private def scalarCells(fixture: LowRankModelsScalarOracleCase): Vector[ObservationCell] =
    fixture.data.indices.toVector.flatMap: row =>
      fixture.data(row).indices.toVector.map: feature =>
        if fixture.observed(row)(feature) then ObservationCell.Observed(fixture.data(row)(feature))
        else ObservationCell.Missing(ObservationReason.unsafe("withheld by LowRankModels fixture"))

  private def penalties(fixture: LowRankModelsOracleCase): Vector[GlrmFactorPenaltyTerm] =
    Vector(
      penalty(fixture.id, GlrmFactorTarget.RowCodes, fixture.rowPenalty),
      penalty(fixture.id, GlrmFactorTarget.FeatureDecoder, fixture.decoderPenalty)
    ).flatten

  private def penalty(
      fixtureId: String,
      target: GlrmFactorTarget,
      fixture: LowRankModelsOraclePenalty
  ): Option[GlrmFactorPenaltyTerm] =
    fixture.kind match
      case LowRankModelsOraclePenaltyKind.NoPenalty => None
      case LowRankModelsOraclePenaltyKind.L1 =>
        Some(
          GlrmFactorPenaltyTerm(
            target,
            GlrmFactorPenalty.ElementwiseL1,
            PenaltyWeight.unsafe(fixture.scalaWeight),
            id(s"$fixtureId-${target.toString}-l1")
          )
        )
      case LowRankModelsOraclePenaltyKind.SquaredFrobenius =>
        Some(
          GlrmFactorPenaltyTerm(
            target,
            GlrmFactorPenalty.SquaredFrobenius,
            PenaltyWeight.unsafe(fixture.scalaWeight),
            id(s"$fixtureId-${target.toString}-ridge")
          )
        )

  private def factorsFor(
      fixture: LowRankModelsOracleCase,
      rows: SpaceRef,
      features: SpaceRef,
      latent: SpaceRef
  )(
      layout: GlrmFeatureLayout[features.Id]
  ): GlrmFactors[rows.Id, features.Id, latent.Id] =
    val rowCodes = accepted(
      GlrmRowCodes.from(
        rows.evidence,
        latent.evidence,
        matrix(fixture.rowCodes),
        id(s"${fixture.id}-row-codes")
      )
    )
    val decoder = accepted(
      FeatureDecoder.from(
        layout,
        latent.evidence,
        matrix(fixture.decoder).t,
        id(s"${fixture.id}-decoder")
      )
    )
    accepted(GlrmFactors.from(rowCodes, decoder))

  private def exactPoissonResponseConstant(fixture: LowRankModelsScalarOracleCase): Double =
    var result = 0.0
    var row = 0
    while row < fixture.data.length do
      var feature = 0
      while feature < fixture.data(row).length do
        if fixture.observed(row)(feature) then
          val count = fixture.data(row)(feature).toInt
          var value = 2
          while value <= count do
            result += Math.log(value.toDouble)
            value += 1
        feature += 1
      row += 1
    result

  private def assertVectorClose(
      actual: gale.linalg.DVec,
      expected: Vector[Double],
      label: String
  ): Unit =
    assertEquals(actual.length, expected.length, label)
    var index = 0
    while index < expected.length do
      assertClose(actual(index), expected(index), s"$label[$index]")
      index += 1

  private def assertVectorClose(
      actual: Vector[Double],
      expected: Vector[Double],
      label: String
  ): Unit =
    assertEquals(actual.length, expected.length, label)
    actual.indices.foreach(index => assertClose(actual(index), expected(index), s"$label[$index]"))

  private def assertClose(actual: Double, expected: Double, label: String): Unit =
    assert(
      Math.abs(actual - expected) <= tolerance + tolerance * Math.abs(expected),
      s"$label: expected $expected, got $actual"
    )

  private def matrix(values: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(values)

  private def space(name: String, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(name), SpaceRole.Observed, Dimension.unsafe(dimension)))

  private def id(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))

  private def accepted[A](value: Either[GeneralizedLowRankError, A]): A =
    value.fold(error => fail(error.message), identity)
