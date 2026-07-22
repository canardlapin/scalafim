package scalafim.multivar
package family.glrm

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*
import scalafim.multivar.family.glrm.*

import gale.linalg.DMat
import gale.linalg.DVec

class GeneralizedLowRankModelSuite extends munit.FunSuite:

  test("feature domains and entry losses expose checked dimensions, convexity, and gauges"):
    val ordered = accepted(OrderedLevels.from(Vector("low", "middle", "high")))
    val categories = accepted(CategoryLevels.from(Vector("red", "green", "blue")))

    assertEquals(FeatureDomain.Ordinal(ordered).naturalDimension, 2)
    assertEquals(FeatureDomain.Categorical(categories).naturalDimension, 3)
    assertEquals(EntryLoss.Quadratic.convexity, LossConvexity.StrictlyConvexInNaturalParameter)
    assertEquals(EntryLoss.Huber(HuberDelta.unsafe(1.0)).convexity, LossConvexity.ConvexInNaturalParameter)
    assertEquals(EntryLoss.CategoricalCrossEntropy.gauge, NaturalParameterGauge.CommonShiftInvariant)
    assertEquals(
      EntryLoss.OrdinalCumulativeLogit.naturalDomain(FeatureDomain.Ordinal(ordered)),
      NaturalParameterDomain.OrderedNonIncreasing(2)
    )
    assert(OrderedLevels.from(Vector("same", "same")).isLeft)
    assert(CategoryLevels.from(Vector("only-one")).isLeft)
    assert(ObservationWeight(0.0).isLeft)

    val mismatch = GlrmFeatureSpec.from(
      GlrmFeatureId.unsafe("binary-as-gaussian"),
      FeatureDomain.Binary,
      EntryLoss.Quadratic
    )
    assert(mismatch.left.exists(_.isInstanceOf[GeneralizedLowRankError.LossDomainMismatch]))

  test("continuous, binary, count, ordinal, and categorical losses match analytic oracles"):
    val real = GlrmFeatureId.unsafe("real")
    val binary = GlrmFeatureId.unsafe("binary")
    val count = GlrmFeatureId.unsafe("count")
    val ordinal = GlrmFeatureId.unsafe("ordinal")
    val categorical = GlrmFeatureId.unsafe("categorical")
    val ordered = OrderedLevels.unsafe(Vector("low", "middle", "high"))
    val categories = CategoryLevels.unsafe(Vector("red", "green", "blue"))

    val quadratic = loss(
      EntryLoss.Quadratic,
      real,
      FeatureDomain.Real,
      observed = 2.0,
      natural = Vector(3.0)
    )
    val huber = loss(
      EntryLoss.Huber(HuberDelta.unsafe(1.0)),
      real,
      FeatureDomain.Real,
      observed = 0.0,
      natural = Vector(3.0)
    )
    val logistic = loss(
      EntryLoss.Logistic,
      binary,
      FeatureDomain.Binary,
      observed = 1.0,
      natural = Vector(0.0)
    )
    val poisson = loss(
      EntryLoss.Poisson,
      count,
      FeatureDomain.Count,
      observed = 2.0,
      natural = Vector(Math.log(2.0))
    )
    val ordinalLoss = loss(
      EntryLoss.OrdinalCumulativeLogit,
      ordinal,
      FeatureDomain.Ordinal(ordered),
      observed = 1.0,
      natural = Vector(1.0, -1.0)
    )
    val categoricalLoss = loss(
      EntryLoss.CategoricalCrossEntropy,
      categorical,
      FeatureDomain.Categorical(categories),
      observed = 1.0,
      natural = Vector(0.0, Math.log(2.0), 0.0)
    )

    assertEqualsDouble(quadratic, 0.5, 1e-14)
    assertEqualsDouble(huber, 2.5, 1e-14)
    assertEqualsDouble(logistic, Math.log(2.0), 1e-14)
    assertEqualsDouble(poisson, 2.0 - Math.log(2.0), 1e-14)
    assertEqualsDouble(ordinalLoss, 2.0 * Math.log1p(Math.exp(-1.0)), 1e-14)
    assertEqualsDouble(categoricalLoss, Math.log(2.0), 1e-14)

  test("decoded predictions retain feature-domain meaning"):
    val ordered = OrderedLevels.unsafe(Vector("low", "middle", "high"))
    val categories = CategoryLevels.unsafe(Vector("red", "green", "blue"))
    val binary = accepted(
      EntryLoss.Logistic.decode(
        GlrmFeatureId.unsafe("binary"),
        FeatureDomain.Binary,
        vector(Vector(0.0))
      )
    )
    val count = accepted(
      EntryLoss.Poisson.decode(
        GlrmFeatureId.unsafe("count"),
        FeatureDomain.Count,
        vector(Vector(Math.log(2.0)))
      )
    )
    val ordinal = accepted(
      EntryLoss.OrdinalCumulativeLogit.decode(
        GlrmFeatureId.unsafe("ordinal"),
        FeatureDomain.Ordinal(ordered),
        vector(Vector(Math.log(3.0), -Math.log(3.0)))
      )
    )
    val categorical = accepted(
      EntryLoss.CategoricalCrossEntropy.decode(
        GlrmFeatureId.unsafe("categorical"),
        FeatureDomain.Categorical(categories),
        vector(Vector(0.0, Math.log(2.0), 0.0))
      )
    )

    binary match
      case DecodedPrediction.Binary(probability) => assertEqualsDouble(probability.value, 0.5, 1e-14)
      case other => fail(s"expected binary prediction, got $other")
    count match
      case DecodedPrediction.ExpectedCount(mean) => assertEqualsDouble(mean, 2.0, 1e-14)
      case other => fail(s"expected count prediction, got $other")
    ordinal match
      case DecodedPrediction.Ordinal(actualLevels, probabilities) =>
        assertEquals(actualLevels.values, ordered.values)
        assertVectorClose(probabilities.map(_.value), Vector(0.25, 0.5, 0.25), 1e-14)
      case other => fail(s"expected ordinal prediction, got $other")
    categorical match
      case DecodedPrediction.Categorical(actualLevels, probabilities) =>
        assertEquals(actualLevels.values, categories.values)
        assertVectorClose(probabilities.map(_.value), Vector(0.25, 0.5, 0.25), 1e-14)
      case other => fail(s"expected categorical prediction, got $other")

    val incoherent = EntryLoss.OrdinalCumulativeLogit.decode(
      GlrmFeatureId.unsafe("incoherent-ordinal"),
      FeatureDomain.Ordinal(ordered),
      vector(Vector(-1.0, 1.0))
    )
    assert(incoherent.left.exists(_.isInstanceOf[GeneralizedLowRankError.InvalidNaturalParameter]))

  test("observation patterns distinguish zero, missing, structural, censored, and weighted cells"):
    val rows = space("observation-state-rows", 2)
    val features = space("observation-state-features", 3)
    val censoring = accepted(CensoringInterval.between(-1.0, 1.0))
    val pattern = accepted(
      ObservationPattern.from[rows.Id, features.Id](
        rows.evidence,
        features.evidence,
        Vector(
          ObservationCell.Observed(0.0),
          ObservationCell.Missing(ObservationReason.unsafe("not measured")),
          ObservationCell.StructurallyInapplicable(ObservationReason.unsafe("instrument branch")),
          ObservationCell.Censored(censoring),
          ObservationCell.Observed(2.0, ObservationWeight.unsafe(0.5)),
          ObservationCell.Observed(1.0)
        ),
        id("observation-state-pattern")
      )
    )

    assertEquals(pattern.observedCount, 3)
    assertEquals(pattern.missingCount, 1)
    assertEquals(pattern.structurallyInapplicableCount, 1)
    assertEquals(pattern.censoredCount, 1)
    assert(!pattern.isPointComplete)
    assert(pattern.cell(-1, 0).left.exists(_.isInstanceOf[GeneralizedLowRankError.IndexOutOfBounds]))
    assert(pattern.cell(0, 3).left.exists(_.isInstanceOf[GeneralizedLowRankError.IndexOutOfBounds]))

  test("the GLRM objective sums only declared observed losses and keeps factor penalties separate"):
    val fixture = quadraticFixture("masked-objective", 2, 2)
    val pattern = accepted(
      ObservationPattern.from[fixture.rows.Id, fixture.features.Id](
        fixture.rows.evidence,
        fixture.features.evidence,
        Vector(
          ObservationCell.Observed(0.0, ObservationWeight.unsafe(2.0)),
          ObservationCell.Missing(ObservationReason.unsafe("withheld")),
          ObservationCell.StructurallyInapplicable(ObservationReason.unsafe("design")),
          ObservationCell.Observed(2.0, ObservationWeight.unsafe(0.5))
        ),
        id("masked-objective-pattern")
      )
    )
    val rowPenalty = GlrmFactorPenaltyTerm(
      GlrmFactorTarget.RowCodes,
      GlrmFactorPenalty.ElementwiseL1,
      PenaltyWeight.unsafe(0.1),
      id("masked-row-l1")
    )
    val decoderPenalty = GlrmFactorPenaltyTerm(
      GlrmFactorTarget.FeatureDecoder,
      GlrmFactorPenalty.SquaredFrobenius,
      PenaltyWeight.unsafe(0.2),
      id("masked-decoder-ridge")
    )
    val program = accepted(
      GeneralizedLowRankProgram.from(
        pattern,
        fixture.layout,
        Vector(rowPenalty, decoderPenalty),
        MissingnessStatement.StructuralByDesign(ObservationReason.unsafe("fixture design")),
        GlrmPredictionTarget.ImputeDeclaredMissing
      )
    )
    val factors = fixture.factors(
      matrix(Vector(Vector(1.0), Vector(2.0))),
      matrix(Vector(Vector(1.0, 1.0)))
    )
    val objective = accepted(program.evaluate(factors))

    assertEqualsDouble(objective.observedEntryLoss, 1.0, 1e-14)
    assertEqualsDouble(objective.rowPenalty, 0.3, 1e-14)
    assertEqualsDouble(objective.decoderPenalty, 0.2, 1e-14)
    assertEqualsDouble(objective.total, 1.5, 1e-14)
    assert(!program.missingness.grantsInferentialClaim)
    assertEquals(program.predictionTarget, GlrmPredictionTarget.ImputeDeclaredMissing)
    assert(factors.decoded(2, 0).left.exists(_.isInstanceOf[GeneralizedLowRankError.IndexOutOfBounds]))

  test("all-observed quadratic GLRM reduces exactly to the PCA-style reconstruction loss"):
    val fixture = quadraticFixture("quadratic-pca-reduction", 2, 2)
    val observed = matrix(Vector(Vector(3.0, 0.0), Vector(0.0, 2.0)))
    val cells = Vector.tabulate(4): index =>
      ObservationCell.Observed(observed(index / 2, index % 2))
    val pattern = accepted(
      ObservationPattern.from[fixture.rows.Id, fixture.features.Id](
        fixture.rows.evidence,
        fixture.features.evidence,
        cells,
        id("quadratic-pca-complete-pattern")
      )
    )
    val program = accepted(
      GeneralizedLowRankProgram.from(
        pattern,
        fixture.layout,
        Vector.empty,
        MissingnessStatement.Complete,
        GlrmPredictionTarget.ReconstructObserved
      )
    )
    val factors = fixture.factors(
      matrix(Vector(Vector(3.0), Vector(0.0))),
      matrix(Vector(Vector(1.0, 0.0)))
    )
    val objective = accepted(program.evaluate(factors))
    val reconstruction = matrix(Vector(Vector(3.0, 0.0), Vector(0.0, 0.0)))
    var squaredResidual = 0.0
    var row = 0
    while row < observed.rows do
      var column = 0
      while column < observed.cols do
        val residual = observed(row, column) - reconstruction(row, column)
        squaredResidual += residual * residual
        column += 1
      row += 1

    assertEqualsDouble(objective.total, 0.5 * squaredResidual, 1e-14)
    assertEqualsDouble(objective.total, 2.0, 1e-14)

  test("invalid observed domains and false complete-data declarations fail before evaluation"):
    val rows = space("domain-error-rows", 1)
    val features = space("domain-error-features", 1)
    val binary = accepted(
      GlrmFeatureSpec.from(GlrmFeatureId.unsafe("binary"), FeatureDomain.Binary, EntryLoss.Logistic)
    )
    val layout = accepted(
      GlrmFeatureLayout.from(features.evidence, Vector(binary), id("domain-error-layout"))
    )
    val invalidPattern = accepted(
      ObservationPattern.from[rows.Id, features.Id](
        rows.evidence,
        features.evidence,
        Vector(ObservationCell.Observed(2.0)),
        id("domain-error-pattern")
      )
    )
    val missingPattern = accepted(
      ObservationPattern.from[rows.Id, features.Id](
        rows.evidence,
        features.evidence,
        Vector(ObservationCell.Missing(ObservationReason.unsafe("absent"))),
        id("false-complete-pattern")
      )
    )

    assert(
      GeneralizedLowRankProgram
        .from(
          invalidPattern,
          layout,
          Vector.empty,
          MissingnessStatement.Unspecified,
          GlrmPredictionTarget.ReconstructObserved
        )
        .left
        .exists(_.isInstanceOf[GeneralizedLowRankError.InvalidObservedValue])
    )
    assert(
      GeneralizedLowRankProgram
        .from(
          missingPattern,
          layout,
          Vector.empty,
          MissingnessStatement.Complete,
          GlrmPredictionTarget.ReconstructObserved
        )
        .left
        .exists:
          case GeneralizedLowRankError.InvalidDefinition(detail) => detail.contains("complete")
          case _ => false
    )

  test("censored observations require an explicit censoring likelihood and are never skipped as missing"):
    val fixture = quadraticFixture("censoring-boundary", 2, 1)
    val interval = accepted(CensoringInterval.atOrBelow(0.0))
    val pattern = accepted(
      ObservationPattern.from[fixture.rows.Id, fixture.features.Id](
        fixture.rows.evidence,
        fixture.features.evidence,
        Vector(ObservationCell.Observed(1.0), ObservationCell.Censored(interval)),
        id("censoring-pattern")
      )
    )
    val program = accepted(
      GeneralizedLowRankProgram.from(
        pattern,
        fixture.layout,
        Vector.empty,
        MissingnessStatement.Unspecified,
        GlrmPredictionTarget.ReconstructObserved
      )
    )
    val factors = fixture.factors(
      matrix(Vector(Vector(1.0), Vector(1.0))),
      matrix(Vector(Vector(1.0)))
    )

    assertEquals(pattern.censoredCount, 1)
    assert(program.evaluate(factors).left.exists(_.isInstanceOf[GeneralizedLowRankError.UnsupportedCensoring]))

  private final class QuadraticFixture(
      val name: String,
      val rows: SpaceRef,
      val features: SpaceRef,
      val latent: SpaceRef,
      val layout: GlrmFeatureLayout[features.Id]
  ):
    def factors(
        rowValues: DMat,
        decoderValues: DMat
    ): GlrmFactors[rows.Id, features.Id, latent.Id] =
      val rowCodes = accepted(
        GlrmRowCodes.from(rows.evidence, latent.evidence, rowValues, id(s"$name-row-codes"))
      )
      val decoder = accepted(
        FeatureDecoder.from(layout, latent.evidence, decoderValues, id(s"$name-decoder"))
      )
      accepted(GlrmFactors.from(rowCodes, decoder))

  private def quadraticFixture(name: String, rowCount: Int, featureCount: Int): QuadraticFixture =
    quadraticFixtureForSpaces(name, space(s"$name-rows", rowCount), space(s"$name-features", featureCount))

  private def quadraticFixtureForSpaces(
      name: String,
      rows: SpaceRef,
      features: SpaceRef
  ): QuadraticFixture =
    val latent = space(s"$name-latent", 1)
    val specifications = Vector.tabulate(features.evidence.dimension): index =>
      accepted(
        GlrmFeatureSpec.from(
          GlrmFeatureId.unsafe(s"$name-feature-$index"),
          FeatureDomain.Real,
          EntryLoss.Quadratic
        )
      )
    val layout = accepted(
      GlrmFeatureLayout.from(features.evidence, specifications, id(s"$name-layout"))
    )
    new QuadraticFixture(name, rows, features, latent, layout)

  private def loss(
      loss: EntryLoss,
      feature: GlrmFeatureId,
      domain: FeatureDomain,
      observed: Double,
      natural: Vector[Double]
  ): Double =
    val embedding = accepted(FeatureEmbedding.from(feature, domain, row = 0, observed))
    accepted(loss.value(feature, embedding, vector(natural)))

  private def assertVectorClose(actual: Vector[Double], expected: Vector[Double], tolerance: Double): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).foreach:
      case (left, right) => assertEqualsDouble(left, right, tolerance)

  private def matrix(values: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(values)

  private def vector(values: Vector[Double]): DVec =
    GaleNumerics.vectorFromArray(values.toArray)

  private def space(name: String, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(name), SpaceRole.Observed, Dimension.unsafe(dimension)))

  private def id(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))

  private def accepted[A](value: Either[GeneralizedLowRankError, A]): A =
    value.fold(error => fail(error.message), identity)
