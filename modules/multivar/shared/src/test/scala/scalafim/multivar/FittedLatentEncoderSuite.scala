package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec
import scalafim.linalg.FirstOrderConfig
import scalafim.linalg.FirstOrderTolerance

class FittedLatentEncoderSuite extends munit.FunSuite:

  test("ridge encoding matches the analytic one-dimensional least-squares solution"):
    val fixture = quadraticFixture("ridge-code", latentDimension = 1, decoder = matrix(Vector(Vector(2.0))))
    val encoder = fixture.encoder(Vector(fixture.rowRidge(1.0)))
    val pattern = fixture.densePattern(Vector(ObservationCell.Observed(3.0)), "ridge-code-pattern")
    val result = accepted(encoder.encode(pattern))

    assertEqualsDouble(result.code.values(0), 6.0 / 5.0, 2e-7)
    assertEquals(result.support.features.indices, Vector(0))
    assertEqualsDouble(result.objective.total, 0.9, 2e-7)
    assert(result.certificate.proxGradientResidual <= 3e-7)
    assert(result.achievedGuarantee.isInstanceOf[AchievedOptimizationGuarantee.Stationary])
    assertEquals(
      result.achievedGuarantee.semanticEvidence.bindings.program,
      encoder.encoderIdentity
    )
    assertEquals(
      result.achievedGuarantee.semanticEvidence.bindings.data,
      pattern.valueIdentity
    )
    assertEquals(
      result.achievedGuarantee.semanticEvidence.bindings.mask,
      ObservationMaskIdentity.Observed(pattern.valueIdentity)
    )
    assertEquals(
      result.achievedGuarantee.semanticEvidence.bindings.result,
      result.resultIdentity
    )
    result.uniqueness match
      case LatentCodeUniqueness.UniqueByStrongConvexity(modulus) =>
        assertEqualsDouble(modulus.doubleValue, 1.0, 1e-14)
      case other => fail(s"expected strong-convexity uniqueness, got $other")
    result.decoded.head.prediction match
      case DecodedPrediction.Point(value) => assertEqualsDouble(value, 12.0 / 5.0, 2e-7)
      case other => fail(s"expected point prediction, got $other")

  test("full-rank unregularized encoding matches the analytic least-squares code"):
    val fixture = quadraticFixture(
      "full-rank-code",
      latentDimension = 2,
      decoder = matrix(Vector(Vector(1.0, 0.0), Vector(0.0, 1.0)))
    )
    val pattern = fixture.densePattern(
      Vector(ObservationCell.Observed(2.0), ObservationCell.Observed(-1.0)),
      "full-rank-pattern"
    )
    val result = accepted(fixture.encoder().encode(pattern))

    assertEqualsDouble(result.code.values(0), 2.0, 2e-7)
    assertEqualsDouble(result.code.values(1), -1.0, 2e-7)
    assertEqualsDouble(result.objective.total, 0.0, 2e-12)
    assert(result.uniqueness.isInstanceOf[LatentCodeUniqueness.UniqueByStrongConvexity])

  test("iteration exhaustion is an evidence-bound unresolved achievement"):
    val fixture = quadraticFixture("limited-code", latentDimension = 1, decoder = matrix(Vector(Vector(2.0))))
    val config = acceptedFirstOrder(FirstOrderConfig.from(1, FirstOrderTolerance.strict))
    val encoder = accepted(FittedLatentEncoder.from(fixture.decoder, config = config))
    val pattern = fixture.densePattern(Vector(ObservationCell.Observed(3.0)), "limited-code-pattern")
    val result = accepted(encoder.encode(pattern))

    assertEquals(result.achievedGuarantee.claimClass, OptimizationClaimClass.Unresolved)
    assertEquals(
      result.achievedGuarantee.semanticEvidence.termination,
      NumericalTermination.IterationLimit
    )
    assert(result.achievedGuarantee.semanticEvidence.stationarity.nonEmpty)
    assertEquals(
      result.achievedGuarantee.semanticEvidence.bindings.result,
      result.resultIdentity
    )

  test("logistic ridge encoding matches an independent monotone root oracle"):
    val fixture = makeFixture(
      "logistic-code",
      Vector(FeatureDomain.Binary -> EntryLoss.Logistic),
      latentDimension = 1,
      decoder = matrix(Vector(Vector(1.0)))
    )
    val pattern = fixture.densePattern(Vector(ObservationCell.Observed(1.0)), "logistic-code-pattern")
    val result = accepted(fixture.encoder(Vector(fixture.rowRidge(1.0))).encode(pattern))
    val oracle = logisticRidgeRoot()

    assertEqualsDouble(result.code.values(0), oracle, 3e-7)
    assert(result.certificate.proxGradientResidual <= 4e-7)
    result.decoded.head.prediction match
      case DecodedPrediction.Binary(probability) =>
        assertEqualsDouble(probability.value, sigmoid(oracle), 3e-7)
      case other => fail(s"expected binary prediction, got $other")

  test("l1 encoding uses the exact soft-threshold code oracle"):
    val fixture = quadraticFixture("l1-code", latentDimension = 1, decoder = matrix(Vector(Vector(1.0))))
    val pattern = fixture.densePattern(Vector(ObservationCell.Observed(2.0)), "l1-code-pattern")
    val result = accepted(fixture.encoder(Vector(fixture.rowL1(0.5))).encode(pattern))

    assertEqualsDouble(result.code.values(0), 1.5, 3e-7)
    assertEqualsDouble(result.objective.total, 0.875, 3e-7)
    assert(result.certificate.proxGradientResidual <= 4e-7)

  test("dense and sparse observation masks compile to the same latent code"):
    val fixture = quadraticFixture(
      "mask-equivalence",
      latentDimension = 1,
      decoder = matrix(Vector(Vector(1.0, 2.0, -1.0)))
    )
    val missing = ObservationReason.unsafe("not supplied")
    val dense = fixture.densePattern(
      Vector(
        ObservationCell.Observed(2.0),
        ObservationCell.Missing(missing),
        ObservationCell.Observed(-2.0)
      ),
      "dense-mask-pattern"
    )
    val sparse = fixture.sparsePattern(
      Vector(
        accepted(SparsePointObservation.from(0, 3, 2.0)),
        accepted(SparsePointObservation.from(2, 3, -2.0))
      ),
      missing,
      "sparse-mask-pattern"
    )
    val encoder = fixture.encoder()
    val denseResult = accepted(encoder.encode(dense))
    val sparseResult = accepted(encoder.encode(sparse))

    assertEqualsDouble(denseResult.code.values(0), 2.0, 3e-7)
    assertEqualsDouble(sparseResult.code.values(0), denseResult.code.values(0), 3e-7)
    assertEquals(denseResult.support.features.indices, sparseResult.support.features.indices)
    assertEqualsDouble(sparseResult.objective.total, denseResult.objective.total, 2e-12)

  test("adding an exactly compatible observed feature leaves the encoded code unchanged"):
    val fixture = quadraticFixture(
      "compatible-observation",
      latentDimension = 1,
      decoder = matrix(Vector(Vector(1.0, 2.0)))
    )
    val missing = ObservationReason.unsafe("withheld")
    val base = fixture.densePattern(
      Vector(ObservationCell.Observed(2.0), ObservationCell.Missing(missing)),
      "compatible-base-pattern"
    )
    val augmented = fixture.densePattern(
      Vector(ObservationCell.Observed(2.0), ObservationCell.Observed(4.0)),
      "compatible-augmented-pattern"
    )
    val encoder = fixture.encoder()
    val baseResult = accepted(encoder.encode(base))
    val augmentedResult = accepted(encoder.encode(augmented))

    assertEqualsDouble(baseResult.code.values(0), 2.0, 3e-7)
    assertEqualsDouble(augmentedResult.code.values(0), baseResult.code.values(0), 3e-7)
    assertEqualsDouble(augmentedResult.objective.total, 0.0, 2e-12)

  test("empty support, foreign domains, unseen levels, and censoring are typed outcomes"):
    val realFixture = quadraticFixture("encoding-errors", latentDimension = 1, decoder = matrix(Vector(Vector(1.0))))
    val empty = realFixture.densePattern(
      Vector(ObservationCell.Missing(ObservationReason.unsafe("empty"))),
      "empty-support-pattern"
    )
    val foreignRows = space("foreign-encoding-row", 1)
    val foreignFeatures = space("foreign-encoding-feature", 1)
    val foreignEvidence = SpaceEvidence.unsafe[realFixture.features.Id](foreignFeatures.descriptor)
    val foreign = accepted(
      ObservationPattern.from[foreignRows.Id, realFixture.features.Id](
        foreignRows.evidence,
        foreignEvidence,
        Vector(ObservationCell.Observed(1.0)),
        id("foreign-encoding-pattern")
      )
    )
    val censored = realFixture.densePattern(
      Vector(ObservationCell.Censored(accepted(CensoringInterval.atOrBelow(0.0)))),
      "censored-encoding-pattern"
    )
    val categories = CategoryLevels.unsafe(Vector("known-a", "known-b"))
    val categoricalFixture = makeFixture(
      "unseen-category",
      Vector(FeatureDomain.Categorical(categories) -> EntryLoss.CategoricalCrossEntropy),
      latentDimension = 1,
      decoder = matrix(Vector(Vector(0.0, 1.0)))
    )
    val unseen = categoricalFixture.densePattern(
      Vector(ObservationCell.Observed(2.0)),
      "unseen-category-pattern"
    )

    assert(realFixture.encoder().encode(empty).left.exists(_ == LatentEncodingError.EmptySupport))
    assert(realFixture.encoder().encode(foreign).left.exists:
      case LatentEncodingError.InvalidDefinition(detail) => detail.contains("foreign feature")
      case _ => false
    )
    assert(realFixture.encoder().encode(censored).left.exists(_.isInstanceOf[LatentEncodingError.CensoredObservation]))
    assert(categoricalFixture.encoder().encode(unseen).left.exists(_.isInstanceOf[LatentEncodingError.UnseenCategoricalLevel]))

  test("rank-deficient observed decoder support returns a non-identifiability certificate"):
    val fixture = quadraticFixture(
      "nonidentifiable-code",
      latentDimension = 2,
      decoder = matrix(Vector(Vector(1.0), Vector(0.0)))
    )
    val pattern = fixture.densePattern(Vector(ObservationCell.Observed(2.0)), "nonidentifiable-pattern")
    val result = accepted(fixture.encoder().encode(pattern))

    assertEqualsDouble(result.code.values(0), 2.0, 3e-7)
    assertEqualsDouble(result.code.values(1), 0.0, 1e-14)
    result.uniqueness match
      case LatentCodeUniqueness.NotCertified(reason) => assert(reason.contains("rank is deficient"))
      case other => fail(s"expected a typed non-identifiability outcome, got $other")

  test("Poisson and ordinal encoding fail closed until their required solver capabilities exist"):
    val poisson = makeFixture(
      "poisson-encoding-boundary",
      Vector(FeatureDomain.Count -> EntryLoss.Poisson),
      latentDimension = 1,
      decoder = matrix(Vector(Vector(1.0)))
    )
    val ordinalLevels = OrderedLevels.unsafe(Vector("low", "middle", "high"))
    val ordinal = makeFixture(
      "ordinal-encoding-boundary",
      Vector(FeatureDomain.Ordinal(ordinalLevels) -> EntryLoss.OrdinalCumulativeLogit),
      latentDimension = 1,
      decoder = matrix(Vector(Vector(1.0, -1.0)))
    )

    assert(
      poisson.encoder().encode(
        poisson.densePattern(Vector(ObservationCell.Observed(2.0)), "poisson-partial-row")
      ).left.exists(_.isInstanceOf[LatentEncodingError.UnsupportedLoss])
    )
    assert(
      ordinal.encoder().encode(
        ordinal.densePattern(Vector(ObservationCell.Observed(1.0)), "ordinal-partial-row")
      ).left.exists(_.isInstanceOf[LatentEncodingError.UnsupportedLoss])
    )

  private final class Fixture(
      val name: String,
      val rows: SpaceRef,
      val features: SpaceRef,
      val latent: SpaceRef,
      val layout: GlrmFeatureLayout[features.Id],
      val decoder: FeatureDecoder[features.Id, latent.Id]
  ):
    def encoder(
        rowPenalties: Vector[GlrmFactorPenaltyTerm] = Vector.empty
    ): FittedLatentEncoder[features.Id, latent.Id] =
      accepted(FittedLatentEncoder.from(decoder, rowPenalties))

    def rowRidge(weight: Double): GlrmFactorPenaltyTerm =
      GlrmFactorPenaltyTerm(
        GlrmFactorTarget.RowCodes,
        GlrmFactorPenalty.SquaredFrobenius,
        PenaltyWeight.unsafe(weight),
        id(s"$name-row-ridge-$weight")
      )

    def rowL1(weight: Double): GlrmFactorPenaltyTerm =
      GlrmFactorPenaltyTerm(
        GlrmFactorTarget.RowCodes,
        GlrmFactorPenalty.ElementwiseL1,
        PenaltyWeight.unsafe(weight),
        id(s"$name-row-l1-$weight")
      )

    def densePattern(
        cells: Vector[ObservationCell],
        identity: String
    ): ObservationPattern[rows.Id, features.Id] =
      accepted(
        ObservationPattern.from(
          rows.evidence,
          features.evidence,
          cells,
          id(identity)
        )
      )

    def sparsePattern(
        observed: Vector[SparsePointObservation],
        missing: ObservationReason,
        identity: String
    ): ObservationPattern[rows.Id, features.Id] =
      accepted(
        ObservationPattern.singleRowSparse(
          rows.evidence,
          features.evidence,
          observed,
          missing,
          id(identity)
        )
      )

  private def quadraticFixture(
      name: String,
      latentDimension: Int,
      decoder: DMat
  ): Fixture =
    makeFixture(
      name,
      Vector.fill(decoder.cols)(FeatureDomain.Real -> EntryLoss.Quadratic),
      latentDimension,
      decoder
    )

  private def makeFixture(
      name: String,
      domains: Vector[(FeatureDomain, EntryLoss)],
      latentDimension: Int,
      decoder: DMat
  ): Fixture =
    val rows = space(s"$name-new-row", 1)
    val features = space(s"$name-features", domains.length)
    val latent = space(s"$name-latent", latentDimension)
    val specifications = domains.zipWithIndex.map: (definition, index) =>
      accepted(
        GlrmFeatureSpec.from(
          GlrmFeatureId.unsafe(s"$name-feature-$index"),
          definition._1,
          definition._2
        )
      )
    val layout = accepted(
      GlrmFeatureLayout.from(features.evidence, specifications, id(s"$name-layout"))
    )
    val fittedDecoder = accepted(
      FeatureDecoder.from(layout, latent.evidence, decoder, id(s"$name-decoder"))
    )
    new Fixture(name, rows, features, latent, layout, fittedDecoder)

  private def logisticRidgeRoot(): Double =
    var lower = 0.0
    var upper = 1.0
    var iteration = 0
    while iteration < 100 do
      val middle = 0.5 * (lower + upper)
      val value = sigmoid(middle) - 1.0 + middle
      if value > 0.0 then upper = middle else lower = middle
      iteration += 1
    0.5 * (lower + upper)

  private def sigmoid(value: Double): Double =
    1.0 / (1.0 + Math.exp(-value))

  private def acceptedFirstOrder[A](result: Either[scalafim.linalg.FirstOrderError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def matrix(values: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(values)

  private def space(name: String, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(name), SpaceRole.Observed, Dimension.unsafe(dimension)))

  private def id(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))

  private def accepted[A](value: Either[?, A]): A =
    value match
      case Left(error) =>
        error match
          case actual: LatentEncodingError => fail(actual.message)
          case actual: GeneralizedLowRankError => fail(actual.message)
          case other => fail(other.toString)
      case Right(result) => result
