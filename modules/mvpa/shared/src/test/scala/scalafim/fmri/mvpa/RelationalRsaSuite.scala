package scalafim.fmri.mvpa

import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.IndexSpace
import resample4s.core.Injection

class RelationalRsaSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val effects = right(
    AxisRef.create(
      AxisId.unsafe("rsa-effects"),
      AxisPurpose.Effects,
      Vector("a", "b", "c", "d").map(AxisKey.unsafe),
      CoordinateBasis.unsafe("condition-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-rsa-suite", "v1")
    )
  )

  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("rsa-neural"),
      AxisPurpose.NeuralFeatures,
      Vector(FeatureId.unsafe("x"), FeatureId.unsafe("y")),
      CoordinateBasis.unsafe("feature-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-rsa-suite", "v1")
    )
  )

  private val partitions = right(
    AxisRef.create(
      AxisId.unsafe("rsa-runs"),
      AxisPurpose.Partitions,
      Vector(PartitionId.unsafe("run-1"), PartitionId.unsafe("run-2")),
      CoordinateBasis.unsafe("run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-rsa-suite", "v1")
    )
  )

  private val namedPartitions = right(
    PartitionAxis(ScientificAxisName.unsafe("runs"), partitions)
  )

  private val training = right(
    AxisRef.create(
      AxisId.unsafe("rsa-training"),
      AxisPurpose.Samples,
      Vector(SampleId.unsafe("t1"), SampleId.unsafe("t2")),
      CoordinateBasis.unsafe("time-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-rsa-suite", "v1")
    )
  )

  private val measurement =
    val injection = right(
      Injection.from(
        IArray.unsafeFromArray(Array(0, 1)),
        right(IndexSpace.of(neural.size))
      )
    )
    right(
      Measurement.hardSelection(
        neural,
        MeasurementId.unsafe("rsa-all-features"),
        injection
      )
    )

  private val patterns = GaleTestMatrix.fromRows(
    Seq(
      Seq(0.0, 0.0),
      Seq(1.0, 0.0),
      Seq(0.0, 1.0),
      Seq(1.0, 1.0)
    )
  )

  private val relationDesign = right(DesignIdentity(DesignKind.unsafe("rsa-relation")))

  private def fit(
      normalization: RdmNormalization = RdmNormalization.Raw
  ): IdentityPrecisionRelationFit[
    partitions.Id,
    effects.Id,
    measurement.local.Id,
    AxisKey,
    FeatureId
  ] =
    val entries = partitions.keys.map: partition =>
      val estimate = right(
        EvidenceTable.dense(
          effects,
          neural,
          patterns,
          ValueId.unsafe(s"rsa-estimate-${partition.value}")
        )
      )
      val receipt = right(
        RelationFitReceipt(
          ValueIdentity.source(ValueId.unsafe(s"rsa-source-${partition.value}")),
          relationDesign,
          right(Estimability(effects, Vector.fill(4)(true))),
          NormalizationIdentity.none,
          training
        )
      )
      PartitionRelation(
        partition,
        right(Relation(estimate, receipt, right(EstimateOnlyCapabilities(neural))))
      )
    val source = right(PartitionedRelations(namedPartitions, effects, neural, entries))
    val pairing = right(
      PairingDesign.allOrdered(
        namedPartitions,
        PairingReducer.WeightedMean,
        GeneralizationAxis(ScientificAxisName.unsafe("runs"), partitions.identity),
        right(
          PartitionIndependenceTestSupport.declareAllPairs(
            source.identity,
            namedPartitions,
            "rsa-independent-runs"
          )
        )
      )
    )
    val domain = right(WithinPairDomain(source.effects))
    val query = right(
      RelationalFitQuery.identityPrecision(source, domain, normalization)
    )
    right(RelationalFit.query(query, pairing, measurement))

  test("Pearson, rank, partial, and regression RSA state distinct exact estimands"):
    val relationFit = fit()
    val observed = right(relationFit.rdm).distances.toVector
    assertEquals(observed, Vector(1.0, 1.0, 2.0, 2.0, 1.0, 1.0))
    val signal = right(
      SecondOrderModel.signal(
        relationFit.definition.domain,
        SecondOrderModelName.unsafe("spatial-model"),
        observed
      )
    )
    val nuisance = right(
      SecondOrderModel.nuisance(
        relationFit.definition.domain,
        SecondOrderModelName.unsafe("run-nuisance"),
        Vector(0.0, 1.0, 0.0, 1.0, 0.0, 1.0)
      )
    )

    val pearson = right(RelationalRsa.pearson(relationFit, signal))
    val rank = right(RelationalRsa.rank(relationFit, signal))
    val partial = right(RelationalRsa.partial(relationFit, signal, Vector(nuisance)))
    val regression = right(
      RelationalRsa.regression(
        relationFit,
        Vector(signal),
        Vector(nuisance),
        intercept = true
      )
    )

    assertEqualsDouble(pearson.correlation, 1.0, 1e-12)
    assertEqualsDouble(rank.correlation, 1.0, 1e-12)
    assertEqualsDouble(partial.partialCorrelation, 1.0, 1e-12)
    assertEqualsDouble(
      regression.coefficients.find(_.role == RsaCoefficientRole.Signal).map(_.value).get,
      1.0,
      1e-12
    )
    assertEqualsDouble(
      regression.coefficients.find(_.role == RsaCoefficientRole.Nuisance).map(_.value).get,
      0.0,
      1e-12
    )
    assertEqualsDouble(regression.residualSumSquares, 0.0, 1e-12)
    assertEquals(
      pearson.estimand.fields.find(_.name == "second-order-estimand").map(_.value),
      Some("pearson-correlation-of-signed-canonical-pair-distances")
    )
    assertEquals(
      rank.estimand.fields.find(_.name == "second-order-estimand").map(_.value),
      Some("spearman-correlation-of-canonical-pair-distances")
    )
    assertEquals(
      partial.estimand.fields.find(_.name == "second-order-estimand").map(_.value),
      Some("pearson-correlation-after-intercept-and-nuisance-residualization")
    )
    assertEquals(
      regression.estimand.fields.find(_.name == "second-order-estimand").map(_.value),
      Some("ordinary-least-squares-on-signed-canonical-pair-distances")
    )

  test("signal and nuisance roles are statically distinct"):
    val errors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace

      def invalid[P <: SemanticSpace, E <: SemanticSpace, L <: SemanticSpace, EK, LK](
          fit: CompatibleSecondOrderFit[P, E, L, EK, LK],
          nuisance: SecondOrderModel[NuisanceModelRole, E, EK]
      ) = RelationalRsa.rank(fit, nuisance)
    """)
    assert(errors.nonEmpty)

  test("model values are bound to the fit's exact canonical pair domain"):
    val relationFit = fit()
    val domain = relationFit.definition.domain
    val observed = right(relationFit.rdm).distances.toVector
    val model = right(
      SecondOrderModel.signal(
        domain,
        SecondOrderModelName.unsafe("canonical-domain"),
        observed
      )
    )

    assertEquals(model.domain.pairs, domain.pairs)
    assertEquals(model.values.rowIdentity, domain.pairAxis.identity)
    assert(model.values.rows eq domain.pairAxis.evidence)
    assertEquals(right(RelationalRsa.pearson(relationFit, model)).model, model.identity)

  test("invalid model banks fail closed"):
    val relationFit = fit()
    val signal = right(
      SecondOrderModel.signal(
        relationFit.definition.domain,
        SecondOrderModelName.unsafe("duplicate"),
        Vector(1.0, 1.0, 2.0, 2.0, 1.0, 1.0)
      )
    )
    val nuisance = right(
      SecondOrderModel.nuisance(
        relationFit.definition.domain,
        SecondOrderModelName.unsafe("duplicate"),
        Vector(0.0, 1.0, 0.0, 1.0, 0.0, 1.0)
      )
    )
    val constant = right(
      SecondOrderModel.signal(
        relationFit.definition.domain,
        SecondOrderModelName.unsafe("constant"),
        Vector.fill(6)(1.0)
      )
    )

    assert(
      RelationalRsa
        .partial(relationFit, signal, Vector.empty)
        .left
        .exists(
          _ == RelationalRsaError.EmptyNuisanceModels
        )
    )
    assert(
      RelationalRsa
        .regression(
          relationFit,
          Vector(signal),
          Vector(nuisance),
          intercept = true
        )
        .left
        .exists:
          case RelationalRsaError.DuplicateModelName(name) => name.value == "duplicate"
          case _                                           => false
    )
    assert(
      RelationalRsa
        .rank(relationFit, constant)
        .left
        .exists:
          case RelationalRsaError.ConstantVector("signal ranks") => true
          case _                                                 => false
    )

  test("comparison results retain the exact compatible fit identity"):
    val rawFit = fit(RdmNormalization.Raw)
    val normalizedFit = fit(RdmNormalization.DivideByNeuralDimension)
    val rawModel = right(
      SecondOrderModel.signal(
        rawFit.definition.domain,
        SecondOrderModelName.unsafe("raw-model"),
        right(rawFit.rdm).distances.toVector
      )
    )
    val normalizedModel = right(
      SecondOrderModel.signal(
        normalizedFit.definition.domain,
        SecondOrderModelName.unsafe("normalized-model"),
        right(normalizedFit.rdm).distances.toVector
      )
    )
    val raw = right(RelationalRsa.rank(rawFit, rawModel))
    val normalized = right(RelationalRsa.rank(normalizedFit, normalizedModel))

    assertNotEquals(rawFit.identity, normalizedFit.identity)
    assertEquals(raw.fit, rawFit.identity)
    assertEquals(normalized.fit, normalizedFit.identity)
    assertNotEquals(raw.estimand, normalized.estimand)
