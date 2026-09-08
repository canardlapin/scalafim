package scalafim.fmri.mvpa

import multivar.core.ValueId

class RelationalQuerySuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private def effects(id: String = "query-effects"): AxisRef[AxisKey] =
    right(
      AxisRef.create(
        AxisId.unsafe(id),
        AxisPurpose.Effects,
        Vector(
          AxisKey.unsafe("condition-a"),
          AxisKey.unsafe("condition-b"),
          AxisKey.unsafe("condition-c")
        ),
        CoordinateBasis.unsafe("condition-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("relational-query-suite", "v1")
      )
    )

  private def neural(id: String = "query-neural"): AxisRef[FeatureId] =
    right(
      AxisRef.create(
        AxisId.unsafe(id),
        AxisPurpose.NeuralFeatures,
        Vector(FeatureId.unsafe("voxel-1"), FeatureId.unsafe("voxel-2")),
        CoordinateBasis.unsafe("voxel-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("relational-query-suite", "v1")
      )
    )

  private def partitions(id: String = "query-runs"): AxisRef[PartitionId] =
    right(
      AxisRef.create(
        AxisId.unsafe(id),
        AxisPurpose.Partitions,
        Vector(
          PartitionId.unsafe("run-1"),
          PartitionId.unsafe("run-2"),
          PartitionId.unsafe("run-3")
        ),
        CoordinateBasis.unsafe("run-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("relational-query-suite", "v1")
      )
    )

  private def design(
      runs: AxisRef[PartitionId],
      forwardOnly: Boolean = false
  ): PairingDesign[runs.Id, runs.Id] =
    val named = right(PartitionAxis(ScientificAxisName.unsafe("runs"), runs))
    val generalization = GeneralizationAxis(
      ScientificAxisName.unsafe("runs"),
      runs.identity
    )
    val independence = right(
      PartitionIndependenceTestSupport.declareAllPairsForDesign(
        named,
        if forwardOnly then "query-forward" else "query-ordered"
      )
    )
    if forwardOnly then
      right(
        PairingDesign.forwardOnly(
          named,
          PairingReducer.WeightedMean,
          generalization,
          independence
        )
      )
    else
      right(
        PairingDesign.allOrdered(
          named,
          PairingReducer.WeightedMean,
          generalization,
          independence
        )
      )

  test("within-domain pairs and contrast rows have one canonical order"):
    val items = effects()
    val domain = right(WithinPairDomain(items))

    assertEquals(domain.size, 3)
    assertEquals(
      domain.pairs.map(pair => pair.first.value -> pair.second.value),
      Vector(
        "condition-a" -> "condition-b",
        "condition-a" -> "condition-c",
        "condition-b" -> "condition-c"
      )
    )
    assertEquals(right(domain.position(items.keys(2), items.keys(0))), 1)
    assertEquals(domain.pairAxis.identity.purpose.value, "effect-pairs")

    val contrast = right(
      domain.contrastQuery.map.materialize(
        MaterializationPolicy.Allow(MaterializationBudget.unsafe(64L))
      )
    ).value
    val expected = GaleTestMatrix.fromRows(
      Seq(
        Seq(1.0, -1.0, 0.0),
        Seq(1.0, 0.0, -1.0),
        Seq(0.0, 1.0, -1.0)
      )
    )
    var row = 0
    while row < expected.rows do
      var column = 0
      while column < expected.cols do
        assertEqualsDouble(contrast(row, column), expected(row, column), 0.0)
        column += 1
      row += 1

  test("rectangular pair coordinates are canonical left-major products"):
    val left = effects("left-effects")
    val rightAxis = right(
      AxisRef.create(
        AxisId.unsafe("right-effects"),
        AxisPurpose.Effects,
        Vector(AxisKey.unsafe("x"), AxisKey.unsafe("y")),
        CoordinateBasis.unsafe("condition-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("relational-query-suite", "v1")
      )
    )
    val domain = right(RectangularPairDomain(left, rightAxis))

    assertEquals(domain.size, 6)
    assertEquals(
      domain.pairs.map(pair => pair.left.value -> pair.right.value),
      Vector(
        "condition-a" -> "x",
        "condition-a" -> "y",
        "condition-b" -> "x",
        "condition-b" -> "y",
        "condition-c" -> "x",
        "condition-c" -> "y"
      )
    )

  test("effect and neural queries preserve exact ordered owners"):
    val itemAxis = effects()
    val output = right(
      AxisRef.create(
        AxisId.unsafe("query-contrasts"),
        AxisPurpose.Components,
        Vector(AxisKey.unsafe("a-minus-b")),
        CoordinateBasis.unsafe("contrast-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("relational-query-suite", "v1")
      )
    )
    val query = right(
      EffectQuery.dense(
        itemAxis,
        output,
        GaleTestMatrix.fromRows(Seq(Seq(1.0, -1.0, 0.0))),
        ValueId.unsafe("a-minus-b-query")
      )
    )
    assertEquals(query.source.identity, itemAxis.identity)
    assertEquals(query.output.identity, output.identity)
    assert(query.source.evidence eq itemAxis.evidence)
    assert(query.output.evidence eq output.evidence)

    val neuralAxis = neural()
    val identity = right(NeuralQuery.identity(neuralAxis))
    val precision = right(
      NeuralQuery.fixedPrecision(
        neuralAxis,
        GaleTestMatrix.fromRows(Seq(Seq(2.0, 0.25), Seq(0.25, 1.0))),
        ValueId.unsafe("fixed-neural-precision")
      )
    )
    assertEquals(identity.neural.identity, neuralAxis.identity)
    assertEquals(identity.kind, NeuralQueryKind.IdentityInnerProduct)
    assertEquals(precision.kind, NeuralQueryKind.FixedPrecision)
    assertNotEquals(identity.identity, precision.identity)

    assert(
      NeuralQuery
        .fixedPrecision(
          neuralAxis,
          GaleTestMatrix.fromRows(Seq(Seq(1.0, 0.0, 0.0))),
          ValueId.unsafe("wrong-shape")
        )
        .isLeft
    )

  test("RDM identity covers every scientific choice and its pairing design"):
    val itemAxis = effects()
    val neuralAxis = neural()
    val runs = partitions()
    val domain = right(WithinPairDomain(itemAxis))
    val identityMetric = right(NeuralQuery.identity(neuralAxis))
    val fixedMetric = right(
      NeuralQuery.fixedPrecision(
        neuralAxis,
        GaleTestMatrix.fromRows(Seq(Seq(2.0, 0.0), Seq(0.0, 1.0))),
        ValueId.unsafe("identity-court-precision")
      )
    )
    val allOrdered = design(runs)

    def definition(
        metric: NeuralQuery[neuralAxis.Id, FeatureId] = identityMetric,
        pairing: PairingDesign[runs.Id, runs.Id] = allOrdered,
        centering: EffectCentering = EffectCentering.None,
        normalization: RdmNormalization = RdmNormalization.Raw
    ): RdmDefinition[
      runs.Id,
      runs.Id,
      itemAxis.Id,
      neuralAxis.Id,
      AxisKey,
      FeatureId
    ] =
      right(
        RdmDefinition(
          domain,
          metric,
          pairing,
          centering = centering,
          normalization = normalization
        )
      )

    val baseline = definition()
    val alternatives = Vector(
      definition(metric = fixedMetric),
      definition(pairing = design(runs, forwardOnly = true)),
      definition(centering = EffectCentering.GrandMean),
      definition(normalization = RdmNormalization.DivideByNeuralDimension)
    )
    assert(alternatives.forall(_.identity != baseline.identity))
    val fields = baseline.identity.fields.map(field => field.name -> field.value).toMap
    assertEquals(fields("geometry"), "crossvalidated-bilinear")
    assertEquals(fields("squaredness"), "squared")
    assertEquals(fields("centering"), "none")
    assertEquals(fields("normalization"), "raw")
    assertEquals(fields("precision-policy"), "identity-inner-product")
    assertEquals(fields("partition-design"), allOrdered.identity.fingerprint.value)
    assertEquals(fields("pair-order"), WithinPairDomain.PairOrderProtocol)

  test("RDM values remain bound to their pair coordinates"):
    val itemAxis = effects()
    val neuralAxis = neural()
    val runs = partitions()
    val definition = right(
      RdmDefinition(
        right(WithinPairDomain(itemAxis)),
        right(NeuralQuery.identity(neuralAxis)),
        design(runs)
      )
    )
    val rdm = right(IdentifiedRdm(definition, Vector(1.0, -0.25, 2.0)))

    assertEqualsDouble(
      right(rdm.distance(itemAxis.keys(2), itemAxis.keys(0))),
      -0.25,
      0.0
    )
    assertEquals(rdm.distances.rowIdentity, definition.domain.pairAxis.identity)
    assert(IdentifiedRdm(definition, Vector(1.0, 2.0)).isLeft)
    assert(IdentifiedRdm(definition, Vector(1.0, Double.NaN, 2.0)).left.exists:
      case RelationalQueryError.NonFiniteRdmValue(1, value) => value.isNaN
      case _                                                => false)

  test("distinct effect owners cannot be substituted through query types"):
    val errors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace

      def invalid[A <: SemanticSpace, B <: SemanticSpace, Q <: SemanticSpace, K, QK](
          query: EffectQuery[A, Q, K, QK]
      ): EffectQuery[B, Q, K, QK] =
        query
    """)
    assert(errors.nonEmpty)
