package scalafim.fmri.mvpa

import multivar.core.SemanticSpace

class RelationalDesignSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private def partitionAxis(
      id: String,
      keys: String*
  ): AxisRef[PartitionId] =
    partitionAxisWithPurpose(id, keys, AxisPurpose.Partitions)

  private def partitionAxisWithPurpose(
      id: String,
      keys: Seq[String],
      purpose: AxisPurpose
  ): AxisRef[PartitionId] =
    right(
      AxisRef.create(
        AxisId.unsafe(id),
        purpose,
        keys.map(PartitionId.unsafe),
        CoordinateBasis.unsafe("partition-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("pairing-suite", "v1")
      )
    )

  private def namedAxis(
      name: String,
      axis: AxisRef[PartitionId]
  ): PartitionAxis[axis.Id] =
    right(PartitionAxis(ScientificAxisName.unsafe(name), axis))

  private def generalization(
      name: String,
      axis: AxisRef[PartitionId]
  ): GeneralizationAxis =
    GeneralizationAxis(ScientificAxisName.unsafe(name), axis.identity)

  private def evidence[S <: SemanticSpace](
      partitions: PartitionAxis[S],
      token: String
  ): ScientificSourceIdentity =
    right(
      ScientificSourceIdentity(
        ScientificSourceKind.unsafe("relational-design-fixture"),
        Vector(ScientificSourceAxis(partitions.name, partitions.axis.identity)),
        Vector("evidence" -> token)
      )
    )

  private def allDeclared[S <: SemanticSpace](
      partitions: PartitionAxis[S],
      token: String
  ): PartitionIndependenceDeclaration[S, S] =
    right(
      PartitionIndependenceTestSupport.declareAllPairsForDesign(
        partitions,
        token
      )
    )

  test("all-ordered designs are directed edges, not validation folds"):
    val runs = partitionAxis("runs", "run-1", "run-2", "run-3")
    val namedRuns = namedAxis("runs", runs)
    val declaration = allDeclared(namedRuns, "all-directed-runs")
    val all = right(
      PairingDesign.allOrdered(
        namedRuns,
        PairingReducer.WeightedMean,
        generalization("runs", runs),
        declaration
      )
    )
    assertEquals(all.edges.length, 6)
    assert(all.edges.forall(edge => edge.left != edge.right))
    assertEquals(all.mode, PairingMode.AllOrdered)
    assertEquals(all.referencedAxes.map(_.name.value), Vector("runs"))

    val errors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace
      import resample4s.core.Coverage

      def invalid[S <: SemanticSpace](
          design: PairingDesign[S, S]
      ): ValidationDesign[S, Coverage.ExactOnce, BoundSelectionSplit[SampleId, S]] =
        design
    """)
    assert(errors.nonEmpty)

  test("forward-only reversal is an involutive transpose of paired values"):
    val runs = partitionAxis("forward-runs", "run-1", "run-2", "run-3")
    val namedRuns = namedAxis("runs", runs)
    val design = right(
      PairingDesign.forwardOnly(
        namedRuns,
        PairingReducer.WeightedMean,
        generalization("runs", runs),
        allDeclared(namedRuns, "forward-runs")
      )
    )
    val values = right(PairingValues(design, Vector("12", "13", "23")))
    val transposed = right(values.transpose)
    val roundTrip = right(transposed.transpose)

    assertEquals(
      design.edges.map(edge => edge.left.value -> edge.right.value),
      Vector("run-1" -> "run-2", "run-1" -> "run-3", "run-2" -> "run-3")
    )
    assertEquals(transposed.design.mode, PairingMode.ReversedForwardOnly)
    assertEquals(
      transposed.design.edges.map(edge => edge.left.value -> edge.right.value),
      Vector("run-2" -> "run-1", "run-3" -> "run-1", "run-3" -> "run-2")
    )
    assertEquals(transposed.values, Vector("12", "13", "23"))
    assertEquals(roundTrip.design.identity, design.identity)
    assertEquals(roundTrip.values, values.values)

  test("rectangular pairing consumes exact source-level independence declarations"):
    val left = partitionAxis("subject-a-runs", "a-1", "a-2")
    val rightAxis = partitionAxis("subject-b-runs", "b-1", "b-2", "b-3")
    val leftNamed = namedAxis("left-runs", left)
    val rightNamed = namedAxis("right-runs", rightAxis)
    val declaration = right(
      PartitionIndependenceTestSupport.declareBetween(
        evidence(leftNamed, "subject-a"),
        leftNamed,
        evidence(rightNamed, "subject-b"),
        rightNamed,
        for
          leftKey <- left.keys
          rightKey <- rightAxis.keys
        yield DeclaredIndependentPair(leftKey, rightKey),
        "independent-acquisitions-2026"
      )
    )
    val design = right(
      PairingDesign.rectangular(
        leftNamed,
        rightNamed,
        PairingReducer.WeightedSum,
        generalization("subjects", left),
        declaration
      )
    )

    assertEquals(design.mode, PairingMode.Rectangular)
    assertEquals(design.edges.length, 6)
    assertEquals(
      design.referencedAxes.map(_.name.value),
      Vector("left-runs", "right-runs", "subjects")
    )
    assertEquals(design.independence.id.value, "independent-acquisitions-2026")
    assertEquals(design.independence.pairs.length, 6)

  test("invalid edges, declarations, reducers, and axes fail closed"):
    val runs = partitionAxis("invalid-runs", "run-1", "run-2")
    val same = namedAxis("runs", runs)
    val generalizes = generalization("runs", runs)
    val self = right(PairingEdge(runs.keys(0), runs.keys(0)))
    val negative = right(PairingEdge(runs.keys(0), runs.keys(1), -1.0))
    val unknown = right(
      PairingEdge(PartitionId.unsafe("unknown"), runs.keys(1))
    )
    val declaration = allDeclared(same, "invalid-runs")
    val source = evidence(same, "invalid-declaration-source")
    val sourceEvidence = right(PartitionEvidenceIdentity(source, same))

    assert(
      PartitionIndependenceDeclaration(
        IndependenceDeclarationId.unsafe("self-pair"),
        sourceEvidence,
        sourceEvidence,
        Vector(DeclaredIndependentPair(runs.keys(0), runs.keys(0)))
      ).left.exists:
        case PairingDesignError.SelfPairCannotBeDeclaredIndependent(key) =>
          key == runs.keys(0)
        case _ => false
    )
    assert(
      PairingDesign(
        same,
        same,
        Vector(self),
        PairingReducer.WeightedSum,
        generalizes,
        declaration
      ).left.exists:
        case PairingDesignError.UnattestedPair(left, right) => left == right
        case _                                              => false
    )
    assert(
      PairingDesign(
        same,
        same,
        Vector(negative),
        PairingReducer.WeightedMean,
        generalizes,
        declaration
      ).left.exists:
        case PairingDesignError.NonPositiveMeanWeight(_, _, -1.0) => true
        case _                                                    => false
    )
    assert(
      PairingDesign(
        same,
        same,
        Vector(unknown),
        PairingReducer.WeightedSum,
        generalizes,
        declaration
      ).left.exists:
        case PairingDesignError.UnknownLeft(key) => key.value == "unknown"
        case _                                   => false
    )
    val wrongPurpose = partitionAxisWithPurpose(
      "not-partitions",
      Vector("run-1", "run-2"),
      AxisPurpose.Samples
    )
    assert(
      PartitionAxis(
        ScientificAxisName.unsafe("runs"),
        wrongPurpose
      ).left.exists:
        case PairingDesignError.InvalidPartitionPurpose(
              AxisPurpose.Partitions,
              AxisPurpose.Samples
            ) =>
          true
        case _ => false
    )

  test("unequal but dependent partition IDs remain unattested and fail closed"):
    val runs = partitionAxis("dependent-runs", "run-1", "run-2", "run-3")
    val namedRuns = namedAxis("runs", runs)
    val declaration = right(
      PartitionIndependenceTestSupport.declarePairs(
        evidence(namedRuns, "dependent-runs"),
        namedRuns,
        Vector(
          DeclaredIndependentPair(runs.keys(0), runs.keys(2)),
          DeclaredIndependentPair(runs.keys(1), runs.keys(2))
        ),
        "run-1-and-run-2-share-errors"
      )
    )

    val result = PairingDesign.allOrdered(
      namedRuns,
      PairingReducer.WeightedMean,
      generalization("runs", runs),
      declaration
    )

    assert(result.left.exists:
      case PairingDesignError.UnattestedPair(left, right) =>
        Set(left, right) == Set(runs.keys(0), runs.keys(1))
      case _ => false)

    val oldInference = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace

      def invalid[S <: SemanticSpace](
          partitions: PartitionAxis[S],
          generalization: GeneralizationAxis
      ) = PairingDesign.allOrdered(
        partitions,
        PairingReducer.WeightedMean,
        generalization
      )
    """)
    assert(oldInference.nonEmpty)
  test("edge weights, reducer, generalization, and independence enter identity"):
    val left = partitionAxis("identity-left", "left-1", "left-2")
    val rightAxis = partitionAxis("identity-right", "right-1", "right-2")
    val leftNamed = namedAxis("left", left)
    val rightNamed = namedAxis("right", rightAxis)
    val edge = right(PairingEdge(left.keys(0), rightAxis.keys(0), 1.0))
    val leftEvidence = evidence(leftNamed, "identity-left")
    val rightEvidence = evidence(rightNamed, "identity-right")

    def build(
        weight: Double,
        reducer: PairingReducer,
        generalizationName: String,
        declarationId: String
    ): PairingDesign[left.Id, rightAxis.Id] =
      val declaration = right(
        PartitionIndependenceTestSupport.declareBetween(
          leftEvidence,
          leftNamed,
          rightEvidence,
          rightNamed,
          Vector(DeclaredIndependentPair(edge.left, edge.right)),
          declarationId
        )
      )
      right(
        PairingDesign(
          leftNamed,
          rightNamed,
          Vector(right(PairingEdge(edge.left, edge.right, weight))),
          reducer,
          generalization(generalizationName, left),
          declaration
        )
      )

    val baseline = build(1.0, PairingReducer.WeightedSum, "subjects", "receipt-a")
    val alternatives = Vector(
      build(2.0, PairingReducer.WeightedSum, "subjects", "receipt-a"),
      build(1.0, PairingReducer.WeightedMean, "subjects", "receipt-a"),
      build(1.0, PairingReducer.WeightedSum, "sessions", "receipt-a"),
      build(1.0, PairingReducer.WeightedSum, "subjects", "receipt-b")
    )

    assert(alternatives.forall(_.identity != baseline.identity))
