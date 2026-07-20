package scalafim.inference

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector
import scalafim.multivar.BlockId
import scalafim.multivar.BlockPartition
import scalafim.multivar.BlockSpec
import scalafim.multivar.ComponentCount
import scalafim.multivar.CpcaBlock
import scalafim.multivar.CpcaConstraint
import scalafim.multivar.CpcaProblem
import scalafim.multivar.Dimension
import scalafim.multivar.DualityDiagram
import scalafim.multivar.IndexAxis
import scalafim.multivar.IndexSet
import scalafim.multivar.MatrixView
import scalafim.multivar.SpaceId
import scala.compiletime.testing.typeCheckErrors

class Phase6ProtocolSuite extends munit.FunSuite:

  private def accepted[A](value: Either[InferenceError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedMultivar[A](
      value: Either[scalafim.multivar.MultivarError, A]
  ): A =
    value.fold(error => fail(error.message), identity)

  private def matrix(value: Phase6RReferenceFixtures.MatrixData): DoubleMatrix =
    DoubleMatrix.fromRows(value.toRows)

  test("CCA correlation protocol matches independent base-R ridge-CCA roots") {
    val state = accepted(CcaCorrelationState.from(
      matrix(Phase6RReferenceFixtures.ccaX),
      matrix(Phase6RReferenceFixtures.ccaY),
      ridge = Phase6RReferenceFixtures.ccaRidge
    ))
    val protocol = CcaCorrelationProtocol()
    val roots = accepted(protocol.roots(state))

    assertEquals(roots.length, Phase6RReferenceFixtures.ccaRoots.length)
    roots.indices.foreach(index =>
      assertEqualsDouble(roots(index), Phase6RReferenceFixtures.ccaRoots(index), 1e-8)
    )
    assertEqualsDouble(accepted(protocol.observed(state)), roots.head, 1e-12)

    val random = RandomSource.forReplicate(RootSeed(91L), accepted(ReplicateId(2)))
    val firstNull = accepted(protocol.nullStatistic(
      state,
      accepted(ComponentIx(0)),
      accepted(ReplicateId(2)),
      random
    ))
    val secondNull = accepted(protocol.nullStatistic(
      state,
      accepted(ComponentIx(0)),
      accepted(ReplicateId(2)),
      random
    ))
    assertEqualsDouble(firstNull, secondNull, 0.0)
    assert(firstNull >= 0.0 && firstNull <= 1.0)

    val removed = accepted(protocol.remove(state))
    assert(accepted(protocol.observed(removed)) <= roots.head + 1e-10)
  }

  test("CCA conditioned compilation carries conditional validity") {
    val rows = accepted(RowCount(8))
    val reference = accepted(ConditioningRef("cca-nuisance-v1"))
    val spec = InferenceSpec.of(
      FitDescriptor.Cca,
      TargetSpec.CanonicalCorrelations,
      NullSpec.BreakY,
      ResamplingDesign.nuisanceAdjusted(
        rows,
        reference,
        WhiteningRequirement.Required
      ),
      UnitPolicy.SingleAxes,
      MonteCarloPolicy.Fixed(accepted(MonteCarloDraws(19))),
      RequestedEvidence.SignificanceOnly,
      RootSeed(7L)
    )
    val program = InferenceCompiler.compile(spec)

    assertEquals(program.validity, ValidityClaim.Conditional)
  }

  test("generalized-eigen protocol matches independent diagonal roots and deflates lawfully") {
    val state = accepted(GeneralizedEigenState.from(
      matrix(Phase6RReferenceFixtures.generalizedA),
      matrix(Phase6RReferenceFixtures.generalizedB)
    ))
    val protocol = GeneralizedEigenProtocol()
    val roots = accepted(protocol.roots(state))

    assertEquals(roots.length, Phase6RReferenceFixtures.generalizedRoots.length)
    roots.indices.foreach(index =>
      assertEqualsDouble(roots(index), Phase6RReferenceFixtures.generalizedRoots(index), 1e-10)
    )

    val removed = accepted(protocol.remove(state))
    val next = accepted(protocol.roots(removed))
    assertEqualsDouble(next.head, roots(1), 1e-9)
  }

  test("RRR predictive gain uses an explicit held-out split and matches base R") {
    val data = accepted(PredictiveData.from(
      matrix(Phase6RReferenceFixtures.rrrX),
      matrix(Phase6RReferenceFixtures.rrrY)
    ))
    val split = accepted(HeldOutSplit.from(
      accepted(RowCount(data.x.rows)),
      Phase6RReferenceFixtures.rrrTrainingZeroBased,
      Phase6RReferenceFixtures.rrrTestZeroBased
    ))
    val protocol = RrrPredictiveProtocol(components = 1)
    val result = accepted(protocol.observe(data, split))

    assertEqualsDouble(result.gain, Phase6RReferenceFixtures.rrrGain, 1e-10)
    assertEqualsDouble(result.modelSquaredError, Phase6RReferenceFixtures.rrrModelSse, 1e-10)
    assertEqualsDouble(result.baselineSquaredError, Phase6RReferenceFixtures.rrrBaselineSse, 1e-10)
    assertEquals(result.validity, ValidityClaim.Conditional)

    val nullA = accepted(protocol.nullStatistic(data, split, RootSeed(12L), accepted(ReplicateId(4))))
    val nullB = accepted(protocol.nullStatistic(data, split, RootSeed(12L), accepted(ReplicateId(4))))
    assertEqualsDouble(nullA, nullB, 0.0)
  }

  test("held-out splits reject overlap and incomplete row assignment") {
    val rows = accepted(RowCount(4))
    assert(HeldOutSplit.from(rows, Vector(0, 1), Vector(1, 2, 3)).isLeft)
    assert(HeldOutSplit.from(rows, Vector(0, 1), Vector(2)).isLeft)
  }

  test("CPCA block inference matches an independently projected base-R SVD") {
    val data = matrix(Phase6RReferenceFixtures.cpcaData)
    val diagram = acceptedMultivar(DualityDiagram.from(MatrixView.dense(data)))
    val rowConstraint = acceptedMultivar(CpcaConstraint.basis(
      IndexAxis.Row,
      diagram.rowSpace,
      matrix(Phase6RReferenceFixtures.cpcaRowDesign),
      diagram.rowMetric
    ))
    val columnConstraint = acceptedMultivar(CpcaConstraint.basis(
      IndexAxis.Column,
      diagram.columnSpace,
      matrix(Phase6RReferenceFixtures.cpcaColumnDesign),
      diagram.columnMetric
    ))
    val problem = acceptedMultivar(CpcaProblem.from(
      diagram,
      rowConstraint,
      columnConstraint
    ))
    val state = accepted(CpcaInferenceState.from(
      problem,
      CpcaBlock.GxH,
      acceptedMultivar(ComponentCount(2))
    ))
    val protocol = CpcaBlockProtocol()
    val roots = accepted(protocol.roots(state))

    assertEquals(roots.length, Phase6RReferenceFixtures.cpcaRoots.length)
    roots.indices.foreach(index =>
      assertEqualsDouble(roots(index), Phase6RReferenceFixtures.cpcaRoots(index), 1e-8)
    )

    val random = RandomSource.forReplicate(RootSeed(55L), accepted(ReplicateId(1)))
    assert(accepted(protocol.nullStatistic(
      state,
      accepted(ComponentIx(0)),
      accepted(ReplicateId(1)),
      random
    )).isFinite)
    assert(accepted(protocol.remove(state)).removed == 1)
  }

  test("multiblock consensus roots match base R and block-independence preserves domains") {
    val data = matrix(Phase6RReferenceFixtures.multiblockData)
    val block1 = BlockSpec(
      BlockId.unsafe("x"),
      acceptedMultivar(IndexSet.from(Vector(0, 1), IndexAxis.Column))
    )
    val block2 = BlockSpec(
      BlockId.unsafe("y"),
      acceptedMultivar(IndexSet.from(Vector(2, 3), IndexAxis.Column))
    )
    val partition = acceptedMultivar(BlockPartition.from(
      Dimension.unsafe(4),
      Vector(block1, block2)
    ))
    val state = accepted(MultiblockInferenceState.from(data, partition))
    val protocol = MultiblockConsensusProtocol()
    val roots = accepted(protocol.roots(state))

    assertEquals(roots.length, Phase6RReferenceFixtures.multiblockRoots.length)
    roots.indices.foreach(index =>
      assertEqualsDouble(roots(index), Phase6RReferenceFixtures.multiblockRoots(index), 1e-9)
    )
    val nullRoot = accepted(protocol.nullStatistic(
      state,
      accepted(ComponentIx(0)),
      accepted(ReplicateId(3)),
      RandomSource.forReplicate(RootSeed(81L), accepted(ReplicateId(3)))
    ))
    assert(nullRoot.isFinite)
    assertEquals(state.partition.blocks.map(_.id), Vector(BlockId.unsafe("x"), BlockId.unsafe("y")))

    val removed = accepted(protocol.remove(state))
    assert(accepted(protocol.observed(removed)) <= roots.head + 1e-10)
  }

  test("method-native feature evidence matches R multiplicity adjustments") {
    val nullRows = Phase6RReferenceFixtures.featureNull.toRows.map(DoubleVector.fromSeq)
    val evidence = accepted(FeatureNullEvidence.from(
      "loading-magnitude-null-v1",
      accepted(UnitId("u1")),
      SpaceId.unsafe("features"),
      DoubleVector.fromSeq(Phase6RReferenceFixtures.featureObserved),
      nullRows,
      ValidityClaim.Conditional
    ))
    val bh = accepted(FeatureEvidence.evaluate(evidence, MultiplicityMethod.BenjaminiHochberg))
    val holm = accepted(FeatureEvidence.evaluate(evidence, MultiplicityMethod.Holm))

    bh.tests.indices.foreach { index =>
      assertEqualsDouble(
        bh.tests(index).rawPValue.value,
        Phase6RReferenceFixtures.featureRawP(index),
        1e-12
      )
      assertEqualsDouble(
        bh.tests(index).adjustedPValue.value,
        Phase6RReferenceFixtures.featureBhP(index),
        1e-12
      )
      assertEqualsDouble(
        holm.tests(index).adjustedPValue.value,
        Phase6RReferenceFixtures.featureHolmP(index),
        1e-12
      )
    }
    assertEquals(bh.validity, ValidityClaim.Conditional)
  }

  test("all Phase 6 families compile only through their declared target/null contracts") {
    val rows = accepted(RowCount(8))
    val design = ResamplingDesign.exchangeableRows(rows)
    val policy = MonteCarloPolicy.Fixed(accepted(MonteCarloDraws(19)))

    val rrr = InferenceCompiler.compile(InferenceSpec.of(
      FitDescriptor.ReducedRankRegression,
      TargetSpec.PredictiveGain,
      NullSpec.PermuteResponse,
      design,
      UnitPolicy.SingleAxes,
      policy,
      RequestedEvidence.SignificanceOnly,
      RootSeed(1L)
    ))
    val generalized = InferenceCompiler.compile(InferenceSpec.of(
      FitDescriptor.GeneralizedEigen,
      TargetSpec.GeneralizedEigenRoots,
      NullSpec.PermuteRows,
      design,
      UnitPolicy.SingleAxes,
      policy,
      RequestedEvidence.SignificanceOnly,
      RootSeed(2L)
    ))
    val cpca = InferenceCompiler.compile(InferenceSpec.of(
      FitDescriptor.CpcaBlock,
      TargetSpec.ConstrainedInertiaRoots,
      NullSpec.PermuteRows,
      design,
      UnitPolicy.SingleAxes,
      policy,
      RequestedEvidence.SignificanceOnly,
      RootSeed(3L)
    ))
    val multiblock = InferenceCompiler.compile(InferenceSpec.of(
      FitDescriptor.MultiblockConsensus,
      TargetSpec.MultiblockConsensusRoots,
      NullSpec.BreakBlocks,
      design,
      UnitPolicy.SingleAxes,
      policy,
      RequestedEvidence.SignificanceOnly,
      RootSeed(4L)
    ))

    assertEquals(rrr.validity, ValidityClaim.Conditional)
    assertEquals(generalized.validity, ValidityClaim.Exact)
    assertEquals(cpca.validity, ValidityClaim.Conditional)
    assertEquals(multiblock.validity, ValidityClaim.Exact)
  }

  test("incoherent Phase 6 target and null combinations do not typecheck") {
    val generalizedErrors = typeCheckErrors("""
      import scalafim.inference.*
      val rows = RowCount(4).toOption.get
      val spec = InferenceSpec.of(
        FitDescriptor.GeneralizedEigen,
        TargetSpec.GeneralizedEigenRoots,
        NullSpec.BreakBlocks,
        ResamplingDesign.exchangeableRows(rows),
        UnitPolicy.SingleAxes,
        MonteCarloPolicy.Fixed(MonteCarloDraws(19).toOption.get),
        RequestedEvidence.SignificanceOnly,
        RootSeed(1L)
      )
      InferenceCompiler.compile(spec)
    """)
    val multiblockErrors = typeCheckErrors("""
      import scalafim.inference.*
      val rows = RowCount(4).toOption.get
      val spec = InferenceSpec.of(
        FitDescriptor.MultiblockConsensus,
        TargetSpec.CovarianceRoots,
        NullSpec.BreakBlocks,
        ResamplingDesign.exchangeableRows(rows),
        UnitPolicy.SingleAxes,
        MonteCarloPolicy.Fixed(MonteCarloDraws(19).toOption.get),
        RequestedEvidence.SignificanceOnly,
        RootSeed(1L)
      )
      InferenceCompiler.compile(spec)
    """)

    assert(generalizedErrors.exists(_.message.contains("SupportsNull")))
    assert(multiblockErrors.exists(_.message.contains("SupportsTarget")))
  }
