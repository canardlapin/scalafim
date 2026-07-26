package scalafim.multivar
package family.multiblock

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*
import scalafim.multivar.family.glrm.*
import scalafim.multivar.family.multiblock.*

import gale.linalg.DMat

class StructuredMultiblockGlrmSuite extends munit.FunSuite:

  test("aligned shared scores remain a distinct row-semantic family"):
    val rows = space("family-rows", SpaceRole.Samples, 2)
    val latent = space("family-latent", SpaceRole.Latent, 1)
    val binding = accepted(SharedRowBinding.verified(rows.evidence, id("family-row-keys")))
    val features = space("family-features", SpaceRole.Observed, 1)
    val natural = space("family-natural", SpaceRole.Observed, 1)
    val block = realBlock(
      "family-a",
      rows.evidence,
      features.evidence,
      latent.evidence,
      natural.evidence,
      binding,
      observed = Vector(1.0, 2.0),
      decoder = matrix(Vector(Vector(1.0))),
      scaling = BlockLossScaling.MeanObservedLoss(BlockImportance.unsafe(1.0))
    )
    val program = accepted(AlignedSharedScoreGlrm.from(binding, latent.evidence, Vector(block)))
    val family: StructuredMultiblockStudy = StructuredMultiblockStudy.Aligned(program)

    assertEquals(family.family, StructuredMultiblockFamily.AlignedSharedScores)
    assertNotEquals(family.family, StructuredMultiblockFamily.IndependentDirectSum)
    assertNotEquals(family.family, StructuredMultiblockFamily.HubAlignedEntities)

  test("mean-observed scaling prevents block size from dominating while entry sums declare that estimand"):
    val rows = space("scale-rows", SpaceRole.Samples, 2)
    val latent = space("scale-latent", SpaceRole.Latent, 1)
    val binding = accepted(SharedRowBinding.verified(rows.evidence, id("scale-row-keys")))
    val smallFeatures = space("scale-small-features", SpaceRole.Observed, 1)
    val smallNatural = space("scale-small-natural", SpaceRole.Observed, 1)
    val largeFeatures = space("scale-large-features", SpaceRole.Observed, 2)
    val largeNatural = space("scale-large-natural", SpaceRole.Observed, 2)
    val codes = rowCodes(rows.evidence, latent.evidence, Vector(0.0, 0.0), "scale-codes")

    val smallMean = realBlock(
      "small-mean",
      rows.evidence,
      smallFeatures.evidence,
      latent.evidence,
      smallNatural.evidence,
      binding,
      Vector(1.0, 1.0),
      matrix(Vector(Vector(1.0))),
      BlockLossScaling.MeanObservedLoss(BlockImportance.unsafe(1.0))
    )
    val largeMean = realBlock(
      "large-mean",
      rows.evidence,
      largeFeatures.evidence,
      latent.evidence,
      largeNatural.evidence,
      binding,
      Vector(1.0, 1.0, 1.0, 1.0),
      matrix(Vector(Vector(1.0, 1.0))),
      BlockLossScaling.MeanObservedLoss(BlockImportance.unsafe(1.0))
    )
    val mean = accepted(AlignedSharedScoreGlrm.from(binding, latent.evidence, Vector(smallMean, largeMean)))
    val meanObjective = accepted(mean.evaluate(codes)).objective.blocks

    assertEqualsDouble(meanObjective(0).weightedObservedLoss, 0.5, 1e-14)
    assertEqualsDouble(meanObjective(1).weightedObservedLoss, 0.5, 1e-14)
    assert(meanObjective.forall(_.scaling.estimand.contains("mean")))

    val smallSum = realBlock(
      "small-sum",
      rows.evidence,
      smallFeatures.evidence,
      latent.evidence,
      smallNatural.evidence,
      binding,
      Vector(1.0, 1.0),
      matrix(Vector(Vector(1.0))),
      BlockLossScaling.ObservedEntrySum(BlockImportance.unsafe(1.0))
    )
    val largeSum = realBlock(
      "large-sum",
      rows.evidence,
      largeFeatures.evidence,
      latent.evidence,
      largeNatural.evidence,
      binding,
      Vector(1.0, 1.0, 1.0, 1.0),
      matrix(Vector(Vector(1.0, 1.0))),
      BlockLossScaling.ObservedEntrySum(BlockImportance.unsafe(1.0))
    )
    val sum = accepted(AlignedSharedScoreGlrm.from(binding, latent.evidence, Vector(smallSum, largeSum)))
    val sumObjective = accepted(sum.evaluate(codes)).objective.blocks

    assertEqualsDouble(sumObjective(0).weightedObservedLoss, 1.0, 1e-14)
    assertEqualsDouble(sumObjective(1).weightedObservedLoss, 2.0, 1e-14)
    assert(sumObjective.forall(_.scaling.estimand.contains("sum")))

  test("shared row-code penalties are applied exactly once"):
    val rows = space("shared-penalty-rows", SpaceRole.Samples, 2)
    val latent = space("shared-penalty-latent", SpaceRole.Latent, 1)
    val binding = accepted(SharedRowBinding.verified(rows.evidence, id("shared-penalty-keys")))
    val firstFeatures = space("shared-penalty-first-features", SpaceRole.Observed, 1)
    val firstNatural = space("shared-penalty-first-natural", SpaceRole.Observed, 1)
    val secondFeatures = space("shared-penalty-second-features", SpaceRole.Observed, 1)
    val secondNatural = space("shared-penalty-second-natural", SpaceRole.Observed, 1)
    val scaling = BlockLossScaling.ObservedEntrySum(BlockImportance.unsafe(1.0))
    val first = realBlock(
      "shared-penalty-first",
      rows.evidence,
      firstFeatures.evidence,
      latent.evidence,
      firstNatural.evidence,
      binding,
      Vector(1.0, 2.0),
      matrix(Vector(Vector(1.0))),
      scaling
    )
    val second = realBlock(
      "shared-penalty-second",
      rows.evidence,
      secondFeatures.evidence,
      latent.evidence,
      secondNatural.evidence,
      binding,
      Vector(1.0, 2.0),
      matrix(Vector(Vector(1.0))),
      scaling
    )
    val ridge = GlrmFactorPenaltyTerm(
      GlrmFactorTarget.RowCodes,
      GlrmFactorPenalty.SquaredFrobenius,
      PenaltyWeight.unsafe(2.0),
      id("shared-row-ridge")
    )
    val program = accepted(AlignedSharedScoreGlrm.from(binding, latent.evidence, Vector(first, second), Vector(ridge)))
    val codes = rowCodes(rows.evidence, latent.evidence, Vector(1.0, 2.0), "shared-penalty-codes")
    val result = accepted(program.evaluate(codes))

    assertEqualsDouble(result.objective.blocks.map(_.total).sum, 0.0, 1e-14)
    assertEqualsDouble(result.objective.sharedRowPenalty, 5.0, 1e-14)
    assertEqualsDouble(result.objective.total, 5.0, 1e-14)
    assert(result.blockScores.forall(_.sharedCodes == codes.valueIdentity))
    assert(result.blockScores.forall(_.values == result.sharedScores.values))

  test("block-local graph TV preserves its block identity and algebraic adjoint"):
    val rows = space("graph-rows", SpaceRole.Samples, 1)
    val latent = space("graph-latent", SpaceRole.Latent, 1)
    val features = space("graph-features", SpaceRole.Observed, 2)
    val natural = space("graph-natural", SpaceRole.Observed, 2)
    val edges = space("graph-edges", SpaceRole.Block, 1)
    val binding = accepted(SharedRowBinding.verified(rows.evidence, id("graph-row-keys")))
    val difference = accepted(
      Op.fromDense(
        matrix(Vector(Vector(-1.0, 1.0))),
        CoordinateEvidence.primal(natural.evidence),
        CoordinateEvidence.primal(edges.evidence),
        OperatorRoleWitness.penalty,
        id("graph-incidence")
      )
    )
    val structure = accepted(
      BlockDecoderStructure.from(
        BlockId.unsafe("graph-block"),
        natural.evidence,
        edges.evidence,
        difference,
        BlockStructuredPenaltyKind.GraphTotalVariation,
        PenaltyWeight.unsafe(0.5)
      )
    )
    val block = realBlock(
      "graph-block",
      rows.evidence,
      features.evidence,
      latent.evidence,
      natural.evidence,
      binding,
      Vector(1.0, 3.0),
      matrix(Vector(Vector(1.0, 3.0))),
      BlockLossScaling.MeanObservedLoss(BlockImportance.unsafe(1.0)),
      structures = Vector(structure)
    )
    val program = accepted(AlignedSharedScoreGlrm.from(binding, latent.evidence, Vector(block)))
    val codes = rowCodes(rows.evidence, latent.evidence, Vector(1.0), "graph-codes")
    val result = accepted(program.evaluate(codes)).objective.blocks.head

    assertEqualsDouble(result.weightedObservedLoss, 0.0, 1e-14)
    assertEqualsDouble(result.structuredPenalty, 1.0, 1e-14)
    assertEquals(result.structureAdjoints, Vector(structure.valueIdentity -> structure.adjointIdentity))
    assertEquals(structure.adjointIdentity, structure.operator.dual.valueIdentity)

  test("permuting blocks changes presentation order but not the global objective"):
    val rows = space("permutation-rows", SpaceRole.Samples, 2)
    val latent = space("permutation-latent", SpaceRole.Latent, 1)
    val binding = accepted(SharedRowBinding.verified(rows.evidence, id("permutation-row-keys")))
    val aFeatures = space("permutation-a-features", SpaceRole.Observed, 1)
    val aNatural = space("permutation-a-natural", SpaceRole.Observed, 1)
    val bFeatures = space("permutation-b-features", SpaceRole.Observed, 1)
    val bNatural = space("permutation-b-natural", SpaceRole.Observed, 1)
    val scale = BlockLossScaling.ObservedEntrySum(BlockImportance.unsafe(1.0))
    val a = realBlock(
      "permutation-a",
      rows.evidence,
      aFeatures.evidence,
      latent.evidence,
      aNatural.evidence,
      binding,
      Vector(1.0, 0.0),
      matrix(Vector(Vector(1.0))),
      scale
    )
    val b = realBlock(
      "permutation-b",
      rows.evidence,
      bFeatures.evidence,
      latent.evidence,
      bNatural.evidence,
      binding,
      Vector(0.0, 2.0),
      matrix(Vector(Vector(2.0))),
      scale
    )
    val codes = rowCodes(rows.evidence, latent.evidence, Vector(0.5, 0.5), "permutation-codes")
    val ab = accepted(accepted(AlignedSharedScoreGlrm.from(binding, latent.evidence, Vector(a, b))).evaluate(codes))
    val ba = accepted(accepted(AlignedSharedScoreGlrm.from(binding, latent.evidence, Vector(b, a))).evaluate(codes))

    assertEqualsDouble(ab.objective.total, ba.objective.total, 1e-14)
    assertEquals(ab.objective.blocks.map(_.block), Vector(BlockId.unsafe("permutation-a"), BlockId.unsafe("permutation-b")))
    assertEquals(ba.objective.blocks.map(_.block), ab.objective.blocks.map(_.block).reverse)

  test("relabeling feature and graph coordinates together preserves the structured objective"):
    val rows = space("relabel-rows", SpaceRole.Samples, 1)
    val latent = space("relabel-latent", SpaceRole.Latent, 1)
    val binding = accepted(SharedRowBinding.verified(rows.evidence, id("relabel-row-keys")))
    val originalFeatures = space("relabel-original-features", SpaceRole.Observed, 2)
    val originalNatural = space("relabel-original-natural", SpaceRole.Observed, 2)
    val originalEdges = space("relabel-original-edges", SpaceRole.Block, 1)
    val relabeledFeatures = space("relabel-permuted-features", SpaceRole.Observed, 2)
    val relabeledNatural = space("relabel-permuted-natural", SpaceRole.Observed, 2)
    val relabeledEdges = space("relabel-permuted-edges", SpaceRole.Block, 1)
    val originalOperator = accepted(
      Op.fromDense(
        matrix(Vector(Vector(-1.0, 1.0))),
        CoordinateEvidence.primal(originalNatural.evidence),
        CoordinateEvidence.primal(originalEdges.evidence),
        OperatorRoleWitness.penalty,
        id("relabel-original-incidence")
      )
    )
    val relabeledOperator = accepted(
      Op.fromDense(
        matrix(Vector(Vector(1.0, -1.0))),
        CoordinateEvidence.primal(relabeledNatural.evidence),
        CoordinateEvidence.primal(relabeledEdges.evidence),
        OperatorRoleWitness.penalty,
        id("relabel-permuted-incidence")
      )
    )
    val originalStructure = accepted(
      BlockDecoderStructure.from(
        BlockId.unsafe("relabel-original"),
        originalNatural.evidence,
        originalEdges.evidence,
        originalOperator,
        BlockStructuredPenaltyKind.GraphSmoothness,
        PenaltyWeight.unsafe(1.0)
      )
    )
    val relabeledStructure = accepted(
      BlockDecoderStructure.from(
        BlockId.unsafe("relabel-permuted"),
        relabeledNatural.evidence,
        relabeledEdges.evidence,
        relabeledOperator,
        BlockStructuredPenaltyKind.GraphSmoothness,
        PenaltyWeight.unsafe(1.0)
      )
    )
    val scale = BlockLossScaling.MeanObservedLoss(BlockImportance.unsafe(1.0))
    val original = realBlock(
      "relabel-original",
      rows.evidence,
      originalFeatures.evidence,
      latent.evidence,
      originalNatural.evidence,
      binding,
      Vector(1.0, 3.0),
      matrix(Vector(Vector(1.0, 3.0))),
      scale,
      Vector(originalStructure)
    )
    val relabeled = realBlock(
      "relabel-permuted",
      rows.evidence,
      relabeledFeatures.evidence,
      latent.evidence,
      relabeledNatural.evidence,
      binding,
      Vector(3.0, 1.0),
      matrix(Vector(Vector(3.0, 1.0))),
      scale,
      Vector(relabeledStructure)
    )
    val codes = rowCodes(rows.evidence, latent.evidence, Vector(1.0), "relabel-codes")
    val originalObjective = accepted(
      accepted(AlignedSharedScoreGlrm.from(binding, latent.evidence, Vector(original))).evaluate(codes)
    ).objective.total
    val relabeledObjective = accepted(
      accepted(AlignedSharedScoreGlrm.from(binding, latent.evidence, Vector(relabeled))).evaluate(codes)
    ).objective.total

    assertEqualsDouble(originalObjective, 2.0, 1e-14)
    assertEqualsDouble(relabeledObjective, originalObjective, 1e-14)

  test("splitting importance gives the declared block-duplication invariance"):
    val original = BlockLossScaling.MeanObservedLoss(BlockImportance.unsafe(2.0))
    val copies = accepted(original.splitImportance(2))

    assertEquals(copies.length, 2)
    assertEqualsDouble(accepted(original.effectiveCoefficient(4)), 0.5, 1e-14)
    assertEqualsDouble(copies.map(scale => accepted(scale.effectiveCoefficient(4))).sum, 0.5, 1e-14)
    assert(original.splitImportance(0).isLeft)

  test("duplicating an identical block with split importance preserves the fitted objective"):
    val rows = space("duplication-rows", SpaceRole.Samples, 1)
    val latent = space("duplication-latent", SpaceRole.Latent, 1)
    val binding = accepted(SharedRowBinding.verified(rows.evidence, id("duplication-row-keys")))
    val originalFeatures = space("duplication-original-features", SpaceRole.Observed, 1)
    val originalNatural = space("duplication-original-natural", SpaceRole.Observed, 1)
    val leftFeatures = space("duplication-left-features", SpaceRole.Observed, 1)
    val leftNatural = space("duplication-left-natural", SpaceRole.Observed, 1)
    val rightFeatures = space("duplication-right-features", SpaceRole.Observed, 1)
    val rightNatural = space("duplication-right-natural", SpaceRole.Observed, 1)
    val split = accepted(
      BlockLossScaling.MeanObservedLoss(BlockImportance.unsafe(2.0)).splitImportance(2)
    )
    val original = realBlock(
      "duplication-original",
      rows.evidence,
      originalFeatures.evidence,
      latent.evidence,
      originalNatural.evidence,
      binding,
      Vector(1.0),
      matrix(Vector(Vector(1.0))),
      BlockLossScaling.MeanObservedLoss(BlockImportance.unsafe(2.0))
    )
    val left = realBlock(
      "duplication-left",
      rows.evidence,
      leftFeatures.evidence,
      latent.evidence,
      leftNatural.evidence,
      binding,
      Vector(1.0),
      matrix(Vector(Vector(1.0))),
      split(0)
    )
    val right = realBlock(
      "duplication-right",
      rows.evidence,
      rightFeatures.evidence,
      latent.evidence,
      rightNatural.evidence,
      binding,
      Vector(1.0),
      matrix(Vector(Vector(1.0))),
      split(1)
    )
    val codes = rowCodes(rows.evidence, latent.evidence, Vector(0.0), "duplication-codes")
    val originalObjective = accepted(
      accepted(AlignedSharedScoreGlrm.from(binding, latent.evidence, Vector(original))).evaluate(codes)
    ).objective.total
    val duplicatedObjective = accepted(
      accepted(AlignedSharedScoreGlrm.from(binding, latent.evidence, Vector(left, right))).evaluate(codes)
    ).objective.total

    assertEqualsDouble(originalObjective, 1.0, 1e-14)
    assertEqualsDouble(duplicatedObjective, originalObjective, 1e-14)

  test("joint partial encoding matches an analytic ridge oracle and reports block contributions"):
    val rows = space("encoding-training-rows", SpaceRole.Samples, 2)
    val newRows = space("encoding-new-row", SpaceRole.Samples, 1)
    val latent = space("encoding-latent", SpaceRole.Latent, 1)
    val binding = accepted(SharedRowBinding.verified(rows.evidence, id("encoding-row-keys")))
    val aFeatures = space("encoding-a-features", SpaceRole.Observed, 1)
    val aNatural = space("encoding-a-natural", SpaceRole.Observed, 1)
    val bFeatures = space("encoding-b-features", SpaceRole.Observed, 1)
    val bNatural = space("encoding-b-natural", SpaceRole.Observed, 1)
    val scale = BlockLossScaling.ObservedEntrySum(BlockImportance.unsafe(1.0))
    val a = realBlock(
      "encoding-a",
      rows.evidence,
      aFeatures.evidence,
      latent.evidence,
      aNatural.evidence,
      binding,
      Vector(0.0, 0.0),
      matrix(Vector(Vector(1.0))),
      scale
    )
    val b = realBlock(
      "encoding-b",
      rows.evidence,
      bFeatures.evidence,
      latent.evidence,
      bNatural.evidence,
      binding,
      Vector(0.0, 0.0),
      matrix(Vector(Vector(2.0))),
      scale
    )
    val ridge = GlrmFactorPenaltyTerm(
      GlrmFactorTarget.RowCodes,
      GlrmFactorPenalty.SquaredFrobenius,
      PenaltyWeight.unsafe(1.0),
      id("encoding-shared-ridge")
    )
    val program = accepted(AlignedSharedScoreGlrm.from(binding, latent.evidence, Vector(a, b), Vector(ridge)))
    val aPattern = accepted(
      ObservationPattern.from(
        newRows.evidence,
        aFeatures.evidence,
        Vector(ObservationCell.Observed(2.0)),
        id("encoding-a-pattern")
      )
    )
    val bPattern = accepted(
      ObservationPattern.from(
        newRows.evidence,
        bFeatures.evidence,
        Vector(ObservationCell.Observed(4.0)),
        id("encoding-b-pattern")
      )
    )
    val aInput = accepted(PartialAlignedBlockObservation.from(a, aPattern))
    val bInput = accepted(PartialAlignedBlockObservation.from(b, bPattern))
    val result = accepted(program.fittedEncoder().encode(Vector(bInput, aInput)))
    val oracle = 10.0 / 6.0

    assertEqualsDouble(result.global.code.values(0), oracle, 4e-7)
    assertEquals(result.blocks.map(_.block), Vector(BlockId.unsafe("encoding-a"), BlockId.unsafe("encoding-b")))
    assertEqualsDouble(result.weightedBlockLoss, result.blocks.map(_.weightedObservedLoss).sum, 1e-14)
    assertEqualsDouble(result.objective, result.global.objective.total, 2e-10)
    assert(result.global.certificate.proxGradientResidual <= 5e-7)
    assert(result.blocks.forall(_.decoded.length == 1))

  test("misaligned rows, foreign fitted blocks, and incompatible domains fail before numerical execution"):
    val rows = space("failure-rows", SpaceRole.Samples, 2)
    val foreignRows = space("failure-foreign-rows", SpaceRole.Samples, 2)
    val latent = space("failure-latent", SpaceRole.Latent, 1)
    val features = space("failure-features", SpaceRole.Observed, 1)
    val natural = space("failure-natural", SpaceRole.Observed, 1)
    val binding = accepted(SharedRowBinding.verified(rows.evidence, id("failure-row-keys")))
    val foreignEvidence = SpaceEvidence.unsafe[rows.Id](foreignRows.descriptor)
    val foreignBinding = accepted(SharedRowBinding.verified(foreignEvidence, id("failure-foreign-keys")))
    val binaryBlock = binaryBlockFixture(
      "failure-binary",
      rows.evidence,
      features.evidence,
      latent.evidence,
      natural.evidence,
      binding
    )
    val invalidPattern = accepted(
      ObservationPattern.from(
        space("failure-new-row", SpaceRole.Samples, 1).evidence,
        features.evidence,
        Vector(ObservationCell.Observed(2.0)),
        id("failure-invalid-pattern")
      )
    )

    assert(PartialAlignedBlockObservation.from(binaryBlock, invalidPattern).left.exists:
      case StructuredMultiblockGlrmError.Generalized(_: GeneralizedLowRankError.InvalidObservedValue) => true
      case _ => false
    )
    assert(
      AlignedGlrmBlock
        .from(
          binaryBlock.id,
          binaryBlock.program,
          binaryBlock.decoder,
          natural.evidence,
          foreignBinding,
          binaryBlock.lossScaling,
          binaryBlock.geometry
        )
        .left
        .exists(_.message.contains("shared-row binding"))
    )
    assert(SharedRowBinding.verified(rows.evidence, id("unsafe-keys"), AlignmentOrigin.UnsafeAssumption).isLeft)

  private def realBlock[
      Rows <: SemanticSpace,
      Feature0 <: SemanticSpace,
      Latent <: SemanticSpace,
      Natural0 <: SemanticSpace
  ](
      name: String,
      rows: SpaceEvidence[Rows],
      features: SpaceEvidence[Feature0],
      latent: SpaceEvidence[Latent],
      natural: SpaceEvidence[Natural0],
      binding: SharedRowBinding[Rows],
      observed: Vector[Double],
      decoder: DMat,
      scaling: BlockLossScaling,
      structures: Vector[BlockDecoderStructure[Natural0, ? <: SemanticSpace]] =
        Vector.empty[BlockDecoderStructure[Natural0, ? <: SemanticSpace]]
  ): AlignedGlrmBlock[Rows, Latent] { type Feature = Feature0; type Natural = Natural0 } =
    val specifications = Vector.tabulate(features.dimension): index =>
      accepted(
        GlrmFeatureSpec.from(
          GlrmFeatureId.unsafe(s"$name-feature-$index"),
          FeatureDomain.Real,
          EntryLoss.Quadratic
        )
      )
    val layout = accepted(GlrmFeatureLayout.from(features, specifications, id(s"$name-layout")))
    val pattern = accepted(
      ObservationPattern.from(
        rows,
        features,
        observed.map(ObservationCell.Observed(_)),
        id(s"$name-observations")
      )
    )
    val program = accepted(
      GeneralizedLowRankProgram.from(
        pattern,
        layout,
        Vector.empty,
        MissingnessStatement.Complete,
        GlrmPredictionTarget.DescribeLatentStructure
      )
    )
    val fittedDecoder = accepted(FeatureDecoder.from(layout, latent, decoder, id(s"$name-decoder")))
    accepted(
      AlignedGlrmBlock.from(
        BlockId.unsafe(name),
        program,
        fittedDecoder,
        natural,
        binding,
        scaling,
        BlockNaturalGeometry.euclidean(natural, id(s"$name-geometry")),
        structures
      )
    )

  private def binaryBlockFixture[
      Rows <: SemanticSpace,
      Feature0 <: SemanticSpace,
      Latent <: SemanticSpace,
      Natural0 <: SemanticSpace
  ](
      name: String,
      rows: SpaceEvidence[Rows],
      features: SpaceEvidence[Feature0],
      latent: SpaceEvidence[Latent],
      natural: SpaceEvidence[Natural0],
      binding: SharedRowBinding[Rows]
  ): AlignedGlrmBlock[Rows, Latent] { type Feature = Feature0; type Natural = Natural0 } =
    val specification = accepted(
      GlrmFeatureSpec.from(GlrmFeatureId.unsafe(s"$name-feature"), FeatureDomain.Binary, EntryLoss.Logistic)
    )
    val layout = accepted(GlrmFeatureLayout.from(features, Vector(specification), id(s"$name-layout")))
    val pattern = accepted(
      ObservationPattern.from(
        rows,
        features,
        Vector(ObservationCell.Observed(0.0), ObservationCell.Observed(1.0)),
        id(s"$name-observations")
      )
    )
    val program = accepted(
      GeneralizedLowRankProgram.from(
        pattern,
        layout,
        Vector.empty,
        MissingnessStatement.Complete,
        GlrmPredictionTarget.DescribeLatentStructure
      )
    )
    val decoder = accepted(FeatureDecoder.from(layout, latent, matrix(Vector(Vector(1.0))), id(s"$name-decoder")))
    accepted(
      AlignedGlrmBlock.from(
        BlockId.unsafe(name),
        program,
        decoder,
        natural,
        binding,
        BlockLossScaling.MeanObservedLoss(BlockImportance.unsafe(1.0)),
        BlockNaturalGeometry.euclidean(natural, id(s"$name-geometry"))
      )
    )

  private def rowCodes[Rows <: SemanticSpace, Latent <: SemanticSpace](
      rows: SpaceEvidence[Rows],
      latent: SpaceEvidence[Latent],
      values: Vector[Double],
      name: String
  ): GlrmRowCodes[Rows, Latent] =
    accepted(
      GlrmRowCodes.from(
        rows,
        latent,
        GaleNumerics.matrixFromRowMajor(rows.dimension, latent.dimension, values.toArray),
        id(name)
      )
    )

  private def space(name: String, role: SpaceRole, dimension: Int): SpaceRef =
    accepted(SpaceRef.of(name, role, dimension))

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(rows)

  private def id(name: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(name))

  private def accepted[A](result: Either[?, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(s"unexpected error: $error")
