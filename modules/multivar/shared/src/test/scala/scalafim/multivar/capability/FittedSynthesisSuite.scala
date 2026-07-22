package scalafim.multivar
package capability

import scalafim.multivar.core.*
import scalafim.multivar.capability.*
import scalafim.multivar.family.paired.*

import gale.linalg.DMat
import gale.linalg.DVec

class FittedSynthesisSuite extends munit.FunSuite:

  import ProjectionParityReferenceFixtures as R

  private val sourceFeatureIds =
    Vector("motion", "age", "task", "site").map(FeatureId.unsafe)

  private def sourcePreprocessor: FittedColumnAffine =
    val scale = R.scale.map(value => 1.0 / value)
    val shift = R.center.zip(scale).map((center, weight) => -center * weight)
    FittedColumnAffine(R.center.length, DVec.fromSeq(scale), DVec.fromSeq(shift))

  private def sourceTransform: FittedFrameTransform =
    FittedFrameTransform
      .fromTraining(
        MatrixView.dense(R.newRaw),
        R.weights,
        sourcePreprocessor,
        "synthesis-source",
        ComponentCount.unsafe(2),
        featureIds = Some(sourceFeatureIds)
      )
      .toOption
      .get

  private def targetPreprocessor: FittedColumnAffine =
    val scale = R.pairedTargetScale.map(value => 1.0 / value)
    val shift = R.pairedTargetCenter.zip(scale).map((center, weight) => -center * weight)
    FittedColumnAffine(R.pairedTargetCenter.length, DVec.fromSeq(scale), DVec.fromSeq(shift))

  private def targetTransform: FittedFrameTransform =
    val training = GaleNumerics.matrixFromRows(
      Vector(
        R.pairedTargetCenter,
        R.pairedTargetCenter.zip(R.pairedTargetScale).map(_ + _),
        R.pairedTargetCenter.zip(R.pairedTargetScale).map(_ - _)
      )
    )
    FittedFrameTransform
      .fromTraining(
        MatrixView.dense(training),
        R.pairedTargetWeights,
        targetPreprocessor,
        "synthesis-target",
        ComponentCount.unsafe(2),
        featureIds = Some(Vector("response-a", "response-b", "response-c").map(FeatureId.unsafe))
      )
      .toOption
      .get

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double = 1e-12): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1

  private def selected(input: DMat, columns: Vector[Int]): DMat =
    input.selectColumns(columns)

  test("analysis-only fits fail with a typed synthesis capability error"):
    sourceTransform.requireSynthesis match
      case Left(MultivarError.DecoderUnavailable(detail)) => assert(detail.contains("analysis capability only"))
      case other => fail(s"expected synthesis capability failure, got $other")

  test("explicit synthesis reproduces working and original-coordinate R fixtures"):
    val analysis = sourceTransform
    val synthesis = analysis
      .withExplicitSynthesis(
        R.decoder,
        ValueIdentity.source(ValueId.unsafe("projection-fixture-decoder"))
      )
      .toOption
      .get
    val working = synthesis
      .reconstruct(MatrixView.dense(R.newRaw), ReconstructionCoordinate.Working)
      .toOption
      .get
    val original = synthesis
      .reconstruct(MatrixView.dense(R.newRaw), ReconstructionCoordinate.Original)
      .toOption
      .get
    val supplied = synthesis.synthesizeWorking(R.fullScores).toOption.get

    assertMatrixClose(working.values, R.reconstructionWorking)
    assertMatrixClose(original.values, R.reconstructionRaw)
    assertMatrixClose(supplied.values, R.reconstructionWorking)
    assertEquals(synthesis.decoder.role.value, OperatorRole.Synthesis)
    assertEquals(original.provenance.policy, synthesis.policy)
    assertEquals(original.provenance.source, ReconstructionSource.FullProjection)

  test("Euclidean least-squares synthesis solves for the decoder without inversion"):
    val synthesis = sourceTransform
      .withEuclideanSynthesis(Ridge(0.0).toOption.get)
      .toOption
      .get

    assertMatrixClose(synthesis.decoderValues, R.decoder)
    assertMatrixClose(
      synthesis.reconstruct(MatrixView.dense(R.newRaw)).toOption.get.values,
      R.reconstructionRaw
    )

  test("orthonormal transpose is capability-gated by a measured frame law"):
    val input = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 2.0, 3.0),
          Vector(4.0, 5.0, 6.0),
          Vector(-1.0, 0.0, 2.0)
        )
      )
    )
    val weights = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 0.0)
      )
    )
    val transform = FittedFrameTransform
      .fromTraining(
        input,
        weights,
        PreprocessSpec.Pass.fit(input).toOption.get,
        "orthonormal-synthesis",
        ComponentCount.unsafe(2)
      )
      .toOption
      .get
    val synthesis = transform.withOrthonormalSynthesis().toOption.get

    assertMatrixClose(synthesis.decoderValues, weights.transpose)
    assert(sourceTransform.withOrthonormalSynthesis().isLeft)

  test("truncated and selected-output reconstruction restrict synthesis independently"):
    val synthesis = sourceTransform
      .withExplicitSynthesis(R.decoder, ValueIdentity.source(ValueId.unsafe("selected-decoder")))
      .toOption
      .get
    val firstComponent = IndexSet.from(Vector(0), IndexAxis.Component).toOption.get
    val outputFeatures = IndexSet.from(Vector(1, 3), IndexAxis.Feature).toOption.get
    val reversedComponents = IndexSet.from(Vector(1, 0), IndexAxis.Component).toOption.get
    val truncated = synthesis
      .reconstruct(
        MatrixView.dense(R.newRaw),
        ReconstructionCoordinate.Working,
        components = Some(firstComponent)
      )
      .toOption
      .get
    val expectedTruncated = GaleNumerics.multiply(
      R.fullScores.selectColumns(Vector(0)),
      R.decoder.selectRows(Vector(0))
    )
    val selectedWorking = synthesis
      .reconstruct(
        MatrixView.dense(R.newRaw),
        ReconstructionCoordinate.Working,
        targetFeatures = Some(outputFeatures)
      )
      .toOption
      .get
    val selectedOriginal = synthesis
      .reconstruct(
        MatrixView.dense(R.newRaw),
        ReconstructionCoordinate.Original,
        targetFeatures = Some(outputFeatures)
      )
      .toOption
      .get
    val reordered = synthesis
      .reconstruct(
        MatrixView.dense(R.newRaw),
        ReconstructionCoordinate.Working,
        components = Some(reversedComponents)
      )
      .toOption
      .get
    val expectedReordered = GaleNumerics.multiply(
      R.fullScores.selectColumns(Vector(1, 0)),
      R.decoder.selectRows(Vector(1, 0))
    )

    assertMatrixClose(truncated.values, expectedTruncated)
    assertMatrixClose(reordered.values, expectedReordered)
    assertMatrixClose(selectedWorking.values, selected(R.reconstructionWorking, Vector(1, 3)))
    assertMatrixClose(selectedOriginal.values, selected(R.reconstructionRaw, Vector(1, 3)))
    assertEquals(selectedOriginal.provenance.targetFeatures, Vector(sourceFeatureIds(1), sourceFeatureIds(3)))

  test("partial input score policy and selected output features remain separate"):
    val analysis = sourceTransform
    val synthesis = analysis
      .withExplicitSynthesis(R.decoder, ValueIdentity.source(ValueId.unsafe("partial-decoder")))
      .toOption
      .get
    val columns = IndexSet.from(R.subset, IndexAxis.Feature).toOption.get
    val partial = analysis.restrictFeatures(columns).toOption.get
    val input = partial.restriction
      .bind(MatrixView.dense(R.partialRaw), partial.restriction.restrictedSchema)
      .toOption
      .get
    val output = IndexSet.from(Vector(1), IndexAxis.Feature).toOption.get
    val contribution = synthesis
      .reconstructPartial(
        partial,
        input,
        PartialScorePolicy.Contribution,
        ReconstructionCoordinate.Working,
        Some(output)
      )
      .toOption
      .get
    val recoveryPolicy = PartialScorePolicy
      .euclideanLeastSquares(R.subset.length, R.ridge)
      .toOption
      .get
    val recovered = synthesis
      .reconstructPartial(
        partial,
        input,
        recoveryPolicy,
        ReconstructionCoordinate.Working,
        Some(output)
      )
      .toOption
      .get
    val expectedContribution = GaleNumerics.multiply(R.partialContribution, R.decoder.selectColumns(Vector(1)))
    val expectedRecovered = GaleNumerics.multiply(R.partialLeastSquares, R.decoder.selectColumns(Vector(1)))

    assertMatrixClose(contribution.values, expectedContribution)
    assertMatrixClose(recovered.values, expectedRecovered)
    assert(Math.abs(contribution.values(0, 0) - recovered.values(0, 0)) > 1e-3)
    assertEquals(recovered.provenance.targetFeatures, Vector(sourceFeatureIds(1)))
    assertEquals(recovered.provenance.source, ReconstructionSource.PartialProjection(recoveryPolicy))

  test("paired transfer composes source analysis, explicit scaling, and target synthesis"):
    val source = sourceTransform
      .withExplicitSynthesis(R.decoder, ValueIdentity.source(ValueId.unsafe("transfer-source-decoder")))
      .toOption
      .get
    val target = targetTransform
      .withExplicitSynthesis(
        R.pairedTargetDecoder,
        ValueIdentity.source(ValueId.unsafe("transfer-target-decoder"))
      )
      .toOption
      .get
    val scaling = ComponentScaling.identity(2).toOption.get
    val transfer = PairedTransfer
      .from(PairedTransferEstimand.Plsc, source, target, scaling)
      .toOption
      .get
    val working = transfer(MatrixView.dense(R.newRaw), ReconstructionCoordinate.Working).toOption.get
    val original = transfer(MatrixView.dense(R.newRaw), ReconstructionCoordinate.Original).toOption.get

    assertMatrixClose(working.values, R.transferWorking)
    assertMatrixClose(original.values, R.transferRaw)
    assertEquals(original.provenance.estimand, PairedTransferEstimand.Plsc)
    assertEquals(original.provenance.orientation.source, source.analysis.featureSpace.descriptor)
    assertEquals(original.provenance.orientation.target, target.analysis.featureSpace.descriptor)

  test("paired transfer rejects equal domains and incompatible component scaling"):
    val source = sourceTransform
      .withExplicitSynthesis(R.decoder, ValueIdentity.source(ValueId.unsafe("invalid-transfer-decoder")))
      .toOption
      .get
    val identity = ComponentScaling.identity(2).toOption.get
    val badScaling = ComponentScaling
      .from(
        GaleNumerics.matrixFromRows(Vector(Vector(1.0), Vector(0.0))),
        ComponentScalingId.unsafe("bad-component-scaling")
      )
      .toOption
      .get

    assert(PairedTransfer.from(PairedTransferEstimand.Plsc, source, source, identity).isLeft)
    val target = targetTransform
      .withExplicitSynthesis(R.pairedTargetDecoder, ValueIdentity.source(ValueId.unsafe("bad-target-decoder")))
      .toOption
      .get
    assert(PairedTransfer.from(PairedTransferEstimand.Plsc, source, target, badScaling).isLeft)

  test("public paired-transfer factories are limited to PLSC and CCA fitted estimands"):
    val x = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 1.0),
          Vector(-1.0, 0.0),
          Vector(0.0, -1.0),
          Vector(0.5, 0.5)
        )
      )
    )
    val y = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(2.0, 0.1),
          Vector(0.0, 1.5),
          Vector(-2.0, -0.1),
          Vector(0.0, -1.5),
          Vector(1.0, 0.8)
        )
      )
    )
    val components = ComponentCount.unsafe(1)
    val plsc = Plsc.fit(x, y, components).toOption.get
    val plscSource = plsc.sourceTransform.withEuclideanSynthesis(Ridge(0.0).toOption.get).toOption.get
    val plscTarget = plsc.targetTransform.withEuclideanSynthesis(Ridge(0.0).toOption.get).toOption.get
    val one = ComponentScaling.identity(1).toOption.get
    val plscTransfer = PairedTransfer.forPlsc(plsc, plscSource, plscTarget, one).toOption.get
    assertEquals(plscTransfer(x).toOption.get.values.rows, x.rows)

    val cca = Cca.fit(x, y, components, ridge = 0.1).toOption.get
    val ccaSource = cca.sourceTransform.withEuclideanSynthesis(Ridge(0.0).toOption.get).toOption.get
    val ccaTarget = cca.targetTransform.withEuclideanSynthesis(Ridge(0.0).toOption.get).toOption.get
    val ccaTransfer = PairedTransfer.forCca(cca, ccaSource, ccaTarget, one).toOption.get
    assertEquals(ccaTransfer(x).toOption.get.values.cols, y.cols)
