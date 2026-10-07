package scalafim.fmri.fit.profile

import gale.linalg.Matrix
import scalafim.dataset.{DataSelection, DatasetError, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, InMemoryDatasetBackend}
import scalafim.image.SampleSpaces
import scalafim.fmri.design.{RowLayout, ScanIndex}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, DctCutoffPeriod, Intercept}
import scalafim.fmri.hrf.family.{GaussianFamily, NormalizationRule, ParametricHrfFamily, ShapePoint}
import scalafim.fmri.design.hrf.HrfKernelBasis
import scalafim.fmri.model.{FitConfig, ProfileHrfPlan}
import scala.collection.mutable.ArrayBuffer

class ProfileHrfTrialOutputsSuite extends ProfileHrfFitSuite:
  protected def outputPolicy(block: Int = 1, workers: Int = 1): ProfileDecodePolicy =
    parallelPolicy(block, workers).copy(budget = DecodeBudget(maxNewtonSteps = 16, maxJets = 20,
      maxExactEvaluations = 40, maxCandidateAttempts = 12, stationarityStepTolerance = 1e-8))

  protected def outputPrepared(block: Int = 1, workers: Int = 1): PreparedProfileHrf =
    parallelChecked(ProfileHrfFit.prepare(plan(0.4), selection, parallelWhitening, outputPolicy(block, workers)))

  protected def outputSink(values: ArrayBuffer[ProfileTrialOutputBlock]): BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt] =
    new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
      def accept(block: VoxelBlock, value: ProfileTrialOutputBlock): Either[String, ProfileFitReceipt] =
        values += value
        Right(ProfileFitReceipt(value.ordinal, value.voxelIds))

  protected def emitted(voxel: ProfileTrialOutputVoxel): (ProfileTrialReference, ProfileTrialReadoutResult) = voxel.output match
    case ProfileTrialOutputOutcome.Emitted(reference, value) => (reference, value)
    case other => fail(s"expected accepted conditional readout, got $other at ${voxel.selection}")

  private def execute(view: PreparedProfileTrialOutputs, request: OutputRequest,
      mode: ProfileTrialReadoutMode): (ProfileRunSummary, Vector[ProfileTrialOutputVoxel]) =
    val values = ArrayBuffer.empty[ProfileTrialOutputBlock]
    val summary = parallelChecked(view.run(reader, request, mode, outputSink(values)))
    (summary, values.toVector.flatMap(_.results))

  test("public physical axes keep preparation identity, caller order and structural nuisance ids"):
    val prepared = outputPrepared()
    val view = parallelChecked(prepared.trialOutputs)
    val equivalent = parallelChecked(PreparedProfileTrialOutputs.make(prepared, view.axis.preparation, view.bank))
    assert(view.axis.sameBinding(equivalent.axis))
    assert(view.axis.preparation.source eq view.axis.source)
    assert(view.bank.preparation eq view.axis.preparation)
    assertEquals(view.axis.trialIds, drive.trialLabels)
    assertEquals(view.axis.conditionIds, drive.conditionLabels)
    assertEquals(view.axis.conditionForTrial, drive.conditionForTrial)
    assertEquals(view.axis.nuisanceColumnIds, baseline.compiledSchema.get.coefficientAxis.columnIds)
    assertEquals(view.axis.selectedResponseRows, (1 to rows).toVector.map(ScanIndex.unsafeOneBased))
    assertEquals(baseline.compiledSchema.get.rows, RowLayout.fromSamplingFrame(frame))
    val q = ProfileTrialSignedQuery.make("same binding", equivalent.axis, Vector(1.0, -1.0, 0, 0, 0, 0), 1e-6).toOption.get
    assert(OutputRequest.TrialQueries(Vector(q), NormalizationRule.Density).validateForTrial(view.axis).isRight)

  test("accepted AR-reset signed off-node outputs equal a separately invoked existing readout exactly once whitened"):
    val view = parallelChecked(outputPrepared(2).trialOutputs)
    for mode <- Vector(ProfileTrialReadoutMode.CorrectedReference, ProfileTrialReadoutMode.ExactShape) do
      val request = OutputRequest.TrialAmplitudes(NormalizationRule.Unnormalised)
      val (summary, voxels) = execute(view, request, mode)
      assertEquals(voxels.map(_.voxelId), Vector(3, 0, 2))
      assertEquals(summary.receipts.map(_.voxelIds), Vector(Vector(3, 0), Vector(2)))
      assert(voxels.exists(v => (0 until view.bank.grid.count).forall(i => view.bank.grid.point(i).coordinates != v.selection.coordinates)))
      voxels.foreach { voxel =>
        val (reference, actual) = emitted(voxel)
        assert(actual.axis eq view.axis)
        val response = ProfileTrialResponse.make(view.axis, view.axis.selectedResponseRows,
          ProfileTrialResponseDomain.Original, responseColumns(voxel.voxelId)).toOption.get
        val operator = ProfileTrialReadout.freeze(view.bank, view.axis, voxel.selection.coordinates,
          reference.index, request.rule, mode).fold(error => fail(error.message), identity)
        val expected = operator.newWorker().evaluate(response, request).fold(error => fail(error.message), identity)
        actual.trialAmplitudes.get.zip(expected.trialAmplitudes.get).foreach((a, b) => assertEqualsDouble(a, b, 1e-10))
        actual.nuisanceCoefficients.zip(expected.nuisanceCoefficients).foreach((a, b) => assertEqualsDouble(a, b, 1e-10))
        assertEquals(actual.actualCoordinates, voxel.selection.coordinates)
        assertEquals(actual.work.whiteningForwardRowsVisited, 0)
        assertEquals(actual.work.referenceInverseAttempts, if mode == ProfileTrialReadoutMode.ExactShape then 1L else 3L)
        assertEquals(actual.work.exactReadoutFactorAttempts, if mode == ProfileTrialReadoutMode.ExactShape then 1L else 0L)
        if mode == ProfileTrialReadoutMode.ExactShape then
          val (energy, condition, trial, nuisance) = denseAllAt(voxel.voxelId, voxel.selection.coordinates)
          assertEqualsDouble(voxel.selection.energy, energy, 5e-7)
          actual.trialAmplitudes.get.zip(trial).foreach((a, b) => assertEqualsDouble(a, b, 5e-7))
          actual.conditionMeans.zip(condition).foreach((a, b) => assertEqualsDouble(a, b, 5e-7))
          actual.nuisanceCoefficients.zip(nuisance).foreach((a, b) => assertEqualsDouble(a, b, 5e-7))
      }
      val public = summary.progress.publicReadout.get
      assertEquals(public.successes, 3L)
      assertEquals(public.failures, 0L)
      assertEquals(public.numerical.voxels, 3L)
      assertEquals(summary.progress.trial.get.voxels, 6L)
      assertEquals(summary.progress.trial.get.attempted.conditionalReadoutAttempts, public.numerical.attempted.conditionalReadoutAttempts)
      assertEquals(public.completedResponseRowsEncoded, 3L * rows)
      assertEquals(public.storage.emittedTrialAmplitudeValues, 18L)
      assertEquals(summary.progress.attemptedVoxels, 3)
      assertEquals(view.bank.setupReceipt, summary.setup.bankSetup.get)
      val (again, repeated) = execute(view, request, mode)
      assertEquals(again.progress, summary.progress)
      assertEquals(repeated.map(_.selection), voxels.map(_.selection))

  test("normalization transports lambda and trial units while keeping nuisance units and raw selection"):
    val view = parallelChecked(outputPrepared().trialOutputs)
    val (_, native) = execute(view, OutputRequest.TrialAmplitudes(NormalizationRule.Unnormalised), ProfileTrialReadoutMode.ExactShape)
    val (_, normalized) = execute(view, OutputRequest.TrialAmplitudes(NormalizationRule.Density), ProfileTrialReadoutMode.ExactShape)
    native.zip(normalized).foreach { (a, b) =>
      assertEquals(a.selection, b.selection)
      val (_, raw) = emitted(a)
      val (_, scaled) = emitted(b)
      assertEqualsDouble(raw.normalizationScale, 1.0, 0.0)
      assertEqualsDouble(scaled.nativeLambda, 2.5, 0.0)
      assertEqualsDouble(scaled.equivalentNormalizedLambda, 2.5 * scaled.normalizationScale * scaled.normalizationScale, 1e-12)
      raw.trialAmplitudes.get.zip(scaled.trialAmplitudes.get).foreach((x, y) => assertEqualsDouble(x / scaled.normalizationScale, y, 1e-10))
      assertEquals(raw.nuisanceCoefficients, scaled.nuisanceCoefficients)
    }

  test("public corrected provenance identifies execution intent without the legacy exact route assertion"):
    val view = parallelChecked(outputPrepared().trialOutputs)
    val request = OutputRequest.TrialAmplitudes(NormalizationRule.Density)
    val (corrected, _) = execute(view, request, ProfileTrialReadoutMode.CorrectedReference)
    val (exact, _) = execute(view, request, ProfileTrialReadoutMode.ExactShape)
    assertEquals(corrected.progress.publicReadout.get.numerical.attempted.exactReadoutFactorAttempts, 0L)
    assert(!corrected.provenance.contains("|exact-readout=true"), corrected.provenance)
    assertNotEquals(corrected.provenance, exact.provenance)
    val declaration = corrected.publicExecution.get
    assert(declaration.axis eq view.axis)
    assertEquals(declaration.mode, ProfileTrialReadoutMode.CorrectedReference)
    assertEquals(declaration.normalization, NormalizationRule.Density)
    assertEquals(declaration.evidence, ProfileTrialEvidenceRequest.PreparedBasisResidual)
    assertEquals(declaration.outputKind, ProfileTrialOutputKind.TrialAmplitudes)
    assertEquals(declaration.provenance, corrected.provenance)
    assertEquals(corrected.progress.publicExecution, corrected.publicExecution)
    assertEquals(exact.publicExecution.get.mode, ProfileTrialReadoutMode.ExactShape)
    val (repeat, _) = execute(view, request, ProfileTrialReadoutMode.CorrectedReference)
    assertEquals(repeat.publicExecution, corrected.publicExecution)
    assertEquals(repeat.publicExecution.get.hashCode(), declaration.hashCode())
    assertEquals(repeat.provenance, corrected.provenance)

  test("checked public declarations retain ordered query values, tolerances, output kind and exact preparation binding"):
    val prepared = outputPrepared()
    val view = parallelChecked(prepared.trialOutputs)
    val equivalent = parallelChecked(PreparedProfileTrialOutputs.make(prepared, view.axis.preparation, view.bank))
    val weights = Vector(0.5, -0.3, 0.0, 0.2, -0.1, 0.7)
    def query(axis: ProfileTrialAxis, label: String, values: Vector[Double] = weights, tolerance: Double = 1e-6) =
      ProfileTrialSignedQuery.make(label, axis, values, tolerance).toOption.get
    val first = query(view.axis, "same|query:[x]")
    val second = query(view.axis, "second", weights.reverse)
    val request = OutputRequest.TrialQueries(Vector(first, second), NormalizationRule.Density)
    val mode = ProfileTrialReadoutMode.CorrectedReference
    val declaration = parallelChecked(view.executionDeclaration(request, mode))
    assertEquals(declaration.contract, "profile-public-trial-output/v1")
    assertEquals(declaration.queries.map(_.label), Vector(first.label, second.label))
    assertEquals(declaration.queries.map(_.weights.map(java.lang.Double.toHexString)),
      Vector(weights.map(java.lang.Double.toHexString), weights.reverse.map(java.lang.Double.toHexString)))
    declaration.queries.foreach(q => assertEqualsDouble(q.absoluteTolerance, 1e-6, 0.0))
    assertEquals(declaration.preparationProvenance, prepared.provenance.stripSuffix("|exact-readout=true"))
    val repeated = OutputRequest.TrialQueries(Vector(query(equivalent.axis, first.label),
      query(equivalent.axis, second.label, weights.reverse)), NormalizationRule.Density)
    val same = parallelChecked(equivalent.executionDeclaration(repeated, mode))
    assertEquals(same, declaration)
    assertEquals(same.hashCode(), declaration.hashCode())
    val changes = Vector(
      OutputRequest.TrialQueries(Vector(query(view.axis, first.label, weights.updated(0, 0.6)), second), NormalizationRule.Density),
      OutputRequest.TrialQueries(Vector(query(view.axis, first.label, tolerance = 2e-6), second), NormalizationRule.Density),
      OutputRequest.TrialQueries(Vector(second, first), NormalizationRule.Density),
      OutputRequest.TrialQueries(Vector(first, second), NormalizationRule.Unnormalised),
      OutputRequest.TrialAmplitudes(NormalizationRule.Density),
      OutputRequest.ConditionAmplitudes(NormalizationRule.Density),
      OutputRequest.ConditionQueries(Vector(SignedQuery.make(first.label,
        Vector.fill(view.axis.conditionIds.length)(0.5), 1e-6).toOption.get), NormalizationRule.Density))
    changes.foreach: changed =>
      val actual = parallelChecked(view.executionDeclaration(changed, mode))
      assertNotEquals(actual, declaration)
      assertNotEquals(actual.provenance, declaration.provenance)
    assertNotEquals(parallelChecked(view.executionDeclaration(request, ProfileTrialReadoutMode.ExactShape)), declaration)
    val foreign = parallelChecked(outputPrepared().trialOutputs)
    val foreignRequest = OutputRequest.TrialQueries(Vector(query(foreign.axis, first.label),
      query(foreign.axis, second.label, weights.reverse)), NormalizationRule.Density)
    assertNotEquals(parallelChecked(foreign.executionDeclaration(foreignRequest, mode)), declaration)
    assert(view.executionDeclaration(foreignRequest, mode).isLeft)
    assert(view.executionDeclaration(request, mode, ProfileTrialEvidenceRequest.CertifiedOriginalEquations).isLeft)

  test("nearest bank reference uses interval-scaled distances, lower ties and final rather than starting nodes"):
    val view = parallelChecked(outputPrepared().trialOutputs)
    val grid = view.bank.grid
    def oracle(coords: Vector[Double]): Int =
      (0 until grid.count).minBy { index =>
        val ref = grid.point(index).coordinates
        (coords.indices.map(i => math.pow((coords(i) - ref(i)) / grid.step(i), 2)).sum, index)
      }
    val half = grid.point(0).coordinates.updated(0, grid.coordinateOf(0, 0) + grid.step(0) / 2)
    assertEquals(view.referenceAt(half).toOption.get.index, 0)
    val points = Vector(half, grid.point(grid.count - 1).coordinates,
      Vector(grid.chart.lower(0) + 0.73 * grid.chart.width(0), grid.chart.lower(1) + 0.19 * grid.chart.width(1)))
    points.foreach(coords => assertEquals(view.referenceAt(coords).toOption.get.index, oracle(coords)))
    val (_, results) = execute(view, OutputRequest.TrialAmplitudes(NormalizationRule.Density), ProfileTrialReadoutMode.CorrectedReference)
    results.foreach(v => assertEquals(emitted(v)._1.index, oracle(v.selection.coordinates)))
    // The correlated prior is centered at a data-qualified continuous optimum.
    // It changes the discrete bank minimizer without changing that optimum;
    // its metric differs from the nearest-reference chart metric.
    val x = designAt(Vector(6.77, math.log(2.2)))
    val amplitudes = Vector(-1.2, 0.4, 0.8, 0.9, 1.1, 1.9)
    val column = Vector.tabulate(rows)(t => 0.8 + amplitudes.indices.map(i => x(t * amplitudes.length + i) * amplitudes(i)).sum + 0.03 * math.sin(0.37 * t))
    val source = FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("reference-face-crossing"),
      Matrix.dense(rows, 1, column), SampleSpaces(Vector(1, 1, 1))), frame).dataset
    val declaration = ProfileHrfPlan.fromTrialEvents(source, drive, baseline, arConfig, basis, 0.4).toOption.get
    val control = parallelChecked(ProfileHrfFit.prepare(declaration, DataSelection.All, parallelWhitening,
      outputPolicy()).flatMap(_.trialOutputs))
    def one(view: PreparedProfileTrialOutputs): ProfileTrialOutputVoxel =
      val blocks = ArrayBuffer.empty[ProfileTrialOutputBlock]
      parallelChecked(view.run(parallelReader(source), OutputRequest.TrialAmplitudes(NormalizationRule.Density),
        ProfileTrialReadoutMode.CorrectedReference, outputSink(blocks)))
      blocks.head.results.head
    val first = one(control)
    val mean = emitted(first)._2.actualCoordinates
    val step0 = grid.step(0)
    val step1 = grid.step(1)
    val prior = ShapePrior(mean, Vector(100 / (step0 * step0), 90 / (step0 * step1),
      90 / (step0 * step1), 100 / (step1 * step1)))
    val crossedView = parallelChecked(ProfileHrfFit.prepare(declaration, DataSelection.All, parallelWhitening,
      outputPolicy().copy(prior = Some(prior))).flatMap(_.trialOutputs))
    val crossed = one(crossedView)
    val (token, _) = emitted(crossed)
    assertEquals(token.index, oracle(crossed.selection.coordinates))
    assert(token.index != crossed.selection.node, s"starting=${crossed.selection.node}, nearest=${token.index}, theta=${crossed.selection.coordinates}")
    crossed.selection.coordinates.zip(mean).foreach((a, b) => assertEqualsDouble(a, b, 1e-6))
    val foreign = parallelChecked(outputPrepared().trialOutputs)
    val foreignToken = foreign.referenceAt(half).toOption.get
    assertEquals(view.freeze(foreignToken, OutputRequest.TrialAmplitudes(NormalizationRule.Density),
      ProfileTrialReadoutMode.ExactShape, ProfileTrialEvidenceRequest.PreparedBasisResidual), Left(ProfileTrialReadoutError.ForeignAxis))

  test("signed near-cancellation queries retain no full trial vector and conversion audit stays explicit"):
    val view = parallelChecked(outputPrepared().trialOutputs)
    val (_, full) = execute(view, OutputRequest.TrialAmplitudes(NormalizationRule.Density), ProfileTrialReadoutMode.CorrectedReference)
    val values = emitted(full.head)._2.trialAmplitudes.get
    val near = ProfileTrialSignedQuery.make("near", view.axis, Vector(values(1), -values(0), 0, 0, 0, 0), 1e-10).toOption.get
    val fine = ProfileTrialSignedQuery.make("fine", view.axis, Vector(math.Pi, 0, 0, 0, 0, 0), 1e-20).toOption.get
    val (summary, queryResults) = execute(view, OutputRequest.TrialQueries(Vector(near, fine), NormalizationRule.Density), ProfileTrialReadoutMode.CorrectedReference)
    queryResults.foreach { v =>
      val result = emitted(v)._2
      assertEquals(result.trialAmplitudes, None)
      assertEquals(result.work.retainedTrialAmplitudeValues, 0)
      assertEquals(result.work.wrapperCoefficientBufferValues, 7)
      assertEquals(result.work.alwaysRetainedAdjointRowValues, rows)
      assert(!result.queries(1).withinTolerance)
      assert(result.queries(1).conversionError > 0.0)
      assert(result.evidence.preparedBasisNormalResidualNorm.isFinite)
    }
    assertEqualsDouble(emitted(queryResults.head)._2.queries.head.value, 0.0, 1e-10)
    assertEquals(summary.progress.publicReadout.get.storage.emittedTrialAmplitudeValues, 0L)

  test("foreign and duplicate queries, wrong dimensions, rule and certificate refuse before any reader or sink access"):
    val view = parallelChecked(outputPrepared().trialOutputs)
    val foreign = parallelChecked(outputPrepared().trialOutputs)
    var touched = 0
    val poison = new DatasetSeriesReader:
      def dataset: FmriDataset = { touched += 1; fail("descriptor touched") }
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] = { touched += 1; fail("payload touched") }
    val sink = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
      def accept(block: VoxelBlock, value: ProfileTrialOutputBlock) = { touched += 1; fail("sink touched") }
    val q = ProfileTrialSignedQuery.make("foreign", foreign.axis, Vector(1.0, 0, 0, 0, 0, 0), 1e-6).toOption.get
    val local = ProfileTrialSignedQuery.make("repeat", view.axis, q.weights, 1e-6).toOption.get
    val wrong = SignedQuery.make("wrong", Vector(1.0, -1.0), 1e-6).toOption.get
    val cases = Vector(OutputRequest.TrialQueries(Vector(q), NormalizationRule.Density),
      OutputRequest.TrialQueries(Vector(local, local), NormalizationRule.Density),
      OutputRequest.ConditionQueries(Vector(wrong), NormalizationRule.Density),
      OutputRequest.TrialAmplitudes(NormalizationRule.PositiveComponentArea))
    cases.foreach(request => assert(view.run(poison, request, ProfileTrialReadoutMode.ExactShape, sink,
      () => { touched += 1; true }).isLeft))
    assert(view.run(poison, OutputRequest.TrialAmplitudes(NormalizationRule.Density),
      ProfileTrialReadoutMode.ExactShape, sink, evidence = ProfileTrialEvidenceRequest.CertifiedOriginalEquations).isLeft)
    assertEquals(touched, 0)
    assert(fixedPrepared.trialOutputs.isLeft)
    // Raw alpha=0 is dispatched to compact condition without a trial view.
    val compact = plan(0.0, FitConfig())
    assert(!compact.amplitudes.hasTrialDeviations)

  test("missing or stale compiled baseline matrix and rows refuse; an actually empty nuisance axis is valid"):
    def viewFor(b: BaselineModel): Either[ProfileFitError, PreparedProfileTrialOutputs] =
      val p = ProfileHrfPlan.fromTrialEvents(dataset, drive, b, arConfig, basis, 0.4).toOption.get
      ProfileHrfFit.prepare(p, selection, parallelWhitening, outputPolicy()).flatMap(_.trialOutputs)
    assert(viewFor(baseline.copy(compiledSchema = None)).isLeft)
    val changed = scalafim.fmri.hrf.linalg.Mat.unsafe(rows, baseline.designMatrix.cols, baseline.designMatrix.data.map(_ * 2.0))
    assert(viewFor(baseline.copy(designMatrix = changed)).isLeft)
    val otherFrame = scalafim.fmri.hrf.design.SamplingFrame(blockLens = Seq(60, 60), tr = Seq(2.0, 2.0))
    val otherSchema = BaselineModel.build(otherFrame, BaselineBasis.Constant, intercept = Intercept.Global).compiledSchema
    assert(viewFor(baseline.copy(compiledSchema = otherSchema)).isLeft)
    val empty = BaselineModel.build(frame, BaselineBasis.Dct(DctCutoffPeriod.unsafeSeconds(1000)), intercept = Intercept.None)
    val view = parallelChecked(viewFor(empty.copy(compiledSchema = None)))
    assertEquals(view.axis.nuisanceColumnIds, Vector.empty)
    assert(viewFor(empty.copy(compiledSchema = baseline.compiledSchema)).isLeft)
    val (_, emptyResults) = execute(view, OutputRequest.TrialAmplitudes(NormalizationRule.Unnormalised), ProfileTrialReadoutMode.ExactShape)
    emptyResults.foreach(v => v.output match
      case ProfileTrialOutputOutcome.Emitted(_, result) => assertEquals(result.nuisanceCoefficients, Vector.empty)
      case _ => ())

  test("decode refusals skip conditional work; cancellation immediately before delivery retains completed work"):
    val p = parallelChecked(ProfileHrfFit.prepare(plan(0.4), selection, parallelWhitening,
      outputPolicy().copy(budget = DecodeBudget(maxNewtonSteps = 0, maxJets = 1, maxExactEvaluations = 0))))
    val view = parallelChecked(p.trialOutputs)
    val (summary, values) = execute(view, OutputRequest.TrialAmplitudes(NormalizationRule.Density), ProfileTrialReadoutMode.ExactShape)
    assert(values.forall(_.output.isInstanceOf[ProfileTrialOutputOutcome.DecodeRefused]))
    assertEquals(summary.progress.publicReadout.get.attempts, 0L)
    assertEquals(summary.progress.publicReadout.get.decodeRefusals, 3L)
    assertEquals(summary.progress.publicReadout.get.numerical, ProfileHrfFit.sumTrialWork(Vector.empty))
    val accepted = parallelChecked(outputPrepared().trialOutputs)
    val outputs = ArrayBuffer.empty[ProfileTrialOutputBlock]
    var calls = 0
    accepted.run(reader, OutputRequest.TrialAmplitudes(NormalizationRule.Density), ProfileTrialReadoutMode.CorrectedReference,
      outputSink(outputs), () => { calls += 1; calls >= 4 }) match
      case Left(ProfileFitError.Cancelled(progress)) =>
        assertEquals(progress.attemptedVoxels, 1)
        assertEquals(progress.publicExecution.get.mode, ProfileTrialReadoutMode.CorrectedReference)
        assertEquals(progress.publicExecution.get.outputKind, ProfileTrialOutputKind.TrialAmplitudes)
        assertEquals(progress.deliveredVoxels, 0)
        assertEquals(progress.publicReadout.get.successes, 1L)
        assertEquals(progress.publicReadout.get.storage.emittedTrialAmplitudeValues, 0L)
      case other => fail(s"expected pre-delivery cancellation, got $other")
    assertEquals(outputs.length, 0)

  test("failed conditional operation keeps nested numerical error and actual failed work"):
    var poison: () => Unit = () => ()
    val original = GaussianFamily.Default
    val family = new ParametricHrfFamily:
      def name = original.name
      def kind = original.kind
      def chart = original.chart
      def horizon = original.horizon
      def supports(rule: NormalizationRule) = original.supports(rule)
      def libraryNormalization = original.libraryNormalization
      def evalInto(lags: Array[Double], point: ShapePoint, out: Array[Double]) = original.evalInto(lags, point, out)
      def jetInto(lags: Array[Double], point: ShapePoint, out: Array[Double]) = original.jetInto(lags, point, out)
      def scaleJetInto(rule: NormalizationRule, point: ShapePoint, out: Array[Double]): Unit =
        original.scaleJetInto(rule, point, out)
        poison()
      def summaries(point: ShapePoint) = original.summaries(point)
      def descriptor(point: ShapePoint) = original.descriptor(point)
      def toHrf(point: ShapePoint) = original.toHrf(point)
    val poisonedBasis = HrfKernelBasis.compile(basis.spec.copy(family = family)).fold(error => fail(error.message), identity)
    val p = ProfileHrfPlan.fromTrialEvents(dataset, drive, baseline, arConfig, poisonedBasis, 0.4).toOption.get
    val prepared = parallelChecked(ProfileHrfFit.prepare(p, selection, parallelWhitening, outputPolicy()))
    val view = parallelChecked(prepared.trialOutputs)
    val gram = view.axis.preparation.gramBlocksData
    val retained = gram.clone()
    val values = ArrayBuffer.empty[ProfileTrialOutputBlock]
    poison = () => java.util.Arrays.fill(gram, Double.NaN)
    try
      view.run(reader, OutputRequest.TrialAmplitudes(NormalizationRule.Unnormalised),
        ProfileTrialReadoutMode.CorrectedReference, outputSink(values)) match
        case Left(ProfileFitError.TrialReadoutFailure(ProfileTrialReadoutError.Conditional(detail), progress)) =>
          assert(detail.contains("directional RHS"), detail)
          assertEquals(progress.publicExecution.get.mode, ProfileTrialReadoutMode.CorrectedReference)
          assertEquals(progress.publicExecution.get.normalization, NormalizationRule.Unnormalised)
          val work = progress.publicReadout.get
          assertEquals(work.attempts, 1L)
          assertEquals(work.failures, 1L)
          assertEquals(work.successes, 0L)
          assertEquals(work.completedNormalActionApplications, 0L)
          assertEquals(work.numerical.attempted.conditionalReadoutAttempts, 1L)
          assertEquals(work.numerical.attempted.conditionalReadoutFailures, 1L)
          assertEquals(work.numerical.attempted.conditionalCorrectionFailures, 0L)
          assertEquals(work.numerical.attempted.conditionalInverseAttempts, 1L)
          assertEquals(progress.trial.get.attempted.conditionalReadoutFailures, 1L)
          assertEquals(progress.deliveredBlocks, 0)
          assertEquals(values.length, 0)
        case other => fail(s"expected typed numerical failure, got $other")
    finally Array.copy(retained, 0, gram, 0, gram.length)

  private def mlView(block: Int = 1): PreparedProfileTrialOutputs =
    parallelChecked(ProfileHrfFit.prepare(withMl(plan(0.4)), selection,
      parallelWhitening, outputPolicy(block)).flatMap(_.trialOutputs))

  test("ML same-worker public exact output matches dense returned-shape trial, nuisance and normal residual"):
    val view = mlView(2)
    val (summary, voxels) = execute(view, OutputRequest.TrialAmplitudes(NormalizationRule.Unnormalised), ProfileTrialReadoutMode.ExactShape)
    assertEquals(voxels.map(_.voxelId), Vector(3, 0, 2))
    assert(voxels.exists(v => (0 until view.bank.grid.count).forall(i => view.bank.grid.point(i).coordinates != v.selection.coordinates)))
    voxels.foreach: voxel =>
      val (reference, result) = emitted(voxel)
      val (energy, means, trials, nuisance) = denseAllAt(voxel.voxelId, voxel.selection.coordinates)
      result.trialAmplitudes.get.zip(trials).foreach((a, b) => assertEqualsDouble(a, b, 5e-7))
      result.conditionMeans.zip(means).foreach((a, b) => assertEqualsDouble(a, b, 5e-7))
      result.nuisanceCoefficients.zip(nuisance).foreach((a, b) => assertEqualsDouble(a, b, 5e-7))
      assert(result.nuisanceCoefficients.exists(math.abs(_) > 0.01))
      assertEqualsDouble(result.evidence.preparedBasisNormalResidualNorm,
        denseResidualAt(voxel.voxelId, voxel.selection.coordinates, result.trialAmplitudes.get, result.nuisanceCoefficients), 1e-9)
      assert(math.abs(voxel.selection.energy - energy) > 1e-5, "selection is J while readout energy is E")
      assertEquals(result.actualCoordinates, voxel.selection.coordinates)
      assertEquals(reference.actualCoordinates, voxel.selection.coordinates)
      assertEquals(result.work.referenceInverseAttempts, 0L)
      assertEquals(result.work.exactReadoutFactorAttempts, 1L)
      assertEquals(result.work.normalActionApplications, 1L)
      assertEquals(result.work.responseRowsEncoded, 0)
      assertEquals(result.work.whiteningForwardRowsVisited, 0)
      assertEquals(result.work.alwaysRetainedAdjointRowValues, 0)
    val public = summary.progress.publicReadout.get
    assertEquals(public.attempts, 3L)
    assertEquals(public.successes, 3L)
    assertEquals(public.residualMeasurementAttempts, 3L)
    assertEquals(public.residualMeasurementFailures, 0L)
    assertEquals(public.residualMeasurementNormalActions, 3L)
    assertEquals(public.numerical.attempted.readoutAttempts, 3L)
    assertEquals(public.numerical.attempted.conditionalReadoutAttempts, 0L)
    assertEquals(public.completedResponseRowsEncoded, 0L)
    assertEquals(summary.progress.trial.get.voxels, 3L)
    assertEquals(summary.progress.trial.get.trialBasisScores, 3L * view.axis.preparation.trials * view.axis.preparation.basisRank)
    assertEquals(summary.progress.trial.get.attempted.readoutAttempts, 3L)
    assertEquals(summary.progress.trial.get.attempted.exactReadoutFactorAttempts, 3L)
    assertEquals(summary.progress.trial.get.attempted.referenceAttempts, summary.progress.trialMl.get.referenceAttempts + 3L)
    assert(public.storage.workerMlMeasurementScratchValues.forall(_ > 10 * 7))
    assertEquals(public.storage.completedMemoizedRawTrialValues, 18L)
    assert(summary.provenance.contains("criterion=native-trial-ml|conditional-returned-shape=true|raw-energy=E|optimised=J"))
    assertEquals(summary.setup.mlSetup, view.prepared.setup.mlSetup)

  test("ML signed queries and condition queries share normalization, native nuisance and explicit Float32 audits"):
    val view = mlView()
    val (_, native) = execute(view, OutputRequest.TrialAmplitudes(NormalizationRule.Unnormalised), ProfileTrialReadoutMode.ExactShape)
    val (_, scaled) = execute(view, OutputRequest.TrialAmplitudes(NormalizationRule.Density), ProfileTrialReadoutMode.ExactShape)
    val first = emitted(scaled.head)._2.trialAmplitudes.get
    val weights = Vector(first(1), -first(0), 0, 0, 0, 0)
    val queries = Vector(ProfileTrialSignedQuery.make("cancel", view.axis, weights, 1e-10).toOption.get,
      ProfileTrialSignedQuery.make("negative", view.axis, weights.map(-_), 1e-10).toOption.get,
      ProfileTrialSignedQuery.make("round", view.axis, Vector(math.Pi, 0, 0, 0, 0, 0), 1e-20).toOption.get)
    val (summary, values) = execute(view, OutputRequest.TrialQueries(queries, NormalizationRule.Density), ProfileTrialReadoutMode.ExactShape)
    val conditionQuery = SignedQuery.make("conditions", Vector(0.3, -0.7, 1.1), 1e-6).toOption.get
    val (_, conditions) = execute(view, OutputRequest.ConditionQueries(Vector(conditionQuery), NormalizationRule.Density), ProfileTrialReadoutMode.ExactShape)
    native.zip(scaled).zip(values.zip(conditions)).foreach: (pair, queryPair) =>
      val (raw, normalized) = pair
      val (qv, cv) = queryPair
      val r = emitted(raw)._2
      val n = emitted(normalized)._2
      val q = emitted(qv)._2
      assertEquals(raw.selection, normalized.selection)
      r.trialAmplitudes.get.zip(n.trialAmplitudes.get).foreach((a, b) => assertEqualsDouble(a / n.normalizationScale, b, 1e-10))
      assertEquals(r.nuisanceCoefficients, n.nuisanceCoefficients)
      assertEqualsDouble(n.equivalentNormalizedLambda, n.nativeLambda * n.normalizationScale * n.normalizationScale, 1e-12)
      val (_, _, denseTrials, _) = denseAllAt(qv.voxelId, qv.selection.coordinates)
      queries.zip(q.queries).foreach: (query, actual) =>
        assertEqualsDouble(actual.value, query.weights.zip(denseTrials).map(_ * _).sum / n.normalizationScale, 5e-7)
      assertEqualsDouble(q.queries(0).value, -q.queries(1).value, 1e-12)
      assertEqualsDouble(emitted(cv)._2.queries.head.value, conditionQuery.weights.zip(n.conditionMeans).map(_ * _).sum, 1e-10)
      assertEquals(q.trialAmplitudes, None)
      assertEquals(q.work.retainedTrialAmplitudeValues, 0)
      assertEquals(q.work.wrapperCoefficientBufferValues, 7)
      assert(!q.queries(2).withinTolerance)
    assertEqualsDouble(emitted(values.head)._2.queries.head.value, 0.0, 1e-10)
    assertEquals(summary.progress.publicReadout.get.storage.emittedTrialAmplitudeValues, 0L)
    assertEquals(summary.progress.publicReadout.get.storage.completedMemoizedRawTrialValues, 18L)

  test("ML unsupported intents refuse before readers or callbacks and decode refusals perform zero readout"):
    val view = mlView()
    var touched = 0
    val poison = new DatasetSeriesReader:
      def dataset: FmriDataset = { touched += 1; fail("descriptor touched") }
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] = { touched += 1; fail("response touched") }
    val sink = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
      def accept(block: VoxelBlock, value: ProfileTrialOutputBlock) = { touched += 1; fail("sink touched") }
    val request = OutputRequest.TrialAmplitudes(NormalizationRule.Density)
    assertEquals(view.run(poison, request, ProfileTrialReadoutMode.CorrectedReference, sink), Left(ProfileFitError.TrialOutputMlIntent(ProfileTrialReadoutMode.CorrectedReference, ProfileTrialEvidenceRequest.PreparedBasisResidual)))
    assert(view.executionDeclaration(request, ProfileTrialReadoutMode.ExactShape, ProfileTrialEvidenceRequest.CertifiedOriginalEquations).isLeft)
    assert(view.run(poison, request, ProfileTrialReadoutMode.ExactShape, sink,
      () => { touched += 1; true }, ProfileTrialEvidenceRequest.CertifiedOriginalEquations).isLeft)
    val foreign = parallelChecked(outputPrepared().trialOutputs)
    assert(PreparedProfileTrialOutputs.make(view.prepared, foreign.axis.preparation, foreign.bank).isLeft)
    assertEquals(touched, 0)
    val tight = parallelChecked(ProfileHrfFit.prepare(withMl(plan(0.4)), selection, parallelWhitening,
      outputPolicy().copy(budget = DecodeBudget(maxNewtonSteps = 0, maxJets = 1, maxExactEvaluations = 0))).flatMap(_.trialOutputs))
    val (summary, refused) = execute(tight, request, ProfileTrialReadoutMode.ExactShape)
    assert(refused.forall(_.output.isInstanceOf[ProfileTrialOutputOutcome.DecodeRefused]))
    assertEquals(summary.progress.publicReadout.get.attempts, 0L)
    assertEquals(summary.progress.publicReadout.get.residualMeasurementAttempts, 0L)
    assertEquals(summary.progress.trial.get.attempted.readoutAttempts, 0L)
    assertEquals(summary.progress.publicReadout.get.storage.workerCoefficientHighWaterValues, Vector(7))
    assert(summary.progress.publicReadout.get.storage.workerMlMeasurementScratchValues.forall(_ > 10 * 7))

  test("ML sink refusal and pre-delivery cancellation preserve same-worker readout attempts"):
    val view = mlView()
    val sink = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
      def accept(block: VoxelBlock, value: ProfileTrialOutputBlock) = Left("refused receipt")
    val failed = view.run(reader, OutputRequest.TrialAmplitudes(NormalizationRule.Density), ProfileTrialReadoutMode.ExactShape, sink)
    val progress = failed match
      case Left(ProfileFitError.SinkRefused(_, progress)) => progress
      case other => fail(s"expected sink refusal, got $other")
    assertEquals(progress.deliveredVoxels, 0)
    assertEquals(progress.publicReadout.get.successes, 1L)
    assertEquals(progress.trial.get.attempted.readoutAttempts, 1L)
    var calls = 0
    val values = ArrayBuffer.empty[ProfileTrialOutputBlock]
    val cancelled = view.run(reader, OutputRequest.TrialAmplitudes(NormalizationRule.Density),
      ProfileTrialReadoutMode.ExactShape, outputSink(values), () => { calls += 1; calls >= 4 })
    val stopped = cancelled match
      case Left(ProfileFitError.Cancelled(progress)) => progress
      case other => fail(s"expected cancellation, got $other")
    assertEquals(stopped.deliveredVoxels, 0)
    assertEquals(stopped.publicReadout.get.successes, 1L)
    assertEquals(stopped.trial.get.attempted.readoutAttempts, 1L)
    assertEquals(values.size, 0)

  test("ML failed residual measurement emits nothing and retains the exact readout and failed normal action"):
    val original = GaussianFamily.Default
    var arm: () => Unit = () => ()
    var jetCall: () => Unit = () => ()
    val family = new ParametricHrfFamily:
      def name = original.name
      def kind = original.kind
      def chart = original.chart
      def horizon = original.horizon
      def supports(rule: NormalizationRule) = original.supports(rule)
      def libraryNormalization = original.libraryNormalization
      def evalInto(lags: Array[Double], point: ShapePoint, out: Array[Double]) = original.evalInto(lags, point, out)
      def jetInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
        original.jetInto(lags, point, out)
        jetCall()
      def scaleJetInto(rule: NormalizationRule, point: ShapePoint, out: Array[Double]): Unit =
        original.scaleJetInto(rule, point, out)
        arm()
      def summaries(point: ShapePoint) = original.summaries(point)
      def descriptor(point: ShapePoint) = original.descriptor(point)
      def toHrf(point: ShapePoint) = original.toHrf(point)
    val poisonedBasis = HrfKernelBasis.compile(basis.spec.copy(family = family)).fold(error => fail(error.message), identity)
    val p = ProfileHrfPlan.fromTrialEvents(dataset, drive, baseline, arConfig, poisonedBasis, 0.4).toOption.get
    val view = parallelChecked(ProfileHrfFit.prepare(withMl(p), selection, parallelWhitening, outputPolicy()).flatMap(_.trialOutputs))
    val gram = view.axis.preparation.gramBlocksData
    val saved = gram.clone()
    arm = () =>
      var calls = 0
      jetCall = () =>
        calls += 1
        // One jet contracts the exact factor. The second contracts the
        // measurement operator after that factor/solve has already succeeded.
        if calls == 2 then java.util.Arrays.fill(gram, Double.NaN)
    val values = ArrayBuffer.empty[ProfileTrialOutputBlock]
    try
      view.run(reader, OutputRequest.TrialAmplitudes(NormalizationRule.Unnormalised),
        ProfileTrialReadoutMode.ExactShape, outputSink(values)) match
        case Left(ProfileFitError.TrialReadoutFailure(ProfileTrialReadoutError.Conditional(detail), progress)) =>
          assert(detail.contains("normal residual"), detail)
          assertEquals(progress.deliveredVoxels, 0)
          val work = progress.publicReadout.get
          assertEquals(work.attempts, 1L)
          assertEquals(work.successes, 0L)
          assertEquals(work.failures, 1L)
          assertEquals(work.residualMeasurementAttempts, 1L)
          assertEquals(work.residualMeasurementFailures, 1L)
          assertEquals(work.residualMeasurementNormalActions, 1L)
          assertEquals(work.completedNormalActionApplications, 0L)
          assertEquals(work.numerical.attempted.readoutAttempts, 1L)
          assertEquals(work.numerical.attempted.readoutFailures, 0L)
          assertEquals(work.numerical.attempted.exactReadoutFactorAttempts, 1L)
          assertEquals(progress.trial.get.attempted.readoutAttempts, 1L)
          assertEquals(work.storage.completedRetainedTrialAmplitudeValues, 0L)
        case other => fail(s"expected failed measurement, got $other")
    finally Array.copy(saved, 0, gram, 0, gram.length)
    assertEquals(values.size, 0)
