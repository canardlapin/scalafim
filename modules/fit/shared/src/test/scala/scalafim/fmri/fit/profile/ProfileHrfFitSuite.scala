package scalafim.fmri.fit.profile

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.{DataSelection, DatasetError, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, InMemoryDatasetBackend, SynchronousFmriDataset, TimepointSelection, VoxelSelection}
import scalafim.fmri.ar.{ArmaCoefficients, InitialConditionPolicy, TimeSegment, WhiteningPlan, WhiteningTransform}
import scalafim.fmri.design.{ConditionId, FactorId, FactorLevelSet, TrialId}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, DctCutoffPeriod, Intercept}
import scalafim.fmri.design.event.{Event, EventModel, EventTerm, EventSchedule}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, ExpandedTrialDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.fit.CanonicalTemporalWhitening
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, ShapePoint}
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig, FitPlan, FmriModel, ProfileCriterion, ProfileHrfPlan, ProfileTrialDrive, VolumeWeighting}
import scalafim.image.SampleSpaces

class ProfileHrfFitSuite extends munit.FunSuite:
  protected val rows = 120
  protected val frame = SamplingFrame(blockLens = Seq(60, 60), tr = Seq(1.0, 1.0))
  private val point = ShapePoint.unsafe(Vector(5.3, math.log(1.55)))
  protected lazy val basis = HrfKernelBasis.compile(KernelBasisSpec(
    GaussianFamily.Default, PositiveSeconds.unsafe(Seconds(0.2)), Vector(26, 21), tolerance = 1e-4,
    maxRank = 40)).fold(error => fail(error.message), identity)
  private lazy val fixedBasis = HrfKernelBasis.compile(KernelBasisSpec(
    GaussianFamily.Default, PositiveSeconds.unsafe(Seconds(0.2)), Vector(8, 7), tolerance = 0.5,
    maxRank = 4, heldOutPoints = 1)).fold(error => fail(error.message), identity)
  private val onsets = Vector(3.0, 10.0, 18.0, 5.0, 14.0, 31.0).map(Seconds(_))
  private val blocks = Vector(0, 0, 0, 1, 1, 1)
  private val membership = TrialMembership.make(Vector(0, 1, 1, 1, 2, 2), 3)
    .fold(error => fail(error.message), identity)
  private val labels = Vector("zeta", "alpha", "mu")
  private val means = Vector(-1.2, 0.7, 1.5)
  private val deviations = Vector(0.0, -0.3, 0.1, 0.2, -0.4, 0.4)
  private lazy val schedule = EventSchedule.fromParts(onsets, Vector.fill(6)(Seconds(0.0)), blocks)
    .fold(error => fail(error.message), identity)
  protected lazy val drive = ProfileTrialDrive.make(schedule, membership,
    labels.map(ConditionId.unsafe), Vector.tabulate(6)(i => TrialId.unsafe(s"trial${i + 1}")),
    membership.conditionOfTrial.map(i => ConditionId.unsafe(labels(i))))
    .fold(error => fail(error.message), identity)
  private lazy val expanded = ExpandedTrialDesign.lower(onsets, blocks, Vector.fill(6)(Seconds(0.0)),
    membership, frame, basis, Seconds(0.2)).fold(error => fail(error.message), identity)
  protected lazy val baseline = BaselineModel.build(frame, BaselineBasis.Constant, intercept = Intercept.Global)
  private lazy val nuisance =
    val mat = baseline.designMatrix
    DMat.tabulate(rows, mat.cols)((t, j) => mat(t, j))
  protected val arPlan = WhiteningPlan.global(ArmaCoefficients.ar(0.37),
    Vector(TimeSegment(0, 60, 0), TimeSegment(60, 120, 1)), exactFirstAr1 = true)
  protected val arConfig = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), global = true, rho = Some(0.37)))

  protected def designAt(coords: Vector[Double]): Array[Double] =
    val coefficients = new Array[Double](expanded.rank)
    basis.coefficientsInto(ShapePoint.unsafe(coords), new Array[Double](basis.fineCount), coefficients)
    val x = new Array[Double](rows * expanded.trials)
    val source = expanded.term.data.data
    var t = 0
    while t < rows do
      var trial = 0
      while trial < expanded.trials do
        var value = 0.0
        var p = 0
        while p < expanded.rank do
          value += source(t * expanded.columns + p * expanded.trials + trial) * coefficients(p)
          p += 1
        x(t * expanded.trials + trial) = value
        trial += 1
      t += 1
    x

  protected lazy val responseColumns: Vector[Array[Double]] =
    val x = designAt(point.coordinates)
    Vector.tabulate(4) { voxel =>
      Array.tabulate(rows) { t =>
        var value = (0.8 - 0.15 * voxel) * nuisance(t, 0)
        var trial = 0
        while trial < expanded.trials do
          val amplitude = means(membership.conditionOfTrial(trial)) + deviations(trial) + 0.11 * voxel
          value += x(t * expanded.trials + trial) * amplitude
          trial += 1
        value + 0.03 * math.sin(0.37 * t + voxel) - 0.02 * math.cos(0.11 * t)
      }
    }
  protected lazy val dataset: FmriDataset =
    val data = Matrix.dense(rows, 4, (0 until rows).flatMap(t => responseColumns.map(_(t))))
    FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("profile-executor"), data,
      SampleSpaces(Vector(4, 1, 1))), frame).dataset
  protected def plan(alpha: Double, config: FitConfig = arConfig,
                   criterion: ProfileCriterion = ProfileCriterion.PenalizedProfile(1.0)): ProfileHrfPlan =
    ProfileHrfPlan.fromTrialEvents(dataset, drive, baseline, config, basis, alpha, criterion)
      .fold(error => fail(error.message), identity)
  protected def reader: SynchronousFmriDataset =
    SynchronousFmriDataset.readerFor(dataset).fold(error => fail(error.message), identity)
  protected def selection = DataSelection(voxels = VoxelSelection.indices(3, 0, 2))
  protected def policy(blockSize: Int = 2, admission: Option[ObservedFamilyAdmission] = None) =
    ProfileDecodePolicy(Vector(3, 3), DecodeBudget(maxNewtonSteps = 3, maxJets = 4,
      maxExactEvaluations = 8), prior = None, execution = ExecutionBudget(blockSize, 1),
      observedAdmission = admission)
  private def sink(results: scala.collection.mutable.ArrayBuffer[ProfileFitBlock]) =
    new BlockSink[ProfileFitBlock, ProfileFitReceipt]:
      def accept(block: VoxelBlock, payload: ProfileFitBlock): Either[String, ProfileFitReceipt] =
        results += payload
        Right(ProfileFitReceipt(payload.ordinal, payload.voxelIds))
  private def checked[A](value: Either[ProfileFitError, A]): A =
    value.fold(error => fail(error.message), identity)

  protected def parallelFixture(count: Int): (FmriDataset, ProfileHrfPlan) =
    val values = (0 until rows).flatMap(t => (0 until count).map(i => responseColumns(i % responseColumns.length)(t)))
    val expandedDataset = FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("profile-parallel"),
      Matrix.dense(rows, count, values), SampleSpaces(Vector(count, 1, 1))), frame).dataset
    val expandedPlan = ProfileHrfPlan.fromTrialEvents(expandedDataset, drive, baseline, arConfig, basis, 0.4)
      .fold(error => fail(error.message), identity)
    (expandedDataset, expandedPlan)

  protected def parallelPolicy(blockSize: Int, workers: Int): ProfileDecodePolicy =
    policy(blockSize).copy(execution = ExecutionBudget(blockSize, workers))

  protected def parallelWhitening: CanonicalTemporalWhitening = CanonicalTemporalWhitening.Shared(arPlan)

  protected def parallelReader(dataset: FmriDataset): SynchronousFmriDataset =
    SynchronousFmriDataset.readerFor(dataset).fold(error => fail(error.message), identity)

  protected def parallelChecked[A](value: Either[ProfileFitError, A]): A = checked(value)

  protected def parallelSink(results: scala.collection.mutable.ArrayBuffer[ProfileFitBlock]) = sink(results)

  protected def whiten(values: Array[Double], columns: Int): Array[Double] =
    val matrix = DMat.tabulate(rows, columns)((t, j) => values(t * columns + j))
    val result = WhiteningTransform.matrix(arPlan, matrix).fold(error => fail(error.message), identity)
    val out = new Array[Double](rows * columns)
    result.copyRowMajorTo(out)
    out

  /** Independent dense augmented normal equations at the returned shape. */
  protected def denseAt(voxel: Int, coords: Vector[Double]): (Double, Vector[Double], Vector[Double]) =
    val (energy, conditions, trials, _) = denseAllAt(voxel, coords)
    (energy, conditions, trials)

  protected def denseAllAt(voxel: Int, coords: Vector[Double]): (Double, Vector[Double], Vector[Double], Vector[Double]) =
    val n = expanded.trials
    val c = membership.conditionCount
    val f = nuisance.cols
    val x = whiten(designAt(coords), n)
    val nuisanceRaw = Array.tabulate(rows * f)(i => nuisance(i / f, i % f))
    val nf = whiten(nuisanceRaw, f)
    val y = whiten(responseColumns(voxel), 1)
    val size = n + f + c
    def observed(t: Int, j: Int): Double =
      if j < n then x(t * n + j)
      else if j < n + f then nf(t * f + j - n)
      else 0.0
    val lambda = 2.5
    val normal = DMat.tabulate(size, size) { (i, j) =>
      var value = 0.0
      var t = 0
      while t < rows do
        value += observed(t, i) * observed(t, j)
        t += 1
      if i < n && j < n && i == j then value += lambda
      if i < n && j >= n + f && membership.conditionOfTrial(i) == j - n - f then value -= lambda
      if j < n && i >= n + f && membership.conditionOfTrial(j) == i - n - f then value -= lambda
      if i >= n + f && i == j then value += lambda * membership.trialsOf(i - n - f).length
      value
    }
    val rhs = DMat.tabulate(size, 1) { (i, _) =>
      var value = 0.0
      var t = 0
      while t < rows do
        value += observed(t, i) * y(t)
        t += 1
      value
    }
    val solved = normal.cholesky.fold(throw _, identity).solve(rhs).fold(throw _, identity)
    var energy = 0.0
    var t = 0
    while t < rows do
      var fitted = 0.0
      var j = 0
      while j < n + f do
        fitted += observed(t, j) * solved(j, 0)
        j += 1
      val residual = y(t) - fitted
      energy += residual * residual
      t += 1
    var trial = 0
    while trial < n do
      val delta = solved(trial, 0) - solved(n + f + membership.conditionOfTrial(trial), 0)
      energy += lambda * delta * delta
      trial += 1
    (energy, Vector.tabulate(c)(i => solved(n + f + i, 0)), Vector.tabulate(n)(i => solved(i, 0)),
      Vector.tabulate(f)(i => solved(n + i, 0)))

  test("off-node trial fit streams selected voxel IDs and agrees with dense augmented equations at returned shape"):
    val prepared = checked(ProfileHrfFit.prepare(plan(0.4), selection,
      CanonicalTemporalWhitening.Shared(arPlan), policy()))
    val payloads = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
    val completed = checked(prepared.run(reader, sink(payloads)))
    assertEquals(completed.provenance, prepared.provenance)
    assert(completed.provenance.endsWith("|exact-readout=true"))
    assertEquals(completed.publicExecution, None)
    assertEquals(completed.progress.deliveredBlocks, 2)
    assertEquals(completed.progress.deliveredVoxels, 3)
    assertEquals(completed.progress.workersUsed, 1)
    assert(completed.setup.trial.nonEmpty)
    assert(completed.setup.expandedTrialLoweringDoubles.exists(_ > 0L))
    assertEquals(payloads.map(_.voxelIds).toVector, Vector(Vector(3, 0), Vector(2)))
    assertEquals(payloads.map(_.results.map(_.voxelId)).toVector, payloads.map(_.voxelIds).toVector)
    val fits = payloads.flatMap(_.results)
    val grid = NodeGrid(basis.family.chart, Vector(3, 3))
    assert(fits.exists(fit => (0 until grid.count).forall(node => grid.point(node).coordinates != fit.coordinates)))
    fits.foreach { fit =>
      val (energy, condition, trial) = denseAt(fit.voxelId, fit.coordinates)
      assertEqualsDouble(fit.penalizedEnergy, energy, 5e-7)
      condition.indices.foreach(i => assertEqualsDouble(fit.conditionMeans(i), condition(i), 5e-7))
      fit.readout match
        case ProfileAmplitudeReadout.AdaptiveTrial(backend) =>
          assertEquals(backend.factorMode, TrialReadoutFactorMode.ExactShape(fit.coordinates))
          trial.indices.foreach(i => assertEqualsDouble(backend.trialAmplitudes(i), trial(i), 5e-7))
        case _ => fail("expected adaptive trial readout")
    }
    assert(completed.progress.trial.exists(_.exactReadoutFactors == 3))
    val smaller = checked(ProfileHrfFit.prepare(plan(0.4), selection,
      CanonicalTemporalWhitening.Shared(arPlan), policy(1)))
    val again = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
    checked(smaller.run(reader, sink(again)))
    fits.zip(again.flatMap(_.results)).foreach { (a, b) =>
      assertEquals(a.voxelId, b.voxelId)
      assertEqualsDouble(a.penalizedEnergy, b.penalizedEnergy, 1e-8)
    }
    val reused = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
    checked(prepared.run(reader, sink(reused)))
    fits.zip(reused.flatMap(_.results)).foreach((a, b) => assertEqualsDouble(a.penalizedEnergy, b.penalizedEnergy, 1e-8))

  test("work aggregation retains conditional attempts and failures from distinct workers"):
    val prepared = TrialBandedPreparation.prepare(expanded, Some(arPlan), Some(nuisance), 2.5)
      .fold(error => fail(error.message), identity)
    val bank = prepared.objective(NodeGrid(basis.family.chart, Vector(3, 3)))
      .fold(error => fail(error.message), identity)
    val setup = bank.setupReceipt
    val whitened = prepared.whitenResponses(1, responseColumns(0))
      .fold(error => fail(error.message), identity)
    val encoded = prepared.encodeWhitened(whitened).fold(error => fail(error.message), identity)
    val node = bank.grid.indexOf(Array(1, 1))
    val coordinates = bank.grid.point(node).coordinates
    val first = bank.newWorker()
    val second = bank.newWorker()
    first.pointAt(encoded)
    second.pointAt(encoded)
    first.readout(TrialReadoutFactorMode.ExactShape(coordinates))
      .fold(error => fail(error.message), identity)
    assert(second.readout(TrialReadoutFactorMode.PreparedNode(-1)).isLeft)
    val successful = new TrialConditionalSolve(first)
    (0 until 2).foreach { _ =>
      successful.solve(encoded, node, coordinates).fold(error => fail(error.message), identity)
    }
    val refused = new TrialConditionalSolve(second)
    assertEquals(refused.solve(encoded, -1, coordinates),
      Left(TrialConditionalError.InvalidReferenceNode(-1, bank.grid.count)))
    assertEquals(second.solveConditionalReference(node, Array.emptyDoubleArray,
      new Array[Double](prepared.trials + prepared.nuisanceColumns)),
      Left(TrialBandedError.ReadoutRhs(prepared.trials + prepared.nuisanceColumns, 0)))

    // Reference factors are already built. At zero displacement, the directional
    // Gram action is zero, so a poisoned value Gram reaches the correction RHS.
    val originalGram = prepared.gramBlocksData.clone()
    try
      java.util.Arrays.fill(prepared.gramBlocksData, Double.NaN)
      assertEquals(refused.solve(encoded, node, coordinates),
        Left(TrialConditionalError.NonFiniteAssembly("correction RHS")))
    finally
      Array.copy(originalGram, 0, prepared.gramBlocksData, 0, originalGram.length)

    val parts = Vector(first.work.snapshot, second.work.snapshot)
    val total = ProfileHrfFit.sumTrialWork(parts)
    val attempted = total.attempted
    assertEquals(attempted.conditionalReadoutAttempts, 4L)
    assertEquals(attempted.conditionalReadoutFailures, 2L)
    assertEquals(attempted.conditionalInverseAttempts, 9L)
    assertEquals(attempted.conditionalInverseFailures, 1L)
    assertEquals(attempted.conditionalCorrectionAttempts, 3L)
    assertEquals(attempted.conditionalCorrectionFailures, 1L)
    assertEquals(attempted.readoutAttempts, 2L)
    assertEquals(attempted.readoutFailures, 1L)
    def fields(value: TrialBandedAttemptedWorkSnapshot): Vector[Long] =
      value.productIterator.map(_.asInstanceOf[Long]).toVector
    val expected = fields(parts(0).attempted).zip(fields(parts(1).attempted)).map((a, b) => a + b)
    assertEquals(expected.length, 22)
    assertEquals(fields(attempted), expected)
    assertEquals(ProfileHrfFit.sumTrialWork(parts.reverse), total)
    assertEquals(ProfileHrfFit.sumTrialWork(Vector.empty).attempted,
      TrialBandedAttemptedWorkSnapshot(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    assertEquals(bank.work.snapshot.voxels, 0L)
    assertEquals(bank.setupReceipt, setup)
    assert(total.attempted.factorAttempts < setup.work.factorAttempts)

  test("alpha zero lowers a declared-order condition term directly and has no trial work"):
    val levels = FactorLevelSet.unsafe(FactorId.unsafe("condition"), labels)
    val event = Event.factorWithLevels(drive.conditionForTrial.map(_.value), "condition", levels)
      .fold(error => fail(error.message), identity)
    val term = EventTerm.fromSchedule(Vector(event), schedule, Some("condition"))
      .fold(error => fail(error.message), identity)
    val condition = ExpandedConditionDesign.lower(term, frame, basis, Seconds(0.2), dropEmpty = false)
      .fold(error => fail(error.message), identity)
    assertEquals(condition.conditions.length, labels.length)
    val admission = ObservedFamilyCertification.admitForCompact(condition, term, frame, Seconds(0.2),
      None, Some(nuisance), Vector(point), ObservedFamilyRequirements(0.5, 1e10, 1e-9))
      .fold(error => fail(error.message), identity)
    val prepared = checked(ProfileHrfFit.prepare(plan(0.0, FitConfig()), selection,
      CanonicalTemporalWhitening.Iid, policy(admission = Some(admission))))
    val payloads = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
    val completed = checked(prepared.run(reader, sink(payloads)))
    assertEquals(completed.progress.trial, None)
    assert(prepared.trialOutputs.isLeft)
    assert(completed.provenance.contains("direct-condition-compact"))
    val aggregate = expanded.aggregateConditions
    val expected = condition.term.data
    assertEquals(aggregate.rows, expected.rows)
    assertEquals(aggregate.cols, expected.cols)
    var i = 0
    while i < aggregate.data.length do
      assertEqualsDouble(aggregate.data(i), expected.data(i), 1e-10)
      i += 1
    payloads.flatMap(_.results).foreach(fit => assert(fit.readout.isInstanceOf[ProfileAmplitudeReadout.ConditionMeans]))

  test("noiseless zero-deviation control recovers the declared continuous shape"):
    val x = designAt(point.coordinates)
    val values = Vector.tabulate(rows) { t =>
      var response = 0.8 * nuisance(t, 0)
      var trial = 0
      while trial < expanded.trials do
        response += x(t * expanded.trials + trial) * means(membership.conditionOfTrial(trial))
        trial += 1
      response
    }
    val cleanDataset = FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("profile-clean"),
      Matrix.dense(rows, 1, values), SampleSpaces(Vector(1, 1, 1))), frame).dataset
    val clean = ProfileHrfPlan.fromTrialEvents(cleanDataset, drive, baseline, FitConfig(), basis, 0.4)
      .fold(error => fail(error.message), identity)
    val prepared = checked(ProfileHrfFit.prepare(clean, DataSelection.All, CanonicalTemporalWhitening.Iid, policy(1)))
    val payloads = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
    checked(prepared.run(SynchronousFmriDataset.readerFor(cleanDataset).toOption.get, sink(payloads)))
    val fit = payloads.head.results.head
    assertEqualsDouble(fit.coordinates(0), point.coordinates(0), 0.25)
    assertEqualsDouble(fit.coordinates(1), point.coordinates(1), 0.2)
    assertEqualsDouble(fit.penalizedEnergy, 0.0, 1e-5)

  protected def fixedPrepared: PreparedProfileHrf =
    val levels = FactorLevelSet.unsafe(FactorId.unsafe("condition"), labels)
    val event = Event.factorWithLevels(drive.conditionForTrial.map(_.value), "condition", levels)
      .fold(error => fail(error.message), identity)
    val term = EventTerm.fromSchedule(Vector(event), schedule, Some("condition"))
      .fold(error => fail(error.message), identity)
    val expandedCondition = ExpandedConditionDesign.lower(term, frame, fixedBasis, Seconds(0.2), dropEmpty = false)
      .fold(error => fail(error.message), identity)
    val convolved = term.convolve(fixedBasis.kernel, frame, precision = Seconds(0.2), dropEmpty = false)
    val events = EventModel(Vector("task" -> convolved), frame, convolved.data, convolved.columnNames,
      Vector(0 -> convolved.data.cols), Map("task" -> (0 until convolved.data.cols).toVector))
    val fixed = FitPlan(FmriModel(events, baseline, dataset))
    val structure = ConditionProfileFit.structureFor(fixed, convolved).fold(error => fail(error.message), identity)
    val admission = ObservedFamilyCertification.admitForCondition(fixed, structure, expandedCondition,
      term, frame, Seconds(0.2), None, Some(nuisance), Vector(point),
      ObservedFamilyRequirements(0.5, 1e10, 1e-9)).fold(error => fail(error.message), identity)
    val profile = ProfileHrfPlan.fromFixed(fixed, convolved, fixedBasis).fold(error => fail(error.message), identity)
    checked(ProfileHrfFit.prepare(profile, DataSelection.All,
      CanonicalTemporalWhitening.Iid, policy(admission = Some(admission))))

  test("fixed condition plan uses the existing admitted OLS retention"):
    val prepared = fixedPrepared
    val payloads = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
    val completed = checked(prepared.run(reader, sink(payloads)))
    assertEquals(completed.setup.route, "fixed-condition-ols")
    assertEquals(completed.progress.deliveredVoxels, 4)
    assertEquals(completed.progress.trial, None)
    assert(payloads.flatMap(_.results).forall(_.penalizedEnergy.isFinite))

  test("unsupported intent, temporal selection and whitening are rejected before a read"):
    assert(ProfileHrfFit.prepare(plan(0.4, criterion = ProfileCriterion.TrialRandomEffectsML(1.0)),
      selection, CanonicalTemporalWhitening.Shared(arPlan), policy()).left.toOption
      .exists(_.isInstanceOf[ProfileFitError.Unsupported]))
    assert(ProfileHrfFit.prepare(plan(0.4, FitConfig(volumeWeighting = VolumeWeighting.Fixed(Vector.fill(rows)(1.0)))),
      selection, CanonicalTemporalWhitening.Iid, policy()).isLeft)
    assert(ProfileHrfFit.prepare(plan(0.4), selection, CanonicalTemporalWhitening.Iid, policy()).isLeft)
    assert(ProfileHrfFit.prepare(plan(0.4, FitConfig(autocorrelation = ArOptions(
      structure = ArStructure.Ar(1), rho = Some(0.37)))), selection,
      CanonicalTemporalWhitening.Shared(arPlan), policy()).isLeft)
    assert(ProfileHrfFit.prepare(plan(0.4), DataSelection(time = TimepointSelection.window(1, rows - 1).toOption.get),
      CanonicalTemporalWhitening.Shared(arPlan), policy()).isLeft)
    val upperBound = checked(ProfileHrfFit.prepare(plan(0.4), selection, CanonicalTemporalWhitening.Shared(arPlan),
      policy().copy(execution = ExecutionBudget(2, 2))))
    val upperPayloads = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
    assertEquals(checked(upperBound.run(reader, sink(upperPayloads))).progress.workersUsed, 1)

  test("shared AR preparation rejects an MA component and a different first-row policy"):
    val options = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), global = true,
      rho = Some(0.37), exactFirst = false))
    val segments = Vector(TimeSegment(0, 60, 0), TimeSegment(60, 120, 1))
    val arma = WhiteningPlan.global(ArmaCoefficients.arma(Seq(0.37), Seq(0.2)), segments,
      exactFirstAr1 = false)
    val wrongInitial = WhiteningPlan.globalWithInitialCondition(ArmaCoefficients.ar(0.37), segments,
      InitialConditionPolicy.PrecomputedScale(0.5)).fold(error => fail(error.message), identity)
    assert(ProfileHrfFit.prepare(plan(0.4, options), selection,
      CanonicalTemporalWhitening.Shared(arma), policy()).isLeft)
    assert(ProfileHrfFit.prepare(plan(0.4, options), selection,
      CanonicalTemporalWhitening.Shared(wrongInitial), policy()).isLeft)

  test("zero-column baseline admits the trial route"):
    val empty = BaselineModel.build(frame, BaselineBasis.Dct(DctCutoffPeriod.unsafeSeconds(1000.0)),
      intercept = Intercept.None)
    assertEquals(empty.designMatrix.cols, 0)
    val zeroPlan = ProfileHrfPlan.fromTrialEvents(dataset, drive, empty, arConfig, basis, 0.4)
      .fold(error => fail(error.message), identity)
    val prepared = checked(ProfileHrfFit.prepare(zeroPlan, selection,
      CanonicalTemporalWhitening.Shared(arPlan), policy()))
    val payloads = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
    val summary = checked(prepared.run(reader, sink(payloads)))
    assertEquals(summary.progress.deliveredVoxels, 3)
    assert(payloads.flatMap(_.results).forall(_.penalizedEnergy.isFinite))

  test("shape prior requires symmetric positive semidefinite precision"):
    val nonsymmetric = ShapePrior(Vector(5.0, 0.0), Vector(1.0, 0.2, 0.0, 1.0))
    val indefinite = ShapePrior(Vector(5.0, 0.0), Vector(1.0, 0.0, 0.0, -0.1))
    val tinyNegative = ShapePrior(Vector(5.0, 0.0), Vector(-1e-13, 0.0, 0.0, -1e-13))
    val zero = ShapePrior(Vector(5.0, 0.0), Vector(0.0, 0.0, 0.0, 0.0))
    val semidefinite = ShapePrior(Vector(5.0, 0.0), Vector(1.0, 0.0, 0.0, 0.0))
    assert(ProfileHrfFit.prepare(plan(0.4), selection, CanonicalTemporalWhitening.Shared(arPlan),
      policy().copy(prior = Some(nonsymmetric))).isLeft)
    assert(ProfileHrfFit.prepare(plan(0.4), selection, CanonicalTemporalWhitening.Shared(arPlan),
      policy().copy(prior = Some(indefinite))).isLeft)
    assertEquals(ProfileHrfFit.prepare(plan(0.4), selection, CanonicalTemporalWhitening.Shared(arPlan),
      policy().copy(prior = Some(tinyNegative))).left.toOption,
      Some(ProfileFitError.Unsupported("shape prior precision must be positive semidefinite")))
    assert(ProfileHrfFit.prepare(plan(0.4), selection, CanonicalTemporalWhitening.Shared(arPlan),
      policy().copy(prior = Some(zero))).isRight)
    assert(ProfileHrfFit.prepare(plan(0.4), selection, CanonicalTemporalWhitening.Shared(arPlan),
      policy().copy(prior = Some(semidefinite))).isRight)

  test("nonunit noise variance does not rescale an energy-unit shape prior"):
    val grid = NodeGrid(basis.family.chart, Vector(3, 3))
    val mean = grid.point(0).coordinates
    val scores = Vector.tabulate(grid.count)(node => denseAt(0, grid.point(node).coordinates)._1)
    val distance = Vector.tabulate(grid.count) { node =>
      val coords = grid.point(node).coordinates
      coords.indices.map(i => math.pow(coords(i) - mean(i), 2)).sum
    }
    def choice(weight: Double): Int =
      scores.indices.minBy(node => scores(node) + weight * distance(node))
    val weight = (-30 to 30).iterator.map(i => math.pow(10.0, i / 5.0))
      .find(value => choice(value) != choice(4.0 * value))
      .getOrElse(fail("fixture does not distinguish a prior scaled by noise variance"))
    val prior = ShapePrior(mean, Vector(weight, 0.0, 0.0, weight))
    val nodeOnly = policy(blockSize = 1).copy(budget = DecodeBudget(coarseStride = 1,
      maxNewtonSteps = 0, maxJets = 1, maxExactEvaluations = 0), prior = Some(prior))
    val prepared = checked(ProfileHrfFit.prepare(plan(0.4, criterion = ProfileCriterion.PenalizedProfile(4.0)),
      DataSelection(voxels = VoxelSelection.indices(0)), CanonicalTemporalWhitening.Shared(arPlan), nodeOnly))
    val payloads = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
    checked(prepared.run(reader, sink(payloads)))
    assertEquals(payloads.head.results.head.coordinates, grid.point(choice(weight)).coordinates)

  test("a decode budget refusal remains a voxel status and is tallied as attempted"):
    val tight = policy(blockSize = 1).copy(budget = DecodeBudget(coarseStride = 1,
      maxNewtonSteps = 0, maxJets = 1, maxExactEvaluations = 0))
    val prepared = checked(ProfileHrfFit.prepare(plan(0.4), selection,
      CanonicalTemporalWhitening.Shared(arPlan), tight))
    val payloads = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
    val summary = checked(prepared.run(reader, sink(payloads)))
    val statuses = payloads.flatMap(_.results.map(_.status)).toVector
    assert(statuses.contains(DecodeStatus.BudgetExceeded))
    assertEquals(summary.progress.attemptedVoxels, statuses.length)
    assertEquals(summary.progress.decodeStatuses.getOrElse(DecodeStatus.BudgetExceeded, 0L),
      statuses.count(_ == DecodeStatus.BudgetExceeded).toLong)

  test("provenance binds the numeric basis and event durations"):
    val original = checked(ProfileHrfFit.prepare(plan(0.4), selection,
      CanonicalTemporalWhitening.Shared(arPlan), policy())).provenance
    val lag = basis.lags(1)
    val changedBasis = try
      basis.lags(1) = lag + 0.001
      checked(ProfileHrfFit.prepare(plan(0.4), selection,
        CanonicalTemporalWhitening.Shared(arPlan), policy())).provenance
    finally basis.lags(1) = lag
    assertNotEquals(original, changedBasis)
    val durationSchedule = EventSchedule.fromParts(onsets, Vector(Seconds(0.1)) ++ Vector.fill(5)(Seconds(0.0)), blocks)
      .fold(error => fail(error.message), identity)
    val durationDrive = ProfileTrialDrive.make(durationSchedule, membership,
      labels.map(ConditionId.unsafe), Vector.tabulate(6)(i => TrialId.unsafe(s"trial${i + 1}")),
      membership.conditionOfTrial.map(i => ConditionId.unsafe(labels(i))))
      .fold(error => fail(error.message), identity)
    val durationPlan = ProfileHrfPlan.fromTrialEvents(dataset, durationDrive, baseline, arConfig, basis, 0.4)
      .fold(error => fail(error.message), identity)
    val changedDuration = checked(ProfileHrfFit.prepare(durationPlan, selection,
      CanonicalTemporalWhitening.Shared(arPlan), policy())).provenance
    assertNotEquals(original, changedDuration)

  test("fixed retention translates reader and cancellation exceptions with partial progress"):
    val prepared = fixedPrepared
    val source = reader
    val payloads = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
    var readerReads = 0
    val throwingReader = new DatasetSeriesReader:
      val dataset: FmriDataset = source.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        readerReads += 1
        if readerReads == 2 then throw new IllegalStateException("reader exploded")
        source.seriesEither(selection)
    prepared.run(throwingReader, sink(payloads)) match
      case Left(ProfileFitError.Dataset(_, progress)) =>
        assertEquals(progress.deliveredBlocks, 1)
        assertEquals(progress.attemptedVoxels, 2)
      case other => fail(s"unexpected $other")
    assertEquals(readerReads, 2)
    assertEquals(payloads.length, 1)
    val callbackPayloads = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
    var callbackReads = 0
    val callbackReader = new DatasetSeriesReader:
      val dataset: FmriDataset = source.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        callbackReads += 1
        source.seriesEither(selection)
    prepared.run(callbackReader, sink(callbackPayloads), () =>
      if callbackPayloads.nonEmpty then throw new IllegalStateException("cancel exploded")
      false) match
      case Left(ProfileFitError.Backend(_, progress)) =>
        assertEquals(progress.deliveredBlocks, 1)
        assertEquals(progress.attemptedVoxels, 2)
      case other => fail(s"unexpected $other")
    assertEquals(callbackReads, 1)

  test("cancellation and sink failures stop subsequent reads and report delivered progress"):
    val prepared = checked(ProfileHrfFit.prepare(plan(0.4), selection,
      CanonicalTemporalWhitening.Shared(arPlan), policy(blockSize = 1)))
    val source = reader
    class Counting extends DatasetSeriesReader:
      val dataset: FmriDataset = source.dataset
      var reads = 0
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        reads += 1
        source.seriesEither(selection)
    val before = new Counting
    val ignored = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
    val cancelledBefore = prepared.run(before, sink(ignored), () => true)
    assert(cancelledBefore.left.toOption.exists(_.isInstanceOf[ProfileFitError.Cancelled]))
    assertEquals(before.reads, 0)
    val preparedTwo = checked(ProfileHrfFit.prepare(plan(0.4), selection,
      CanonicalTemporalWhitening.Shared(arPlan), policy(blockSize = 2)))
    val during = new Counting
    var calls = 0
    val stoppedDuring = preparedTwo.run(during, sink(ignored), () => { calls += 1; calls >= 4 })
    stoppedDuring match
      case Left(ProfileFitError.Cancelled(progress)) =>
        assertEquals(progress.attemptedVoxels, 1)
        assertEquals(progress.deliveredBlocks, 0)
      case other => fail(s"unexpected $other")
    assertEquals(during.reads, 1)
    val beforeDelivery = new Counting
    calls = 0
    val stoppedBeforeDelivery = prepared.run(beforeDelivery, sink(ignored), () => { calls += 1; calls >= 4 })
    stoppedBeforeDelivery match
      case Left(ProfileFitError.Cancelled(progress)) =>
        assertEquals(progress.attemptedVoxels, 1)
        assertEquals(progress.deliveredBlocks, 0)
      case other => fail(s"unexpected $other")
    assertEquals(beforeDelivery.reads, 1)
    val refused = new Counting
    var accepts = 0
    val rejecting = new BlockSink[ProfileFitBlock, ProfileFitReceipt]:
      def accept(block: VoxelBlock, payload: ProfileFitBlock): Either[String, ProfileFitReceipt] =
        accepts += 1
        if accepts == 2 then Left("refused") else Right(ProfileFitReceipt(payload.ordinal, payload.voxelIds))
    val outcome = prepared.run(refused, rejecting)
    outcome match
      case Left(ProfileFitError.SinkRefused(_, progress)) =>
        assertEquals(progress.deliveredBlocks, 1)
        assertEquals(progress.deliveredVoxels, 1)
      case other => fail(s"unexpected $other")
    assertEquals(refused.reads, 2)
    val threw = new Counting
    val throwing = new BlockSink[ProfileFitBlock, ProfileFitReceipt]:
      def accept(block: VoxelBlock, payload: ProfileFitBlock): Either[String, ProfileFitReceipt] =
        throw new IllegalStateException("sink exploded")
    assert(prepared.run(threw, throwing).left.toOption.exists(_.isInstanceOf[ProfileFitError.SinkThrew]))
    assertEquals(threw.reads, 1)
    val wrongAxis = new DatasetSeriesReader:
      val dataset: FmriDataset = source.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        source.seriesEither(selection).flatMap { series =>
          FmriSeries.fromIntIndices(series.data, series.voxelIndices.reverse,
            series.timepoints, series.shape, series.metadata)
        }
    assert(preparedTwo.run(wrongAxis, sink(ignored)).left.toOption.exists(_.isInstanceOf[ProfileFitError.Dataset]))
    val foreign = new DatasetSeriesReader:
      val dataset: FmriDataset = FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("foreign"),
        Matrix.dense(rows, 4, Vector.fill(rows * 4)(0.0)), SampleSpaces(Vector(4, 1, 1))), frame).dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        fail("foreign reader must not read")
    assert(prepared.run(foreign, sink(ignored)).left.toOption.exists(_.isInstanceOf[ProfileFitError.Dataset]))
