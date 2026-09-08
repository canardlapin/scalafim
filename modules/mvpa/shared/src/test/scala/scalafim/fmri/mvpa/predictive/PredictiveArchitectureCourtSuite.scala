package scalafim.fmri.mvpa.predictive

import gale.linalg.DMat
import multivar.core.ValueId
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.predictive.PredictiveAnalysis.given

final class PredictiveArchitectureCourtSuite extends munit.FunSuite:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private val cat = ClassId.unsafe("cat")
  private val dog = ClassId.unsafe("dog")
  private val samples =
    AxisRef
      .create(
        AxisId.unsafe("predictive-court-samples"),
        AxisPurpose.Samples,
        Vector.tabulate(8)(index => SampleId.unsafe(s"sample-$index")),
        CoordinateBasis.unsafe("run-trial-table"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("predictive-court", "v1")
      )
      .toOption
      .get
  private val features =
    AxisRef
      .create(
        AxisId.unsafe("predictive-court-features"),
        AxisPurpose.NeuralFeatures,
        Vector(FeatureId.unsafe("voxel-x"), FeatureId.unsafe("voxel-y")),
        CoordinateBasis.unsafe("voxel-table"),
        Some(AxisUnits.unsafe("percent-signal-change")),
        AxisScale.nominal,
        CoordinateProvenance.unsafe("predictive-court", "v1")
      )
      .toOption
      .get
  private val classes = classAxis(Vector(cat, dog), "predictive-court-classes")
  private val labels = Vector(cat, dog, cat, dog, cat, dog, cat, dog)
  private val groups = Vector(0, 0, 1, 1, 2, 2, 3, 3)
  private val patterns = DMat.dense(
    8,
    2,
    Vector(
      -4.0, -1.0, 4.0, 1.0, -3.0, -0.5, 3.0, 0.5, -2.0, -1.5, 2.0, 1.5, -5.0, -0.25, 5.0, 0.25
    )
  )
  private val configuration =
    ClassificationConfiguration(
      StandardizedNearestCentroid(
        StandardizationSpecification.CenterScaleRejectConstant
      )
    ).toOption.get
  private val global =
    Measurement
      .hardSelection(
        features,
        MeasurementId.unsafe("predictive-court-global"),
        Injection
          .from(indices(0, 1), IndexSpace.of(features.size).toOption.get)
          .toOption
          .get
      )
      .toOption
      .get
  private val frame = MeasurementFrame(features)(
    Vector(MeasurementEntry(global, NoRendition))
  ).toOption.get

  private def indices(values: Int*): IArray[Int] =
    IArray.unsafeFromArray(values.toArray)

  private def classAxis(order: Vector[ClassId], id: String): AxisRef[ClassId] =
    ClassAxis
      .create(
        AxisId.unsafe(id),
        order,
        CoordinateProvenance.unsafe("predictive-court", "v1")
      )
      .toOption
      .get

  private def evidence[S <: multivar.core.SemanticSpace](
      rows: AxisRef.Aux[SampleId, S],
      values: DMat,
      id: String
  ) =
    EvidenceTable
      .dense(rows, features, values, ValueId.unsafe(id))
      .toOption
      .get

  private def target[S <: multivar.core.SemanticSpace](
      rows: AxisRef.Aux[SampleId, S],
      classOrder: AxisRef[ClassId],
      values: Vector[ClassId]
  ) =
    CategoricalTarget(
      rows,
      classOrder,
      Column(rows, values).toOption.get
    ).toOption.get

  private def validation[S <: multivar.core.SemanticSpace](
      rows: AxisRef.Aux[SampleId, S],
      groupValues: Vector[Int],
      seed: Long
  ): ExactClassificationValidation[S] =
    val ordinal = LeaveOneGroupOut(
      Labels.dense(IArray.unsafeFromArray(groupValues.toArray), rows.size).toOption.get
    )
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, seed)
    val compiled = ordinal
      .compile(IndexSpace.of(rows.size).toOption.get, authority.seed)
      .toOption
      .get
    val schedule = BoundSchedule(
      compiled,
      rows,
      AxisPopulationFingerprint.fromAxis(rows).toOption.get,
      ScheduleLabels.fromDesign(rows, ordinal).toOption.get,
      authority
    ).toOption.get
    ValidationDesign(
      schedule,
      ScientificAxisName.unsafe("samples"),
      GeneralizationAxis(ScientificAxisName.unsafe("samples"), rows.identity)
    ).toOption.get

  private def strategy(
      representation: ExecutionRepresentation = ExecutionRepresentation.Dense,
      precision: NumericPrecision = NumericPrecision.Binary64,
      solver: SolverChoice = SolverChoice.NotApplicable,
      materialization: MaterializationPolicy = MaterializationPolicy.Allow(MaterializationBudget.unsafe(16L))
  ) =
    ExecutionStrategy(
      BackendId.unsafe("alder-court"),
      representation,
      precision,
      solver,
      Vector.empty,
      Scheduling.serial,
      materialization,
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    ).toOption.get

  private def execute[S <: multivar.core.SemanticSpace](
      sourceEvidence: EvidenceTable[S, features.Id, SampleId, FeatureId],
      sourceTarget: CategoricalTarget[S],
      sourceDesign: ExactClassificationValidation[S],
      execution: ExecutionStrategy = strategy()
  ): Either[
    MvpaRunError[ClassificationBindRejection],
    AnalysisResult[
      OutOfFoldClassification[
        S,
        StandardizedNearestCentroid,
        StandardizedNearestCentroidFit,
        CategoricalClassifierError,
        AlderClassPrediction
      ],
      ClassificationBindRejection,
      ClassificationCompileError[CategoricalClassifierError],
      NoRendition.type
    ]
  ] =
    CategoricalObservationSource(sourceEvidence, sourceTarget) match
      case Left(error)   => fail(error.message)
      case Right(source) =>
        Mvpa.run(source)(
          sourceDesign,
          frame,
          source.classify(configuration),
          execution
        )

  private def successful[S <: multivar.core.SemanticSpace](
      result: Either[
        MvpaRunError[ClassificationBindRejection],
        AnalysisResult[
          OutOfFoldClassification[
            S,
            StandardizedNearestCentroid,
            StandardizedNearestCentroidFit,
            CategoricalClassifierError,
            AlderClassPrediction
          ],
          ClassificationBindRejection,
          ClassificationCompileError[CategoricalClassifierError],
          NoRendition.type
        ]
      ]
  ): (
      AnalysisResult[
        OutOfFoldClassification[
          S,
          StandardizedNearestCentroid,
          StandardizedNearestCentroidFit,
          CategoricalClassifierError,
          AlderClassPrediction
        ],
        ClassificationBindRejection,
        ClassificationCompileError[CategoricalClassifierError],
        NoRendition.type
      ],
      OutOfFoldClassification[
        S,
        StandardizedNearestCentroid,
        StandardizedNearestCentroidFit,
        CategoricalClassifierError,
        AlderClassPrediction
      ]
  ) =
    result match
      case Left(error)     => fail(error.message)
      case Right(analysis) =>
        analysis.values.head.outcome match
          case MeasurementOutcome.Success(value, _)   => analysis -> value
          case MeasurementOutcome.Rejected(reason, _) =>
            fail(s"unexpected rejection: ${reason.message}")
          case MeasurementOutcome.Failed(error, _) =>
            fail(error.message(configuration.compiler.failureMessage))

  private def score[S <: multivar.core.SemanticSpace](
      result: OutOfFoldClassification[
        S,
        StandardizedNearestCentroid,
        StandardizedNearestCentroidFit,
        CategoricalClassifierError,
        AlderClassPrediction
      ],
      sample: Int,
      label: ClassId
  ): Double =
    val classPosition = result.predictions.classes.positionOf(label).get
    result.predictions.scores.get
      .scoreAt(sample, classPosition)
      .toOption
      .get
      .value

  test("held-out data and target perturbations match one train-only dense oracle"):
    val baselineTarget = target(samples, classes, labels)
    val baselineDesign = validation(samples, groups, 211L)
    val (_, baseline) = successful(
      execute(
        evidence(samples, patterns, "court-baseline"),
        baselineTarget,
        baselineDesign
      )
    )

    val perturbedPatterns = DMat.tabulate(patterns.rows, patterns.cols): (row, column) =>
      if row == 0 then Vector(100.0, -50.0)(column)
      else if row == 1 then Vector(-100.0, 50.0)(column)
      else patterns(row, column)
    val (_, perturbedData) = successful(
      execute(
        evidence(samples, perturbedPatterns, "court-heldout-data-perturbed"),
        baselineTarget,
        baselineDesign
      )
    )

    val train = Vector(2, 3, 4, 5, 6, 7)
    Vector(0, 1).foreach: heldOut =>
      val baselineOracle = classifyOracle(
        patterns,
        labels,
        classes.keys,
        train,
        heldOut
      )
      val perturbedOracle = classifyOracle(
        perturbedPatterns,
        labels,
        classes.keys,
        train,
        heldOut
      )
      classes.keys.zipWithIndex.foreach: (label, classPosition) =>
        assertEqualsDouble(
          score(baseline, heldOut, label),
          baselineOracle(classPosition),
          1e-12
        )
        assertEqualsDouble(
          score(perturbedData, heldOut, label),
          perturbedOracle(classPosition),
          1e-12
        )

    val swappedLabels = labels.updated(0, dog).updated(1, cat)
    val (_, perturbedTarget) = successful(
      execute(
        evidence(samples, patterns, "court-target-perturbed"),
        target(samples, classes, swappedLabels),
        baselineDesign
      )
    )
    Vector(0, 1).foreach: heldOut =>
      classes.keys.foreach: label =>
        assertEqualsDouble(
          score(perturbedTarget, heldOut, label),
          score(baseline, heldOut, label),
          1e-12
        )
      assertEquals(
        perturbedTarget.predictions.predicted.values(heldOut),
        baseline.predictions.predicted.values(heldOut)
      )
    assert(perturbedTarget.accuracy.value < baseline.accuracy.value)

  test("OOF sample, run, class, and seed order laws retain scientific identity"):
    val baselineTarget = target(samples, classes, labels)
    val baselineDesign = validation(samples, groups, 223L)
    val (baselineAnalysis, baseline) = successful(
      execute(
        evidence(samples, patterns, "court-order-baseline"),
        baselineTarget,
        baselineDesign
      )
    )

    val permutationValues = Vector(4, 5, 6, 7, 0, 1, 2, 3)
    val reindexing = ReindexingLeg
      .permutation(
        samples,
        Permutation
          .from(IArray.unsafeFromArray(permutationValues.toArray))
          .toOption
          .get
      )
      .toOption
      .get
    val reorderedEvidence = evidence(
      samples,
      patterns,
      "court-order-reindex-source"
    ).restrictRows(reindexing).toOption.get
    val reorderedLabels = Column(samples, labels).toOption.get
      .reindex(reindexing)
      .toOption
      .get
    val reorderedTarget = CategoricalTarget(
      reindexing.child,
      classes,
      reorderedLabels
    ).toOption.get
    val reorderedGroups = permutationValues.map(groups)
    val (_, reordered) = successful(
      execute(
        reorderedEvidence,
        reorderedTarget,
        validation(reindexing.child, reorderedGroups, 223L)
      )
    )
    assertEquals(reordered.predictions.samples.keys, reindexing.child.keys)
    val baselineBySample = baseline.predictions.samples.keys
      .zip(baseline.predictions.predicted.toVector)
      .toMap
    assertEquals(
      reordered.predictions.samples.keys
        .zip(reordered.predictions.predicted.toVector)
        .map((sample, prediction) => sample -> prediction)
        .toMap,
      baselineBySample
    )

    val reversedClasses = classAxis(
      Vector(dog, cat),
      "predictive-court-reversed-classes"
    )
    val (_, reversed) = successful(
      execute(
        evidence(samples, patterns, "court-class-order"),
        target(samples, reversedClasses, labels),
        baselineDesign
      )
    )
    assertEquals(reversed.predictions.classes.keys, Vector(dog, cat))
    assertEquals(
      reversed.predictions.predicted.toVector,
      baseline.predictions.predicted.toVector
    )
    var row = 0
    while row < samples.size do
      Vector(cat, dog).foreach: label =>
        assertEqualsDouble(
          score(reversed, row, label),
          score(baseline, row, label),
          1e-12
        )
      row += 1

    val (differentSeedAnalysis, differentSeed) = successful(
      execute(
        evidence(samples, patterns, "court-order-baseline"),
        baselineTarget,
        validation(samples, groups, 224L)
      )
    )
    assertEquals(
      differentSeed.predictions.predicted.toVector,
      baseline.predictions.predicted.toVector
    )
    assertNotEquals(
      differentSeedAnalysis.plan.fingerprint,
      baselineAnalysis.plan.fingerprint
    )
    assertNotEquals(
      differentSeed.foldReceipts.map(_.seed.value),
      baseline.foldReceipts.map(_.seed.value)
    )

  test("adversarial training-class coverage fails at binding"):
    val classGrouped = Vector(0, 1, 0, 1, 0, 1, 0, 1)
    val rejected = execute(
      evidence(samples, patterns, "court-missing-training-class"),
      target(samples, classes, labels),
      validation(samples, classGrouped, 227L)
    )
    val failedClosed = rejected.left.exists:
      case MvpaRunError.Binding(
            BindError.EstimandRejected(
              _,
              ClassificationBindRejection.Target(
                CategoricalError.MissingTrainingClasses(_, missing)
              ),
              _
            )
          ) =>
        missing.nonEmpty
      case _ => false
    assert(failedClosed)

  test("strategy admission and execution work fail closed and reconcile"):
    val sourceEvidence = evidence(samples, patterns, "court-work")
    val sourceTarget = target(samples, classes, labels)
    val sourceDesign = validation(samples, groups, 229L)
    val invalid = Vector(
      strategy(representation = ExecutionRepresentation.Operator) ->
        ((error: ExecutionPlanError) =>
          error match
            case ExecutionPlanError.UnsupportedRepresentation(
                  ExecutionRepresentation.Operator,
                  Vector(ExecutionRepresentation.Dense)
                ) =>
              true
            case _ => false
        ),
      strategy(precision = NumericPrecision.Binary32) ->
        ((error: ExecutionPlanError) =>
          error match
            case ExecutionPlanError.UnsupportedPrecision(
                  NumericPrecision.Binary32,
                  Vector(NumericPrecision.Binary64)
                ) =>
              true
            case _ => false
        ),
      strategy(
        solver = SolverChoice.Selected(
          SolverIdentity(SolverId.unsafe("irrelevant-solver")).toOption.get
        )
      ) ->
        ((error: ExecutionPlanError) =>
          error match
            case ExecutionPlanError.UnsupportedSolver(
                  SolverChoice.Selected(identity)
                ) =>
              identity.id.value == "irrelevant-solver"
            case _ => false
        ),
      strategy(materialization = MaterializationPolicy.Reject) ->
        ((error: ExecutionPlanError) =>
          error match
            case ExecutionPlanError.MaterializationRequired(reason) =>
              reason.contains("Alder learners")
            case _ => false
        )
    )
    invalid.foreach: (invalidStrategy, recognizes) =>
      val rejected = execute(
        sourceEvidence,
        sourceTarget,
        sourceDesign,
        invalidStrategy
      )
      val admittedFailure = rejected.left.exists:
        case MvpaRunError.Planning(error) => recognizes(error)
        case _                            => false
      assert(admittedFailure)

    val (analysis, result) = successful(
      execute(sourceEvidence, sourceTarget, sourceDesign)
    )
    val measurementReceipt = analysis.receipt.measurements.head
    assertEquals(analysis.receipt.work.planned, 1)
    assertEquals(analysis.receipt.work.attempted, 1)
    assertEquals(analysis.receipt.work.succeeded, 1)
    assertEquals(analysis.receipt.work.failed, 0)
    assertEquals(
      analysis.receipt.work.operatorApplications,
      measurementReceipt.operatorApplications
    )
    assertEquals(
      analysis.receipt.work.materializedCells,
      measurementReceipt.materializedCells
    )
    assertEquals(measurementReceipt.operatorApplications, 1L)
    assertEquals(measurementReceipt.materializedCells, 16L)
    assertEquals(result.receipts.length, samples.size)
    assertEquals(
      result.foldReceipts.flatMap(_.assessmentSamples),
      samples.keys
    )

  private def classifyOracle(
      values: DMat,
      truth: Vector[ClassId],
      classOrder: Vector[ClassId],
      trainingRows: Vector[Int],
      assessmentRow: Int
  ): Vector[Double] =
    val means = Array.fill(values.cols)(0.0)
    trainingRows.foreach: row =>
      var column = 0
      while column < values.cols do
        means(column) += values(row, column)
        column += 1
    var column = 0
    while column < values.cols do
      means(column) /= trainingRows.length.toDouble
      column += 1
    val scales = Array.fill(values.cols)(0.0)
    trainingRows.foreach: row =>
      column = 0
      while column < values.cols do
        val centered = values(row, column) - means(column)
        scales(column) += centered * centered
        column += 1
    column = 0
    while column < values.cols do
      scales(column) = math.sqrt(scales(column) / trainingRows.length.toDouble)
      column += 1

    classOrder.map: label =>
      val members = trainingRows.filter(row => truth(row) == label)
      var squaredDistance = 0.0
      column = 0
      while column < values.cols do
        var centroid = 0.0
        members.foreach: row =>
          centroid += (values(row, column) - means(column)) / scales(column)
        centroid /= members.length.toDouble
        val assessed =
          (values(assessmentRow, column) - means(column)) / scales(column)
        val delta = assessed - centroid
        squaredDistance += delta * delta
        column += 1
      -squaredDistance
