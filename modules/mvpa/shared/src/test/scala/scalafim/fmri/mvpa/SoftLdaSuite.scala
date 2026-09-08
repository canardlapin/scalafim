package scalafim.fmri.mvpa

import gale.linalg.{DMat, DVec, DoubleLinearOperator, Matrix, MutableDVec}
import multivar.core.ValueId
import multivar.family.canonical.{LdaObjective, TraceRidgeFraction, TrialNuisanceDesign, WithinScatterPolicy}
import resample4s.core.*
import scalafim.fmri.mvpa.predictive.*

final class SoftLdaSuite extends munit.FunSuite:
  private val samples =
    AxisRef
      .create(
        AxisId.unsafe("soft-lda-kernel-samples"),
        AxisPurpose.Samples,
        Vector.tabulate(12)(index => SampleId.unsafe(s"sample-$index")),
        CoordinateBasis.unsafe("trial-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("soft-lda-kernel", "v1")
      )
      .toOption
      .get
  private val features =
    AxisRef
      .create(
        AxisId.unsafe("soft-lda-kernel-features"),
        AxisPurpose.NeuralFeatures,
        Vector.tabulate(3)(index => FeatureId.unsafe(s"voxel-$index")),
        CoordinateBasis.unsafe("voxel-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("soft-lda-kernel", "v1")
      )
      .toOption
      .get
  private val classes =
    ClassAxis
      .create(
        AxisId.unsafe("soft-lda-kernel-classes"),
        Vector(ClassId.unsafe("a"), ClassId.unsafe("b"), ClassId.unsafe("c")),
        CoordinateProvenance.unsafe("soft-lda-kernel", "v1")
      )
      .toOption
      .get
  private val patterns = fromRows(
    Seq(
      Seq(2.2, 0.1, 0.2),
      Seq(0.0, 2.0, -0.2),
      Seq(-2.0, -1.8, 0.1),
      Seq(2.0, -0.1, 0.0),
      Seq(0.2, 2.2, 0.1),
      Seq(-2.2, -2.0, -0.1),
      Seq(2.3, 0.2, -0.1),
      Seq(-0.1, 1.9, 0.2),
      Seq(-1.9, -2.1, 0.0),
      Seq(1.9, 0.0, 0.1),
      Seq(0.1, 2.1, -0.1),
      Seq(-2.1, -1.9, 0.2)
    )
  )
  private val labels = Vector.tabulate(samples.size)(index => classes.keys(index % 3))
  private val categorical =
    CategoricalTarget(samples, classes, Column(samples, labels).toOption.get).toOption.get
  private val hard = ClassMembershipTarget.hard(categorical).toOption.get
  private val trainSelection =
    Selection
      .from(
        IArray.unsafeFromArray((3 until 12).toArray),
        IndexSpace.of(samples.size).toOption.get
      )
      .toOption
      .get
  private val testSelection =
    Selection
      .from(
        IArray.unsafeFromArray((0 until 3).toArray),
        IndexSpace.of(samples.size).toOption.get
      )
      .toOption
      .get
  private val trainLeg = ReindexingLeg.selection(samples, trainSelection).toOption.get
  private val testLeg = ReindexingLeg.selection(samples, testSelection).toOption.get
  private val config = SoftLdaKernelConfig(
    WithinScatterPolicy.FixedTraceScaledRidge(TraceRidgeFraction.unsafe(0.05))
  )

  test("identified operator and dense evidence agree without ordinal adaptation"):
    val dense = EvidenceTable
      .dense(samples, features, patterns, ValueId.unsafe("soft-lda-dense"))
      .toOption
      .get
    val operator = EvidenceTable
      .operator(
        samples,
        features,
        MatrixFree(patterns),
        ValueId.unsafe("soft-lda-operator")
      )
      .toOption
      .get
    val denseResult = fitFold(dense, hard, config)
    val operatorResult = fitFold(operator, hard, config)

    assertMatrixClose(
      operatorResult.decisionWeights,
      denseResult.decisionWeights,
      1e-9
    )
    assertEquals(
      operatorResult.receipt.trainingAxis,
      trainLeg.child.identity
    )
    assertEquals(operatorResult.receipt.featureAxis, features.identity)
    assertEquals(operatorResult.receipt.classAxis, classes.identity)
    assert(operatorResult.receipt.operatorApplications > 0L)
    assertEquals(
      operatorResult.receipt.fit.programFit.program.objective.label,
      "generalized-rayleigh"
    )

  test("soft simplex memberships remain soft in the fitted kernel"):
    val softened = Matrix.tabulate(samples.size, classes.size): (row, klass) =>
      val primary = row % classes.size
      val secondary = (primary + 1 + (row / classes.size) % 2) % classes.size
      if klass == primary then 0.65
      else if klass == secondary then 0.30
      else 0.05
    val soft = ClassMembershipTarget
      .simplex(samples, classes, softened)
      .toOption
      .get
    val evidence = EvidenceTable
      .dense(samples, features, patterns, ValueId.unsafe("soft-lda-soft-target"))
      .toOption
      .get
    val hardFit = fitFold(evidence, hard, config)
    val softFit = fitFold(evidence, soft, config)

    assert(matrixDistance(hardFit.decisionWeights, softFit.decisionWeights) > 1e-5)

  test("fold-local nuisance remains distinct in fit evidence"):
    val nuisanceValues = Matrix.tabulate(samples.size, 1)((row, _) => (row / 3).toDouble - 1.5)
    val selectedNuisance = Matrix.tabulate(trainLeg.size, 1)((row, column) => nuisanceValues(row + 3, column))
    val nuisance = TrialNuisanceDesign.from(selectedNuisance).toOption.get
    val evidence = EvidenceTable
      .dense(samples, features, patterns, ValueId.unsafe("soft-lda-nuisance"))
      .toOption
      .get
    val adjusted = fitFold(
      evidence,
      hard,
      config.copy(objective = LdaObjective.TraceRatio),
      Some(nuisance)
    )

    assertEquals(adjusted.receipt.trialNuisanceColumns, 1)
    assertEquals(adjusted.receipt.fit.diagnostics.objective, LdaObjective.TraceRatio)
    assertEquals(
      adjusted.receipt.fit.programFit.program.objective.label,
      "trace-ratio"
    )

  private def fitFold(
      evidence: EvidenceTable[samples.Id, features.Id, SampleId, FeatureId],
      target: ClassMembershipTarget[samples.Id],
      configuration: SoftLdaKernelConfig,
      nuisance: Option[TrialNuisanceDesign] = None
  ): SoftLdaKernelResult =
    val train = evidence.restrictRows(trainLeg).toOption.get
    val test = evidence.restrictRows(testLeg).toOption.get
    val membership = target.select(trainLeg).toOption.get
    SoftLdaKernel
      .fit(
        train,
        test,
        classes,
        membership,
        nuisance,
        configuration,
        "held-out-run-0"
      )
      .toOption
      .get

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, column) => rows(row)(column))

  private def matrixDistance(left: DMat, right: DMat): Double =
    var squared = 0.0
    var row = 0
    while row < left.rows do
      var column = 0
      while column < left.cols do
        val difference = left(row, column) - right(row, column)
        squared += difference * difference
        column += 1
      row += 1
    math.sqrt(squared)

  private def assertMatrixClose(
      actual: DMat,
      expected: DMat,
      tolerance: Double
  ): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertEqualsDouble(actual(row, column), expected(row, column), tolerance)
        column += 1
      row += 1

  private final class MatrixFree(matrix: DMat) extends DoubleLinearOperator:
    override def rows: Int = matrix.rows
    override def cols: Int = matrix.cols

    override def applyTo(input: DVec, output: MutableDVec): Unit =
      var row = 0
      while row < rows do
        var total = 0.0
        var column = 0
        while column < cols do
          total += matrix(row, column) * input(column)
          column += 1
        output(row) = total
        row += 1

    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      var column = 0
      while column < cols do
        var total = 0.0
        var row = 0
        while row < rows do
          total += matrix(row, column) * input(row)
          row += 1
        output(column) = total
        column += 1
