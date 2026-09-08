package scalafim.fmri.mvpa

import gale.linalg.{CholeskyOptions, DMat, Matrix}
import multivar.core.*
import multivar.family.canonical.*
import scalafim.fmri.mvpa.predictive.ClassId

enum SoftLdaComponents:
  case Maximum
  case Fixed(count: ComponentCount)

final case class SoftLdaKernelConfig(
    withinPolicy: WithinScatterPolicy,
    objective: LdaObjective = LdaObjective.FisherRayleigh,
    components: SoftLdaComponents = SoftLdaComponents.Maximum
)

enum SoftLdaError:
  case SemanticFailure(fitId: String, detail: String)
  case LdaFailure(fitId: String, cause: MultivarError)
  case NumericalFailure(fitId: String, detail: String)
  case TargetShapeMismatch(
      expectedRows: Int,
      expectedClasses: Int,
      actualRows: Int,
      actualClasses: Int
  )
  case FeatureAxisMismatch(
      expected: AxisFingerprint,
      actual: AxisFingerprint
  )
  case FeatureWitnessMismatch
  case Evidence(fitId: String, error: EvidenceTableError)

  def message: String =
    this match
      case SemanticFailure(fitId, detail) =>
        s"soft-LDA semantic operator failed in fit '$fitId': $detail"
      case LdaFailure(fitId, cause) =>
        s"soft-LDA fit failed in '$fitId': ${cause.message}"
      case NumericalFailure(fitId, detail) =>
        s"soft-LDA prediction failed in '$fitId': $detail"
      case TargetShapeMismatch(
            expectedRows,
            expectedClasses,
            actualRows,
            actualClasses
          ) =>
        s"soft-LDA target expected ${expectedRows}x$expectedClasses values, obtained ${actualRows}x$actualClasses"
      case FeatureAxisMismatch(expected, actual) =>
        s"soft-LDA assessment feature axis ${actual.value} does not match training axis ${expected.value}"
      case FeatureWitnessMismatch =>
        "soft-LDA training and assessment use different nominal feature witnesses"
      case Evidence(fitId, error) =>
        s"soft-LDA evidence failed in fit '$fitId': ${error.message}"

final case class SoftLdaKernelFitReceipt private[mvpa] (
    fitId: String,
    trainingAxis: AxisIdentity,
    assessmentAxis: AxisIdentity,
    featureAxis: AxisIdentity,
    classAxis: AxisIdentity,
    trialNuisanceColumns: Int,
    forwardApplications: Int,
    transposeApplications: Int,
    fit: LdaOperatorFit[?, ?, ?]
):
  require(fitId.trim.nonEmpty, "soft-LDA fit id must be non-empty")
  require(trialNuisanceColumns >= 0, "trial nuisance columns must be non-negative")
  require(forwardApplications >= 0, "forward applications must be non-negative")
  require(transposeApplications >= 0, "transpose applications must be non-negative")

  def operatorApplications: Long =
    forwardApplications.toLong + transposeApplications.toLong

final case class SoftLdaKernelResult(
    decisionWeights: DMat,
    receipt: SoftLdaKernelFitReceipt
)

/** Fold-local identified-evidence SoftLDA kernel. The evidence table already carries its exact Multivar endpoints, so
  * this path performs no ordinal-axis adaptation.
  */
object SoftLdaKernel:
  def fit[
      TrainRows <: SemanticSpace,
      TestRows <: SemanticSpace,
      Features <: SemanticSpace,
      FeatureKey
  ](
      train: EvidenceTable[TrainRows, Features, SampleId, FeatureKey],
      test: EvidenceTable[TestRows, Features, SampleId, FeatureKey],
      classes: AxisRef[ClassId],
      membership: DMat,
      nuisance: Option[TrialNuisanceDesign],
      config: SoftLdaKernelConfig,
      fitId: String
  ): Either[SoftLdaError, SoftLdaKernelResult] =
    if membership.rows != train.rowCount || membership.cols != classes.size then
      Left(
        SoftLdaError.TargetShapeMismatch(
          train.rowCount,
          classes.size,
          membership.rows,
          membership.cols
        )
      )
    else if test.columns.identity != train.columns.identity then
      Left(
        SoftLdaError.FeatureAxisMismatch(
          train.columns.identity.fingerprint,
          test.columns.identity.fingerprint
        )
      )
    else if !(test.columns.evidence eq train.columns.evidence) then Left(SoftLdaError.FeatureWitnessMismatch)
    else
      for
        counted <- countedEvidence(train, test, fitId)
        incidence <- ClassIncidence
          .fromSimplex(membership)
          .left
          .map(SoftLdaError.LdaFailure(fitId, _))
        problem <- LdaProblem
          .fromTable(
            counted._1.rows.evidence,
            counted._1.columns.evidence,
            Op.fromLin(counted._1.table, OperatorRoleWitness.table),
            incidence,
            config.withinPolicy,
            nuisance,
            SemanticProvenance.source("scalafim-soft-lda")
          )
          .left
          .map(SoftLdaError.LdaFailure(fitId, _))
        componentCount <- components(
          config.components,
          problem.maximumComponents,
          fitId
        )
        fit <- problem
          .fit(componentCount, config.objective)
          .left
          .map(SoftLdaError.LdaFailure(fitId, _))
        trainScores <- fit
          .scores(problem.table)
          .toDense
          .left
          .map(error => SoftLdaError.SemanticFailure(fitId, error.message))
        testScores <- fit
          .scores(Op.fromLin(counted._2.table, OperatorRoleWitness.table))
          .toDense
          .left
          .map(error => SoftLdaError.SemanticFailure(fitId, error.message))
        weights <- classify(
          trainScores,
          testScores,
          membership,
          fit,
          nuisance.fold(0)(_.columns),
          fitId
        )
      yield SoftLdaKernelResult(
        weights,
        SoftLdaKernelFitReceipt(
          fitId,
          train.rows.identity,
          test.rows.identity,
          train.columns.identity,
          classes.identity,
          nuisance.fold(0)(_.columns),
          counted._3.forwardApplications,
          counted._3.transposeApplications,
          fit
        )
      )

  private def countedEvidence[
      TrainRows <: SemanticSpace,
      TestRows <: SemanticSpace,
      Features <: SemanticSpace,
      FeatureKey
  ](
      train: EvidenceTable[TrainRows, Features, SampleId, FeatureKey],
      test: EvidenceTable[TestRows, Features, SampleId, FeatureKey],
      fitId: String
  ): Either[
    SoftLdaError,
    (
        EvidenceTable[TrainRows, Features, SampleId, FeatureKey],
        EvidenceTable[TestRows, Features, SampleId, FeatureKey],
        SoftLdaApplicationCounter
    )
  ] =
    val counter = new SoftLdaApplicationCounter
    for
      countedTrain <- EvidenceTable
        .operator(
          train.rows,
          train.columns,
          counter.wrap(train.operator),
          ValueId.unsafe(s"soft-lda-$fitId-counted-train"),
          train.table.provenance
        )
        .left
        .map(error => SoftLdaError.Evidence(fitId, error))
      countedTest <- EvidenceTable
        .operator(
          test.rows,
          test.columns,
          counter.wrap(test.operator),
          ValueId.unsafe(s"soft-lda-$fitId-counted-test"),
          test.table.provenance
        )
        .left
        .map(error => SoftLdaError.Evidence(fitId, error))
    yield (countedTrain, countedTest, counter)

  private def components(
      requested: SoftLdaComponents,
      maximum: Int,
      fitId: String
  ): Either[SoftLdaError, ComponentCount] =
    requested match
      case SoftLdaComponents.Maximum =>
        ComponentCount(maximum).left.map(SoftLdaError.LdaFailure(fitId, _))
      case SoftLdaComponents.Fixed(count) if count.value <= maximum =>
        Right(count)
      case SoftLdaComponents.Fixed(count) =>
        Left(
          SoftLdaError.LdaFailure(
            fitId,
            MultivarError.InvalidComponentRequest(count.value, maximum)
          )
        )

  private def classify(
      trainScores: DMat,
      testScores: DMat,
      membership: DMat,
      fit: LdaOperatorFit[?, ?, ?],
      nuisanceColumns: Int,
      fitId: String
  ): Either[SoftLdaError, DMat] =
    val masses = new Array[Double](membership.cols)
    val centroids = Matrix.newBuilder(membership.cols, trainScores.cols)
    var sample = 0
    while sample < membership.rows do
      var klass = 0
      while klass < membership.cols do
        val weight = membership(sample, klass)
        masses(klass) += weight
        var component = 0
        while component < trainScores.cols do
          centroids(klass, component) = centroids(klass, component) + weight * trainScores(sample, component)
          component += 1
        klass += 1
      sample += 1
    var klass = 0
    while klass < membership.cols do
      var component = 0
      while component < trainScores.cols do
        centroids(klass, component) = centroids(klass, component) / masses(klass)
        component += 1
      klass += 1
    val classMeans = centroids.result()

    for
      within <- fit.realizedWithin.toDense.left.map(error => SoftLdaError.SemanticFailure(fitId, error.message))
      weights <- fit.functionalFrame.weights.toDense.left.map(error =>
        SoftLdaError.SemanticFailure(fitId, error.message)
      )
      projected = weights.t * within * weights
      degreesOfFreedom =
        math.max(1, trainScores.rows - membership.cols - nuisanceColumns)
      covariance = scale(projected, 1.0 / degreesOfFreedom)
      factor <- covariance
        .cholesky(CholeskyOptions(1e-12))
        .left
        .map(error => SoftLdaError.NumericalFailure(fitId, error.getMessage))
      coefficients <- factor
        .solve(classMeans.t)
        .left
        .map(error => SoftLdaError.NumericalFailure(fitId, error.getMessage))
    yield
      val raw = testScores * coefficients
      val shifted = Matrix.newBuilder(raw.rows, raw.cols)
      var row = 0
      while row < raw.rows do
        klass = 0
        while klass < raw.cols do
          var quadratic = 0.0
          var component = 0
          while component < classMeans.cols do
            quadratic +=
              classMeans(klass, component) * coefficients(component, klass)
            component += 1
          val prior = masses(klass) / membership.rows
          shifted(row, klass) = raw(row, klass) - 0.5 * quadratic + Math.log(math.max(prior, 1e-300))
          klass += 1
        row += 1
      normalizeScores(shifted.result())

  private def normalizeScores(value: DMat): DMat =
    val normalized = Matrix.newBuilder(value.rows, value.cols)
    var row = 0
    while row < value.rows do
      var maximum = Double.NegativeInfinity
      var column = 0
      while column < value.cols do
        maximum = math.max(maximum, value(row, column))
        column += 1

      var total = 0.0
      column = 0
      while column < value.cols do
        val weight = math.exp(value(row, column) - maximum)
        normalized(row, column) = weight
        total += weight
        column += 1

      column = 0
      while column < value.cols do
        normalized(row, column) = normalized(row, column) / total
        column += 1
      row += 1
    normalized.result()

  private def scale(value: DMat, factor: Double): DMat =
    Matrix.tabulate(value.rows, value.cols): (row, col) =>
      factor * value(row, col)

  private final class SoftLdaApplicationCounter:
    var forwardApplications: Int = 0
    var transposeApplications: Int = 0

    def wrap(source: gale.linalg.DoubleLinearOperator): gale.linalg.DoubleLinearOperator =
      gale.linalg.LinearOperator.fromFunctions(source.rows, source.cols)(
        (input, output) =>
          forwardApplications += 1
          source.applyTo(input, output)
        ,
        (input, output) =>
          transposeApplications += 1
          source.transposeApplyTo(input, output)
      )
