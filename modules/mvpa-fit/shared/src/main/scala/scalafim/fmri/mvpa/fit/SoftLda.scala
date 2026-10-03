package scalafim.fmri.mvpa.fit

import multivar.core.*
import multivar.family.canonical.*

import gale.linalg.{CholeskyOptions, DMat, Matrix}
import scalafim.fmri.mvpa.*

enum SoftLdaComponents:
  case Maximum
  case Fixed(count: ComponentCount)

final case class SoftLdaConfig(
    withinPolicy: WithinScatterPolicy,
    objective: LdaObjective = LdaObjective.FisherRayleigh,
    components: SoftLdaComponents = SoftLdaComponents.Maximum,
    trialNuisance: Option[TrialNuisanceDesign] = None
)

enum SoftLdaError:
  case TargetLengthMismatch(expected: Int, actual: Int)
  case TrialNuisanceLengthMismatch(expected: Int, actual: Int)
  case FeatureAxisMismatch(expected: Vector[FeatureIndex], actual: Vector[FeatureIndex])
  case InvalidTrainingFold(foldId: String, detail: String)
  case PatternFailure(foldId: String, detail: String)
  case SemanticFailure(foldId: String, detail: String)
  case LdaFailure(foldId: String, cause: MultivarError)
  case NumericalFailure(foldId: String, detail: String)

  def message: String =
    this match
      case TargetLengthMismatch(expected, actual) => s"soft-LDA target length mismatch: expected $expected, got $actual"
      case TrialNuisanceLengthMismatch(expected, actual) => s"trial-level nuisance row count mismatch: expected $expected, got $actual"
      case FeatureAxisMismatch(expected, actual) => s"soft-LDA model feature axis ${expected.map(_.value)} != prediction axis ${actual.map(_.value)}"
      case InvalidTrainingFold(foldId, detail) => s"soft-LDA fold '$foldId' is invalid: $detail"
      case PatternFailure(foldId, detail) => s"soft-LDA pattern operator failed in fold '$foldId': $detail"
      case SemanticFailure(foldId, detail) => s"soft-LDA operator adaptation failed in fold '$foldId': $detail"
      case LdaFailure(foldId, cause) => s"soft-LDA fit failed in fold '$foldId': ${cause.message}"
      case NumericalFailure(foldId, detail) => s"soft-LDA prediction failed in fold '$foldId': $detail"

/** A single fold-local soft-LDA fit.  `predict` retains the feature-space
  * evidence created during fitting, so callers can score another
  * `PatternOperator` without materializing a trial-by-feature table or
  * weakening the semantic spaces with an existential cast.
  */
final class SoftLdaModel private[scalafim] (
    val classes: Vector[ClassLabel],
    val targetKind: ClassMembershipKind,
    val fit: LdaOperatorFit[?, ?, ?],
    val trainingSamples: Vector[SampleIndex],
    val featureIndices: Vector[FeatureIndex],
    val trialNuisanceColumns: Int,
    private val predictRows: PatternOperator => Either[SoftLdaError, DMat]
):
  def predict(data: PatternOperator): Either[SoftLdaError, ClassificationPrediction] =
    if data.featureIndices != featureIndices then Left(SoftLdaError.FeatureAxisMismatch(featureIndices, data.featureIndices))
    else predictRows(data).map(probabilities => ClassificationPrediction(classes, probabilities, data.sampleIndices))

/** Fold-local soft LDA over a sample-by-feature linear operator.
  *
  * The trial table is adapted directly to `multivar.OpTable`; class and
  * nuisance row relations are then pulled back with `secondOrder`. No
  * trial-by-feature matrix is requested by this path.
  */
object SoftLda:
  private val MassTolerance = 1e-12

  /** Fits the numerical soft-LDA kernel once for one training population.
    * Cross-validation and Alder both call this method; neither path invokes
    * the other's evaluation lifecycle.
    */
  def fit(
      train: PatternOperator,
      targets: ClassMembership,
      config: SoftLdaConfig,
      foldId: String = "single-fit",
      ordinal: Int = 0
  ): Either[SoftLdaError, SoftLdaModel] =
    if targets.samples != train.samples then Left(SoftLdaError.TargetLengthMismatch(train.samples, targets.samples))
    else fitMembership(train, targets.classes, targets.values, targets.kind, config, foldId, ordinal)

  private def fitMembership(
      train: PatternOperator,
      classes: Vector[ClassLabel],
      membership: DMat,
      targetKind: ClassMembershipKind,
      config: SoftLdaConfig,
      foldId: String,
      ordinal: Int
  ): Either[SoftLdaError, SoftLdaModel] =
    if membership.rows != train.samples then Left(SoftLdaError.TargetLengthMismatch(train.samples, membership.rows))
    else if config.trialNuisance.exists(_.samples != train.samples) then
      Left(SoftLdaError.TrialNuisanceLengthMismatch(train.samples, config.trialNuisance.fold(0)(_.samples)))
    else
      for
        _ <- validateMasses(membership, classes, foldId)
        incidence <- ClassIncidence.fromSimplex(membership).left.map(SoftLdaError.LdaFailure(foldId, _))
        rows <- SpaceRef.of(s"soft-lda-fold-$ordinal-train", SpaceRole.Samples, train.samples).left.map(SoftLdaError.LdaFailure(foldId, _))
        features <- SpaceRef.of(s"soft-lda-fold-$ordinal-features", SpaceRole.Observed, train.features).left.map(SoftLdaError.LdaFailure(foldId, _))
        trainTable <- adapt(train, rows.evidence, features.evidence, foldId, "train")
        problem <- LdaProblem
          .fromTable(rows.evidence, features.evidence, trainTable, incidence, config.withinPolicy, config.trialNuisance, SemanticProvenance.source("mvpa-soft-lda"))
          .left.map(SoftLdaError.LdaFailure(foldId, _))
        componentCount <- components(config.components, problem.maximumComponents, foldId)
        ldaFit <- problem.fit(componentCount, config.objective).left.map(SoftLdaError.LdaFailure(foldId, _))
        trainScores <- ldaFit.scores(problem.table).toDense.left.map(error => SoftLdaError.SemanticFailure(foldId, error.message))
      yield
        new SoftLdaModel(
          classes, targetKind, ldaFit, train.sampleIndices, train.featureIndices, config.trialNuisance.fold(0)(_.columns),
          test =>
            for
              testRows <- SpaceRef.of(s"soft-lda-fold-$ordinal-test", SpaceRole.Samples, test.samples).left.map(SoftLdaError.LdaFailure(foldId, _))
              testTable <- adapt(test, testRows.evidence, features.evidence, foldId, "test")
              testScores <- ldaFit.scores(testTable).toDense.left.map(error => SoftLdaError.SemanticFailure(foldId, error.message))
              probabilities <- classify(trainScores, testScores, membership, ldaFit, config.trialNuisance.fold(0)(_.columns), foldId)
            yield probabilities
        )

  private def adapt[Rows <: SemanticSpace, Feature <: SemanticSpace](
      patterns: PatternOperator,
      rows: SpaceEvidence[Rows],
      features: SpaceEvidence[Feature],
      foldId: String,
      partition: String
  ): Either[SoftLdaError, OpTable[Rows, Feature, UncheckedEvidence]] =
    Op.fromLinearMap(
      patterns.linear,
      CoordinateEvidence.dual(features),
      CoordinateEvidence.primal(rows),
      OperatorRoleWitness.table,
      ValueIdentity.source(ValueId.unsafe(s"soft-lda-$partition-table")),
      SemanticProvenance.source("mvpa-pattern-operator")
    ).left.map(error => SoftLdaError.SemanticFailure(foldId, error.message))

  private def components(
      requested: SoftLdaComponents,
      maximum: Int,
      foldId: String
  ): Either[SoftLdaError, ComponentCount] =
    requested match
      case SoftLdaComponents.Maximum => ComponentCount(maximum).left.map(SoftLdaError.LdaFailure(foldId, _))
      case SoftLdaComponents.Fixed(count) if count.value <= maximum => Right(count)
      case SoftLdaComponents.Fixed(count) =>
        Left(SoftLdaError.LdaFailure(foldId, MultivarError.InvalidComponentRequest(count.value, maximum)))

  private def classify(
      trainScores: DMat,
      testScores: DMat,
      membership: DMat,
      fit: LdaOperatorFit[?, ?, ?],
      nuisanceColumns: Int,
      foldId: String
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
      within <- fit.realizedWithin.toDense.left.map(error => SoftLdaError.SemanticFailure(foldId, error.message))
      weights <- fit.functionalFrame.weights.toDense.left.map(error => SoftLdaError.SemanticFailure(foldId, error.message))
      projected = weights.t * within * weights
      degreesOfFreedom = math.max(1, trainScores.rows - membership.cols - nuisanceColumns)
      covariance = scale(projected, 1.0 / degreesOfFreedom)
      factor <- covariance
        .cholesky(CholeskyOptions(1e-12))
        .left
        .map(error => SoftLdaError.NumericalFailure(foldId, error.getMessage))
      coefficients <- factor
        .solve(classMeans.t)
        .left
        .map(error => SoftLdaError.NumericalFailure(foldId, error.getMessage))
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
            quadratic += classMeans(klass, component) * coefficients(component, klass)
            component += 1
          val prior = masses(klass) / membership.rows
          shifted(row, klass) = raw(row, klass) - 0.5 * quadratic + Math.log(math.max(prior, 1e-300))
          klass += 1
        row += 1
      Classification.softmax(shifted.result())

  private def validateMasses(
      membership: DMat,
      classes: Vector[ClassLabel],
      foldId: String
  ): Either[SoftLdaError, Unit] =
    var klass = 0
    while klass < membership.cols do
      var mass = 0.0
      var row = 0
      while row < membership.rows do
        mass += membership(row, klass)
        row += 1
      if mass <= MassTolerance then
        return Left(SoftLdaError.InvalidTrainingFold(foldId, s"class ${classes(klass).value} has no training mass"))
      klass += 1
    Right(())

  private def selectRows(values: DMat, positions: IndexedSeq[Int]): DMat =
    val out = Matrix.newBuilder(positions.length, values.cols)
    var row = 0
    while row < positions.length do
      var col = 0
      while col < values.cols do
        out(row, col) = values(positions(row), col)
        col += 1
      row += 1
    out.result()

  private def scale(value: DMat, factor: Double): DMat =
    val out = Matrix.newBuilder(value.rows, value.cols)
    var row = 0
    while row < value.rows do
      var col = 0
      while col < value.cols do
        out(row, col) = factor * value(row, col)
        col += 1
      row += 1
    out.result()
