package scalafim.fmri.mvpa.dataset.predictive

import alder.data.{FixedCoverage, IdentifiedRows}
import alder.kernel.*
import cats.Id
import cats.data.EitherT
import gale.linalg.{DMat, Matrix}
import resample4s.core.Coverage
import scalafim.fmri.mvpa.*

final case class AlderFeatureModelFoldFit(
    unit: resample4s.core.UnitKey,
    trainingStableKeys: Vector[String],
    coefficients: DMat,
    sourceMeans: Vector[Double],
    sourceScales: Vector[Double],
    targetMeans: Vector[Double],
    targetScales: Vector[Double],
    audit: Audit
)

final case class AlderFeatureModelResult(
    result: RoiAnalysisResult,
    validationReceipt: resample4s.core.PlanReceipt,
    fits: Vector[AlderFeatureModelFoldFit],
    materialization: MaterializationReceipt
)

enum AlderFeatureModelError:
  case Admission(error: AlderPredictiveAdmissionError)
  case Evidence(error: EvidenceError)
  case Numerical(error: MvpaError)
  case Binding(detail: String)

/** Directional, training-standardized ridge over explicitly admitted dense
  * observations. This surface never materializes a PatternOperator. Item keys
  * must bind the design rows exactly; feature names bind the output columns.
  */
object AlderFeatureModel:
  def crossValidate[S <: multivar.core.SemanticSpace, K, M, Cov <: Coverage.Exact](
      patterns: AlderMaterializedRows[M],
      features: FeatureModelDesign,
      direction: FeaturePredictionDirection,
      validation: ValidationDesign[S, K, Cov],
      patternNames: Vector[String],
      budget: MaterializationBudget,
      estimator: FeatureRidgeEstimator = FeatureRidgeEstimator(),
      storePrediction: Boolean = false
  ): Either[AlderFeatureModelError, AlderFeatureModelResult] =
    for
      _ <- NativeAxisMapping.verify(validation.samples.descriptor, patterns.mapping, patterns.mapping.declaredSource).left.map(AlderFeatureModelError.Admission.apply)
      _ <- if features.items == patterns.mapping.entriesByOrdinal.map(_.stableKey) then Right(()) else Left(AlderFeatureModelError.Binding("ordered item keys differ from the admitted sample axis"))
      _ <- if patternNames.length == patterns.receipt.inputs && patternNames.forall(_.trim.nonEmpty) && patternNames.distinct == patternNames then Right(()) else Left(AlderFeatureModelError.Binding("pattern names must uniquely identify every admitted neural column"))
      receipt <- MaterializationBudget.authorize(budget, validation.samples.size, patterns.receipt.inputs, features.features.cols).left.map(AlderFeatureModelError.Admission.apply)
      projected <- project(patterns, features, direction, patternNames, estimator)
      result <- evaluate(projected, patterns.mapping, features, direction, validation, patternNames, estimator, storePrediction, receipt)
    yield result

  private def project[M](patterns: AlderMaterializedRows[M], features: FeatureModelDesign, direction: FeaturePredictionDirection, names: Vector[String], estimator: FeatureRidgeEstimator): Either[AlderFeatureModelError, IdentifiedRows[Example[Array[Double], Array[Double], Unit]]] =
    val fingerprint = DataFingerprint.external(AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.feature-model.projected.v1")
      writer.string(patterns.root.fingerprint.digest)
      writer.string(direction.label)
      writer.string(java.lang.Double.toHexString(estimator.lambda))
      names.foreach(writer.string)
      features.items.foreach(writer.string)
      features.featureNames.foreach(writer.string)
      var row = 0
      while row < features.features.rows do
        var column = 0
        while column < features.features.cols do
          writer.string(java.lang.Double.toHexString(features.features(row, column)))
          column += 1
        row += 1
    )
    val ordinalById = patterns.mapping.nativeIds.zipWithIndex.toMap
    patterns.root.training(patterns.root.ids).left.map(error => AlderFeatureModelError.Binding(error.toString)).flatMap: all =>
      val examples = all.data.foldRows(Vector.empty[(Long, Example[Array[Double], Array[Double], Unit])]): (out, id, example) =>
        val ordinal = ordinalById(id.value)
        val design = Array.tabulate(features.features.cols)(column => features.features(ordinal, column))
        val neural = example.input.clone()
        val (input, target) = direction match
          case FeaturePredictionDirection.FeaturesToPatterns => design -> neural
          case FeaturePredictionDirection.PatternsToFeatures => neural -> design
        out :+ (id.value -> Example(input, target, ()))
      IdentifiedRows.fromRows(examples, fingerprint).left.map(error => AlderFeatureModelError.Binding(error.toString))

  private def evaluate[S <: multivar.core.SemanticSpace, K, Cov <: Coverage.Exact](root: IdentifiedRows[Example[Array[Double], Array[Double], Unit]], mapping: NativeAxisMapping, features: FeatureModelDesign, direction: FeaturePredictionDirection, validation: ValidationDesign[S, K, Cov], patternNames: Vector[String], estimator: FeatureRidgeEstimator, store: Boolean, receipt: MaterializationReceipt): Either[AlderFeatureModelError, AlderFeatureModelResult] =
    val targetNames = direction match
      case FeaturePredictionDirection.FeaturesToPatterns => patternNames
      case FeaturePredictionDirection.PatternsToFeatures => features.featureNames
    val sums = Matrix.newBuilder(validation.samples.size, targetNames.length)
    val observed = Matrix.newBuilder(validation.samples.size, targetNames.length)
    val counts = Array.fill(validation.samples.size)(0)
    val ordinalById = mapping.nativeIds.zipWithIndex.toMap
    val fits = Vector.newBuilder[AlderFeatureModelFoldFit]
    var failure = Option.empty[AlderFeatureModelError]
    var fold = 0
    while fold < validation.keys.length && failure.isEmpty do
      val outcome = for
        unit <- validation.at(validation.keys(fold)).left.map(AlderFeatureModelError.Evidence.apply)
        split <- root.fixedHoldout(unit.analysis.ordinals.toVector.map(mapping.nativeIds), unit.assessment.ordinals.toVector.map(mapping.nativeIds), FixedCoverage.Exhaustive).left.map(error => AlderFeatureModelError.Binding(error.toString))
        context = FitContext.root(Seed(validation.receipt.seed.value), PlanFingerprint(AxisDigest.sha256Hex: writer =>
          writer.string("scalafim.feature-model.fit.v1")
          writer.string(root.fingerprint.digest)
          writer.string(validation.samples.descriptor.stableKey)
          writer.intLE(unit.key.repeat)
          writer.intLE(unit.key.fold)
        ), SchemaFingerprint("scalafim.feature-model.v1"), NumericMode.Deterministic)
        trained <- new FeatureLearner(estimator).fit(split.train)(using context).value.left.map(_.cause)
      yield
        val fit = trained.artifact.model
        val keys = split.train.data.foldRows(Vector.empty[String])((out, id, _) => out :+ mapping.entriesByOrdinal(ordinalById(id.value)).stableKey)
        fits += AlderFeatureModelFoldFit(unit.key, keys, fit.coefficients, fit.sourceMeans.toVector, fit.sourceScales.toVector, fit.targetMeans.toVector, fit.targetScales.toVector, trained.audit)
        split.test.data.foreachRow: (id, example) =>
          if failure.isEmpty then trained.artifact.run(example.input) match
            case Left(error) => failure = Some(error.cause)
            case Right(prediction) =>
              val ordinal = ordinalById(id.value)
              var column = 0
              while column < targetNames.length do
                sums(ordinal, column) = sums(ordinal, column) + prediction(column)
                observed(ordinal, column) = example.target(column)
                column += 1
              counts(ordinal) += 1
      outcome.left.foreach(error => failure = Some(error))
      fold += 1
    failure match
      case Some(error) => Left(error)
      case None if counts.contains(0) => Left(AlderFeatureModelError.Binding("validation did not assess every bound item"))
      case None =>
        var row = 0
        while row < counts.length do
          var column = 0
          while column < targetNames.length do
            sums(row, column) = sums(row, column) / counts(row)
            column += 1
          row += 1
        val prediction = FeatureModelPrediction(direction, features.items, targetNames, sums.result(), observed.result())
        FeatureModelMetrics.compute(prediction).left.map(AlderFeatureModelError.Numerical.apply).map: metrics =>
          AlderFeatureModelResult(RoiAnalysisResult(metrics.withEstimator(estimator.lambda), if store then Some(RoiPayload.FeatureModel(prediction)) else None), validation.receipt, fits.result(), receipt)

  private final class FeatureLearner(estimator: FeatureRidgeEstimator) extends Learner[Id, Array[Double], Array[Double], Unit, Vector[Double]]:
    type FitError = AlderFeatureModelError
    type RunError = AlderFeatureModelError
    type Model = FeaturePipe
    def fit[U <: Use.Fit](data: NonEmptyData[U, Example[Array[Double], Array[Double], Unit]])(using context: FitContext): FitResult[Id, FitError, Trained[Model]] =
      val rows = data.data.foldRows(Vector.empty[Example[Array[Double], Array[Double], Unit]])((out, _, example) => out :+ example)
      val x = DMat.dense(rows.length, rows.head.input.length, rows.flatMap(_.input.toVector))
      val y = DMat.dense(rows.length, rows.head.target.length, rows.flatMap(_.target.toVector))
      StandardizedRidgeMap.fit(x, y, estimator.lambda) match
        case Left(error) => EitherT.leftT(context.stagePath.failure(AlderFeatureModelError.Numerical(error)))
        case Right(model) => EitherT.rightT(context.complete(new FeaturePipe(model, context.stagePath), data,
          ComponentDescriptor(ComponentId("scalafim.feature-model"), ComponentVersion("1"), AuditValue.record("lambda" -> AuditValue.text(java.lang.Double.toHexString(estimator.lambda))), BackendFingerprint("gale", "pinned", AuditValue.record()))))

  private final class FeaturePipe(val model: StandardizedRidgeMap, stage: StagePath) extends Pipe[Array[Double], AlderFeatureModelError, Vector[Double]]:
    def run(input: Array[Double]): Either[Failure[AlderFeatureModelError], Vector[Double]] =
      model.predict(DMat.dense(1, input.length, input.toVector)).left.map(error => stage.failure(AlderFeatureModelError.Numerical(error))).map(matrix => Vector.tabulate(matrix.cols)(column => matrix(0, column)))
