package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.*
import cats.Id
import cats.data.EitherT
import scalafim.fmri.mvpa.AxisDigest
import scalafim.fmri.mvpa.pattern.*

enum AlderPatternPredictionError:
  case ExpectedTrainingReceiptMismatch
  case ArtifactTrainingBindingMismatch
  case Admission(error: AlderPredictiveAdmissionError)
  case TrainingRowOrderMismatch
  case Prediction(error: PatternPredictionError)

/** Attaches an already fitted relationship to a real Alder predictive Pipe.
  * It does not fit the relationship again or grant assessment rows Fit access.
  * Unlike the artifact carrier, running this Pipe predicts from its input.
  */
object AlderPatternPrediction:
  def classification[N, Q, R, M](data: NonEmptyData[Use.Fit, Example[Array[Double], Array[Double], M]],
      model: PatternPrediction[N, Q, R], mapping: NativeAxisMapping, expectedTraining: DataFingerprint
  )(using FitContext): FitResult[Id, AlderPatternPredictionError, Trained[Pipe[Array[Double], AlderPatternPredictionError, ClassPosterior]]] =
    attach(data, model, mapping, expectedTraining, "classification"): input =>
      AxisValues(model.factors.neuralAxis, input.toVector).left.map(PatternPredictionError.Artifact.apply).flatMap(model.classify)

  def decoding[N, Q, R, M](data: NonEmptyData[Use.Fit, Example[Array[Double], Array[Double], M]],
      model: PatternPrediction[N, Q, R], mapping: NativeAxisMapping, expectedTraining: DataFingerprint
  )(using FitContext): FitResult[Id, AlderPatternPredictionError, Trained[Pipe[Array[Double], AlderPatternPredictionError, PosteriorTargetMean[Q]]]] =
    attach(data, model, mapping, expectedTraining, "Gaussian decoding"): input =>
      AxisValues(model.factors.neuralAxis, input.toVector).left.map(PatternPredictionError.Artifact.apply).flatMap(model.decode)

  def encoding[N, Q, R, M](data: NonEmptyData[Use.Fit, Example[Array[Double], Array[Double], M]],
      model: PatternPrediction[N, Q, R], mapping: NativeAxisMapping, expectedTraining: DataFingerprint
  )(using FitContext): FitResult[Id, AlderPatternPredictionError, Trained[Pipe[Array[Double], AlderPatternPredictionError, AxisValues[N]]]] =
    attach(data, model, mapping, expectedTraining, "encoding"): input =>
      AxisValues(model.factors.targetAxis, input.toVector).left.map(PatternPredictionError.Artifact.apply).flatMap(model.encode)

  private def attach[N, Q, R, M, O](data: NonEmptyData[Use.Fit, Example[Array[Double], Array[Double], M]],
      model: PatternPrediction[N, Q, R], mapping: NativeAxisMapping, expectedTraining: DataFingerprint,
      head: String)(predict: Array[Double] => Either[PatternPredictionError, O]
  )(using context: FitContext): FitResult[Id, AlderPatternPredictionError, Trained[Pipe[Array[Double], AlderPatternPredictionError, O]]] =
    val headAdmission = head match
      case "classification" => model.admitClassification
      case "Gaussian decoding" => model.admitDecoding
      case _ => Right(())
    val admission = for
      _ <- headAdmission.left.map(AlderPatternPredictionError.Prediction.apply)
      _ <- if data.fingerprint == expectedTraining then Right(()) else Left(AlderPatternPredictionError.ExpectedTrainingReceiptMismatch)
      _ <- if model.artifact.trainingBinding.fingerprintDigest == data.fingerprint.digest then Right(()) else Left(AlderPatternPredictionError.ArtifactTrainingBindingMismatch)
      _ <- NativeAxisMapping.verify(model.artifact.trainingBinding.declaredSampleAxis, mapping, expectedTraining).left.map(AlderPatternPredictionError.Admission.apply)
      ids = data.data.foldRows(Vector.empty[Long])((out, id, _) => out :+ id.value)
      _ <- if ids == mapping.nativeIds then Right(()) else Left(AlderPatternPredictionError.TrainingRowOrderMismatch)
    yield ()
    admission match
      case Left(error) => EitherT.leftT(context.stagePath.failure(error))
      case Right(_) =>
        val learner = new AttachedHeadLearner[M, O](model.artifact, head, model.numericalIdentity, predict)
        learner.fit(data)

  private final class AttachedHeadLearner[M, O](artifact: PatternArtifact, head: String, numericalIdentity: String,
      predict: Array[Double] => Either[PatternPredictionError, O]
  ) extends Learner[Id, Array[Double], Array[Double], M, O]:
    type FitError = AlderPatternPredictionError
    type RunError = AlderPatternPredictionError
    type Model = Pipe[Array[Double], AlderPatternPredictionError, O]
    def fit[U <: Use.Fit](data: NonEmptyData[U, Example[Array[Double], Array[Double], M]])(using context: FitContext): FitResult[Id, FitError, Trained[Model]] =
      val pipe = new Pipe[Array[Double], AlderPatternPredictionError, O]:
        def run(input: Array[Double]): Either[Failure[AlderPatternPredictionError], O] =
          predict(input).left.map(error => context.stagePath.failure(AlderPatternPredictionError.Prediction(error)))
      val binding = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.pattern-head.v1")
        writer.string(head)
        writer.string(numericalIdentity)
        writer.string(artifact.trainingBinding.fingerprintDigest)
        writer.string(artifact.factors.neuralAxis.descriptor.stableKey)
        writer.string(artifact.factors.targetAxis.descriptor.stableKey)
        writer.string(artifact.factors.componentAxis.descriptor.stableKey)
      val descriptor = ComponentDescriptor(ComponentId("scalafim.pattern-head"), ComponentVersion("1"),
        AuditValue.record("head" -> AuditValue.text(head), "binding" -> AuditValue.text(binding)),
        BackendFingerprint("gale", "pinned", AuditValue.record()))
      EitherT.rightT(context.complete(pipe, data, descriptor))
