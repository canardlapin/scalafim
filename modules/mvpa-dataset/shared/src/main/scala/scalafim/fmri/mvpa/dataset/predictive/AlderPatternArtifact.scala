package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.*
import cats.Id
import cats.data.EitherT
import scalafim.fmri.mvpa.pattern.PatternArtifact

enum AlderPatternArtifactError:
  case TrainingScopeMismatch

/** Carries a ScalaFIM pattern artifact through Alder's real fitted lifecycle.
  * It intentionally makes no predictive or inferential qualification claim.
  */
final class AttachedPatternArtifact private[predictive] (
    val artifact: PatternArtifact,
    val trained: Trained[Pipe[Array[Double], AlderPatternArtifactError, PatternArtifact]]
):
  val trainingReceipt: DataFingerprint = trained.audit.data

object AlderPatternArtifact:
  /** Attaches an already-fitted experimental artifact to an exact Alder training
    * receipt. This is deliberately not a numeric fitting entry point. The
    * fitted carrier Pipe ignores its input and returns the artifact; running it
    * does not predict an outcome or authorize fitting on assessment rows.
    */
  def attach[M](data: NonEmptyData[Use.Fit, Example[Array[Double], Array[Double], M]], artifact: PatternArtifact, expectedTraining: DataFingerprint)(using context: FitContext): FitResult[Id, AlderPatternArtifactError, AttachedPatternArtifact] =
    if data.fingerprint != expectedTraining || artifact.trainingBinding.fingerprintDigest != data.fingerprint.digest then EitherT.leftT(context.stagePath.failure(AlderPatternArtifactError.TrainingScopeMismatch))
    // This attaches an experimental domain artifact to an Alder receipt. Its
    // domain lineage remains declared provenance; it is not a fitted-source proof.
    else
      val learner = new CarrierLearner[M](artifact)
      learner.fit(data).map(trained => new AttachedPatternArtifact(artifact, trained))

  private val descriptor = ComponentDescriptor(ComponentId("scalafim.pattern-artifact"), ComponentVersion("1"), AuditValue.record(), BackendFingerprint("scalafim", "1", AuditValue.record()))

  private final class CarrierLearner[M](artifact: PatternArtifact) extends Learner[Id, Array[Double], Array[Double], M, PatternArtifact]:
    type FitError = AlderPatternArtifactError
    type RunError = AlderPatternArtifactError
    type Model = Pipe[Array[Double], AlderPatternArtifactError, PatternArtifact]
    def fit[U <: Use.Fit](data: NonEmptyData[U, Example[Array[Double], Array[Double], M]])(using context: FitContext): FitResult[Id, FitError, Trained[Model]] =
      val pipe = new Pipe[Array[Double], AlderPatternArtifactError, PatternArtifact]:
        def run(input: Array[Double]): Either[Failure[AlderPatternArtifactError], PatternArtifact] = Right(artifact)
      EitherT.rightT(context.complete(pipe, data, descriptor))
