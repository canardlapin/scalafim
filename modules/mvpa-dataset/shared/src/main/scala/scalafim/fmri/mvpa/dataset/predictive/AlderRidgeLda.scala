package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.{Audit, DataFingerprint}
import gale.linalg.DMat
import resample4s.core.Coverage
import scalafim.fmri.mvpa.*

/** Fold-level evidence for the regularized LDA head. */
final case class RidgeLdaFoldFit(
    unit: resample4s.core.UnitKey,
    trainingStableKeys: Vector[String],
    classes: Vector[ClassLabel],
    audit: Audit
)

/** Out-of-fold dense probabilities from a fixed ridge-LDA penalty.
  * This is a classifier result, not a calibration or model-selection receipt.
  */
final case class AlderRidgeLdaResult(
    classes: Vector[ClassLabel],
    probabilities: DMat,
    rows: Vector[SwiftOofRow],
    validationReceipt: resample4s.core.PlanReceipt,
    fits: Vector[RidgeLdaFoldFit],
    assessment: SwiftAssessment,
    materialization: MaterializationReceipt,
    nativeRead: Option[NativeReadReceipt],
    penalty: RidgePenalty
)

enum AlderRidgeLdaError:
  case CorrelationLifecycle(error: AlderCorrelationCentroidError)

/** Alder adapter for a fixed-penalty ridge LDA classifier.  It deliberately
  * reuses the correlation head's complete FitContext/OOF lifecycle rather than
  * exposing a legacy cross-validation wrapper. */
object AlderRidgeLda:
  def crossValidate[S <: multivar.core.SemanticSpace, K, M](
      rows: AlderMaterializedRows[M],
      design: ValidationDesign[S, K, Coverage.ExactOnce],
      penalty: RidgePenalty,
      coding: SwiftTargetCoding
  ): Either[AlderRidgeLdaError, AlderRidgeLdaResult] =
    AlderCorrelationCentroid.crossValidateWith(rows, design, coding, RidgeLdaClassifier.fromPenalty(penalty), "scalafim.ridge-lda",
      Vector(java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(penalty.value))))
      .left.map(AlderRidgeLdaError.CorrelationLifecycle.apply)
      .map: result =>
        AlderRidgeLdaResult(
          result.classes, result.probabilities, result.rows, result.validationReceipt,
          result.fits.map(fit => RidgeLdaFoldFit(fit.unit, fit.trainingStableKeys, fit.classes, fit.audit)),
          result.assessment, result.materialization, result.nativeRead, penalty
        )
