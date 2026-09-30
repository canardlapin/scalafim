package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import multivar.core.SemanticSpace
import scalafim.fmri.mvpa.{AxisDigest, MultiResponse, Observations}
import scalafim.fmri.mvpa.analysis.{EvidenceExposure, ExposureActorRole, ExposureAssurance, ExposureAttempt, ExposureControl, ExposureError, ExposurePayload, ExposurePermit, ExposurePurpose, ExposureReference, ExposureRequest, ExposureScope, PlanId, ResultIdentity}

/** Result of a provider read coupled to the immutable exposure snapshot left
  * by that attempt. Failures after an operator invocation keep that snapshot.
  */
enum AlderExposureReadResult[+M]:
  case Refused(error: ExposureError, exposure: EvidenceExposure)
  case PreflightFailed(error: AlderPredictiveAdmissionError, exposure: EvidenceExposure)
  case ReadFailed(error: AlderPredictiveAdmissionError, exposure: EvidenceExposure)
  case Read(rows: AlderMaterializedRows[M], exposure: EvidenceExposure)

/** Exposure-aware entry point for native semantic tables.
  *
  * `Observations.reindex` composes a leg with the parent operator, so a
  * training-shaped child is not evidence that a native application cannot
  * touch holdout rows. Alder exposes no constrained-row provider capability;
  * consequently this wrapper refuses training-only requests before calling
  * the operator and records every approved read as whole-population exposure.
  */
object AlderExposureReads:
  /** Metadata-only reference for this exact pair of tables and declared native
    * mapping. It is an association claim, not a payload-content hash. */
  def referenceFor[S <: SemanticSpace, N <: SemanticSpace, F <: SemanticSpace](
      plan: PlanId,
      observations: Observations[S, N],
      targets: MultiResponse[S, F],
      mapping: NativeAxisMapping,
      result: ResultIdentity
  ): ExposureReference =
    val evidence = AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.mvpa.alder-exposure-evidence.v1")
      writeIdentity(writer, observations.identity)
      writeIdentity(writer, targets.identity)
      writer.string(mapping.declaredMappingIdentity.policy.toString)
      writer.string(mapping.declaredMappingIdentity.digest)
    val provenance = AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.mvpa.alder-exposure-provenance.v1")
      observations.identity.provenanceRoots.foreach(root => writer.string(root.value))
      observations.identity.provenanceNodes.foreach(node => writer.string(node.id.value))
      targets.identity.provenanceRoots.foreach(root => writer.string(root.value))
      targets.identity.provenanceNodes.foreach(node => writer.string(node.id.value))
    ExposureReference(plan, evidence, provenance, result)

  def nativeTables[S <: SemanticSpace, N <: SemanticSpace, F <: SemanticSpace, M](
      observations: Observations[S, N],
      targets: MultiResponse[S, F],
      metadata: Vector[M],
      metadataIdentity: DataFingerprint,
      mapping: NativeAxisMapping,
      policy: NativeReadPolicy,
      exposure: EvidenceExposure,
      permit: ExposurePermit,
      request: ExposureRequest
  ): AlderExposureReadResult[M] =
    val expected = referenceFor(exposure.reference.plan, observations, targets, mapping, exposure.reference.result)
    if exposure.reference.evidenceIdentity != expected.evidenceIdentity || exposure.reference.provenanceIdentity != expected.provenanceIdentity then
      AlderExposureReadResult.Refused(ExposureError.ReferenceMismatch, exposure)
    else if request.assurance != ExposureAssurance.Instrumented then
      AlderExposureReadResult.Refused(ExposureError.NativeAssuranceUnsupported, exposure)
    else if request.purpose != ExposurePurpose.PayloadRead || request.payload != ExposurePayload.Payload then
      AlderExposureReadResult.Refused(ExposureError.RequestMismatch, exposure)
    else if request.scope == ExposureScope.Training then
      AlderExposureReadResult.Refused(ExposureError.TrainingOnlyConstraintUnsupported, exposure)
    else if request.scope != ExposureScope.WholePopulation then
      AlderExposureReadResult.Refused(ExposureError.UnknownFootprintForTrainingProbe, exposure)
    else
      MaterializationBudget.authorizeNative(policy.budget, observations.sampleAxis.size, observations.columns, targets.columns, policy.maximumColumnsPerRead) match
        case Left(error) => AlderExposureReadResult.PreflightFailed(error, exposure)
        case Right(receipt) =>
          val required = receipt.copiedCells + receipt.workspaceCells
          if request.maximumCells < required then
            AlderExposureReadResult.Refused(ExposureError.BudgetExceeded(required, request.maximumCells), exposure)
          else
            var native: Option[Either[AlderPredictiveAdmissionError, AlderMaterializedRows[M]]] = None
            val controlled = ExposureControl.read(exposure, permit, request):
              val attempted = AlderPredictiveAdmission.nativeTables(observations, targets, metadata, metadataIdentity, mapping, policy)
              native = Some(attempted)
              attempted.fold(error => Left(error.toString), _ => Right(()))
            controlled match
              case ExposureAttempt.Refused(error, current) => AlderExposureReadResult.Refused(error, current)
              case ExposureAttempt.Completed(_, current) =>
                native match
                  case Some(Right(rows)) => AlderExposureReadResult.Read(rows, current)
                  case Some(Left(error)) => AlderExposureReadResult.ReadFailed(error, current)
                  case None => AlderExposureReadResult.ReadFailed(AlderPredictiveAdmissionError.MatrixNativeRidgeUnavailable, current)
              case ExposureAttempt.Failed(_, current) =>
                native match
                  case Some(Left(error)) => AlderExposureReadResult.ReadFailed(error, current)
                  case _ => AlderExposureReadResult.ReadFailed(AlderPredictiveAdmissionError.MatrixNativeRidgeUnavailable, current)

  private def writeIdentity(writer: AxisDigest.Writer, identity: scalafim.fmri.mvpa.EvidenceIdentity): Unit =
    writer.string(identity.rows.coordinateSignature.value)
    writer.string(identity.columns.coordinateSignature.value)
    writer.string(identity.source.value)
    writer.string(identity.values.toString)
    writer.string(identity.origins.identityDigest)
