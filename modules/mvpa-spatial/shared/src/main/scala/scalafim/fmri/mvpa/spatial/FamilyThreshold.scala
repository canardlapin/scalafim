package scalafim.fmri.mvpa.spatial

import scalafim.fmri.mvpa.pattern.{CompletedVoxelFamily, FrozenVoxelOmnibusReference, VoxelFamilyError, VoxelFamilyReferenceMode}
import scalafim.fmri.threshold.{AdjustedP, Alpha, MaxNull, MaxNullDistribution, NullReference, ThresholdAlternative, ThresholdCutoff, ThresholdError}

enum FamilyThresholdError:
  case Binding(detail: String)
  case Provider(cause: ThresholdError)
  case Unavailable(detail: String)

/** Candidate corrected omnibus family only. A complete execution receipt
  * cannot itself establish the frozen scientific qualification. */
final class CandidateVoxelFamilyThreshold private[spatial] (
    val family: CompletedVoxelFamily, val adjustedP: Vector[AdjustedP],
    val cutoff: ThresholdCutoff, val rejected: Vector[Boolean]
):
  def admittedC1: Either[FamilyThresholdError, Nothing] =
    Left(FamilyThresholdError.Unavailable("M4.09 released strong-FWER qualification is pending"))
  def fdr: Either[FamilyThresholdError, Nothing] =
    Left(FamilyThresholdError.Unavailable("single-step max-statistic candidate is not an FDR procedure"))

object FamilyThreshold:
  /** Consume sealed complete execution, not user-supplied maxima. The core
    * already streamed every complete upper-tail F field into a maximum and
    * its raw field digest. This explicit trust boundary avoids repeating B
    * model fits or retaining a B-by-voxel matrix. */
  def omnibus(reference: FrozenVoxelOmnibusReference, family: CompletedVoxelFamily
  ): Either[FamilyThresholdError, CandidateVoxelFamilyThreshold] =
    val p = reference.members.size
    if family.reference.identity != reference.identity || family.reference.members != reference.members ||
        family.reference.brainSource != reference.brainSource || family.reference.targetSource != reference.targetSource then
      Left(FamilyThresholdError.Binding("exact family, frozen null and confirmation sources required"))
    else if reference.mode != VoxelFamilyReferenceMode.DistinctNonIdentityMonteCarlo || !family.matchesObservedDigest || family.observed.size != p ||
        family.maxima.size != reference.draws || family.replicates.size != reference.draws ||
        family.replicates.zipWithIndex.exists((value, i) => value.action.replicate != i ||
          value.fieldMembers != p || value.completeMembers != p || value.failedMembers != 0 || value.firstFailure.nonEmpty ||
          value.maximum.isEmpty || value.action.actionSeed != reference.actionSeed.value) ||
        family.replicates.map(_.action.permutationDigest).distinct.size != reference.draws ||
        family.observed.exists(value => !value.isFinite || value < 0.0) then
      Left(FamilyThresholdError.Binding("complete fixed-B distinct nonidentity upper-tail omnibus execution required"))
    else
      for
        alpha <- Alpha(reference.alpha).left.map(FamilyThresholdError.Provider.apply)
        distribution <- MaxNullDistribution.fromOrientedMaxima(family.maxima.toArray,
          ThresholdAlternative.Greater, NullReference.MonteCarlo).left.map(FamilyThresholdError.Provider.apply)
        adjusted <- MaxNull.pValues(family.observed.toArray, distribution).left.map(FamilyThresholdError.Provider.apply)
        cutoff <- MaxNull.cutoff(distribution, alpha).left.map(FamilyThresholdError.Provider.apply)
        decisions <-
          family.observed.foldLeft[Either[FamilyThresholdError, Vector[Boolean]]](Right(Vector.empty)): (current, value) =>
            for
              prefix <- current
              decision <- cutoff.rejects(value).left.map(FamilyThresholdError.Provider.apply)
            yield prefix :+ decision
      yield new CandidateVoxelFamilyThreshold(family, adjusted.toVector, cutoff, decisions)

  def mixedComponentFamilyUnavailable: Either[VoxelFamilyError, Nothing] =
    Left(VoxelFamilyError.Unavailable("association and fixed-head predictive-loss nulls have no qualified common action or predictive p-value provider; the original mixed 2r family is not corrected by this voxel procedure"))
