package scalafim.archive.lna

import scalafim.archive.{
  ArchivePath,
  LocalityClaim,
  PayloadId,
  PayloadPlan,
  PayloadPlanSummary,
  SelectionAxes
}

sealed trait LnaPayloadPlan[A] extends PayloadPlan[A]:
  def path: ArchivePath

final case class LnaDoubleMatrixPlan private (
    path: ArchivePath,
    summary: PayloadPlanSummary
) extends LnaPayloadPlan[Payload.DoubleMatrix]

object LnaDoubleMatrixPlan:
  def full(
      path: ArchivePath,
      axes: SelectionAxes = SelectionAxes.TimeAndSample
  ): LnaDoubleMatrixPlan =
    LnaDoubleMatrixPlan(path, summary(path, axes, "lna-double-matrix"))

final case class LnaDoubleVectorPlan private (
    path: ArchivePath,
    summary: PayloadPlanSummary
) extends LnaPayloadPlan[Payload.DoubleVector]

object LnaDoubleVectorPlan:
  def full(
      path: ArchivePath,
      axes: SelectionAxes = SelectionAxes.TimeAndSample
  ): LnaDoubleVectorPlan =
    LnaDoubleVectorPlan(path, summary(path, axes, "lna-double-vector"))

final case class LnaIntMatrixPlan private (
    path: ArchivePath,
    summary: PayloadPlanSummary
) extends LnaPayloadPlan[Payload.IntMatrix]

object LnaIntMatrixPlan:
  def full(
      path: ArchivePath,
      axes: SelectionAxes = SelectionAxes.TimeAndSample
  ): LnaIntMatrixPlan =
    LnaIntMatrixPlan(path, summary(path, axes, "lna-int-matrix"))

private def summary(
    path: ArchivePath,
    axes: SelectionAxes,
    operation: String
): PayloadPlanSummary =
  PayloadPlanSummary(
    PayloadId.unsafe(path.value),
    operation,
    LocalityClaim.wholePayload(axes)
  )
