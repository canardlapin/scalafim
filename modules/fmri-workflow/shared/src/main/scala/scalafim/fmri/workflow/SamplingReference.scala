package scalafim.fmri.workflow

import scalafim.dataset.RunId
import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.design.SamplingFrame

/** Within-volume model sampling reference, expressed as a fraction of the run TR.
  * This is a modeling declaration, not evidence of slice-timing correction.
  */
opaque type SamplingFraction = Double

object SamplingFraction:
  def apply(value: Double): Either[WorkflowError, SamplingFraction] =
    if value.isFinite && value >= 0.0 && value <= 1.0 then Right(if value == 0.0 then 0.0 else value)
    else Left(WorkflowError.InvalidValue("sampling fraction", value.toString, "must be finite and between 0 and 1 inclusive"))

  def unsafe(value: Double): SamplingFraction =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  val VolumeOnset: SamplingFraction = 0.0
  val VolumeMidpoint: SamplingFraction = 0.5
  val VolumeEnd: SamplingFraction = 1.0

  extension (fraction: SamplingFraction)
    inline def value: Double = fraction

/** Event onsets keep their run-local source origin. No event-table shifts occur.
  * PerRun requires exactly the selected run identities, including label padding.
  */
enum SamplingReference:
  case Uniform(fraction: SamplingFraction)
  case PerRun(fractions: Map[RunId, SamplingFraction])

  def resolve(runs: Vector[RunInput], precision: Double = 0.1): Either[WorkflowError, ResolvedSamplingReference] =
    if runs.isEmpty then Left(WorkflowError.InvalidCatalog("sampling reference requires at least one run"))
    else if runs.map(_.id).distinct.length != runs.length then
      Left(WorkflowError.DuplicateValues("sampling run ids", WorkflowValidation.duplicates(runs.map(_.id.value)).sorted))
    else
      val ids = runs.map(_.id)
      val selected = this match
        case Uniform(fraction) => Right(Vector.fill(runs.length)(fraction))
        case PerRun(fractions) =>
          val missing = ids.filterNot(fractions.contains)
          val foreign = fractions.keySet.diff(ids.toSet).toVector.sortBy(_.value)
          if missing.nonEmpty || foreign.nonEmpty then
            Left(WorkflowError.InvalidCatalog(
              s"sampling reference run identities must match exactly; missing: ${missing.map(_.value).mkString(", ")}; unknown: ${foreign.map(_.value).mkString(", ")}"))
          else Right(ids.map(fractions))
      selected.flatMap { fractions =>
        val duration = runs.foldLeft(0.0)((sum, run) => sum + run.timepoints.toDouble * run.repetitionTime.seconds)
        val count = runs.foldLeft(0L)((sum, run) => sum + run.timepoints.toLong)
        if !duration.isFinite || count > Int.MaxValue then
          Left(WorkflowError.InvalidCatalog("sampling grid exceeds finite time or supported sample count"))
        else
          SamplingFrame.validated(runs.map(_.timepoints), runs.map(_.repetitionTime.seconds),
            runs.zip(fractions).map((run, fraction) => run.repetitionTime.seconds * fraction.value), precision)
            .left.map(error => WorkflowError.InvalidCatalog(s"sampling reference: ${error.message}"))
            .map { frame =>
              val resolved = runs.indices.map { index =>
                new ResolvedRunSampling(runs(index).id, fractions(index), frame.tr(index),
                  frame.startTime(index), runs(index).timepoints)
              }.toVector
              new ResolvedSamplingReference(this, resolved, frame)
            }
      }

object SamplingReference:
  val VolumeOnset: SamplingReference = Uniform(SamplingFraction.VolumeOnset)
  val VolumeMidpoint: SamplingReference = Uniform(SamplingFraction.VolumeMidpoint)
  val VolumeEnd: SamplingReference = Uniform(SamplingFraction.VolumeEnd)

final class ResolvedRunSampling private[workflow] (
    val run: RunId,
    val fraction: SamplingFraction,
    val repetitionTime: Seconds,
    val firstSample: Seconds,
    val timepoints: Int
):
  def lastSample: Seconds = Seconds(firstSample.value + (timepoints - 1).toDouble * repetitionTime.value)

/** Exact grid supplied to the native dataset; source metadata is reported separately. */
final class ResolvedSamplingReference private[workflow] (
    val declaration: SamplingReference,
    val runs: Vector[ResolvedRunSampling],
    val frame: SamplingFrame
)
