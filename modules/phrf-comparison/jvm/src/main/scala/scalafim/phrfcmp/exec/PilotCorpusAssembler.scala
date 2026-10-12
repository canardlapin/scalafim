package scalafim.phrfcmp.exec

import scala.util.control.NonFatal
import scalafim.phrfcmp.run.{ConditionAdapter, ConditionArm}
import scalafim.phrfcmp.score.*
import scalafim.phrfcmp.score.PilotCell as ScoreCell

/** Scorer-side access to held, bound generator truth. This capability is never passed to PilotArmRunner. */
trait PilotTruthSource:
  def load(job: Job): Either[String, PilotScoreTruth]

object PilotCorpusAssembler:
  private final class Refusal(val token: String) extends RuntimeException
  private[phrfcmp] def arms(cell: ScoreCell): Vector[(String, Method)] =
    if cell.isCondition then ConditionArm.All.map(a => a.id -> ConditionAdapter.method(a))
    else Vector("phrf-ml" -> Method.Phrf, "lsa" -> Method.Lsa, "lss" -> Method.Lss, "rlss" -> Method.Rlss) ++
      (if cell == ScoreCell.TTSFast then Vector.empty else Vector("glmsingle" -> Method.GlmsD))

/** Pure reconstruction of S8 inputs from committed contributions. Requires every kept slot exactly once;
  * absent bytes, wrong unit/input/arm, malformed estimates and missing Accepted TG readouts refuse. A sealed
  * unavailable result contributes Failed for every voxel, preserving its denominator. No retry history is read.
  */
final class PilotCorpusAssembler(truth: PilotTruthSource, coverage: GaussianCoverageTarget) extends CorpusAssembler:
  val schema: String = s"phrf-pilot-score-v1-${coverage.code}-mean-signed-peak"

  def outcomes(datasets: Int, contributions: Vector[UnitContribution]): Either[String, CorpusOutcomes] =
    if datasets < PilotCorpus.MinDatasets || datasets > 10000 then Left("dataset_count")
    else assemble(datasets, contributions)

  /** S10's fixed two-dataset rehearsal can inspect mappings, but cannot construct
    * a PilotCorpus or release a pilot whitelist. Ordinary outcomes keeps its
    * minimum dataset requirement; this entry point is package-internal.
    */
  private[phrfcmp] def rehearsalOutcomes(contributions: Vector[UnitContribution]): Either[String, CorpusOutcomes] =
    assemble(2, contributions)

  private def assemble(datasets: Int, contributions: Vector[UnitContribution]): Either[String, CorpusOutcomes] =
    // This private exception carries a fixed token only and is contained at this boundary.
    import PilotCorpusAssembler.Refusal
    def need[A](e: Either[String, A]): A = e.fold(t => throw new Refusal(t), identity)
    def check(ok: Boolean, token: String): Unit = if !ok then throw new Refusal(token)
    try
      val expected = (for
        c <- ScoreCell.values.toVector
        d <- 0 until datasets
        (a, _) <- PilotCorpusAssembler.arms(c)
      yield WorkUnit(CellId.parse(c.id).toOption.get, d, ArmId.parse(a).toOption.get)).toSet
      check(contributions.length == expected.size && contributions.map(_.unit).toSet == expected, "contribution_inventory")
      val decoded = contributions.map { c =>
        val v = need(c.payload.toRight("missing_contribution").flatMap(PilotContribution.decode))
        check(v.unit == c.unit, "contribution_unit")
        c.unit -> v
      }.toMap
      var condition = Map.empty[ScoreCell, Vector[ConditionDataset]]
      var trial = Map.empty[ScoreCell, Vector[TrialDataset]]
      var cov = Vector.empty[CoverageDataset]
      for c <- ScoreCell.values.toVector do
        val id = CellId.parse(c.id).toOption.get
        for d <- 0 until datasets do
          val t = need(truth.load(Job(id, d)).left.map(_ => "truth_unavailable"))
          check(t.cell == c && t.dataset == d, "truth_identity")
          val byMethod = PilotCorpusAssembler.arms(c).map { (a, m) =>
            val v = decoded(WorkUnit(id, d, ArmId.parse(a).toOption.get))
            check(v.inputFingerprint.forall(_ == t.inputFingerprint), "contribution_input")
            v.data match
              case ContributionData.Trial(vs) if m == Method.GlmsD && vs.exists(!_.isMissing) =>
                check(v.sourceFileSha256.contains(t.sourceFileSha256), "glmsingle_source_file")
              case _ => ()
            check(v.sourceFileSha256.isEmpty || (m == Method.GlmsD && v.sourceFileSha256.contains(t.sourceFileSha256)), "source_file_identity")
            m -> v.data
          }.toMap
          if c.isCondition then
            val estimates = byMethod.map { (m, data) =>
              val vs = data match
                case ContributionData.Condition(v) => v
                case ContributionData.Unavailable => Vector.fill(t.voxels)(VoxelOutcome.Failed)
                case _ => throw new Refusal("contribution_kind")
              check(vs.length == t.voxels, "voxel_count")
              m -> vs
            }
            val errors = estimates.map { (m, vs) =>
              m -> vs.zip(t.responses).map { (v, ref) => v match
                case VoxelOutcome.Estimated(e) =>
                  VoxelOutcome.Estimated(need(ConditionAdapter.mise(e.response, ref.response).toRight("response_layout")))
                case VoxelOutcome.Refused => VoxelOutcome.Refused
                case VoxelOutcome.Failed => VoxelOutcome.Failed
              }
            }
            val ds = need(ConditionDataset.of(d, 1.0, 32.0, errors).left.map(_ => "condition_dataset"))
            condition = condition.updated(c, condition.getOrElse(c, Vector.empty) :+ ds)
            if c == ScoreCell.CTG5 then
              check(t.gaussian.length == t.voxels, "gaussian_truth")
              val vs = estimates(Method.Phrf).zip(t.gaussian).map { (v, ref) => v match
                case VoxelOutcome.Estimated(e) => VoxelOutcome.Estimated(need(PilotCoverage.observation(e, ref, coverage)))
                case VoxelOutcome.Refused => VoxelOutcome.Refused
                case VoxelOutcome.Failed => VoxelOutcome.Failed
              }
              cov :+= need(CoverageDataset.of(d, vs).left.map(_ => "coverage_dataset"))
          else
            val arms = byMethod.map { (m, data) =>
              val vs = data match
                case ContributionData.Trial(v) => v
                case ContributionData.Unavailable => Vector.fill(t.voxels)(VoxelOutcome.Failed)
                case _ => throw new Refusal("contribution_kind")
              m -> vs
            }
            val ds = need(TrialDataset.of(d, t.trial.get, arms).left.map(_ => "trial_dataset"))
            trial = trial.updated(c, trial.getOrElse(c, Vector.empty) :+ ds)
      Right(CorpusOutcomes(condition, trial, cov))
    catch
      case r: Refusal => Left(r.token)
      case NonFatal(_) => Left("corpus_assembly_failed")
