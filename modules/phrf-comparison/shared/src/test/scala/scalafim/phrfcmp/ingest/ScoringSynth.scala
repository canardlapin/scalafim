package scalafim.phrfcmp.ingest

import scalafim.phrfcmp.exec.*
import scalafim.phrfcmp.run.*
import scalafim.phrfcmp.score.*
import scalafim.phrfcmp.score.PilotCell as ScoreCell
import scalafim.fmri.fit.profile.DecodeStatus

/** Synthetic scoring geometry only; no generator root, real fitting, pilot stream or qualification claim. */
object ScoringSynth:
  private def matrix(r: Int, c: Int)(f: (Int, Int) => Double): Matrix =
    Matrix.of(r, c, Array.tabulate(r * c)(i => f(i / c, i % c))).toOption.get
  val voxels = 8
  val conditions = Vector.tabulate(18)(_ % 3)
  def bound(cell: ScoreCell, d: Int): BoundDataset =
    val y = matrix(voxels, 36)((v, i) => math.sin(i + v * 0.13 + d * 0.2))
    val in = FitInputs(y, None, matrix(36, 1)((_, _) => 1.0), Array.tabulate(36)(_.toDouble), Array.fill(36)(0),
      Array.tabulate(18)(_.toDouble), conditions.toArray, Array.tabulate(18)(identity), Array.fill(18)(0), Array.fill(18)(0.0))
    val tau = (v: Int) => 5.0 + 0.01 * v
    val kernel = matrix(voxels, 481)((v, k) => math.exp(-0.5 * math.pow((k / 10.0 - tau(v)) / 1.5, 2)))
    val coef = matrix(voxels, 3)((v, c) => (if c == 1 then -1.0 else 1.0) * (1 + 0.03 * v + 0.2 * c))
    val beta = matrix(voxels, 18)((v, i) => coef(v, conditions(i)) + math.sin(i * 0.7 + v * 0.1))
    val truth = ScoreTruth(y, in.evOnset.clone(), Array.tabulate(481)(_ / 10.0), kernel,
      matrix(voxels, 2)((v, k) => if k == 0 then tau(v) else 1.5), coef, coef, Array.fill(voxels)(1.0),
      matrix(voxels, 1)((_, _) => 0.0), None, Option.when(!cell.isCondition)(beta), None)
    val spec = CellSpec(cell.id, if cell.isCondition then CellKind.Condition else CellKind.Trial, voxels, 0, 1.0, true)
    val manifest = Manifest("phrf-gen-npz-1", "0" * 64, RootKind.Harness, "0" * 16, false, Set.empty, d,
      Map.empty, Map.empty, Map.empty, "synthetic.npz", "0" * 64, Map.empty, spec, 36, 18)
    BoundDataset(manifest, "0" * 64, Map.empty, in, truth)

  def unit(cell: ScoreCell, d: Int, arm: String): WorkUnit =
    WorkUnit(CellId.parse(cell.id).toOption.get, d, ArmId.parse(arm).toOption.get)

  def contribution(u: WorkUnit, shift: Double = 0.0): Array[Byte] =
    val cell = ScoreCell.values.find(_.id == u.cell.value).get
    val b = bound(cell, u.dataset)
    val fp = ScoringInputFingerprint.of(b.fit)
    if cell.isCondition then
      val arm = ConditionArm.All.find(_.id == u.arm.value).get
      val grid = ResponseGrid.standard().toOption.get
      val vs = Vector.tabulate(voxels) { v =>
        val factor = 1.1 + 0.01 * arm.ordinal + 0.002 * v + 0.004 * u.dataset + shift
        val curves = Vector.tabulate(3)(c => Array.tabulate(321)(i => b.truth.condCoef(v, c) * b.truth.truthKernel(v, i) * factor))
        val detail = Option.when(arm == ConditionArm.Phrf)(PhrfVoxelDetail(
          Vector(b.truth.truthParams(v, 0) + 0.01 * (v - 12) + 0.003 * u.dataset, math.log(1.5)),
          Vector.fill(3)(1.0), DecodeStatus.Accepted, 0.0, Vector(0.08, 0.1),
          Vector.tabulate(3)(c => b.truth.condCoef(v, c) * factor)))
        ConditionVoxel(v, ConditionArmStatus.Estimated, Some(ConditionResponse(grid, curves)), detail)
      }
      PilotContribution.condition(u, fp, ConditionArmResult(arm, "0" * 64, vs, None, None)).toOption.get
    else
      val beta = b.truth.trialBeta.get
      val vs = Vector.tabulate(voxels) { v =>
        val error = 0.1 + 0.003 * u.dataset + 0.002 * v + 0.01 * u.arm.value.length + shift
        VoxelOutcome.Estimated(TrialEstimate.of(Vector.tabulate(18)(i => beta(v, i) + error * math.cos(i * 1.3 + v))).toOption.get)
      }
      PilotContribution.trial(u, fp, vs, Option.when(u.arm.value == "glmsingle")(b.inputSha256)).toOption.get
