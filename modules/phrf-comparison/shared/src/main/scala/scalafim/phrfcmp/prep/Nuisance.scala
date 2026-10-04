package scalafim.phrfcmp.prep

import scalafim.fmri.ar.TimeSegment
import scalafim.phrfcmp.ingest.Matrix

/** Contiguous run structure derived from the generator `run_id` (0..R-1, non-decreasing). */
object Runs:

  def segments(runId: Array[Int]): Either[PrepRefusal, Vector[TimeSegment]] =
    if runId.isEmpty then Left(PrepRefusal.RunLayout("no samples"))
    else if runId(0) != 0 then Left(PrepRefusal.RunLayout(s"first run id is ${runId(0)}, expected 0"))
    else
      val out = Vector.newBuilder[TimeSegment]
      var start = 0
      var t = 1
      var bad: Option[PrepRefusal] = None
      while bad.isEmpty && t < runId.length do
        if runId(t) != runId(t - 1) then
          if runId(t) != runId(t - 1) + 1 then
            bad = Some(PrepRefusal.RunLayout(s"run id jumps from ${runId(t - 1)} to ${runId(t)} at sample $t"))
          else
            out += TimeSegment(start, t, runId(t - 1))
            start = t
        t += 1
      bad match
        case Some(b) => Left(b)
        case None =>
          out += TimeSegment(start, runId.length, runId(runId.length - 1))
          Right(out.result())

/**
  * The generator nuisance (6 columns per run: intercept, linear, quadratic, 3 cosines, block diagonal) split into its
  * per-run intercept columns and the remainder. `BaselineBasis.Constant` supplies one intercept per run itself, so
  * keeping the generator's own would make every trial preparation rank deficient (S0, B8). The spans agree:
  * `runIntercepts ++ dropped` spans exactly the generator nuisance.
  *
  * @param dropped   generator nuisance with the intercept columns removed (T x (P - R)), original order preserved
  * @param runIntercepts one indicator column per run (T x R), exactly the dropped columns
  * @param droppedColumns indices (in the generator matrix) of the removed columns, one per run, ascending by run
  */
final case class SplitNuisance(dropped: Matrix, runIntercepts: Matrix, droppedColumns: Vector[Int])

object Nuisance:

  /** Column `j` is run `r`'s intercept iff it is exactly 1 on run `r`'s rows and exactly 0 elsewhere. */
  private def interceptOfRun(n: Matrix, runId: Array[Int], j: Int): Option[Int] =
    var candidate = -1
    var ok = true
    var t = 0
    while ok && t < n.rows do
      val v = n(t, j)
      if v == 1.0 then
        if candidate < 0 then candidate = runId(t)
        else if candidate != runId(t) then ok = false
      else if v != 0.0 then ok = false
      t += 1
    if !ok || candidate < 0 then None
    else
      // 1 on every row of the candidate run, 0 elsewhere
      var exact = true
      t = 0
      while exact && t < n.rows do
        val inRun = runId(t) == candidate
        if (n(t, j) == 1.0) != inRun then exact = false
        t += 1
      if exact then Some(candidate) else None

  def dropIntercepts(nuisance: Matrix, runId: Array[Int]): Either[PrepRefusal, SplitNuisance] =
    if nuisance.rows != runId.length then
      Left(PrepRefusal.Inconsistent(s"nuisance has ${nuisance.rows} rows but run_id has ${runId.length}"))
    else
      Runs.segments(runId).flatMap { segs =>
        val nRuns = segs.length
        val byRun = Array.fill(nRuns)(Vector.empty[Int])
        var j = 0
        while j < nuisance.cols do
          interceptOfRun(nuisance, runId, j).foreach(r => byRun(r) = byRun(r) :+ j)
          j += 1
        byRun.indices.find(r => byRun(r).length != 1) match
          case Some(r) => Left(PrepRefusal.NuisanceIntercepts(r, byRun(r).length))
          case None =>
            val dropCols = byRun.map(_.head).toVector
            val dropSet = dropCols.toSet
            val keep = (0 until nuisance.cols).filterNot(dropSet.contains).toArray
            val kept = new Array[Double](nuisance.rows * keep.length)
            val ints = new Array[Double](nuisance.rows * nRuns)
            var t = 0
            while t < nuisance.rows do
              var k = 0
              while k < keep.length do
                kept(t * keep.length + k) = nuisance(t, keep(k))
                k += 1
              ints(t * nRuns + runId(t)) = 1.0
              t += 1
            for
              d <- Matrix.of(nuisance.rows, keep.length, kept).left.map(e => PrepRefusal.Inconsistent(e.message))
              i <- Matrix.of(nuisance.rows, nRuns, ints).left.map(e => PrepRefusal.Inconsistent(e.message))
            yield SplitNuisance(d, i, dropCols)
      }
