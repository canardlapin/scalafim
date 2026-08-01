package scalafim.fmri.hrf.design

import scalafim.fmri.hrf.{Hrf, Seconds}
import scalafim.fmri.hrf.Hrfs
import scalafim.fmri.hrf.regressor.Regressor
import scalafim.fmri.hrf.linalg.Mat

object Design:

  /** Build a condition-by-time design matrix from an event table.
    *
    * Each block is convolved on its own sampling grid using only its own
    * events. Rendering the whole experiment as one concatenated time axis —
    * which is what this used to do — lets an HRF tail cross a run boundary: an
    * event 2 s before the end of a 100 s run put its entire response, peak
    * included, into the opening scans of the next run. Two runs that both begin
    * at numerical time zero do not share a time line, and concatenating them is
    * a presentation choice rather than a modelling one.
    *
    * Columns are condition-major: for each level in first-appearance order,
    * `nbasis` consecutive columns.
    */
  def regressorDesign(
      onsets: Seq[Double],
      fac: Seq[String],
      block: Seq[Int],
      sframe: SamplingFrame,
      hrf: Hrf = Hrfs.SPMG1,
      duration: Seq[Double] = Seq(0.0),
      amplitude: Seq[Double] = Seq(1.0),
      span: Double = 40.0,
      precision: Double = 0.33,
      method: Regressor.EvalMethod = Regressor.EvalMethod.Conv,
      summate: Boolean = true
  ): Mat =
    require(
      onsets.length == fac.length && onsets.length == block.length,
      "`onsets`, `fac`, and `block` must match length"
    )
    val n = onsets.length
    require(duration.length == 1 || duration.length == n, "`duration` must have length 1 or match `onsets`")
    require(amplitude.length == 1 || amplitude.length == n, "`amplitude` must have length 1 or match `onsets`")
    require(
      block.forall(b => b >= 0 && b < sframe.nBlocks),
      s"`block` values must lie in [0, ${sframe.nBlocks - 1}]"
    )

    def durAt(i: Int): Double = if duration.length == 1 then duration.head else duration(i)
    def ampAt(i: Int): Double = if amplitude.length == 1 then amplitude.head else amplitude(i)

    // Levels are fixed across blocks so every block contributes the same
    // columns in the same order, including blocks where a level is absent.
    val levels = fac.foldLeft(Vector.empty[String])((acc, f) => if acc.contains(f) then acc else acc :+ f)
    val nb = hrf.nbasis
    val totalCols = levels.length * nb
    val totalRows = sframe.blockLens.sum
    val out = new Array[Double](totalRows * totalCols)

    var rowOffset = 0
    var b = 0
    while b < sframe.nBlocks do
      val blockLen = sframe.blockLens(b)
      // Run-local sample times matched with run-local onsets: no global offset
      // is applied on either side, so nothing can leak across the join.
      val grid = sframe.samples(blocks = Seq(b), global = false).map(_.value)
      val inBlock = block.indices.filter(i => block(i) == b).toVector

      var lv = 0
      while lv < levels.length do
        val idx = inBlock.filter(i => fac(i) == levels(lv))
        val column =
          if idx.isEmpty then Mat.zeros(blockLen, nb)
          else
            val reg = Regressor(
              onsets = idx.map(onsets),
              hrf = hrf,
              duration = idx.map(durAt),
              amplitude = idx.map(ampAt),
              span = Some(span),
              summate = summate
            )
            Regressor.evaluate(reg, grid, precision, method)
        var r = 0
        while r < blockLen do
          var j = 0
          while j < nb do
            out((rowOffset + r) * totalCols + lv * nb + j) = column(r, j)
            j += 1
          r += 1
        lv += 1

      rowOffset += blockLen
      b += 1

    Mat.unsafe(totalRows, totalCols, out)
