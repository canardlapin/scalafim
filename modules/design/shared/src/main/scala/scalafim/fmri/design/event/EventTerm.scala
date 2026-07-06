package scalafim.fmri.design.event

import scalafim.fmri.design.Names
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.hrf.regressor.{HrfAssignment, Regressor}

final case class TermDesignMatrix(data: Mat, conditionTags: Vector[String])

final case class ConvolvedTerm(
    term: EventTerm,
    hrf: Hrf,
    data: Mat,
    columnNames: Vector[String],
    role: EventTermRole = EventTermRole.Task,
    columnRoles: Vector[EventTermColumnRole] = Vector.empty
) extends EventModelTerm:
  requireColumnMetadata()

  def keyHint: Option[String] = term.termTag
  def hrfOpt: Option[Hrf] = Some(hrf)

final case class EventTerm(
    events: Vector[Event],
    onsets: Vector[Seconds],
    durations: Vector[Seconds] = Vector.empty,
    blockIds: Vector[Int] = Vector.empty,
    termTag: Option[String] = None
):

  private val n: Int =
    val n0 = onsets.length
    require(events.forall(_.nEvents == n0), "all events must match onsets length")
    if durations.nonEmpty then require(durations.length == n0, "durations length mismatch")
    if blockIds.nonEmpty then require(blockIds.length == n0, "blockIds length mismatch")
    n0

  val durations0: Vector[Seconds] =
    if durations.isEmpty then Vector.fill(n)(0.0.s) else durations

  val blockIds0: Vector[Int] =
    if blockIds.isEmpty then Vector.fill(n)(0) else
      require(!isStrictlyDecreasing(blockIds), "'blockIds' must be non-decreasing")
      blockIds

  def conditions: Vector[String] =
    val tokenLists = events.map(_.conditionTokens).filter(_.nonEmpty)
    if tokenLists.isEmpty then Vector.empty
    else
      val rows = expandGrid(tokenLists)
      rows.map(Names.makeCondTag)

  def designMatrix(dropEmpty: Boolean = true): TermDesignMatrix =
    if n == 0 then TermDesignMatrix(Mat.zeros(0, 0), Vector.empty)
    else
      val perEvent = events.map(eventMatrix)
      val condTagsAll = conditions
      require(perEvent.nonEmpty == condTagsAll.nonEmpty, "internal: token/matrix mismatch")

      val full =
        perEvent.reduceOption(interaction).getOrElse(Mat.zeros(n, 0))

      val keepCols =
        if !dropEmpty || full.cols == 0 then (0 until full.cols).toVector
        else
          val tol = 0.0
          val kept = Vector.newBuilder[Int]
          var c = 0
          while c < full.cols do
            var acc = 0.0
            var r = 0
            while r < full.rows do
              acc += math.abs(full.data(r * full.cols + c))
              r += 1
            if acc > tol then kept += c
            c += 1
          kept.result()

      val outMat =
        if keepCols.length == full.cols then full
        else
          val out = new Array[Double](full.rows * keepCols.length)
          var r = 0
          while r < full.rows do
            var j = 0
            while j < keepCols.length do
              out(r * keepCols.length + j) = full.data(r * full.cols + keepCols(j))
              j += 1
            r += 1
          Mat.unsafe(full.rows, keepCols.length, out)

      val outTags =
        if keepCols.length == condTagsAll.length then condTagsAll
        else keepCols.map(condTagsAll)

      TermDesignMatrix(outMat, outTags)

  def convolve(
      hrf: Hrf,
      samplingFrame: SamplingFrame,
      precision: Seconds = 0.3.s,
      dropEmpty: Boolean = true,
      summate: Boolean = true,
      normalize: Boolean = false
  ): ConvolvedTerm =
    val dm = designMatrix(dropEmpty = dropEmpty)
    val nConds = dm.conditionTags.length
    val nb = hrf.nbasis
    val finalNames = Names.makeColumnNames(termTag, dm.conditionTags, nb)

    val totalRows = samplingFrame.blockLens.sum
    val totalCols = nConds * nb
    val out = new Array[Double](totalRows * totalCols)

    if nConds == 0 || totalCols == 0 || totalRows == 0 then
      return ConvolvedTerm(this, hrf, Mat.unsafe(totalRows, totalCols, out), finalNames)

    require(blockIds0.forall(b => b >= 0 && b < samplingFrame.nBlocks), "blockIds out of range for samplingFrame")

    val globOns = samplingFrame.globalOnsets(onsets, blockIds0)

    var rowOffset = 0
    var b = 0
    while b < samplingFrame.nBlocks do
      val blockLen = samplingFrame.blockLens(b)
      val grid = samplingFrame.samples(blocks = Seq(b), global = true).map(_.value)

      // Collect event indices for this block (can be empty).
      val eIdx = blockIds0.indices.filter(i => blockIds0(i) == b).toVector
      val onsB = eIdx.map(i => globOns(i).value)
      val durB = eIdx.map(i => durations0(i).value)

      var cond = 0
      while cond < nConds do
        val ampB = eIdx.map(i => dm.data.data(i * dm.data.cols + cond))
        val reg = sharedRegressor(onsets = onsB, hrf = hrf, duration = durB, amplitude = ampB, summate = summate)
        val ev = Regressor.evaluate(reg, grid, precision = precision.value)
        // Store basis-major: [b01: all conds] [b02: all conds] ...
        var basis = 0
        while basis < nb do
          val outCol = basis * nConds + cond
          var r = 0
          while r < blockLen do
            out((rowOffset + r) * totalCols + outCol) = ev.data(r * nb + basis)
            r += 1
          basis += 1
        cond += 1

      rowOffset += blockLen
      b += 1

    if normalize then normalizeColumnsInPlace(out, totalRows, totalCols)
    ConvolvedTerm(this, hrf, Mat.unsafe(totalRows, totalCols, out), finalNames)

  def convolvePerEvent(
      hrfs: Seq[Hrf],
      samplingFrame: SamplingFrame,
      precision: Seconds = 0.3.s,
      dropEmpty: Boolean = true,
      summate: Boolean = true,
      normalize: Boolean = false
  ): ConvolvedTerm =
    val dm = designMatrix(dropEmpty = dropEmpty)
    val nConds = dm.conditionTags.length

    val hrfs0: Vector[Hrf] =
      if hrfs.length == n then hrfs.toVector
      else if hrfs.length == 1 then Vector.fill(n)(hrfs.head)
      else throw new IllegalArgumentException(s"`hrfs` must have length 1 or $n, not ${hrfs.length}")

    val rep = hrfs0.headOption.getOrElse(Hrfs.SPMG1)
    val nb = rep.nbasis
    require(hrfs0.forall(_.nbasis == nb), "all per-event HRFs must have the same nbasis")

    val finalNames = Names.makeColumnNames(termTag, dm.conditionTags, nb)

    val totalRows = samplingFrame.blockLens.sum
    val totalCols = nConds * nb
    val out = new Array[Double](totalRows * totalCols)

    if nConds == 0 || totalCols == 0 || totalRows == 0 || n == 0 then
      return ConvolvedTerm(this, rep, Mat.unsafe(totalRows, totalCols, out), finalNames)

    require(blockIds0.forall(b => b >= 0 && b < samplingFrame.nBlocks), "blockIds out of range for samplingFrame")

    val globOns = samplingFrame.globalOnsets(onsets, blockIds0)

    var rowOffset = 0
    var b = 0
    while b < samplingFrame.nBlocks do
      val blockLen = samplingFrame.blockLens(b)
      val grid = samplingFrame.samples(blocks = Seq(b), global = true).map(_.value)

      val eIdx = blockIds0.indices.filter(i => blockIds0(i) == b).toVector
      val onsB = eIdx.map(i => globOns(i).value)
      val durB = eIdx.map(i => durations0(i).value)
      val hrfB = eIdx.map(i => hrfs0(i))

      var cond = 0
      while cond < nConds do
        val ampB = eIdx.map(i => dm.data.data(i * dm.data.cols + cond))
        val reg = perEventRegressor(onsets = onsB, hrfs = hrfB, duration = durB, amplitude = ampB, summate = summate)
        val ev = Regressor.evaluate(reg, grid, precision = precision.value)
        var basis = 0
        while basis < nb do
          val outCol = basis * nConds + cond
          var r = 0
          while r < blockLen do
            out((rowOffset + r) * totalCols + outCol) = ev.data(r * nb + basis)
            r += 1
          basis += 1
        cond += 1

      rowOffset += blockLen
      b += 1

    if normalize then normalizeColumnsInPlace(out, totalRows, totalCols)
    ConvolvedTerm(this, rep, Mat.unsafe(totalRows, totalCols, out), finalNames)

  private def normalizeColumnsInPlace(data: Array[Double], rows: Int, cols: Int): Unit =
    var c = 0
    while c < cols do
      var maxAbs = 0.0
      var r = 0
      while r < rows do
        val a = math.abs(data(r * cols + c))
        if a > maxAbs then maxAbs = a
        r += 1
      if maxAbs > 1e-10 then
        r = 0
        while r < rows do
          val idx = r * cols + c
          data(idx) = data(idx) / maxAbs
          r += 1
      c += 1

  private def sharedRegressor(
      onsets: Seq[Double],
      hrf: Hrf,
      duration: Seq[Double],
      amplitude: Seq[Double],
      summate: Boolean
  ): Regressor =
    val ons = onsets.map(Seconds(_)).toVector
    val durs = duration.map(Seconds(_)).toVector
    val amps = amplitude.toVector
    validateRegressorParts(ons, durs, amps)
    val keep = amps.indices.filter(i => amps(i) != 0.0)
    Regressor(
      onsets = keep.map(ons).toVector,
      durations = keep.map(durs).toVector,
      amplitudes = keep.map(amps).toVector,
      hrf = HrfAssignment.Shared(hrf),
      span = hrf.span,
      summate = summate
    )

  private def perEventRegressor(
      onsets: Seq[Double],
      hrfs: Seq[Hrf],
      duration: Seq[Double],
      amplitude: Seq[Double],
      summate: Boolean
  ): Regressor =
    val ons = onsets.map(Seconds(_)).toVector
    val durs = duration.map(Seconds(_)).toVector
    val amps = amplitude.toVector
    val hrs = hrfs.toVector
    require(hrs.length == ons.length, "per-event HRF/onset length mismatch")
    validateRegressorParts(ons, durs, amps)
    val keep = amps.indices.filter(i => amps(i) != 0.0)
    val hrsF = keep.map(hrs).toVector
    val span = hrsF.map(_.span).maxOption.getOrElse(hrs.map(_.span).maxOption.getOrElse(Seconds(0.0)))
    Regressor(
      onsets = keep.map(ons).toVector,
      durations = keep.map(durs).toVector,
      amplitudes = keep.map(amps).toVector,
      hrf = HrfAssignment.PerEvent(hrsF),
      span = span,
      summate = summate
    )

  private def validateRegressorParts(onsets: Vector[Seconds], durations: Vector[Seconds], amplitudes: Vector[Double]): Unit =
    require(onsets.length == durations.length && onsets.length == amplitudes.length, "onsets/durations/amplitudes length mismatch")
    require(onsets.forall(_.value.isFinite), "`onsets` must be finite")
    require(durations.forall(d => d.value >= 0.0 && d.value.isFinite), "`duration` must be finite and non-negative")
    require(amplitudes.forall(_.isFinite), "`amplitude` must be finite")

  private def eventMatrix(ev: Event): Mat =
    ev match
      case e: ContinuousEvent =>
        require(e.value.cols == e.columnTags.length, "continuous event colname mismatch")
        e.value
      case e: CategoricalEvent =>
        val L = e.levels.length
        val out = new Array[Double](n * L)
        var i = 0
        while i < n do
          val code = e.codes(i)
          if code >= 0 && code < L then out(i * L + code) = 1.0
          i += 1
        Mat.unsafe(n, L, out)

  private def interaction(a: Mat, b: Mat): Mat =
    if a.cols == 0 || b.cols == 0 then Mat.zeros(a.rows, 0)
    else
      require(a.rows == b.rows, "row mismatch")
      val outCols = a.cols * b.cols
      val out = new Array[Double](a.rows * outCols)
      var cb = 0
      while cb < b.cols do
        var ca = 0
        while ca < a.cols do
          val outCol = cb * a.cols + ca
          var r = 0
          while r < a.rows do
            val av = a.data(r * a.cols + ca)
            val bv = b.data(r * b.cols + cb)
            out(r * outCols + outCol) = av * bv
            r += 1
          ca += 1
        cb += 1
      Mat.unsafe(a.rows, outCols, out)

  private def expandGrid(tokenLists: Vector[Vector[String]]): Vector[Vector[String]] =
    tokenLists.foldLeft(Vector(Vector.empty[String])) { (acc, toks) =>
      val out = Vector.newBuilder[Vector[String]]
      toks.foreach(tok => acc.foreach(row => out += (row :+ tok)))
      out.result()
    }

  private def isStrictlyDecreasing(xs: Vector[Int]): Boolean =
    var i = 1
    while i < xs.length do
      if xs(i) < xs(i - 1) then return true
      i += 1
    false
