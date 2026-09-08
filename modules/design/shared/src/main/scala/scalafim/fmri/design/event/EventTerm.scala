package scalafim.fmri.design.event

import scalafim.fmri.design.{CellKey, ConditionProvenance, DesignError, EventRowProvenance, FactorId, HrfColumnScale, HrfColumnScaling, ModulatorId, PhaseId, TermId, CellAssignment}
import scalafim.fmri.design.contrast.LevelId
import scalafim.fmri.design.Names
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.hrf.regressor.{HrfAssignment, Regressor}

import scala.util.control.NonFatal

final case class TermDesignMatrix(data: Mat, conditionTags: Vector[String])

final case class ConvolvedTerm(
    term: EventTerm,
    hrf: Hrf,
    data: Mat,
    columnNames: Vector[String],
    role: EventTermRole = EventTermRole.Task,
    columnRoles: Vector[EventTermColumnRole] = Vector.empty,
    columnConditions: Vector[Option[String]] = Vector.empty,
    columnBasisIx: Vector[Option[Int]] = Vector.empty,
    columnCells: Vector[Option[CellKey]] = Vector.empty,
    columnModulators: Vector[Option[ModulatorId]] = Vector.empty,
    columnHrfs: Vector[Hrf] = Vector.empty,
    columnScales: Vector[HrfColumnScale] = Vector.empty,
    convolutionRecipe: Option[ConvolutionRecipe] = None
) extends EventModelTerm:
  requireColumnMetadata()
  require(
    columnConditions.isEmpty || columnConditions.length == data.cols,
    s"columnConditions has ${columnConditions.length} entries for ${data.cols} data columns"
  )
  require(
    columnBasisIx.isEmpty || columnBasisIx.length == data.cols,
    s"columnBasisIx has ${columnBasisIx.length} entries for ${data.cols} data columns"
  )
  require(
    columnCells.isEmpty || columnCells.length == data.cols,
    s"columnCells has ${columnCells.length} entries for ${data.cols} data columns"
  )
  require(
    columnModulators.isEmpty || columnModulators.length == data.cols,
    s"columnModulators has ${columnModulators.length} entries for ${data.cols} data columns"
  )
  require(
    columnHrfs.isEmpty || columnHrfs.length == data.cols,
    s"columnHrfs has ${columnHrfs.length} entries for ${data.cols} data columns"
  )
  require(
    columnScales.isEmpty || columnScales.length == data.cols,
    s"columnScales has ${columnScales.length} entries for ${data.cols} data columns"
  )

  def sampledSupport(
      retainedScans: Vector[scalafim.fmri.design.ScanIndex],
      unscaledAbsoluteTolerance: Double = 0.0
  ): Either[ConvolutionSupportError, ConvolvedSupport] =
    convolutionRecipe.toRight(ConvolutionSupportError.MissingRecipe)
      .flatMap(_.inspect(this, retainedScans, unscaledAbsoluteTolerance))

  def keyHint: Option[String] = term.termTag
  def hrfOpt: Option[Hrf] = Some(hrf)

  def hrfForColumn(localColumn: Int): Hrf =
    require(localColumn >= 0 && localColumn < data.cols, "local column is out of bounds")
    columnHrfs.lift(localColumn).getOrElse(hrf)

  def scaleForColumn(localColumn: Int): HrfColumnScale =
    require(localColumn >= 0 && localColumn < data.cols, "local column is out of bounds")
    columnScales.lift(localColumn).getOrElse(HrfColumnScale.identity)

  /** Distinct basis cardinalities represented by this term's realized columns.
    *
    * A conventional term has one shared HRF and therefore one cardinality.
    * Cell-specific lowering records the assigned HRF for every realized
    * column, so consumers can detect mixed-width terms before applying a
    * homogeneous-basis contrast algorithm.
    */
  def basisWidths: Vector[Int] =
    if columnHrfs.nonEmpty then columnHrfs.map(_.nbasis).distinct.sorted
    else Vector(hrf.nbasis)

  def hasHeterogeneousBasis: Boolean =
    basisWidths.lengthCompare(1) > 0

final case class EventTerm(
    events: Vector[Event],
    onsets: Vector[Seconds],
    durations: Vector[Seconds] = Vector.empty,
    blockIds: Vector[Int] = Vector.empty,
    termTag: Option[String] = None,
    /** The phase identity when this term was lowered from a multiphase trial. */
    phaseId: Option[PhaseId] = None,
    /** Source-row identity retained alongside the numerical event schedule. */
    eventProvenance: Vector[EventRowProvenance] = Vector.empty,
    /** Original input table rows; empty means unknown, never an implicit renumbering. */
    sourceRows: Vector[Int] = Vector.empty
):

  val schedule: EventSchedule =
    EventSchedule
      .fromParts(onsets, durations, blockIds)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  private val n: Int =
    val n0 = schedule.size
    require(events.forall(_.nEvents == n0), "all events must match onsets length")
    n0

  require(
    eventProvenance.isEmpty || eventProvenance.length == n,
    s"event provenance has ${eventProvenance.length} entries but expected $n"
  )
  require(
    phaseId.forall(id => eventProvenance.isEmpty || eventProvenance.forall(_.phase.contains(id))),
    "term phase id must agree with every event provenance entry"
  )

  require(sourceRows.isEmpty || sourceRows.length == n, "source rows must match the event schedule")
  require(sourceRows.forall(_ >= 0), "source rows must be non-negative")
  require(sourceRows.isEmpty || eventProvenance.isEmpty || sourceRows == eventProvenance.map(_.sourceRow),
    "source rows must agree with phase provenance")

  /** Known source indices for ordinary or phased events. Hand-built terms may
    * have no source table and therefore return an empty vector.
    */
  def sourceRowIndices: Vector[Int] =
    if sourceRows.nonEmpty then sourceRows else eventProvenance.map(_.sourceRow)

  val durations0: Vector[Seconds] =
    schedule.durations

  val blockIds0: Vector[Int] =
    schedule.blockIds

  def conditions: Vector[String] =
    val tokenLists = events.map(_.conditionTokens).filter(_.nonEmpty)
    if tokenLists.isEmpty then Vector.empty
    else
      val rows = expandGrid(tokenLists)
      rows.map(Names.makeCondTag)

  /** Structural provenance for each retained condition column.
    *
    * This is deliberately computed from the event values and factor levels,
    * not from a rendered condition token.  The order follows the same
    * interaction/grid order as [[designMatrix]], so basis-major lowering can
    * attach the result to realized columns without parsing names.
    */
  def conditionProvenance(dropEmpty: Boolean = true): Vector[ConditionProvenance] =
    val choices = events.map {
      case c: CategoricalEvent =>
        c.levels.map { level =>
          (
            Names.levelToken(c.varName, level),
            Some(CellAssignment(FactorId.unsafe(c.varName), LevelId.unsafe(level))),
            Option.empty[ModulatorId]
          )
        }
      case c: ContinuousEvent =>
        c.columnTags.zip(c.modulatorIds).map { case (tag, modulator) =>
          (
            tag,
            Option.empty[CellAssignment],
            Some(modulator)
          )
        }
    }.filter(_.nonEmpty)

    if choices.isEmpty then Vector.empty
    else
      val rows =
        choices.foldLeft(Vector(Vector.empty[(String, Option[CellAssignment], Option[ModulatorId])])) {
          (acc, next) =>
            val out = Vector.newBuilder[Vector[(String, Option[CellAssignment], Option[ModulatorId])]]
            next.foreach(item => acc.foreach(row => out += (row :+ item)))
            out.result()
        }
      val all = rows.map { row =>
        val cells = row.flatMap(_._2)
        val modulators = row.flatMap(_._3).map(_.value)
        val modulator =
          modulators match
            case Vector() => None
            case xs        => Some(ModulatorId.unsafe(xs.mkString("+")))
        ConditionProvenance(CellKey.unsafe(cells), modulator)
      }

      val perEvent = events.map(eventMatrix)
      val full = perEvent.reduceOption(interaction).getOrElse(Mat.zeros(n, 0))
      val keep = keepConditionIndices(full, dropEmpty)
      keep.map(all)

  def designMatrix(dropEmpty: Boolean = true): TermDesignMatrix =
    if n == 0 then TermDesignMatrix(Mat.zeros(0, 0), Vector.empty)
    else
      val perEvent = events.map(eventMatrix)
      val condTagsAll = conditions
      require(perEvent.nonEmpty == condTagsAll.nonEmpty, "internal: token/matrix mismatch")

      val full =
        perEvent.reduceOption(interaction).getOrElse(Mat.zeros(n, 0))

      val keepCols = keepConditionIndices(full, dropEmpty)

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
      scaling: HrfColumnScaling = HrfColumnScaling.AsConvolved
  ): ConvolvedTerm =
    val dm = designMatrix(dropEmpty = dropEmpty)
    val nConds = dm.conditionTags.length
    val nb = hrf.nbasis
    val finalNames = Names.makeColumnNames(termTag, dm.conditionTags, nb)
    val finalConditions = columnConditionsFor(dm.conditionTags, nb)
    val finalBasisIx = columnBasisIxFor(nConds, nb)
    val provenance = conditionProvenance(dropEmpty)
    val finalCells = provenanceFor(provenance, p => Some(p.cell), nb)
    val finalModulators = provenanceFor(provenance, _.modulator, nb)

    val totalRows = samplingFrame.blockLens.sum
    val totalCols = nConds * nb
    val out = new Array[Double](totalRows * totalCols)
    val emptyScales = Vector.fill(totalCols)(HrfColumnScale.applied(scaling, 1.0))
    val channels = Vector.newBuilder[ConvolutionChannel]
    def recorded(value: ConvolvedTerm): ConvolvedTerm =
      value.copy(convolutionRecipe = Some(new ConvolutionRecipe(value, samplingFrame, precision, channels.result())))


    if nConds == 0 || totalCols == 0 || totalRows == 0 then
      return recorded(ConvolvedTerm(
        this,
        hrf,
        Mat.unsafe(totalRows, totalCols, out),
        finalNames,
        columnConditions = finalConditions,
        columnBasisIx = finalBasisIx,
        columnCells = finalCells,
        columnModulators = finalModulators,
        columnScales = emptyScales
      ))

    require(blockIds0.forall(b => b >= 0 && b < samplingFrame.nBlocks), "blockIds out of range for samplingFrame")

    val globOns = samplingFrame.globalOnsets(onsets, blockIds0)

    // Sampling the shared kernel is response-independent and can be reused for
    // every condition and run. Keep it lazy so an explicitly retained all-zero
    // term never evaluates the HRF at all.
    lazy val preparedKernel =
      Regressor
        .prepareConvolution(hrf, hrf.span, precision)
        .fold(error => throw new IllegalArgumentException(error.message), identity)

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
        channels += ConvolutionChannel(b, rowOffset, grid.map(Seconds(_)), eIdx,
          Vector.tabulate(nb)(basis => basis * nConds + cond), reg)
        if ampB.exists(_ != 0.0) then
          val ev = preparedKernel.evaluate(reg, grid)
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

    val columnScales = scaleColumnsInPlace(out, totalRows, totalCols, scaling)
    recorded(ConvolvedTerm(
      this,
      hrf,
      Mat.unsafe(totalRows, totalCols, out),
      finalNames,
      columnConditions = finalConditions,
      columnBasisIx = finalBasisIx,
      columnCells = finalCells,
      columnModulators = finalModulators,
      columnScales = columnScales
    ))

  /** Convolve one HRF per realized condition cell.  Unlike
    * [[convolvePerEvent]], this path permits different basis cardinalities:
    * each condition owns a contiguous block whose width is derived from its
    * assigned HRF.  Structural metadata is emitted alongside every column,
    * so downstream consumers never need to recover offsets from names.
    */
  def convolveByCondition(
      hrfs: Seq[Hrf],
      samplingFrame: SamplingFrame,
      precision: Seconds = 0.3.s,
      dropEmpty: Boolean = true,
      summate: Boolean = true,
      scaling: HrfColumnScaling = HrfColumnScaling.AsConvolved
  ): ConvolvedTerm =
    val dm = designMatrix(dropEmpty = dropEmpty)
    val nConds = dm.conditionTags.length
    val hrfs0: Vector[Hrf] =
      if hrfs.length == nConds then hrfs.toVector
      else if hrfs.length == 1 then Vector.fill(nConds)(hrfs.head)
      else throw new IllegalArgumentException(s"`hrfs` must have length 1 or $nConds, not ${hrfs.length}")
    val provenance = conditionProvenance(dropEmpty)
    require(provenance.length == nConds, "condition provenance and design columns must have equal length")

    val representative = hrfs0.headOption.getOrElse(Hrfs.SPMG1)
    val totalRows = samplingFrame.blockLens.sum
    val totalCols = hrfs0.map(_.nbasis).sum
    val finalNames =
      dm.conditionTags.zip(hrfs0).flatMap { case (condition, conditionHrf) =>
        Names.makeColumnNames(termTag, Vector(condition), conditionHrf.nbasis)
      }
    val finalConditions =
      dm.conditionTags.zip(hrfs0).flatMap { case (condition, conditionHrf) =>
        Vector.fill(conditionHrf.nbasis)(Some(condition))
      }
    val finalBasisIx =
      hrfs0.flatMap(conditionHrf => (1 to conditionHrf.nbasis).map(Some(_)))
    val finalCells =
      provenance.zip(hrfs0).flatMap { case (p, conditionHrf) => Vector.fill(conditionHrf.nbasis)(Some(p.cell)) }
    val finalModulators =
      provenance.zip(hrfs0).flatMap { case (p, conditionHrf) => Vector.fill(conditionHrf.nbasis)(p.modulator) }
    val finalHrfs = hrfs0.flatMap(conditionHrf => Vector.fill(conditionHrf.nbasis)(conditionHrf))
    val out = new Array[Double](totalRows * totalCols)
    val emptyScales = Vector.fill(totalCols)(HrfColumnScale.applied(scaling, 1.0))
    val channels = Vector.newBuilder[ConvolutionChannel]
    def recorded(value: ConvolvedTerm): ConvolvedTerm =
      value.copy(convolutionRecipe = Some(new ConvolutionRecipe(value, samplingFrame, precision, channels.result())))


    if nConds == 0 || totalCols == 0 || totalRows == 0 then
      return recorded(ConvolvedTerm(
        this,
        representative,
        Mat.unsafe(totalRows, totalCols, out),
        finalNames,
        columnConditions = finalConditions,
        columnBasisIx = finalBasisIx,
        columnCells = finalCells,
        columnModulators = finalModulators,
        columnHrfs = finalHrfs,
        columnScales = emptyScales
      ))

    require(blockIds0.forall(b => b >= 0 && b < samplingFrame.nBlocks), "blockIds out of range for samplingFrame")

    val globOns = samplingFrame.globalOnsets(onsets, blockIds0)
    var rowOffset = 0
    var columnOffset = 0
    var b = 0
    while b < samplingFrame.nBlocks do
      val blockLen = samplingFrame.blockLens(b)
      val grid = samplingFrame.samples(blocks = Seq(b), global = true).map(_.value)
      val eIdx = blockIds0.indices.filter(i => blockIds0(i) == b).toVector
      val onsB = eIdx.map(i => globOns(i).value)
      val durB = eIdx.map(i => durations0(i).value)

      var cond = 0
      columnOffset = 0
      while cond < nConds do
        val conditionHrf = hrfs0(cond)
        val ampB = eIdx.map(i => dm.data.data(i * dm.data.cols + cond))
        val reg = sharedRegressor(
          onsets = onsB,
          hrf = conditionHrf,
          duration = durB,
          amplitude = ampB,
          summate = summate
        )
        channels += ConvolutionChannel(b, rowOffset, grid.map(Seconds(_)), eIdx,
          Vector.tabulate(conditionHrf.nbasis)(basis => columnOffset + basis), reg)
        val ev = Regressor.evaluate(reg, grid, precision = precision.value)
        var basis = 0
        while basis < conditionHrf.nbasis do
          val outCol = columnOffset + basis
          var r = 0
          while r < blockLen do
            out((rowOffset + r) * totalCols + outCol) = ev.data(r * conditionHrf.nbasis + basis)
            r += 1
          basis += 1
        columnOffset += conditionHrf.nbasis
        cond += 1

      rowOffset += blockLen
      b += 1

    val columnScales = scaleColumnsInPlace(out, totalRows, totalCols, scaling)
    recorded(ConvolvedTerm(
      this,
      representative,
      Mat.unsafe(totalRows, totalCols, out),
      finalNames,
      columnConditions = finalConditions,
      columnBasisIx = finalBasisIx,
      columnCells = finalCells,
      columnModulators = finalModulators,
      columnHrfs = finalHrfs,
      columnScales = columnScales
    ))

  def convolvePerEvent(
      hrfs: Seq[Hrf],
      samplingFrame: SamplingFrame,
      precision: Seconds = 0.3.s,
      dropEmpty: Boolean = true,
      summate: Boolean = true,
      scaling: HrfColumnScaling = HrfColumnScaling.AsConvolved
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
    val finalConditions = columnConditionsFor(dm.conditionTags, nb)
    val finalBasisIx = columnBasisIxFor(nConds, nb)
    val provenance = conditionProvenance(dropEmpty)
    val finalCells = provenanceFor(provenance, p => Some(p.cell), nb)
    val finalModulators = provenanceFor(provenance, _.modulator, nb)

    val totalRows = samplingFrame.blockLens.sum
    val totalCols = nConds * nb
    val out = new Array[Double](totalRows * totalCols)
    val emptyScales = Vector.fill(totalCols)(HrfColumnScale.applied(scaling, 1.0))
    val channels = Vector.newBuilder[ConvolutionChannel]
    def recorded(value: ConvolvedTerm): ConvolvedTerm =
      value.copy(convolutionRecipe = Some(new ConvolutionRecipe(value, samplingFrame, precision, channels.result())))


    if nConds == 0 || totalCols == 0 || totalRows == 0 || n == 0 then
      return recorded(ConvolvedTerm(
        this,
        rep,
        Mat.unsafe(totalRows, totalCols, out),
        finalNames,
        columnConditions = finalConditions,
        columnBasisIx = finalBasisIx,
        columnCells = finalCells,
        columnModulators = finalModulators,
        columnScales = emptyScales
      ))

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
        channels += ConvolutionChannel(b, rowOffset, grid.map(Seconds(_)), eIdx,
          Vector.tabulate(nb)(basis => basis * nConds + cond), reg)
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

    val columnScales = scaleColumnsInPlace(out, totalRows, totalCols, scaling)
    recorded(ConvolvedTerm(
      this,
      rep,
      Mat.unsafe(totalRows, totalCols, out),
      finalNames,
      columnConditions = finalConditions,
      columnBasisIx = finalBasisIx,
      columnCells = finalCells,
      columnModulators = finalModulators,
      columnScales = columnScales
    ))

  private def columnConditionsFor(conditionTags: Vector[String], nbasis: Int): Vector[Option[String]] =
    if conditionTags.isEmpty || nbasis <= 0 then Vector.empty
    else
      Vector.tabulate(conditionTags.length * nbasis) { c =>
        Some(conditionTags(c % conditionTags.length))
      }

  private def columnBasisIxFor(nConditions: Int, nbasis: Int): Vector[Option[Int]] =
    if nConditions <= 0 || nbasis <= 0 then Vector.empty
    else
      Vector.tabulate(nConditions * nbasis) { c =>
        Some((c / nConditions) + 1)
      }

  private def provenanceFor[A](
      provenance: Vector[ConditionProvenance],
      select: ConditionProvenance => Option[A],
      nbasis: Int
  ): Vector[Option[A]] =
    if provenance.isEmpty || nbasis <= 0 then Vector.empty
    else Vector.tabulate(provenance.length * nbasis)(c => select(provenance(c % provenance.length)))

  private def keepConditionIndices(full: Mat, dropEmpty: Boolean): Vector[Int] =
    if !dropEmpty || full.cols == 0 then (0 until full.cols).toVector
    else
      val kept = Vector.newBuilder[Int]
      var c = 0
      while c < full.cols do
        var acc = 0.0
        var r = 0
        while r < full.rows do
          acc += math.abs(full.data(r * full.cols + c))
          r += 1
        if acc > 0.0 then kept += c
        c += 1
      kept.result()

  private def scaleColumnsInPlace(
      data: Array[Double],
      rows: Int,
      cols: Int,
      policy: HrfColumnScaling
  ): Vector[HrfColumnScale] =
    if policy == HrfColumnScaling.AsConvolved then
      Vector.fill(cols)(HrfColumnScale.identity)
    else
      val scales = Vector.newBuilder[HrfColumnScale]
      scales.sizeHint(cols)
      var c = 0
      while c < cols do
        var maxAbs = 0.0
        var r = 0
        while r < rows do
          val a = math.abs(data(r * cols + c))
          if a > maxAbs then maxAbs = a
          r += 1
        val divisor = if maxAbs > 0.0 then maxAbs else 1.0
        if divisor != 1.0 then
          r = 0
          while r < rows do
            val idx = r * cols + c
            data(idx) = data(idx) / divisor
            r += 1
        scales += HrfColumnScale.applied(policy, divisor)
        c += 1
      scales.result()

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
    Regressor.unsafeFromParts(
      onsets = ons,
      durations = durs,
      amplitudes = amps,
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
    val keep = ons.indices.filter(i => amps(i) != 0.0)
    val hrsF = keep.map(hrs).toVector
    val span = hrsF.map(_.span).maxOption.getOrElse(hrs.map(_.span).maxOption.getOrElse(Seconds(0.0)))
    Regressor.unsafeFromParts(
      onsets = ons,
      durations = durs,
      amplitudes = amps,
      hrf = HrfAssignment.PerEvent(hrs),
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

object EventTerm:

  /** Lower one checked phase without rebuilding or independently filtering its
    * factor/modulator event values. */
  def fromPhase(
      events: Vector[Event],
      phase: EventPhase,
      termTag: Option[String] = None
  ): Either[DesignError, EventTerm] =
    validated(
      events = events,
      onsets = phase.onsets,
      durations = phase.durations,
      blockIds = phase.blockIds,
      termTag = termTag
    ).map(_.copy(phaseId = Some(phase.id), eventProvenance = phase.provenance))
  def validated(
      events: Vector[Event],
      onsets: Vector[Seconds],
      durations: Vector[Seconds] = Vector.empty,
      blockIds: Vector[Int] = Vector.empty,
      termTag: Option[String] = None
  ): Either[DesignError, EventTerm] =
    EventSchedule.fromParts(onsets, durations, blockIds).flatMap(fromSchedule(events, _, termTag))

  def fromSchedule(
      events: Vector[Event],
      schedule: EventSchedule,
      termTag: Option[String] = None
  ): Either[DesignError, EventTerm] =
    termTag match
      case Some(tag) =>
        TermId(tag).flatMap(_ => construct(events, schedule, termTag))
      case None =>
        construct(events, schedule, termTag)

  private def construct(
      events: Vector[Event],
      schedule: EventSchedule,
      termTag: Option[String]
  ): Either[DesignError, EventTerm] =
    if events.exists(_.nEvents != schedule.size) then
      Left(DesignError.InvalidSchedule("all events must match schedule length"))
    else
      try
        Right(
          EventTerm(
            events = events,
            onsets = schedule.onsets,
            durations = schedule.durations,
            blockIds = schedule.blockIds,
            termTag = termTag
          )
        )
      catch
        case NonFatal(t) => Left(DesignError.fromThrowable(t))
