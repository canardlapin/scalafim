package scalafim.fmri.design.formula

import scalafim.fmri.design.Names
import scalafim.fmri.design.contrast.ContrastSpec
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.basis.{BasisRegistry, ParametricBasis}
import scalafim.fmri.design.event.*
import scalafim.fmri.design.hrf.HrfFun
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.HrfCombinators.*
import scalafim.fmri.hrf.design.SamplingFrame

import scala.collection.immutable.VectorMap

object EventModelBuilder:

  final case class TableEnv(eventData: DataTable, other: Map[String, DataTable] = Map.empty):
    def resolve(name: Option[String]): DataTable =
      name match
        case None           => eventData
        case Some(tableKey) => other.getOrElse(tableKey, throw new IllegalArgumentException(s"Unknown data table: '$tableKey'"))

  /** Build an [[scalafim.fmri.design.event.EventModel]] using an R-ish block formula such as `"~1"` or `"~run"`.
    *
    * Block ids are canonicalized to 0-based contiguous integers and must be non-decreasing.
    */
  def buildWithBlockFormula(
      formula: String,
      data: DataTable,
      samplingFrame: SamplingFrame,
      block: String,
      durations: Seq[Double] = Seq(0.0),
      tables: Map[String, DataTable] = Map.empty,
      defaultHrf: Hrf = Hrfs.SPMG1,
      precision: Seconds = 0.3.s,
      dropEmpty: Boolean = true,
      summate: Boolean = true,
      hrfFuns: Map[String, HrfFun] = Map.empty,
      contrastSets: Map[String, ContrastSpec.ContrastSet] = Map.empty,
      strict: Boolean = false,
      basisRegistry: BasisRegistry = BasisRegistry.default
  ): EventModel =
    val blockIds = parseBlockIds(block, data)
    build(
      formula = formula,
      data = data,
      samplingFrame = samplingFrame,
      blockIds = blockIds,
      durations = durations,
      tables = tables,
      defaultHrf = defaultHrf,
      precision = precision,
      dropEmpty = dropEmpty,
      summate = summate,
      hrfFuns = hrfFuns,
      contrastSets = contrastSets,
      basisRegistry = basisRegistry,
      strict = strict
    )

  def build(
      formula: String,
      data: DataTable,
      samplingFrame: SamplingFrame,
      blockIds: Seq[Int],
      durations: Seq[Double] = Seq(0.0),
      tables: Map[String, DataTable] = Map.empty,
      defaultHrf: Hrf = Hrfs.SPMG1,
      precision: Seconds = 0.3.s,
      dropEmpty: Boolean = true,
      summate: Boolean = true,
      hrfFuns: Map[String, HrfFun] = Map.empty,
      contrastSets: Map[String, ContrastSpec.ContrastSet] = Map.empty,
      strict: Boolean = false,
      basisRegistry: BasisRegistry = BasisRegistry.default
  ): EventModel =
    val parsed = FormulaParser.parse(formula)
    val env = TableEnv(eventData = data, other = tables)
    build(
      parsed,
      env,
      samplingFrame,
      blockIds = blockIds,
      durations = durations,
      defaultHrf = defaultHrf,
      precision = precision,
      dropEmpty = dropEmpty,
      summate = summate,
      hrfFuns = hrfFuns,
      contrastSets = contrastSets,
      basisRegistry = basisRegistry,
      strict = strict
    )

  def build(
      formula: ModelFormula,
      env: TableEnv,
      samplingFrame: SamplingFrame,
      blockIds: Seq[Int],
      durations: Seq[Double],
      defaultHrf: Hrf,
      precision: Seconds,
      dropEmpty: Boolean,
      summate: Boolean,
      hrfFuns: Map[String, HrfFun],
      contrastSets: Map[String, ContrastSpec.ContrastSet],
      strict: Boolean,
      basisRegistry: BasisRegistry
  ): EventModel =
    val onsetVals = env.eventData.doubles(formula.onset)
    val nEvents = onsetVals.length
    require(blockIds.length == nEvents, s"`blockIds` must have length $nEvents, not ${blockIds.length}")

    val durVals =
      if durations.length == nEvents then durations.toVector
      else if durations.length == 1 then Vector.fill(nEvents)(durations.head)
      else throw new IllegalArgumentException(s"`durations` must have length 1 or $nEvents, not ${durations.length}")

    val defaultOnsets = onsetVals.map(Seconds(_))
    val defaultDurs = durVals.map(Seconds(_))
    val blockIds0 = blockIds.toVector

    val terms = Vector.newBuilder[EventModelTerm]
    val contrastRefs = Vector.newBuilder[Option[ArgValue]]
    val diagnostics = Vector.newBuilder[EventModelDiagnostic]

    formula.terms.foreach {
      case h: HrfCall =>
        val termTag = inferTermTag(h, basisRegistry)

        val events0 = h.vars.map(v => toEvent(env.eventData, v))
        val termOnsets = h.onsets.fold(defaultOnsets)(ref => resolveSeconds(ref, env.eventData, nEvents, argName = "onsets"))
        val termDurs = h.durations.fold(defaultDurs)(ref => resolveSeconds(ref, env.eventData, nEvents, argName = "durations"))

        val subsetMask =
          h.subset match
            case None => Vector.fill(nEvents)(true)
            case Some(expr) =>
              val keep = evalSubset(expr, env.eventData)
              require(keep.length == nEvents, s"subset mask has length ${keep.length} but expected $nEvents")
              keep

        val originalSub =
          if subsetMask.forall(identity) then env.eventData else env.eventData.filterRows(subsetMask)

        val (events, onsetsS, dursS, blockIdsS) =
          if subsetMask.forall(identity) then (events0, termOnsets, termDurs, blockIds0)
          else subsetTerm(events0, termOnsets, termDurs, blockIds0, subsetMask)

        val (eventsClean, nonFiniteDiagnostics) = sanitizeContinuousEvents(events, termTag)
        val term = EventTerm(
          events = eventsClean,
          onsets = onsetsS,
          durations = dursS,
          blockIds = blockIdsS,
          termTag = termTag
        )

        val termDiagnostics = nonFiniteDiagnostics ++ diagnoseTerm(term, samplingFrame)
        diagnostics ++= termDiagnostics
        raiseStrictDiagnostics(termDiagnostics, strict)

        val summate0 = h.summate.getOrElse(summate)
        val normalize0 = h.normalize.getOrElse(false)
        val conv =
          (h.hrfFun, term.onsets.nonEmpty) match
            case (Some(ref), true) =>
              val eventData = buildEventDataForGenerator(term, originalSub)
              val hsel = resolveHrfFun(ref, eventData, hrfFuns = hrfFuns, termTag = termTag)
              hsel match
                case Left(shared) =>
                  term.convolve(shared, samplingFrame, precision = precision, dropEmpty = dropEmpty, summate = summate0, normalize = normalize0)
                case Right(perEvent) =>
                  term.convolvePerEvent(perEvent, samplingFrame, precision = precision, dropEmpty = dropEmpty, summate = summate0, normalize = normalize0)
            case _ =>
              val hrf0 = resolveHrf(h, defaultHrf)
              term.convolve(hrf0, samplingFrame, precision = precision, dropEmpty = dropEmpty, summate = summate0, normalize = normalize0)

        terms += conv
        contrastRefs += h.contrasts
      case t: TrialwiseCall =>
        val label0 = t.label.getOrElse("trial")
        val termTag = Names.sanitize(label0, allowDot = false)
        val termDurs = t.durations.fold(defaultDurs)(ref => resolveSeconds(ref, env.eventData, nEvents, argName = "durations"))

        val trialEvent = Event.factor(trialLevels(nEvents), name = "trial")
        val term = EventTerm(
          events = Vector(trialEvent),
          onsets = defaultOnsets,
          durations = termDurs,
          blockIds = blockIds0,
          termTag = Some(termTag)
        )

        val termDiagnostics = diagnoseTerm(term, samplingFrame)
        diagnostics ++= termDiagnostics
        raiseStrictDiagnostics(termDiagnostics, strict)

        val basisName = t.basis.getOrElse("spmg1")
        val hrf0 = resolveHrfBasis(basisName, nbasis = t.nbasis, lag = t.lag)
        val conv0 = term.convolve(
          hrf0,
          samplingFrame,
          precision = precision,
          dropEmpty = dropEmpty,
          summate = summate,
          normalize = t.normalize.getOrElse(false)
        )
        val conv = conv0.copy(
          role = EventTermRole.Trialwise,
          columnRoles = Vector.fill(conv0.columnNames.length)(EventTermColumnRole.Trial)
        )

        val out = if t.addSum.getOrElse(false) then addMeanColumn(conv, label = label0) else conv
        terms += out
        contrastRefs += None

      case c: CovariateCall =>
        val table = env.resolve(c.data)
        val vars = c.vars.map {
          case ArgValue.Ident(v) => v
          case other             => throw new IllegalArgumentException(s"covariate vars must be identifiers, found $other")
        }
        val spec = CovariateSpec(vars = vars, data = table, id = c.id, prefix = c.prefix)
        terms += spec.construct(samplingFrame)
        contrastRefs += None
    }

    val model0 = EventModel.buildTerms(terms.result(), samplingFrame)
    val refs = contrastRefs.result()
    require(refs.length == model0.terms.length, "internal: contrastRefs length mismatch")
    model0.copy(
      contrastSetsByTerm = attachContrastSets(model0, refs, contrastSets),
      diagnostics = diagnostics.result()
    )

  private def attachContrastSets(
      model: EventModel,
      refs: Vector[Option[ArgValue]],
      available: Map[String, ContrastSpec.ContrastSet]
  ): VectorMap[String, ContrastSpec.ContrastSet] =
    if refs.isEmpty then VectorMap.empty
    else
      val out = VectorMap.newBuilder[String, ContrastSpec.ContrastSet]
      var i = 0
      while i < refs.length do
        refs(i) match
          case None => ()
          case Some(ref) =>
            val (termKey, term) = model.terms(i)
            term match
              case _: ConvolvedTerm =>
                val key = contrastSetKey(ref, termKey)
                val set = available.getOrElse(
                  key,
                  throw new IllegalArgumentException(
                    s"Unknown contrast set '$key' for term '$termKey' (known: ${available.keys.toVector.sorted.mkString(", ")})"
                  )
                )
                out += (termKey -> set)
              case other =>
                throw new IllegalArgumentException(s"Term '$termKey' does not support contrasts (found $other)")
        i += 1
      out.result()

  private def contrastSetKey(ref: ArgValue, termKey: String): String =
    ref match
      case ArgValue.Ident(v) => v
      case ArgValue.Str(v)   => v
      case other =>
        throw new IllegalArgumentException(s"contrasts for term '$termKey' must be a string/identifier, found $other")

  private def resolveSeconds(ref: ArgValue, data: DataTable, nEvents: Int, argName: String): Vector[Seconds] =
    resolveNumericVector(ref, data, nEvents, argName).map(Seconds(_))

  private def resolveNumericVector(ref: ArgValue, data: DataTable, nEvents: Int, argName: String): Vector[Double] =
    val values =
      ref match
        case ArgValue.Num(v) =>
          require(v.isFinite, s"$argName scalar must be finite")
          Vector.fill(nEvents)(v)
        case ArgValue.Ident(name) =>
          data.doubles(name)
        case ArgValue.Str(name) =>
          data.doubles(name)
        case other =>
          throw new IllegalArgumentException(s"$argName must be a column reference or numeric scalar, found $other")

    require(values.length == nEvents, s"$argName has length ${values.length} but expected $nEvents")
    require(values.forall(_.isFinite), s"$argName must be finite")
    if argName == "durations" then require(values.forall(_ >= 0.0), "durations must be non-negative")
    values

  private def diagnoseTerm(term: EventTerm, samplingFrame: SamplingFrame): Vector[EventModelDiagnostic] =
    degenerateModulatorDiagnostics(term) ++ onsetBoundDiagnostics(term, samplingFrame)

  private def sanitizeContinuousEvents(
      events: Vector[Event],
      termTag: Option[String]
  ): (Vector[Event], Vector[EventModelDiagnostic]) =
    val termLabel = termTag.getOrElse("term")
    val diagnostics = Vector.newBuilder[EventModelDiagnostic]

    val cleaned = events.map {
      case e: ContinuousEvent =>
        val m = e.value
        var out: Array[Double] = null

        var c = 0
        while c < m.cols do
          var hasNonFinite = false
          var r = 0
          while r < m.rows do
            val idx = r * m.cols + c
            val v = m.data(idx)
            if !v.isFinite then
              hasNonFinite = true
              if out == null then out = m.data.clone()
              out(idx) = 0.0
            r += 1

          if hasNonFinite then
            val colLabel =
              if m.cols == 1 then e.varName
              else e.columnTags.lift(c).getOrElse(s"${e.varName}_${c + 1}")
            diagnostics += EventModelDiagnostic(
              EventModelDiagnosticKind.NonFiniteModulator,
              termLabel,
              s"NA or non-finite values detected in continuous modulator '$colLabel' in term '$termLabel'; replacing them with 0.0"
            )
          c += 1

        if out == null then e
        else e.copy(value = scalafim.fmri.hrf.linalg.Mat.unsafe(m.rows, m.cols, out))

      case other => other
    }

    (cleaned, diagnostics.result())

  private def degenerateModulatorDiagnostics(term: EventTerm, tol: Double = 1e-8): Vector[EventModelDiagnostic] =
    val termLabel = term.termTag.getOrElse("term")
    val out = Vector.newBuilder[EventModelDiagnostic]

    term.events.foreach {
      case e: ContinuousEvent =>
        val m = e.value
        var c = 0
        while c < m.cols do
          val colLabel =
            if m.cols == 1 then e.varName
            else e.columnTags.lift(c).getOrElse(s"${e.varName}_${c + 1}")

          var nFinite = 0
          var min = Double.PositiveInfinity
          var max = Double.NegativeInfinity
          var maxAbs = 0.0
          var allZero = true
          var r = 0
          while r < m.rows do
            val v = m.data(r * m.cols + c)
            if v.isFinite then
              nFinite += 1
              if v < min then min = v
              if v > max then max = v
              val a = math.abs(v)
              if a > maxAbs then maxAbs = a
              if a > tol then allZero = false
            r += 1

          val msg =
            if nFinite == 0 then Some(s"continuous modulator '$colLabel' in term '$termLabel' has no finite values")
            else if allZero then Some(s"continuous modulator '$colLabel' in term '$termLabel' is all zero")
            else if (max - min) <= tol * math.max(1.0, maxAbs) then
              Some(s"continuous modulator '$colLabel' in term '$termLabel' has zero variance")
            else None

          msg.foreach { m =>
            out += EventModelDiagnostic(EventModelDiagnosticKind.DegenerateModulator, termLabel, m)
          }
          c += 1

      case _ => ()
    }

    out.result()

  private def onsetBoundDiagnostics(term: EventTerm, samplingFrame: SamplingFrame): Vector[EventModelDiagnostic] =
    val termLabel = term.termTag.getOrElse("term")
    val out = Vector.newBuilder[EventModelDiagnostic]

    var i = 0
    while i < term.onsets.length do
      val block = term.blockIds0(i)
      val onset = term.onsets(i).value
      val duration = term.durations0(i).value

      def add(message: String): Unit =
        out += EventModelDiagnostic(EventModelDiagnosticKind.OnsetOutOfBounds, termLabel, message)

      if block < 0 || block >= samplingFrame.nBlocks then
        add(s"event ${i + 1} in term '$termLabel' references block index $block, but samplingFrame has ${samplingFrame.nBlocks} block(s)")
      else
        val runEnd = samplingFrame.blockLens(block).toDouble * samplingFrame.tr(block).value
        if !onset.isFinite then add(s"event ${i + 1} in term '$termLabel' has a non-finite onset")
        else
          if onset < 0.0 then add(s"event ${i + 1} in term '$termLabel' has negative onset $onset in block index $block")
          if onset >= runEnd then add(s"event ${i + 1} in term '$termLabel' starts at $onset, outside block index $block ending at $runEnd")
          if duration.isFinite && duration > 0.0 && onset + duration > runEnd then
            add(s"event ${i + 1} in term '$termLabel' ends at ${onset + duration}, past block index $block ending at $runEnd")
      i += 1

    out.result()

  private def raiseStrictDiagnostics(diagnostics: Vector[EventModelDiagnostic], strict: Boolean): Unit =
    if strict then
      val blocking = diagnostics.filter(_.kind == EventModelDiagnosticKind.OnsetOutOfBounds)
      if blocking.nonEmpty then
        throw new IllegalArgumentException(blocking.map(_.message).mkString("; "))

  private def buildEventDataForGenerator(term: EventTerm, original: DataTable): DataTable =
    val n = term.onsets.length
    require(original.nrows == n, s"internal: original data rows (${original.nrows}) != term events ($n)")

    val cols = Vector.newBuilder[(String, Column)]
    val seen = scala.collection.mutable.HashSet.empty[String]

    def add(name: String, col: Column): Unit =
      if !seen.contains(name) then
        require(col.size == n, s"internal: generator column '$name' has length ${col.size} != $n")
        cols += (name -> col)
        seen += name

    add("onset", Column.Doubles(term.onsets.map(_.value)))
    add("duration", Column.Doubles(term.durations0.map(_.value)))
    add("blockid", Column.Ints(term.blockIds0))

    term.events.foreach {
      case c: CategoricalEvent =>
        val vals = c.codes.map { code =>
          require(code >= 0 && code < c.levels.length, s"internal: categorical code out of range for ${c.varName}")
          c.levels(code)
        }
        add(c.varName, Column.Strings(vals))
      case e: ContinuousEvent =>
        val m = e.value
        if m.cols == 1 then
          add(e.varName, Column.Doubles(matCol(m, 0)))
        else
          var j = 0
          while j < m.cols do
            add(s"${e.varName}_${j + 1}", Column.Doubles(matCol(m, j)))
            j += 1
    }

    original.columns.foreach { case (k, col) =>
      if !seen.contains(k) then add(k, col)
    }

    DataTable(n, cols.result())

  private def matCol(m: scalafim.fmri.hrf.linalg.Mat, c: Int): Vector[Double] =
    val out = new Array[Double](m.rows)
    var r = 0
    while r < m.rows do
      out(r) = m.data(r * m.cols + c)
      r += 1
    out.toVector

  private def resolveHrfFun(
      ref: ArgValue,
      eventData: DataTable,
      hrfFuns: Map[String, HrfFun],
      termTag: Option[String]
  ): Either[Hrf, Vector[Hrf]] =
    val termLabel = termTag.getOrElse("unknown")
    val nEvents = eventData.nrows

    def validateHrfs(hrfs0: Seq[Hrf]): Either[Hrf, Vector[Hrf]] =
      val hrfs = hrfs0.toVector
      if hrfs.isEmpty then throw new IllegalArgumentException(s"hrf_fun for term '$termLabel' returned 0 HRFs but $nEvents events exist")
      else if hrfs.length == 1 then Left(hrfs.head)
      else
        require(hrfs.length == nEvents, s"hrf_fun for term '$termLabel' returned ${hrfs.length} HRFs but $nEvents events exist")
        val nb = hrfs.head.nbasis
        require(hrfs.forall(_.nbasis == nb), s"All HRFs from hrf_fun for term '$termLabel' must have the same nbasis")
        Right(hrfs)

    ref match
      case ArgValue.Ident(key) if hrfFuns.contains(key) =>
        val res = hrfFuns(key)(eventData)
        res match
          case h: Hrf      => Left(h)
          case hs: Seq[?]  => validateHrfs(hs.asInstanceOf[Seq[Hrf]])
      case ArgValue.Ident(key) =>
        validateHrfs(eventData.hrfs(key))
      case ArgValue.Str(colName) =>
        validateHrfs(eventData.hrfs(colName))
      case other =>
        throw new IllegalArgumentException(s"hrf_fun for term '$termLabel' must be a string/identifier, found $other")

  private enum ScalarVec:
    case Num(values: Vector[Double])
    case Str(values: Vector[String])
    case Bool(values: Vector[Boolean])

  private def evalSubset(expr: ArgValue, data: DataTable): Vector[Boolean] =
    val n = data.nrows

    def err(msg: String): Nothing = throw new IllegalArgumentException(s"subset: $msg")

    def boolVec(e: ArgValue): Vector[Boolean] =
      e match
        case ArgValue.Bool(v) => Vector.fill(n)(v)
        case ArgValue.Ident(name) =>
          data.column(name) match
            case Column.Bools(v) =>
              require(v.length == n, "internal: bool column length mismatch")
              v
            case other => err(s"identifier '$name' is not a boolean column (found $other)")
        case ArgValue.Call("!", args) =>
          val a = requireArity(args, 1, op = "!")
          boolVec(a).map(b => !b)
        case ArgValue.Call("&", args) =>
          val (l, r) = requireBinary(args, op = "&")
          val lv = boolVec(l)
          val rv = boolVec(r)
          lv.indices.map(i => lv(i) && rv(i)).toVector
        case ArgValue.Call("|", args) =>
          val (l, r) = requireBinary(args, op = "|")
          val lv = boolVec(l)
          val rv = boolVec(r)
          lv.indices.map(i => lv(i) || rv(i)).toVector
        case ArgValue.Call(op @ ("==" | "!=" | "<" | "<=" | ">" | ">="), args) =>
          val (l, r) = requireBinary(args, op = op)
          compare(op, scalarVec(l), scalarVec(r))
        case other =>
          err(s"unsupported subset expression: $other")

    def scalarVec(e: ArgValue): ScalarVec =
      e match
        case ArgValue.Num(v)  => ScalarVec.Num(Vector.fill(n)(v))
        case ArgValue.Str(v)  => ScalarVec.Str(Vector.fill(n)(v))
        case ArgValue.Bool(v) => ScalarVec.Bool(Vector.fill(n)(v))
        case ArgValue.Ident(name) =>
          data.column(name) match
            case Column.Doubles(v) => ScalarVec.Num(v)
            case Column.Ints(v)    => ScalarVec.Num(v.map(_.toDouble))
            case Column.Strings(v) => ScalarVec.Str(v)
            case Column.Bools(v)   => ScalarVec.Bool(v)
            case Column.DoubleLists(_) => err(s"column '$name' is a list column and cannot be used in subset comparisons")
            case Column.Hrfs(_)    => err(s"column '$name' is an HRF list and cannot be used in subset comparisons")
        case other =>
          err(s"expected scalar in comparison but found $other")

    def compare(op: String, left: ScalarVec, right: ScalarVec): Vector[Boolean] =
      (left, right) match
        case (ScalarVec.Num(a), ScalarVec.Num(b)) =>
          require(a.length == n && b.length == n, "internal: numeric vector length mismatch")
          op match
            case "==" => a.indices.map(i => a(i) == b(i)).toVector
            case "!=" => a.indices.map(i => a(i) != b(i)).toVector
            case "<"  => a.indices.map(i => a(i) < b(i)).toVector
            case "<=" => a.indices.map(i => a(i) <= b(i)).toVector
            case ">"  => a.indices.map(i => a(i) > b(i)).toVector
            case ">=" => a.indices.map(i => a(i) >= b(i)).toVector
            case _    => err(s"unknown numeric comparator '$op'")
        case (ScalarVec.Str(a), ScalarVec.Str(b)) =>
          require(a.length == n && b.length == n, "internal: string vector length mismatch")
          op match
            case "==" => a.indices.map(i => a(i) == b(i)).toVector
            case "!=" => a.indices.map(i => a(i) != b(i)).toVector
            case _    => err(s"string columns only support == and != (got '$op')")
        case (ScalarVec.Bool(a), ScalarVec.Bool(b)) =>
          require(a.length == n && b.length == n, "internal: bool vector length mismatch")
          op match
            case "==" => a.indices.map(i => a(i) == b(i)).toVector
            case "!=" => a.indices.map(i => a(i) != b(i)).toVector
            case _    => err(s"boolean columns only support == and != (got '$op')")
        case (a, b) =>
          err(s"type mismatch in comparison: $a vs $b")

    boolVec(expr)

  private def requireArity(args: Vector[Arg], n: Int, op: String): ArgValue =
    if args.length != n then throw new IllegalArgumentException(s"subset: '$op' expects $n argument(s), got ${args.length}")
    args.head.value

  private def requireBinary(args: Vector[Arg], op: String): (ArgValue, ArgValue) =
    if args.length != 2 then throw new IllegalArgumentException(s"subset: '$op' expects 2 arguments, got ${args.length}")
    (args(0).value, args(1).value)

  private def subsetTerm(
      events: Vector[Event],
      onsets: Vector[Seconds],
      durs: Vector[Seconds],
      blockIds: Vector[Int],
      keep: Vector[Boolean]
  ): (Vector[Event], Vector[Seconds], Vector[Seconds], Vector[Int]) =
    require(onsets.length == keep.length, "subset: onsets/mask length mismatch")
    require(durs.length == keep.length, "subset: durations/mask length mismatch")
    require(blockIds.length == keep.length, "subset: blockIds/mask length mismatch")
    val idx = keep.iterator.zipWithIndex.collect { case (true, i) => i }.toVector

    val onS = idx.map(onsets)
    val duS = idx.map(durs)
    val blS = idx.map(blockIds)

    val evS = events.map(ev => subsetEvent(ev, idx, keep))
    (evS, onS, duS, blS)

  private def subsetEvent(ev: Event, idx: Vector[Int], keep: Vector[Boolean]): Event =
    ev match
      case c: CategoricalEvent =>
        c.copy(codes = idx.map(c.codes))
      case e: ContinuousEvent =>
        e.basis match
          case Some(b) =>
            val b2 = b.subset(keep)
            e.copy(value = b2.y, columnTags = b2.columns, basis = Some(b2))
          case None =>
            e.copy(value = subsetRows(e.value, idx))

  private def subsetRows(m: scalafim.fmri.hrf.linalg.Mat, idx: Vector[Int]): scalafim.fmri.hrf.linalg.Mat =
    if idx.isEmpty then scalafim.fmri.hrf.linalg.Mat.zeros(0, m.cols)
    else
      val out = new Array[Double](idx.length * m.cols)
      var r = 0
      while r < idx.length do
        val src = idx(r) * m.cols
        val dst = r * m.cols
        System.arraycopy(m.data, src, out, dst, m.cols)
        r += 1
      scalafim.fmri.hrf.linalg.Mat.unsafe(idx.length, m.cols, out)

  private def parseBlockIds(block: String, data: DataTable): Vector[Int] =
    val rhs0 =
      val s = block.trim
      if s.isEmpty then throw new IllegalArgumentException("block formula must be non-empty")
      if s.startsWith("~") then s.drop(1).trim else s

    if rhs0 == "1" || rhs0 == "1.0" then Vector.fill(data.nrows)(0)
    else
      val raw = data.column(rhs0)
      raw match
        case Column.Doubles(v) =>
          requireNonDecreasing(v, name = rhs0)
          canonicalize(v)
        case Column.Ints(v) =>
          requireNonDecreasing(v.map(_.toDouble), name = rhs0)
          canonicalize(v)
        case Column.Strings(v) =>
          requireBlocksContiguous(v, name = rhs0)
          canonicalize(v)
        case Column.Bools(v) =>
          requireBlocksContiguous(v.map(_.toString), name = rhs0)
          canonicalize(v.map(_.toString))
        case Column.DoubleLists(_) =>
          throw new IllegalArgumentException(s"block formula '$rhs0' references a list column")
        case Column.Hrfs(_) =>
          throw new IllegalArgumentException(s"block formula '$rhs0' references an HRF column")

  private def requireNonDecreasing(xs: Vector[Double], name: String): Unit =
    var i = 1
    while i < xs.length do
      if xs(i) < xs(i - 1) then throw new IllegalArgumentException(s"'blockIds' must be non-decreasing (from '$name')")
      i += 1

  private def requireBlocksContiguous(xs: Vector[String], name: String): Unit =
    val codes = canonicalize(xs)
    var i = 1
    while i < codes.length do
      if codes(i) < codes(i - 1) then throw new IllegalArgumentException(s"'blockIds' must be non-decreasing (from '$name')")
      i += 1

  private def canonicalize[A](xs: Vector[A]): Vector[Int] =
    val map = scala.collection.mutable.LinkedHashMap.empty[A, Int]
    val out = new Array[Int](xs.length)
    var i = 0
    while i < xs.length do
      val x = xs(i)
      val k = map.getOrElseUpdate(x, map.size)
      out(i) = k
      i += 1
    out.toVector

  private def toEvent(data: DataTable, expr: ArgValue): Event =
    expr match
      case ArgValue.Ident(name) =>
        data.column(name) match
          case Column.Strings(v) => Event.factor(v, name)
          case Column.Doubles(v) => Event.variable(v, name)
          case Column.Ints(v)    => Event.variable(v.map(_.toDouble), name)
          case Column.Bools(v)   => Event.factor(v.map(_.toString), name)
          case Column.DoubleLists(_) => throw new IllegalArgumentException(s"Column '$name' is a list column and cannot be used as an event variable")
          case Column.Hrfs(_)    => throw new IllegalArgumentException(s"Column '$name' is an HRF list and cannot be used as an event variable")
      case ArgValue.Call(fun, args) =>
        evalBasisCall(data, fun, args)
      case other =>
        throw new IllegalArgumentException(s"Unsupported event expression: $other")

  private def evalBasisCall(data: DataTable, funName: String, args: Vector[Arg]): Event =
    val fun = funName.trim.toLowerCase
    fun match
      case "scale" =>
        val (xVar, xs) = requireNumeric1(data, args, "Scale")
        val basis = ParametricBasis.Scale.fit(xs, argName = xVar)
        Event.basis(basis)
      case "standardized" =>
        val (xVar, xs) = requireNumeric1(data, args, "Standardized")
        val basis = ParametricBasis.Standardized.fit(xs, argName = xVar)
        Event.basis(basis)
      case "robustscale" =>
        val (xVar, xs) = requireNumeric1(data, args, "RobustScale")
        val basis = ParametricBasis.RobustScale.fit(xs, argName = xVar)
        Event.basis(basis)
      case "poly" =>
        val (xVar, xs) = requireNumeric1(data, args, "Poly")
        val degree = requireIntArg(args, "degree", fallbackPos = 1, ctx = "Poly")
        val basis = ParametricBasis.Poly.fit(xs, degree = degree, argName = xVar)
        Event.basis(basis)
      case "bspline" =>
        val (xVar, xs) = requireNumeric1(data, args, "BSpline")
        val degree = requireIntArg(args, "degree", fallbackPos = 1, ctx = "BSpline")
        val basis = ParametricBasis.BSpline.fit(xs, degree = degree, argName = xVar)
        Event.basis(basis)
      case "scalewithin" =>
        val xVar = requireIdentArg(args, pos = 0, ctx = "ScaleWithin")
        val gVar = requireIdentArg(args, pos = 1, ctx = "ScaleWithin")
        val xs = data.doubles(xVar)
        val gs = toFactorStrings(data, gVar)
        val basis = ParametricBasis.ScaleWithin.fit(xs, gs, argName = xVar, groupName = gVar)
        Event.basis(basis)
      case other =>
        throw new IllegalArgumentException(s"Unknown basis call '$funName' in formula")

  private def requireNumeric1(data: DataTable, args: Vector[Arg], ctx: String): (String, Vector[Double]) =
    val xVar = requireIdentArg(args, pos = 0, ctx = ctx)
    (xVar, data.doubles(xVar))

  private def requireIdentArg(args: Vector[Arg], pos: Int, ctx: String): String =
    args.lift(pos) match
      case Some(Arg(None, ArgValue.Ident(v))) => v
      case Some(a)                            => throw new IllegalArgumentException(s"$ctx positional arg ${pos + 1} must be an identifier, found $a")
      case None                               => throw new IllegalArgumentException(s"$ctx requires at least ${pos + 1} positional args")

  private def requireIntArg(args: Vector[Arg], name: String, fallbackPos: Int, ctx: String): Int =
    val named = args.collectFirst { case Arg(Some(nm), ArgValue.Num(v)) if nm == name => v }
    val pos = args.lift(fallbackPos).collect { case Arg(None, ArgValue.Num(v)) => v }
    val v0 = named.orElse(pos).getOrElse(throw new IllegalArgumentException(s"$ctx requires '$name' (e.g. $name=3)"))
    if !v0.isFinite || !v0.isValidInt || v0 != v0.toInt.toDouble then
      throw new IllegalArgumentException(s"$ctx '$name' must be an integer, got $v0")
    v0.toInt

  private def toFactorStrings(data: DataTable, name: String): Vector[String] =
    data.column(name) match
      case Column.Strings(v) => v
      case Column.Ints(v)    => v.map(_.toString)
      case Column.Doubles(v) => v.map(_.toString)
      case Column.Bools(v)   => v.map(_.toString)
      case Column.DoubleLists(_) => throw new IllegalArgumentException(s"Column '$name' is a list column and cannot be used as a factor")
      case Column.Hrfs(v)    => v.map(_.name)

  private def inferTermTag(call: HrfCall, basisRegistry: BasisRegistry): Option[String] =
    call.id.map(id => Names.sanitize(id, allowDot = false)).orElse(call.prefix.map(p => Names.sanitize(p, allowDot = false))).orElse {
      if call.vars.length == 1 then
        call.vars.head match
          case ArgValue.Call(fun, args) =>
            val f = fun.trim.toLowerCase
            if f == "ident" then None
            else parametricBasisTag(f, args, basisRegistry).orElse(Some(Names.sanitize(exprLabel(call.vars.head), allowDot = false)))
          case ArgValue.Ident(v) =>
            Some(Names.sanitize(v, allowDot = false))
          case other =>
            Some(Names.sanitize(exprLabel(other), allowDot = false))
      else
        Some(Names.sanitize(call.vars.map(exprLabel).mkString("_"), allowDot = false))
    }

  private def parametricBasisTag(funLower: String, args: Vector[Arg], basisRegistry: BasisRegistry): Option[String] =
    basisRegistry.getFormulaEntry(funLower).flatMap(_.prefix).map { p =>
      val xVar = requireIdentArg(args, pos = 0, ctx = funLower)
      s"${p}_${Names.sanitize(xVar, allowDot = false)}"
    }

  private def exprLabel(v: ArgValue): String =
    v match
      case ArgValue.Ident(x) => x
      case ArgValue.Str(x)   => "\"" + x + "\""
      case ArgValue.Num(x)   => x.toString
      case ArgValue.Bool(x)  => x.toString
      case ArgValue.Call(fun, args) =>
        val as = args.map { a =>
          val pref = a.name.map(_ + "=").getOrElse("")
          pref + exprLabel(a.value)
        }.mkString(",")
        s"$fun($as)"

  private def resolveHrf(call: HrfCall, defaultHrf: Hrf): Hrf =
    val base: Hrf =
      call.basis match
        case None => defaultHrf
        case Some(basisName0) =>
          val basisName = basisName0.trim.toLowerCase
          basisName match
            case "spmg1"    => Hrfs.SPMG1
            case "spmg2"    => Hrfs.SPMG2
            case "spmg3"    => Hrfs.SPMG3
            case "gamma"    => Hrfs.Gamma
            case "gaussian" => Hrfs.Gaussian
            case "fir"      => Hrfs.fir(nBasis = call.nbasis.getOrElse(12))
            case "bspline"  => Hrfs.bspline(nBasis = call.nbasis.getOrElse(5))
            case "tent"     => Hrfs.tent(nBasis = call.nbasis.getOrElse(5))
            case "fourier"  => Hrfs.fourier(nBasis = call.nbasis.getOrElse(5))
            case other      => throw new IllegalArgumentException(s"Unknown HRF basis: '$other'")

    call.lag match
      case None => base
      case Some(l) =>
        require(l.isFinite, "`lag` must be finite")
        base.lag(Seconds(l))

  private def resolveHrfBasis(basis: String, nbasis: Option[Int], lag: Option[Double]): Hrf =
    val call = HrfCall(vars = Vector(ArgValue.Ident("x")), basis = Some(basis), lag = lag, nbasis = nbasis)
    resolveHrf(call, defaultHrf = Hrfs.SPMG1)

  private def trialLevels(n: Int): Vector[String] =
    if n <= 0 then Vector.empty
    else
      val width = n.toString.length
      val fmt = s"%0${width}d"
      (1 to n).iterator.map(i => fmt.format(i)).toVector

  private def addMeanColumn(term: ConvolvedTerm, label: String): ConvolvedTerm =
    val m = term.data
    if m.cols == 0 || m.rows == 0 then term
    else
      val meanName = Names.sanitize(s"${label}_mean", allowDot = true)
      val out = new Array[Double](m.rows * (m.cols + 1))
      var r = 0
      while r < m.rows do
        System.arraycopy(m.data, r * m.cols, out, r * (m.cols + 1), m.cols)
        var s = 0.0
        var c = 0
        while c < m.cols do
          s += m.data(r * m.cols + c)
          c += 1
        out(r * (m.cols + 1) + m.cols) = s / m.cols.toDouble
        r += 1
      term.copy(
        data = scalafim.fmri.hrf.linalg.Mat.unsafe(m.rows, m.cols + 1, out),
        columnNames = term.columnNames :+ meanName,
        columnRoles = term.resolvedColumnRoles :+ EventTermColumnRole.TrialAggregate
      )
