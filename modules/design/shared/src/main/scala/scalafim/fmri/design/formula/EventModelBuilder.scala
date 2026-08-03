package scalafim.fmri.design.formula

import scalafim.fmri.design.{ColumnId, DesignError, Names}
import scalafim.fmri.design.contrast.ContrastSpec
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.basis.{BasisDiagnostic, BasisFit, BasisFitError, BasisRegistry, ParametricBasis}
import scalafim.fmri.design.event.*
import scalafim.fmri.design.hrf.{HrfFun, HrfSelection}
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.HrfCombinators.*
import scalafim.fmri.hrf.design.SamplingFrame

import scala.collection.immutable.VectorMap
import scala.util.control.NonFatal

object EventModelBuilder:

  final case class BuildOptions(
      defaultHrf: Hrf = Hrfs.SPMG1,
      precision: Seconds = 0.3.s,
      dropEmpty: Boolean = true,
      summate: Boolean = true,
      strict: Boolean = false
  )

  final case class DesignExtensionEnv(
      hrfFuns: Map[String, HrfFun] = Map.empty,
      contrastSets: Map[String, ContrastSpec.ContrastSet] = Map.empty,
      basisRegistry: BasisRegistry = BasisRegistry.default
  )

  enum DurationPlan:
    case Values(seconds: Vector[Double])

    def resolve(nEvents: Int): Either[DesignError, Vector[Double]] =
      this match
        case Values(values) =>
          if values.length == nEvents then Right(values)
          else if values.length == 1 then Right(Vector.fill(nEvents)(values.head))
          else Left(DesignError.InvalidSchedule(s"`durations` must have length 1 or $nEvents, not ${values.length}"))

  object DurationPlan:
    def apply(durations: Seq[Double]): DurationPlan =
      Values(durations.toVector)

  enum BlockPlan:
    case Explicit(ids: Vector[Int])
    case Formula(text: String)
    case SingleBlock

    def resolve(data: DataTable): Either[DesignError, Vector[Int]] =
      this match
        case Explicit(ids) => Right(ids)
        case Formula(text) => parseBlockIds(text, data)
        case SingleBlock   => Right(Vector.fill(data.nrows)(0))

  object BlockPlan:
    def explicit(ids: Seq[Int]): BlockPlan =
      Explicit(ids.toVector)

  final case class TableEnv(eventData: DataTable, other: Map[String, DataTable] = Map.empty):
    def resolveEither(name: Option[String]): Either[DesignError, DataTable] =
      name match
        case None           => Right(eventData)
        case Some(tableKey) => other.get(tableKey).toRight(DesignError.UnknownTable(tableKey))

  final case class EventDesignRequest(
      formula: ModelFormula,
      env: TableEnv,
      samplingFrame: SamplingFrame,
      blockPlan: BlockPlan,
      durationPlan: DurationPlan = DurationPlan(Seq(0.0)),
      options: BuildOptions = BuildOptions(),
      extensions: DesignExtensionEnv = DesignExtensionEnv()
  )

  object EventDesignRequest:
    def fromText(
        formula: String,
        data: DataTable,
        samplingFrame: SamplingFrame,
        blockPlan: BlockPlan = BlockPlan.SingleBlock,
        durationPlan: DurationPlan = DurationPlan(Seq(0.0)),
        tables: Map[String, DataTable] = Map.empty,
        options: BuildOptions = BuildOptions(),
        extensions: DesignExtensionEnv = DesignExtensionEnv()
    ): Either[DesignError, EventDesignRequest] =
      FormulaParser.parseEither(formula).left.map(parseError).map { parsed =>
        EventDesignRequest(
          formula = parsed,
          env = TableEnv(eventData = data, other = tables),
          samplingFrame = samplingFrame,
          blockPlan = blockPlan,
          durationPlan = durationPlan,
          options = options,
          extensions = extensions
        )
      }

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
    build(
      EventDesignRequest.fromText(
        formula = formula,
        data = data,
        samplingFrame = samplingFrame,
        blockPlan = BlockPlan.Formula(block),
        durationPlan = DurationPlan(durations),
        tables = tables,
        options = BuildOptions(
          defaultHrf = defaultHrf,
          precision = precision,
          dropEmpty = dropEmpty,
          summate = summate,
          strict = strict
        ),
        extensions = DesignExtensionEnv(
          hrfFuns = hrfFuns,
          contrastSets = contrastSets,
          basisRegistry = basisRegistry
        )
      ).fold(err => throw new IllegalArgumentException(err.message), identity)
    )

  def bindFormula(
      formula: String,
      data: DataTable,
      availableContrastSets: Set[String] = Set.empty,
      requireKnownContrasts: Boolean = false
  ): Either[DesignError, BoundFormula] =
    FormulaParser.parseEither(formula).left.map(parseError).flatMap { parsed =>
      BoundFormula.bind(
        parsed,
        data,
        availableContrastSets = availableContrastSets,
        requireKnownContrasts = requireKnownContrasts
      )
    }

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
      EventDesignRequest(
        formula = parsed,
        env = env,
        samplingFrame = samplingFrame,
        blockPlan = BlockPlan.explicit(blockIds),
        durationPlan = DurationPlan(durations),
        options = BuildOptions(
          defaultHrf = defaultHrf,
          precision = precision,
          dropEmpty = dropEmpty,
          summate = summate,
          strict = strict
        ),
        extensions = DesignExtensionEnv(
          hrfFuns = hrfFuns,
          contrastSets = contrastSets,
          basisRegistry = basisRegistry
        )
      )
    )

  def buildEither(
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
  ): Either[DesignError, EventModel] =
    FormulaParser.parseEither(formula).left.map(parseError).flatMap { parsed =>
      buildEither(
        EventDesignRequest(
          formula = parsed,
          env = TableEnv(eventData = data, other = tables),
          samplingFrame = samplingFrame,
          blockPlan = BlockPlan.explicit(blockIds),
          durationPlan = DurationPlan(durations),
          options = BuildOptions(
            defaultHrf = defaultHrf,
            precision = precision,
            dropEmpty = dropEmpty,
            summate = summate,
            strict = strict
          ),
          extensions = DesignExtensionEnv(
            hrfFuns = hrfFuns,
            contrastSets = contrastSets,
            basisRegistry = basisRegistry
          )
        )
      )
    }

  def build(request: EventDesignRequest): EventModel =
    buildEither(request).fold(err => throw new IllegalArgumentException(err.message), identity)

  def buildEither(request: EventDesignRequest): Either[DesignError, EventModel] =
    for
      blockIds <- request.blockPlan.resolve(request.env.eventData)
      durations <- request.durationPlan.resolve(request.env.eventData.nrows)
      model <- buildEither(
        formula = request.formula,
        env = request.env,
        samplingFrame = request.samplingFrame,
        blockIds = blockIds,
        durations = durations,
        options = request.options,
        extensions = request.extensions
      )
    yield model

  private def parseError(error: FormulaParser.ParseError): DesignError =
    DesignError.FormulaParse(error.message, error.pos)

  def buildEither(
      formula: ModelFormula,
      env: TableEnv,
      samplingFrame: SamplingFrame,
      blockIds: Seq[Int],
      durations: Seq[Double],
      options: BuildOptions,
      extensions: DesignExtensionEnv
  ): Either[DesignError, EventModel] =
    compile(
      formula,
      env,
      samplingFrame,
      blockIds = blockIds,
      durations = durations,
      options = options,
      extensions = extensions
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
    buildEither(
      formula,
      env,
      samplingFrame,
      blockIds = blockIds,
      durations = durations,
      options = BuildOptions(
        defaultHrf = defaultHrf,
        precision = precision,
        dropEmpty = dropEmpty,
        summate = summate,
        strict = strict
      ),
      extensions = DesignExtensionEnv(
        hrfFuns = hrfFuns,
        contrastSets = contrastSets,
        basisRegistry = basisRegistry
      )
    ).fold(err => throw new IllegalArgumentException(err.message), identity)

  private final case class ResolvedSchedule(
      defaultOnsets: Vector[Seconds],
      defaultDurs: Vector[Seconds],
      blockIds0: Vector[Int]
  )

  private final case class CompiledTerm(
      term: EventModelTerm,
      contrastRef: Option[String],
      diagnostics: Vector[EventModelDiagnostic]
  )

  private final case class EventExpression(
      event: Event,
      diagnostics: Vector[EventModelDiagnostic]
  )

  private final case class CompiledTerms(
      terms: Vector[EventModelTerm],
      contrastRefs: Vector[Option[String]],
      diagnostics: Vector[EventModelDiagnostic]
  )

  private def compile(
      formula: ModelFormula,
      env: TableEnv,
      samplingFrame: SamplingFrame,
      blockIds: Seq[Int],
      durations: Seq[Double],
      options: BuildOptions,
      extensions: DesignExtensionEnv
  ): Either[DesignError, EventModel] =
    for
      schedule <- resolveSchedule(formula, env, blockIds, durations)
      compiled <- compileTerms(formula, env, samplingFrame, schedule, options, extensions)
      model <- assembleModel(compiled, samplingFrame, extensions.contrastSets)
    yield model

  private def resolveSchedule(
      formula: ModelFormula,
      env: TableEnv,
      blockIds: Seq[Int],
      durations: Seq[Double]
  ): Either[DesignError, ResolvedSchedule] =
    for
      onsetVals <- env.eventData.get[Double](formula.onset)
      _ <-
        if blockIds.length == onsetVals.length then Right(())
        else Left(DesignError.InvalidSchedule(s"`blockIds` must have length ${onsetVals.length}, not ${blockIds.length}"))
      durVals <- resolveDurationValues(durations, onsetVals.length)
    yield ResolvedSchedule(
      defaultOnsets = onsetVals.map(Seconds(_)),
      defaultDurs = durVals.map(Seconds(_)),
      blockIds0 = blockIds.toVector
    )

  private def resolveDurationValues(durations: Seq[Double], nEvents: Int): Either[DesignError, Vector[Double]] =
    val values =
      if durations.length == nEvents then durations.toVector
      else if durations.length == 1 then Vector.fill(nEvents)(durations.head)
      else return Left(DesignError.InvalidSchedule(s"`durations` must have length 1 or $nEvents, not ${durations.length}"))
    validateScheduleValues(values, argName = "durations")

  private def validateScheduleValues(values: Vector[Double], argName: String): Either[DesignError, Vector[Double]] =
    if values.exists(!_.isFinite) then Left(DesignError.InvalidSchedule(s"$argName must be finite"))
    else if argName == "durations" && values.exists(_ < 0.0) then Left(DesignError.InvalidSchedule("durations must be non-negative"))
    else Right(values)

  private def compileTerms(
      formula: ModelFormula,
      env: TableEnv,
      samplingFrame: SamplingFrame,
      schedule: ResolvedSchedule,
      options: BuildOptions,
      extensions: DesignExtensionEnv
  ): Either[DesignError, CompiledTerms] =
    val terms = Vector.newBuilder[EventModelTerm]
    val contrastRefs = Vector.newBuilder[Option[String]]
    val diagnostics = Vector.newBuilder[EventModelDiagnostic]
    var failed: Option[DesignError] = None
    var i = 0
    while i < formula.terms.length && failed.isEmpty do
      compileTerm(formula.terms(i), env, samplingFrame, schedule, options, extensions) match
        case Left(error) =>
          failed = Some(error)
        case Right(compiled) =>
          terms += compiled.term
          contrastRefs += compiled.contrastRef
          diagnostics ++= compiled.diagnostics
      i += 1

    failed match
      case Some(error) => Left(error)
      case None =>
        Right(CompiledTerms(terms.result(), contrastRefs.result(), diagnostics.result()))

  private def compileTerm(
      call: TermCall,
      env: TableEnv,
      samplingFrame: SamplingFrame,
      schedule: ResolvedSchedule,
      options: BuildOptions,
      extensions: DesignExtensionEnv
  ): Either[DesignError, CompiledTerm] =
    call match
      case h: HrfCall =>
        compileHrfCall(h, env, samplingFrame, schedule, options, extensions)
      case t: TrialwiseCall =>
        compileTrialwiseCall(t, env, samplingFrame, schedule, options)
      case c: CovariateCall =>
        compileCovariateCall(c, env, samplingFrame)

  private def compileHrfCall(
      h: HrfCall,
      env: TableEnv,
      samplingFrame: SamplingFrame,
      schedule: ResolvedSchedule,
      options: BuildOptions,
      extensions: DesignExtensionEnv
  ): Either[DesignError, CompiledTerm] =
    val nEvents = schedule.defaultOnsets.length
    for
      termTag <- inferTermTag(h, extensions.basisRegistry)
      expressions <- toEvents(h.vars, env.eventData, termTag)
      events0 = expressions.map(_.event)
      basisDiagnostics = expressions.flatMap(_.diagnostics)
      termOnsets <- h.onsets.fold[Either[DesignError, Vector[Seconds]]](Right(schedule.defaultOnsets)) { ref =>
        resolveSecondsEither(ref, env.eventData, nEvents, argName = "onsets")
      }
      termDurs <- h.durations.fold[Either[DesignError, Vector[Seconds]]](Right(schedule.defaultDurs)) { ref =>
        resolveSecondsEither(ref, env.eventData, nEvents, argName = "durations")
      }
      subsetMask <- resolveSubsetMaskEither(h.subset, env.eventData, nEvents)
      originalSub = if subsetMask.forall(identity) then env.eventData else env.eventData.filterRows(subsetMask)
      subset = if subsetMask.forall(identity) then (events0, termOnsets, termDurs, schedule.blockIds0)
        else subsetTerm(events0, termOnsets, termDurs, schedule.blockIds0, subsetMask)
      (events, onsetsS, dursS, blockIdsS) = subset
      cleaned = sanitizeContinuousEvents(events, termTag)
      (eventsClean, nonFiniteDiagnostics) = cleaned
      term <- eventTermEither(
        events = eventsClean,
        onsets = onsetsS,
        durations = dursS,
        blockIds = blockIdsS,
        termTag = termTag
      )
      termDiagnostics = basisDiagnostics ++ nonFiniteDiagnostics ++ diagnoseTerm(term, samplingFrame)
      _ <- validateStrictDiagnostics(termDiagnostics, options.strict)
      conv <- convolveHrfTermEither(h, term, originalSub, samplingFrame, options, extensions)
    yield CompiledTerm(conv, h.contrasts, termDiagnostics)

  private def compileTrialwiseCall(
      t: TrialwiseCall,
      env: TableEnv,
      samplingFrame: SamplingFrame,
      schedule: ResolvedSchedule,
      options: BuildOptions
  ): Either[DesignError, CompiledTerm] =
    val nEvents = schedule.defaultOnsets.length
    val label0 = t.label.fold("trial")(_.value)
    val termTag = Names.sanitize(label0, allowDot = false)

    for
      termDurs <- t.durations.fold[Either[DesignError, Vector[Seconds]]](Right(schedule.defaultDurs)) { ref =>
        resolveSecondsEither(ref, env.eventData, nEvents, argName = "durations")
      }
      trialEvent <- catchBuild(DesignError.fromThrowable)(Event.factor(trialLevels(nEvents), name = "trial"))
      term <- eventTermEither(
        events = Vector(trialEvent),
        onsets = schedule.defaultOnsets,
        durations = termDurs,
        blockIds = schedule.blockIds0,
        termTag = Some(termTag)
      )
      termDiagnostics = diagnoseTerm(term, samplingFrame)
      _ <- validateStrictDiagnostics(termDiagnostics, options.strict)
      basisName = t.basis.getOrElse("spmg1")
      hrf0 <- resolveHrfBasisEither(basisName, nbasis = t.nbasis, lag = t.lag)
      conv0 <- catchBuild(DesignError.fromThrowable) {
        term.convolve(
          hrf0,
          samplingFrame,
          precision = options.precision,
          dropEmpty = options.dropEmpty,
          summate = options.summate,
          normalize = t.normalize.getOrElse(false)
        )
      }
      conv = conv0.copy(
        role = EventTermRole.Trialwise,
        columnRoles = Vector.fill(conv0.columnNames.length)(EventTermColumnRole.Trial)
      )
      out = if t.addSum.getOrElse(false) then addMeanColumn(conv, label = label0) else conv
    yield CompiledTerm(out, None, termDiagnostics)

  private def compileCovariateCall(
      c: CovariateCall,
      env: TableEnv,
      samplingFrame: SamplingFrame
  ): Either[DesignError, CompiledTerm] =
    for
      table <- env.resolveEither(c.data)
      vars <- covariateVars(c.vars)
      spec = CovariateSpec(vars = vars, data = table, id = c.id.map(_.value), prefix = c.prefix.map(_.value))
      term <- spec.construct(samplingFrame)
    yield CompiledTerm(term, None, Vector.empty)

  private def covariateVars(values: Vector[ArgValue]): Either[DesignError, Vector[ColumnId]] =
    val out = Vector.newBuilder[ColumnId]
    var i = 0
    while i < values.length do
      values(i) match
        case ArgValue.Ident(id) => out += id
        case other =>
          return Left(DesignError.FormulaBinding(s"covariate vars must be identifiers, found $other"))
      i += 1
    Right(out.result())

  private def assembleModel(
      compiled: CompiledTerms,
      samplingFrame: SamplingFrame,
      contrastSets: Map[String, ContrastSpec.ContrastSet]
  ): Either[DesignError, EventModel] =
    for
      model0 <- EventModel.buildTermsEither(compiled.terms, samplingFrame)
      refs <-
        if compiled.contrastRefs.length == model0.terms.length then Right(compiled.contrastRefs)
        else Left(DesignError.BuildFailed("internal: contrastRefs length mismatch"))
      attached <- attachContrastSetsEither(model0, refs, contrastSets)
    yield model0.copy(
      contrastSetsByTerm = attached,
      diagnostics = compiled.diagnostics
    )

  private def attachContrastSetsEither(
      model: EventModel,
      refs: Vector[Option[String]],
      available: Map[String, ContrastSpec.ContrastSet]
  ): Either[DesignError, VectorMap[String, ContrastSpec.ContrastSet]] =
    if refs.isEmpty then Right(VectorMap.empty)
    else
      val out = VectorMap.newBuilder[String, ContrastSpec.ContrastSet]
      var failed: Option[DesignError] = None
      var i = 0
      while i < refs.length && failed.isEmpty do
        refs(i) match
          case None => ()
          case Some(key) =>
            val (termKey, term) = model.terms(i)
            term match
              case _: ConvolvedTerm =>
                available.get(key) match
                  case Some(set) => out += (termKey -> set)
                  case None      => failed = Some(DesignError.UnknownContrast(key, available.keys.toVector.sorted))
              case other =>
                failed = Some(DesignError.UnsupportedContrastTarget(termKey, other.getClass.getSimpleName))
        i += 1
      failed match
        case Some(error) => Left(error)
        case None        => Right(out.result())

  private def catchBuild[A](mapError: Throwable => DesignError)(body: => A): Either[DesignError, A] =
    try Right(body)
    catch
      case NonFatal(t) => Left(mapError(t))

  /** Look up a column for the subset evaluator, which reports through `err`.
    *
    * `evalSubset` is a recursive expression evaluator with one error protocol —
    * throw, and let `resolveSubsetMaskEither` turn every failure into
    * `DesignError.InvalidSubset`, which names the offending expression. This is
    * the only place in `compile` that still reports a lookup by throwing.
    */
  private def subsetColumn(data: DataTable, id: ColumnId): Column =
    data.column(id).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def throwableMessage(t: Throwable): String =
    Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.toString)

  private def eventTermEither(
      events: Vector[Event],
      onsets: Vector[Seconds],
      durations: Vector[Seconds],
      blockIds: Vector[Int],
      termTag: Option[String]
  ): Either[DesignError, EventTerm] =
    catchBuild(t => DesignError.InvalidSchedule(throwableMessage(t))) {
      EventTerm(
        events = events,
        onsets = onsets,
        durations = durations,
        blockIds = blockIds,
        termTag = termTag
      )
    }

  private def validateStrictDiagnostics(
      diagnostics: Vector[EventModelDiagnostic],
      strict: Boolean
  ): Either[DesignError, Unit] =
    if !strict then Right(())
    else
      val blocking = diagnostics.filter(d => strictDiagnosticKinds.contains(d.kind))
      if blocking.isEmpty then Right(())
      else Left(DesignError.InvalidSchedule(blocking.map(_.message).mkString("; ")))

  private val strictDiagnosticKinds: Set[EventModelDiagnosticKind] =
    Set(EventModelDiagnosticKind.BasisDegeneracy, EventModelDiagnosticKind.OnsetOutOfBounds)

  private def resolveSubsetMaskEither(
      subset: Option[ArgValue],
      data: DataTable,
      nEvents: Int
  ): Either[DesignError, Vector[Boolean]] =
    subset match
      case None => Right(Vector.fill(nEvents)(true))
      case Some(expr) =>
        catchBuild(t => DesignError.InvalidSubset(stripSubsetPrefix(throwableMessage(t)))) {
          val keep = evalSubset(expr, data)
          require(keep.length == nEvents, s"subset mask has length ${keep.length} but expected $nEvents")
          keep
        }

  private def stripSubsetPrefix(message: String): String =
    message.stripPrefix("subset: ").trim

  private def resolveSecondsEither(
      ref: ArgValue,
      data: DataTable,
      nEvents: Int,
      argName: String
  ): Either[DesignError, Vector[Seconds]] =
    resolveNumericVectorEither(ref, data, nEvents, argName).map(_.map(Seconds(_)))

  private def resolveNumericVectorEither(
      ref: ArgValue,
      data: DataTable,
      nEvents: Int,
      argName: String
  ): Either[DesignError, Vector[Double]] =
    val valuesEither: Either[DesignError, Vector[Double]] =
      ref match
        case ArgValue.Num(v) =>
          if v.isFinite then Right(Vector.fill(nEvents)(v))
          else Left(scheduleArgError(argName, s"$argName scalar must be finite"))
        case ArgValue.Ident(id) =>
          data.get[Double](id)
        case ArgValue.Str(name) =>
          ColumnId(name).flatMap(data.get[Double])
        case other =>
          Left(DesignError.FormulaBinding(s"$argName must be a column reference or numeric scalar, found $other"))

    valuesEither.flatMap { values =>
      if values.length != nEvents then Left(scheduleArgError(argName, s"$argName has length ${values.length} but expected $nEvents"))
      else validateScheduleValues(values, argName)
    }

  private def scheduleArgError(argName: String, detail: String): DesignError =
    argName match
      case "durations" | "onsets" => DesignError.InvalidSchedule(detail)
      case _                      => DesignError.FormulaBinding(detail)

  private def convolveHrfTermEither(
      h: HrfCall,
      term: EventTerm,
      originalSub: DataTable,
      samplingFrame: SamplingFrame,
      options: BuildOptions,
      extensions: DesignExtensionEnv
  ): Either[DesignError, ConvolvedTerm] =
    val summate0 = h.summate.getOrElse(options.summate)
    val normalize0 = h.normalize.getOrElse(false)

    (h.hrfFun, term.onsets.nonEmpty) match
      case (Some(ref), true) =>
        for
          eventData <- catchBuild(DesignError.fromThrowable)(buildEventDataForGenerator(term, originalSub))
          hsel <- resolveHrfFunEither(ref, eventData, hrfFuns = extensions.hrfFuns, termTag = term.termTag)
          conv <- catchBuild(DesignError.fromThrowable) {
            hsel match
              case Left(shared) =>
                term.convolve(shared, samplingFrame, precision = options.precision, dropEmpty = options.dropEmpty, summate = summate0, normalize = normalize0)
              case Right(perEvent) =>
                term.convolvePerEvent(perEvent, samplingFrame, precision = options.precision, dropEmpty = options.dropEmpty, summate = summate0, normalize = normalize0)
          }
        yield conv
      case _ =>
        for
          hrf0 <- resolveHrfEither(h, options.defaultHrf)
          conv <- catchBuild(DesignError.fromThrowable) {
            term.convolve(hrf0, samplingFrame, precision = options.precision, dropEmpty = options.dropEmpty, summate = summate0, normalize = normalize0)
          }
        yield conv

  private def resolveHrfFunEither(
      ref: ArgValue,
      eventData: DataTable,
      hrfFuns: Map[String, HrfFun],
      termTag: Option[String]
  ): Either[DesignError, Either[Hrf, Vector[Hrf]]] =
    val termLabel = termTag.getOrElse("unknown")
    val nEvents = eventData.nrows

    def invalid(detail: String): DesignError =
      DesignError.InvalidHrfFun(termLabel, detail)

    def validateHrfs(hrfs0: Seq[Hrf]): Either[DesignError, Either[Hrf, Vector[Hrf]]] =
      val hrfs = hrfs0.toVector
      if hrfs.isEmpty then Left(invalid(s"hrf_fun for term '$termLabel' returned 0 HRFs but $nEvents events exist"))
      else if hrfs.length == 1 then Right(Left(hrfs.head))
      else if hrfs.length != nEvents then
        Left(invalid(s"hrf_fun for term '$termLabel' returned ${hrfs.length} HRFs but $nEvents events exist"))
      else if !hrfs.forall(_.nbasis == hrfs.head.nbasis) then
        Left(invalid(s"All HRFs from hrf_fun for term '$termLabel' must have the same nbasis"))
      else Right(Right(hrfs))

    def hrfColumn(id: ColumnId): Either[DesignError, Either[Hrf, Vector[Hrf]]] =
      eventData.get[Hrf](id).left.map(error => invalid(error.message)).flatMap(validateHrfs)

    ref match
      // A generator is user code with no error channel of its own, so its throw
      // is caught here and named as this term's hrf_fun failure.
      case ArgValue.Ident(key) if hrfFuns.contains(key.value) =>
        catchBuild(t => invalid(throwableMessage(t)))(hrfFuns(key.value)(eventData)).flatMap {
          case HrfSelection.Shared(hrf)    => Right(Left(hrf))
          case HrfSelection.PerEvent(hrfs) => validateHrfs(hrfs)
        }
      case ArgValue.Ident(id) => hrfColumn(id)
      case ArgValue.Str(name) => ColumnId(name).left.map(error => invalid(error.message)).flatMap(hrfColumn)
      case other              => Left(invalid(s"hrf_fun for term '$termLabel' must be a string/identifier, found $other"))

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
        case ArgValue.Ident(id) =>
          subsetColumn(data, id) match
            case Column.Bools(v) =>
              require(v.length == n, "internal: bool column length mismatch")
              v
            case other => err(s"identifier '${id.value}' is not a boolean column (found $other)")
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
        case ArgValue.Ident(id) =>
          subsetColumn(data, id) match
            case Column.Doubles(v) => ScalarVec.Num(v)
            case Column.Ints(v)    => ScalarVec.Num(v.map(_.toDouble))
            case Column.Strings(v) => ScalarVec.Str(v)
            case Column.Bools(v)   => ScalarVec.Bool(v)
            case Column.DoubleLists(_) => err(s"column '${id.value}' is a list column and cannot be used in subset comparisons")
            case Column.Hrfs(_)    => err(s"column '${id.value}' is an HRF list and cannot be used in subset comparisons")
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

  private def parseBlockIds(block: String, data: DataTable): Either[DesignError, Vector[Int]] =
    val trimmed = block.trim
    val rhs0 = if trimmed.startsWith("~") then trimmed.drop(1).trim else trimmed

    if trimmed.isEmpty then Left(DesignError.InvalidSchedule("block formula must be non-empty"))
    else if rhs0 == "1" || rhs0 == "1.0" then Right(Vector.fill(data.nrows)(0))
    else
      for
        id <- ColumnId(rhs0)
        raw <- data.column(id)
        ids <- raw match
          case Column.Doubles(v) =>
            requireNonDecreasing(v, name = rhs0).map(_ => canonicalize(v))
          case Column.Ints(v) =>
            requireNonDecreasing(v.map(_.toDouble), name = rhs0).map(_ => canonicalize(v))
          case Column.Strings(v) =>
            requireBlocksContiguous(v, name = rhs0).map(_ => canonicalize(v))
          case Column.Bools(v) =>
            requireBlocksContiguous(v.map(_.toString), name = rhs0).map(_ => canonicalize(v.map(_.toString)))
          case other =>
            Left(DesignError.InvalidColumnType(rhs0, "usable as a block index", other.typeName))
      yield ids

  private def requireNonDecreasing(xs: Vector[Double], name: String): Either[DesignError, Unit] =
    var i = 1
    while i < xs.length do
      if xs(i) < xs(i - 1) then return Left(nonDecreasingError(name))
      i += 1
    Right(())

  private def requireBlocksContiguous(xs: Vector[String], name: String): Either[DesignError, Unit] =
    val codes = canonicalize(xs)
    var i = 1
    while i < codes.length do
      if codes(i) < codes(i - 1) then return Left(nonDecreasingError(name))
      i += 1
    Right(())

  private def nonDecreasingError(name: String): DesignError =
    DesignError.InvalidSchedule(s"'blockIds' must be non-decreasing (from '$name')")

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

  private def toEvents(
      exprs: Vector[ArgValue],
      data: DataTable,
      termTag: Option[String]
  ): Either[DesignError, Vector[EventExpression]] =
    val out = Vector.newBuilder[EventExpression]
    var i = 0
    while i < exprs.length do
      toEvent(data, exprs(i), termTag) match
        case Left(error)       => return Left(error)
        case Right(expression) => out += expression
      i += 1
    Right(out.result())

  private def toEvent(data: DataTable, expr: ArgValue, termTag: Option[String]): Either[DesignError, EventExpression] =
    expr match
      case ArgValue.Ident(id) =>
        data.column(id).flatMap {
          case Column.Strings(v) => Right(Event.factor(v, id.value))
          case Column.Doubles(v) => Right(Event.variable(v, id.value))
          case Column.Ints(v)    => Right(Event.variable(v.map(_.toDouble), id.value))
          case Column.Bools(v)   => Right(Event.factor(v.map(_.toString), id.value))
          case other =>
            Left(DesignError.InvalidColumnType(id.value, "usable as an event variable", other.typeName))
        }.map(EventExpression(_, Vector.empty))
      case ArgValue.Call(fun, args) =>
        evalBasisCall(data, fun, args, termTag)
      case other =>
        Left(DesignError.FormulaBinding(s"Unsupported event expression: $other"))

  /** Basis calls the formula grammar understands, for [[DesignError.UnknownBasisFunction]]. */
  private val basisCalls: Vector[String] =
    Vector("scale", "standardized", "robustscale", "poly", "bspline", "scalewithin")

  private def evalBasisCall(
      data: DataTable,
      funName: String,
      args: Vector[Arg],
      termTag: Option[String]
  ): Either[DesignError, EventExpression] =
    funName.trim.toLowerCase match
      case "scale" =>
        requireNumeric1(data, args, "Scale").flatMap { (xVar, xs) =>
          basisExpression(ParametricBasis.Scale.fitWithDiagnostics(xs, argName = xVar.value), termTag)
        }
      case "standardized" =>
        requireNumeric1(data, args, "Standardized").flatMap { (xVar, xs) =>
          basisExpression(ParametricBasis.Standardized.fitWithDiagnostics(xs, argName = xVar.value), termTag)
        }
      case "robustscale" =>
        requireNumeric1(data, args, "RobustScale").flatMap { (xVar, xs) =>
          basisExpression(ParametricBasis.RobustScale.fitWithDiagnostics(xs, argName = xVar.value), termTag)
        }
      case "poly" =>
        for
          (xVar, xs) <- requireNumeric1(data, args, "Poly")
          degree <- requireIntArg(args, "degree", fallbackPos = 1, ctx = "Poly")
          basis <- catchBuild(t => DesignError.FormulaBinding(throwableMessage(t))) {
            ParametricBasis.Poly.fit(xs, degree = degree, argName = xVar.value)
          }
        yield EventExpression(Event.basis(basis), Vector.empty)
      case "bspline" =>
        for
          (xVar, xs) <- requireNumeric1(data, args, "BSpline")
          degree <- requireIntArg(args, "degree", fallbackPos = 1, ctx = "BSpline")
          basis <- catchBuild(t => DesignError.FormulaBinding(throwableMessage(t))) {
            ParametricBasis.BSpline.fit(xs, degree = degree, argName = xVar.value)
          }
        yield EventExpression(Event.basis(basis), Vector.empty)
      case "scalewithin" =>
        for
          xVar <- requireIdentArg(args, pos = 0, ctx = "ScaleWithin")
          gVar <- requireIdentArg(args, pos = 1, ctx = "ScaleWithin")
          xs <- data.get[Double](xVar)
          gs <- toFactorStrings(data, gVar)
          expression <- basisExpression(
            ParametricBasis.ScaleWithin.fitWithDiagnostics(xs, gs, argName = xVar.value, groupName = gVar.value),
            termTag
          )
        yield expression
      case _ =>
        Left(DesignError.UnknownBasisFunction(funName, basisCalls))

  private def basisExpression[A <: ParametricBasis](
      fitEither: Either[BasisFitError, BasisFit[A]],
      termTag: Option[String]
  ): Either[DesignError, EventExpression] =
    fitEither match
      case Right(fit) =>
        Right(EventExpression(Event.basis(fit.basis), fit.diagnostics.map(toModelDiagnostic(termTag))))
      case Left(error) =>
        Left(DesignError.DegenerateBasis(error.message))

  private def toModelDiagnostic(termTag: Option[String])(diagnostic: BasisDiagnostic): EventModelDiagnostic =
    val termLabel = termTag.getOrElse("term")
    EventModelDiagnostic(
      EventModelDiagnosticKind.BasisDegeneracy,
      termLabel,
      s"${diagnostic.message} in term '$termLabel'"
    )

  private def requireNumeric1(
      data: DataTable,
      args: Vector[Arg],
      ctx: String
  ): Either[DesignError, (ColumnId, Vector[Double])] =
    for
      xVar <- requireIdentArg(args, pos = 0, ctx = ctx)
      xs <- data.get[Double](xVar)
    yield (xVar, xs)

  private def requireIdentArg(args: Vector[Arg], pos: Int, ctx: String): Either[DesignError, ColumnId] =
    args.lift(pos) match
      case Some(Arg(None, ArgValue.Ident(id))) => Right(id)
      case Some(a) =>
        Left(DesignError.FormulaBinding(s"$ctx positional arg ${pos + 1} must be an identifier, found $a"))
      case None =>
        Left(DesignError.FormulaBinding(s"$ctx requires at least ${pos + 1} positional args"))

  private def requireIntArg(args: Vector[Arg], name: String, fallbackPos: Int, ctx: String): Either[DesignError, Int] =
    val named = args.collectFirst { case Arg(Some(nm), ArgValue.Num(v)) if nm == name => v }
    val pos = args.lift(fallbackPos).collect { case Arg(None, ArgValue.Num(v)) => v }
    named.orElse(pos) match
      case None => Left(DesignError.FormulaBinding(s"$ctx requires '$name' (e.g. $name=3)"))
      case Some(v0) =>
        if !v0.isFinite || !v0.isValidInt || v0 != v0.toInt.toDouble then
          Left(DesignError.FormulaBinding(s"$ctx '$name' must be an integer, got $v0"))
        else Right(v0.toInt)

  private def toFactorStrings(data: DataTable, id: ColumnId): Either[DesignError, Vector[String]] =
    data.column(id).flatMap {
      case Column.Strings(v) => Right(v)
      case Column.Ints(v)    => Right(v.map(_.toString))
      case Column.Doubles(v) => Right(v.map(_.toString))
      case Column.Bools(v)   => Right(v.map(_.toString))
      case Column.Hrfs(v)    => Right(v.map(_.name))
      case other             => Left(DesignError.InvalidColumnType(id.value, "usable as a factor", other.typeName))
    }

  /** The term tag is a *generated* name, so this is where sanitization belongs. */
  private def inferTermTag(call: HrfCall, basisRegistry: BasisRegistry): Either[DesignError, Option[String]] =
    def tagOf(label: String): Option[String] = Some(Names.sanitize(label, allowDot = false))

    call.id.orElse(call.prefix) match
      case Some(id) => Right(tagOf(id.value))
      case None =>
        if call.vars.length != 1 then Right(tagOf(call.vars.map(exprLabel).mkString("_")))
        else
          call.vars.head match
            case ArgValue.Call(fun, args) =>
              val f = fun.trim.toLowerCase
              if f == "ident" then Right(None)
              else
                parametricBasisTag(f, args, basisRegistry).map(_.orElse(tagOf(exprLabel(call.vars.head))))
            case ArgValue.Ident(id) => Right(tagOf(id.value))
            case other              => Right(tagOf(exprLabel(other)))

  private def parametricBasisTag(
      funLower: String,
      args: Vector[Arg],
      basisRegistry: BasisRegistry
  ): Either[DesignError, Option[String]] =
    basisRegistry.getFormulaEntry(funLower).flatMap(_.prefix) match
      case None => Right(None)
      case Some(p) =>
        requireIdentArg(args, pos = 0, ctx = funLower).map { xVar =>
          Some(s"${p}_${Names.sanitize(xVar.value, allowDot = false)}")
        }

  private def exprLabel(v: ArgValue): String =
    v match
      case ArgValue.Ident(x) => x.value
      case ArgValue.Str(x)   => "\"" + x + "\""
      case ArgValue.Num(x)   => x.toString
      case ArgValue.Bool(x)  => x.toString
      case ArgValue.Call(fun, args) =>
        val as = args.map { a =>
          val pref = a.name.map(_ + "=").getOrElse("")
          pref + exprLabel(a.value)
        }.mkString(",")
        s"$fun($as)"

  private def resolveHrfEither(call: HrfCall, defaultHrf: Hrf): Either[DesignError, Hrf] =
    val baseEither: Either[DesignError, Hrf] =
      call.basis match
        case None => Right(defaultHrf)
        case Some(basisName0) =>
          val basisName = basisName0.trim.toLowerCase
          basisName match
            case "spmg1"    => Right(Hrfs.SPMG1)
            case "spmg2"    => Right(Hrfs.SPMG2)
            case "spmg3"    => Right(Hrfs.SPMG3)
            case "gamma"    => Right(Hrfs.Gamma)
            case "gaussian" => Right(Hrfs.Gaussian)
            case "fir"      => catchBuild(DesignError.fromThrowable)(Hrfs.fir(nBasis = call.nbasis.getOrElse(12)))
            case "bspline"  => catchBuild(DesignError.fromThrowable)(Hrfs.bspline(nBasis = call.nbasis.getOrElse(5)))
            case "tent"     => catchBuild(DesignError.fromThrowable)(Hrfs.tent(nBasis = call.nbasis.getOrElse(5)))
            case "fourier"  => catchBuild(DesignError.fromThrowable)(Hrfs.fourier(nBasis = call.nbasis.getOrElse(5)))
            case other      => Left(DesignError.UnknownBasis(other))

    baseEither.flatMap { base =>
      call.lag match
        case None => Right(base)
        case Some(l) =>
          if l.isFinite then Right(base.lag(Seconds(l)))
          else Left(DesignError.FormulaBinding("`lag` must be finite"))
    }

  private def resolveHrfBasisEither(basis: String, nbasis: Option[Int], lag: Option[Double]): Either[DesignError, Hrf] =
    val call = HrfCall(vars = Vector(ArgValue.Ident(ColumnId.unsafe("x"))), basis = Some(basis), lag = lag, nbasis = nbasis)
    resolveHrfEither(call, defaultHrf = Hrfs.SPMG1)

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
        columnRoles = term.resolvedColumnRoles :+ EventTermColumnRole.TrialAggregate,
        columnConditions = term.columnConditions :+ Some("mean"),
        columnBasisIx = term.columnBasisIx :+ None
      )
