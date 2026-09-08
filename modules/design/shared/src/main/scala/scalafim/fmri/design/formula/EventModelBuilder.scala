package scalafim.fmri.design.formula

import scalafim.fmri.design.{CellAssignment, CellKey, CenteringOutcome, CenteringPolicy, CenteringReceipt, CenteringGroupReceipt, ColumnId, DegenerateModulatorOutcome, DegenerateModulatorPolicy, DegenerateModulatorReceipt, DesignError, EmptyCellAudit, EmptyCellDisposition, EmptyCellPolicy, EmptyCellScope, EventRowProvenance, FactorLevelAudit, FactorLevelRegistry, FactorId, FactorPartitionAudit, FactorSchemaBinding, HrfAssignment, HrfByCell, HrfByPhase, HrfColumnScale, HrfColumnScaling, MissingValuePolicy, MissingValueResolution, ModulatorId, ModulatorOrthogonalization, ModulatorOrthogonalizationPlan, Names, OrthogonalizationGroupReceipt, OrthogonalizationOutcome, OrthogonalizationReceipt, OrthogonalizationScope, OrthogonalizationStepReceipt, PhaseId, PolicyReceipt, RunIndex, TermId, TrialId}
import scalafim.fmri.design.{ResponseSupportRequest, EventSupportReceipt, EventResponseSupport}
import scalafim.fmri.design.contrast.ContrastSpec
import scalafim.fmri.design.contrast.{LevelId, TermCells}
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.basis.{BasisDiagnostic, BasisFit, BasisFitError, BasisRegistry, ParametricBasis}
import scalafim.fmri.design.linalg.QrDecomposition
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
      strict: Boolean = false,
      missingValuePolicy: MissingValuePolicy = MissingValuePolicy.ZeroContribution,
      factorLevels: FactorLevelRegistry = FactorLevelRegistry.empty,
      emptyCellPolicy: EmptyCellPolicy = EmptyCellPolicy.UseDropEmptyFlag,
      factorSchemaBinding: Option[FactorSchemaBinding] = None,
      hrfByCell: Option[HrfByCell] = None,
      hrfByPhase: Option[HrfByPhase] = None,
      degenerateModulatorPolicy: DegenerateModulatorPolicy = DegenerateModulatorPolicy.RetainAndReport,
      orthogonalization: ModulatorOrthogonalizationPlan = ModulatorOrthogonalizationPlan.None,
      responseSupport: Option[ResponseSupportRequest] = None
  ):
    def validate: Either[DesignError, Unit] =
      for
        _ <- missingValuePolicy.validate
        _ <- factorSchemaBinding match
          case Some(binding) if !factorLevels.isEmpty && binding.registry.canonical != factorLevels.canonical =>
            Left(DesignError.InvalidSchema("factor schema binding registry does not match factorLevels"))
          case _ => Right(())
        _ <- hrfByCell match
          case Some(assignments) if assignments.assignments.isEmpty =>
            Left(DesignError.InvalidSchema("HRF-by-cell assignment must not be empty"))
          case _ => Right(())
        _ <-
          if hrfByCell.nonEmpty && hrfByPhase.nonEmpty then
            Left(DesignError.InvalidSchema("hrfByCell and hrfByPhase are alternative assignment plans"))
          else Right(())
      yield ()

    def factorRegistry: FactorLevelRegistry =
      factorSchemaBinding.fold(factorLevels)(_.registry)

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
      basisRegistry: BasisRegistry = BasisRegistry.default,
      factorLevels: FactorLevelRegistry = FactorLevelRegistry.empty,
      emptyCellPolicy: EmptyCellPolicy = EmptyCellPolicy.UseDropEmptyFlag,
      factorSchemaBinding: Option[FactorSchemaBinding] = None,
      hrfByCell: Option[HrfByCell] = None,
      missingValuePolicy: MissingValuePolicy = MissingValuePolicy.ZeroContribution,
      degenerateModulatorPolicy: DegenerateModulatorPolicy = DegenerateModulatorPolicy.RetainAndReport,
      orthogonalization: ModulatorOrthogonalizationPlan = ModulatorOrthogonalizationPlan.None
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
          strict = strict,
          factorLevels = factorLevels,
          emptyCellPolicy = emptyCellPolicy,
          factorSchemaBinding = factorSchemaBinding,
          hrfByCell = hrfByCell,
          missingValuePolicy = missingValuePolicy,
          degenerateModulatorPolicy = degenerateModulatorPolicy,
          orthogonalization = orthogonalization
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
      basisRegistry: BasisRegistry = BasisRegistry.default,
      factorLevels: FactorLevelRegistry = FactorLevelRegistry.empty,
      emptyCellPolicy: EmptyCellPolicy = EmptyCellPolicy.UseDropEmptyFlag,
      factorSchemaBinding: Option[FactorSchemaBinding] = None,
      hrfByCell: Option[HrfByCell] = None,
      missingValuePolicy: MissingValuePolicy = MissingValuePolicy.ZeroContribution,
      degenerateModulatorPolicy: DegenerateModulatorPolicy = DegenerateModulatorPolicy.RetainAndReport,
      orthogonalization: ModulatorOrthogonalizationPlan = ModulatorOrthogonalizationPlan.None
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
          strict = strict,
          factorLevels = factorLevels,
          emptyCellPolicy = emptyCellPolicy,
          factorSchemaBinding = factorSchemaBinding,
          hrfByCell = hrfByCell,
          missingValuePolicy = missingValuePolicy,
          degenerateModulatorPolicy = degenerateModulatorPolicy,
          orthogonalization = orthogonalization
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
      basisRegistry: BasisRegistry = BasisRegistry.default,
      factorLevels: FactorLevelRegistry = FactorLevelRegistry.empty,
      emptyCellPolicy: EmptyCellPolicy = EmptyCellPolicy.UseDropEmptyFlag,
      factorSchemaBinding: Option[FactorSchemaBinding] = None,
      hrfByCell: Option[HrfByCell] = None,
      missingValuePolicy: MissingValuePolicy = MissingValuePolicy.ZeroContribution,
      degenerateModulatorPolicy: DegenerateModulatorPolicy = DegenerateModulatorPolicy.RetainAndReport,
      orthogonalization: ModulatorOrthogonalizationPlan = ModulatorOrthogonalizationPlan.None
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
            strict = strict,
            factorLevels = factorLevels,
            emptyCellPolicy = emptyCellPolicy,
            factorSchemaBinding = factorSchemaBinding,
            hrfByCell = hrfByCell,
            missingValuePolicy = missingValuePolicy,
            degenerateModulatorPolicy = degenerateModulatorPolicy,
            orthogonalization = orthogonalization
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
    for
      _ <- options.validate
      model <- compile(
        formula,
        env,
        samplingFrame,
        blockIds = blockIds,
        durations = durations,
        options = options,
        extensions = extensions
      )
    yield model

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

  private final case class ResolvedPhase(
      id: PhaseId,
      parentTrialIds: Vector[TrialId]
  )

  /** Parallel event-row axes kept in one value so subsetting cannot advance
    * timing, run, and source provenance independently.
    */
  private final case class TermRows(
      events: Vector[Event],
      onsets: Vector[Seconds],
      durations: Vector[Seconds],
      blockIds: Vector[Int],
      sourceRows: Vector[Int]
  ):
    private val size = onsets.length
    require(durations.length == size, "term rows: duration length mismatch")
    require(blockIds.length == size, "term rows: block-id length mismatch")
    require(sourceRows.length == size, "term rows: source-row length mismatch")
    require(events.forall(_.nEvents == size), "term rows: event length mismatch")

  private final case class CompiledTerm(
      term: EventModelTerm,
      contrastRef: Option[String],
      diagnostics: Vector[EventModelDiagnostic],
      missingValues: Vector[MissingValueResolution] = Vector.empty,
      centeringReceipts: Vector[CenteringReceipt] = Vector.empty,
      degenerateModulatorReceipts: Vector[DegenerateModulatorReceipt] = Vector.empty,
      orthogonalizationReceipts: Vector[OrthogonalizationReceipt] = Vector.empty,
      policyReceipts: Vector[PolicyReceipt] = Vector.empty,
      factorLevels: Vector[FactorLevelAudit] = Vector.empty,
      emptyCells: Vector[CellKey] = Vector.empty,
      emptyCellAudits: Vector[EmptyCellAudit] = Vector.empty,
      responseSupport: Vector[EventSupportReceipt] = Vector.empty
  )

  private final case class EventExpression(
      event: Event,
      diagnostics: Vector[EventModelDiagnostic],
      centering: Vector[CenteringRequest] = Vector.empty
  )

  /** A formula-level centering request retained until term subsetting and
    * missing-value handling have been resolved.  Applying it earlier would
    * let excluded trials influence the centering reference.
    */
  private final case class CenteringRequest(
      source: ColumnId,
      policy: CenteringPolicy,
      groupKeys: Vector[String] = Vector.empty
  ):
    def subset(mask: Vector[Boolean]): CenteringRequest =
      if groupKeys.isEmpty then this
      else copy(groupKeys = groupKeys.zip(mask).collect { case (key, keep) if keep => key })

  private final case class CenteredEvents(
      events: Vector[Event],
      receipts: Vector[CenteringReceipt]
  )

  private final case class OrthogonalizedEvents(
      events: Vector[Event],
      receipts: Vector[OrthogonalizationReceipt]
  )

  /** Exact location of one sibling parametric-modulator column. */
  private final case class ModulatorColumn(
      continuousEventIndex: Int,
      columnIndex: Int,
      id: ModulatorId
  )

  private final case class CompiledTerms(
      terms: Vector[EventModelTerm],
      contrastRefs: Vector[Option[String]],
      diagnostics: Vector[EventModelDiagnostic],
      missingValues: Vector[MissingValueResolution] = Vector.empty,
      centeringReceipts: Vector[CenteringReceipt] = Vector.empty,
      degenerateModulatorReceipts: Vector[DegenerateModulatorReceipt] = Vector.empty,
      orthogonalizationReceipts: Vector[OrthogonalizationReceipt] = Vector.empty,
      policyReceipts: Vector[PolicyReceipt] = Vector.empty,
      factorLevels: Vector[FactorLevelAudit] = Vector.empty,
      emptyCells: Vector[CellKey] = Vector.empty,
      emptyCellAudits: Vector[EmptyCellAudit] = Vector.empty,
      responseSupport: Vector[EventSupportReceipt] = Vector.empty
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
      hrfTermCount = formula.terms.count {
        case _: HrfCall => true
        case _          => false
      }
      _ <-
        if options.hrfByCell.nonEmpty && hrfTermCount != 1 then
          Left(
            DesignError.InvalidSchema(
              s"hrfByCell requires exactly one HRF term, found $hrfTermCount; use hrfByPhase for multiphase formulas"
            )
          )
        else Right(())
      declaredPhases = formula.terms.collect { case term: HrfCall => term.phase.map(_.id) }.flatten
      _ <- options.hrfByPhase.fold[Either[DesignError, Unit]](Right(()))(_.validateCoverage(declaredPhases))
      compiled <- compileTerms(formula, env, samplingFrame, schedule, options, extensions)
      _ <- validateFactorRegistryCoverage(options.factorRegistry, compiled.factorLevels)
      realizedTerms = compiled.terms.flatMap(_.keyHint.flatMap(value => TermId(value).toOption))
      _ <- options.orthogonalization.validateCoverage(realizedTerms)
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
    val missingValues = Vector.newBuilder[MissingValueResolution]
    val centeringReceipts = Vector.newBuilder[CenteringReceipt]
    val degenerateModulatorReceipts = Vector.newBuilder[DegenerateModulatorReceipt]
    val orthogonalizationReceipts = Vector.newBuilder[OrthogonalizationReceipt]
    val policyReceipts = Vector.newBuilder[PolicyReceipt]
    val factorLevels = Vector.newBuilder[FactorLevelAudit]
    val emptyCells = Vector.newBuilder[CellKey]
    val emptyCellAudits = Vector.newBuilder[EmptyCellAudit]
    val responseSupport = Vector.newBuilder[EventSupportReceipt]
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
          missingValues ++= compiled.missingValues
          centeringReceipts ++= compiled.centeringReceipts
          degenerateModulatorReceipts ++= compiled.degenerateModulatorReceipts
          orthogonalizationReceipts ++= compiled.orthogonalizationReceipts
          policyReceipts ++= compiled.policyReceipts
          factorLevels ++= compiled.factorLevels
          emptyCells ++= compiled.emptyCells
          emptyCellAudits ++= compiled.emptyCellAudits
          responseSupport ++= compiled.responseSupport
      i += 1

    failed match
      case Some(error) => Left(error)
      case None =>
        Right(
          CompiledTerms(
            terms.result(),
            contrastRefs.result(),
            diagnostics.result(),
            missingValues.result(),
            centeringReceipts.result(),
            degenerateModulatorReceipts.result(),
            orthogonalizationReceipts.result(),
            policyReceipts.result(),
            factorLevels.result(),
            emptyCells.result(),
            emptyCellAudits.result(),
            responseSupport.result()
          )
        )

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
      expressions <- toEvents(h.vars, env.eventData, termTag, options.factorRegistry, schedule.blockIds0)
      events0 = expressions.map(_.event)
      centering0 = expressions.map(_.centering)
      initialBasisDiagnostics = expressions.flatMap(_.diagnostics)
      termOnsets <- h.onsets.fold[Either[DesignError, Vector[Seconds]]](Right(schedule.defaultOnsets)) { ref =>
        resolveSecondsEither(ref, env.eventData, nEvents, argName = "onsets")
      }
      termDurs <- h.durations.fold[Either[DesignError, Vector[Seconds]]](Right(schedule.defaultDurs)) { ref =>
        resolveSecondsEither(ref, env.eventData, nEvents, argName = "durations")
      }
      phase0 <- resolvePhase(h.phase, env.eventData, nEvents)
      subsetMask <- resolveSubsetMaskEither(h.subset, env.eventData, nEvents)
      formulaData = if subsetMask.forall(identity) then env.eventData else env.eventData.filterRows(subsetMask)
      rows0 = TermRows(events0, termOnsets, termDurs, schedule.blockIds0, Vector.tabulate(nEvents)(identity))
      formulaRows = if subsetMask.forall(identity) then rows0 else subsetTerm(rows0, subsetMask)
      formulaPhase <- resolveEventPhase(phase0, formulaRows)
      supportReceipt <- assessResponseSupport(formulaRows, formulaPhase, termTag, options.responseSupport) { potential =>
        convolveHrfTermEither(h, potential, formulaData, samplingFrame, options, extensions)
      }
      supportMask = supportReceipt.fold(Vector.fill(formulaRows.sourceRows.length)(true))(_.keepMask)
      originalSub0 = if supportMask.forall(identity) then formulaData else formulaData.filterRows(supportMask)
      supportedRows = if supportMask.forall(identity) then formulaRows else subsetTerm(formulaRows, supportMask)
      selectedExpressions <- if options.responseSupport.isEmpty then Right(Vector.empty[EventExpression])
        else toEvents(h.vars, originalSub0, termTag, options.factorRegistry, supportedRows.blockIds)
      subset0 = if options.responseSupport.isEmpty then supportedRows else supportedRows.copy(events = selectedExpressions.map(_.event))
      basisDiagnostics = if options.responseSupport.isEmpty then initialBasisDiagnostics else selectedExpressions.flatMap(_.diagnostics)
      phase0s <- resolveEventPhase(phase0, subset0)
      centering0s = if options.responseSupport.isEmpty then subsetCenteringRequests(subsetCenteringRequests(centering0, subsetMask), supportMask)
        else selectedExpressions.map(_.centering)
      missingRows = nonFiniteRows(subset0.events)
      dropMissingMask = options.missingValuePolicy match
        case MissingValuePolicy.DropFromTerm => missingRows.map(!_)
        case _ => Vector.empty
      originalSub =
        if dropMissingMask.isEmpty || dropMissingMask.forall(identity) then originalSub0
        else originalSub0.filterRows(dropMissingMask)
      subset =
        if dropMissingMask.isEmpty || dropMissingMask.forall(identity) then subset0
        else subsetTerm(subset0, dropMissingMask)
      phase <- resolveEventPhase(phase0, subset)
      centeringS = subsetCenteringRequests(centering0s, dropMissingMask)
      factorAudits0 = factorLevelAudits(
        subset.events,
        options.factorRegistry,
        subset.blockIds,
        schedule.blockIds0.distinct.sorted
      )
      droppedMissing =
        if options.missingValuePolicy == MissingValuePolicy.DropFromTerm then
          missingValueResolutions(
            subset0.events,
            options.missingValuePolicy,
            action = "dropped-event",
            provenance = phase0s.toVector.flatMap(_.provenance)
          )
        else Vector.empty
      centered <- applyCentering(subset.events, centeringS, subset.blockIds)
      rawDegenerateModulators <- classifyDegenerateModulators(
        events = centered.events,
        termTag = termTag,
        sourceRows = subset.sourceRows,
        parentTrials = phase.toVector.flatMap(_.parentTrialIds),
        policy = options.degenerateModulatorPolicy
      )
      cleaned <- sanitizeContinuousEvents(
        centered.events,
        termTag,
        options.missingValuePolicy,
        provenance = phase.toVector.flatMap(_.provenance)
      )
      _ <- validateDegenerateModulatorReceipts(rawDegenerateModulators)
      orthogonalized <- applyOrthogonalization(
        events = cleaned.events,
        termTag = termTag,
        blockIds = subset.blockIds,
        sourceRows = subset.sourceRows,
        parentTrials = phase.toVector.flatMap(_.parentTrialIds),
        policy = options.orthogonalization.forTerm(termTag)
      )
      eventsClean = orthogonalized.events
      nonFiniteDiagnostics = cleaned.diagnostics
      term <- eventTermEither(
        events = eventsClean,
        onsets = subset.onsets,
        durations = subset.durations,
        blockIds = subset.blockIds,
        termTag = termTag,
        sourceRows = subset.sourceRows,
        phase = phase
      )
      emptyCells = emptyCellsFor(term)
      _ <- validateEmptyCellPolicy(termTag, emptyCells, options.emptyCellPolicy)
      runEmptyCells = emptyCellsByRunFor(term, options.emptyCellPolicy)
      _ <- validateRunEmptyCellPolicy(termTag, runEmptyCells, options.emptyCellPolicy)
      globalEmptyCellAudits = emptyCells.map { cell =>
        EmptyCellAudit(
          term = termTag.flatMap(value => TermId(value).toOption),
          cell = cell,
          policy = options.emptyCellPolicy,
          disposition = if effectiveDropEmpty(options) then EmptyCellDisposition.Omitted else EmptyCellDisposition.RetainedZero
        )
      }
      runEmptyCellAudits = runEmptyCells.flatMap { scoped =>
        scoped.cells.map { cell =>
          EmptyCellAudit(
            term = termTag.flatMap(value => TermId(value).toOption),
            cell = cell,
            policy = options.emptyCellPolicy,
            disposition = if effectiveDropEmpty(options) then EmptyCellDisposition.Omitted else EmptyCellDisposition.RetainedZero,
            scope = EmptyCellScope.Run(scoped.run)
          )
        }
      }
      emptyCellAudits = globalEmptyCellAudits ++ runEmptyCellAudits
      termDiagnostics = basisDiagnostics ++ nonFiniteDiagnostics ++ diagnoseTerm(term, samplingFrame)
      _ <- validateStrictDiagnostics(termDiagnostics, options.strict)
      conv <- convolveHrfTermEither(h, term, originalSub, samplingFrame, options, extensions)
      finalSupport <- assessResponseSupport(subset.copy(events = eventsClean), phase, termTag, options.responseSupport) { potential =>
        convolveHrfTermEither(h, potential, originalSub, samplingFrame, options, extensions)
      }
      _ <- if finalSupport.exists(_.keepMask.contains(false)) then
        Left(DesignError.InvalidSchedule("Response support changed after event selection; remaining rows cannot be silently excluded"))
        else Right(())
      supportEvidence = supportReceipt.map { initial =>
        val finalRows = finalSupport.toVector.flatMap(_.decisions).map(row => row.sourceRow -> row).toMap
        initial.copy(decisions = initial.decisions.map(row =>
          if row.disposition == scalafim.fmri.design.EventSupportDisposition.Retained then finalRows.getOrElse(row.sourceRow, row) else row))
      }
      schemaBindingReceipt = options.factorSchemaBinding.toVector.map { binding =>
        PolicyReceipt("factor-schema-binding", binding.canonical)
      }
      hrfCellReceipt = options.hrfByCell.toVector.map { assignments =>
        PolicyReceipt("hrf-by-cell", assignments.canonical)
      }
      hrfPhaseReceipt = options.hrfByPhase.toVector.flatMap { assignments =>
        term.phaseId.map { phase =>
          PolicyReceipt("hrf-by-phase", s"phase=${phase.value};${assignments.canonical}")
        }
      }
    yield CompiledTerm(
      conv,
      h.contrasts,
      termDiagnostics,
      responseSupport = supportEvidence.toVector,
      missingValues = droppedMissing ++ cleaned.missingValues,
      centeringReceipts = centered.receipts,
      degenerateModulatorReceipts = rawDegenerateModulators,
      orthogonalizationReceipts = orthogonalized.receipts,
      policyReceipts = schemaBindingReceipt ++ hrfCellReceipt ++ hrfPhaseReceipt ++ Vector(
        PolicyReceipt(
          "modulator-missing-values",
          s"term=${termTag.getOrElse("term")};policy=${options.missingValuePolicy.label}"
        ),
        PolicyReceipt(
          "factor-levels",
          factorAudits0.map(_.canonical).mkString("term=", ";", "")
        ),
        PolicyReceipt(
          "degenerate-modulators",
          s"term=${termTag.getOrElse("term")};policy=${options.degenerateModulatorPolicy.label}"
        ),
        PolicyReceipt(
          "modulator-orthogonalization",
          options.orthogonalization.forTerm(termTag).fold(s"term=${termTag.getOrElse("term")};policy=none")(_.canonical)
        ),
        PolicyReceipt(
          "empty-cells",
          s"term=${termTag.getOrElse("term")};policy=${options.emptyCellPolicy.label};effective=${if effectiveDropEmpty(options) then "omit" else "retain-zero"};" +
            s"cells=${emptyCells.map(_.canonical).mkString(",")};runs=${runEmptyCells.map(_.canonical).mkString("|")}"
        )
      ),
      factorLevels = factorAudits0,
      emptyCells = emptyCells,
      emptyCellAudits = emptyCellAudits
    )

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
      basisName = t.basis.getOrElse("spmg1")
      hrf0 <- resolveHrfBasisEither(basisName, nbasis = t.nbasis, lag = t.lag)
      rows0 = TermRows(Vector(trialEvent), schedule.defaultOnsets, termDurs, schedule.blockIds0, Vector.tabulate(nEvents)(identity))
      supportReceipt <- assessResponseSupport(rows0, None, Some(termTag), options.responseSupport) { potential =>
        catchBuild(DesignError.fromThrowable) {
          potential.convolve(hrf0, samplingFrame, precision = options.precision,
            dropEmpty = effectiveDropEmpty(options), summate = options.summate)
        }
      }
      rows = supportReceipt.fold(rows0)(receipt => subsetTerm(rows0, receipt.keepMask))
      term <- eventTermEither(rows.events, rows.onsets, rows.durations, rows.blockIds, Some(termTag), rows.sourceRows)
      termDiagnostics = diagnoseTerm(term, samplingFrame)
      _ <- validateStrictDiagnostics(termDiagnostics, options.strict)
      conv0 <- catchBuild(DesignError.fromThrowable) {
        term.convolve(
          hrf0,
          samplingFrame,
          precision = options.precision,
          dropEmpty = effectiveDropEmpty(options),
          summate = options.summate,
          scaling = effectiveScaling(t.scaling, t.normalize)
        )
      }
      conv = conv0.copy(
        role = EventTermRole.Trialwise,
        columnRoles = Vector.fill(conv0.columnNames.length)(EventTermColumnRole.Trial)
      )
      out = if t.addSum.getOrElse(false) then addMeanColumn(conv, label = label0) else conv
    yield CompiledTerm(out, None, termDiagnostics, responseSupport = supportReceipt.toVector)

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
      diagnosed <- model0.withDiagnosticsEither(compiled.diagnostics)
      evidenced <- diagnosed.withPolicyEvidenceEither(
        compiled.missingValues,
        compiled.policyReceipts,
        factorLevels = compiled.factorLevels,
        emptyCells = compiled.emptyCells,
        emptyCellAudits = compiled.emptyCellAudits,
        centering = compiled.centeringReceipts,
        degenerateModulators = compiled.degenerateModulatorReceipts,
        orthogonalization = compiled.orthogonalizationReceipts
      )
      supported <- evidenced.withResponseSupportEvidence(compiled.responseSupport)
    yield supported.copy(contrastSetsByTerm = attached)

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

  private def assessResponseSupport(
      rows: TermRows,
      phase: Option[EventPhase],
      termTag: Option[String],
      request: Option[ResponseSupportRequest]
  )(convolve: EventTerm => Either[DesignError, ConvolvedTerm]): Either[DesignError, Option[EventSupportReceipt]] =
    request match
      case None => Right(None)
      case Some(policy) =>
        val potentialEvents = rows.events.map {
          case continuous: ContinuousEvent =>
            continuous.copy(value = scalafim.fmri.hrf.linalg.Mat.unsafe(continuous.value.rows, continuous.value.cols,
              Array.fill(continuous.value.rows * continuous.value.cols)(1.0)))
          case categorical: CategoricalEvent => categorical
        }
        for
          term <- eventTermEither(potentialEvents, rows.onsets, rows.durations, rows.blockIds, termTag, rows.sourceRows, phase)
          potential <- convolve(term)
          receipt <- EventResponseSupport.assess(potential, policy)
        yield Some(receipt)

  private def eventTermEither(
      events: Vector[Event],
      onsets: Vector[Seconds],
      durations: Vector[Seconds],
      blockIds: Vector[Int],
      termTag: Option[String],
      sourceRows: Vector[Int],
      phase: Option[EventPhase] = None
  ): Either[DesignError, EventTerm] =
    phase match
      case None =>
        EventTerm.validated(
          events = events,
          onsets = onsets,
          durations = durations,
          blockIds = blockIds,
          termTag = termTag
        ).map(_.copy(sourceRows = sourceRows))
      case Some(value) =>
        value.term(events, termTag).map(_.copy(sourceRows = sourceRows))

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

  private def resolvePhase(
      phase: Option[PhaseRef],
      data: DataTable,
      nEvents: Int
  ): Either[DesignError, Option[ResolvedPhase]] =
    phase match
      case None => Right(None)
      case Some(ref) =>
        data.column(ref.parent).flatMap { column =>
          val valuesEither: Either[DesignError, Vector[String]] =
            column match
              case Column.Strings(values) => Right(values)
              case Column.Ints(values)    => Right(values.map(_.toString))
              case Column.Doubles(values) if values.forall(_.isFinite) => Right(values.map(_.toString))
              case Column.Doubles(_) => Left(DesignError.InvalidColumnType(ref.parent.value, "finite trial identifiers", "numeric with non-finite values"))
              case Column.Bools(values) => Right(values.map(_.toString))
              case other => Left(DesignError.InvalidColumnType(ref.parent.value, "scalar trial identifiers", other.typeName))
          valuesEither.flatMap { values =>
            if values.length != nEvents then
              Left(DesignError.InvalidSchedule(s"parent column '${ref.parent.value}' has length ${values.length} but expected $nEvents"))
            else
              val out = Vector.newBuilder[TrialId]
              var failed: Option[DesignError] = None
              var i = 0
              while i < values.length && failed.isEmpty do
                TrialId(values(i)) match
                  case Left(error) => failed = Some(error)
                  case Right(id)   => out += id
                i += 1
              failed match
                case Some(error) => Left(error)
                case None        => Right(Some(ResolvedPhase(ref.id, out.result())))
          }
        }

  /** Materialize selected phase rows once after row policies have been
    * applied. Policy receipts and the final event term then share one checked
    * source identity instead of indexing parallel provenance vectors.
    */
  private def resolveEventPhase(
      phase: Option[ResolvedPhase],
      rows: TermRows
  ): Either[DesignError, Option[EventPhase]] =
    phase match
      case None => Right(None)
      case Some(value) =>
        EventPhase.fromParts(
          id = value.id,
          onsets = rows.onsets,
          durations = rows.durations,
          blockIds = rows.blockIds,
          parentTrialIds = rows.sourceRows.map(value.parentTrialIds),
          sourceRows = rows.sourceRows
        ).map(Some(_))

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
    val scaling0 = effectiveScaling(h.scaling, h.normalize)

    val assigned =
      options.hrfByCell.map(HrfAssignment.ByCell.apply) match
        case some @ Some(_) => Right(some)
        case None =>
          (options.hrfByPhase, term.phaseId) match
            case (Some(plan), Some(phase)) => plan.resolve(phase).map(Some(_))
            case _                         => Right(None)

    assigned.flatMap {
      case Some(_) if h.hrfFun.nonEmpty =>
        Left(DesignError.InvalidSchema("explicit HRF assignment cannot be combined with hrf_fun on the same term"))
      case Some(_) if h.basis.nonEmpty || h.lag.nonEmpty || h.nbasis.nonEmpty =>
        Left(DesignError.InvalidSchema("explicit HRF assignment cannot be combined with formula basis, lag, or nbasis on the same term"))
      case Some(HrfAssignment.Shared(assignedHrf)) =>
        catchBuild(DesignError.fromThrowable) {
          term.convolve(
            assignedHrf,
            samplingFrame,
            precision = options.precision,
            dropEmpty = effectiveDropEmpty(options),
            summate = summate0,
            scaling = scaling0
          )
        }
      case Some(HrfAssignment.ByCell(assignments)) =>
        convolveByCell(term, assignments, samplingFrame, options, summate0, scaling0)
      case None =>
        (h.hrfFun, term.onsets.nonEmpty) match
          case (Some(ref), true) =>
            for
              eventData <- catchBuild(DesignError.fromThrowable)(buildEventDataForGenerator(term, originalSub))
              hsel <- resolveHrfFunEither(ref, eventData, hrfFuns = extensions.hrfFuns, termTag = term.termTag)
              conv <- catchBuild(DesignError.fromThrowable) {
                hsel match
                  case Left(shared) =>
                    term.convolve(shared, samplingFrame, precision = options.precision, dropEmpty = effectiveDropEmpty(options), summate = summate0, scaling = scaling0)
                  case Right(perEvent) =>
                    term.convolvePerEvent(perEvent, samplingFrame, precision = options.precision, dropEmpty = effectiveDropEmpty(options), summate = summate0, scaling = scaling0)
              }
            yield conv
          case _ =>
            for
              hrf0 <- resolveHrfEither(h, options.defaultHrf)
              conv <- catchBuild(DesignError.fromThrowable) {
                term.convolve(hrf0, samplingFrame, precision = options.precision, dropEmpty = effectiveDropEmpty(options), summate = summate0, scaling = scaling0)
              }
            yield conv
    }

  private def convolveByCell(
      term: EventTerm,
      assignments: HrfByCell,
      samplingFrame: SamplingFrame,
      options: BuildOptions,
      summate: Boolean,
      scaling: HrfColumnScaling
  ): Either[DesignError, ConvolvedTerm] =
    val schemaCells = term.conditionProvenance(dropEmpty = false).map(_.cell)
    val realizedCells = term.conditionProvenance(dropEmpty = effectiveDropEmpty(options)).map(_.cell)
    assignments.resolve(schemaCells, term.termTag.getOrElse("term")).flatMap { schemaHrfs =>
      val byCell = schemaCells.zip(schemaHrfs).toMap
      val hrfs = realizedCells.map(byCell)
      catchBuild(DesignError.fromThrowable) {
        term.convolveByCondition(
          hrfs,
          samplingFrame,
          precision = options.precision,
          dropEmpty = effectiveDropEmpty(options),
          summate = summate,
          scaling = scaling
        )
      }
    }

  private def effectiveScaling(
      scaling: Option[HrfColumnScaling],
      normalize: Option[Boolean]
  ): HrfColumnScaling =
    scaling.getOrElse {
      if normalize.contains(true) then HrfColumnScaling.UnitMaximumAbsolute
      else HrfColumnScaling.AsConvolved
    }

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

  private def classifyDegenerateModulators(
      events: Vector[Event],
      termTag: Option[String],
      sourceRows: Vector[Int],
      parentTrials: Vector[TrialId],
      policy: DegenerateModulatorPolicy,
      tolerance: Double = 1e-8
  ): Either[DesignError, Vector[DegenerateModulatorReceipt]] =
    val eventRows = events.headOption.map(_.nEvents).getOrElse(sourceRows.length)
    if sourceRows.length != eventRows then
      Left(DesignError.InvalidSchema("degenerate-modulator source rows do not align with term rows"))
    else if parentTrials.nonEmpty && parentTrials.length != eventRows then
      Left(DesignError.InvalidSchema("degenerate-modulator parent trials do not align with term rows"))
    else
      val term = TermId.unsafe(termTag.getOrElse("term"))
      val receipts = Vector.newBuilder[DegenerateModulatorReceipt]
      events.foreach {
        case event: ContinuousEvent =>
          var column = 0
          while column < event.value.cols do
            val finiteRows = Vector.newBuilder[Int]
            var min = Double.PositiveInfinity
            var max = Double.NegativeInfinity
            var maxAbs = 0.0
            var allZero = true
            var row = 0
            while row < event.value.rows do
              val value = event.value(row, column)
              if value.isFinite then
                finiteRows += row
                if value < min then min = value
                if value > max then max = value
                val absolute = math.abs(value)
                if absolute > maxAbs then maxAbs = absolute
                if absolute > tolerance then allZero = false
              row += 1

            val finite = finiteRows.result()
            val outcome =
              if event.value.rows == 0 then Some(DegenerateModulatorOutcome.EmptyScope)
              else if finite.isEmpty then Some(DegenerateModulatorOutcome.AllNonFinite)
              else if allZero then Some(DegenerateModulatorOutcome.AllZero)
              else if (max - min) <= tolerance * math.max(1.0, maxAbs) then
                Some(DegenerateModulatorOutcome.Constant)
              else None

            outcome.foreach { value =>
              receipts += DegenerateModulatorReceipt(
                term = term,
                modulator = event.modulatorIds(column),
                sourceRows = sourceRows,
                finiteSourceRows = finite.map(sourceRows),
                parentTrials = parentTrials,
                outcome = value,
                policy = policy
              )
            }
            column += 1
        case _ => ()
      }
      Right(receipts.result())

  private def validateDegenerateModulatorReceipts(
      receipts: Vector[DegenerateModulatorReceipt]
  ): Either[DesignError, Unit] =
    receipts.find(_.policy == DegenerateModulatorPolicy.Reject) match
      case None => Right(())
      case Some(receipt) =>
        Left(
          DesignError.DegenerateModulator(
            receipt.term.value,
            receipt.modulator,
            receipt.outcome.label,
            receipt.policy
          )
        )

  private def factorLevelAudits(
      events: Vector[Event],
      registry: FactorLevelRegistry,
      blockIds: Vector[Int],
      partitions: Vector[Int]
  ): Vector[FactorLevelAudit] =
    events.collect { case categorical: CategoricalEvent =>
      val factor = FactorId.unsafe(categorical.varName)
      def levelsFor(codes: Vector[Int]): Vector[LevelId] =
        categorical.levels.indices
          .filter(codes.contains)
          .map(index => LevelId.unsafe(categorical.levels(index)))
          .toVector
      val observed = levelsFor(categorical.codes)
      val partitionAudits = partitions.map { partition =>
        val codes = blockIds.zip(categorical.codes).collect { case (block, code) if block == partition => code }
        FactorPartitionAudit(
          partition = s"run-${partition + 1}",
          observed = levelsFor(codes)
        )
      }
      FactorLevelAudit(
        factor = factor,
        declared = registry.get(factor).map(_.levels),
        observed = observed,
        partitions = partitionAudits
      )
    }

  private def emptyCellsFor(term: EventTerm): Vector[CellKey] =
    val cells = TermCells.from(term, dropEmpty = false)
    if cells.vars.isEmpty then Vector.empty
    else
      cells.rows.collect {
        case row if row.count == 0 =>
          CellKey.unsafe(
            cells.vars.zip(row.levels).map { case (factor, level) =>
              CellAssignment(FactorId.unsafe(factor), LevelId.unsafe(level))
            }
          )
      }

  private final case class RunEmptyCells(run: RunIndex, cells: Vector[CellKey]):
    require(cells.nonEmpty, "a run empty-cell scope must contain at least one cell")

    def canonical: String =
      s"run-${run.oneBased}[${cells.map(_.canonical).mkString(",")}]"

  /** Find cells missing within a run even when they are represented elsewhere
    * in the same compiled term.  The legacy drop-empty compatibility mode did
    * not define a run-local policy, so it deliberately retains the historical
    * non-estimable projection behavior.
    */
  private def emptyCellsByRunFor(
      term: EventTerm,
      policy: EmptyCellPolicy
  ): Vector[RunEmptyCells] =
    if policy == EmptyCellPolicy.UseDropEmptyFlag then Vector.empty
    else
      val categorical = term.events.collect { case event: CategoricalEvent => event }
      if categorical.isEmpty then Vector.empty
      else
        val declared = TermCells.from(term, dropEmpty = false).rows.map { row =>
          CellKey.unsafe(
            categorical.zip(row.levels).map { case (event, level) =>
              CellAssignment(FactorId.unsafe(event.varName), LevelId.unsafe(level))
            }
          )
        }
        term.blockIds0.distinct.sorted.flatMap { block =>
          val observed = term.blockIds0.indices.collect {
            case eventIndex if term.blockIds0(eventIndex) == block =>
              CellKey.unsafe(
                categorical.map { event =>
                  val code = event.codes(eventIndex)
                  CellAssignment(FactorId.unsafe(event.varName), LevelId.unsafe(event.levels(code)))
                }
              )
          }.toSet
          val missing = declared.filterNot(observed.contains)
          if missing.isEmpty then None
          else Some(RunEmptyCells(RunIndex.unsafeOneBased(block + 1), missing))
        }

  private def validateEmptyCellPolicy(
      termTag: Option[String],
      emptyCells: Vector[CellKey],
      policy: EmptyCellPolicy
  ): Either[DesignError, Unit] =
    policy match
      case EmptyCellPolicy.Reject =>
        emptyCells.headOption match
          case Some(cell) =>
            Left(
              DesignError.EmptyFactorCell(
                termTag.getOrElse("term"),
                cell.canonical,
                policy.label
              )
            )
          case None => Right(())
      case _ => Right(())

  private def validateRunEmptyCellPolicy(
      termTag: Option[String],
      emptyCells: Vector[RunEmptyCells],
      policy: EmptyCellPolicy
  ): Either[DesignError, Unit] =
    policy match
      case EmptyCellPolicy.Reject =>
        emptyCells.collectFirst { case RunEmptyCells(run, cell +: _) =>
          DesignError.EmptyFactorCellInRun(
            term = termTag.getOrElse("term"),
            cell = cell,
            run = run,
            policy = policy
          )
        }.toLeft(())
      case _ => Right(())

  private def validateFactorRegistryCoverage(
      registry: FactorLevelRegistry,
      audits: Vector[FactorLevelAudit]
  ): Either[DesignError, Unit] =
    val used = audits.map(_.factor.value).toSet
    registry.factorIds.find(factor => !used.contains(factor.value)) match
      case Some(unused) =>
        Left(DesignError.InvalidSchema(s"declared factor '${unused.value}' is not used by any event term"))
      case None => Right(())

  private def effectiveDropEmpty(options: BuildOptions): Boolean =
    options.emptyCellPolicy match
      case EmptyCellPolicy.UseDropEmptyFlag => options.dropEmpty
      case EmptyCellPolicy.Omit             => true
      case EmptyCellPolicy.RetainZero       => false
      case EmptyCellPolicy.Reject           => true

  private def subsetCenteringRequests(
      requests: Vector[Vector[CenteringRequest]],
      mask: Vector[Boolean]
  ): Vector[Vector[CenteringRequest]] =
    if mask.isEmpty || mask.forall(identity) then requests
    else requests.map(_.map(_.subset(mask)))

  private def applyCentering(
      events: Vector[Event],
      requests: Vector[Vector[CenteringRequest]],
      blockIds: Vector[Int]
  ): Either[DesignError, CenteredEvents] =
    if events.length != requests.length then
      Left(DesignError.InvalidSchema("centering request count does not match event count"))
    else
      val out = Vector.newBuilder[Event]
      val receipts = Vector.newBuilder[CenteringReceipt]
      var i = 0
      while i < events.length do
        (events(i), requests(i)) match
          case (event: ContinuousEvent, eventRequests) =>
            var current = event
            var requestIndex = 0
            var failed: Option[DesignError] = None
            while requestIndex < eventRequests.length && failed.isEmpty do
              centerEvent(current, eventRequests(requestIndex), blockIds) match
                case Left(error) => failed = Some(error)
                case Right((centered, receipt)) =>
                  current = centered
                  receipts += receipt
              requestIndex += 1
            failed match
              case Some(error) => return Left(error)
              case None        => out += current
          case (_: CategoricalEvent, eventRequests) if eventRequests.isEmpty =>
            out += events(i)
          case (_: CategoricalEvent, _) =>
            return Left(DesignError.InvalidColumnType(events(i).varName, "continuous event for centering", "categorical"))
        i += 1
      Right(CenteredEvents(out.result(), receipts.result()))

  private def applyOrthogonalization(
      events: Vector[Event],
      termTag: Option[String],
      blockIds: Vector[Int],
      sourceRows: Vector[Int],
      parentTrials: Vector[TrialId],
      policy: Option[ModulatorOrthogonalization]
  ): Either[DesignError, OrthogonalizedEvents] =
    policy match
      case None => Right(OrthogonalizedEvents(events, Vector.empty))
      case Some(value) =>
        val term = termTag.getOrElse(value.term.value)
        val continuousEvents = events.zipWithIndex.collect {
          case (event: ContinuousEvent, originalEventIndex) => originalEventIndex -> event
        }
        val columns = continuousEvents.zipWithIndex.flatMap {
          case ((_, event), continuousEventIndex) =>
            event.modulatorIds.zipWithIndex.map { case (id, columnIndex) =>
              ModulatorColumn(continuousEventIndex, columnIndex, id)
            }
        }
        for
          orderedColumns <- resolveOrderedModulatorColumns(columns, value.order, term)
          _ <-
            if sourceRows.length == blockIds.length then Right(())
            else Left(DesignError.InvalidOrthogonalization(term, "source rows do not align with term rows"))
          _ <-
            if parentTrials.isEmpty || parentTrials.length == blockIds.length then Right(())
            else Left(DesignError.InvalidOrthogonalization(term, "parent trials do not align with term rows"))
          groups <- orthogonalizationGroups(events, blockIds, value.scope, term)
          _ <-
            if groups.nonEmpty then Right(())
            else Left(DesignError.InvalidOrthogonalization(term, "ordered orthogonalization has an empty event scope"))
          lowered <- orthogonalizeColumns(
            events = events,
            continuousEvents = continuousEvents,
            orderedColumns = orderedColumns,
            policy = value,
            groups = groups,
            sourceRows = sourceRows,
            parentTrials = parentTrials,
            term = term
          )
        yield lowered

  private def resolveOrderedModulatorColumns(
      columns: Vector[ModulatorColumn],
      order: Vector[ModulatorId],
      term: String
  ): Either[DesignError, Vector[ModulatorColumn]] =
    val occurrences = columns.groupBy(_.id)
    val resolved = Vector.newBuilder[ModulatorColumn]
    var index = 0
    var failed: Option[DesignError] = None
    while index < order.length && failed.isEmpty do
      val id = order(index)
      occurrences.getOrElse(id, Vector.empty) match
        case Vector(column) => resolved += column
        case Vector() =>
          failed = Some(
            DesignError.InvalidOrthogonalization(
              term,
              s"ordered modulator '${id.value}' is absent (available: ${occurrences.keys.toVector.map(_.value).sorted.mkString(", ")})"
            )
          )
        case matches =>
          failed = Some(
            DesignError.InvalidOrthogonalization(
              term,
              s"ordered modulator '${id.value}' must identify exactly one sibling column, found ${matches.length}"
            )
          )
      index += 1
    failed.fold[Either[DesignError, Vector[ModulatorColumn]]](Right(resolved.result()))(Left(_))

  private def orthogonalizeColumns(
      events: Vector[Event],
      continuousEvents: Vector[(Int, ContinuousEvent)],
      orderedColumns: Vector[ModulatorColumn],
      policy: ModulatorOrthogonalization,
      groups: Vector[(String, Vector[Int])],
      sourceRows: Vector[Int],
      parentTrials: Vector[TrialId],
      term: String
  ): Either[DesignError, OrthogonalizedEvents] =
    var current = continuousEvents.map(_._2)
    val receipts = Vector.newBuilder[OrthogonalizationStepReceipt]
    var targetIndex = 1
    var failed: Option[DesignError] = None
    while targetIndex < policy.order.length && failed.isEmpty do
      val targetId = policy.order(targetIndex)
      val targetColumn = orderedColumns(targetIndex)
      val targetEvent = current(targetColumn.continuousEventIndex)
      val predecessors = policy.order.take(targetIndex)
      val referenceColumns = orderedColumns.take(targetIndex)
      val residualData = targetEvent.value.data.clone()
      val groupReceipts = Vector.newBuilder[OrthogonalizationGroupReceipt]
      var groupIndex = 0
      while groupIndex < groups.length && failed.isEmpty do
        val (key, rows) = groups(groupIndex)
        val reference = bindModulatorRows(current, referenceColumns, rows)
        val target = selectModulatorRows(current, targetColumn, rows)
        val qr = QrDecomposition.decomposeScaleAware(reference.data, reference.rows, reference.cols, pivoting = true)
        val residual = residualize(qr, target)
        writeModulatorRows(
          destination = residualData,
          columns = targetEvent.value.cols,
          targetColumn = targetColumn.columnIndex,
          rows = rows,
          values = residual
        )
        val sourceNorm = frobeniusNorm(target.data)
        val residualNorm = frobeniusNorm(residual.data)
        val degenerate = residualNorm == 0.0 || residualNorm <= policy.tolerance * sourceNorm
        val outcome =
          if degenerate then OrthogonalizationOutcome.DegenerateRetained
          else OrthogonalizationOutcome.Applied
        if degenerate && policy.degenerate == DegenerateModulatorPolicy.Reject then
          failed = Some(DesignError.DegenerateModulator(term, targetId, key, policy.degenerate))
        else
          groupReceipts += OrthogonalizationGroupReceipt(
            key = key,
            sourceRows = rows.map(sourceRows),
            parentTrials = if parentTrials.isEmpty then Vector.empty else rows.map(parentTrials),
            referenceRank = qr.rank,
            sourceNorm = sourceNorm,
            residualNorm = residualNorm,
            outcome = outcome
          )
        groupIndex += 1

      failed match
        case Some(_) => ()
        case None =>
          val updated = targetEvent.copy(
            value = scalafim.fmri.hrf.linalg.Mat.unsafe(targetEvent.value.rows, targetEvent.value.cols, residualData)
          )
          current = current.updated(targetColumn.continuousEventIndex, updated)
          receipts += OrthogonalizationStepReceipt(targetId, predecessors, groupReceipts.result())
      targetIndex += 1

    failed match
      case Some(error) => Left(error)
      case None =>
        val lowered = continuousEvents.zipWithIndex.foldLeft(events) {
          case (result, ((originalEventIndex, _), continuousEventIndex)) =>
            result.updated(originalEventIndex, current(continuousEventIndex))
        }
        Right(
          OrthogonalizedEvents(
            lowered,
            Vector(OrthogonalizationReceipt(policy, receipts.result()))
          )
        )

  private def orthogonalizationGroups(
      events: Vector[Event],
      blockIds: Vector[Int],
      scope: OrthogonalizationScope,
      term: String
  ): Either[DesignError, Vector[(String, Vector[Int])]] =
    scope match
      case OrthogonalizationScope.WholeTerm =>
        Right(if blockIds.isEmpty then Vector.empty else Vector("whole-term" -> blockIds.indices.toVector))
      case OrthogonalizationScope.WithinRun =>
        Right(groupIndices(blockIds.map(run => s"run-${run + 1}")))
      case OrthogonalizationScope.WithinCells(factors) =>
        val categorical = events.collect { case event: CategoricalEvent => FactorId.unsafe(event.varName) -> event }.toMap
        factors.find(factor => !categorical.contains(factor)) match
          case Some(missing) =>
            Left(DesignError.InvalidOrthogonalization(term, s"scope factor '${missing.value}' is absent from the term"))
          case None =>
            val scopedEvents = factors.flatMap(categorical.get)
            val keys = blockIds.indices.map { row =>
              factors.zip(scopedEvents).map { case (factor, event) =>
                s"${factor.value}=${event.levels(event.codes(row))}"
              }.mkString("cell:", ",", "")
            }.toVector
            Right(groupIndices(keys))

  private def groupIndices(keys: Vector[String]): Vector[(String, Vector[Int])] =
    keys.zipWithIndex.groupBy(_._1).toVector.sortBy(_._1).map { case (key, rows) =>
      key -> rows.map(_._2).sorted
    }

  private def bindModulatorRows(
      events: Vector[ContinuousEvent],
      columns: Vector[ModulatorColumn],
      rows: Vector[Int]
  ): scalafim.fmri.hrf.linalg.Mat =
    val out = new Array[Double](rows.length * columns.length)
    var outRow = 0
    while outRow < rows.length do
      val sourceRow = rows(outRow)
      var columnIndex = 0
      while columnIndex < columns.length do
        val column = columns(columnIndex)
        val event = events(column.continuousEventIndex)
        out(outRow * columns.length + columnIndex) = event.value(sourceRow, column.columnIndex)
        columnIndex += 1
      outRow += 1
    scalafim.fmri.hrf.linalg.Mat.unsafe(rows.length, columns.length, out)

  private def selectModulatorRows(
      events: Vector[ContinuousEvent],
      column: ModulatorColumn,
      rows: Vector[Int]
  ): scalafim.fmri.hrf.linalg.Mat =
    val event = events(column.continuousEventIndex)
    val out = new Array[Double](rows.length)
    var outRow = 0
    while outRow < rows.length do
      val sourceRow = rows(outRow)
      out(outRow) = event.value(sourceRow, column.columnIndex)
      outRow += 1
    scalafim.fmri.hrf.linalg.Mat.unsafe(rows.length, 1, out)

  private def residualize(
      qr: QrDecomposition,
      target: scalafim.fmri.hrf.linalg.Mat
  ): scalafim.fmri.hrf.linalg.Mat =
    val out = target.data.clone()
    qr.applyQtInPlace(out, target.cols)
    var row = 0
    while row < qr.rank do
      var column = 0
      while column < target.cols do
        out(row * target.cols + column) = 0.0
        column += 1
      row += 1
    qr.applyQInPlace(out, target.cols)
    scalafim.fmri.hrf.linalg.Mat.unsafe(target.rows, target.cols, out)

  private def writeModulatorRows(
      destination: Array[Double],
      columns: Int,
      targetColumn: Int,
      rows: Vector[Int],
      values: scalafim.fmri.hrf.linalg.Mat
  ): Unit =
    require(values.cols == 1, "internal: residualized modulator must be a single column")
    var outRow = 0
    while outRow < rows.length do
      destination(rows(outRow) * columns + targetColumn) = values(outRow, 0)
      outRow += 1

  private def frobeniusNorm(values: Array[Double]): Double =
    var sum = 0.0
    var index = 0
    while index < values.length do
      sum += values(index) * values(index)
      index += 1
    math.sqrt(sum)

  private def centerEvent(
      event: ContinuousEvent,
      request: CenteringRequest,
      blockIds: Vector[Int]
  ): Either[DesignError, (ContinuousEvent, CenteringReceipt)] =
    for
      _ <- request.policy.validate
      requestedModulator = ModulatorId.unsafe(request.source.value)
      matchingColumns = event.modulatorIds.zipWithIndex.collect {
        case (modulator, column) if modulator == requestedModulator => column
      }
      targetColumn <- matchingColumns match
        case Vector(column) => Right(column)
        case Vector() =>
          Left(
            DesignError.InvalidSchema(
              s"centering source '${request.source.value}' is not represented by continuous event '${event.varName}'"
            )
          )
        case columns =>
          Left(
            DesignError.InvalidSchema(
              s"centering source '${request.source.value}' identifies ${columns.length} columns in continuous event '${event.varName}'"
            )
          )
      centered <- centerEventColumn(event, request, blockIds, requestedModulator, targetColumn)
    yield centered

  private def centerEventColumn(
      event: ContinuousEvent,
      request: CenteringRequest,
      blockIds: Vector[Int],
      requestedModulator: ModulatorId,
      targetColumn: Int
  ): Either[DesignError, (ContinuousEvent, CenteringReceipt)] =
    val n = event.value.rows
    val keys: Vector[String] =
      request.policy match
        case CenteringPolicy.None => Vector.fill(n)("global")
        case CenteringPolicy.GrandMean | CenteringPolicy.At(_) => Vector.fill(n)("global")
        case CenteringPolicy.WithinRun =>
          if request.groupKeys.length == n then request.groupKeys else blockIds.map(b => b.toString)
        case CenteringPolicy.WithinFactor(_) | CenteringPolicy.WithinCells(_) =>
          request.groupKeys

    if keys.length != n then
      Left(
        DesignError.InvalidSchema(
          s"centering grouping has ${keys.length} rows but modulator '${event.varName}' has $n"
        )
      )
    else
      val grouped =
        if keys.isEmpty then Vector("empty-scope" -> Vector.empty[(String, Int)])
        else keys.zipWithIndex.groupBy(_._1).toVector.sortBy(_._1)
      val values = event.value.data
      val centered = values.clone()
      val groupReceipts = Vector.newBuilder[CenteringGroupReceipt]
      val affected = Vector.newBuilder[Int]
      var g = 0
      while g < grouped.length do
        val key = grouped(g)._1
        val indices = grouped(g)._2.map(_._2).sorted
        val finite = indices.filter(index => values(index * event.value.cols + targetColumn).isFinite)
        val reference =
          request.policy match
            case CenteringPolicy.At(value) => Some(value)
            case _ if finite.nonEmpty =>
              Some(finite.map(index => values(index * event.value.cols + targetColumn)).sum / finite.length.toDouble)
            case _ => None
        val outcome =
          if indices.isEmpty then CenteringOutcome.EmptyScope
          else if finite.isEmpty then CenteringOutcome.AllNonFinite
          else if reference.exists(ref => finite.forall(index => values(index * event.value.cols + targetColumn) == ref)) then CenteringOutcome.Constant
          else CenteringOutcome.Applied

        reference.foreach { ref =>
          var j = 0
          while j < finite.length do
            val index = finite(j)
            val offset = index * event.value.cols + targetColumn
            centered(offset) = values(offset) - ref
            affected += index
            j += 1
        }
        groupReceipts += CenteringGroupReceipt(
          key = key,
          eventIndices = indices,
          finiteEventIndices = finite,
          center = reference,
          outcome = outcome
        )
        g += 1

      val receipt =
        CenteringReceipt(
          modulator = requestedModulator,
          source = request.source,
          policy = request.policy,
          groups = groupReceipts.result(),
          affectedEvents = affected.result().distinct.sorted
        )
      Right(
        (
          event.copy(value = scalafim.fmri.hrf.linalg.Mat.unsafe(n, event.value.cols, centered)),
          receipt
        )
      )

  private final case class SanitizedEvents(
      events: Vector[Event],
      diagnostics: Vector[EventModelDiagnostic],
      missingValues: Vector[MissingValueResolution]
  )

  private def sanitizeContinuousEvents(
      events: Vector[Event],
      termTag: Option[String],
      policy: MissingValuePolicy,
      provenance: Vector[EventRowProvenance]
  ): Either[DesignError, SanitizedEvents] =
    val termLabel = termTag.getOrElse("term")
    val diagnostics = Vector.newBuilder[EventModelDiagnostic]
    val missingValues = Vector.newBuilder[MissingValueResolution]

    val cleaned = Vector.newBuilder[Event]
    var eventPosition = 0
    while eventPosition < events.length do
      val event = events(eventPosition)
      event match
        case e: ContinuousEvent =>
          val m = e.value
          var out = m.data
          var copied = false

          var c = 0
          while c < m.cols do
            var hasNonFinite = false
            var firstMissingRow: Option[Int] = None
            var r = 0
            while r < m.rows do
              val idx = r * m.cols + c
              val v = m.data(idx)
              if !v.isFinite then
                hasNonFinite = true
                if firstMissingRow.isEmpty then firstMissingRow = Some(r)
                val column = e.columnTags.lift(c).getOrElse(s"${e.varName}_${c + 1}")
                val modulator = e.modulatorIds(c)
                missingValues += MissingValueResolution(
                  modulator = modulator,
                  eventIndex = r,
                  policy = policy.label,
                  action = policy match
                    case MissingValuePolicy.ZeroContribution  => "zero-contribution"
                    case MissingValuePolicy.ImputeConstant(_) => "imputed-constant"
                    case MissingValuePolicy.DropFromTerm      => "dropped-event"
                    case MissingValuePolicy.Reject            => "rejected",
                  column = Some(column),
                  source = provenance.lift(r)
                )
                policy match
                  case MissingValuePolicy.Reject =>
                    return Left(DesignError.MissingModulatorValue(termLabel, column, r, policy.label))
                  case MissingValuePolicy.DropFromTerm =>
                    return Left(DesignError.InvalidSchema("drop-from-term must remove non-finite rows before sanitization"))
                  case MissingValuePolicy.ZeroContribution =>
                    if !copied then
                      out = m.data.clone()
                      copied = true
                    out(idx) = 0.0
                  case MissingValuePolicy.ImputeConstant(value) =>
                    if !copied then
                      out = m.data.clone()
                      copied = true
                    out(idx) = value
              r += 1

            if hasNonFinite then
              val colLabel =
                if m.cols == 1 then e.varName
                else e.columnTags.lift(c).getOrElse(s"${e.varName}_${c + 1}")
              diagnostics += EventModelDiagnostic(
                EventModelDiagnosticKind.NonFiniteModulator,
                termLabel,
                s"NA or non-finite values detected in continuous modulator '$colLabel' in term '$termLabel'; policy=${policy.label}",
                eventIndex = firstMissingRow,
                column = Some(colLabel)
              )
            c += 1

          cleaned += (if copied then e.copy(value = scalafim.fmri.hrf.linalg.Mat.unsafe(m.rows, m.cols, out)) else e)

        case other => cleaned += other
      eventPosition += 1

    Right(SanitizedEvents(cleaned.result(), diagnostics.result(), missingValues.result()))

  private def nonFiniteRows(events: Vector[Event]): Vector[Boolean] =
    val n = events.headOption.map(_.nEvents).getOrElse(0)
    Vector.tabulate(n) { row =>
      events.exists {
        case e: ContinuousEvent =>
          var col = 0
          var found = false
          while col < e.value.cols && !found do
            found = !e.value.data(row * e.value.cols + col).isFinite
            col += 1
          found
        case _ => false
      }
    }

  private def missingValueResolutions(
      events: Vector[Event],
      policy: MissingValuePolicy,
      action: String,
      provenance: Vector[EventRowProvenance]
  ): Vector[MissingValueResolution] =
    val out = Vector.newBuilder[MissingValueResolution]
    var eventIndex = 0
    while eventIndex < events.headOption.map(_.nEvents).getOrElse(0) do
      events.foreach {
        case e: ContinuousEvent =>
          var col = 0
          while col < e.value.cols do
            val value = e.value.data(eventIndex * e.value.cols + col)
            if !value.isFinite then
              out += MissingValueResolution(
                modulator = e.modulatorIds(col),
                eventIndex = eventIndex,
                policy = policy.label,
                action = action,
                column = e.columnTags.lift(col),
                source = provenance.lift(eventIndex)
              )
            col += 1
        case _ => ()
      }
      eventIndex += 1
    out.result()

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
          val modulator = e.modulatorIds(c)

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
            out += EventModelDiagnostic(
              EventModelDiagnosticKind.DegenerateModulator,
              termLabel,
              m,
              column = Some(modulator.value)
            )
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

  private def subsetTerm(rows: TermRows, keep: Vector[Boolean]): TermRows =
    require(rows.onsets.length == keep.length, "subset: rows/mask length mismatch")
    val idx = keep.iterator.zipWithIndex.collect { case (true, i) => i }.toVector

    TermRows(
      events = rows.events.map(ev => subsetEvent(ev, idx, keep)),
      onsets = idx.map(rows.onsets),
      durations = idx.map(rows.durations),
      blockIds = idx.map(rows.blockIds),
      sourceRows = idx.map(rows.sourceRows)
    )

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
      termTag: Option[String],
      factorLevels: FactorLevelRegistry,
      blockIds: Vector[Int]
  ): Either[DesignError, Vector[EventExpression]] =
    val out = Vector.newBuilder[EventExpression]
    var i = 0
    while i < exprs.length do
      toEvent(data, exprs(i), termTag, factorLevels, blockIds) match
        case Left(error)       => return Left(error)
        case Right(expression) => out += expression
      i += 1
    Right(out.result())

  private def toEvent(
      data: DataTable,
      expr: ArgValue,
      termTag: Option[String],
      factorLevels: FactorLevelRegistry,
      blockIds: Vector[Int]
  ): Either[DesignError, EventExpression] =
    expr match
      case ArgValue.Ident(id) =>
        data.column(id).flatMap {
          case Column.Strings(v) =>
            factorEventOr(id, v, factorLevels)
          case Column.Doubles(v) =>
            factorLevels.get(FactorId.unsafe(Names.sanitize(id.value, allowDot = true))) match
              case Some(_) => factorEventOr(id, v.map(canonicalNumericLevel), factorLevels)
              case None    => Right(Event.variable(v, id.value))
          case Column.Ints(v) =>
            factorLevels.get(FactorId.unsafe(Names.sanitize(id.value, allowDot = true))) match
              case Some(_) => factorEventOr(id, v.map(_.toString), factorLevels)
              case None    => Right(Event.variable(v.map(_.toDouble), id.value))
          case Column.Bools(v) =>
            factorEventOr(id, v.map(_.toString), factorLevels)
          case other =>
            Left(DesignError.InvalidColumnType(id.value, "usable as an event variable", other.typeName))
        }.map(EventExpression(_, Vector.empty))
      case ArgValue.Call(fun, args) =>
        evalBasisCall(data, fun, args, termTag, factorLevels, blockIds)
      case other =>
        Left(DesignError.FormulaBinding(s"Unsupported event expression: $other"))

  private def factorEventOr(
      id: ColumnId,
      values: Vector[String],
      factorLevels: FactorLevelRegistry
  ): Either[DesignError, Event] =
    val factor = FactorId.unsafe(Names.sanitize(id.value, allowDot = true))
    factorLevels.get(factor) match
      case Some(levelSet) => Event.factorWithLevels(values, id.value, levelSet)
      case None           => Right(Event.factor(values, id.value))

  private def canonicalNumericLevel(value: Double): String =
    val raw = value.toString
    val normalized =
      if raw.endsWith(".0") then raw.dropRight(2)
      else raw.replace(".0E", "E").replace(".0e", "e")
    if normalized == "-0" then "0" else normalized

  /** Event/basis calls the formula grammar understands, for [[DesignError.UnknownBasisFunction]]. */
  private val basisCalls: Vector[String] =
    Vector(
      "modulators",
      "scale",
      "standardized",
      "robustscale",
      "poly",
      "bspline",
      "scalewithin",
      "center",
      "center_at",
      "center_within",
      "center_within_run"
    )

  private def evalBasisCall(
      data: DataTable,
      funName: String,
      args: Vector[Arg],
      termTag: Option[String],
      factorLevels: FactorLevelRegistry,
      blockIds: Vector[Int]
  ): Either[DesignError, EventExpression] =
    funName.trim.toLowerCase match
      case "modulators" =>
        modulatorFamilyExpression(data, args, termTag, factorLevels, blockIds)
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
      case "center" =>
        for
          (xVar, xs) <- requireNumeric1(data, args, "Center")
          _ <- requireArgumentCount(args, expected = 1, ctx = "Center")
          _ <- CenteringPolicy.GrandMean.validate
        yield EventExpression(
          Event.variable(xs, xVar.value),
          Vector.empty,
          Vector(CenteringRequest(xVar, CenteringPolicy.GrandMean))
        )
      case "center_at" =>
        for
          (xVar, xs) <- requireNumeric1(data, args, "CenterAt")
          _ <- requireArgumentCount(args, expected = 2, ctx = "CenterAt")
          value <- requireCenterValue(args, ctx = "CenterAt")
          policy = CenteringPolicy.At(value)
          _ <- policy.validate
        yield EventExpression(
          Event.variable(xs, xVar.value),
          Vector.empty,
          Vector(CenteringRequest(xVar, policy))
        )
      case "center_within_run" =>
        for
          (xVar, xs) <- requireNumeric1(data, args, "CenterWithinRun")
          _ <- requireArgumentCount(args, expected = 1, ctx = "CenterWithinRun")
          _ <-
            if blockIds.length == xs.length then Right(())
            else Left(DesignError.InvalidSchedule(s"CenterWithinRun block ids have length ${blockIds.length} but expected ${xs.length}"))
        yield EventExpression(
          Event.variable(xs, xVar.value),
          Vector.empty,
          Vector(CenteringRequest(xVar, CenteringPolicy.WithinRun, blockIds.map(_.toString)))
        )
      case "center_within" =>
        for
          (xVar, xs) <- requireNumeric1(data, args, "CenterWithin")
          groupVars <- requireGroupArgs(args, ctx = "CenterWithin")
          groups <- groupValues(data, groupVars)
          factors = groupVars.map(v => FactorId.unsafe(Names.sanitize(v.value, allowDot = true)))
          policy = if factors.length == 1 then CenteringPolicy.WithinFactor(factors.head) else CenteringPolicy.WithinCells(factors)
          _ <- policy.validate
        yield EventExpression(
          Event.variable(xs, xVar.value),
          Vector.empty,
          Vector(CenteringRequest(xVar, policy, groups))
        )
      case _ =>
        Left(DesignError.UnknownBasisFunction(funName, basisCalls))

  private def modulatorFamilyExpression(
      data: DataTable,
      args: Vector[Arg],
      termTag: Option[String],
      factorLevels: FactorLevelRegistry,
      blockIds: Vector[Int]
  ): Either[DesignError, EventExpression] =
    if args.isEmpty then
      Left(DesignError.FormulaBinding("modulators(...) requires at least one numeric expression"))
    else if args.exists(_.name.nonEmpty) then
      Left(DesignError.FormulaBinding("modulators(...) accepts ordered positional expressions only"))
    else
      val members = Vector.newBuilder[(ModulatorId, ContinuousEvent, Vector[EventModelDiagnostic], Vector[CenteringRequest])]
      var failed: Option[DesignError] = None
      var index = 0
      while index < args.length && failed.isEmpty do
        args(index).value match
          case ArgValue.Call(fun, _) if fun.trim.equalsIgnoreCase("modulators") =>
            failed = Some(DesignError.FormulaBinding("modulators(...) cannot be nested"))
          case value =>
            toEvent(data, value, termTag, factorLevels, blockIds) match
              case Left(error) => failed = Some(error)
              case Right(expression) =>
                expression.event match
                  case event: ContinuousEvent if event.value.cols == 1 =>
                    event.modulatorIds match
                      case Vector(modulator) =>
                        members += ((modulator, event, expression.diagnostics, expression.centering))
                      case identities =>
                        failed = Some(
                          DesignError.InvalidSchema(
                            s"modulators(...) expression ${index + 1} has ${identities.length} scientific identities for one numerical column"
                          )
                        )
                  case event: ContinuousEvent =>
                    failed = Some(
                      DesignError.FormulaBinding(
                        s"modulators(...) expression ${index + 1} realizes ${event.value.cols} columns; each member must realize exactly one"
                      )
                    )
                  case event: CategoricalEvent =>
                    failed = Some(
                      DesignError.InvalidColumnType(
                        event.varName,
                        "a continuous modulator expression",
                        "categorical"
                      )
                    )
        index += 1

      failed match
        case Some(error) => Left(error)
        case None =>
          val values = members.result()
          val columns = values.map { case (modulator, event, _, _) =>
            modulator -> matCol(event.value, 0)
          }
          Event.modulatorFamily(columns, termTag.fold("modulators")(_ + "_modulators")).map { event =>
            EventExpression(
              event = event,
              diagnostics = values.flatMap(_._3),
              centering = values.flatMap(_._4)
            )
          }

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

  private def requireArgumentCount(args: Vector[Arg], expected: Int, ctx: String): Either[DesignError, Unit] =
    if args.length == expected then Right(())
    else Left(DesignError.FormulaBinding(s"$ctx expects $expected argument${if expected == 1 then "" else "s"}, found ${args.length}"))

  private def requireCenterValue(args: Vector[Arg], ctx: String): Either[DesignError, Double] =
    val candidate =
      args.drop(1).collectFirst { case Arg(Some("value"), value) => value }
        .orElse(args.drop(1).collectFirst { case Arg(None, value) => value })
    candidate match
      case Some(ArgValue.Num(value)) if value.isFinite => Right(value)
      case Some(ArgValue.Num(value)) => Left(DesignError.FormulaBinding(s"$ctx reference value must be finite, got $value"))
      case Some(ArgValue.Ident(id)) =>
        Left(DesignError.FormulaBinding(s"$ctx reference value must be a numeric literal, found '${id.value}'"))
      case Some(other) => Left(DesignError.FormulaBinding(s"$ctx reference value must be numeric, found $other"))
      case None => Left(DesignError.FormulaBinding(s"$ctx requires a numeric reference value"))

  private def requireGroupArgs(args: Vector[Arg], ctx: String): Either[DesignError, Vector[ColumnId]] =
    if args.length < 2 then
      Left(DesignError.FormulaBinding(s"$ctx requires an x column and at least one grouping factor"))
    else
      val out = Vector.newBuilder[ColumnId]
      var i = 1
      while i < args.length do
        args(i) match
          case Arg(None, ArgValue.Ident(id)) => out += id
          case other => return Left(DesignError.FormulaBinding(s"$ctx grouping args must be identifiers, found $other"))
        i += 1
      Right(out.result())

  private def groupValues(
      data: DataTable,
      columns: Vector[ColumnId]
  ): Either[DesignError, Vector[String]] =
    val values = Vector.newBuilder[Vector[String]]
    var i = 0
    while i < columns.length do
      toFactorStrings(data, columns(i)) match
        case Left(error) => return Left(error)
        case Right(xs)   => values += xs
      i += 1
    val columns0 = values.result()
    val n = columns0.headOption.map(_.length).getOrElse(0)
    if columns0.exists(_.length != n) then
      Left(DesignError.InvalidSchema("centering grouping columns must have equal row counts"))
    else
      Right(
        Vector.tabulate(n) { row =>
          columns0.map { column =>
            val value = column(row)
            s"${value.length}:$value"
          }.mkString("|")
        }
      )

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
        columnBasisIx = term.columnBasisIx :+ None,
        columnCells =
          (if term.columnCells.isEmpty then Vector.fill(m.cols)(None) else term.columnCells) :+ Some(scalafim.fmri.design.CellKey.empty),
        columnModulators =
          (if term.columnModulators.isEmpty then Vector.fill(m.cols)(None) else term.columnModulators) :+ None,
        columnScales =
          (if term.columnScales.isEmpty then Vector.fill(m.cols)(HrfColumnScale.identity) else term.columnScales) :+ HrfColumnScale.identity
      )
