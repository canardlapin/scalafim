package scalafim.fmri.design.formula

import scalafim.fmri.design.{BasisOrthogonalizationReceipt, CellAssignment, CellKey, CenteringOutcome, CenteringPolicy, CenteringReceipt, CenteringGroupReceipt, ColumnId, DegenerateModulatorOutcome, DegenerateModulatorPolicy, DegenerateModulatorReceipt, DesignError, EmptyCellAudit, EmptyCellDisposition, EmptyCellPolicy, EmptyCellScope, EventRowProvenance, FactorLevelAudit, FactorLevelRegistry, FactorId, FactorPartitionAudit, FactorSchemaBinding, HrfAssignment, HrfByCell, HrfByPhase, HrfColumnScale, HrfColumnScaling, MissingValuePolicy, MissingValueResolution, ModulatorId, ModulatorOrthogonalization, ModulatorOrthogonalizationPlan, Names, OrthogonalizationGroupReceipt, OrthogonalizationOutcome, OrthogonalizationReceipt, OrthogonalizationScope, OrthogonalizationStepReceipt, PhaseId, PolicyReceipt, RunIndex, TermId, TrialId}
import scalafim.fmri.design.ObservedModulator
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
      orthogonalization: ModulatorOrthogonalizationPlan = ModulatorOrthogonalizationPlan.None
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

  /** A bounded cache over one fixed compilation context. Only the formula may
    * change; new data, sampling, policies or extension implementations require
    * a fresh preparation. As elsewhere in the compiler, extension functions
    * must be pure for a fixed input. Returned model buffers never own cache data.
    */
  final class IncrementalDesign private[EventModelBuilder] (
      private val request: EventDesignRequest,
      private val cached: Map[TermCall, CompiledTerm],
      val model: EventModel,
      val recompiledTerms: Int,
      val reusedTerms: Int
  ):
    def formula: ModelFormula = request.formula
    def replaceTerm(index: Int, term: TermCall): Either[DesignError, IncrementalDesign] =
      if index < 0 || index >= formula.terms.size then Left(DesignError.InvalidSchema("term index is outside the formula"))
      else updateFormula(formula.copy(terms = formula.terms.updated(index, term)))
    def updateFormula(formula: ModelFormula): Either[DesignError, IncrementalDesign] =
      if formula.onset != request.formula.onset then
        prepareIncremental(request.copy(formula = formula))
      else incremental(request.copy(formula = formula), cached)

  def prepareIncremental(request: EventDesignRequest): Either[DesignError, IncrementalDesign] =
    incremental(request, Map.empty)

  private def incremental(request: EventDesignRequest, previous: Map[TermCall, CompiledTerm]): Either[DesignError, IncrementalDesign] =
    val cache = new TermCache(previous)
    for
      _ <- request.options.validate
      blocks <- request.blockPlan.resolve(request.env.eventData)
      durations <- request.durationPlan.resolve(request.env.eventData.nrows)
      model <- compile(request.formula, request.env, request.samplingFrame, blocks, durations, request.options, request.extensions, Some(cache))
    yield new IncrementalDesign(request, cache.entries.toMap, model, cache.compiledCount, cache.reusedCount)

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
      basisOrthogonalizationReceipts: Vector[BasisOrthogonalizationReceipt] = Vector.empty,
      policyReceipts: Vector[PolicyReceipt] = Vector.empty,
      factorLevels: Vector[FactorLevelAudit] = Vector.empty,
      emptyCells: Vector[CellKey] = Vector.empty,
      emptyCellAudits: Vector[EmptyCellAudit] = Vector.empty
  )

  private final class TermCache(previous: Map[TermCall, CompiledTerm]):
    val entries = scala.collection.mutable.Map.empty[TermCall, CompiledTerm]
    var compiledCount = 0
    var reusedCount = 0
    def resolve(call: TermCall)(build: => Either[DesignError, CompiledTerm]): Either[DesignError, CompiledTerm] =
      val result = previous.get(call).orElse(entries.get(call)) match
        case Some(value) =>
          reusedCount += 1
          Right(value)
        case None =>
          compiledCount += 1
          build
      result.map: value =>
        entries.update(call, value)
        detach(value)

  private def detach(value: CompiledTerm): CompiledTerm =
    def copyMatrix(matrix: scalafim.fmri.hrf.linalg.Mat): scalafim.fmri.hrf.linalg.Mat =
      scalafim.fmri.hrf.linalg.Mat.unsafe(matrix.rows, matrix.cols, matrix.data.clone())
    val term = value.term match
      case c: ConvolvedTerm =>
        val events = c.term.events.map:
          case e: ContinuousEvent => e.copy(value = copyMatrix(e.value))
          case e => e
        c.copy(term = c.term.copy(events = events), data = copyMatrix(c.data))
      case c: CovariateConvolvedTerm => c.copy(data = copyMatrix(c.data))
      case other => throw new IllegalArgumentException(s"incremental compiler cannot detach ${other.getClass.getName}")
    value.copy(term = term)

  private final case class EventExpression(
      event: Event,
      diagnostics: Vector[EventModelDiagnostic],
      centering: Vector[CenteringRequest] = Vector.empty,
      observed: Vector[ObservedRequest] = Vector.empty,
      products: Vector[ProductRequest] = Vector.empty
  )

  private final case class ProductRequest(
      modulator: ModulatorId,
      leftSource: ColumnId,
      rightSource: ColumnId,
      left: Vector[Double],
      right: Vector[Double]
  )

  private final case class ObservedRequest(
      source: ModulatorId,
      centering: ObservedModulator.Centering,
      scaling: ObservedModulator.Scaling,
      missing: MissingValuePolicy
  )

  private final case class ObservedEvents(
      events: Vector[Event],
      missingValues: Vector[MissingValueResolution],
      policies: Vector[PolicyReceipt]
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
      receipts: Vector[OrthogonalizationReceipt],
      /** Formula `orthogonalize = TRUE` only: (group key, removed mean) of the first modulator. */
      firstCentering: Vector[(String, Double)] = Vector.empty
  )

  private final case class BasisOrthogonalizedTerm(
      term: ConvolvedTerm,
      receipts: Vector[BasisOrthogonalizationReceipt]
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
      basisOrthogonalizationReceipts: Vector[BasisOrthogonalizationReceipt] = Vector.empty,
      policyReceipts: Vector[PolicyReceipt] = Vector.empty,
      factorLevels: Vector[FactorLevelAudit] = Vector.empty,
      emptyCells: Vector[CellKey] = Vector.empty,
      emptyCellAudits: Vector[EmptyCellAudit] = Vector.empty
  )

  private def compile(
      formula: ModelFormula,
      env: TableEnv,
      samplingFrame: SamplingFrame,
      blockIds: Seq[Int],
      durations: Seq[Double],
      options: BuildOptions,
      extensions: DesignExtensionEnv,
      cache: Option[TermCache] = None
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
      declaredPhases = formula.terms.collect {
        case term: HrfCall       => term.phase.map(_.id)
        case term: TrialwiseCall => term.phase.map(_.id)
      }.flatten
      _ <- options.hrfByPhase.fold[Either[DesignError, Unit]](Right(()))(_.validateCoverage(declaredPhases))
      compiled <- compileTerms(formula, env, samplingFrame, schedule, options, extensions, cache)
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
      extensions: DesignExtensionEnv,
      cache: Option[TermCache]
  ): Either[DesignError, CompiledTerms] =
    val terms = Vector.newBuilder[EventModelTerm]
    val contrastRefs = Vector.newBuilder[Option[String]]
    val diagnostics = Vector.newBuilder[EventModelDiagnostic]
    val missingValues = Vector.newBuilder[MissingValueResolution]
    val centeringReceipts = Vector.newBuilder[CenteringReceipt]
    val degenerateModulatorReceipts = Vector.newBuilder[DegenerateModulatorReceipt]
    val orthogonalizationReceipts = Vector.newBuilder[OrthogonalizationReceipt]
    val basisOrthogonalizationReceipts = Vector.newBuilder[BasisOrthogonalizationReceipt]
    val policyReceipts = Vector.newBuilder[PolicyReceipt]
    val factorLevels = Vector.newBuilder[FactorLevelAudit]
    val emptyCells = Vector.newBuilder[CellKey]
    val emptyCellAudits = Vector.newBuilder[EmptyCellAudit]
    var failed: Option[DesignError] = None
    var i = 0
    while i < formula.terms.length && failed.isEmpty do
      val call = formula.terms(i)
      val result = cache match
        case None => compileTerm(call, env, samplingFrame, schedule, options, extensions)
        case Some(value) => value.resolve(call)(compileTerm(call, env, samplingFrame, schedule, options, extensions))
      result match
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
          basisOrthogonalizationReceipts ++= compiled.basisOrthogonalizationReceipts
          policyReceipts ++= compiled.policyReceipts
          factorLevels ++= compiled.factorLevels
          emptyCells ++= compiled.emptyCells
          emptyCellAudits ++= compiled.emptyCellAudits
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
            orthogonalizationReceipts = orthogonalizationReceipts.result(),
            basisOrthogonalizationReceipts = basisOrthogonalizationReceipts.result(),
            policyReceipts = policyReceipts.result(),
            factorLevels = factorLevels.result(),
            emptyCells = emptyCells.result(),
            emptyCellAudits = emptyCellAudits.result()
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
      observedPlans = expressions.flatMap(_.observed)
      productPlans = expressions.flatMap(_.products)
      _ <- if observedPlans.map(_.source).distinct.length == observedPlans.length then Right(())
        else Left(DesignError.FormulaBinding("a modulator may have only one observed-value policy in each term"))
      events0 = expressions.map(_.event)
      centering0 = expressions.map(_.centering)
      basisDiagnostics = expressions.flatMap(_.diagnostics)
      termOnsets <- h.onsets.fold[Either[DesignError, Vector[Seconds]]](Right(schedule.defaultOnsets)) { ref =>
        resolveSecondsEither(ref, env.eventData, nEvents, argName = "onsets")
      }
      termDurs <- h.durations.fold[Either[DesignError, Vector[Seconds]]](Right(schedule.defaultDurs)) { ref =>
        resolveSecondsEither(ref, env.eventData, nEvents, argName = "durations")
      }
      phase0 <- resolvePhase(h.phase, env.eventData, nEvents)
      subsetMask <- resolveSubsetMaskEither(h.subset, env.eventData, nEvents)
      originalSub0 = if subsetMask.forall(identity) then env.eventData else env.eventData.filterRows(subsetMask)
      rows0 = TermRows(events0, termOnsets, termDurs, schedule.blockIds0, Vector.tabulate(nEvents)(identity))
      subset0 = if subsetMask.forall(identity) then rows0 else subsetTerm(rows0, subsetMask)
      phase0s <- resolveEventPhase(phase0, subset0)
      centering0s = subsetCenteringRequests(centering0, subsetMask)
      observedMissingMask = observedDropMask(subset0.events, observedPlans, options.missingValuePolicy)
      productMissingMask =
        if options.missingValuePolicy == MissingValuePolicy.DropFromTerm then
          productObservedMask(productPlans, subset0.sourceRows)
        else Vector.empty
      dropMissingMask <- combineKeepMasks(observedMissingMask, productMissingMask)
      originalSub =
        if dropMissingMask.isEmpty || dropMissingMask.forall(identity) then originalSub0
        else originalSub0.filterRows(dropMissingMask)
      subsetRaw =
        if dropMissingMask.isEmpty || dropMissingMask.forall(identity) then subset0
        else subsetTerm(subset0, dropMissingMask)
      productEvents <- applyProducts(subsetRaw.events, productPlans, subsetRaw.sourceRows, subsetRaw.blockIds)
      subset = subsetRaw.copy(events = productEvents)
      phase <- resolveEventPhase(phase0, subset)
      centeringS = subsetCenteringRequests(centering0s, dropMissingMask)
      factorAudits0 = factorLevelAudits(
        subset.events,
        options.factorRegistry,
        subset.blockIds,
        schedule.blockIds0.distinct.sorted
      )
      droppedMissing =
        if dropMissingMask.contains(false) then
          missingValueResolutions(
            subset0.events,
            MissingValuePolicy.DropFromTerm,
            action = "dropped-event",
            provenance = phase0s.toVector.flatMap(_.provenance)
          ).filter(r => missingPolicyFor(r.modulator, observedPlans, options.missingValuePolicy) == MissingValuePolicy.DropFromTerm)
        else Vector.empty
      prepared <- prepareObserved(subset.events, observedPlans, subset.blockIds, phase.toVector.flatMap(_.provenance))
      centered <- applyCentering(prepared.events, centeringS, subset.blockIds)
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
      formulaOrthogonalization <- formulaOrthogonalization(h, termTag, cleaned.events)
      _ <-
        if formulaOrthogonalization.nonEmpty && options.orthogonalization.forTerm(termTag).nonEmpty then
          Left(DesignError.FormulaBinding("formula orthogonalize cannot be combined with a BuildOptions orthogonalization policy for the same term"))
        else Right(())
      orthogonalized <- applyOrthogonalization(
        events = cleaned.events,
        termTag = termTag,
        blockIds = subset.blockIds,
        sourceRows = subset.sourceRows,
        parentTrials = phase.toVector.flatMap(_.parentTrialIds),
        policy = formulaOrthogonalization.orElse(options.orthogonalization.forTerm(termTag)),
        spmFormula = formulaOrthogonalization.nonEmpty
      )
      eventsClean <- includeMainEffect(orthogonalized.events, h.includeMain.getOrElse(false))
      nonFiniteDiagnostics = cleaned.diagnostics
      term <- eventTermEither(
        events = eventsClean,
        onsets = subset.onsets,
        durations = subset.durations,
        blockIds = subset.blockIds,
        termTag = termTag,
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
      conv0 <- convolveHrfTermEither(h, term, originalSub, samplingFrame, options, extensions)
      basisOrthogonalized <- applyBasisOrthogonalization(conv0, h.orthogonalizeBasis.getOrElse(false))
      conv = basisOrthogonalized.term
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
      eventNormalizationReceipt = h.eventNormalization.toVector.map { mode =>
        val detail = mode match
          case EventResponseNormalization.PreservePulseScale => "policy=as-convolved"
          case EventResponseNormalization.UnitPeak(step) =>
            s"policy=unit-peak;scope=per-event-per-basis;reference-step=${step.value};precision=${options.precision.value}"
        PolicyReceipt("event-response-normalization", s"term=${termTag.getOrElse("term")};$detail")
      }
      productReceipts = productPlans.map { product =>
        PolicyReceipt(
          "product-modulator",
          s"term=${termTag.getOrElse("term")};modulator=${product.modulator.value};" +
            s"left=${product.leftSource.value};right=${product.rightSource.value};centering=within-run"
        )
      }
    yield CompiledTerm(
      conv,
      h.contrasts,
      termDiagnostics,
      missingValues = droppedMissing ++ prepared.missingValues ++ cleaned.missingValues,
      centeringReceipts = centered.receipts,
      degenerateModulatorReceipts = rawDegenerateModulators,
      orthogonalizationReceipts = orthogonalized.receipts,
      basisOrthogonalizationReceipts = basisOrthogonalized.receipts,
      policyReceipts = schemaBindingReceipt ++ hrfCellReceipt ++ hrfPhaseReceipt ++ eventNormalizationReceipt ++ productReceipts ++ prepared.policies ++ formulaOrthogonalizationReceipts(termTag, formulaOrthogonalization, orthogonalized) ++ Vector(
        PolicyReceipt(
          "modulator-missing-values",
          if observedPlans.isEmpty then s"term=${termTag.getOrElse("term")};policy=${options.missingValuePolicy.canonical}"
          else
            // Observed-modulator policies override the build default per modulator.
            val modulators = events0.collect { case event: ContinuousEvent => event.modulatorIds }.flatten.distinct
            s"term=${termTag.getOrElse("term")};default=${options.missingValuePolicy.canonical};effective=" +
              modulators.map(id => s"${id.value}:${missingPolicyFor(id, observedPlans, options.missingValuePolicy).canonical}").mkString(",")
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
          formulaOrthogonalization.orElse(options.orthogonalization.forTerm(termTag)).fold(s"term=${termTag.getOrElse("term")};policy=none")(_.canonical)
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

  private def includeMainEffect(events: Vector[Event], include: Boolean): Either[DesignError, Vector[Event]] =
    if !include then Right(events)
    else
      val continuous = events.zipWithIndex.collect { case (event: ContinuousEvent, index) => (event, index) }
      if continuous.length != 1 then
        Left(DesignError.FormulaBinding("include_main requires exactly one continuous expression or modulators(...) family"))
      else
        val (event, index) = continuous.head
        val columns = event.value.cols + 1
        val values = Array.tabulate(event.value.rows * columns): offset =>
          val column = offset % columns
          if column == 0 then 1.0 else event.value(offset / columns, column - 1)
        val paired = event.copy(
          value = scalafim.fmri.hrf.linalg.Mat.unsafe(event.value.rows, columns, values),
          columnTags = "main" +: event.columnTags,
          columnModulators = ModulatorId.unsafe("main") +: event.modulatorIds,
          mainEffectColumn = Some(0)
        )
        Right(events.updated(index, paired))

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
      termOnsets <- t.onsets.fold[Either[DesignError, Vector[Seconds]]](Right(schedule.defaultOnsets)) { ref =>
        resolveSecondsEither(ref, env.eventData, nEvents, argName = "onsets")
      }
      termDurs <- t.durations.fold[Either[DesignError, Vector[Seconds]]](Right(schedule.defaultDurs)) { ref =>
        resolveSecondsEither(ref, env.eventData, nEvents, argName = "durations")
      }
      trialIds <- resolveTrialwiseIds(t.id, env.eventData, nEvents, schedule.blockIds0, label0)
      phase0 <- resolveTrialwisePhase(t.phase, env.eventData, nEvents, schedule.blockIds0)
      subsetMask <- resolveSubsetMaskEither(t.subset, env.eventData, nEvents)
      trialEvent <- catchBuild(DesignError.fromThrowable)(Event.factor(trialIds, name = "trial"))
      rows0 = TermRows(Vector(trialEvent), termOnsets, termDurs, schedule.blockIds0, Vector.tabulate(nEvents)(identity))
      rows = if subsetMask.forall(identity) then rows0 else subsetTerm(rows0, subsetMask)
      phase <- resolveEventPhase(phase0, rows)
      term <- eventTermEither(
        events = rows.events,
        onsets = rows.onsets,
        durations = rows.durations,
        blockIds = rows.blockIds,
        termTag = Some(termTag),
        phase = phase
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
      diagnosed <- model0.withDiagnosticsEither(compiled.diagnostics)
      evidenced <- diagnosed.withPolicyEvidenceEither(
        compiled.missingValues,
        compiled.policyReceipts,
        factorLevels = compiled.factorLevels,
        emptyCells = compiled.emptyCells,
        emptyCellAudits = compiled.emptyCellAudits,
        centering = compiled.centeringReceipts,
        degenerateModulators = compiled.degenerateModulatorReceipts,
        orthogonalization = compiled.orthogonalizationReceipts,
        basisOrthogonalization = compiled.basisOrthogonalizationReceipts
      )
    yield evidenced.copy(contrastSetsByTerm = attached)

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
  private def throwableMessage(t: Throwable): String =
    Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.toString)

  private def eventTermEither(
      events: Vector[Event],
      onsets: Vector[Seconds],
      durations: Vector[Seconds],
      blockIds: Vector[Int],
      termTag: Option[String],
      phase: Option[EventPhase]
  ): Either[DesignError, EventTerm] =
    phase match
      case None =>
        EventTerm.validated(
          events = events,
          onsets = onsets,
          durations = durations,
          blockIds = blockIds,
          termTag = termTag
        )
      case Some(value) =>
        value.term(events, termTag)

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
        // Existing formulas permit selecting an absent level (an empty term).
        // Strict level validation is available in EventExpressions for authoring.
        EventExpressions.filter(expr, data, checkLevels = false).left.map {
          case EventExpressionError.UnknownColumn(_, id) => DesignError.MissingColumn(id.value)
          case error => DesignError.InvalidSubset(error.message)
        }

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

  /** Resolve a trialwise parent axis. Parent ids are caller-local within a
    * run, then qualified before entering [[EventPhase]], whose provenance
    * identity is global across the complete event table.
    */
  private def resolveTrialwisePhase(
      phase: Option[PhaseRef],
      data: DataTable,
      nEvents: Int,
      blockIds: Vector[Int]
  ): Either[DesignError, Option[ResolvedPhase]] =
    phase match
      case None => Right(None)
      case Some(ref) =>
        resolveIdentifierValues(ref.parent, data, nEvents, "parent").flatMap { values =>
          validateUniqueWithinRun(values, blockIds, "parent")
            .flatMap(_ => qualifyTrialIds(values, blockIds).map(ids => Some(ResolvedPhase(ref.id, ids))))
        }

  private def resolveTrialwiseIds(
      identity: Option[ArgValue],
      data: DataTable,
      nEvents: Int,
      blockIds: Vector[Int],
      label: String
  ): Either[DesignError, Vector[String]] =
    identity match
      case None => Right(trialLevels(nEvents))
      case Some(ArgValue.Ident(column)) =>
        resolveIdentifierValues(column, data, nEvents, "id").flatMap { values =>
          validateUniqueWithinRun(values, blockIds, "id").map { _ =>
            values.indices.map { index =>
              s"run${blockIds(index) + 1}_${label}_${values(index)}"
            }.toVector
          }
        }
      case Some(ArgValue.Str(name)) =>
        ColumnId(name).flatMap(column => resolveTrialwiseIds(Some(ArgValue.Ident(column)), data, nEvents, blockIds, label))
      case Some(other) =>
        Left(DesignError.FormulaBinding(s"id must be a column reference, found $other"))

  private def resolveIdentifierValues(
      columnId: ColumnId,
      data: DataTable,
      nEvents: Int,
      argName: String
  ): Either[DesignError, Vector[String]] =
    data.column(columnId).flatMap { column =>
      val valuesEither: Either[DesignError, Vector[String]] =
        column match
          case Column.Strings(values) => Right(values)
          case Column.Ints(values)    => Right(values.map(_.toString))
          case Column.Doubles(values) if values.forall(_.isFinite) => Right(values.map(_.toString))
          case Column.Doubles(_) => Left(DesignError.InvalidColumnType(columnId.value, "finite trial identifiers", "numeric with non-finite values"))
          case Column.Bools(values) => Right(values.map(_.toString))
          case other => Left(DesignError.InvalidColumnType(columnId.value, "scalar trial identifiers", other.typeName))
      valuesEither.flatMap { values =>
        if values.length != nEvents then
          Left(DesignError.InvalidSchedule(s"$argName column '${columnId.value}' has length ${values.length} but expected $nEvents"))
        else
          val out = Vector.newBuilder[String]
          var failed: Option[DesignError] = None
          var index = 0
          while index < values.length && failed.isEmpty do
            TrialId(values(index)) match
              case Left(error) => failed = Some(error)
              case Right(_)    => out += values(index)
            index += 1
          failed.fold[Either[DesignError, Vector[String]]](Right(out.result()))(Left(_))
      }
    }

  private def validateUniqueWithinRun(
      ids: Vector[String],
      blockIds: Vector[Int],
      argName: String
  ): Either[DesignError, Unit] =
    val duplicate = blockIds.indices
      .groupBy(blockIds)
      .collectFirst { case (block, rows) if rows.map(ids).distinct.length != rows.length => block }
    duplicate match
      case Some(block) => Left(DesignError.InvalidSchedule(s"$argName values must be unique within run ${block + 1}"))
      case None        => Right(())

  private def qualifyTrialIds(values: Vector[String], blockIds: Vector[Int]): Either[DesignError, Vector[TrialId]] =
    val out = Vector.newBuilder[TrialId]
    var failed: Option[DesignError] = None
    var index = 0
    while index < values.length && failed.isEmpty do
      TrialId(s"run${blockIds(index) + 1}:${values(index)}") match
        case Left(error) => failed = Some(error)
        case Right(id)   => out += id
      index += 1
    failed.fold[Either[DesignError, Vector[TrialId]]](Right(out.result()))(Left(_))

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
    if h.hrfFun.nonEmpty && (h.span.nonEmpty || h.kernelNormalization.nonEmpty || h.temporalDerivative.nonEmpty || h.eventNormalization.nonEmpty) then
      return Left(DesignError.FormulaBinding("kernel options cannot be combined with hrf_fun"))
    if h.sharedSlopes.contains(true) && !h.includeMain.contains(true) then
      return Left(DesignError.FormulaBinding("shared_slopes requires include_main = TRUE"))
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
      case Some(_) if h.sharedSlopes.contains(true) =>
        Left(DesignError.FormulaBinding("shared_slopes requires the formula-owned shared HRF path"))
      case Some(_) if h.hrfFun.nonEmpty =>
        Left(DesignError.InvalidSchema("explicit HRF assignment cannot be combined with hrf_fun on the same term"))
      case Some(_) if h.basis.nonEmpty || h.lag.nonEmpty || h.nbasis.nonEmpty || h.span.nonEmpty || h.kernelNormalization.nonEmpty || h.temporalDerivative.nonEmpty || h.eventNormalization.nonEmpty =>
        Left(DesignError.InvalidSchema("explicit HRF assignment cannot be combined with formula kernel options on the same term"))
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
              hrf0 <- resolveHrfEither(h, options.defaultHrf, Some(samplingFrame))
              conv <-
                if h.sharedSlopes.contains(true) then
                  convolveSharedSlopes(term, hrf0, samplingFrame, options.precision, effectiveDropEmpty(options), summate0, scaling0, h.eventNormalization)
                else convolveResponse(term, hrf0, samplingFrame, options.precision, effectiveDropEmpty(options), summate0, scaling0, h.eventNormalization)
            yield conv
    }

  /** Omitted and `as-convolved` event normalization are the same plain
    * convolution (no per-event scales); `unit-peak` uses the same precision,
    * span truncation and onset windowing with per-event, per-basis divisors. */
  private def convolveResponse(
      term: EventTerm, hrf: Hrf, frame: SamplingFrame, precision: Seconds,
      dropEmpty: Boolean, summate: Boolean, scaling: HrfColumnScaling,
      normalization: Option[EventResponseNormalization]
  ): Either[DesignError, ConvolvedTerm] = normalization match
    case None | Some(EventResponseNormalization.PreservePulseScale) =>
      catchBuild(DesignError.fromThrowable)(term.convolve(hrf, frame, precision, dropEmpty, summate, scaling))
    case Some(mode: EventResponseNormalization.UnitPeak) =>
      EventResponseConvolution
        .convolve(term, hrf, frame, mode, dropEmpty, summate, scaling, precision = precision)
        .map(_.convolved)
        .left.map(error => DesignError.FormulaBinding(s"event_normalization: ${error.message}"))

  private def convolveSharedSlopes(
      term: EventTerm,
      hrf: Hrf,
      samplingFrame: SamplingFrame,
      precision: Seconds,
      dropEmpty: Boolean,
      summate: Boolean,
      scaling: HrfColumnScaling,
      normalization: Option[EventResponseNormalization]
  ): Either[DesignError, ConvolvedTerm] =
    val categorical = term.events.collect { case event: CategoricalEvent => event }
    val slopes = term.events.collect { case event: ContinuousEvent => withoutMainEffect(event) }.filter(_.value.cols > 0)
    if categorical.isEmpty || slopes.isEmpty then
      Left(DesignError.FormulaBinding("shared_slopes requires categorical main effects and at least one modulator slope"))
    else
      for
        main <- convolveResponse(term.copy(events = categorical), hrf, samplingFrame, precision, dropEmpty, summate, scaling, normalization)
        slope <- convolveResponse(term.copy(events = slopes), hrf, samplingFrame, precision, dropEmpty, summate, scaling, normalization)
        joined <- catchBuild(DesignError.fromThrowable)(joinSharedSlopes(term, main, slope))
      yield joined

  private def joinSharedSlopes(term: EventTerm, main: ConvolvedTerm, slope: ConvolvedTerm): ConvolvedTerm =
    val left = main.data
    val right = slope.data
    val out = new Array[Double](left.rows * (left.cols + right.cols))
    var row = 0
    while row < left.rows do
      System.arraycopy(left.data, row * left.cols, out, row * (left.cols + right.cols), left.cols)
      System.arraycopy(right.data, row * right.cols, out, row * (left.cols + right.cols) + left.cols, right.cols)
      row += 1
    main.copy(
      term = term,
      data = scalafim.fmri.hrf.linalg.Mat.unsafe(left.rows, left.cols + right.cols, out),
      columnNames = main.columnNames ++ slope.columnNames,
      columnConditions = main.columnConditions ++ slope.columnConditions,
      columnBasisIx = main.columnBasisIx ++ slope.columnBasisIx,
      columnCells = main.columnCells ++ slope.columnCells,
      columnModulators = main.columnModulators ++ slope.columnModulators,
      columnHrfs = main.columnHrfs ++ slope.columnHrfs,
      columnScales = main.columnScales ++ slope.columnScales,
      columnEventScales = main.columnEventScales ++ slope.columnEventScales
    )

  private def withoutMainEffect(event: ContinuousEvent): ContinuousEvent =
    val keep = (0 until event.value.cols).filterNot(event.mainEffectColumn.contains).toVector
    val values = new Array[Double](event.value.rows * keep.length)
    var row = 0
    while row < event.value.rows do
      var column = 0
      while column < keep.length do
        values(row * keep.length + column) = event.value(row, keep(column))
        column += 1
      row += 1
    event.copy(
      value = scalafim.fmri.hrf.linalg.Mat.unsafe(event.value.rows, keep.length, values),
      columnTags = keep.map(event.columnTags),
      basis = event.basis,
      columnModulators = keep.map(event.modulatorIds),
      mainEffectColumn = None
    )

  /** One explicit transform over all scans gives shared coefficients the same
    * physical response coordinates across runs. Effective per-group HRFs carry
    * both the column scaling and the serial basis transformation.
    */
  private def applyBasisOrthogonalization(
      term: ConvolvedTerm,
      enabled: Boolean
  ): Either[DesignError, BasisOrthogonalizedTerm] =
    if !enabled then Right(BasisOrthogonalizedTerm(term, Vector.empty))
    else ConvolvedBasisOrthogonalization(term).map { case (changed, receipts) =>
      BasisOrthogonalizedTerm(changed, receipts)
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
      policy: Option[ModulatorOrthogonalization],
      spmFormula: Boolean
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
          groups <-
            if spmFormula then Right(runCellGroups(events, blockIds))
            else orthogonalizationGroups(events, blockIds, value.scope, term)
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
            term = term,
            withIntercept = spmFormula
          )
        yield lowered

  private def formulaOrthogonalization(
      call: HrfCall,
      termTag: Option[String],
      events: Vector[Event]
  ): Either[DesignError, Option[ModulatorOrthogonalization]] =
    if !call.orthogonalize.getOrElse(false) then Right(None)
    else
      termTag match
        case None => Left(DesignError.FormulaBinding("orthogonalize = TRUE requires an explicit hrf id or prefix"))
        case Some(name) =>
          val order = events.collect { case event: ContinuousEvent => event.modulatorIds }.flatten
          if order.lengthCompare(2) < 0 then
            Left(DesignError.FormulaBinding("orthogonalize = TRUE requires at least two ordered modulator streams"))
          else
            // The declared scope names the cell factors; lowering further splits
            // by run and includes an intercept (see `runCellGroups`).
            val factors = events.collect { case event: CategoricalEvent => FactorId.unsafe(event.varName) }.distinct
            val scope = if factors.isEmpty then OrthogonalizationScope.WithinRun else OrthogonalizationScope.WithinCells(factors)
            ModulatorOrthogonalization
              .ordered(TermId.unsafe(name), order, scope)
              .map(Some(_))

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
      term: String,
      withIntercept: Boolean
  ): Either[DesignError, OrthogonalizedEvents] =
    var current = continuousEvents.map(_._2)
    val receipts = Vector.newBuilder[OrthogonalizationStepReceipt]
    // SPM-like formula lowering: the first modulator is mean-centred within each
    // group, i.e. orthogonalized against the group's unit (intercept) column.
    val firstCentering =
      if !withIntercept then Vector.empty
      else
        val first = orderedColumns.head
        val event = current(first.continuousEventIndex)
        val data = event.value.data.clone()
        val means = groups.map { (key, rows) =>
          var total = 0.0
          rows.foreach(row => total += data(row * event.value.cols + first.columnIndex))
          val mean = total / rows.length.toDouble
          rows.foreach(row => data(row * event.value.cols + first.columnIndex) -= mean)
          key -> mean
        }
        current = current.updated(first.continuousEventIndex, event.copy(value = scalafim.fmri.hrf.linalg.Mat.unsafe(event.value.rows, event.value.cols, data)))
        means
    var targetIndex = 1
    var failed: Option[DesignError] =
      firstCentering.collectFirst { case (key, mean) if !mean.isFinite => DesignError.InvalidOrthogonalization(term, s"non-finite first-modulator mean in group $key") }
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
        val reference0 = bindModulatorRows(current, referenceColumns, rows)
        val reference = if withIntercept then prependIntercept(reference0) else reference0
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
            Vector(OrthogonalizationReceipt(policy, receipts.result())),
            firstCentering
          )
        )

  private def prependIntercept(values: scalafim.fmri.hrf.linalg.Mat): scalafim.fmri.hrf.linalg.Mat =
    val cols = values.cols + 1
    val out = new Array[Double](values.rows * cols)
    var row = 0
    while row < values.rows do
      out(row * cols) = 1.0
      System.arraycopy(values.data, row * values.cols, out, row * cols + 1, values.cols)
      row += 1
    scalafim.fmri.hrf.linalg.Mat.unsafe(values.rows, cols, out)

  /** Formula `orthogonalize = TRUE` groups: one group per run and realized
    * cell (all categorical events of the term), as SPM orthogonalizes
    * parametric modulators per session and condition. */
  private def runCellGroups(events: Vector[Event], blockIds: Vector[Int]): Vector[(String, Vector[Int])] =
    val categorical = events.collect { case event: CategoricalEvent => event }
    val keys = blockIds.indices.map { row =>
      val cell = categorical.map(event => s"${event.varName}=${event.levels(event.codes(row))}")
      (s"run-${blockIds(row) + 1}" +: cell).mkString("|")
    }.toVector
    groupIndices(keys)

  private def formulaOrthogonalizationReceipts(
      termTag: Option[String],
      policy: Option[ModulatorOrthogonalization],
      lowered: OrthogonalizedEvents
  ): Vector[PolicyReceipt] =
    policy.toVector.map { value =>
      PolicyReceipt(
        "formula-orthogonalization",
        s"term=${termTag.getOrElse(value.term.value)};order=${value.order.map(_.value).mkString(",")};groups=run-by-cell;" +
          "reference=intercept+earlier-modulators;first=centered-within-group;" +
          s"first-means=${lowered.firstCentering.map((key, mean) => s"$key:${java.lang.Double.doubleToLongBits(mean)}").mkString(",")}"
      )
    }

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
                  policy = policy.canonical,
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
              val policyKind = policy match
                case MissingValuePolicy.ImputeConstant(_) => "impute-constant"
                case _ => policy.label
              diagnostics += EventModelDiagnostic(
                EventModelDiagnosticKind.NonFiniteModulator,
                termLabel,
                s"NA or non-finite values detected in continuous modulator '$colLabel' in term '$termLabel'; policy=$policyKind",
                eventIndex = firstMissingRow,
                column = Some(colLabel)
              )
            c += 1

          cleaned += (if copied then e.copy(value = scalafim.fmri.hrf.linalg.Mat.unsafe(m.rows, m.cols, out)) else e)

        case other => cleaned += other
      eventPosition += 1

    Right(SanitizedEvents(cleaned.result(), diagnostics.result(), missingValues.result()))

  private def missingPolicyFor(id: ModulatorId, plans: Vector[ObservedRequest], fallback: MissingValuePolicy): MissingValuePolicy =
    plans.find(_.source == id).fold(fallback)(_.missing)

  private def observedDropMask(events: Vector[Event], plans: Vector[ObservedRequest], fallback: MissingValuePolicy): Vector[Boolean] =
    Vector.tabulate(events.headOption.fold(0)(_.nEvents)): row =>
      !events.exists:
        case event: ContinuousEvent =>
          event.modulatorIds.indices.exists: column =>
            missingPolicyFor(event.modulatorIds(column), plans, fallback) == MissingValuePolicy.DropFromTerm &&
              !event.value(row, column).isFinite
        case _ => false

  /** `true` means that every deferred product component is observed for the row. */
  private def productObservedMask(products: Vector[ProductRequest], sourceRows: Vector[Int]): Vector[Boolean] =
    Vector.tabulate(sourceRows.length) { row =>
      products.forall { product =>
        val source = sourceRows(row)
        product.left(source).isFinite && product.right(source).isFinite
      }
    }

  /** Empty masks mean that their corresponding policy has no row exclusions;
    * non-empty masks of different lengths are an internal alignment error. */
  private def combineKeepMasks(left: Vector[Boolean], right: Vector[Boolean]): Either[DesignError, Vector[Boolean]] =
    if left.isEmpty then Right(right)
    else if right.isEmpty then Right(left)
    else if left.length != right.length then
      Left(DesignError.InvalidSchema(s"missing-value keep masks do not align with term rows (${left.length} vs ${right.length})"))
    else Right(Vector.tabulate(left.length)(row => left(row) && right(row)))

  private def prepareObserved(
      events: Vector[Event], plans: Vector[ObservedRequest], blocks: Vector[Int], provenance: Vector[EventRowProvenance]
  ): Either[DesignError, ObservedEvents] =
    if plans.isEmpty then Right(ObservedEvents(events, Vector.empty, Vector.empty))
    else
      val cells = blocks.indices.map: row =>
        CellKey.unsafe(events.collect:
          case factor: CategoricalEvent =>
            CellAssignment(FactorId.unsafe(factor.varName), LevelId.unsafe(factor.levels(factor.codes(row))))
        )
      val missing = Vector.newBuilder[MissingValueResolution]
      val policies = Vector.newBuilder[PolicyReceipt]
      val output = Vector.newBuilder[Event]
      var eventIndex = 0
      while eventIndex < events.length do
        events(eventIndex) match
          case event: ContinuousEvent =>
            val data = event.value.data.clone()
            var column = 0
            while column < event.value.cols do
              plans.find(_.source == event.modulatorIds(column)) match
                case None => ()
                case Some(plan) =>
                  var row = 0
                  while row < event.value.rows do
                    if !event.value(row, column).isFinite then
                      missing += MissingValueResolution(plan.source, row, plan.missing.canonical,
                        "zero-contribution", event.columnTags.lift(column), provenance.lift(row))
                    row += 1
                  val runs = blocks.distinct
                  var runIndex = 0
                  while runIndex < runs.length do
                    val indices = blocks.indices.filter(blocks(_) == runs(runIndex)).toVector
                    ObservedModulator.prepare(indices.map(event.value(_, column)), indices.map(cells),
                      plan.centering, plan.scaling, plan.missing) match
                      case Left(error) => return Left(DesignError.FormulaBinding(error.message))
                      case Right(result) =>
                        if result.retainedIndices.length != indices.length then
                          return Left(DesignError.InvalidSchema("observed modulator drop mask was not applied to the term"))
                        var local = 0
                        while local < indices.length do
                          data(indices(local) * event.value.cols + column) = result.values(local)
                          local += 1
                        val receipt = result.receipt
                        val cellsDetail = receipt.groups.map { group =>
                          s"${group.cell.fold("all")(_.canonical)}:n=${group.observedIndices.length}:mean=${group.mean.fold("")(_.toString)}"
                        }.mkString(",")
                        policies += PolicyReceipt("observed-modulator",
                          s"modulator=${plan.source.value};run=${runs(runIndex)};center=${plan.centering};effective-center=${receipt.effectiveCentering};" +
                            s"scale=${plan.scaling};missing=${plan.missing.canonical};divisor=${receipt.scale};observed=${receipt.observedIndices.length};" +
                            s"degenerate=${receipt.degenerate};degenerate-scale=${receipt.degenerateScale};groups=$cellsDetail")
                        if receipt.degenerate then
                          val reason =
                            if receipt.observedIndices.length < 2 then s"fewer than two observed values (${receipt.observedIndices.length})"
                            else "prepared values do not vary within the run"
                          policies += PolicyReceipt("observed-modulator-degenerate",
                            s"modulator=${plan.source.value};run=${runs(runIndex)};effective-center=${receipt.effectiveCentering};reason=$reason")
                    runIndex += 1
              column += 1
            output += event.copy(value = scalafim.fmri.hrf.linalg.Mat.unsafe(event.value.rows, event.value.cols, data))
          case other => output += other
        eventIndex += 1
      Right(ObservedEvents(output.result(), missing.result(), policies.result()))

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
                policy = policy.canonical,
                action = action,
                column = e.columnTags.lift(col),
                source = provenance.lift(eventIndex)
              )
            col += 1
        case _ => ()
      }
      eventIndex += 1
    out.result()

  /** Materialize products only after subsetting. Both component means use the
    * same observed intersection, so a missing component cannot influence the
    * other component's centering reference. */
  private def applyProducts(
      events: Vector[Event],
      products: Vector[ProductRequest],
      sourceRows: Vector[Int],
      blocks: Vector[Int]
  ): Either[DesignError, Vector[Event]] =
    if products.isEmpty then Right(events)
    else if sourceRows.length != blocks.length then Left(DesignError.InvalidSchema("product source rows do not align with term rows"))
    else
      products.foldLeft[Either[DesignError, Map[ModulatorId, Vector[Double]]]](Right(Map.empty)) { (acc, request) =>
        for
          values <- acc
          product <- centeredProductValues(request, sourceRows, blocks)
        yield values.updated(request.modulator, product)
      }.map: productsById =>
        events.map {
          case event: ContinuousEvent =>
            val output = event.value.data.clone()
            event.modulatorIds.zipWithIndex.foreach: (id, column) =>
              productsById.get(id).foreach: values =>
                var row = 0
                while row < values.size do
                  output(row * event.value.cols + column) = values(row)
                  row += 1
            event.copy(value = scalafim.fmri.hrf.linalg.Mat.unsafe(event.value.rows, event.value.cols, output), basis = event.basis)
          case event => event
        }

  private def centeredProductValues(request: ProductRequest, sourceRows: Vector[Int], blocks: Vector[Int]): Either[DesignError, Vector[Double]] =
    val sums = scala.collection.mutable.Map.empty[Int, (Double, Double, Int)]
    var row = 0
    while row < sourceRows.size do
      val source = sourceRows(row)
      if request.left(source).isFinite && request.right(source).isFinite then
        val (left, right, count) = sums.getOrElse(blocks(row), (0.0, 0.0, 0))
        val nextLeft = left + request.left(source)
        val nextRight = right + request.right(source)
        if !nextLeft.isFinite || !nextRight.isFinite then return Left(DesignError.FormulaBinding(s"non-finite centered-product sum for '${request.modulator.value}'"))
        sums.update(blocks(row), (nextLeft, nextRight, count + 1))
      row += 1
    val values = Vector.newBuilder[Double]
    row = 0
    while row < sourceRows.size do
      val source = sourceRows(row)
      if !request.left(source).isFinite || !request.right(source).isFinite then values += Double.NaN
      else
        val (left, right, count) = sums(blocks(row))
        val value = (request.left(source) - left / count) * (request.right(source) - right / count)
        if !value.isFinite then return Left(DesignError.FormulaBinding(s"non-finite centered product for '${request.modulator.value}'"))
        values += value
      row += 1
    Right(values.result())

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
      case ArgValue.Num(1.0) =>
        Right(EventExpression(Event.factor(Vector.fill(data.nrows)("events"), "all"), Vector.empty))
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
      "modulator",
      "scale_within_run",
      "sd_within_run",
      "product",
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
      case "modulator" =>
        observedModulatorExpression(data, args)
      case "scale_within_run" =>
        observedModulatorAlias(data, args, "scale_within_run", center = "run", scale = "z")
      case "sd_within_run" =>
        observedModulatorAlias(data, args, "sd_within_run", center = "none", scale = "sd")
      case "modulators" =>
        modulatorFamilyExpression(data, args, termTag, factorLevels, blockIds)
      case "product" =>
        Left(DesignError.FormulaBinding("product(...) is only valid as a member of modulators(...)"))
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

  private def centeredProductExpression(data: DataTable, args: Vector[Arg]): Either[DesignError, EventExpression] =
    def source(value: ArgValue): Either[DesignError, (ColumnId, Vector[Double])] = value match
      case ArgValue.Call(name, Vector(Arg(None, ArgValue.Ident(id)))) if name.equalsIgnoreCase("center_within_run") =>
        data.get[Double](id).map(id -> _)
      case _ => Left(DesignError.FormulaBinding("product requires center_within_run(column) components"))
    if args.length != 2 || args.exists(_.name.nonEmpty) then
      Left(DesignError.FormulaBinding("product requires exactly two positional centered components"))
    else
      for
        left <- source(args(0).value)
        right <- source(args(1).value)
      yield
        val name = s"${left._1.value}_x_${right._1.value}"
        val event = Event.variable(Vector.fill(data.nrows)(0.0), name)
        EventExpression(event, Vector.empty, products = Vector(ProductRequest(event.modulatorIds.head, left._1, right._1, left._2, right._2)))

  private def observedModulatorExpression(data: DataTable, args: Vector[Arg]): Either[DesignError, EventExpression] =
    val allowed = Set("center", "scale", "missing")
    val named = args.flatMap(_.name)
    if named.distinct.length != named.length || named.exists(n => !allowed(n)) || args.count(_.name.isEmpty) != 1 then
      Left(DesignError.FormulaBinding("modulator(x, center=..., scale=..., missing=...) requires one column and unique known options"))
    else
      def option(name: String, default: String): Either[DesignError, String] =
        args.find(_.name.contains(name)).map(_.value) match
          case None => Right(default)
          case Some(ArgValue.Str(value)) => Right(value)
          case Some(ArgValue.Ident(value)) => Right(value.value)
          case _ => Left(DesignError.FormulaBinding(s"modulator $name must be a string or identifier"))
      for
        source <- args.find(_.name.isEmpty).map(_.value) match
          case Some(ArgValue.Ident(id)) => Right(id)
          case _ => Left(DesignError.FormulaBinding("modulator source must be a numeric column"))
        values <- data.get[Double](source)
        centerName <- option("center", "none")
        center <- centerName match
          case "none" => Right(ObservedModulator.Centering.None)
          case "run" => Right(ObservedModulator.Centering.Global)
          case "cell" => Right(ObservedModulator.Centering.ByCell)
          case _ => Left(DesignError.FormulaBinding(s"unknown modulator center '$centerName'"))
        scaleName <- option("scale", "raw")
        scale <- scaleName match
          case "raw" => Right(ObservedModulator.Scaling.Raw)
          case "z" => Right(ObservedModulator.Scaling.ZScore)
          case "sd" => Right(ObservedModulator.Scaling.StandardDeviation)
          case _ => Left(DesignError.FormulaBinding(s"unknown modulator scale '$scaleName'"))
        missingName <- option("missing", "reject")
        missing <- missingName match
          case "reject" => Right(MissingValuePolicy.Reject)
          case "drop_from_term" => Right(MissingValuePolicy.DropFromTerm)
          case "zero" => Right(MissingValuePolicy.ZeroContribution)
          case _ => Left(DesignError.FormulaBinding(s"unknown modulator missing policy '$missingName'"))
        _ <- if missing == MissingValuePolicy.ZeroContribution && center == ObservedModulator.Centering.None && scale != ObservedModulator.Scaling.ZScore then
          Left(DesignError.FormulaBinding("zero contribution requires centering")) else Right(())
      yield
        val event = Event.variable(values, source.value)
        EventExpression(event, Vector.empty,
          observed = Vector(ObservedRequest(event.modulatorIds.head, center, scale, missing)))

  /** Portable spellings for the two observed-value transforms.  They lower to
    * the same checked policy path as `modulator`, so missing-value handling
    * and receipts cannot diverge. */
  private def observedModulatorAlias(
      data: DataTable,
      args: Vector[Arg],
      name: String,
      center: String,
      scale: String
  ): Either[DesignError, EventExpression] =
    if args.length != 1 || args.head.name.nonEmpty then
      Left(DesignError.FormulaBinding(s"$name requires exactly one positional numeric column"))
    else
      observedModulatorExpression(
        data,
        Vector(
          args.head,
          Arg(Some("center"), ArgValue.Ident(ColumnId.unsafe(center))),
          Arg(Some("scale"), ArgValue.Ident(ColumnId.unsafe(scale))),
          Arg(Some("missing"), ArgValue.Ident(ColumnId.unsafe("reject")))
        )
      )

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
      val members = Vector.newBuilder[(ModulatorId, ContinuousEvent, Vector[EventModelDiagnostic], Vector[CenteringRequest], Vector[ObservedRequest], Vector[ProductRequest])]
      var failed: Option[DesignError] = None
      var index = 0
      while index < args.length && failed.isEmpty do
        args(index).value match
          case ArgValue.Call(fun, _) if fun.trim.equalsIgnoreCase("modulators") =>
            failed = Some(DesignError.FormulaBinding("modulators(...) cannot be nested"))
          case ArgValue.Call(fun, productArgs) if fun.trim.equalsIgnoreCase("product") =>
            centeredProductExpression(data, productArgs) match
              case Left(error) => failed = Some(error)
              case Right(expression) =>
                expression.event match
                  case event: ContinuousEvent =>
                    members += ((event.modulatorIds.head, event, expression.diagnostics, expression.centering, expression.observed, expression.products))
                  case _ => failed = Some(DesignError.InvalidSchema("product must realize a continuous modulator"))
          case value =>
            toEvent(data, value, termTag, factorLevels, blockIds) match
              case Left(error) => failed = Some(error)
              case Right(expression) =>
                expression.event match
                  case event: ContinuousEvent if event.value.cols == 1 =>
                    event.modulatorIds match
                      case Vector(modulator) =>
                        members += ((modulator, event, expression.diagnostics, expression.centering, expression.observed, expression.products))
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
          val columns = values.map { case (modulator, event, _, _, _, _) =>
            modulator -> matCol(event.value, 0)
          }
          Event.modulatorFamily(columns, termTag.fold("modulators")(_ + "_modulators")).map { event =>
            EventExpression(
              event = event,
              diagnostics = values.flatMap(_._3),
              centering = values.flatMap(_._4),
              observed = values.flatMap(_._5),
              products = values.flatMap(_._6)
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

  private def resolveHrfEither(call: HrfCall, defaultHrf: Hrf, frame: Option[SamplingFrame] = None): Either[DesignError, Hrf] =
    if call.span.nonEmpty && !call.basis.exists(b => Set("fir", "bspline", "tent", "fourier").contains(b.trim.toLowerCase)) then
      return Left(DesignError.FormulaBinding("formula span requires an explicit fir, bspline, tent, or fourier basis"))
    if call.hrfFun.nonEmpty && (call.span.nonEmpty || call.kernelNormalization.nonEmpty) then
      return Left(DesignError.FormulaBinding("kernel options cannot be combined with hrf_fun"))
    val span = call.span.fold(24.s)(_.seconds)
    if call.temporalDerivative.nonEmpty && !call.basis.exists(name => Set("spmg2", "spmg3").contains(name.trim.toLowerCase)) then
      return Left(DesignError.FormulaBinding("temporal_derivative requires an explicit spmg2 or spmg3 basis"))
    // "spm-1s" is SPM12's informed basis (spm_get_bf + spm_orth) on SPM's
    // kernel grid, dt = TR / 16 over 32 s, so it needs one repetition time.
    def spmGrid: Either[DesignError, SpmKernelGrid] =
      frame.map(_.tr.distinct) match
        case Some(Vector(tr)) => SpmKernelGrid(tr).left.map(error => DesignError.FormulaBinding(error.message))
        case Some(trs) =>
          Left(DesignError.FormulaBinding(s"temporal_derivative = spm-1s requires one repetition time across runs, found ${trs.map(_.value).mkString(", ")}"))
        case None => Left(DesignError.FormulaBinding("temporal_derivative = spm-1s requires the sampling frame repetition time"))
    def informedBasis(columns: Int): Either[DesignError, Hrf] =
      call.temporalDerivative match
        case None | Some(TemporalDerivativeConvention.AnalyticSpmg) => Right(if columns == 2 then Hrfs.SPMG2 else Hrfs.SPMG3)
        case Some(TemporalDerivativeConvention.SpmOneSecondBackwardDifference) =>
          spmGrid.flatMap: grid =>
            TemporalDerivativeConvention.spmInformedBasis(Hrfs.SPMG1, columns, grid).left.map(error => DesignError.FormulaBinding(error.message))
    val baseEither: Either[DesignError, Hrf] =
      call.basis match
        case None => Right(defaultHrf)
        case Some(basisName0) =>
          val basisName = basisName0.trim.toLowerCase
          basisName match
            case "spmg1"    => Right(Hrfs.SPMG1)
            case "spmg2"    => informedBasis(2)
            case "spmg3"    => informedBasis(3)
            case "gamma"    => Right(Hrfs.Gamma)
            case "gaussian" => Right(Hrfs.Gaussian)
            case "fir"      => catchBuild(DesignError.fromThrowable)(Hrfs.fir(nBasis = call.nbasis.getOrElse(12), span = span))
            case "bspline"  => catchBuild(DesignError.fromThrowable)(Hrfs.bspline(nBasis = call.nbasis.getOrElse(5), span = span))
            case "tent"     => catchBuild(DesignError.fromThrowable)(Hrfs.tent(nBasis = call.nbasis.getOrElse(5), span = span))
            case "fourier"  => catchBuild(DesignError.fromThrowable)(Hrfs.fourier(nBasis = call.nbasis.getOrElse(5), span = span))
            case other      => Left(DesignError.UnknownBasis(other))

    val normalized = baseEither.flatMap { base =>
      call.kernelNormalization match
        case None => Right(base)
        case Some(mode) => base.normalize(mode).left.map(error => DesignError.FormulaBinding(error.message))
    }
    normalized.flatMap { base =>
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
