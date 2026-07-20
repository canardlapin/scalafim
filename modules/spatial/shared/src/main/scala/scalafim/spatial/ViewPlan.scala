package scalafim.spatial

opaque type FieldRootId = String

object FieldRootId:
  def apply(value: String): Either[ViewPlanError, FieldRootId] =
    val normalized = value.trim
    if normalized.isEmpty then Left(ViewPlanError.EmptyRootId)
    else Right(normalized)

  private[spatial] def unsafe(value: String): FieldRootId =
    value

  extension (id: FieldRootId)
    def value: String =
      id

enum ViewPlanError:
  case EmptyRootId
  case InvalidRootShape(sampleCount: Int, observations: Int)
  case InvalidTargetShape(target: DomainId, sampleCount: Int)
  case InvalidSelection(error: SpatialError)
  case InvalidDemand(error: DemandError)
  case SelectionMustBeTerminal(domain: DomainId)

  def message: String =
    this match
      case EmptyRootId =>
        "field root id must be non-empty"
      case InvalidRootShape(sampleCount, observations) =>
        s"field root shape must be non-negative: samples=$sampleCount observations=$observations"
      case InvalidTargetShape(target, sampleCount) =>
        s"view target ${target.value} must have a non-negative sample count: $sampleCount"
      case InvalidSelection(error) =>
        error.message
      case InvalidDemand(error) =>
        error.message
      case SelectionMustBeTerminal(domain) =>
        s"row selection in ${domain.value} must remain terminal before another spatial transform"

enum ViewStepOrigin:
  case Descriptive
  case LegacyCompiled(path: Vector[MorphismId], compiler: String)

enum ViewPlanStep:
  case Reexpress(
    source: DomainId,
    target: DomainId,
    targetSampleCount: Int,
    routing: RoutingPolicy,
    sampling: SamplingPolicy,
    allowInverses: Boolean,
    origin: ViewStepOrigin
  )
  case SelectRows(
    domain: DomainId,
    requested: RowSelection,
    resolvedTargetRows: Vector[Int]
  )
  case SelectDemand(
    domain: DomainId,
    requested: FieldDemand,
    resolvedTargetRows: Vector[Int],
    resolvedObservations: Vector[Int]
  )

final case class ViewIntent(
  root: DomainId,
  target: DomainId,
  targetSampleCount: Int,
  demand: ResolvedDemand,
  routing: RoutingPolicy,
  sampling: SamplingPolicy,
  allowInverses: Boolean
):
  def rowSelection: RowSelection =
    demand.rowSelection

  def observationSelection: ObservationSelection =
    demand.observationSelection

  def selectedSampleCount: Int =
    demand.sampleCount

  def selectedObservations: Int =
    demand.observations

final case class ViewPlan private (
  rootId: FieldRootId,
  rootDomain: DomainId,
  currentDomain: DomainId,
  rootSampleCount: Int,
  rootObservations: Int,
  sampleCount: Int,
  observations: Int,
  intent: ViewIntent,
  steps: Vector[ViewPlanStep]
):
  require(rootSampleCount >= 0, "view root sample count must be non-negative")
  require(sampleCount >= 0, "view sample count must be non-negative")
  require(observations >= 0, "view observation count must be non-negative")
  require(rootObservations >= 0, "view root observation count must be non-negative")
  require(intent.root == rootDomain, "view intent must start at the root domain")
  require(intent.target == currentDomain, "view intent must end at the current domain")
  require(intent.selectedSampleCount == sampleCount, "view intent selection must match the view sample count")
  require(intent.selectedObservations == observations, "view intent observation demand must match the view observation count")

  def isRoot: Boolean =
    steps.isEmpty &&
      currentDomain == rootDomain &&
      sampleCount == rootSampleCount &&
      observations == rootObservations &&
      intent.demand.isFull

  def isView: Boolean =
    !isRoot

  def reexpress(
    target: DomainId,
    targetSampleCount: Int,
    routing: RoutingPolicy = RoutingPolicy.Shortest,
    sampling: SamplingPolicy = SamplingPolicy.Trilinear,
    allowInverses: Boolean = false
  ): Either[ViewPlanError, ViewPlan] =
    reexpressFrom(
      target,
      targetSampleCount,
      routing,
      sampling,
      allowInverses,
      ViewStepOrigin.Descriptive
    )

  def selectRows(selection: RowSelection): Either[ViewPlanError, ViewPlan] =
    selection match
      case RowSelection.All =>
        Right(this)
      case RowSelection.Rows(_) =>
        intent.demand
          .refineRows(selection)
          .left
          .map(error => ViewPlanError.InvalidSelection(rowSelectionError(error)))
          .map { resolved =>
            copy(
              sampleCount = resolved.sampleCount,
              observations = resolved.observations,
              intent = intent.copy(demand = resolved),
              steps = steps :+ ViewPlanStep.SelectRows(currentDomain, selection, resolved.targetRows)
            )
          }

  def select(
    requested: FieldDemand,
    domain: Domain
  ): Either[ViewPlanError, ViewPlan] =
    if domain.id != currentDomain then
      Left(ViewPlanError.InvalidDemand(DemandError.DomainMismatch(currentDomain, domain.id)))
    else if requested == FieldDemand.full then
      Right(this)
    else
      intent.demand
        .refine(requested, domain)
        .left
        .map(error => ViewPlanError.InvalidDemand(error))
        .map { resolved =>
          copy(
            sampleCount = resolved.sampleCount,
            observations = resolved.observations,
            intent = intent.copy(demand = resolved),
            steps = steps :+ ViewPlanStep.SelectDemand(
              currentDomain,
              requested,
              resolved.targetRows,
              resolved.observationIndices
            )
          )
        }

  private[spatial] def reexpressLegacy(
    operator: SpatialOperator
  ): Either[ViewPlanError, ViewPlan] =
    reexpressFrom(
      target = operator.target,
      targetSampleCount = operator.rows,
      routing = operator.provenance.routing,
      sampling = operator.provenance.sampling,
      allowInverses = operator.provenance.allowInverses,
      origin = ViewStepOrigin.LegacyCompiled(
        operator.provenance.path,
        operator.provenance.compiler
      )
    )

  private def reexpressFrom(
    target: DomainId,
    targetSampleCount: Int,
    routing: RoutingPolicy,
    sampling: SamplingPolicy,
    allowInverses: Boolean,
    origin: ViewStepOrigin
  ): Either[ViewPlanError, ViewPlan] =
    if targetSampleCount < 0 then
      Left(ViewPlanError.InvalidTargetShape(target, targetSampleCount))
    else if !intent.demand.isFull then
      Left(ViewPlanError.SelectionMustBeTerminal(currentDomain))
    else
      val step =
        ViewPlanStep.Reexpress(
          source = currentDomain,
          target = target,
          targetSampleCount = targetSampleCount,
          routing = routing,
          sampling = sampling,
          allowInverses = allowInverses,
          origin = origin
        )
      Right(
        copy(
          currentDomain = target,
          sampleCount = targetSampleCount,
          intent = ViewIntent(
            root = rootDomain,
            target = target,
            targetSampleCount = targetSampleCount,
            demand = ResolvedDemand.unsafeFull(targetSampleCount, rootObservations),
            routing = routing,
            sampling = sampling,
            allowInverses = allowInverses
          ),
          steps = steps :+ step
        )
      )

object ViewPlan:
  def root(
    rootId: FieldRootId,
    domain: DomainId,
    sampleCount: Int,
    observations: Int
  ): Either[ViewPlanError, ViewPlan] =
    if sampleCount < 0 || observations < 0 then
      Left(ViewPlanError.InvalidRootShape(sampleCount, observations))
    else
      Right(unsafeRoot(rootId, domain, sampleCount, observations))

  private[spatial] def unsafeRoot(
    rootId: FieldRootId,
    domain: DomainId,
    sampleCount: Int,
    observations: Int
  ): ViewPlan =
    ViewPlan(
      rootId = rootId,
      rootDomain = domain,
      currentDomain = domain,
      rootSampleCount = sampleCount,
      rootObservations = observations,
      sampleCount = sampleCount,
      observations = observations,
      intent = ViewIntent(
        root = domain,
        target = domain,
        targetSampleCount = sampleCount,
        demand = ResolvedDemand.unsafeFull(sampleCount, observations),
        routing = RoutingPolicy.Shortest,
        sampling = SamplingPolicy.Trilinear,
        allowInverses = false
      ),
      steps = Vector.empty
      )

private def rowSelectionError(error: DemandError): SpatialError =
  error match
    case DemandError.EmptySelection(_) =>
      SpatialError.EmptyRoi
    case DemandError.DuplicateIndex(_, index) =>
      SpatialError.DuplicateRoiRow(index)
    case DemandError.IndexOutOfBounds(_, index, limit) =>
      SpatialError.InvalidRoiRow(index, limit)
    case other =>
      SpatialError.OperatorAssemblyFailed(other.message)
