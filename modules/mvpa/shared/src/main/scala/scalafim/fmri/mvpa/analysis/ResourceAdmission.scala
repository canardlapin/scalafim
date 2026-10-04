package scalafim.fmri.mvpa.analysis

/** Bounds are supplied by their owning provider. Known is a declaration, not
  * a measurement of process RSS or a certificate discovered by inspection.
  */
enum ResourceBound:
  case Known(bytes: BigInt, receipt: String)
  case Unknown(reason: String)

final case class ResourceFootprint(
    source: ResourceBound,
    workerScratch: ResourceBound,
    workers: Int,
    ownedWorkerBytes: BigInt,
    retainedOutputBytes: BigInt,
    sharedBytes: BigInt,
    maximumProviderWorkers: Int = 1
):
  require(workers > 0 && maximumProviderWorkers > 0 && ownedWorkerBytes >= 0 && retainedOutputBytes >= 0 && sharedBytes >= 0)

enum ResourceLimit:
  /** Enforces only the explicitly counted numeric allocations owned by the
    * adapter. Unknown source/provider costs remain visible in the receipt. */
  case OwnedNumeric(maximumBytes: Long)
  /** Conditional bound requiring numeric source and provider declarations.
    * Owned formulas are checked; provider claims are not independently measured. Objects,
    * allocator/GC overhead and other processes require external isolation. */
  case WholeNumeric(maximumBytes: Long)

enum MaterializationPolicy:
  case ForbidSourceCopy
  case AllowSourceCopy(maximumBytes: Long)

final case class ResourceBudget(memory: ResourceLimit, materialization: MaterializationPolicy):
  require((memory match
    case ResourceLimit.OwnedNumeric(value) => value >= 0
    case ResourceLimit.WholeNumeric(value) => value >= 0))
  require((materialization match
    case MaterializationPolicy.ForbidSourceCopy => true
    case MaterializationPolicy.AllowSourceCopy(value) => value >= 0))

enum ResourceError:
  case InvalidBound(field: String)
  case Overflow(field: String, required: BigInt)
  case UnknownStrictCost(fields: Vector[String])
  case MemoryExceeded(required: BigInt, allowed: Long)
  case MaterializationRefused(required: BigInt)
  case MaterializationExceeded(required: BigInt, allowed: Long)
  case ScientificPlanMismatch
  case ConcurrencyUnsupported(requested: Int, supported: Int)
  case NoAdmittedRoute(rejections: Vector[(String, ResourceError)])

/** The method supplies equivalence evidence for alternatives of the exact same
  * bound scientific plan. Resource selection never modifies that plan. */
final case class ResourceCandidate(
    plan: PlanId, route: String, equivalenceReceipt: String,
    footprint: ResourceFootprint, sourceCopyBytes: BigInt,
    estimatedWork: BigInt
):
  require(route.trim.nonEmpty && equivalenceReceipt.trim.nonEmpty)
  require(sourceCopyBytes >= 0 && estimatedWork >= 0)

final class NumericResourceAdmission private[analysis] (
    val footprint: ResourceFootprint, val sourceCopyBytes: BigInt,
    val ownedNumericBytes: Long, val wholeNumericBytes: Option[Long],
    val unknownCosts: Vector[String]
)

final class ResourceAdmission private[analysis] (
    val candidate: ResourceCandidate, val numeric: NumericResourceAdmission
):
  def ownedNumericBytes: Long = numeric.ownedNumericBytes
  def wholeNumericBytes: Option[Long] = numeric.wholeNumericBytes
  def unknownCosts: Vector[String] = numeric.unknownCosts

object ResourceAdmission:
  /** Metadata only. BigInt arithmetic precedes every machine-width conversion. */
  def bytes(rows: BigInt, columns: BigInt, bytesPerCell: Int = 8): Either[ResourceError, Long] =
    val result = rows * columns * bytesPerCell
    if rows < 0 || columns < 0 || bytesPerCell <= 0 then Left(ResourceError.InvalidBound("shape"))
    else if result > Long.MaxValue then Left(ResourceError.Overflow("shape bytes", result))
    else Right(result.toLong)

  def evaluate(candidate: ResourceCandidate, budget: ResourceBudget): Either[ResourceError, ResourceAdmission] =
    evaluateFootprint(candidate.footprint, candidate.sourceCopyBytes, budget)
      .map(value => new ResourceAdmission(candidate, value))

  def evaluateFootprint(f: ResourceFootprint, sourceCopyBytes: BigInt, budget: ResourceBudget): Either[ResourceError, NumericResourceAdmission] =
    val bounds = Vector("source" -> f.source, "worker scratch" -> f.workerScratch)
    val invalid = bounds.collectFirst:
      case (name, ResourceBound.Known(value, receipt)) if value < 0 || receipt.trim.isEmpty => name
      case (name, ResourceBound.Unknown(reason)) if reason.trim.isEmpty => name
    val unknown = bounds.collect { case (name, ResourceBound.Unknown(reason)) => s"$name: $reason" }
    val owned = f.sharedBytes + f.retainedOutputBytes + BigInt(f.workers) * f.ownedWorkerBytes
    val known = bounds.map:
      case ("source", ResourceBound.Known(value, _)) => value
      case (_, ResourceBound.Known(value, _)) => BigInt(f.workers) * value
      case _ => BigInt(0)
    val whole = owned + known.sum
    invalid match
      case _ if sourceCopyBytes < 0 => Left(ResourceError.InvalidBound("source copy"))
      case _ if f.workers > f.maximumProviderWorkers => Left(ResourceError.ConcurrencyUnsupported(f.workers, f.maximumProviderWorkers))
      case Some(field) => Left(ResourceError.InvalidBound(field))
      case None =>
        val materialization = budget.materialization match
          case MaterializationPolicy.ForbidSourceCopy if sourceCopyBytes > 0 => Left(ResourceError.MaterializationRefused(sourceCopyBytes))
          case MaterializationPolicy.AllowSourceCopy(maximum) if sourceCopyBytes > maximum => Left(ResourceError.MaterializationExceeded(sourceCopyBytes, maximum))
          case _ => Right(())
        materialization.flatMap: _ =>
          val (required, allowed) = budget.memory match
            case ResourceLimit.OwnedNumeric(maximum) => (owned, maximum)
            case ResourceLimit.WholeNumeric(maximum) => (whole, maximum)
          if budget.memory.isInstanceOf[ResourceLimit.WholeNumeric] && unknown.nonEmpty then Left(ResourceError.UnknownStrictCost(unknown))
          else if owned > Long.MaxValue || (budget.memory.isInstanceOf[ResourceLimit.WholeNumeric] && whole > Long.MaxValue) then Left(ResourceError.Overflow("live numeric bytes", required))
          else if required > allowed then Left(ResourceError.MemoryExceeded(required, allowed))
          else Right(new NumericResourceAdmission(f, sourceCopyBytes, owned.toLong, if unknown.isEmpty && whole <= Long.MaxValue then Some(whole.toLong) else None, unknown ++ (if whole > Long.MaxValue then Vector("whole numeric bytes exceed Long capacity") else Vector.empty)))

  /** Work estimates rank only method-certified alternatives. Stable route name
    * ordering makes ties deterministic; no population or randomization edits. */
  def choose(plan: PlanId, candidates: Vector[ResourceCandidate], budget: ResourceBudget): Either[ResourceError, ResourceAdmission] =
    if candidates.exists(_.plan != plan) then Left(ResourceError.ScientificPlanMismatch)
    else
      val evaluated = candidates.sortBy(value => (value.estimatedWork, value.route)).map(value => value.route -> evaluate(value, budget))
      evaluated.collectFirst { case (_, Right(admitted)) => admitted } match
        case Some(admitted) => Right(admitted)
        case None => Left(ResourceError.NoAdmittedRoute(evaluated.collect { case (route, Left(error)) => route -> error }))
