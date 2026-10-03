package scalafim.fmri.mvpa.analysis

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SemanticSpace}
import scala.util.control.NonFatal
import scalafim.fmri.mvpa.{AxisDigest, AxisRef, EvidenceError, Observations}

/** Declared provider costs. These are provider statements, not RSS probes.
  * Source includes resident evidence and live source buffers. applicationBuffers
  * must come from an enclosing matrix-level admission: the vector provider
  * cannot certify arbitrary caller-owned matrix RHS/output widths. */
final case class ObservationProviderCosts(
    source: ResourceBound = ResourceBound.Unknown("provider source bytes not declared"),
    workerScratch: ResourceBound = ResourceBound.Unknown("provider scratch bytes not declared"),
    workers: Int = 1,
    applicationBuffers: ResourceBound = ResourceBound.Unknown("caller-owned matrix input/output storage has no enclosing admission")
):
  require(workers > 0)

enum ObservationProductRoute:
  case Direct
  case OwnedDenseCopy

/** A provider must explicitly offer a bounded replay scope. One-shot evidence
  * cannot be promoted to a replayable product by this adapter.
  */
enum ObservationReplay:
  case OneShot
  case Scoped(owner: String, revision: String)

trait ObservationProductResource:
  def acquire(): Either[String, Unit]
  def close(): Either[String, Unit]

enum ObservationProductError:
  case Resource(error: ResourceError)
  case ReplayRequired
  case ConcurrentWorkersUnsupported(workers: Int)
  case Capacity(requiredCells: BigInt)
  case InvalidShape(detail: String)
  case Access(detail: String)
  case Construction(error: EvidenceError)
  case Materialization(detail: String)
  case TaskFailure(detail: String)
  case CloseFailure(detail: String)
  case TaskAndCloseFailure(task: ObservationProductError, close: String)

/** Actual adapter calls. Provider process memory and execution time are never
  * inferred from these counts.
  */
final case class ObservationProductWork(
    forwardCalls: Long,
    adjointCalls: Long,
    forwardColumns: Long,
    adjointColumns: Long,
    returnedCells: Long,
    materializationForwardCalls: Long,
    materializationForwardColumns: Long,
    materializationReturnedCells: Long,
    copiedCells: Long,
    attemptedCells: Long = 0L,
    fitCalls: Long = 0L,
    ownedCopyArrays: Long = 0L
)

private final class ProductCounter:
  private var forwardCalls0 = 0L
  private var adjointCalls0 = 0L
  private var forwardColumns0 = 0L
  private var adjointColumns0 = 0L
  private var returnedCells0 = 0L
  private var materializationForwardCalls0 = 0L
  private var materializationForwardColumns0 = 0L
  private var materializationReturnedCells0 = 0L
  private var copiedCells0 = 0L
  private var attemptedCells0 = 0L
  private var fitCalls0 = 0L
  private var ownedCopyArrays0 = 0L
  def allocatedCopyArray(): Unit = ownedCopyArrays0 += 1L
  private var probeLimits = Vector.empty[(Long, Long, Long, Long)]

  def bounded[A](maximumCells: Long, maximumFits: Long)(callback: => A): A =
    val before = probeLimits
    probeLimits :+= (attemptedCells0, maximumCells, fitCalls0, maximumFits)
    try callback finally probeLimits = before

  private def charge(cells: Long): Unit =
    val next = BigInt(attemptedCells0) + cells
    if next > Long.MaxValue || probeLimits.exists((start, allowed, _, _) => next - start > allowed) then
      throw IllegalStateException("probe cell budget would be exceeded before provider read")
    attemptedCells0 = next.toLong

  def fit[A](callback: => A): A =
    val next = BigInt(fitCalls0) + 1
    if next > Long.MaxValue || probeLimits.exists((_, _, start, allowed) => next - start > allowed) then
      throw IllegalStateException("probe fit budget would be exceeded before fitting")
    fitCalls0 = next.toLong
    callback

  def forwardAttempt(columns: Int, cells: Long, materialization: Boolean): Unit = synchronized:
    charge(cells)
    if materialization then
      materializationForwardCalls0 += 1L
      materializationForwardColumns0 += columns.toLong
    else
      forwardCalls0 += 1L
      forwardColumns0 += columns.toLong

  def forwardReturned(cells: Long, materialization: Boolean): Unit = synchronized:
    if materialization then materializationReturnedCells0 += cells
    else returnedCells0 += cells

  def adjointAttempt(columns: Int, cells: Long): Unit = synchronized:
    charge(cells)
    adjointCalls0 += 1L
    adjointColumns0 += columns.toLong

  def adjointReturned(cells: Long): Unit = synchronized(returnedCells0 += cells)

  def copied(cells: Long): Unit = synchronized(copiedCells0 += cells)

  def snapshot: ObservationProductWork = synchronized:
    ObservationProductWork(
      forwardCalls0, adjointCalls0, forwardColumns0, adjointColumns0,
      returnedCells0, materializationForwardCalls0,
      materializationForwardColumns0, materializationReturnedCells0, copiedCells0, attemptedCells0, fitCalls0, ownedCopyArrays0
    )

private final class ProductScope:
  private var open = true
  def requireOpen(): Unit = synchronized:
    if !open then throw IllegalStateException("observation product scope is closed")
  def expire(): Unit = synchronized:
    open = false

final class PreparedObservationProduct[S <: SemanticSpace, N <: SemanticSpace] private[analysis] (
    val observations: Observations[S, N],
    val route: ObservationProductRoute,
    val admission: NumericResourceAdmission,
    private val counter: ProductCounter,
    private val scope: ProductScope
):
  val evidenceIdentity: String = AxisDigest.sha256Hex: writer =>
    writer.string("observation-product-evidence-v1")
    observations.identity.writeFramed(writer)
  val provenanceIdentity: String = AxisDigest.sha256Hex: writer =>
    writer.string("observation-product-lineage-v1")
    observations.identity.writeFramed(writer)
  def exposureReference(plan: PlanId, result: ResultIdentity): ExposureReference =
    ExposureReference(plan, evidenceIdentity, provenanceIdentity, result)
  def work: ObservationProductWork = counter.snapshot
  private[analysis] def bounded[A](maximumCells: Long, maximumFits: Long)(callback: => A): A = counter.bounded(maximumCells, maximumFits)(callback)
  /** Called by the owning fitter at each concrete fit attempt. */
  def fit[A](callback: => A): A =
    scope.requireOpen()
    counter.fit(callback)

final case class ObservationCostProbe[A](
    attempt: ExposureAttempt[A], work: ObservationProductWork, elapsedNanos: Long,
    request: ExposureRequest, maximumFits: Long, evidenceIdentity: String
)

object ObservationProduct:
  /** Acquire exactly one provider resource, prepare the selected equivalent
    * route, and expire every escaped operator before closing that resource.
    */
  def withPrepared[SK, NK, A](
      samples: AxisRef[SK],
      neural: AxisRef[NK],
      observations: Observations[samples.Id, neural.Id],
      replay: ObservationReplay,
      resource: ObservationProductResource,
      costs: ObservationProviderCosts,
      route: ObservationProductRoute,
      budget: ResourceBudget,
      equivalenceReceipt: String
  )(task: PreparedObservationProduct[samples.Id, neural.Id] => Either[ObservationProductError, A]): Either[ObservationProductError, A] =
    val checked =
      if observations.sampleAxis != samples.descriptor || observations.neuralAxis != neural.descriptor then
        Left(ObservationProductError.InvalidShape("nominal axis descriptors do not match observations"))
      else preflight(observations, replay, costs, route, budget, equivalenceReceipt)
    checked.flatMap: admission =>
      var acquired = false
      var scope: Option[ProductScope] = None
      val result =
        try
          resource.acquire().left.map(ObservationProductError.Access.apply).flatMap: _ =>
            acquired = true
            val madeScope = new ProductScope
            scope = Some(madeScope)
            prepare(samples, neural, observations, madeScope, route, admission).flatMap(task)
        catch case NonFatal(error) => Left(ObservationProductError.TaskFailure(detail(error)))
      // The capability expires before the provider is closed whether task
      // construction, materialization, or the callback itself failed.
      scope.foreach(_.expire())
      val close = if acquired then
        try resource.close() catch case NonFatal(error) => Left(detail(error))
      else Right(())
      close match
        case Right(_) => result
        case Left(closeError) => result match
          case Right(_) => Left(ObservationProductError.CloseFailure(closeError))
          case Left(error) => Left(ObservationProductError.TaskAndCloseFailure(error, closeError))

  /** A bounded direct-product probe uses the immutable exposure mechanism.
    * Limits count only calls through this prepared product. Timing starts
    * after acquisition/preparation and is one sample, never a latency bound.
    * Dense-copy preparation must be exposure-accounted by its owning caller
    * and cannot be represented by this direct-only probe. A training probe
    * remains refused for external unknown evidence; this adapter only records
    * an instrumented callback attempt and never claims constrained reads.
    */
  def probe[S <: SemanticSpace, N <: SemanticSpace, A](
      product: PreparedObservationProduct[S, N],
      exposure: EvidenceExposure,
      request: ExposureRequest, maximumFits: Long = 0L
  )(callback: PreparedObservationProduct[S, N] => Either[String, A]): ObservationCostProbe[A] =
    require(maximumFits >= 0L, "fit budget must be nonnegative")
    val before = product.work
    val started = System.nanoTime()
    val attempt =
      if request.assurance != ExposureAssurance.Instrumented || product.route != ObservationProductRoute.Direct then
        ExposureAttempt.Refused(ExposureError.NativeAssuranceUnsupported, exposure)
      else if exposure.reference.evidenceIdentity != product.evidenceIdentity ||
          exposure.reference.provenanceIdentity != product.provenanceIdentity then
        ExposureAttempt.Refused(ExposureError.ReferenceMismatch, exposure)
      else ExposureControl.permit(exposure, request) match
        case Left(error) => ExposureAttempt.Refused(error, exposure)
        case Right(permit) =>
          ExposureControl.read(exposure, permit, request):
            product.bounded(request.maximumCells, maximumFits)(callback(product))
    val after = product.work
    val delta = ObservationProductWork(
      after.forwardCalls - before.forwardCalls, after.adjointCalls - before.adjointCalls,
      after.forwardColumns - before.forwardColumns, after.adjointColumns - before.adjointColumns,
      after.returnedCells - before.returnedCells,
      after.materializationForwardCalls - before.materializationForwardCalls,
      after.materializationForwardColumns - before.materializationForwardColumns,
      after.materializationReturnedCells - before.materializationReturnedCells,
      after.copiedCells - before.copiedCells, after.attemptedCells - before.attemptedCells,
      after.fitCalls - before.fitCalls, after.ownedCopyArrays - before.ownedCopyArrays
    )
    ObservationCostProbe(attempt, delta, math.max(0L, System.nanoTime() - started), request, maximumFits, product.evidenceIdentity)

  /** Metadata-only admission. It does not acquire or apply the source. */
  def preflight(
      observations: Observations[? <: SemanticSpace, ? <: SemanticSpace],
      replay: ObservationReplay,
      costs: ObservationProviderCosts,
      route: ObservationProductRoute,
      budget: ResourceBudget,
      equivalenceReceipt: String
  ): Either[ObservationProductError, NumericResourceAdmission] =
    val malformed = Vector("source" -> costs.source, "scratch" -> costs.workerScratch, "application buffers" -> costs.applicationBuffers).collectFirst:
      case (field, ResourceBound.Known(bytes, receipt)) if bytes < 0 || receipt.trim.isEmpty => field
      case (field, ResourceBound.Unknown(reason)) if reason.trim.isEmpty => field
    if malformed.nonEmpty then Left(ObservationProductError.Resource(ResourceError.InvalidBound(malformed.get)))
    else if equivalenceReceipt.trim.isEmpty then Left(ObservationProductError.InvalidShape("equivalence receipt is empty"))
    else replay match
      case ObservationReplay.OneShot => Left(ObservationProductError.ReplayRequired)
      case ObservationReplay.Scoped(owner, revision) if owner.trim.isEmpty || revision.trim.isEmpty =>
        Left(ObservationProductError.InvalidShape("replay owner and revision must be nonempty"))
      case ObservationReplay.Scoped(_, _) if costs.workers > 1 =>
        Left(ObservationProductError.ConcurrentWorkersUnsupported(costs.workers))
      case ObservationReplay.Scoped(_, _) =>
        val cells = BigInt(observations.rows) * BigInt(observations.columns)
        if observations.rows < 0 || observations.columns < 0 then Left(ObservationProductError.InvalidShape("negative observation shape"))
        // Direct composition does not allocate a dense n-by-p buffer. Its
        // input/output vectors are bounded by existing Int dimensions only.
        else if route == ObservationProductRoute.OwnedDenseCopy && cells > Int.MaxValue then Left(ObservationProductError.Capacity(cells))
        else
          val copyBytes = cells * 8
          val direct = route == ObservationProductRoute.Direct
          // Array, Vector conversion, DMat storage, guard-owned dense values,
          // p-wide basis, and an n-cell provider result are simultaneously
          // possible while the source remains acquired.
          val externalScratch = (costs.workerScratch, costs.applicationBuffers) match
            case (ResourceBound.Known(scratch, a), ResourceBound.Known(buffers, b)) =>
              ResourceBound.Known(scratch + buffers, s"provider=$a; enclosing application=$b")
            case _ => ResourceBound.Unknown("provider scratch and enclosing matrix input/output admission are both required")
          val footprint = ResourceFootprint(
            costs.source, externalScratch, 1,
            4 * (BigInt(observations.rows) + observations.columns) * 8,
            BigInt(0), if direct then BigInt(0) else cells * 4 * 8
          )
          ResourceAdmission.evaluateFootprint(footprint, if direct then BigInt(0) else copyBytes, budget)
            .left.map(ObservationProductError.Resource.apply)

  private def prepare[SK, NK](
      samples: AxisRef[SK], neural: AxisRef[NK],
      original: Observations[samples.Id, neural.Id],
      scope: ProductScope,
      route: ObservationProductRoute,
      admission: NumericResourceAdmission
  ): Either[ObservationProductError, PreparedObservationProduct[samples.Id, neural.Id]] =
    val counter = new ProductCounter
    route match
      case ObservationProductRoute.Direct =>
        guarded(samples, neural, original, scope, counter, materialization = false).map: observations =>
          new PreparedObservationProduct(observations, route, admission, counter, scope)
      case ObservationProductRoute.OwnedDenseCopy =>
        copyDense(samples, neural, original, scope, counter).flatMap: copied =>
          guardedDense(samples, neural, original, copied, scope, counter).map: observations =>
            new PreparedObservationProduct(observations, route, admission, counter, scope)

  private def copyDense[SK, NK](
      samples: AxisRef[SK], neural: AxisRef[NK], original: Observations[samples.Id, neural.Id],
      scope: ProductScope, counter: ProductCounter
  ): Either[ObservationProductError, DMat] =
    val data = new Array[Double](original.rows * original.columns)
    counter.allocatedCopyArray()
    var column = 0
    while column < original.columns do
      scope.requireOpen()
      val basis = DMat.dense(original.columns, 1, Vector.tabulate(original.columns)(index => if index == column then 1.0 else 0.0))
      counter.forwardAttempt(1, original.rows.toLong, materialization = true)
      original.patterns(basis) match
        case Left(error) => return Left(ObservationProductError.Materialization(error.message))
        case Right(value) =>
          counter.forwardReturned(value.rows.toLong, materialization = true)
          var row = 0
          while row < original.rows do
            data(row * original.columns + column) = value(row, 0)
            row += 1
          counter.copied(original.rows.toLong)
      column += 1
    // Retain all scientific evidence identity; only execution realization has
    // changed. The guarded dense operator prevents escaped use after closure.
    Right(DMat.dense(original.rows, original.columns, data.toVector))

  private def guarded[SK, NK](
      samples: AxisRef[SK], neural: AxisRef[NK], original: Observations[samples.Id, neural.Id], scope: ProductScope,
      counter: ProductCounter, materialization: Boolean
  ): Either[ObservationProductError, Observations[samples.Id, neural.Id]] =
    val operator = new DoubleLinearOperator:
      val rows = original.rows
      val cols = original.columns
      def applyTo(input: DVec, output: MutableDVec): Unit =
        scope.requireOpen()
        counter.forwardAttempt(1, rows.toLong, materialization)
        original.patterns(DMat.dense(input.length, 1, Vector.tabulate(input.length)(input.apply))) match
          case Left(error) => throw IllegalStateException(error.message)
          case Right(result) =>
            counter.forwardReturned(result.rows.toLong, materialization)
            var row = 0
            while row < rows do
              output(row) = result(row, 0)
              row += 1
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
        scope.requireOpen()
        counter.adjointAttempt(1, cols.toLong)
        original.patterns.star(DMat.dense(input.length, 1, Vector.tabulate(input.length)(input.apply))) match
          case Left(error) => throw IllegalStateException(error.message)
          case Right(result) =>
            counter.adjointReturned(result.rows.toLong)
            var column = 0
            while column < cols do
              output(column) = result(column, 0)
              column += 1
    Observations.fromOperator(samples, neural, operator, original.patterns.valueIdentity, original.source, original.origins)
      .left.map(ObservationProductError.Construction.apply)

  private def guardedDense[SK, NK](
      samples: AxisRef[SK], neural: AxisRef[NK], original: Observations[samples.Id, neural.Id], values: DMat,
      scope: ProductScope, counter: ProductCounter
  ): Either[ObservationProductError, Observations[samples.Id, neural.Id]] =
    val operator = new DoubleLinearOperator:
      val rows = values.rows
      val cols = values.cols
      def applyTo(input: DVec, output: MutableDVec): Unit =
        scope.requireOpen()
        counter.forwardAttempt(1, rows.toLong, materialization = false)
        values.applyTo(input, output)
        counter.forwardReturned(rows.toLong, materialization = false)
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
        scope.requireOpen()
        counter.adjointAttempt(1, cols.toLong)
        values.transposeApplyTo(input, output)
        counter.adjointReturned(cols.toLong)
    Observations.fromOperator(samples, neural, operator, original.patterns.valueIdentity, original.source, original.origins)
      .left.map(ObservationProductError.Construction.apply)

  private def detail(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getName)
