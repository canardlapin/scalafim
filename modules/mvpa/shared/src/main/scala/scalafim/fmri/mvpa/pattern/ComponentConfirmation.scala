package scalafim.fmri.mvpa.pattern

import gale.backend.Backend.given
import gale.linalg.{DMat, QROptions, QRPivoting}
import multivar.core.SemanticSpace
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, EvidenceIdentity, MultiResponse, Observations}
import scalafim.fmri.mvpa.analysis.{EvidenceExposure, ExposureControl, ExposureScope}

enum ComponentConfirmationError:
  case Invalid(detail: String)
  case AxisMismatch(detail: String)
  case Exposure(detail: String)
  case Unavailable(detail: String)
  case Budget(requiredCells: BigInt, allowedCells: Long)
  case Evidence(detail: String)
  case NonEstimable(detail: String)
  case Numerical(detail: String)

enum ComponentCalibrationStatus:
  case PendingFrozenProtocol

/** Conservative owned numeric-cell admission, checked before source reads.
  * Borrowed evidence/projections/heads, provider and Gale scratch, object
  * overhead, and process RSS are outside this bound. */
final case class ComponentConfirmationBudget(maximumOwnedCells: Long = 10000000L):
  require(maximumOwnedCells >= 0L)

/** Complete named families and the target metric are frozen before reading
  * confirmation outcomes. Nuisance names declare the same column semantics in
  * training and confirmation; callers own the provenance of those declarations.
  * This does not authenticate the physical source of renamed foreign evidence. */
final class FrozenComponentConfirmation private (
    val design: ConfirmationDesign[?, ?], val associationMembers: Vector[String],
    val incrementalMembers: Vector[String], val nuisanceColumns: Vector[String],
    val targetMetric: Vector[Double], val identity: String
)
object FrozenComponentConfirmation:
  def freeze(design: ConfirmationDesign[?, ?], associationMembers: Vector[String], incrementalMembers: Vector[String],
      nuisanceColumns: Vector[String], exposure: EvidenceExposure): Either[ComponentConfirmationError, FrozenComponentConfirmation] =
    val r = design.discovery.brainProjection.matrix.cols
    val members = associationMembers ++ incrementalMembers
    if r <= 0 || associationMembers.size != r || incrementalMembers.size != r || members.exists(_.trim.isEmpty) || members.distinct.size != members.size then
      Left(ComponentConfirmationError.Invalid("one distinct association and incremental family member per frozen component"))
    else if members != design.contract.multiplicityFamily then Left(ComponentConfirmationError.Invalid("ordered complete family must equal the confirmation contract"))
    else if nuisanceColumns.size != design.nuisance.matrix.cols || nuisanceColumns.exists(_.trim.isEmpty) || nuisanceColumns.distinct.size != nuisanceColumns.size then
      Left(ComponentConfirmationError.Invalid("distinct names for every nuisance column"))
    else if exposure.reference.evidenceIdentity != design.discovery.identity || exposure.initialScope != ExposureScope.Training || ExposureControl.untouchedConfirmation(exposure, ExposureScope.Holdout).isLeft then
      Left(ComponentConfirmationError.Exposure("freeze requires discovery-bound training exposure without possible holdout access"))
    else design.discovery.artifact.target match
      case TargetGeometry.Categorical(_) => Left(ComponentConfirmationError.Unavailable("categorical loss and head fitting require a separately implemented procedure"))
      case TargetGeometry.Continuous(target) =>
        val metric = target.metricDiagonal.values
        val identity = AxisDigest.sha256Hex: writer =>
          writer.string("scalafim.component-confirmation.v1"); writer.string(design.identity); writer.string(exposure.identity.text)
          Vector(associationMembers, incrementalMembers, nuisanceColumns).foreach: values =>
            writer.intLE(values.size); values.foreach(writer.string)
          writer.intLE(metric.size); metric.foreach(value => writer.string(java.lang.Double.toHexString(value)))
          writer.string("squared-target-metric; equal-rows-within-unit; equal-units; complete-case-refusal; reduced-minus-full")
        Right(new FrozenComponentConfirmation(design, associationMembers, incrementalMembers, nuisanceColumns, metric, identity))

/** Ordinary nuisance-adjusted paired score correlations, with both projections
  * fixed in discovery. These are descriptive arithmetic, not calibrated tests. */
final class ComponentAssociationResult private[pattern] (
    val correlations: Vector[Double], val nuisanceRank: Int, val members: Vector[String],
    val componentAxis: AxisDescriptor, val confirmationRows: AxisDescriptor,
    val independentUnits: AxisDescriptor, val rowUnitOrdinals: Vector[Int],
    val planIdentity: String, val brainEvidenceIdentity: EvidenceIdentity, val targetEvidenceIdentity: EvidenceIdentity,
    val plannedOwnedCells: Long, private[pattern] val nuisanceWorkingDesign: DMat,
    private[pattern] val residualScores: DMat
):
  val calibrationStatus: ComponentCalibrationStatus = ComponentCalibrationStatus.PendingFrozenProtocol

/** Small OLS heads trained on discovery only. Every reduced head is separately
  * refitted with the named component omitted. Neither discovery optimization
  * nor head fitting runs during confirmation evaluation. */
final class ComponentPredictionHeads private[pattern] (
    val plan: FrozenComponentConfirmation, val full: DMat, val reduced: Vector[DMat],
    val trainingNuisanceColumns: Int, val interceptAdded: Boolean,
    val trainingBrainIdentity: EvidenceIdentity, val trainingTargetIdentity: EvidenceIdentity,
    val identity: String, val plannedOwnedCells: Long
)

/** Positive improvement means the full model has lower loss. Rows are averaged
  * within their declared independent unit, then units receive equal weight.
  * All declared units and every row are required; nothing is silently dropped.
  * No standard error, p-value, fold-as-unit, or population claim is supplied. */
final class ComponentIncrementalResult private[pattern] (
    val fullUnitLoss: Vector[Double], val reducedUnitLoss: DMat, val unitImprovements: DMat,
    val meanImprovements: Vector[Double], val members: Vector[String],
    val componentAxis: AxisDescriptor, val confirmationRows: AxisDescriptor,
    val independentUnits: AxisDescriptor, val rowUnitOrdinals: Vector[Int],
    val targetMetric: Vector[Double], val headIdentity: String,
    val brainEvidenceIdentity: EvidenceIdentity, val targetEvidenceIdentity: EvidenceIdentity,
    val plannedOwnedCells: Long, val planIdentity: String,
    private[pattern] val lossContrasts: DMat
):
  val calibrationStatus: ComponentCalibrationStatus = ComponentCalibrationStatus.PendingFrozenProtocol

object ComponentConfirmation:
  private val tolerance = 1e-12

  def association[S <: SemanticSpace, N <: SemanticSpace, Q <: SemanticSpace](
      plan: FrozenComponentConfirmation, observations: Observations[S, N], targets: MultiResponse[S, Q],
      budget: ComponentConfirmationBudget = ComponentConfirmationBudget()
  ): Either[ComponentConfirmationError, ComponentAssociationResult] =
    val design = plan.design; val n = observations.rows; val r = design.discovery.brainProjection.matrix.cols
    val cells = associationOwnedCells(plan, n)
    for
      _ <- endpoints(plan, observations, targets, training = false)
      _ <- admit(cells, budget)
      _ <- if design.errorLaw == ConfirmationErrorLaw.IndependentGaussian && design.confirmation.samples.rowUnitOrdinals.distinct.size == n && design.confirmation.samples.units.size == n then Right(())
        else Left(ComponentConfirmationError.Unavailable("paired association currently requires independent rows; dependent score laws need separate admission"))
      nuisance <- withIntercept(design.nuisance.matrix)
      _ <- if n > nuisance._1.cols + 1 then Right(()) else Left(ComponentConfirmationError.NonEstimable("insufficient residual rows for paired association"))
      y <- targets.targets(design.discovery.targetProjection.matrix).left.map(error => ComponentConfirmationError.Evidence(error.toString))
      _ <- finite(y, "target scores")
      x <- observations.patterns(design.discovery.brainProjection.matrix).left.map(error => ComponentConfirmationError.Evidence(error.toString))
      _ <- finite(x, "brain scores")
      joint = DMat.tabulate(n, 2 * r)((i, j) => if j < r then x(i, j) else y(i, j - r))
      fit <- leastSquares(nuisance._1, joint)
      residual = joint - nuisance._1 * fit
      correlations <-
        val out = Vector.newBuilder[Double]
        var k = 0; var error: Option[ComponentConfirmationError] = None
        while k < r && error.isEmpty do
          var nx = 0.0; var ny = 0.0; var rawX = 0.0; var rawY = 0.0; var i = 0
          while i < n do
            nx = math.hypot(nx, residual(i, k)); ny = math.hypot(ny, residual(i, r + k))
            rawX = math.hypot(rawX, x(i, k)); rawY = math.hypot(rawY, y(i, k)); i += 1
          if !nx.isFinite || !ny.isFinite || nx <= tolerance * rawX || ny <= tolerance * rawY then error = Some(ComponentConfirmationError.NonEstimable(s"aliased or nonfinite residual score norm at relative tolerance 1e-12 for component $k"))
          else
            var cross = 0.0; i = 0
            while i < n do
              cross += (residual(i, k) / nx) * (residual(i, r + k) / ny); i += 1
            if !cross.isFinite || math.abs(cross) > 1.0 + 1e-12 then error = Some(ComponentConfirmationError.Numerical("invalid residual correlation"))
            else out += math.max(-1.0, math.min(1.0, cross))
          k += 1
        error.toLeft(out.result())
    yield new ComponentAssociationResult(correlations, nuisance._1.cols, plan.associationMembers,
      design.discovery.brainProjection.output.descriptor, observations.sampleAxis, design.confirmation.samples.units.descriptor,
      design.confirmation.samples.rowUnitOrdinals, plan.identity, observations.identity, targets.identity, cells.toLong,
      nuisance._1, residual)

  def fitHeads[S <: SemanticSpace, N <: SemanticSpace, Q <: SemanticSpace](
      plan: FrozenComponentConfirmation, observations: Observations[S, N], targets: MultiResponse[S, Q],
      nuisance: ConfirmationNuisance[?], budget: ComponentConfirmationBudget = ComponentConfirmationBudget()
  ): Either[ComponentConfirmationError, ComponentPredictionHeads] =
    val n = observations.rows; val r = plan.design.discovery.brainProjection.matrix.cols; val q = targets.columns
    val m = BigInt(nuisance.matrix.cols) + 1 + r
    val cells = 24 * BigInt(n) * (m + q) + 24 * m * m + (BigInt(r) + 1) * m * q + BigInt(q) * q
    for
      _ <- endpoints(plan, observations, targets, training = true)
      _ <- if nuisance.rows.descriptor == observations.sampleAxis && nuisance.matrix.cols == plan.nuisanceColumns.size then Right(()) else Left(ComponentConfirmationError.AxisMismatch("training nuisance rows or declared columns"))
      _ <- admit(cells, budget)
      working <- withIntercept(nuisance.matrix)
      _ <- if n > working._1.cols.toLong + r then Right(()) else Left(ComponentConfirmationError.NonEstimable("training head requires positive residual degrees of freedom"))
      y <- targets.targets(DMat.eye(q)).left.map(error => ComponentConfirmationError.Evidence(error.toString))
      _ <- finite(y, "training targets")
      x <- observations.patterns(plan.design.discovery.brainProjection.matrix).left.map(error => ComponentConfirmationError.Evidence(error.toString))
      _ <- finite(x, "training brain scores")
      fullDesign = headDesign(working._1, x, None)
      full <- leastSquares(fullDesign, y)
      reduced <-
        val out = Vector.newBuilder[DMat]
        var k = 0; var error: Option[ComponentConfirmationError] = None
        while k < r && error.isEmpty do
          leastSquares(headDesign(working._1, x, Some(k)), y) match
            case Left(value) => error = Some(value)
            case Right(value) => out += value
          k += 1
        error.toLeft(out.result())
      identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.component-prediction-heads.v1"); writer.string(plan.identity)
        observations.identity.writeFramed(writer); targets.identity.writeFramed(writer)
        writer.string(working._2.toString)
        (Vector(working._1, full) ++ reduced).foreach: matrix =>
          writer.intLE(matrix.rows); writer.intLE(matrix.cols)
          var i = 0
          while i < matrix.rows do
            var j = 0
            while j < matrix.cols do
              writer.string(java.lang.Double.toHexString(matrix(i, j))); j += 1
            i += 1
    yield new ComponentPredictionHeads(plan, full, reduced, working._1.cols, working._2, observations.identity, targets.identity, identity, cells.toLong)

  def incremental[S <: SemanticSpace, N <: SemanticSpace, Q <: SemanticSpace](
      heads: ComponentPredictionHeads, observations: Observations[S, N], targets: MultiResponse[S, Q],
      budget: ComponentConfirmationBudget = ComponentConfirmationBudget()
  ): Either[ComponentConfirmationError, ComponentIncrementalResult] =
    val plan = heads.plan; val design = plan.design; val n = observations.rows; val r = heads.reduced.size; val q = targets.columns
    val units = design.confirmation.samples.units.size; val mapping = design.confirmation.samples.rowUnitOrdinals
    val cells = incrementalOwnedCells(heads, n, q)
    for
      _ <- endpoints(plan, observations, targets, training = false)
      _ <- admit(cells, budget)
      _ <- if units > 0 && mapping.distinct.size == units then Right(()) else Left(ComponentConfirmationError.Invalid("every declared independent unit must have confirmation rows"))
      nuisance = if heads.interceptAdded then DMat.tabulate(n, heads.trainingNuisanceColumns)((i, j) => if j == 0 then 1.0 else design.nuisance.matrix(i, j - 1)) else design.nuisance.matrix
      // The training column layout is retained exactly; confirmation never
      // chooses a new intercept or refits/repairs a head.
      y <- targets.targets(DMat.eye(q)).left.map(error => ComponentConfirmationError.Evidence(error.toString))
      _ <- finite(y, "confirmation targets; missing rows or units are refused")
      x <- observations.patterns(design.discovery.brainProjection.matrix).left.map(error => ComponentConfirmationError.Evidence(error.toString))
      _ <- finite(x, "confirmation brain scores; missing rows or units are refused")
      result <-
        val counts = new Array[Int](units)
        mapping.foreach(u => counts(u) += 1)
        val fullPrediction = headDesign(nuisance, x, None) * heads.full
        val contrasts = DMat.newBuilder(n * q, r)
        val differences = DMat.newBuilder(units, r)
        def losses(prediction: DMat): Either[ComponentConfirmationError, Vector[Double]] =
          val sums = new Array[Double](units)
          var i = 0
          while i < n do
            var j = 0; var loss = 0.0
            while j < q do
              val delta = prediction(i, j) - y(i, j)
              loss += plan.targetMetric(j) * delta * delta
              j += 1
            sums(mapping(i)) += loss / counts(mapping(i))
            i += 1
          if sums.exists(value => !value.isFinite) then Left(ComponentConfirmationError.Numerical("nonfinite held-out squared loss")) else Right(sums.toVector)
        for
          full <- losses(fullPrediction)
          reduced <-
            val out = DMat.newBuilder(units, r)
            var k = 0; var error: Option[ComponentConfirmationError] = None
            while k < r && error.isEmpty do
              val prediction = headDesign(nuisance, x, Some(k)) * heads.reduced(k)
              losses(prediction) match
                case Left(value) => error = Some(value)
                case Right(values) =>
                  var u = 0
                  while u < units do
                    out.update(u, k, values(u)); u += 1
                  val unitDifferences = new Array[Double](units)
                  var i = 0
                  while i < n do
                    var improvement = 0.0
                    var j = 0
                    while j < q do
                      // Squared-error Y² terms cancel between fixed heads.
                      // Each column is the affine contrast on row-major Y,
                      // with equal rows within a unit and equal unit weights.
                      val deltaPrediction = fullPrediction(i, j) - prediction(i, j)
                      val coefficient = plan.targetMetric(j) * (deltaPrediction * (2.0 / counts(mapping(i)) / units))
                      contrasts.update(i * q + j, k, coefficient)
                      // Algebraic reduced-minus-full difference; subtracting
                      // separately rounded squared losses loses small effects
                      // under a common large response offset.
                      improvement += deltaPrediction *
                        ((y(i, j) - prediction(i, j)) + (y(i, j) - fullPrediction(i, j))) * plan.targetMetric(j)
                      j += 1
                    unitDifferences(mapping(i)) += improvement / counts(mapping(i))
                    i += 1
                  u = 0
                  while u < units do
                    differences.update(u, k, unitDifferences(u)); u += 1
              k += 1
            error.toLeft(out.result())
          improvements = differences.result()
          means = Vector.tabulate(r): k =>
            var sum = 0.0; var u = 0
            while u < units do
              sum += improvements(u, k) / units; u += 1
            sum
          contrast = contrasts.result()
          _ <- if means.forall(_.isFinite) && ResidualCovariance.finite(improvements) && ResidualCovariance.finite(contrast) then Right(()) else Left(ComponentConfirmationError.Numerical("nonfinite loss improvement or affine contrast"))
        yield new ComponentIncrementalResult(full, reduced, improvements, means, plan.incrementalMembers,
          design.discovery.brainProjection.output.descriptor, observations.sampleAxis, design.confirmation.samples.units.descriptor,
          mapping, plan.targetMetric, heads.identity, observations.identity, targets.identity, cells.toLong, plan.identity, contrast)
    yield result

  private def endpoints[S <: SemanticSpace, N <: SemanticSpace, Q <: SemanticSpace](plan: FrozenComponentConfirmation,
      observations: Observations[S, N], targets: MultiResponse[S, Q], training: Boolean): Either[ComponentConfirmationError, Unit] =
    val discovery = plan.design.discovery
    val rows = if training then discovery.samples.rows.descriptor else plan.design.confirmation.samples.rows.descriptor
    if observations.sampleAxis != rows || targets.sampleAxis != rows then Left(ComponentConfirmationError.AxisMismatch("actual training/confirmation rows"))
    else if observations.neuralAxis != discovery.brainProjection.input.descriptor || targets.featureAxis != discovery.targetProjection.input.descriptor then Left(ComponentConfirmationError.AxisMismatch("frozen brain/target endpoint"))
    else Right(())

  private[pattern] def associationOwnedCells(plan: FrozenComponentConfirmation, rows: Int): BigInt =
    val z = BigInt(plan.design.nuisance.matrix.cols) + 1
    val r = BigInt(plan.associationMembers.size)
    24 * BigInt(rows) * (z + 2 * r) + 24 * z * z

  /** The owned-cell count `incremental` admits against its budget, exposed so
    * instrumented adapters can refuse before an exposure-recorded read. */
  def incrementalOwnedCells(heads: ComponentPredictionHeads, rows: Int, targets: Int): BigInt =
    val r = BigInt(heads.reduced.size)
    val units = BigInt(heads.plan.design.confirmation.samples.units.size)
    val m = BigInt(heads.trainingNuisanceColumns) + r
    // Includes the retained improvement builder and its per-unit accumulator.
    12 * BigInt(rows) * (m + targets) + 7 * units * (r + 1) + BigInt(targets) * targets + BigInt(rows) * targets * r

  private def admit(cells: BigInt, budget: ComponentConfirmationBudget): Either[ComponentConfirmationError, Unit] =
    // Also bounds every individual dense array, conservatively.
    if cells > budget.maximumOwnedCells || cells > Int.MaxValue then Left(ComponentConfirmationError.Budget(cells, budget.maximumOwnedCells)) else Right(())

  private def finite(matrix: DMat, name: String): Either[ComponentConfirmationError, Unit] =
    if ResidualCovariance.finite(matrix) then Right(()) else Left(ComponentConfirmationError.Evidence(s"nonfinite $name"))

  private def withIntercept(nuisance: DMat): Either[ComponentConfirmationError, (DMat, Boolean)] =
    val augmented = DMat.tabulate(nuisance.rows, nuisance.cols + 1)((i, j) => if j == 0 then 1.0 else nuisance(i, j - 1))
    val rank = augmented.qr(QROptions(QRPivoting.Column, Some(tolerance))).diagnostics.rank
    if rank.contains(nuisance.cols) then Right((nuisance, false))
    else if rank.contains(augmented.cols) then Right((augmented, true))
    else Left(ComponentConfirmationError.NonEstimable("nuisance/intercept rank unavailable"))

  private def headDesign(nuisance: DMat, scores: DMat, omitted: Option[Int]): DMat =
    val z = nuisance.cols
    DMat.tabulate(scores.rows, z + scores.cols - omitted.size): (i, j) =>
      if j < z then nuisance(i, j)
      else
        val k = j - z
        scores(i, if omitted.exists(_ <= k) then k + 1 else k)

  private def leastSquares(design: DMat, response: DMat): Either[ComponentConfirmationError, DMat] =
    val qr = design.qr(QROptions(QRPivoting.Column, Some(tolerance)))
    if !qr.diagnostics.rank.contains(design.cols) then Left(ComponentConfirmationError.NonEstimable("aliased nuisance/component head at Gale QR tolerance 1e-12"))
    else qr.solveLeastSquares(response).left.map(error => ComponentConfirmationError.Numerical(error.toString)).flatMap: value =>
      finite(value, "fitted coefficients").map(_ => value)
