package scalafim.fmri.laws.profile

import gale.linalg.{DMat, DVec, LinAlgError}
import gale.spectral.{MatrixEnclosure, RealInterval, ValidatedLinearBound, ValidatedLinearSystem}
import scalafim.fmri.ar.{InitialConditionPolicy, WhiteningTransform}
import scalafim.fmri.hrf.family.Cascade34Family

/** Original continuous Cascade34 impulse court: exact declared binary times, chart coordinates, response and nuisance
  * inputs; analytic impulse convolution, full observation support, exact AR(1) first-row scaling and lambda=1
  * condition-centered penalty. No grid quadrature or tail is omitted. This deliberately materialises dense interval
  * normal equations and is NOT an admitted production certificate or a general drive/whitening adapter.
  */
object TrialObservationEnclosure:
  private def checked[A](value: Either[LinAlgError, A]): A = value.fold(throw _, identity)
  private def point(value: Double): RealInterval = checked(RealInterval.exact(value))
  private def add(a: RealInterval, b: RealInterval): RealInterval = checked(a.add(b))
  private def sub(a: RealInterval, b: RealInterval): RealInterval = checked(a.subtract(b))
  private def mul(a: RealInterval, b: RealInterval): RealInterval = checked(a.multiply(b))
  private def div(a: RealInterval, b: RealInterval): RealInterval = checked(a.divide(b))
  private def exp(a: RealInterval): RealInterval = checked(a.exp)
  private def square(a: RealInterval): RealInterval = checked(a.square)
  private def magnitude(a: RealInterval): Double = math.max(math.abs(a.lower), math.abs(a.upper))
  private val zero = point(0.0)
  private val one = point(1.0)

  final case class Prepared(
      normal: MatrixEnclosure,
      rhs: MatrixEnclosure,
      design: MatrixEnclosure,
      retainedScalarValues: Long,
      normalProductTerms: Long,
      buildSeconds: Double
  ):
    def certify(
        candidate: Vector[Double],
        referenceInverse: DMat,
        query: Option[Vector[Double]] = None
    ): Either[LinAlgError, ValidatedLinearBound] =
      ValidatedLinearSystem.bound(normal, rhs, DVec.fromSeq(candidate), referenceInverse, query.map(DVec.fromSeq))

  /** Shape intervals can cover a box. A successful inverse-defect check then applies uniformly to its enclosed original
    * equations; response-specific coefficient accuracy is certified only for the supplied candidate.
    */
  def prepare(
      f: DecodedTrialCheckpoint.Fixture,
      lower: Vector[Double],
      upper: Vector[Double],
      raw: Array[Double]
  ): Prepared =
    val start = System.nanoTime()
    require(f.plan.basis.family.isInstanceOf[Cascade34Family] && lower.length == 3 && upper.length == 3)
    require(
      raw.length == f.rows && f.whitening.segments.length == 1 &&
        f.whitening.initialCondition == InitialConditionPolicy.ExactAr1
    )
    require(
      f.whitening.arOrder == 1 && f.whitening.maOrder == 0 && f.config.criterion ==
        scalafim.fmri.model.ProfileCriterion.PenalizedProfile(1.0)
    )
    val family = f.plan.basis.family
    family.chart.point(lower).fold(e => throw new IllegalArgumentException(e.message), identity)
    family.chart.point(upper).fold(e => throw new IllegalArgumentException(e.message), identity)
    val coordinates = lower.zip(upper).map((a, b) => checked(RealInterval.checked(a, b)))
    val positive = exp(coordinates(0))
    val logistic = div(one, add(one, exp(sub(zero, coordinates(1)))))
    val under = mul(positive, logistic)
    val rho = coordinates(2)
    val onsets = TrialNeighborhoodAudit.onsets(f)
    val times = f.dataset.samplingFrame.samples().map(_.value)
    val phi = point(f.whitening.coefficients.head.phi.head)
    val firstScale = checked(sub(one, square(phi)).sqrt)
    val n = f.trials + f.nuisance
    def component(rate: RealInterval, lag: RealInterval, order: Int): RealInterval =
      val x = mul(rate, lag)
      val polynomial = if order == 3 then div(square(x), point(2.0)) else div(mul(square(x), x), point(6.0))
      mul(rate, mul(polynomial, exp(sub(zero, x))))
    val unwhitened = Array.tabulate(f.rows * n): index =>
      val row = index / n
      val col = index % n
      if col >= f.trials then point(f.baseline.designMatrix(row, col - f.trials))
      else
        val lag = sub(point(times(row)), point(onsets(col)))
        if lag.upper <= 0.0 then zero
        else
          val causal = checked(RealInterval.checked(math.max(0.0, lag.lower), lag.upper))
          sub(component(positive, causal, 3), mul(rho, component(under, causal, 4)))
    val design = Array.tabulate(f.rows * n): index =>
      if index < n then mul(firstScale, unwhitened(index))
      else sub(unwhitened(index), mul(phi, unwhitened(index - n)))
    val response = Array.tabulate(f.rows): row =>
      if row == 0 then mul(firstScale, point(raw(row)))
      else sub(point(raw(row)), mul(phi, point(raw(row - 1))))
    val normal = new Array[RealInterval](n * n)
    var i = 0
    while i < n do
      var j = 0
      while j <= i do
        var value = zero
        var row = 0
        while row < f.rows do
          value = add(value, mul(design(row * n + i), design(row * n + j)))
          row += 1
        if i < f.trials && j < f.trials then
          val same = f.expanded.membership.conditionOfTrial(i) == f.expanded.membership.conditionOfTrial(j)
          val mean = if same then
            div(one, point(f.expanded.membership.trialsOf(f.expanded.membership.conditionOfTrial(i)).length.toDouble))
          else zero
          value = add(value, sub(if i == j then one else zero, mean))
        normal(i * n + j) = value
        normal(j * n + i) = value
        j += 1
      i += 1
    val rhs = Array.tabulate(n): col =>
      var value = zero
      var row = 0
      while row < f.rows do
        value = add(value, mul(design(row * n + col), response(row)))
        row += 1
      value
    def enclosure(values: Array[RealInterval], rows: Int, cols: Int): MatrixEnclosure =
      checked(
        MatrixEnclosure.checked(
          DMat.tabulate(rows, cols)((i, j) => values(i * cols + j).lower),
          DMat.tabulate(rows, cols)((i, j) => values(i * cols + j).upper)
        )
      )
    val g = enclosure(normal, n, n)
    Prepared(
      g,
      enclosure(rhs, n, 1),
      enclosure(design, f.rows, n),
      2L * f.rows * n + 2L * n * n + 2L * n,
      f.rows.toLong * (n.toLong * (n + 1) / 2 + n),
      (System.nanoTime() - start) / 1e9
    )

  /** Dense diagnostic candidate for the SAME prepared normal matrix used by the band's reference inverse. Construct
    * once at the reference, never at the requested off-node point. Gale owns all factorization/solve work.
    */
  def referenceInverse(f: DecodedTrialCheckpoint.Fixture, reference: Vector[Double]): DMat =
    val basis = DecodedTrialCheckpoint.designAt(f.expanded, reference)
    val n = f.trials + f.nuisance
    val design = DMat.tabulate(f.rows, n)((i, j) =>
      if j < f.trials then basis(i * f.trials + j)
      else f.baseline.designMatrix(i, j - f.trials)
    )
    val whitened =
      WhiteningTransform.matrix(f.whitening, design).fold(e => throw new IllegalArgumentException(e.toString), identity)
    val penalty = DMat.tabulate(n, n): (i, j) =>
      if i >= f.trials || j >= f.trials then 0.0
      else
        val condition = f.expanded.membership.conditionOfTrial(i)
        (if i == j then 1.0 else 0.0) - (if condition == f.expanded.membership.conditionOfTrial(j) then
                                           1.0 / f.expanded.membership.trialsOf(condition).length
                                         else 0.0)
    (whitened.t * whitened + penalty).solve(DMat.eye(n)).fold(throw _, identity)

  def queryErrorUpper(
      candidate: Vector[Double],
      queryValue: Double,
      weights: Vector[Double],
      bounds: ValidatedLinearBound
  ): Double =
    var contraction = zero
    var error = zero
    weights.indices.foreach: i =>
      contraction = add(contraction, mul(point(weights(i)), point(candidate(i))))
      error = add(error, mul(point(math.abs(weights(i))), point(bounds.componentAbsoluteUpper(i))))
    val scientific = bounds.queryAbsoluteUpper.fold(error)(point)
    add(scientific, point(magnitude(sub(contraction, point(queryValue))))).upper

  def trialRelativeErrorUpper(candidate: Vector[Double], trials: Int, bounds: ValidatedLinearBound): Double =
    var error = zero
    var norm = zero
    var i = 0
    while i < trials do
      error = add(error, square(point(bounds.componentAbsoluteUpper(i))))
      norm = add(norm, square(point(candidate(i))))
      i += 1
    val e = checked(checked(RealInterval.checked(0.0, error.upper)).sqrt)
    val a = checked(checked(RealInterval.checked(math.max(0.0, norm.lower), norm.upper)).sqrt)
    val denominator = sub(a, e)
    if denominator.lower <= 0.0 then Double.PositiveInfinity else div(e, denominator).upper
