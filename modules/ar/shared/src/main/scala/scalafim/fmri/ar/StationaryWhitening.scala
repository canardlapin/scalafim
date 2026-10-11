package scalafim.fmri.ar

import gale.linalg.{BandedCholesky, CholeskyOptions, DMat, Matrix, Vec}

/** Scientific covariance assembly for Ansley's stationary ARMA transform.
  * Gale owns the linear and banded triangular solves. No dense time-by-time
  * covariance or factor is constructed; the transformed covariance is banded.
  */
private[ar] object StationaryWhitening:
  def apply(plan: WhiteningPlan, design: DMat, response: DMat): Either[ArError, WhitenedMatrices] =
    for
      _ <- plan.coveredSegments.validateRows(design.rows)
      _ <- plan.coveredSegments.validateRows(response.rows)
      factors <- prepare(plan)
      x <- transform(plan, design, factors, transpose = false)
      y <- transform(plan, response, factors, transpose = false)
    yield WhitenedMatrices(x, y)

  def matrix(plan: WhiteningPlan, input: DMat, transpose: Boolean): Either[ArError, DMat] =
    for
      _ <- plan.coveredSegments.validateRows(input.rows)
      factors <- prepare(plan)
      result <- transform(plan, input, factors, transpose)
    yield result

  private def failure(detail: String): ArError = ArError.StationaryWhiteningFailed(detail)

  private def prepare(plan: WhiteningPlan): Either[ArError, Vector[BandedCholesky]] =
    val cache = scala.collection.mutable.Map.empty[(ArmaCoefficients, Int), Either[ArError, BandedCholesky]]
    val factors = Vector.newBuilder[BandedCholesky]
    var index = 0
    while index < plan.segments.length do
      val segment = plan.segments(index)
      val coefficients = plan.coefficientsFor(segment)
      cache.getOrElseUpdate((coefficients, segment.length), factor(coefficients, segment.length)) match
        case Left(error) => return Left(error)
        case Right(value) => factors += value
      index += 1
    Right(factors.result())

  /** Unit-innovation stationary ACVF from the ARMA covariance equations. */
  private def covariance(coefficients: ArmaCoefficients, maxLag: Int): Either[ArError, Vector[Double]] =
    val phi = coefficients.phi
    val theta = coefficients.theta
    val p = phi.length
    val q = theta.length
    val psi = new Array[Double](q + 1)
    psi(0) = 1.0
    var lag = 1
    while lag <= q do
      var total = theta(lag - 1)
      var k = 1
      while k <= math.min(lag, p) do
        total += phi(k - 1) * psi(lag - k)
        k += 1
      psi(lag) = total
      lag += 1
    def rhs(lag: Int): Double =
      var total = 0.0
      var j = lag
      while j <= q do
        total += (if j == 0 then 1.0 else theta(j - 1)) * psi(j - lag)
        j += 1
      total
    val system = Matrix.newBuilder(p + 1, p + 1)
    lag = 0
    while lag <= p do
      system(lag, lag) = 1.0
      var k = 1
      while k <= p do
        val col = math.abs(lag - k)
        system(lag, col) = system(lag, col) - phi(k - 1)
        k += 1
      lag += 1
    system.result().solve(Vec.tabulate(p + 1)(rhs)).left.map(error => failure(error.getMessage)).flatMap: initial =>
      val gamma = new Array[Double](math.max(maxLag, p) + 1)
      var lag = 0
      while lag <= p do
        gamma(lag) = initial(lag)
        lag += 1
      while lag < gamma.length do
        var total = rhs(lag)
        var k = 1
        while k <= p do
          total += phi(k - 1) * gamma(lag - k)
          k += 1
        gamma(lag) = total
        lag += 1
      if gamma.head <= 0.0 || !gamma.forall(_.isFinite) then Left(failure("non-positive or nonfinite stationary covariance"))
      else Right(gamma.take(maxLag + 1).toVector)

  private def factor(coefficients: ArmaCoefficients, length: Int): Either[ArError, BandedCholesky] =
    val p = coefficients.arOrder
    val q = coefficients.maOrder
    val m = math.max(p, q)
    covariance(coefficients, m + p).flatMap: gamma =>
      val bandwidth = math.min(m, length - 1)
      val bands = Matrix.tabulate(length, bandwidth + 1): (row, offset) =>
        val col = row - offset
        if col < 0 then 0.0
        else if row < m then gamma(offset)
        else if col < m then
          var total = gamma(offset)
          var k = 1
          while k <= p do
            total -= coefficients.phi(k - 1) * gamma(math.abs(offset - k))
            k += 1
          total
        else
          var total = 0.0
          var j = 0
          while j + offset <= q do
            val left = if j == 0 then 1.0 else coefficients.theta(j - 1)
            val right = if j + offset == 0 then 1.0 else coefficients.theta(j + offset - 1)
            total += left * right
            j += 1
          total
      BandedCholesky.factorLower(bands, CholeskyOptions(0.0)).left.map(error => failure(error.getMessage))

  private def transform(
      plan: WhiteningPlan,
      input: DMat,
      factors: Vector[BandedCholesky],
      transpose: Boolean
  ): Either[ArError, DMat] =
    val out = Matrix.newBuilder(input.rows, input.cols)
    var index = 0
    while index < plan.segments.length do
      val segment = plan.segments(index)
      val coefficients = plan.coefficientsFor(segment)
      val m = math.max(coefficients.arOrder, coefficients.maOrder)
      val local = Matrix.tabulate(segment.length, input.cols): (row, col) =>
        var total = input(segment.start + row, col)
        if !transpose && row >= m then
          var k = 1
          while k <= coefficients.arOrder do
            total -= coefficients.phi(k - 1) * input(segment.start + row - k, col)
            k += 1
        total
      val solved = if transpose then factors(index).solveLowerTranspose(local) else factors(index).solveLower(local)
      solved match
        case Left(error) => return Left(failure(error.getMessage))
        case Right(values) =>
          var row = 0
          while row < segment.length do
            var col = 0
            while col < input.cols do
              out(segment.start + row, col) = values(row, col)
              col += 1
            row += 1
          if transpose then
            row = m
            while row < segment.length do
              var k = 1
              while k <= coefficients.arOrder do
                var col = 0
                while col < input.cols do
                  val target = segment.start + row - k
                  out(target, col) = out(target, col) - coefficients.phi(k - 1) * values(row, col)
                  col += 1
                k += 1
              row += 1
      index += 1
    Right(out.result())
