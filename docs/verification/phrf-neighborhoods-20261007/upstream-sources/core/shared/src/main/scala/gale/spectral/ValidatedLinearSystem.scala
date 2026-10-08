package gale.spectral

import gale.linalg.{DMat, DVec, LinAlgError}

/** Error enclosure for every square system G x = b in the supplied intervals.
  * The supplied inverse is only a finite candidate. eta < 1 proves both it
  * and every G nonsingular. No trusted factorization or symmetry is assumed.
  */
final case class ValidatedLinearBound(
    contractionInfinityUpper: Double,
    errorInfinityUpper: Double,
    componentAbsoluteUpper: DVec,
    queryAbsoluteUpper: Option[Double] = None
):
  require(contractionInfinityUpper.isFinite && contractionInfinityUpper >= 0.0 && contractionInfinityUpper < 1.0,
    "a validated contraction must be finite and below one")
  require(errorInfinityUpper.isFinite && errorInfinityUpper >= 0.0, "error bound must be finite and nonnegative")
  require(componentAbsoluteUpper != null && componentAbsoluteUpper.length > 0 &&
    (0 until componentAbsoluteUpper.length).forall(i => componentAbsoluteUpper(i).isFinite && componentAbsoluteUpper(i) >= 0.0),
    "component bounds must be finite and nonnegative")
  require(queryAbsoluteUpper.forall(x => x.isFinite && x >= 0.0), "query bound must be finite and nonnegative")

object ValidatedLinearSystem:
  /** With M the candidate inverse and a the candidate solution, e satisfies
    * e = M(b-Ga) + (I-MG)e. Outward arithmetic encloses both terms;
    * ||e||inf <= ||M(b-Ga)||inf/(1-||I-MG||inf). Component bounds reuse this
    * scalar bound with each individual defect row. A requested signed query
    * uses |q'M(b-Ga)| + ||q'(I-MG)||1 ||e||inf, preserving cancellation.
    * Work is O(n^3), storage
    * O(n^2); this is a validation primitive, not an uncharged fast solve.
    */
  def bound(input: MatrixEnclosure, rhs: MatrixEnclosure, candidate: DVec, inverse: DMat,
      query: Option[DVec] = None)
      : Either[LinAlgError, ValidatedLinearBound] =
    if input == null || rhs == null || candidate == null || inverse == null then
      return Left(LinAlgError.InvalidArgument("validated system inputs must be present"))
    val n = input.lower.rows
    if n == 0 || input.lower.cols != n || rhs.lower.rows != n || rhs.lower.cols != 1 ||
        candidate.length != n || inverse.rows != n || inverse.cols != n then
      return Left(LinAlgError.InvalidArgument("validated system shapes must be n*n, n*1, n and n*n"))
    if (0 until n).exists(i => !candidate(i).isFinite || (0 until n).exists(j => !inverse(i, j).isFinite)) then
      return Left(LinAlgError.InvalidArgument("validated system candidates must be finite"))
    if query.exists(q => q == null || q.length != n || (0 until n).exists(i => !q(i).isFinite)) then
      return Left(LinAlgError.InvalidArgument("query weights must be finite and match the system"))
    // Keep arithmetic failures in Either while using allocation-bounded loops.
    def point(x: Double) = RealInterval.exact(x)
    def entry(i: Int, j: Int) = RealInterval.checked(input.lower(i, j), input.upper(i, j))
    def absolute(x: RealInterval): Double = math.max(math.abs(x.lower), math.abs(x.upper))
    val residual = new Array[RealInterval](n)
    val defectRows = new Array[Double](n)
    val transformed = new Array[Double](n)
    val queryDefect = Array.fill(n)(point(0.0).toOption.get)
    var queryResidual = point(0.0)
    var i = 0
    while i < n do
      var r = RealInterval.checked(rhs.lower(i, 0), rhs.upper(i, 0))
      var j = 0
      while j < n do
        r = for a <- r; g <- entry(i, j); x <- point(candidate(j)); p <- g.multiply(x); b <- a.subtract(p) yield b
        j += 1
      r match
        case Left(error) => return Left(error)
        case Right(value) => residual(i) = value
      i += 1
    i = 0
    while i < n do
      var z = point(0.0)
      var row = point(0.0)
      var j = 0
      while j < n do
        z = for a <- z; m <- point(inverse(i, j)); p <- m.multiply(residual(j)); b <- a.add(p) yield b
        var defect = point(if i == j then 1.0 else 0.0)
        var k = 0
        while k < n do
          defect = for a <- defect; m <- point(inverse(i, k)); g <- entry(k, j); p <- m.multiply(g); b <- a.subtract(p) yield b
          k += 1
        query match
          case Some(weights) =>
            val term = for a <- defect; w <- point(weights(i)); b <- a.multiply(w); c <- queryDefect(j).add(b) yield c
            term match
              case Left(error) => return Left(error)
              case Right(value) => queryDefect(j) = value
          case None => ()
        row = for a <- row; d <- defect; p <- point(absolute(d)); b <- a.add(p) yield b
        j += 1
      (z, row) match
        case (Left(error), _) => return Left(error)
        case (_, Left(error)) => return Left(error)
        case (Right(value), Right(sum)) =>
          transformed(i) = absolute(value)
          defectRows(i) = sum.upper
          query.foreach: weights =>
            queryResidual = for a <- queryResidual; w <- point(weights(i)); b <- w.multiply(value); c <- a.add(b) yield c
      i += 1
    val eta = defectRows.max
    if eta >= 1.0 then Left(LinAlgError.InvalidArgument(s"reference inverse defect $eta must be below one"))
    else
      for
        one <- point(1.0)
        etaInterval <- point(eta)
        denominator <- one.subtract(etaInterval)
        numerator <- point(transformed.max)
        error <- numerator.divide(denominator)
        components <-
          val out = new Array[Double](n)
          var failure: Option[LinAlgError] = None
          i = 0
          while i < n && failure.isEmpty do
            val value = for a <- point(defectRows(i)); b <- a.multiply(error); c <- point(transformed(i)); d <- c.add(b) yield d.upper
            value match
              case Left(e) => failure = Some(e)
              case Right(v) => out(i) = v
            i += 1
          failure.toLeft(DVec.fromArray(out))
        queryBound <- query match
          case None => Right(None)
          case Some(_) =>
            var norm = point(0.0)
            var j = 0
            while j < n do
              norm = for a <- norm; b <- point(absolute(queryDefect(j))); c <- a.add(b) yield c
              j += 1
            for z <- queryResidual; a <- point(absolute(z)); d <- norm; b <- d.multiply(error); c <- a.add(b) yield Some(c.upper)
      yield ValidatedLinearBound(eta, error.upper, components, queryBound)
