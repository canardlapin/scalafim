package scalafim.bids

private[bids] final case class ConfoundReductionResult(scores: BidsTable, pca: ConfoundPca)

private[bids] trait ConfoundReducer:
  def reduce(table: BidsTable, strategy: ConfoundStrategy): Either[BidsError, ConfoundReductionResult]

private[bids] object PrincipalComponentConfoundReducer extends ConfoundReducer:
  def reduce(table: BidsTable, strategy: ConfoundStrategy): Either[BidsError, ConfoundReductionResult] =
    ConfoundStrategy.validate(strategy).flatMap { strategy =>
      if table.columns.isEmpty then
        Left(BidsError.InvalidConfoundStrategy(strategy.name, "no PCA columns remain after cleaning"))
      else if table.nrows == 0 then
        Left(BidsError.InvalidConfoundStrategy(strategy.name, "PCA requires at least one row"))
      else
        val numeric = numericMatrix(table)
        val standardized = standardize(numeric)
        val covariance = covarianceMatrix(standardized)
        val eigen = SymmetricEigen.decompose(covariance)
        val components = retainedComponents(eigen.values, strategy.npcs, strategy.percentVariance)

        if components <= 0 then
          Left(BidsError.InvalidConfoundStrategy(strategy.name, "PCA retained zero components"))
        else
          val loadings = eigen.vectors.take(components)
          val scores = projectScores(standardized, loadings)
          val componentNames = (1 to components).map(i => s"PC$i").toVector
          val scoreTable =
            BidsTable.fromRows(
              componentNames,
              scores.map(row => row.map(value => Some(formatDouble(value))))
            )
          val totalVariance = eigen.values.filter(_ > 0.0).sum
          val variances = eigen.values.take(components)
          val proportions =
            if totalVariance <= 0.0 then Vector.fill(components)(0.0)
            else variances.map(value => value / totalVariance)
          val cumulative =
            proportions.scanLeft(0.0)(_ + _).tail
          scoreTable.map { scoresTable =>
            ConfoundReductionResult(
              scores = scoresTable,
              pca = ConfoundPca(
                sourceColumns = table.columns,
                componentNames = componentNames,
                variances = variances,
                proportionVariance = proportions,
                cumulativeProportion = cumulative,
                loadings = loadings
              )
            )
          }
    }

  private def numericMatrix(table: BidsTable): Vector[Vector[Double]] =
    val fills = table.columns.map(column => column -> median(finiteValues(table, column))).toMap
    table.rows.map { row =>
      row.zip(table.columns).map { case (cell, column) =>
        cell.flatMap(parseFiniteDouble).getOrElse(fills(column))
      }
    }

  private def standardize(matrix: Vector[Vector[Double]]): Vector[Vector[Double]] =
    if matrix.isEmpty || matrix.headOption.forall(_.isEmpty) then matrix
    else
      val rows = matrix.length
      val cols = matrix.head.length
      val means =
        Vector.tabulate(cols) { col =>
          matrix.map(_(col)).sum / rows
        }
      val scales =
        Vector.tabulate(cols) { col =>
          val centered = matrix.map(row => row(col) - means(col))
          val ss = centered.map(value => value * value).sum
          val sd =
            if rows <= 1 then 0.0
            else math.sqrt(ss / (rows - 1))
          if sd.isFinite && sd > 0.0 then sd else 1.0
        }

      matrix.map { row =>
        Vector.tabulate(cols)(col => (row(col) - means(col)) / scales(col))
      }

  private def covarianceMatrix(matrix: Vector[Vector[Double]]): Vector[Vector[Double]] =
    if matrix.isEmpty || matrix.headOption.forall(_.isEmpty) then Vector.empty
    else
      val rows = matrix.length
      val cols = matrix.head.length
      val denom = math.max(rows - 1, 1).toDouble
      Vector.tabulate(cols) { i =>
        Vector.tabulate(cols) { j =>
          var acc = 0.0
          var row = 0
          while row < rows do
            acc += matrix(row)(i) * matrix(row)(j)
            row += 1
          acc / denom
        }
      }

  private def retainedComponents(
      variances: Vector[Double],
      npcs: Option[Int],
      percentVariance: Option[Double]
  ): Int =
    val available = variances.length
    if available == 0 then 0
    else
      npcs.filter(_ > 0) match
        case Some(k) => math.min(k, available)
        case None =>
          percentVariance.filter(_ > 0.0) match
            case Some(percent) =>
              val target = math.min(percent, 100.0) / 100.0
              val total = variances.filter(_ > 0.0).sum
              if total <= 0.0 then 1
              else
                val cumulative = variances.scanLeft(0.0)(_ + _).tail.map(_ / total)
                cumulative.indexWhere(_ >= target) match
                  case -1    => available
                  case index => index + 1
            case None =>
              val positive = variances.count(_ > 1e-12)
              if positive > 0 then positive else 1

  private def projectScores(matrix: Vector[Vector[Double]], loadings: Vector[Vector[Double]]): Vector[Vector[Double]] =
    matrix.map { row =>
      loadings.map { loading =>
        row.zip(loading).map(_ * _).sum
      }
    }

  private def finiteValues(table: BidsTable, column: String): Vector[Double] =
    table.column(column).getOrElse(Vector.empty).flatMap(_.flatMap(parseFiniteDouble))

  private def parseFiniteDouble(value: String): Option[Double] =
    value.trim.toDoubleOption.filter(d => !d.isNaN && !d.isInfinity)

  private def median(values: Vector[Double]): Double =
    if values.isEmpty then 0.0
    else
      val sorted = values.sorted
      val mid = sorted.length / 2
      if sorted.length % 2 == 1 then sorted(mid)
      else (sorted(mid - 1) + sorted(mid)) / 2.0

  private def formatDouble(value: Double): String =
    val raw = f"$value%.12f"
    val stripped = raw.replaceAll("0+$", "").replaceAll("\\.$", "")
    if stripped.isEmpty || stripped == "-0" then "0" else stripped

  private object SymmetricEigen:
    final case class Result(values: Vector[Double], vectors: Vector[Vector[Double]])

    def decompose(matrix: Vector[Vector[Double]], tolerance: Double = 1e-12, maxSweeps: Int = 100): Result =
      val n = matrix.length
      if n == 0 then Result(Vector.empty, Vector.empty)
      else
        val a = matrix.map(_.toArray).toArray
        val vectors = Array.tabulate(n, n)((row, col) => if row == col then 1.0 else 0.0)

        var sweep = 0
        var changed = true
        while sweep < maxSweeps && changed do
          changed = false
          var p = 0
          while p < n - 1 do
            var q = p + 1
            while q < n do
              val apq = a(p)(q)
              if math.abs(apq) > tolerance then
                changed = true
                rotate(a, vectors, p, q)
              q += 1
            p += 1
          sweep += 1

        val pairs =
          (0 until n).toVector.map { component =>
            val value = if a(component)(component) < 0.0 && math.abs(a(component)(component)) <= tolerance then 0.0 else a(component)(component)
            val vector = orient(Vector.tabulate(n)(row => vectors(row)(component)))
            value -> vector
          }.sortBy { case (value, _) => -value }

        Result(
          values = pairs.map(_._1),
          vectors = pairs.map(_._2)
        )

    private def rotate(a: Array[Array[Double]], vectors: Array[Array[Double]], p: Int, q: Int): Unit =
      val app = a(p)(p)
      val aqq = a(q)(q)
      val apq = a(p)(q)
      val tau = (aqq - app) / (2.0 * apq)
      val sign = if tau < 0.0 then -1.0 else 1.0
      val t = sign / (math.abs(tau) + math.sqrt(1.0 + tau * tau))
      val c = 1.0 / math.sqrt(1.0 + t * t)
      val s = t * c
      val n = a.length

      var k = 0
      while k < n do
        if k != p && k != q then
          val akp = a(k)(p)
          val akq = a(k)(q)
          val nextKp = c * akp - s * akq
          val nextKq = s * akp + c * akq
          a(k)(p) = nextKp
          a(p)(k) = nextKp
          a(k)(q) = nextKq
          a(q)(k) = nextKq
        k += 1

      a(p)(p) = c * c * app - 2.0 * s * c * apq + s * s * aqq
      a(q)(q) = s * s * app + 2.0 * s * c * apq + c * c * aqq
      a(p)(q) = 0.0
      a(q)(p) = 0.0

      k = 0
      while k < n do
        val vkp = vectors(k)(p)
        val vkq = vectors(k)(q)
        vectors(k)(p) = c * vkp - s * vkq
        vectors(k)(q) = s * vkp + c * vkq
        k += 1

    private def orient(vector: Vector[Double]): Vector[Double] =
      val pivot =
        vector.indices.maxByOption(index => math.abs(vector(index))).getOrElse(0)
      if vector.isEmpty || vector(pivot) >= 0.0 then vector else vector.map(-_)
