package scalafim.fmri.ar

object Pacf:

  def pacfToAr(kappa: Vector[Double]): Vector[Double] =
    if kappa.isEmpty then Vector.empty
    else
      var previous = Array(kappa.head)
      var m = 2
      while m <= kappa.length do
        val km = kappa(m - 1)
        val current = new Array[Double](m)
        var j = 0
        while j < m - 1 do
          current(j) = previous(j) - km * previous((m - 2) - j)
          j += 1
        current(m - 1) = km
        previous = current
        m += 1
      previous.toVector

  def arToPacf(phi: Vector[Double], eps: Double = 1e-12): Vector[Double] =
    if phi.isEmpty then Vector.empty
    else
      var coefficients = phi.toArray
      val kappa = new Array[Double](phi.length)
      var m = phi.length
      while m >= 1 do
        val km = coefficients(m - 1)
        kappa(m - 1) = km
        if m > 1 then
          val den = math.max(1.0 - km * km, eps)
          val next = new Array[Double](m - 1)
          var j = 0
          while j < m - 1 do
            next(j) = (coefficients(j) + km * coefficients((m - 2) - j)) / den
            j += 1
          coefficients = next
        m -= 1
      kappa.toVector

  def enforceStationary(phi: Vector[Double], bound: Double = 0.99): Vector[Double] =
    require(bound > 0.0 && bound < 1.0 && bound.isFinite, "PACF bound must be finite and in (0, 1)")
    val clipped = arToPacf(phi).map(k => math.max(-bound, math.min(bound, k)))
    pacfToAr(clipped)
