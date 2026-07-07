package scalafim.fmri.ar

object Pacf:

  def pacfToAr(kappa: Vector[Double]): Vector[Double] =
    pacfToAr(PartialAutocorrelations.unsafe(kappa))

  def pacfToAr(kappa: PartialAutocorrelations): Vector[Double] =
    val values = kappa.values
    if values.isEmpty then Vector.empty
    else
      var previous = Array(values.head)
      var m = 2
      while m <= values.length do
        val km = values(m - 1)
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
    arToPartialAutocorrelations(phi, eps).toVector

  def arToPartialAutocorrelations(phi: Vector[Double], eps: Double = 1e-12): PartialAutocorrelations =
    PartialAutocorrelations.unsafe(
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
    )

  def enforceStationary(phi: Vector[Double], bound: Double = 0.99): Vector[Double] =
    enforceStationary(phi, StationarityBound.unsafe(bound))

  def enforceStationary(phi: Vector[Double], bound: StationarityBound): Vector[Double] =
    val clipped = arToPartialAutocorrelations(phi).clipped(bound)
    pacfToAr(clipped)
