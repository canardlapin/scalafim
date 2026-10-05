package scalafim.fmri.fit

import gale.linalg.{DMat, Vec, DVec}

/** Exact IEEE order statistics for nonnegative values, including +infinity and
  * canonical NaN. Eight replay passes retain two 256-bin histograms per row,
  * independent of the spatial population. Both central ranks are selected
  * before applying the dense median's (lower + upper) / 2 rounding rule.
  */
private[fit] object ExactRowMedians:
  def apply(
      rows: Int,
      population: Int,
      replay: (DMat => Unit) => Either[FitError, Unit]
  ): Either[FitError, DVec] =
    require(rows > 0 && population > 0, "median dimensions must be positive")
    val prefixes = Array.fill(rows * 2)(0L)
    val ranks = Array.tabulate(rows * 2)(index => if index % 2 == 0 then (population - 1) / 2 else population / 2)
    val counts = new Array[Int](rows * 2 * 256)
    var pass = 0
    while pass < 8 do
      java.util.Arrays.fill(counts, 0)
      val shift = 56 - pass * 8
      var emitted = 0L
      replay { values =>
        require(values.rows == rows, "median replay rows must match")
        emitted += values.cols.toLong
        var row = 0
        while row < rows do
          var col = 0
          while col < values.cols do
            val value = values(row, col)
            require(value >= 0.0 || value.isNaN, "row median values must be nonnegative")
            val rawBits = java.lang.Double.doubleToLongBits(value)
            val bits = if rawBits < 0L then ~rawBits else rawBits ^ Long.MinValue
            val high = if pass == 0 then 0L else bits >>> (shift + 8)
            var middle = 0
            while middle < 2 do
              val index = row * 2 + middle
              if high == prefixes(index) then
                counts(index * 256 + ((bits >>> shift) & 255L).toInt) += 1
              middle += 1
            col += 1
          row += 1
      } match
        case Left(error) => return Left(error)
        case Right(_) => ()
      if emitted != population.toLong then
        return Left(FitError.PreparationReplayMismatch("median replay population changed"))
      var index = 0
      while index < prefixes.length do
        var bin = 0
        while bin < 256 && ranks(index) >= counts(index * 256 + bin) do
          ranks(index) -= counts(index * 256 + bin)
          bin += 1
        if bin == 256 then return Left(FitError.PreparationReplayMismatch("median replay population changed"))
        prefixes(index) = (prefixes(index) << 8) | bin.toLong
        index += 1
      pass += 1
    Right(Vec.tabulate(rows) { row =>
      def decode(bits: Long): Double =
        java.lang.Double.longBitsToDouble(if bits < 0L then bits ^ Long.MinValue else ~bits)
      val lower = decode(prefixes(row * 2))
      val upper = decode(prefixes(row * 2 + 1))
      if population % 2 == 1 then upper else (lower + upper) / 2.0
    })
