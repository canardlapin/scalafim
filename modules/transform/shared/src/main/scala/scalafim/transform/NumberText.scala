package scalafim.transform

/** Decimal text for transform files: integers without a fraction, otherwise the shortest text that parses back to
  * exactly the same double.
  */
private[transform] object NumberText:
  def format(value: Double): String =
    if value == 0.0 && 1.0 / value < 0.0 then "-0"
    else if value == math.rint(value) && math.abs(value) < 1e15 then value.toLong.toString
    else value.toString
