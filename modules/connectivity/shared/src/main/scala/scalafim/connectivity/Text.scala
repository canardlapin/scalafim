package scalafim.connectivity

private[connectivity] object ConnectivityText:
  def double(value: Double): String =
    val raw = value.toString
    if raw.contains(".") || raw.contains("E") || raw.contains("e") ||
        raw == "NaN" || raw == "Infinity" || raw == "-Infinity"
    then raw
    else s"$raw.0"
