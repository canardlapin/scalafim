package scalafim.fmri.group

/** JVM switch for `GroupBootstrapTimingProbe`. Off unless the sbt JVM is
  * started with -Dscalafim.group.timingProbe=true (tests are not forked).
  */
object GroupBootstrapTimingProbeGate:
  val platform = "jvm"
  val howToEnable = "timing probe skipped; pass -Dscalafim.group.timingProbe=true to run it"

  def enabled: Boolean = sys.props.get("scalafim.group.timingProbe").contains("true")

  def warmupFits: Int = sys.props.get("scalafim.group.timingProbe.warmup").map(_.toInt).getOrElse(2000)

  def timedFits: Int = sys.props.get("scalafim.group.timingProbe.timed").map(_.toInt).getOrElse(20000)
