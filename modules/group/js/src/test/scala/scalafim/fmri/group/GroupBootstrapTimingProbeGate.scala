package scalafim.fmri.group

import scala.scalajs.js

/** Scala.js switch for `GroupBootstrapTimingProbe`. Off unless Node inherits
  * SCALAFIM_GROUP_TIMING_PROBE=true from the environment that launched sbt.
  * Defaults are smaller than the JVM ones: the JS figure is a rough one.
  */
object GroupBootstrapTimingProbeGate:
  val platform = "js"
  val howToEnable = "timing probe skipped; set SCALAFIM_GROUP_TIMING_PROBE=true in the sbt environment to run it"

  private def env(name: String): Option[String] =
    if js.typeOf(js.Dynamic.global.process) == "undefined" then None
    else
      val found = js.Dynamic.global.process.env.selectDynamic(name)
      if js.isUndefined(found) then None else Some(found.asInstanceOf[String])

  def enabled: Boolean = env("SCALAFIM_GROUP_TIMING_PROBE").contains("true")

  def warmupFits: Int = env("SCALAFIM_GROUP_TIMING_PROBE_WARMUP").map(_.toInt).getOrElse(1000)

  def timedFits: Int = env("SCALAFIM_GROUP_TIMING_PROBE_TIMED").map(_.toInt).getOrElse(5000)
