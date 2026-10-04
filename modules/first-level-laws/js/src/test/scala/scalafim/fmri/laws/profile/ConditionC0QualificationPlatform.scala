package scalafim.fmri.laws.profile

import scala.scalajs.js

/** Node runs its own workload; portable process/engine heap measurements are unavailable. */
object ConditionC0QualificationPlatform:
  val name = "JS"
  def arguments(args: Array[String]): Array[String] =
    if args.nonEmpty then args
    else
      val argv = js.Dynamic.global.process.argv.asInstanceOf[js.Array[String]]
      Array.tabulate(math.max(0, argv.length - 2))(i => argv(i + 2))
  def executionSourceId: Option[String] =
    val value = js.Dynamic.global.process.env.selectDynamic("C0_SOURCE_ID")
    if js.isUndefined(value) then None else Option(value.asInstanceOf[String]).filter(_.nonEmpty)
  def endHeapBytes: Option[Long] = None
  def maxHeapBytes: Option[Long] = None
