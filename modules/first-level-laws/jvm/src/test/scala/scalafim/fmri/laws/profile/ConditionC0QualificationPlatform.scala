package scalafim.fmri.laws.profile

object ConditionC0QualificationPlatform:
  val name = "JVM"
  def arguments(args: Array[String]): Array[String] = args
  def executionSourceId: Option[String] = Option(System.getenv("C0_SOURCE_ID")).filter(_.nonEmpty)

  /** Process heap after the run; neither a peak nor an engine-live bound. */
  def endHeapBytes: Option[Long] =
    val runtime = Runtime.getRuntime
    Some(runtime.totalMemory - runtime.freeMemory)
  def maxHeapBytes: Option[Long] = Some(Runtime.getRuntime.maxMemory)
