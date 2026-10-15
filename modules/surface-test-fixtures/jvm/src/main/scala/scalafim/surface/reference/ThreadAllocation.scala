package scalafim.surface.reference

import java.lang.management.ManagementFactory
import scala.util.control.NonFatal

enum AllocationMeasurement:
  case Measured(bytes: Long)
  case Unavailable(reason: String)

/** Current-thread allocation counters are available from JDK 14 onward.
  * Missing/disabled counters produce a limitation, never a fabricated zero.
  */
private[reference] object ThreadAllocation:
  def measure[A](body: => A): AllocationMeasurement =
    val counter = try
      ManagementFactory.getThreadMXBean match
        case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
          if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
          Right(() => bean.getCurrentThreadAllocatedBytes())
        case _ => Left("runtime does not support current-thread allocation measurement")
    catch case NonFatal(error) => Left(error.toString)
    measureWith(counter)(body)

  private[reference] def measureWith[A](counter: Either[String, () => Long])(body: => A): AllocationMeasurement =
    def read(): Either[String, Long] =
      counter.flatMap: value =>
        try
          val bytes = value()
          Either.cond(bytes >= 0L, bytes, "allocation counter is unavailable (-1)")
        catch case NonFatal(error) => Left(error.toString)
    val before = read()
    val result = body
    val measured = for
      start <- before
      end <- read()
      delta <- Either.cond(end >= start, end - start, "allocation counter decreased")
    yield delta
    java.lang.ref.Reference.reachabilityFence(result)
    measured.fold(AllocationMeasurement.Unavailable.apply, AllocationMeasurement.Measured.apply)
