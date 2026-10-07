package scalafim.phrfcmp.exec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

/** A minimal runner used by the interop test: performs root intake exactly as the real runner must, then reports
  * what it observed (never the root itself) to the file named by the second argument.
  * Arguments: expected root, report file.
  */
object RootIntakeProbe:
  def main(args: Array[String]): Unit =
    val report = Paths.get(args(1))
    val rootFd = sys.env.get(RootIntake.RootVar)
    val ackFd = sys.env.get(RootIntake.AckVar)
    val lines = scala.collection.mutable.ArrayBuffer.empty[String]
    val code = RootIntake.receive() match
      case Left(e) =>
        lines += s"error=${e.message}"
        2
      case Right(r) =>
        lines += s"root_matches=${r.root.value == java.lang.Long.parseUnsignedLong(args(0))}"
        lines += s"redacted=${r.root.toString == "PilotRoot(redacted)" && r.toString == "Received(redacted)"}"
        lines += s"children_after_intake=${ProcessHandle.current().children().count()}"
        // a child started the supported way must not see the pipe variables
        val pb = ChildEnvironment.scrub(new ProcessBuilder("sh", "-c", "echo ${PHRF_ROOT_FD-unset}/${PHRF_ROOT_ACK_FD-unset}").redirectErrorStream(true))
        val p = pb.start()
        lines += s"child_env=${new String(p.getInputStream.readAllBytes(), UTF_8).trim}"
        p.waitFor(): Unit
        lines += s"parent_env_had_vars=${rootFd.isDefined && ackFd.isDefined}"
        0
    Files.write(report, lines.mkString("\n").getBytes(UTF_8))
    sys.exit(code)
