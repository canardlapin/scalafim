package scalafim.fmri.laws

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

private[laws] object GlsStudyTrace:
  private val output = LawEnvironment.get("SCALAFIM_GLS_STUDY_LOG").map(prefix => Path.of(prefix + "-jvm.jsonl"))
  def emit(line: String): Unit =
    output match
      case None       => println(line)
      case Some(path) =>
        Files.writeString(
          path,
          line + "\n",
          StandardCharsets.UTF_8,
          StandardOpenOption.CREATE,
          StandardOpenOption.APPEND
        )
        ()
