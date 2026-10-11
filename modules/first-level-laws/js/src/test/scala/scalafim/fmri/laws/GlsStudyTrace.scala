package scalafim.fmri.laws

import scala.scalajs.js

private[laws] object GlsStudyTrace:
  private val output = LawEnvironment.get("SCALAFIM_GLS_STUDY_LOG")
  def emit(line: String): Unit =
    output match
      case None =>
        js.Dynamic.global.console.log(line)
        ()
      case Some(path) =>
        js.Dynamic.global.require("fs").appendFileSync(path + "-js.jsonl", line + "\n", "utf8")
        ()
