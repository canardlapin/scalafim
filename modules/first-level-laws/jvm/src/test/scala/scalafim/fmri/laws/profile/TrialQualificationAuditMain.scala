package scalafim.fmri.laws.profile

import java.nio.file.{Files, Path}
import scalafim.fmri.fit.profile.{DecodeStatus, ProfileTrialReadoutMode}
import scalafim.scenarios.ScenarioResult

object TrialQualificationAuditMain:
  def main(args: Array[String]): Unit =
    require(args.length == 1, "output.json")
    val started = System.nanoTime()
    val f = TrialQualificationAudit.fixture()
    val result = TrialQualificationAudit.run(f, completed = label => println(s"completed $label"))
    def json(value: Any): ujson.Value = value match
      case v: ScenarioResult => ujson.Obj("id" -> v.id, "status" -> v.status.toString,
        "ciPass" -> v.ciPass, "observations" -> ujson.Arr.from(v.observations.map(_.render)),
        "caveats" -> ujson.Arr.from(v.caveats.map(_.render)))
      case v: DecodeStatus => ujson.Str(v.toString)
      case v: ProfileTrialReadoutMode => ujson.Str(v.toString)
      case None => ujson.Null
      case Some(v) => json(v)
      case v: Vector[?] => ujson.Arr.from(v.map(json))
      case v: String => ujson.Str(v)
      case v: Double => ujson.Num(v)
      case v: Int => ujson.Num(v)
      case v: Boolean => ujson.Bool(v)
      case v: Product => ujson.Obj.from(v.productElementNames.zip(v.productIterator).map((k, v) => k -> json(v)))
      case v => throw new IllegalArgumentException(s"unsupported receipt $v")
    val receipt = ujson.Obj("qualification" -> "not-admitted", "format" -> "phrf-original-family-audit/1",
      "rows" -> f.rows, "trials" -> f.trials, "voxelsPerCell" -> f.inputBlockVoxels,
      "cells" -> json(TrialQualificationAudit.cells), "result" -> json(result),
      "seconds" -> ((System.nanoTime() - started) / 1e9),
      "limitations" -> ujson.Arr("exploratory seeds, no coverage claim", "no independent global search reference",
        "Cascade34 only", "full observation-window microtime oracle is empirical, not a continuous-time certificate"))
    val _ = Files.writeString(Path.of(args(0)), ujson.write(receipt, indent = 2) + "\n")
