package scalafim.fmri.laws.profile

import java.nio.file.{Files, Path}
import scalafim.fmri.fit.profile.ProfileTrialReadoutMode
import scalafim.scenarios.ScenarioResult

object TrialReadoutBoundAuditMain:
  def main(args: Array[String]): Unit =
    require(args.length == 1, "output.json")
    val started = System.nanoTime()
    val result = TrialReadoutBoundAudit.run(completed = println)
    def json(value: Any): ujson.Value = value match
      case v: ScenarioResult =>
        ujson.Obj(
          "id" -> v.id,
          "status" -> v.status.toString,
          "ciPass" -> v.ciPass,
          "observations" -> ujson.Arr.from(v.observations.map(_.render)),
          "caveats" -> ujson.Arr.from(v.caveats.map(_.render))
        )
      case v: ProfileTrialReadoutMode => ujson.Str(v.toString)
      case v: Vector[?]               => ujson.Arr.from(v.map(json))
      case v: String                  => ujson.Str(v)
      case v: Double                  => ujson.Num(v)
      case v: Int                     => ujson.Num(v)
      case v: Long                    => ujson.Num(v.toDouble)
      case v: Boolean                 => ujson.Bool(v)
      case v: Product => ujson.Obj.from(v.productElementNames.zip(v.productIterator).map((k, v) => k -> json(v)))
      case v          => throw new IllegalArgumentException(s"unsupported receipt $v")
    val receipt = ujson.Obj(
      "format" -> "phrf-finite-observation-bound-audit/1",
      "qualification" -> "not-admitted",
      "rows" -> 600,
      "trials" -> 30,
      "referenceNodes" -> 8,
      "result" -> json(result),
      "seconds" -> ((System.nanoTime() - started) / 1e9),
      "limitations" -> ujson.Arr(
        "fixed-shape exploratory data",
        "finite rounded arrays only",
        "dense per-shape SVD validation is outside the corrected readout work budget",
        "no enclosure of family evaluation, convolution or whitening",
        "no adaptive coverage claim"
      )
    )
    val _ = Files.writeString(Path.of(args(0)), ujson.write(receipt, indent = 2) + "\n")
