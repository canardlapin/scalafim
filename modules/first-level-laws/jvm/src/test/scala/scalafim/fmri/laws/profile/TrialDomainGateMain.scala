package scalafim.fmri.laws.profile

import java.nio.file.{Files, Path}
import java.security.MessageDigest

object TrialDomainGateMain:
  def main(args: Array[String]): Unit =
    require(args.length == 3, "protocol.json output.json gate|stress")
    val protocolBytes = Files.readAllBytes(Path.of(args(0)))
    val hash = MessageDigest.getInstance("SHA-256").digest(protocolBytes).map(b => f"${b & 0xff}%02x").mkString
    require(hash == TrialDomainProposal.protocolSha256, "protocol changed after freezing")
    def json(value: Any): ujson.Value = value match
      case v: ujson.Value                       => v
      case v: scalafim.scenarios.ScenarioResult =>
        ujson.Obj(
          "id" -> v.id,
          "status" -> v.status.toString,
          "ciPass" -> v.ciPass,
          "observations" -> ujson.Arr.from(v.observations.map(_.render)),
          "caveats" -> ujson.Arr.from(v.caveats.map(_.render))
        )
      case None         => ujson.Null
      case Some(v)      => json(v)
      case Left(v)      => ujson.Obj("status" -> "refused", "error" -> json(v))
      case Right(v)     => ujson.Obj("status" -> "prepared", "geometry" -> json(v))
      case v: Vector[?] => ujson.Arr.from(v.map(json))
      case v: String    => ujson.Str(v)
      case v: Double    => if v.isFinite then ujson.Num(v) else ujson.Str(v.toString)
      case v: Int       => ujson.Num(v)
      case v: Long      => ujson.Num(v.toDouble)
      case v: Boolean   => ujson.Bool(v)
      case v: Product   => ujson.Obj.from(v.productElementNames.zip(v.productIterator).map((k, v) => k -> json(v)))
      case v            => throw new IllegalArgumentException(s"unsupported receipt $v")
    val start = System.nanoTime()
    val result = args(2) match
      case "gate" =>
        json(
          Vector(
            TrialDomainGate.run(30, 64, 32, completed = println),
            TrialDomainGate.run(300, 256, 64, completed = println)
          )
        )
      case "stress" => json(TrialDomainGate.preparation(1200))
      case other    => throw new IllegalArgumentException(s"unknown mode $other")
    val receipt = ujson.Obj(
      "protocol" -> TrialDomainProposal.protocolId,
      "protocolSha256" -> hash,
      "qualification" -> "not-admitted",
      "mode" -> args(2),
      "result" -> result,
      "tailMassUpper" -> TrialDomainProposal.tailMassUpper,
      "shapeSummaries" -> json(TrialDomainGate.summaries),
      "seconds" -> (System.nanoTime() - start) / 1e9
    )
    val _ = Files.writeString(Path.of(args(1)), ujson.write(receipt, indent = 2) + "\n")
