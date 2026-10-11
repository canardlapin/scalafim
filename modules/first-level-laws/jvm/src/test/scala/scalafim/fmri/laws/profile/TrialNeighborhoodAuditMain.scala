package scalafim.fmri.laws.profile

import java.nio.file.{Files, Path}

object TrialNeighborhoodAuditMain:
  def main(args: Array[String]): Unit =
    require(
      args.length >= 2 && args.length <= 4,
      "output.json neighborhoods|certificate|stress-preparation [trials] [fresh-count]"
    )
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
      case v: Vector[?] => ujson.Arr.from(v.map(json))
      case v: String    => ujson.Str(v)
      case v: Double    => if v.isFinite then ujson.Num(v) else ujson.Str(v.toString)
      case v: Int       => ujson.Num(v)
      case v: Long      => ujson.Num(v.toDouble)
      case v: Boolean   => ujson.Bool(v)
      case v: Product   => ujson.Obj.from(v.productElementNames.zip(v.productIterator).map((k, v) => k -> json(v)))
      case v            => throw new IllegalArgumentException(s"unsupported receipt $v")
    val start = System.nanoTime()
    val result = args(1) match
      case "neighborhoods" =>
        Vector(48.0, 96.0).map(h =>
          TrialNeighborhoodAudit.run(
            trials = args.lift(2).fold(30)(_.toInt),
            horizon = h,
            freshCount = args.lift(3).fold(64)(_.toInt),
            selectedRadii =
              if args.lift(2).contains("300") then Vector(0.0, 0.01, 0.1) else TrialNeighborhoodAudit.radii,
            completed = println
          )
        )
      case "certificate" =>
        Vector(48.0, 96.0).map(h => TrialNeighborhoodCertificateAudit.run(horizon = h, completed = println))
      case "stress-preparation" =>
        Vector(48.0, 96.0).map: horizon =>
          val started = System.nanoTime()
          val f = TrialNeighborhoodAudit.fixture(1200, horizon)
          val receipt = ujson.Obj("trials" -> 1200, "horizon" -> horizon)
          f.prepare match
            case Left(error) =>
              receipt("admitted") = false
              receipt("refusal") = error.message
            case Right(prepared) =>
              receipt("admitted") = true
              receipt("sharedReferenceBytes") = prepared.setup.retainedReferenceBytes.get.toDouble
              val trial = prepared.setup.trial.get
              receipt("basisRank") = trial.basisRank
              receipt("bandwidth") = trial.bandwidth
              receipt("retainedPreparationDoubles") = trial.retainedDoubles.toDouble
          receipt("seconds") = (System.nanoTime() - started) / 1e9
          receipt
      case other => throw new IllegalArgumentException(s"unknown mode $other")
    val receipt = ujson.Obj(
      "format" -> "phrf-neighborhood-feasibility/1",
      "qualification" -> "not-admitted",
      "mode" -> args(1),
      "result" -> json(result),
      "seconds" -> ((System.nanoTime() - start) / 1e9),
      "limitations" -> ujson.Arr(
        "fixed shapes, no decoder",
        "exploratory cohort, no scientific confirmation",
        "all-eight reference comparison is an offline oracle, not runtime routing",
        "dense certificate work exceeds the fast-path budget"
      )
    )
    val _ = Files.writeString(Path.of(args(0)), ujson.write(receipt, indent = 2) + "\n")
