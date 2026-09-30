package scalafim.fmri.laws.profile

import scalafim.fmri.fit.profile.*

private[profile] final case class LwuTerminalDiagnostic(
    energy: Double, gradient: Vector[Double], hessian: Vector[Double], amplitudes: Vector[Double], curvature: String)

private[profile] final case class LwuConditionDiagnosticRecord(
    voxel: Int, snr: Double, seed: Long, truth: Vector[Double], truthAmplitudes: Vector[Double],
    decode: ShapeDecodeResult, amplitudes: Vector[Double], fitEnergy: Double, work: Vector[Long],
    terminal: Option[LwuTerminalDiagnostic], whitenedFingerprint: String,
    rawInput: Option[Vector[Double]], whitenedInput: Option[Vector[Double]])

private[profile] object LwuConditionDiagnosticRecord:
  def work(c: DecoderCounters): Vector[Long] =
    Vector(c.nodeScores, c.jets, c.exactEvaluations, c.candidateAttempts, c.terminalVerifications, c.newtonSteps, c.fallbacks)

  /** Noncryptographic exact-word fingerprint; raw receipt hashes provide integrity. */
  def fingerprint(values: Array[Double]): String =
    var hash = 0L
    var i = 0
    while i < values.length do
      hash = java.lang.Long.rotateLeft(hash, 5) ^ java.lang.Double.doubleToRawLongBits(values(i))
      i += 1
    java.lang.Long.toHexString(hash)

  private def quote(value: String): String =
    "\"" + value.flatMap:
      case '"' => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c if c < ' ' => f"\\u${c.toInt}%04x"
      case c => c.toString
    + "\""

  private def number(value: Double): String = if value.isFinite then value.toString else "null"
  private def numbers(values: Vector[Double]): String = values.map(number).mkString("[", ",", "]")
  private def hex(values: Vector[Double]): String = values.map(v => quote(java.lang.Double.toHexString(v))).mkString("[", ",", "]")
  private def fields(values: (String, String)*): String = values.map((k, v) => quote(k) + ":" + v).mkString("{", ",", "}")

  def json(r: LwuConditionDiagnosticRecord): String =
    val terminal = r.terminal.fold("null"): t =>
      fields("energy" -> number(t.energy), "gradient" -> numbers(t.gradient), "hessian" -> numbers(t.hessian),
        "amplitudes" -> numbers(t.amplitudes), "curvature" -> quote(t.curvature),
        "energyHex" -> quote(java.lang.Double.toHexString(t.energy)), "gradientHex" -> hex(t.gradient), "hessianHex" -> hex(t.hessian))
    fields("kind" -> quote("lwu-diagnostic"), "cohort" -> quote("historical-dev"),
      "sourceId" -> quote(ConditionC0QualificationPlatform.executionSourceId.getOrElse("unfrozen-dev")),
      "platform" -> quote(ConditionC0QualificationPlatform.name), "snr" -> number(r.snr), "seed" -> r.seed.toString,
      "voxel" -> r.voxel.toString, "truth" -> numbers(r.truth), "truthAmplitudes" -> numbers(r.truthAmplitudes),
      "coordinates" -> numbers(r.decode.coordinates), "status" -> quote(r.decode.status.toString),
      "budgetExit" -> r.decode.budgetExit.fold("null")(e => quote(e.toString)),
      "decodeEnergy" -> number(r.decode.energy), "fitEnergy" -> number(r.fitEnergy), "amplitudes" -> numbers(r.amplitudes),
      "dataHessian" -> numbers(r.decode.dataHessian), "augmentedHessian" -> numbers(r.decode.augmentedHessian),
      "conditionalSd" -> numbers(r.decode.conditionalSd), "work" -> r.work.mkString("[", ",", "]"),
      "terminal" -> terminal, "whitenedFingerprint" -> quote(r.whitenedFingerprint),
      "rawInputHex" -> r.rawInput.fold("null")(hex), "whitenedInputHex" -> r.whitenedInput.fold("null")(hex))

/** Explicit historical diagnostic entry. It does not invoke the old suite's oracle or throughput tests. */
object LwuConditionDiagnosticMain:
  def main(args: Array[String]): Unit =
    val command = ConditionC0QualificationPlatform.arguments(args)
    require(command.contains("--historical-dev"), "explicit historical DEV diagnostic mode required")
    val fixture = new ConditionMilestoneSuite
    Vector((1.0, 111L), (.5, 112L)).foreach: (snr, seed) =>
      val records = fixture.diagnoseLwu(100, snr, seed)
      records.foreach(r => println(LwuConditionDiagnosticRecord.json(r)))
      if command.contains("--verify-original-counts") then
        val expected = (ConditionC0QualificationPlatform.name, snr) match
          case ("JVM", 1.0) => 91
          case ("JVM", _) => 81
          case ("JS", 1.0) => 89
          case ("JS", _) => 82
          case _ => throw IllegalArgumentException("unknown diagnostic platform")
        require(records.count(_.decode.status == DecodeStatus.Accepted) == expected, "original frozen admission count changed")

class LwuConditionDiagnosticSuite extends munit.FunSuite:
  test("historical LWU diagnostic retains fit, input availability and all work fields"):
    val records = new ConditionMilestoneSuite().diagnoseLwu(6, .5, 112L)
    assertEquals(records.map(_.voxel), (0 until 6).toVector)
    records.foreach: r =>
      assertEquals(r.truth.length, 3)
      assertEquals(r.truthAmplitudes.length, 3)
      assertEquals(r.decode.coordinates.length, 3)
      assertEquals(r.work.length, 7)
      assert(r.work.forall(_ >= 0L))
      assert(r.work(0) <= 819 && r.work(1) <= 8 && r.work(2) <= 2 && r.work(3) <= 9 && r.work(5) <= 6)
      assertEquals(r.rawInput.nonEmpty, r.decode.status != DecodeStatus.Accepted)
      assertEquals(r.whitenedInput.nonEmpty, r.decode.status != DecodeStatus.Accepted)
      r.whitenedInput.foreach: input =>
        assertEquals(input.length, 600)
        assertEquals(LwuConditionDiagnosticRecord.fingerprint(input.toArray), r.whitenedFingerprint)
      val t = r.terminal.getOrElse(fail("terminal jet unavailable"))
      assertEquals(t.hessian.length, 9)
      assertEquals(t.gradient.length, 3)
      assertEquals(t.amplitudes.length, 3)
      assertEqualsDouble(t.energy, r.fitEnergy, 1e-9)
      assertEqualsDouble(t.energy, r.decode.energy, 1e-9)
      if r.decode.dataHessian.forall(_.isFinite) then
        r.decode.dataHessian.zip(t.hessian).foreach((a, b) => assertEqualsDouble(a, b, 1e-8))
        r.decode.augmentedHessian.zip(t.hessian).foreach((a, b) => assertEqualsDouble(a, b, 1e-8))
      else
        assert(r.decode.dataHessian.forall(_.isNaN))
        assert(r.decode.augmentedHessian.forall(_.isNaN))
        assert(r.decode.conditionalSd.forall(_.isNaN))
        assert(r.decode.status != DecodeStatus.Accepted)
      assert(LwuConditionDiagnosticRecord.json(r).contains("\"cohort\":\"historical-dev\""))
