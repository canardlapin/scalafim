package scalafim.fmri.laws.profile

import scalafim.fmri.fit.profile.*

/** Literal one-column diagnostic. Callback data are observations, branches are not. */
class ConditionC0PlatformTraceSuite extends munit.FunSuite:
  private def quoted(value: String): String = "\"" + value + "\""
  private def obj(fields: (String, String)*): String =
    fields.map((key, value) => quoted(key) + ":" + value).mkString("{", ",", "}")
  private def array(values: Iterable[String]): String = values.mkString("[", ",", "]")
  // Raw IEEE-754 hexadecimal words preserve signed zero and explicit nonfinite values.
  private def number(value: Double): String =
    val bits = java.lang.Long.toHexString(java.lang.Double.doubleToRawLongBits(value))
    val kind = if value.isNaN then "nan:" else if value.isInfinite then "infinity:" else "finite:"
    quoted(kind + ("0" * (16 - bits.length)) + bits)
  private def numbers(values: Iterable[Double]): String = array(values.map(number))
  private def counterValues(c: DecoderCounters): Vector[Long] = Vector(
    c.voxels, c.nodeScores, c.jets, c.exactEvaluations, c.candidateAttempts,
    c.terminalVerifications, c.newtonSteps, c.fallbacks)
  private val counterNames = Vector("voxels", "nodeScores", "jets", "exactEvaluations",
    "candidateAttempts", "terminalVerifications", "newtonSteps", "fallbacks")
  private def counters(values: Vector[Long]): String =
    obj(counterNames.zip(values.map(_.toString))*)
  private def result(value: ShapeDecodeResult): String = obj(
    "status" -> quoted(value.status.toString), "budgetExit" -> value.budgetExit.fold("null")(v => quoted(v.toString)),
    "node" -> value.node.toString, "coordinates" -> numbers(value.coordinates),
    "energy" -> number(value.energy), "amplitudes" -> numbers(value.amplitudes),
    "newtonSteps" -> value.newtonSteps.toString, "dataHessian" -> numbers(value.dataHessian),
    "augmentedHessian" -> numbers(value.augmentedHessian), "conditionalSd" -> numbers(value.conditionalSd),
    "ambiguityGap" -> number(value.ambiguityGap))
  private def jet(value: ProfileJetBuffer): String = obj(
    "energy" -> number(value.energy), "gradient" -> numbers(value.gradient.toVector),
    "hessian" -> numbers(value.hessian.toVector), "amplitudes" -> numbers(value.amplitudes.toVector),
    "curvature" -> quoted(value.curvature.toString))

  private final case class Event(operation: String, json: String, before: Vector[Long], after: Vector[Long])
  private final class TraceObjective(delegate: ShapeObjective, work: DecoderCounters) extends ShapeObjective:
    val events = scala.collection.mutable.ArrayBuffer.empty[Event]
    def grid: NodeGrid = delegate.grid
    def amplitudeCount: Int = delegate.amplitudeCount
    private def record(op: String, node: Option[Int], coordinates: Vector[Double], returned: String,
        out: Option[ProfileJetBuffer], before: Vector[Long]): Unit =
      val after = counterValues(work)
      events += Event(op, obj("ordinal" -> events.length.toString, "operation" -> quoted(op),
        "node" -> node.fold("null")(_.toString), "coordinates" -> numbers(coordinates),
        "returned" -> returned, "jet" -> out.fold("null")(jet),
        "before" -> counters(before), "after" -> counters(after)), before, after)
    def scoreNode(node: Int): Double =
      val before = counterValues(work)
      val value = delegate.scoreNode(node)
      record("scoreNode", Some(node), Vector.empty, number(value), None, before)
      value
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      val before = counterValues(work)
      val value = delegate.jetAtNode(node, out)
      record("jetAtNode", Some(node), Vector.empty, value.toString, Some(out), before)
      value
    def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
      val before = counterValues(work)
      val copied = coordinates.toVector
      val value = delegate.jetAt(coordinates, out)
      record("jetAt", None, copied, value.toString, Some(out), before)
      value
    def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
      val before = counterValues(work)
      val copied = coordinates.toVector
      val value = delegate.energyAt(coordinates, out)
      record("energyAt", None, copied, number(value), Some(out), before)
      value

  test("literal input identity and fresh arrays"):
    val a = ConditionC0LiteralFixture.column()
    val b = ConditionC0LiteralFixture.column()
    assertEquals(a.length, 600)
    assert(a.forall(java.lang.Double.isFinite))
    assert(!(a eq b))
    assertEquals(ConditionC0LiteralFixture.digest(a), ConditionC0LiteralFixture.Digest)
    a(0) = 0.0
    assertEquals(ConditionC0LiteralFixture.digest(b), ConditionC0LiteralFixture.Digest)

  test("single literal decode trace preserves every result bit and work field"):
    val (plain, plainProjection, plainEnergy) = ConditionC0QualificationHarness.platformTraceSetup(ConditionC0LiteralFixture.column())
    val plainCounters = new DecoderCounters
    val direct = new ShapeDecoder(plain, ConditionC0QualificationHarness.Candidate, None, 1.0).decode(plainCounters)
    val (delegate, projection, energy) = ConditionC0QualificationHarness.platformTraceSetup(ConditionC0LiteralFixture.column())
    assertEquals(numbers(projection), numbers(plainProjection))
    assertEquals(number(energy), number(plainEnergy))
    val wrappedCounters = new DecoderCounters
    val traced = new TraceObjective(delegate, wrappedCounters)
    val wrapped = new ShapeDecoder(traced, ConditionC0QualificationHarness.Candidate, None, 1.0).decode(wrappedCounters)
    // Serialized raw words compare every result field exactly, including nonfinite/signed zero.
    assertEquals(result(wrapped), result(direct))
    assertEquals(counterValues(wrappedCounters), counterValues(plainCounters))
    assert(traced.events.nonEmpty)
    traced.events.foreach(e => assertEquals(e.after, e.before))
    assertEquals(traced.events.count(_.operation == "scoreNode").toLong, wrappedCounters.nodeScores)
    assertEquals(traced.events.count(e => e.operation == "jetAt" || e.operation == "jetAtNode").toLong, wrappedCounters.jets)
    assertEquals(traced.events.count(_.operation == "energyAt").toLong, wrappedCounters.exactEvaluations)
    assertEquals(traced.events.length.toLong, wrappedCounters.nodeScores + wrappedCounters.jets + wrappedCounters.exactEvaluations)
    println(obj("kind" -> quoted("c0-literal-platform-trace"), "platform" -> quoted(ConditionC0QualificationPlatform.name),
      "fixtureDigest" -> quoted(ConditionC0LiteralFixture.Digest), "sourceFreeze" -> quoted(ConditionC0LiteralFixture.SourceFreeze),
      "policy" -> quoted(ConditionC0QualificationHarness.CandidateLabel), "normalization" -> quoted("Unnormalised"),
      "prior" -> "null", "noiseVariance" -> number(1.0), "encoding" -> quoted("classified-raw-ieee754-hex"),
      "projectedResponse" -> numbers(projection), "projectedEnergy" -> number(energy),
      "direct" -> result(direct), "wrapped" -> result(wrapped), "comparisonExact" -> "true",
      "directCounters" -> counters(counterValues(plainCounters)), "wrappedCounters" -> counters(counterValues(wrappedCounters)),
      "events" -> array(traced.events.map(_.json))))
