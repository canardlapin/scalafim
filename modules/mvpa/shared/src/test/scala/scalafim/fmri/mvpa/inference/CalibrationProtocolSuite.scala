package scalafim.fmri.mvpa.inference

import munit.FunSuite
import resample4s.kernel.{Seed, StreamDomain, StreamPath}
import resample4s.kernel.derive
import scalafim.fmri.mvpa.AxisDigest

/** Python hashlib and independent R/OpenSSL generated these exact vectors. */
class CalibrationProtocolSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private val vectors = Vector(
    ("fixture", "seed-contract.ascii", 0, 7861255102689790007L, -3422439980998861431L),
    ("fixture", "seed-contract.ascii", 1, 2489104234314668213L, 7056132084412592250L),
    ("fixture", "unicode.κ", 0, 7372199832060588911L, 4122544465954848944L),
    ("simulator", "rank.R0.n80.p6.q4.intercept.null", 17, 7612405912445989119L, 7697461855852248671L),
    ("pilot", "voxel.V0.IID80.omnibus", 199, 5012159652097009163L, 8679182933308029592L),
    ("confirmation", "seed-contract.digest-only-not-a-study", 0, 5166346812316291656L, 4429797797707577380L)
  )

  test("raw UTF-8 SHA/64-bit roots and named provider child streams match external fixtures"):
    vectors.foreach: (phase, scenario, ordinal, expectedRoot, expectedNoise) =>
      val root = right(CalibrationProtocolSupport.seed(phase, scenario, ordinal))
      assertEquals(root, expectedRoot)
      assertEquals(right(CalibrationProtocolSupport.child(root, 101, 0)), expectedNoise)

  test("root digest is not the existing length-framed axis digest"):
    val text = CalibrationProtocolSupport.namespace + "\u0000fixture\u0000unicode.κ\u0000" + "0"
    val raw = CalibrationPlatform.sha256Utf8(text)
    val framed = AxisDigest.sha256Hex(_.string(text))
    assertNotEquals(raw, framed)

  test("published resample seed-path fixture agrees independently"):
    val path = right(right(StreamPath.of(StreamDomain.Repeat, 17)).append(StreamDomain.Unit, 3))
    assertEquals(Seed.fromLong(90210L).derive(path).value, -3497511708555549203L)
    assertEquals(Seed.derivationAlgorithm.value, "seed-path/v1")

  test("worker ordering, retries and plan capacity cannot alter immutable assignments"):
    val forward = Vector.range(0, 12).map(i => right(CalibrationProtocolSupport.seed("pilot", "assignment-fixture", i)))
    val reverse = Vector.range(0, 12).reverse.map(i => right(CalibrationProtocolSupport.seed("pilot", "assignment-fixture", i))).reverse
    assertEquals(forward, reverse)
    assertEquals(forward.head, right(CalibrationProtocolSupport.seed("pilot", "assignment-fixture", 0)))
    assertNotEquals(forward.head, right(CalibrationProtocolSupport.seed("simulator", "assignment-fixture", 0)))
    assert(CalibrationProtocolSupport.seed("pilot", "bad\u0000name", 0).isLeft)
    assert(CalibrationProtocolSupport.seed("pilot", "case", -1).isLeft)

  test("frozen phase counts preserve exact datasets and resampling budgets"):
    assertEquals(CalibrationProtocolSupport.nullDatasets, 10000)
    assertEquals(CalibrationProtocolSupport.alternativeDatasets, 5000)
    assertEquals(CalibrationProtocolSupport.confirmationDraws, 1999)

  test("full dotted scenario and ordinal have a portable collision-resistant internal binding"):
    val scenario = "rank.R0.n80.p6.q4.intercept.null"
    val binding = right(CalibrationProtocolSupport.bindingIdentity(scenario, 17, 39251L))
    // Independently computed by Python hashlib over the exact raw UTF-8 framing.
    assertEquals(binding, "case-25fd8a0c46bc0616f8155e0b7e87b4a224d8d05b090c5159e6e4754cbaa4f854")
    assertNotEquals(binding, right(CalibrationProtocolSupport.bindingIdentity(scenario.replace('.', '-'), 17, 39251L)))
    assertNotEquals(binding, right(CalibrationProtocolSupport.bindingIdentity(scenario, 18, 39251L)))
    assertNotEquals(binding, right(CalibrationProtocolSupport.bindingIdentity(scenario, 17, 39252L)))
    val fixtureRoot = right(CalibrationProtocolSupport.seed("fixture", scenario, 17))
    val pilotRoot = right(CalibrationProtocolSupport.seed("pilot", scenario, 17))
    assertNotEquals(right(CalibrationProtocolSupport.bindingIdentity(scenario, 17, fixtureRoot)),
      right(CalibrationProtocolSupport.bindingIdentity(scenario, 17, pilotRoot)))
    assert(scalafim.response.SourceId.fromString(binding + "-brain-source").isRight)
    assert(scalafim.response.ProvenanceId.fromString(binding + "-brain-source-root").isRight)
    assert(multivar.core.ValueId(binding + "-X").isRight)
    assert(CalibrationProtocolSupport.bindingIdentity("bad\u0000name", 0, 39251L).isLeft)

  test("a thrown assigned-case evaluation becomes one failed record and later cases still run"):
    val prefix = "{\"dataset_index\":17"
    val failed = CalibrationProtocolSupport.retainEvaluation(prefix):
      throw new IllegalArgumentException("real dotted scenario\nfailed")
    val evaluated = CalibrationProtocolSupport.retainEvaluation("{\"dataset_index\":18"):
      Right("{\"dataset_index\":18,\"status\":\"evaluated\"}\n")
    val records = Vector(failed, evaluated)
    assertEquals(records.size, 2)
    assert(records.head.contains("\"status\":\"failed\""))
    assert(records.head.contains("IllegalArgumentException"))
    assert(records.head.contains("\\u000a"))
    assert(records.head.contains("\"p_values\":null,\"reject\":null"))
    assertEquals(records.head.linesIterator.size, 1)
    assert(records.last.contains("\"status\":\"evaluated\""))

  test("initial statistic adapter refuses confirmation before any matrix payload parsing"):
    val header = "case\tconfirmation\tunlocked\t0\t1\t1999\t80\n"
    assert(CalibrationProtocolSupport.readCase(header).left.toOption.exists(_.contains("refuses simulator/confirmation")))

  test("raw partial-null rejection remains visible when an earlier stage closes it off"):
    val metrics = right(CalibrationProtocolSupport.rankMetrics(Vector(.10, .02, .05, .01),
      Vector(.10, .10, .10, .10), Vector(19, 3, 9, 1), 199))
    assertEquals(metrics.rawReject, Vector(false, true, true, true))
    assertEquals(metrics.closedReject, Vector.fill(4)(false))
    assert(metrics.jsonFields.contains("\"raw_exceedances\":[19,3,9,1]"))
    assert(metrics.jsonFields.contains("\"p_value_scope\":\"closed-sequential\""))
    assert(CalibrationProtocolSupport.rankMetrics(Vector(.02, .10), Vector(.02, .02), Vector(3, 19), 199).isLeft)
    assert(CalibrationProtocolSupport.rankMetrics(Vector(.02), Vector(.02), Vector(4), 199).isLeft)
    assert(CalibrationProtocolSupport.rankMetrics(Vector(Double.NaN), Vector(.02), Vector(3), 199).isLeft)

  test("record strings escape control characters and failed outputs retain no partial raw metrics"):
    assertEquals(CalibrationProtocolSupport.jsonString("a\"b\nc\\d"), "\"a\\\"b\\u000ac\\\\d\"")
    val failed = CalibrationProtocolSupport.retainEvaluation("{\"dataset_index\":0"):
      Left("partial computation")
    assert(failed.contains("\"raw_p_values\":null"))
    assert(failed.contains("\"closed_p_values\":null"))
    assert(failed.contains("\"record_schema\":2"))
