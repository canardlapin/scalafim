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

  test("initial statistic adapter refuses confirmation before any matrix payload parsing"):
    val header = "case\tconfirmation\tunlocked\t0\t1\t1999\t80\n"
    assert(CalibrationProtocolSupport.readCase(header).left.toOption.exists(_.contains("refuses simulator/confirmation")))
