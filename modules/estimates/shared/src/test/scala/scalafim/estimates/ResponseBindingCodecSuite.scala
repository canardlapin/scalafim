package scalafim.estimates

import java.nio.charset.StandardCharsets
import scalafim.image.SampleSpaces

class ResponseBindingCodecSuite extends munit.FunSuite:
  private def utf8(value: String): Array[Byte] = value.getBytes(StandardCharsets.UTF_8)
  private def digest(label: String) = ResponseActionFixture.digest(label)
  private def level(name: String) = ConditionLevelId(name)
  private val columns = Vector("A_0", "A_1", "B_0", "B_1").map(ColumnId.apply)
  private val binding = ResponseSourceBinding(ResponseActionFixture.unit, ObservationId("sub-01"), digest("design"),
    digest("preparation"), digest("noise"), digest("run"), digest("readout"), ResponseActionFixture.axis(Vector("A", "B"), 2),
    columns, Vector(columns(0), columns(2)), digest("features"),
    RealizedNoise.FixedAr(1, NoiseScope.PerRun, Some(digest("sigma"))), RealizedCombination.FixedWeights(digest("weights")))

  /** Built independently (Python `hashlib`, length-prefixed tokens). */
  private val golden =
    "34:scalafim.response-source-binding/136:00000000-0000-4000-8000-0000000002016:sub-01" +
      "10:fixture/v16:sha25664:a77de8a6daa1129516b14c8ca1e233a51a8414b202eac7c8acfbb9b485046ad4" +
      "10:fixture/v16:sha25664:1d6f04754f9f8c5464cbdd2db0f5dff60b5e4f289b2c3d70a64c35a2cb54e297" +
      "10:fixture/v16:sha25664:efe2a5c09e6d49d7eb735c9875f77404be5b887bb3f56378038968d4e3ff8198" +
      "10:fixture/v16:sha25664:acba25512100f80b56fc3ccd14c65be55d94800cda77585c5f41a887e398f9be" +
      "10:fixture/v16:sha25664:9a46d3ff3e39cd5baa15f2e3f20fc5dadd9a9cae264b634112f9c96adcce86c1" +
      "1:41:A1:01:A1:11:B1:01:B1:11:43:A_03:A_13:B_03:B_11:23:A_03:B_0" +
      "10:fixture/v16:sha25664:5b8a8b56dada6ce7567442b4935298df7de2badd7becdcf4915a59487338ca4b" +
      "7:FixedAr1:16:PerRun4:Some10:fixture/v16:sha25664:38de90475bb334fb3dea5d54f250500aba60fe2c6158115d342b06bcb46e39bf" +
      "12:FixedWeights10:fixture/v16:sha25664:9a129038d9a00aed0cf6a7ea059ca50a813449061ab87848cf1a13eafdf33b2c"

  test("portable SHA-256 matches published and independently computed vectors"):
    def sha(bytes: Array[Byte]) = ResponseDigests.sha256Hex(bytes)
    assertEquals(sha(Array.emptyByteArray), "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    assertEquals(sha(utf8("abc")), "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    assertEquals(sha(utf8("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")),
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
    assertEquals(sha(Array.fill(1000000)('a'.toByte)), "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0")
    assertEquals(sha(utf8("Σ_T ⊗ condition")), "113524357608c580c4a9cb36987c4da856671b36de45387f181a7907c5c6228a")
    // Padding boundaries around one and two 64-byte blocks.
    Vector(55 -> "d5e285683cd4efc02d021a5c62014694958901005d6f71e89e0989fac77e4072",
      56 -> "04c26261370ee7541549d16dee320c723e3fd14671e66a099afe0a377c16888e",
      63 -> "75220b47218278e656f2013bb8f0c455a25eaf01e86c64924e9d48d89776d6f2",
      64 -> "7ce100971f64e7001e8fe5a51973ecdfe1ced42befe7ee8d5fd6219506b5393c",
      65 -> "9537c5fdf120482f7d58d25e9ed583f52c02b4e304ea814db1633ad565aed7e9",
      119 -> "000b48d4edf0fa7bee3c6236ecd2785baa5db4eeb8bb54341b029e0d9fa5fb0c",
      120 -> "13f05a0b594787f5ecd315edc96141bd3243203d1b7d4f0836f37308b276ba98")
      .foreach((n, expected) => assertEquals(sha(Array.fill(n)('x'.toByte)), expected, s"length $n"))

  test("deterministic golden bytes and round trip"):
    val bytes = ResponseBindingCodec.encode(binding)
    assertEquals(new String(bytes, StandardCharsets.UTF_8), golden)
    assertEquals(ResponseDigests.sha256Hex(bytes), "c658a125fccd6fe823b3efba14184b6959cdd0f28156a321a42f1dd9cd479f35")
    assertEquals(ResponseBindingCodec.decode(bytes), Right(DecodedBindingClaim.of(binding)))
    assert(java.util.Arrays.equals(ResponseBindingCodec.encode(binding), bytes), "encoding must be deterministic")

  test("every realized variant round-trips byte-exactly"):
    val noises = Vector(RealizedNoise.White, RealizedNoise.FixedAr(2, NoiseScope.Global, None),
      RealizedNoise.FixedAr(1, NoiseScope.PerFeature, Some(digest("s"))), RealizedNoise.EstimatedAr(3, NoiseScope.PerRun),
      RealizedNoise.Robust, RealizedNoise.LearnedSubspace, RealizedNoise.Unrecorded)
    val combinations = Vector(RealizedCombination.SingleRun, RealizedCombination.FixedWeights(digest("w")),
      RealizedCombination.EstimatedWeights, RealizedCombination.Unrecorded)
    for noise <- noises; combination <- combinations do
      val b = binding.copy(realizedNoise = noise, realizedCombination = combination,
        observation = ObservationId("obs:with:colons 12:x"), readoutAxis = ResponseActionFixture.axis(Vector("1:", "Σ"), 2))
      val bytes = ResponseBindingCodec.encode(b)
      val decoded = ResponseBindingCodec.decode(bytes)
      assertEquals(decoded, Right(DecodedBindingClaim.of(b)))
      assert(java.util.Arrays.equals(ResponseBindingCodec.encode(decoded.toOption.get), bytes))

  test("the decoder refuses malformed, non-canonical and structurally invalid bytes"):
    def decode(text: String) = ResponseBindingCodec.decode(utf8(text))
    val format = "34:scalafim.response-source-binding/1"
    assertEquals(decode(golden.replace(format, "34:scalafim.response-source-binding/2")),
      Left(BindingCodecError.UnsupportedFormat("scalafim.response-source-binding/2")))
    assert(decode(golden.dropRight(3)).left.exists(_.isInstanceOf[BindingCodecError.Malformed]))
    assertEquals(decode(golden + "0:"), Left(BindingCodecError.Malformed("trailing bytes")))
    assert(decode(golden.replace("36:00000000-0000-4000-8000-000000000201", "36:00000000-0000-4000-8000-00000000020Z"))
      .left.exists(_.isInstanceOf[BindingCodecError.Malformed]))
    assertEquals(decode(golden.replace("1:41:A1:01:A1:11:B1:01:B1:1", "1:41:A1:01:B1:01:A1:11:B1:1")),
      Left(BindingCodecError.InvalidAxis(ReadoutDefect.NotConditionMajor(level("A")))))
    assertEquals(decode(golden.replace("1:41:A1:01:A1:11:B1:01:B1:1", "1:41:A1:01:A1:11:B1:11:B1:0")),
      Left(BindingCodecError.InvalidAxis(ReadoutDefect.BinsNotSequential(level("B")))))
    // Upper-case hexadecimal decodes to a lower-case digest, so it is not canonical.
    assertEquals(decode(golden.replace("a77de8a6daa1129516b14c8ca1e233a51a8414b202eac7c8acfbb9b485046ad4",
      "A77DE8A6DAA1129516B14C8CA1E233A51A8414B202EAC7C8ACFBB9B485046AD4")), Left(BindingCodecError.NonCanonical))
    assert(decode(golden.replace("1:23:A_03:B_0", "1:23:A_03:Z_0")).left.exists(_.isInstanceOf[BindingCodecError.Malformed]))
    assert(decode(golden.replace("7:FixedAr1:1", "7:FixedAr2:01")).left.exists(_.isInstanceOf[BindingCodecError.Malformed]))
    assert(decode(golden.replace("7:FixedAr", "7:FixedAq")).left.exists(_.isInstanceOf[BindingCodecError.Malformed]))
    assert(decode("").left.exists(_.isInstanceOf[BindingCodecError.Malformed]))

  test("length prefixes count UTF-8 bytes; non-ASCII goldens are recomputed independently"):
    assertEquals(ResponseDigests.token("\u03a3-01"), "5:\u03a3-01")
    assertEquals(ResponseDigests.token("\uD835\uDEBA"), "4:\uD835\uDEBA")
    val unicode = binding.copy(observation = ObservationId("\u03a3-01"),
      readoutAxis = ResponseActionFixture.axis(Vector("\u03a3", "\uD835\uDEBA"), 2))
    val bytes = ResponseBindingCodec.encode(unicode)
    assertEquals(bytes.length, 899)
    // Python: hashlib.sha256 over the same tokens with len(s.encode()) prefixes.
    assertEquals(ResponseDigests.sha256Hex(bytes), "499929542177c6207d22a409f233671d2569f08bf2146bb148b9f40cf49d6358")
    assertEquals(ResponseBindingCodec.decode(bytes), Right(DecodedBindingClaim.of(unicode)))

  test("declared counts and lengths are bounded by the remaining input before any work"):
    def decode(text: String) = ResponseBindingCodec.decode(utf8(text))
    val rows = "1:41:A1:01:A1:11:B1:01:B1:1"
    assertEquals(decode(golden.replace(rows, "9:9999999991:A1:0")), Left(BindingCodecError.Malformed("count exceeds remaining input")))
    assertEquals(decode(golden.replace("1:43:A_03:A_13:B_03:B_1", "9:9999999993:A_0")),
      Left(BindingCodecError.Malformed("count exceeds remaining input")))
    assertEquals(decode(golden.take(105) + "999999999:x"), Left(BindingCodecError.Malformed("truncated token")))
    assertEquals(decode("1234567890:x"), Left(BindingCodecError.Malformed("invalid token length")))
    assertEquals(decode("12345"), Left(BindingCodecError.Malformed("missing token")))

  test("invalid UTF-8 is refused as non-canonical"):
    val bytes = utf8(golden)
    val at = golden.indexOf("sub-01")
    bytes(at) = 0xff.toByte
    assertEquals(ResponseBindingCodec.decode(bytes), Left(BindingCodecError.NonCanonical))

  test("feature identity is ordered and platform-stable"):
    val domain = EstimateDomain.make(SampleSpaces(Vector(4, 1, 1)), Vector(0, 2, 3), "scanner").toOption.get
    val permuted = EstimateDomain.make(SampleSpaces(Vector(4, 1, 1)), Vector(2, 0, 3), "scanner").toOption.get
    val otherFrame = EstimateDomain.make(SampleSpaces(Vector(4, 1, 1)), Vector(0, 2, 3), "MNI152NLin2009cAsym").toOption.get
    val features = ResponseDigests.features(domain)
    assertEquals(features.schema, ResponseDigests.FeaturesSchema)
    assertNotEquals(ResponseDigests.features(permuted), features)
    assertNotEquals(ResponseDigests.features(otherFrame), features)
    assertEquals(ResponseDigests.features(EstimateDomain.make(SampleSpaces(Vector(4, 1, 1)), Vector(0, 2, 3), "scanner").toOption.get),
      features)
    assertEquals(features.digest.value, FeatureGolden.value)

/** Recomputed independently in Python (identity 4x4 affine, length-prefixed tokens). */
object FeatureGolden:
  val value: String = "4f2cb2e312cf1e9beeac29f2e9a33edecf2ede470d60dc97f330fe1444fe1fc3"
