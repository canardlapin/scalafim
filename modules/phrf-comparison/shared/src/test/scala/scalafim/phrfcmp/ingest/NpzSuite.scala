package scalafim.phrfcmp.ingest

import scalafim.phrfcmp.ingest.TestSupport.*

class NpzSuite extends munit.FunSuite:

  private def refusal(r: Either[NpzError, Npz]): NpzError = r match
    case Left(e)  => e
    case Right(_) => fail("expected a refusal")

  private val good = npyF64(Seq(2), Seq(1.0, 2.0))

  test("sha256 and crc32 known vectors"):
    assertEquals(Digests.sha256Hex(Array.emptyByteArray), "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    assertEquals(Digests.sha256Hex("abc".getBytes("US-ASCII")), "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    assertEquals(
      Digests.sha256Hex("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq".getBytes("US-ASCII")),
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    )
    assertEquals(Digests.sha256Hex(Array.fill(1000000)('a'.toByte)), "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0")
    val padEdge = Map(
      55 -> "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318",
      63 -> "7d3e74a05d7db15bce4ad9ec0658ea98e3f06eeecf16b4c6fff2da457ddc2f34",
      64 -> "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb",
      119 -> "31eba51c313a5c08226adf18d4a359cfdfd8d2e816b13f4af952f7ea6584dcfb",
      120 -> "2f3d335432c70b580af0e8e1b3674a7c020d683aa5f73aaaedfdc55af904c21c"
    )
    padEdge.foreach((n, h) => assertEquals(Digests.sha256Hex(Array.fill(n)('a'.toByte)), h, s"length $n"))
    assertEquals(Digests.crc32("123456789".getBytes("US-ASCII"), 0, 9), 0xcbf43926L)

  test("golden: real generator bytes parse to the exact values"):
    assertEquals(Digests.sha256Hex(GoldenBytes.npz), GoldenBytes.NpzSha256)
    val npz = Npz.parse(GoldenBytes.npz).fold(e => fail(e.message), identity)
    assertEquals(npz.names, Vector("a_f64", "b_i32", "c_empty"))
    npz.get("a_f64").map(_.array) match
      case Some(NpyArray.F64(shape, d)) =>
        assertEquals(shape, Vector(2, 3))
        assertEqualsDouble(d(0), 1.5, 0.0)
        assert(java.lang.Double.doubleToLongBits(d(1)) == java.lang.Double.doubleToLongBits(-0.0))
        assertEqualsDouble(d(2), math.Pi, 0.0)
        assertEqualsDouble(d(3), 1e-300, 0.0)
        assertEqualsDouble(d(4), -2.25, 0.0)
        assert(d(5).isPosInfinity)
      case other => fail(s"unexpected $other")
    npz.get("b_i32").map(_.array) match
      case Some(NpyArray.I32(shape, d)) =>
        assertEquals(shape, Vector(4))
        assertEquals(d.toSeq, Seq(-1, 0, Int.MaxValue, Int.MinValue))
      case other => fail(s"unexpected $other")
    npz.get("c_empty").map(_.array) match
      case Some(NpyArray.F64(shape, d)) =>
        assertEquals(shape, Vector(0, 3))
        assertEquals(d.length, 0)
      case other => fail(s"unexpected $other")

  test("golden: platform-identical decode fingerprint"):
    val fp = fingerprint(Npz.parse(GoldenBytes.npz).fold(e => fail(e.message), identity))
    assertEquals(
      Digests.sha256Hex(fp.getBytes("UTF-8")),
      "b01ceef63c046f82dc7e154e232430a3e7cfc036f6ce6511f79292b63af5afcf"
    )

  test("member raw bytes are the exact npy bytes (what the manifest hashes)"):
    val z = zip(Seq("x.npy" -> good))
    val npz = Npz.parse(z).fold(e => fail(e.message), identity)
    assertEquals(npz.members.head.rawNpy.toSeq, good.toSeq)

  test("frozen adapter output parses identically on JVM and JS"):
    val bytes = AdapterGoldenBytes.npz
    assertEquals(Digests.sha256Hex(bytes), AdapterGoldenBytes.NpzSha256)
    val npz = Npz.parse(bytes).fold(e => fail(e.message), identity)
    assertEquals(npz.names, Vector(
      "d_FRACvalue", "d_HRFindex", "d_R2", "d_beta_data", "d_beta_psc", "hrf_library",
      "meanvol", "noisepool", "trial_cond", "trial_event_index", "trial_run", "trial_vol"
    ))
    assertEquals(npz.get("d_beta_data").map(_.array.shape), Some(Vector(3, 144)))
    assertEquals(npz.get("noisepool").map(_.array.shape), Some(Vector(5)))
    assertEquals(
      Digests.sha256Hex(fingerprint(npz).getBytes("UTF-8")),
      AdapterGoldenBytes.FingerprintSha256
    )

  test("tiny STORED npz built in-test parses (f8, i4, scalar shape, 1-D, 3-D)"):
    val z = zip(
      Seq(
        "a.npy" -> npyF64(Seq(2, 3), Seq(1, 2, 3, 4, 5, 6)),
        "b.npy" -> npyI32(Seq(3), Seq(7, -8, 9)),
        "s.npy" -> npyF64(Seq(), Seq(2.5)),
        "t.npy" -> npyF64(Seq(1, 2, 2), Seq(1, 2, 3, 4))
      )
    )
    val npz = Npz.parse(z).fold(e => fail(e.message), identity)
    assertEquals(npz.names, Vector("a", "b", "s", "t"))
    assertEquals(npz.get("s").map(_.array.shape), Some(Vector.empty[Int]))
    assertEquals(npz.get("t").map(_.array.shape), Some(Vector(1, 2, 2)))

  test("refuses compressed member (DEFLATE) and other methods"):
    assertEquals(refusal(Npz.parse(zip(Seq("x.npy" -> good), ZipOpts(method = 8)))), NpzError.CompressedMember("x.npy", 8))
    assertEquals(refusal(Npz.parse(zip(Seq("x.npy" -> good), ZipOpts(method = 12)))), NpzError.CompressedMember("x.npy", 12))

  test("refuses encrypted, data-descriptor, bad CRC, zip64 marker, comment"):
    assertEquals(refusal(Npz.parse(zip(Seq("x.npy" -> good), ZipOpts(flags = 1)))), NpzError.Encrypted("x.npy"))
    assertEquals(refusal(Npz.parse(zip(Seq("x.npy" -> good), ZipOpts(flags = 8)))), NpzError.DataDescriptor("x.npy"))
    assertEquals(refusal(Npz.parse(zip(Seq("x.npy" -> good), ZipOpts(badCrc = true)))), NpzError.CrcMismatch("x.npy"))
    assertEquals(refusal(Npz.parse(zip(Seq("x.npy" -> good), ZipOpts(zip64 = true)))), NpzError.Zip64OrMultiDisk)
    assertEquals(refusal(Npz.parse(zip(Seq("x.npy" -> good), ZipOpts(comment = true)))), NpzError.Truncated("end of central directory record"))
    assertEquals(refusal(Npz.parse(zip(Seq("x.npy" -> good), ZipOpts(localName = Some("y.npy"))))), NpzError.LocalHeaderMismatch("x.npy"))

  test("refuses unknown and duplicate members"):
    assertEquals(refusal(Npz.parse(zip(Seq("meta.json" -> "{}".getBytes("US-ASCII"))))), NpzError.UnknownMember("meta.json"))
    assertEquals(refusal(Npz.parse(zip(Seq("d/x.npy" -> good)))), NpzError.UnknownMember("d/x.npy"))
    assertEquals(refusal(Npz.parse(zip(Seq(".npy" -> good)))), NpzError.UnknownMember(".npy"))
    assertEquals(refusal(Npz.parse(zip(Seq("x.npy" -> good, "x.npy" -> good)))), NpzError.DuplicateMember("x.npy"))

  test("central directory integrity: swapped offsets, gaps, leading bytes, traversal names"):
    val two = Seq("a.npy" -> npyF64(Seq(1), Seq(1.0)), "b.npy" -> npyF64(Seq(1), Seq(2.0)))
    assert(Npz.parse(zip(two)).isRight)
    // Central entries pointing at each other's local headers.
    assert(refusal(Npz.parse(zip(two, ZipOpts(swapOffsets = true)))).isInstanceOf[NpzError.LocalHeaderMismatch])
    // Unaccounted bytes between local entries or before the first one: the entries do not tile the archive.
    assert(refusal(Npz.parse(zip(two, ZipOpts(gap = 7)))).isInstanceOf[NpzError.NotZip])
    assert(refusal(Npz.parse(zip(two, ZipOpts(lead = 3)))).isInstanceOf[NpzError.NotZip])
    assertEquals(refusal(Npz.parse(zip(Seq("../x.npy" -> good)))), NpzError.UnknownMember("../x.npy"))
    assertEquals(refusal(Npz.parse(zip(Seq("/abs.npy" -> good)))), NpzError.UnknownMember("/abs.npy"))

  test("npy header strictness: padding, shape grammar"):
    def one(b: Array[Byte]) = refusal(Npz.parse(zip(Seq("x.npy" -> b))))
    val d = f64Bytes(1.0, 2.0)
    assert(Npz.parse(zip(Seq("x.npy" -> npy(d, "<f8", "(2,)")))).isRight)
    assert(one(npy(d, "<f8", "(2,)", pad = 16)).isInstanceOf[NpzError.MalformedHeader])
    assert(one(npy(d, "<f8", "(a, 2)")).isInstanceOf[NpzError.MalformedHeader])
    assert(one(npy(d, "<f8", "(02,)")).isInstanceOf[NpzError.MalformedHeader])
    assert(one(npy(d, "<f8", "(2)")).isInstanceOf[NpzError.MalformedHeader])
    assert(one(npy(d, "<f8", "(2,3)")).isInstanceOf[NpzError.MalformedHeader])
    assert(one(npy(d, "<f8", "(-2,)")).isInstanceOf[NpzError.MalformedHeader])
    assertEquals(one(npy(d, "<f8", "(99999999999,)")), NpzError.ShapeOverflow("x"))

  test("refuses non-conforming npy payloads"):
    def one(b: Array[Byte]) = refusal(Npz.parse(zip(Seq("x.npy" -> b))))
    val d = f64Bytes(1.0, 2.0)
    assertEquals(one(npy(d, "<f4", "(2,)")), NpzError.UnsupportedDtype("x", "<f4"))
    assertEquals(one(npy(d, ">f8", "(2,)")), NpzError.UnsupportedDtype("x", ">f8"))
    assertEquals(one(npy(d, "<i8", "(2,)")), NpzError.UnsupportedDtype("x", "<i8"))
    assertEquals(one(npy(d, "<u4", "(2,)")), NpzError.UnsupportedDtype("x", "<u4"))
    assertEquals(one(npy(d, "|O", "(2,)")), NpzError.UnsupportedDtype("x", "|O"))
    assertEquals(one(npy(d, "<U3", "(2,)")), NpzError.UnsupportedDtype("x", "<U3"))
    assertEquals(one(npy(d, "<f8", "(2,)", fortran = true)), NpzError.FortranOrder("x"))
    assertEquals(one(npy(d, "<f8", "(2,)", major = 2)), NpzError.UnsupportedNpyVersion("x", 2, 0))
    assertEquals(one(npy(d, "<f8", "(2,)", major = 3)), NpzError.UnsupportedNpyVersion("x", 3, 0))
    assertEquals(one(npy(d, "<f8", "(2,)", minor = 1)), NpzError.UnsupportedNpyVersion("x", 1, 1))
    assertEquals(one(npy(d, "<f8", "(2,)", magicOk = false)), NpzError.BadMagic("x"))
    assertEquals(one(npy(d, "<f8", "(3,)")), NpzError.DataLengthMismatch("x", 24, 16))
    assertEquals(one(npy(d, "<f8", "(1,)")), NpzError.DataLengthMismatch("x", 8, 16))
    assertEquals(one(npy(d, "<f8", "(70000, 70000)")), NpzError.ShapeOverflow("x"))
    assert(one(npy(d, "<f8", "[2]")).isInstanceOf[NpzError.MalformedHeader])
    assert(one(Array.fill[Byte](4)(0)).isInstanceOf[NpzError.Truncated])

  test("every strict prefix of a valid npz is refused"):
    val z = zip(Seq("a.npy" -> npyF64(Seq(2), Seq(1, 2)), "b.npy" -> npyI32(Seq(2), Seq(3, 4))))
    assert(Npz.parse(z).isRight)
    for n <- 0 until z.length do assert(Npz.parse(java.util.Arrays.copyOf(z, n)).isLeft, s"prefix $n accepted")
    assertEquals(refusal(Npz.parse(java.util.Arrays.copyOf(z, z.length - 5))), NpzError.Truncated("end of central directory record"))

  test("a flipped data byte is caught by the CRC"):
    val z = zip(Seq("a.npy" -> npyF64(Seq(2), Seq(1, 2))))
    val bad = z.clone()
    bad(bad.length - 22 - 46 - "a.npy".length - 3) = (bad(bad.length - 22 - 46 - "a.npy".length - 3) ^ 0x01).toByte
    assertEquals(refusal(Npz.parse(bad)), NpzError.CrcMismatch("a.npy"))
