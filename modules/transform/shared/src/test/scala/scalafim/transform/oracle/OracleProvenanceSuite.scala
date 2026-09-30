package scalafim.transform.oracle

/** Every oracle file must be byte-identical to its recorded provenance, on both platforms. */
class OracleProvenanceSuite extends munit.FunSuite:
  private def recorded(manifest: String): Vector[(String, String)] =
    val text = OracleFixtures.text(manifest)
    val section = text.substring(text.indexOf("\"sha256\""))
    "\"([^\"]+)\":\\s*\"([0-9a-f]{64})\"".r
      .findAllMatchIn(section)
      .map(m => m.group(1) -> m.group(2))
      .toVector

  test("SHA-256 matches the FIPS 180-4 test vectors"):
    assertEquals(Sha256.hex(Array.emptyByteArray), "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    assertEquals(Sha256.hex("abc".getBytes("UTF-8")), "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    assertEquals(
      Sha256.hex("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq".getBytes("UTF-8")),
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    )

  test("vendored neurotransform oracles cover the four imported sets"):
    val files = recorded("neurotransform/PROVENANCE.json")
    assertEquals(files.map(_._1.takeWhile(_ != '/')).distinct.sorted, Vector("afni_oracle", "fsl_coef_oracle", "fsl_dense_oracle", "itk_oracle"))
    assert(files.size > 100, s"only ${files.size} files recorded")

  test("every vendored neurotransform oracle file matches its recorded SHA-256"):
    val mismatched = recorded("neurotransform/PROVENANCE.json").filterNot((path, digest) =>
      OracleFixtures.sha256Hex(s"neurotransform/$path") == digest
    )
    assertEquals(mismatched.map(_._1), Vector.empty)

  test("gzip fixtures decode to NIfTI-1 headers on this platform"):
    val header = OracleFixtures.decoded("neurotransform/itk_oracle/warp.nii.gz")
    val sizeofHdr = (header(0) & 0xff) | ((header(1) & 0xff) << 8) | ((header(2) & 0xff) << 16) | ((header(3) & 0xff) << 24)
    assertEquals(sizeofHdr, 348)
