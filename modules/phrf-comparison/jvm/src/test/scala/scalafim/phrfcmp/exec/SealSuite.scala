package scalafim.phrfcmp.exec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

class SealSuite extends munit.FunSuite:

  private def hex(s: String): Array[Byte] = Fs.unhex(s)

  test("RFC 7748 section 6.1: X25519 public keys and shared secret") {
    val aPriv = hex("77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a")
    val bPriv = hex("5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
    assertEquals(Fs.hex(X25519.publicOf(aPriv)), "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a")
    assertEquals(Fs.hex(X25519.publicOf(bPriv)), "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f")
    val shared = "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742"
    assertEquals(Fs.hex(X25519.agree(aPriv, X25519.publicOf(bPriv)).toOption.get), shared)
    assertEquals(Fs.hex(X25519.agree(bPriv, X25519.publicOf(aPriv)).toOption.get), shared)
  }

  test("RFC 5869 appendix A.1 and A.3: HKDF-SHA256") {
    val ikm = hex("0b" * 22)
    assertEquals(
      Fs.hex(Hkdf.sha256(ikm, hex("000102030405060708090a0b0c"), hex("f0f1f2f3f4f5f6f7f8f9"), 42)),
      "3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865"
    )
    assertEquals(
      Fs.hex(Hkdf.sha256(ikm, Array.emptyByteArray, Array.emptyByteArray, 42)),
      "8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8"
    )
  }

  test("low-order points are errors, not exceptions: 0, 1, the two order-8 points, and p-1, p, p+1") {
    val priv = Array.tabulate[Byte](32)(i => (i + 1).toByte)
    // little-endian u-coordinates (the libsodium blocklist for X25519); p = 2^255 - 19
    val lowOrder = Vector(
      "0000000000000000000000000000000000000000000000000000000000000000", // u = 0
      "0100000000000000000000000000000000000000000000000000000000000000", // u = 1
      "e0eb7a7c3b41b8ae1656e3faf19fc46ada098deb9c32b1fd866205165f49b800", // order 8
      "5f9c95bca3508c24b1d0b1559c83ef5b04445cc4581c8e86d8224eddd09f1157", // order 8
      "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f", // p - 1 (order 2)
      "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f", // p (= 0 mod p)
      "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"  // p + 1 (= 1 mod p)
    )
    lowOrder.foreach(u => assertEquals(X25519.agree(priv, hex(u)), Left(SealError.LowOrderPoint), u))
    val recipients = lowOrder.map(u => RecipientKey.fromRaw(hex(u)).toOption.get)
    recipients.foreach(r => assertEquals(Sealer.seal(r, 0, "x", Array[Byte](1), SealEntropy.secure).left.toOption, Some(SealError.LowOrderPoint)))
  }

  test("the ephemeral private key is zeroed when sealing fails and after a store write, whatever the outcome") {
    final class Recording extends SealEntropy:
      val draws = scala.collection.mutable.ArrayBuffer.empty[SealDraw]
      def draw(kind: Int, name: String): SealDraw = synchronized { val d = SealEntropy.secure.draw(kind, name); draws += d; d }
    val bad = RecipientKey.fromRaw(new Array[Byte](32)).toOption.get
    val e1 = new Recording
    assert(Sealer.seal(bad, 0, "x", Array[Byte](1), e1).isLeft)
    assertEquals(e1.draws.length, 1)
    assert(e1.draws.head.ephemeralPrivate.forall(_ == 0), "key left in memory after a failed seal")
    val ok = Sealer.seal(owner.recipient, 0, "x", Array[Byte](1), new Recording).toOption.get
    assert(!ok.ephemeralPrivate.forall(_ == 0), "a successful seal hands the key to the caller, who zeroes it")
    val dir = Files.createTempDirectory("phrf-s7-zero-")
    val throwing = new StoreHook:
      override def corruptBlob(b: Array[Byte]): Array[Byte] = throw new IllegalStateException("hook")
    val e2 = new Recording
    val st = SealedStore.open(dir.resolve("s"), owner.recipient, owner.fingerprint, e2, throwing).toOption.get
    intercept[IllegalStateException](st.append("y", Array[Byte](2)))
    val e3 = new Recording
    val st3 = SealedStore.open(dir.resolve("t"), owner.recipient, owner.fingerprint, e3).toOption.get
    assert(st3.append("z", Array[Byte](3)).isRight)
    (e2.draws ++ e3.draws).foreach(d => assert(d.ephemeralPrivate.forall(_ == 0), "key left in memory after a store write"))
  }

  test("bucket sizes: 4 KiB floor, powers of two to 1 GiB, GiB multiples above") {
    assertEquals(SealFormat.bucket(0), 4096L)
    assertEquals(SealFormat.bucket(4096), 4096L)
    assertEquals(SealFormat.bucket(4097), 8192L)
    assertEquals(SealFormat.bucket(8192), 8192L)
    assertEquals(SealFormat.bucket(8193), 16384L)
    assertEquals(SealFormat.bucket(1L << 30), 1L << 30)
    assertEquals(SealFormat.bucket((1L << 30) + 1), 2L << 30)
    assertEquals(SealFormat.bucket(2L * (1L << 30) + 1), 3L << 30)
  }

  test("logical names: traversal, NUL, backslash, empty components, length and lone surrogates are refused") {
    Seq("", "/a", "a//b", "a/./b", "a/../b", "a\\b", "a\u0000b", "a/", "..", "a" * 1025, "x\ud800y").foreach(n => assert(!SealFormat.validName(n), n))
    Seq("a", "dataset/C-TS-1/0007", "a" * 1024, "é/ü").foreach(n => assert(SealFormat.validName(n), n))
  }

  private val fixture = ujson.read(Files.readString(Repo.custody.resolve("tests/fixtures/sealed_format_vectors.json")))
  private val recipPriv = hex(fixture("recipient_private_hex").str)
  private val owner = new TestOwner(recipPriv)

  private def caseData(id: String, c: ujson.Value): Array[Byte] = id match
    case "pad-exact-4096" => Array.tabulate[Byte](4084)(i => (i % 251).toByte)
    case "pad-next-8192" => Array.tabulate[Byte](4085)(i => (i % 251).toByte)
    case _ => hex(c("data_hex").str)

  test("golden vectors: key material and every blob reproduce byte for byte with the injected key and nonce") {
    assertEquals(Fs.hex(owner.pub), fixture("recipient_public_hex").str)
    val eph = hex(fixture("ephemeral_private_hex").str)
    assertEquals(Fs.hex(X25519.publicOf(eph)), fixture("ephemeral_public_hex").str)
    val shared = X25519.agree(eph, owner.pub).toOption.get
    assertEquals(Fs.hex(shared), fixture("shared_secret_hex").str)
    assertEquals(Fs.hex(Hkdf.sha256(shared, Array.emptyByteArray, SealFormat.Info ++ owner.pub ++ X25519.publicOf(eph), 32)), fixture("aead_key_hex").str)
    val entropy = new FixedEntropy(eph, hex(fixture("nonce_hex").str))
    fixture("cases").arr.foreach { c =>
      val id = c("id").str
      val data = caseData(id, c)
      val sb = Sealer.seal(owner.recipient, c("kind").num.toInt, c("name").str, data, entropy).toOption.get
      assertEquals(sb.blob.length, c("blob_len").num.toInt, id)
      assertEquals(Fs.sha256(sb.blob), c("blob_sha256").str, id)
      assertEquals(Fs.hex(sb.blob.take(SealFormat.HeaderLen)), c("aad_hex").str, id)
    }
  }

  test("golden vectors: the Python-sealed blobs decrypt on the JVM with the recipient private key") {
    val c = fixture("cases").arr.find(_("id").str == "data-small").get
    val opened = OwnerReader.decrypt(recipPriv, hex(c("blob_hex").str)).toOption.get
    assert(opened.matches(0, "dataset/C-TS-1/0007", hex(c("data_hex").str)))
    // the other four are reproduced from inputs above; decrypt those reproductions too
    val entropy = new FixedEntropy(hex(fixture("ephemeral_private_hex").str), hex(fixture("nonce_hex").str))
    fixture("cases").arr.foreach { cc =>
      val data = caseData(cc("id").str, cc)
      val blob = Sealer.seal(owner.recipient, cc("kind").num.toInt, cc("name").str, data, entropy).toOption.get.blob
      assert(OwnerReader.decrypt(recipPriv, blob).toOption.get.matches(cc("kind").num.toInt, cc("name").str, data))
    }
  }

  test("tampering with header or ciphertext, or a different recipient, fails authentication") {
    val blob = Sealer.seal(owner.recipient, 0, "x", "hello".getBytes(UTF_8), SealEntropy.secure).toOption.get.blob
    Seq(10, 45, 55, 70, blob.length - 1).foreach { i =>
      val t = blob.clone()
      t(i) = (t(i) ^ 1).toByte
      assert(OwnerReader.decrypt(recipPriv, t).isLeft, s"flip at $i")
    }
    assert(OwnerReader.decrypt(TestOwner.random().priv, blob).isLeft)
  }

  test("each blob uses a fresh ephemeral key and nonce") {
    val a = Sealer.seal(owner.recipient, 0, "x", "same".getBytes(UTF_8), SealEntropy.secure).toOption.get.blob
    val b = Sealer.seal(owner.recipient, 0, "x", "same".getBytes(UTF_8), SealEntropy.secure).toOption.get.blob
    assertNotEquals(Fs.hex(a.slice(8, 40)), Fs.hex(b.slice(8, 40)))
    assertNotEquals(Fs.hex(a.slice(48, 60)), Fs.hex(b.slice(48, 60)))
  }

  test("recipient key parsing: hex, PEM, fingerprint check, bad inputs") {
    val rk = owner.recipient
    assertEquals(RecipientKey.parse(Fs.hex(owner.pub)).toOption.map(_.fingerprint), Some(rk.fingerprint))
    val der = hex("302a300506032b656e032100") ++ owner.pub
    val pem = "-----BEGIN PUBLIC KEY-----\n" + java.util.Base64.getEncoder.encodeToString(der) + "\n-----END PUBLIC KEY-----\n"
    assertEquals(RecipientKey.parse(pem).toOption.map(_.fingerprint), Some(rk.fingerprint))
    assert(rk.checkFingerprint(rk.fingerprint).isRight)
    assert(rk.checkFingerprint(rk.fingerprint.replace(" ", "").toUpperCase).isRight)
    assertEquals(rk.checkFingerprint("0000"), Left(SealError.FingerprintMismatch))
    assert(RecipientKey.parse("zz").isLeft)
  }
