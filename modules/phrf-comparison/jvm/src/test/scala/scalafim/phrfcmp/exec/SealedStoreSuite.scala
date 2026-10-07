package scalafim.phrfcmp.exec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

final class Boom extends RuntimeException("boom")

class SealedStoreSuite extends munit.FunSuite:

  private val owner = TestOwner.random()
  private def tmp(): Path = Files.createTempDirectory("phrf-s7-store-")
  private def open(dir: Path, hook: StoreHook = StoreHook.none, entropy: SealEntropy = SealEntropy.secure): SealedStore =
    SealedStore.open(dir, owner.recipient, owner.fingerprint, entropy, hook).fold(e => fail(e.message), identity)
  private def bytes(s: String): Array[Byte] = s.getBytes(UTF_8)
  private def blobFiles(dir: Path): Vector[Path] = SealedStore.listBlobs(dir.resolve("blobs"))

  test("append seals every blob; file name is the hash of the whole file; reader recovers names and data") {
    val dir = tmp(); val s = open(dir)
    val h = s.append("a/b", bytes("payload")).toOption.get
    assertEquals(Files.exists(dir.resolve(s"blobs/$h.enc")), true)
    assertEquals(Fs.sha256File(dir.resolve(s"blobs/$h.enc")), h)
    s.close().fold(e => fail(e.message), identity)
    assertEquals(OwnerReader.readAll(dir, owner.priv).toOption.get.map((k, v) => k -> new String(v, UTF_8)), Map("a/b" -> "payload"))
  }

  test("nothing result-bearing reaches disk in plaintext, including temp files") {
    val dir = tmp()
    val seen = new java.util.concurrent.atomic.AtomicInteger(0)
    val needle = bytes("PLANTED-RESULT-0.123456")
    val hook = new StoreHook:
      override def at(stage: StoreStage, name: String): Unit =
        if stage == StoreStage.TmpWritten then
          Fs.listFiles(dir).foreach { p =>
            if indexOf(Files.readAllBytes(p), needle) >= 0 then seen.incrementAndGet(): Unit
          }
    val s = open(dir, hook)
    s.append("x", needle)
    s.close()
    assertEquals(seen.get(), 0)
    Fs.listFiles(dir).foreach(p => assertEquals(indexOf(Files.readAllBytes(p), needle), -1, p.toString))
  }

  private def indexOf(hay: Array[Byte], needle: Array[Byte]): Int =
    (0 to hay.length - needle.length).find(i => java.util.Arrays.equals(hay, i, i + needle.length, needle, 0, needle.length)).getOrElse(-1)

  test("read-back: a corrupted ciphertext is refused and nothing is published") {
    val dir = tmp()
    val hook = new StoreHook:
      override def corruptBlob(b: Array[Byte]): Array[Byte] = { val c = b.clone(); c(100) = (c(100) ^ 1).toByte; c }
    val s = open(dir, hook)
    assertEquals(s.append("x", bytes("d")).left.toOption.map(_.isInstanceOf[SealError.ReadBackMismatch]), Some(true))
    assertEquals(Fs.listFiles(dir.resolve("blobs")), Vector.empty[Path])
  }

  test("read-back: a blob that decrypts to different plaintext is refused") {
    val dir = tmp()
    val sealedOther = Sealer.seal(owner.recipient, 0, "x", bytes("other"), SealEntropy.secure).toOption.get
    val hook = new StoreHook:
      override def corruptBlob(b: Array[Byte]): Array[Byte] = sealedOther.blob
    assert(open(dir, hook).append("x", bytes("d")).isLeft)
    assertEquals(Fs.listFiles(dir.resolve("blobs")), Vector.empty[Path])
  }

  test("read-back from disk: a file that differs after the durable write is removed and refused") {
    val dir = tmp()
    val hook = new StoreHook:
      override def corruptDisk(p: Path): Unit = { Files.write(p, Array[Byte](1, 2, 3)); () }
    assertEquals(open(dir, hook).append("x", bytes("d")).left.toOption, Some(SealError.ReadBackMismatch("disk")))
    assertEquals(blobFiles(dir), Vector.empty[Path])
  }

  test("crash between tmp write and rename leaves only ciphertext tmp debris; readers and digest ignore it; close deletes it") {
    val dir = tmp()
    val crash = new StoreHook:
      override def at(stage: StoreStage, name: String): Unit = if stage == StoreStage.TmpWritten && name == "b" then throw new Boom
    val s = open(dir, crash)
    s.append("a", bytes("1"))
    val digestBefore = SealedStore.digest(dir)
    intercept[Boom](s.append("b", bytes("2")))
    assertEquals(SealedStore.listTmp(dir.resolve("blobs")).length, 1)
    assertEquals(SealedStore.digest(dir), digestBefore)
    assert(OwnerReader.readAll(dir, owner.priv, allowPartial = true).isRight)
    val s2 = open(dir)
    s2.append("b", bytes("2"))
    s2.close().fold(e => fail(e.message), identity)
    assertEquals(SealedStore.listTmp(dir.resolve("blobs")), Vector.empty[Path])
    assertEquals(OwnerReader.readAll(dir, owner.priv).toOption.get.keySet, Set("a", "b"))
  }

  test("resume re-seals: identical duplicates are accepted, differing duplicates are refused by the reader") {
    val dir = tmp(); val s = open(dir)
    s.append("n", bytes("same")); s.append("n", bytes("same"))
    s.close()
    assertEquals(OwnerReader.readAll(dir, owner.priv).toOption.map(_.keySet), Some(Set("n")))
    assertEquals(blobFiles(dir).length, 3) // two data copies and the CLOSE record
    val dir2 = tmp(); val t = open(dir2)
    t.append("n", bytes("one")); t.append("n", bytes("two"))
    t.close()
    assertEquals(OwnerReader.readAll(dir2, owner.priv).left.toOption, Some("logical name sealed twice with different plaintext"))
  }

  test("close: CLOSE counts every data-blob file, SEALED receipt matches the digest, a second close and later appends refuse") {
    val dir = tmp(); val s = open(dir)
    (1 to 3).foreach(i => s.append(s"k$i", bytes(s"v$i")))
    val r = s.close().toOption.get
    assertEquals(r.sealedTreeDigest, SealedStore.digest(dir))
    assertEquals(r.recipientFp, owner.fingerprint)
    assertEquals(s.close(), Left(SealError.StoreClosed))
    assertEquals(s.append("z", bytes("z")), Left(SealError.StoreClosed))
    assert(SealedStore.open(dir, owner.recipient, owner.fingerprint).isLeft)
    // deleting one copy is detected
    Files.delete(blobFiles(dir).head)
    assert(OwnerReader.readAll(dir, owner.priv).isLeft)
  }

  test("a wrong fingerprint refuses before any byte is written") {
    val dir = tmp()
    assertEquals(SealedStore.open(dir, owner.recipient, "00" * 32).left.toOption, Some(SealError.FingerprintMismatch))
    assert(!Files.exists(dir.resolve("blobs")))
  }

  test("size buckets: ciphertext length reveals only the bucket") {
    val dir = tmp(); val s = open(dir)
    val sizes = Seq((0, "a"), (10, "b"), (4000, "c"), (4084, "d")).map { (n, name) =>
      val h = s.append(name, new Array[Byte](n)).toOption.get
      Files.size(dir.resolve(s"blobs/$h.enc"))
    }
    assertEquals(sizes.distinct, Seq(4172L))
    val big = s.append("big", new Array[Byte](4085)).toOption.get
    assertEquals(Files.size(dir.resolve(s"blobs/$big.enc")), 8268L)
  }

  test("sealed-tree digest is sensitive to one byte and excludes SEALED and tmp files") {
    val dir = tmp(); val s = open(dir)
    s.append("a", bytes("1"))
    val d0 = SealedStore.digest(dir)
    Files.writeString(dir.resolve("blobs/x.enc.tmp"), "junk")
    Files.writeString(dir.resolve("SEALED"), "{}")
    assertEquals(SealedStore.digest(dir), d0)
    val f = blobFiles(dir).head
    val b = Files.readAllBytes(f); b(70) = (b(70) ^ 1).toByte; Files.write(f, b)
    assertNotEquals(SealedStore.digest(dir), d0)
  }

  test("concurrent appends from many threads all verify and unseal") {
    val dir = tmp(); val s = open(dir)
    val ts = (0 until 8).map(t => new Thread(() => (0 until 10).foreach(i => assert(s.append(s"t$t/i$i", bytes(s"$t-$i")).isRight))))
    ts.foreach(_.start()); ts.foreach(_.join())
    s.close().fold(e => fail(e.message), identity)
    assertEquals(OwnerReader.readAll(dir, owner.priv).toOption.get.size, 80)
  }
