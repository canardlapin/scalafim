package scalafim.archive.io

import java.nio.file.Files
import java.nio.charset.StandardCharsets.UTF_8

class LocalObjectStoreSuite extends munit.FunSuite:
  private def right[A](value: Either[LocalStoreError, A]): A = value.fold(e => fail(e.message), identity)
  private def store() = right(LocalObjectStore.open(Files.createTempDirectory("scalafim-objects-"), 113))

  test("streamed publication has independent SHA256 and never replaces an immutable name") {
    val s = store()
    val ref = right(s.write("objects/a.bin")(_.write("abc".getBytes(UTF_8))))
    assertEquals(ref.bytes, 3L)
    assertEquals(ref.digest.value, "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    assertEquals(s.verify(ref), Right(()))
    assert(s.write("objects/a.bin")(_.write(Array[Byte](9))).isLeft)
    assertEquals(s.verify(ref), Right(()))
  }

  test("failed producers expose no immutable destination") {
    val s = store()
    assert(s.write("objects/incomplete") { out =>
      out.write(Array[Byte](1, 2, 3))
      throw new java.io.IOException("injected failure")
    }.isLeft)
    assert(!Files.exists(s.root.resolve("objects/incomplete")))
  }

  test("external staged writers require store ownership and remove their writable alias") {
    val s = store()
    val stage = right(s.stage(".nii"))
    Files.write(stage.path, Array[Byte](1, 2, 3))
    assert(store().publishStaged(stage, "foreign.nii").isLeft)
    val ref = right(s.publishStaged(stage, "objects/image.nii"))
    assert(!Files.exists(stage.path))
    assertEquals(s.verify(ref), Right(()))
  }

  test("pointer CAS rejects stale publishers and leaves their base intact") {
    val s = store()
    val first = right(s.compareAndSwapPointer("current.json", None, "first".getBytes(UTF_8)))
    assert(s.compareAndSwapPointer("current.json", None, "lost".getBytes(UTF_8)).isLeft)
    assertEquals(s.verify(first), Right(()))
    val second = right(s.compareAndSwapPointer("current.json", Some(first.digest), "second".getBytes(UTF_8)))
    assert(s.compareAndSwapPointer("current.json", Some(first.digest), "lost".getBytes(UTF_8)).isLeft)
    assertEquals(s.verify(second), Right(()))
  }

  test("unsafe paths, symlink escapes and tampering are rejected") {
    val s = store()
    for path <- Vector("../escape", "/absolute", "a//b", "a/./b", "a\\b") do
      assert(s.write(path)(_.write(1)).isLeft)
    val external = Files.createTempDirectory("scalafim-external-")
    Files.createSymbolicLink(s.root.resolve("link"), external)
    assert(s.write("link/escape")(_.write(1)).isLeft)
    val ref = right(s.write("payload")(_.write(1)))
    Files.write(s.root.resolve("payload"), Array[Byte](2))
    assert(s.verify(ref).isLeft)
  }
