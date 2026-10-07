package scalafim.phrfcmp.exec

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.security.MessageDigest

class RootIntakeSuite extends munit.FunSuite:

  private final class FakeIo(line: Array[Byte], failAckWrite: Boolean = false) extends FdIo:
    val events = scala.collection.mutable.ArrayBuffer.empty[String]
    val ack = new ByteArrayOutputStream
    def input(fd: Int): Either[IntakeError, InputStream] =
      Right(new ByteArrayInputStream(line):
        override def read(b: Array[Byte], off: Int, len: Int): Int = { events += s"read$fd"; super.read(b, off, len) }
        override def close(): Unit = events += s"close$fd")
    def output(fd: Int): Either[IntakeError, OutputStream] =
      Right(new OutputStream:
        def write(b: Int): Unit = write(Array(b.toByte), 0, 1)
        override def write(b: Array[Byte], off: Int, len: Int): Unit =
          events += s"ack$fd"
          if failAckWrite then throw new java.io.IOException("broken")
          ack.write(b, off, len)
        override def close(): Unit = events += s"close$fd")

  private val env = Map(RootIntake.RootVar -> "7", RootIntake.AckVar -> "9")
  private def sha(b: Array[Byte]): String = Fs.hex(MessageDigest.getInstance("SHA-256").digest(b))

  test("order: read once, close the root fd, write the ack over the exact line bytes (with newline), close the ack fd") {
    val line = "123456789012345\n".getBytes(UTF_8)
    val io = new FakeIo(line)
    val r = RootIntake.receive(env, io).toOption.get
    assertEquals(r.root.value, 123456789012345L)
    assertEquals(io.events.toVector, Vector("read7", "close7", "ack9", "close9"))
    assertEquals(new String(io.ack.toByteArray, UTF_8), sha(line))
    assertEquals(r.ackHex, sha(line))
    assertEquals(io.ack.size(), 64)
    assertNotEquals(sha(line), sha("123456789012345".getBytes(UTF_8)), "the newline is part of the hashed bytes")
  }

  test("unsigned 64-bit roots are accepted; the root is redacted in toString") {
    val r = RootIntake.receive(env, new FakeIo("18446744073709551615\n".getBytes(UTF_8))).toOption.get
    assertEquals(r.root.value, -1L)
    assert(!r.root.toString.contains("1844"))
    assert(!r.toString.contains("1844"))
  }

  test("a bad line is refused and nothing is acknowledged") {
    Seq("12x\n", "12", "\n", "99999999999999999999999\n", "-1\n").foreach { l =>
      val io = new FakeIo(l.getBytes(UTF_8))
      assertEquals(RootIntake.receive(env, io).left.toOption, Some(IntakeError.BadLine), l)
      assertEquals(io.ack.size(), 0)
      assert(io.events.contains("close7"), "the root fd is still closed")
    }
  }

  test("missing or unusable descriptor variables are refused; stdio descriptors are never touched") {
    assertEquals(RootIntake.receive(Map.empty, new FakeIo(Array.emptyByteArray)).left.toOption, Some(IntakeError.MissingVariable(RootIntake.RootVar)))
    assertEquals(RootIntake.receive(Map(RootIntake.RootVar -> "7"), new FakeIo(Array.emptyByteArray)).left.toOption, Some(IntakeError.MissingVariable(RootIntake.AckVar)))
    assertEquals(RootIntake.receive(Map(RootIntake.RootVar -> "1", RootIntake.AckVar -> "9"), new FakeIo(Array.emptyByteArray)).left.toOption, Some(IntakeError.BadDescriptor(RootIntake.RootVar)))
    assertEquals(RootIntake.receive(Map(RootIntake.RootVar -> "x", RootIntake.AckVar -> "9"), new FakeIo(Array.emptyByteArray)).left.toOption, Some(IntakeError.BadDescriptor(RootIntake.RootVar)))
  }

  test("a failing ack write is an error and the ack descriptor is still closed") {
    val io = new FakeIo("5\n".getBytes(UTF_8), failAckWrite = true)
    assertEquals(RootIntake.receive(env, io).left.toOption, Some(IntakeError.AckFailed))
    assert(io.events.contains("close9"))
  }

  test("children never see the pipe variables") {
    val pb = new ProcessBuilder("sh", "-c", "echo ${PHRF_ROOT_FD-unset}/${PHRF_ROOT_ACK_FD-unset}").redirectErrorStream(true)
    pb.environment().put(RootIntake.RootVar, "7")
    pb.environment().put(RootIntake.AckVar, "9")
    val p = ChildEnvironment.scrub(pb).start()
    assertEquals(new String(p.getInputStream.readAllBytes(), UTF_8).trim, "unset/unset")
    p.waitFor(): Unit
  }

  test("the source never writes the root: no file or log call receives it") {
    val src = Files.readString(Repo.root.resolve("modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/RootIntake.scala"))
    assert(!src.contains("println") && !src.contains("System.err") && !src.contains("Files.write"))
  }
