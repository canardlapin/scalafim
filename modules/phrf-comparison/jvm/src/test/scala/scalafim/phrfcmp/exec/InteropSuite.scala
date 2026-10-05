package scalafim.phrfcmp.exec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/** JVM <-> Python interop for phrf-sealed/1 and the root pipe (format spec sections 7 and 8). */
class InteropSuite extends munit.FunSuite:

  private val readStore =
    """import sys, json
from phrf_custody import seal
from cryptography.hazmat.primitives.asymmetric.x25519 import X25519PrivateKey
priv = X25519PrivateKey.from_private_bytes(bytes.fromhex(sys.argv[2]))
partial = len(sys.argv) > 4 and sys.argv[4] == "partial"
out = {n: d.hex() for n, d in seal.read_blobs(sys.argv[1], priv, allow_partial=partial, expect_tree_digest=sys.argv[3])}
print("RESULT " + json.dumps(out, sort_keys=True))
"""

  private def tmp(): Path = Files.createTempDirectory("phrf-s7-interop-")

  private def pyRead(py: String, dir: Path, owner: TestOwner, partial: Boolean = false): Map[String, String] =
    val (rc, out) = Py.run(py, "-c", readStore, dir.toString, Fs.hex(owner.priv), SealedStore.digest(dir), if partial then "partial" else "full")
    assertEquals(rc, 0, out)
    ujson.read(out.linesIterator.find(_.startsWith("RESULT ")).get.drop(7)).obj.map((k, v) => k -> v.str).toMap

  test("blobs sealed by the JVM unseal with the Python reader (random keys, buckets, duplicates, tmp debris, CLOSE)") {
    val py = Py.exe
    val owner = TestOwner.random(); val dir = tmp()
    val crash = new StoreHook:
      override def at(s: StoreStage, n: String): Unit = if s == StoreStage.TmpWritten && n == "crash" then throw new Boom
    val s = SealedStore.open(dir, owner.recipient, owner.fingerprint, hook = crash).toOption.get
    val payloads = Map(
      "dataset/C-TS-1/0007" -> "phrf interop plaintext".getBytes(UTF_8),
      "empty" -> Array.emptyByteArray,
      "pad4084" -> Array.tabulate[Byte](4084)(i => (i % 251).toByte),
      "pad4085" -> Array.tabulate[Byte](4085)(i => (i % 251).toByte),
      "big" -> Array.tabulate[Byte](300000)(i => (i * 7).toByte),
      "uni/é/名" -> Array[Byte](0, 1, 2, -1)
    )
    payloads.foreach((n, d) => assert(s.append(n, d).isRight, n))
    s.append("empty", Array.emptyByteArray) // identical duplicate
    intercept[Boom](s.append("crash", Array[Byte](1))) // leaves a tmp file the Python reader must ignore
    assertEquals(SealedStore.listTmp(dir.resolve("blobs")).length, 1)
    assertEquals(pyRead(py, dir, owner, partial = true), payloads.map((k, v) => k -> Fs.hex(v)))
    val s2 = SealedStore.open(dir, owner.recipient, owner.fingerprint).toOption.get
    s2.close().fold(e => fail(e.message), identity)
    assertEquals(pyRead(py, dir, owner), payloads.map((k, v) => k -> Fs.hex(v)))
  }

  test("Python-sealed stores (SealedStore.append and close, random key) unseal on the JVM") {
    val py = Py.exe
    val owner = TestOwner.random(); val dir = tmp()
    val script =
      """import sys
from pathlib import Path
from phrf_custody import seal
pub = seal.load_public_key(sys.argv[2].encode())
st = seal.SealedStore(Path(sys.argv[1]), pub, expect_fingerprint=seal.fingerprint(pub))
st.append("a/b", b"python wrote this")
st.append("pad", bytes(i % 251 for i in range(5000)))
st.append("empty", b"")
st.close()
"""
    val (rc, out) = Py.run(py, "-c", script, dir.toString, Fs.hex(owner.pub))
    assertEquals(rc, 0, out)
    val items = OwnerReader.readAll(dir, owner.priv).fold(e => fail(e), identity)
    assertEquals(new String(items("a/b"), UTF_8), "python wrote this")
    assertEquals(items("pad").toSeq, Array.tabulate[Byte](5000)(i => (i % 251).toByte).toSeq)
    assertEquals(items("empty").length, 0)
  }

  test("a store the JVM writes verifies against the Python tree digest (digest listing is byte-compatible)") {
    val py = Py.exe
    val owner = TestOwner.random(); val dir = tmp()
    val s = SealedStore.open(dir, owner.recipient, owner.fingerprint).toOption.get
    s.append("x", "1".getBytes(UTF_8)); s.append("y/z", "2".getBytes(UTF_8))
    val receipt = s.close().toOption.get
    val (rc, out) = Py.run(py, "-c", "import sys; from pathlib import Path; from phrf_custody import seal; print('DIGEST', seal.sealed_digest(Path(sys.argv[1])))", dir.toString)
    assertEquals(rc, 0, out)
    assertEquals(out.linesIterator.find(_.startsWith("DIGEST ")).get.drop(7), receipt.sealedTreeDigest)
  }

  private def jvmClasspath: String =
    def loc(c: Class[?]): String = java.nio.file.Paths.get(c.getProtectionDomain.getCodeSource.getLocation.toURI).toString
    Seq(loc(classOf[RootIntake.type]), loc(classOf[RootIntakeProbe.type]), loc(classOf[scala.Predef.type]), loc(classOf[scala.runtime.Scala3RunTime.type])).distinct.mkString(java.io.File.pathSeparator)

  private val launchScript =
    """import sys, json
from phrf_custody import session
root = int(sys.argv[1])
rc = session.launch(sys.argv[3:], root, ack_timeout=float(sys.argv[2]))
print("RC", rc)
"""

  private def launch(py: String, root: String, report: Path, timeout: Double = 120.0): (Int, String) =
    val java = Path.of(sys.props("java.home"), "bin", "java").toString
    Py.run(py, "-c", launchScript, root, timeout.toString, java, "-cp", jvmClasspath, "scalafim.phrfcmp.exec.RootIntakeProbe", root, report.toString)

  test("root pipe: a real JVM runner started by the Python session.launch reads root64, acknowledges it, and leaves no children") {
    val py = Py.exe
    val report = tmp().resolve("report.txt")
    val (rc, out) = launch(py, "9876543210123456789", report)
    assert(out.contains("RC 0"), s"session.launch did not accept the runner: $out")
    val kv = Files.readString(report).linesIterator.map(l => l.split("=", 2)).map(a => a(0) -> a(1)).toMap
    assertEquals(rc, 0, out)
    assertEquals(kv("root_matches"), "true")
    assertEquals(kv("redacted"), "true")
    assertEquals(kv("children_after_intake"), "0")
    assertEquals(kv("child_env"), "unset/unset")
    assertEquals(kv("parent_env_had_vars"), "true")
    assert(!Files.readString(report).contains("9876543210123456789"), "the report must not contain the root")
  }

  test("root pipe: the maximum unsigned root64 round-trips through the real session") {
    val py = Py.exe
    val report = tmp().resolve("report.txt")
    val (_, out) = launch(py, "18446744073709551615", report)
    assert(out.contains("RC 0"), out)
    assert(Files.readString(report).contains("root_matches=true"))
  }
