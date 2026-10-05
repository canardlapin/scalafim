package scalafim.phrfcmp.ingest

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import scalafim.phrfcmp.ingest.IngestRefusal.{ArrayHashMismatch, CellMismatch, HitFlagMismatch, NpzHashMismatch, RootKindMismatch, SeedMismatch, Unreadable}

/** Fixtures produced by `tools/phrf-comparison/generator` under the HARNESS root (see the module README). */
class RealGeneratorSuite extends munit.FunSuite:

  private val GeneratorSha = "b4ba7ac95cc226b7f4132167e67acab885543a0bd157c985bfb51bfd286d3edf"
  private val HarnessRoot = "7a3c91d50b44e2f1"

  private val trialExp = IngestExpectation(
    RootKind.Harness, HarnessRoot, GeneratorSha, CellSpec("T-TX-fast", CellKind.Trial, 3, 2, 1.0, true), 0, Seeds.ProtocolDenylist
  )
  private val condExp = IngestExpectation(
    RootKind.Harness, HarnessRoot, GeneratorSha, CellSpec("C-TX-.5", CellKind.Condition, 3, 0, 1.0, false), 0, Seeds.ProtocolDenylist
  )

  private def fixtureDir: Path =
    val u = getClass.getClassLoader.getResource("fixtures/T-TX-fast__d0000.npz")
    Paths.get(u.toURI).getParent

  private def bytes(stem: String, ext: String): Array[Byte] = Files.readAllBytes(fixtureDir.resolve(s"$stem.$ext"))
  private def manifestText(stem: String): String = new String(bytes(stem, "manifest.json"), StandardCharsets.UTF_8)

  /** Replace exactly one occurrence, so a tamper cannot silently miss. */
  private def patch(text: String, from: String, to: String): String =
    assertEquals(text.split(java.util.regex.Pattern.quote(from), -1).length, 2, s"'$from' must occur exactly once")
    text.replace(from, to)

  test("real trial fixture binds; pool, deviations and events are present"):
    val b = PhrfDatasetLoader.load(fixtureDir, "T-TX-fast__d0000", trialExp).fold(r => fail(r.message), identity)
    assertEquals((b.fit.y.rows, b.fit.y.cols), (3, 600))
    assertEquals(b.fit.yPool.map(m => (m.rows, m.cols)), Some((2, 600)))
    assertEquals((b.fit.nuisance.rows, b.fit.nuisance.cols), (600, 24))
    assertEquals(b.fit.evOnset.length, 144)
    assertEquals(b.truth.trialBeta.map(m => (m.rows, m.cols)), Some((3, 144)))
    assertEquals(b.truth.truthKernel.cols, 481)
    assertEquals(b.arrayHashes.size, 22)
    assertEquals(b.inputSha256, "3b5cb36b65bc5ef8d30455b9522d74a0982014f651fcda513e9812123d86cd0f")
    assertEqualsDouble(b.fit.sampleTime(5), 5.0, 0.0)
    assertEquals(b.fit.runId(0), 0)

  test("real condition fixture binds without pool or trial arrays"):
    val b = PhrfDatasetLoader.load(fixtureDir, "C-TX-.5__d0000", condExp).fold(r => fail(r.message), identity)
    assertEquals(b.fit.yPool, None)
    assertEquals(b.truth.trialBeta, None)
    assertEquals(b.fit.evStim.forall(_ == -1), true)

  test("real file bound against a pilot expectation is refused"):
    val r = PhrfDatasetLoader.load(fixtureDir, "T-TX-fast__d0000", trialExp.copy(rootKind = RootKind.Pilot))
    assertEquals(r, Left(RootKindMismatch(RootKind.Pilot, "harness")))

  test("real file: wrong cell spec flags (tr_aligned_onsets) are refused"):
    val r = PhrfDatasetLoader.load(fixtureDir, "T-TX-fast__d0000", trialExp.copy(cell = trialExp.cell.copy(trAlignedOnsets = false)))
    assertEquals(r, Left(CellMismatch("tr_aligned_onsets", "false", "true")))

  test("real file: byte flips anywhere are refused (hash, then CRC if re-signed)"):
    val npz = bytes("T-TX-fast__d0000", "npz")
    val m = manifestText("T-TX-fast__d0000")
    val sha = Digests.sha256Hex(npz)
    for pos <- Seq(0, 40, npz.length / 2, npz.length - 30) do
      val bad = npz.clone()
      bad(pos) = (bad(pos) ^ 0x10).toByte
      val r = PhrfDatasetBinding.bind(bad, m, trialExp)
      assert(r.left.exists(_.isInstanceOf[NpzHashMismatch]), s"pos $pos: $r")
      val r2 = PhrfDatasetBinding.bind(bad, patch(m, sha, Digests.sha256Hex(bad)), trialExp)
      assert(r2.isLeft, s"re-signed flip at $pos accepted")

  test("real file: re-zipped with DEFLATE (manifest re-signed) is refused"):
    val npz = bytes("T-TX-fast__d0000", "npz")
    val src = Npz.parse(npz).fold(e => fail(e.message), identity)
    val bo = new ByteArrayOutputStream()
    val zo = new ZipOutputStream(bo)
    src.members.foreach { mem =>
      zo.putNextEntry(new ZipEntry(mem.name + ".npy"))
      zo.write(mem.rawNpy)
      zo.closeEntry()
    }
    zo.close()
    val z = bo.toByteArray
    val m = patch(manifestText("T-TX-fast__d0000"), Digests.sha256Hex(npz), Digests.sha256Hex(z))
    val r = PhrfDatasetBinding.bind(z, m, trialExp)
    assert(r.left.exists(_.isInstanceOf[IngestRefusal.Npz]), r.toString)

  test("real file: tampered stream seed digits (above 2^53) and flipped hit flag are refused"):
    val npz = bytes("T-TX-fast__d0000", "npz")
    val m = manifestText("T-TX-fast__d0000")
    // truth seed 3236142379493101838 appears twice (streams and stream_denylist_check).
    val bad = m.replace("3236142379493101838", "3236142379493101839")
    assertEquals(
      PhrfDatasetBinding.bind(npz, bad, trialExp),
      Left(SeedMismatch("truth", "3236142379493101838", "3236142379493101839"))
    )
    val flipped = m.replaceFirst("(\"truth\": \\{\\s*\"hit\": )false", "$1true")
    assertEquals(PhrfDatasetBinding.bind(npz, flipped, trialExp), Left(HitFlagMismatch("truth", true, false)))

  test("real file: per-array hash tamper is refused by name"):
    val npz = bytes("T-TX-fast__d0000", "npz")
    val m = ujson.read(manifestText("T-TX-fast__d0000")).obj
    val hash = m("arrays")("trial_beta")("npy_sha256").str
    assert(PhrfDatasetBinding.bind(npz, patch(manifestText("T-TX-fast__d0000"), hash, "0" * 64), trialExp).left.exists {
      case ArrayHashMismatch("trial_beta", _, _) => true
      case _                                     => false
    })

  test("missing files are a typed refusal"):
    assert(PhrfDatasetLoader.load(fixtureDir, "nope", trialExp).left.exists(_.isInstanceOf[Unreadable]))

  test("fixtures are byte-identical to a fresh generator run (skipped without python3+numpy)"):
    def findGen(p: Path): Option[Path] =
      if p == null then None
      else
        val g = p.resolve("tools/phrf-comparison/generator")
        if Files.isDirectory(g) then Some(g) else findGen(p.getParent)
    val gen = findGen(Paths.get(sys.props("user.dir")).toAbsolutePath)
    assume(gen.isDefined, "generator directory not found")
    val out = Files.createTempDirectory("phrf-s1-regen")
    val script =
      s"""import sys, dataclasses
         |sys.path.insert(0, '${gen.get}')
         |from phrf_gen.cells import CELLS
         |from phrf_gen.io import write_dataset
         |from phrf_gen.seeds import HARNESS_ROOT
         |for cid, kw in [('T-TX-fast', dict(n_voxels=3, n_pool=2)), ('C-TX-.5', dict(n_voxels=3))]:
         |    write_dataset(dataclasses.replace(CELLS[cid], **kw), HARNESS_ROOT, 'harness', 0, '$out')
         |""".stripMargin
    val ran =
      scala.util.Try {
        val python = sys.env.getOrElse("PHRF_GENERATOR_PYTHON", "python3")
        val p = new ProcessBuilder(python, "-c", script).redirectErrorStream(true).start()
        p.getInputStream.readAllBytes()
        p.waitFor()
      }.toOption
    assume(ran.contains(0), "python3 with numpy/scipy unavailable")
    try
      for stem <- Seq("T-TX-fast__d0000", "C-TX-.5__d0000"); ext <- Seq("npz", "manifest.json") do
        assertEquals(Files.readAllBytes(out.resolve(s"$stem.$ext")).toSeq, bytes(stem, ext).toSeq, s"$stem.$ext differs")
    finally
      Files.list(out).iterator().asScala.foreach(Files.delete)
      Files.delete(out)
