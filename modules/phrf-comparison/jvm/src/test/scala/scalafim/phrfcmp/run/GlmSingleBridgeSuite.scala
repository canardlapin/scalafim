package scalafim.phrfcmp.run

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.attribute.{BasicFileAttributes, FileTime, PosixFilePermissions}
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}
import java.security.MessageDigest
import java.util.concurrent.TimeUnit

import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.chaining.*

import scalafim.phrfcmp.ingest.{NpyArray, Npz}
import scalafim.phrfcmp.score.VoxelOutcome

/** S6 Scala bridge. Real GLMsingle runs use the committed harness fixture `T-TX-fast__d0000` (3 scored, 2 pool
  * voxels, 144 trials) and the pinned virtualenv (`PHRF_GLMSINGLE_PYTHON`, else the tools venv found above the
  * working directory); stubs stand in for the interpreter in the failure, timeout and environment tests.
  *
  * Do not run this suite concurrently with another instance of itself (for example from two worktrees). The shared
  * fixture gives both runs identical real betas, so one run's files would trip the other's leak scan. Hits are
  * deliberately not filtered.
  */
class GlmSingleBridgeSuite extends munit.FunSuite:

  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(300, TimeUnit.SECONDS)
  /** Child timeout of a real GLMsingle run: below `munitTimeout`, so a slow child is refused by the bridge (and its
    * scratch released) before munit abandons the test.
    */
  private val RealRunTimeout = 240.0

  // ---- locations --------------------------------------------------------------------------------------------
  private val Fixture = "T-TX-fast__d0000"
  private val FixtureOutputSha = "575a74822329ea91f32acc00d2e5ca979d8424bd5bdf2c34a1ee532384aa72e8"

  private def repoRoot: Path =
    Iterator.iterate(Paths.get(System.getProperty("user.dir")).toAbsolutePath)(_.getParent).takeWhile(_ != null)
      .find(p => Files.exists(p.resolve("build.sbt"))).getOrElse(fail("no build.sbt above user.dir"))

  private def script: Path = repoRoot.resolve("tools/phrf-comparison/glmsingle/from_generator.py")

  private def venvPython: Option[Path] =
    val fromEnv = Option(System.getenv("PHRF_GLMSINGLE_PYTHON")).map(Paths.get(_))
    val local = repoRoot.resolve("tools/phrf-comparison/glmsingle/.venv/bin/python")
    val shared = Paths.get("/private/tmp/scalafim-phrf-v0-20261001/tools/phrf-comparison/glmsingle/.venv/bin/python")
    (fromEnv.toSeq ++ Seq(local, shared)).find(p => Files.isExecutable(p))

  private def fixtureNpz: Path =
    Paths.get(getClass.getClassLoader.getResource(s"fixtures/$Fixture.npz").toURI)

  private def sha(b: Array[Byte]): String = MessageDigest.getInstance("SHA-256").digest(b).map(x => f"${x & 0xff}%02x").mkString

  private class Events extends ScratchHook:
    val all = new java.util.concurrent.CopyOnWriteArrayList[ScratchEvent]
    def event(e: ScratchEvent): Unit = all.add(e): Unit
    def list: Vector[ScratchEvent] = all.asScala.toVector
    def scratchPath: Option[String] = list.collectFirst { case ScratchEvent.Created(_, p) => p }
    def device: Option[String] = list.collectFirst { case ScratchEvent.DeviceAttached(d, _) => d }

  private def config(python: Path, timeout: Double, hook: ScratchHook, scratch: ScratchProvider = RamScratch.host()): GlmSingleConfig =
    GlmSingleConfig(python, script, timeout, hook = hook, scratch = scratch)

  private def inputs: GlmSingleInputs = GlmSingleInputs(fixtureNpz, Some(sha(Files.readAllBytes(fixtureNpz))))

  /** A shell script standing in for the Python interpreter. */
  private def stub(dir: Path, body: String): Path =
    val p = dir.resolve("fakepython")
    Files.writeString(p, "#!/bin/sh\n" + body + "\n")
    Files.setPosixFilePermissions(p, PosixFilePermissions.fromString("rwxr-xr-x"))
    p

  private val tmp = FunFixture[Path](
    setup = _ => Files.createTempDirectory("glmsingle-bridge-test-"),
    teardown = d =>
      Try {
        val s = Files.walk(d)
        try s.sorted(java.util.Comparator.reverseOrder[Path]()).iterator().asScala.foreach(Files.deleteIfExists(_): Unit)
        finally s.close()
      }: Unit
  )

  private def assertCleaned(ev: Events, clue: String = ""): Unit =
    val sp = ev.scratchPath.getOrElse(fail(s"no scratch was created $clue"))
    assert(!Files.exists(Paths.get(sp)), s"scratch path $sp survives $clue")
    ev.device.foreach(d => assert(!Files.exists(Paths.get(d)), s"RAM device $d survives $clue"))
    val mountOut = RamScratch.cmd(Seq("/sbin/mount")).out
    assert(!mountOut.contains(Paths.get(sp).getFileName.toString), s"scratch still mounted $clue")
    assert(ev.list.contains(ScratchEvent.Verified(pathGone = true, deviceGone = true)), s"cleanup was not verified $clue")

  // ---- trial-order mapping (pure) ---------------------------------------------------------------------------
  test("order: chronological betas are placed at their generator event index (input order)") {
    val trials = 5
    val ev = Vector(2, 0, 4, 1, 3) // chronological k -> event index
    // chrono(v, k) = 100 v + k
    val chrono = Array.tabulate(2 * trials)(i => (100 * (i / trials) + (i % trials)).toDouble)
    val out = GlmSingleBridge.toVoxelOutcomes(2, trials, chrono, ev).map(_.toOption.get.values)
    for v <- 0 until 2; k <- 0 until trials do assertEquals(out(v)(ev(k)), (100 * v + k).toDouble, s"v=$v k=$k")
    // explicit expected rows: event e holds the chronological index k with ev(k) = e
    assertEquals(out(0), Vector(1.0, 3.0, 0.0, 4.0, 2.0))
    assertEquals(out(1), Vector(101.0, 103.0, 100.0, 104.0, 102.0))
    assertNotEquals(out(0), (0 until trials).map(_.toDouble).toVector) // a missing reorder is visible
  }

  test("order: identity permutation leaves the chronological order unchanged; non-finite voxel fails alone") {
    val chrono = Array(1.0, 2.0, 3.0, Double.NaN, 5.0, 6.0)
    val out = GlmSingleBridge.toVoxelOutcomes(2, 3, chrono, Vector(0, 1, 2))
    assertEquals(out(0).toOption.map(_.values), Some(Vector(1.0, 2.0, 3.0)))
    assertEquals(out(1), VoxelOutcome.Failed)
  }

  test("order: trial_event_index must be a permutation of 0 until N") {
    def ei(a: Double*) = GlmSingleBridge.eventIndex(a.toArray, a.length)
    assertEquals(ei(1, 0, 2), Right(Vector(1, 0, 2)))
    assert(ei(0, 0, 2).isLeft) // duplicate
    assert(ei(0, 1, 3).isLeft) // out of range
    assert(ei(0, 1, -1).isLeft)
    assert(ei(0, 1, 1.5).isLeft)
    assert(GlmSingleBridge.eventIndex(Array(0.0, 1.0), 3).isLeft) // wrong length
  }

  test("pins and thread environment are the design's") {
    val env = GlmSingleEnv.forScratch(Paths.get("/scratch"), Paths.get("/venv/bin/python"))
    for k <- Seq("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS", "NUMEXPR_NUM_THREADS") do assertEquals(env(k), "1")
    assertEquals(env("TMPDIR"), "/scratch/tmp")
    assertEquals(env("HOME"), "/scratch/home")
    assert(env.keySet.forall(k => !k.startsWith("PHRF")), "no root variables")
    assertEquals(GlmSinglePins.FromGeneratorSha256, sha(Files.readAllBytes(script)))
  }

  // ---- real GLMsingle ---------------------------------------------------------------------------------------
  /** Runs the frozen CLI directly (plain disk; test-only expectation, deleted by the caller). */
  private def directRun(python: Path, dir: Path): (Array[Byte], Vector[Vector[Double]], Vector[Int]) =
    val out = dir.resolve("direct")
    Files.createDirectories(out)
    val pb = new ProcessBuilder(python.toString, script.toString, fixtureNpz.toString, "--out", out.toString)
      .redirectErrorStream(true).directory(dir.toFile)
    GlmSingleEnv.ThreadPins.foreach((k, v) => pb.environment().put(k, v))
    val pr = pb.start()
    val _ = pr.getInputStream.readAllBytes()
    assertEquals(pr.waitFor(), 0)
    val bytes = Files.readAllBytes(out.resolve(s"$Fixture.glmsingle.npz"))
    val npz = Npz.parse(bytes).fold(e => fail(e.message), identity)
    val (v, n, beta) = npz.get("d_beta_data").get.array match
      case NpyArray.F64(Vector(a, b), d) => (a, b, d)
      case _                             => fail("d_beta_data shape")
    val ev = npz.get("trial_event_index").get.array match
      case NpyArray.F64(_, d) => d
      case _                  => fail("trial_event_index dtype")
    // independent expectation: event e holds chrono k with ev(k) = e
    val expected = Vector.tabulate(v) { vox =>
      val row = new Array[Double](n)
      for k <- 0 until n do row(ev(k).toInt) = beta(vox * n + k)
      row.toVector
    }
    (bytes, expected, ev.map(_.toInt).toVector)

  tmp.test("real GLMsingle on T-TX-fast: E-trial in input order equals the converter output bit for bit; scratch gone") { dir =>
    val py = venvPython
    assume(py.isDefined, "GLMsingle virtualenv not found (set PHRF_GLMSINGLE_PYTHON)")
    val (directBytes, expected, ev) = directRun(py.get, dir)
    assertEquals(sha(directBytes), FixtureOutputSha, "converter output is not the receipt-pinned bytes")
    // The committed fixture's trial_event_index is the identity (trial order == chronological), so the permutation
    // itself is exercised by the pure tests and by the end-to-end permuted-output test below.
    assertEquals(ev, ev.indices.toVector)
    val events = new Events
    val r = GlmSingleBridge.run(inputs, config(py.get, RealRunTimeout, events))
    val o = r.fold(e => fail(e.message), identity)
    assertEquals(o.outputNpzSha256, sha(directBytes))
    assertEquals(o.nTrials, 144)
    assertEquals(o.voxels.length, 3)
    for v <- 0 until 3 do
      val got = o.voxels(v).toOption.getOrElse(fail(s"voxel $v not estimated")).values
      assertEquals(got.length, 144)
      for e <- 0 until 144 do
        assertEquals(java.lang.Double.doubleToRawLongBits(got(e)), java.lang.Double.doubleToRawLongBits(expected(v)(e)), s"bit mismatch v=$v e=$e")
    assertEquals(o.glmSingleCommit, GlmSinglePins.GlmSingleCommit)
    assertEquals(o.fromGeneratorSha256, GlmSinglePins.FromGeneratorSha256)
    assert(o.guardCpuSeconds > 0.5, s"cpu ${o.guardCpuSeconds}")
    assert(o.guardCpuSeconds >= o.timingCpuSeconds, "guard CPU (rusage) includes the GLMsingle call (sidecar)")
    assert(o.threadPinning.contains("OMP_NUM_THREADS=1"))
    println(f"[S6 bridge] fixture CPU: guard ${o.guardCpuSeconds}%.2f s; rusage ${o.cpuRusageSeconds.getOrElse(Double.NaN)}%.2f s; timing (sidecar cpu_s) ${o.timingCpuSeconds}%.2f s; wall ${o.wallSeconds}%.2f s")
    assert(!o.toString.exists(_.isDigit) || o.toString.contains("<redacted>"))
    assertCleaned(events)
    // lifecycle order, every step logged as an event
    val names = events.list.map(_.getClass.getSimpleName)
    assertEquals(names.filter(Set("Created", "DeviceAttached", "Formatted", "Mounted", "ChildStarted", "ProcessGroupKilled", "Wiped", "Unmounted", "Detached", "Removed", "Verified")).toVector.distinct,
      Vector("DeviceAttached", "Created", "Formatted", "Mounted", "ChildStarted", "ProcessGroupKilled", "Wiped", "Unmounted", "Detached", "Removed", "Verified"))
    val wiped = events.list.collectFirst { case w: ScratchEvent.Wiped => w }.get
    assert(wiped.files >= 2, "result files (npz + sidecar) must have been present and wiped")
    assertEquals(wiped.residueAfterWipe, 0)
  }

  // ---- disk-leak scan ---------------------------------------------------------------------------------------
  private val scannedFiles = new java.util.concurrent.atomic.AtomicLong
  private val scannedBytes = new java.util.concurrent.atomic.AtomicLong

  private def leakNeedles(doubles: Seq[Double]): Set[Long] = doubles.map(java.lang.Double.doubleToRawLongBits).toSet

  /** Files modified at or after `since` under `roots` whose bytes contain any needle (8-byte LE float64). The
    * lookup is a binary search in a sorted array, so the scan is O(bytes log needles). Directories are never pruned
    * by their own mtime: overwriting an existing file deeper down does not change it.
    */
  private def scanForLeaks(roots: Seq[Path], since: FileTime, needles: Set[Long], skip: Path => Boolean): Vector[Path] =
    val hits = Vector.newBuilder[Path]
    val needleArr = needles.toArray.sorted
    val firstBytes = new Array[Boolean](256)
    needleArr.foreach(n => firstBytes((n & 0xffL).toInt) = true)
    roots.filter(Files.isDirectory(_)).foreach { root =>
      Files.walkFileTree(root, new SimpleFileVisitor[Path]:
        override def preVisitDirectory(d: Path, a: BasicFileAttributes): FileVisitResult =
          val n = Option(d.getFileName).map(_.toString).getOrElse("")
          if n == ".git" || n == "target" || n == "node_modules" || skip(d) then FileVisitResult.SKIP_SUBTREE else FileVisitResult.CONTINUE
        override def visitFile(f: Path, a: BasicFileAttributes): FileVisitResult =
          if a.isRegularFile && a.size > 0 && a.size < 16L * 1024 * 1024 && a.lastModifiedTime.compareTo(since) >= 0 then
            Try {
              val b = Files.readAllBytes(f)
              scannedFiles.incrementAndGet(); scannedBytes.addAndGet(b.length.toLong)
              val bb = ByteBuffer.wrap(b).order(ByteOrder.LITTLE_ENDIAN)
              var i = 0
              var found = false
              while !found && i + 8 <= b.length do
                if firstBytes(b(i) & 0xff) && java.util.Arrays.binarySearch(needleArr, bb.getLong(i)) >= 0 then found = true
                i += 1
              if found then hits += f
            }: Unit
          FileVisitResult.CONTINUE
        override def visitFileFailed(f: Path, e: java.io.IOException): FileVisitResult = FileVisitResult.CONTINUE
      )
    }
    hits.result()

  private def getconf(name: String): Option[Path] =
    val r = RamScratch.cmd(Seq("/usr/bin/getconf", name))
    Option.when(r.exit == 0 && r.out.nonEmpty)(Paths.get(r.out.trim))

  /** The persistent places the child can write. Its HOME, TMPDIR, XDG dirs, MPLCONFIGDIR and NUMBA_CACHE_DIR are
    * in the scratch, so this is: the shared temp dirs, the per-user Darwin temp and cache dirs, `~/Library/Caches`
    * (frameworks resolve home through getpwuid), crash reports, the virtualenv and the tool tree.
    */
  private def leakRoots: Seq[Path] =
    val home = Paths.get(System.getProperty("user.home"))
    val venv = venvPython.flatMap(p => Option(p.getParent)).flatMap(p => Option(p.getParent))
    (Seq(Paths.get("/tmp"), Paths.get("/private/var/tmp"), Paths.get("/dev/shm"), Paths.get(System.getProperty("java.io.tmpdir"))) ++
      getconf("DARWIN_USER_TEMP_DIR") ++ getconf("DARWIN_USER_CACHE_DIR") ++
      Seq(home.resolve("Library/Caches"), home.resolve("Library/Logs/DiagnosticReports")) ++
      venv ++ Seq(repoRoot.resolve("tools/phrf-comparison")))
      // Real paths (`/tmp` is a symlink, which `walkFileTree` would not descend), and a root inside another is
      // dropped so that nothing is scanned twice.
      .flatMap(p => Try(p.toRealPath()).toOption).distinct
      .pipe(rs => rs.filterNot(r => rs.exists(o => o != r && r.startsWith(o))))

  tmp.test("scan control: the leak scanner finds a planted value in a plain file") { dir =>
    val sentinel = 1234567.8901234
    val f = dir.resolve("plain.bin")
    val bb = ByteBuffer.allocate(16).order(ByteOrder.LITTLE_ENDIAN)
    bb.putDouble(0.5).putDouble(sentinel)
    Files.write(f, bb.array())
    val since = FileTime.from(java.time.Instant.now().minusSeconds(5))
    assertEquals(scanForLeaks(Seq(dir), since, leakNeedles(Seq(sentinel)), _ => false), Vector(f))
  }

  tmp.test("disk leak: no file created during a real bridge run contains a beta value (planted sentinel and real betas)") { dir =>
    val py = venvPython
    assume(py.isDefined, "GLMsingle virtualenv not found (set PHRF_GLMSINGLE_PYTHON)")
    val sentinel = 1234567.8901234
    val sentinelHex = {
      val bb = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN); bb.putDouble(sentinel)
      bb.array().map(b => f"\\${b & 0xff}%03o").mkString
    }
    // (1) planted: a stub writes the sentinel to every place a Python process could put a file.
    val plant = stub(dir,
      s"""for d in "$$PWD" "$$TMPDIR" "$$HOME" "$$HOME/.cache" "$$HOME/.config" "$$HOME/mpl" "$$HOME/numba" "$$PWD/out" "$$PWD/GLMestimatesingletrialoutputs"; do
         |  mkdir -p "$$d"; printf '$sentinelHex' > "$$d/planted.bin"
         |done
         |exit 3""".stripMargin)
    val since = FileTime.from(java.time.Instant.now().minusSeconds(1))
    val ev0 = new Events
    val r0 = GlmSingleBridge.run(inputs, config(plant, 60.0, ev0))
    assert(r0.left.toOption.exists(_.isInstanceOf[GlmSingleRefusal.ChildFailed]), s"$r0")
    assert(ev0.list.collectFirst { case w: ScratchEvent.Wiped => w.files }.exists(_ >= 8), "the planted files were not in the scratch when wiped")
    assertCleaned(ev0, "(planted)")
    // (2) real: the actual returned betas.
    val ev = new Events
    val o = GlmSingleBridge.run(inputs, config(py.get, RealRunTimeout, ev)).fold(e => fail(e.message), identity)
    val betas = o.voxels.flatMap(_.toOption.toVector).flatMap(_.values)
    assertEquals(betas.length, 3 * 144)
    assertCleaned(ev, "(real)")
    // positive control for the real needles: they are findable in a plain file (written after `since`)
    val ctl = dir.resolve("control.bin")
    val bb = ByteBuffer.allocate(8 * betas.length).order(ByteOrder.LITTLE_ENDIAN)
    betas.foreach(bb.putDouble(_): Unit)
    Files.write(ctl, bb.array())
    val needles = leakNeedles(betas :+ sentinel)
    assertEquals(scanForLeaks(Seq(dir), since, needles, _ => false), Vector(ctl))
    Files.delete(ctl)
    // One scan over every persistent location the child can write, for files created or modified since before the
    // first run. A control file holding one real beta is planted in a scanned root (outside the skipped test dir):
    // the scan must find exactly it, so a root, lookup or traversal defect cannot pass as "no leak".
    val planted = Files.createTempFile(Paths.get(System.getProperty("java.io.tmpdir")), "phrf-leak-control-", ".bin")
    try
      val pb = ByteBuffer.allocate(24).order(ByteOrder.LITTLE_ENDIAN)
      pb.putDouble(0.5).putDouble(betas(betas.length / 2)).putDouble(0.25)
      Files.write(planted, pb.array())
      scannedFiles.set(0L); scannedBytes.set(0L)
      val roots = leakRoots
      val t0 = System.nanoTime()
      val dirReal = dir.toRealPath()
      val hits = scanForLeaks(roots, since, needles, p => p.startsWith(dirReal))
      println(s"[S6 leak scan] roots=${roots.mkString(",")}; files modified since the run and scanned=${scannedFiles.get}, bytes=${scannedBytes.get}, seconds=${(System.nanoTime() - t0) / 1e9}, hits=${hits.length} (1 planted control)")
      assertEquals(hits.map(_.toRealPath()).distinct, Vector(planted.toRealPath()),
        "expected exactly the planted control; anything else is a sentinel or beta value on a persistent path")
    finally Files.deleteIfExists(planted): Unit
  }

  // ---- permuted output end to end (stub emits a well-formed STORED npz + sidecar) -----------------------------
  private def npy(shape: Seq[Int], data: Seq[Double]): Array[Byte] =
    val hdr0 = s"{'descr': '<f8', 'fortran_order': False, 'shape': (${shape.mkString(", ")}${if shape.length == 1 then "," else ""}), }"
    val total = 10 + hdr0.length + 1
    val pad = (64 - total % 64) % 64
    val hdr = hdr0 + (" " * pad) + "\n"
    val bb = ByteBuffer.allocate(10 + hdr.length + 8 * data.length).order(ByteOrder.LITTLE_ENDIAN)
    bb.put(Array[Byte](0x93.toByte, 'N', 'U', 'M', 'P', 'Y', 1, 0)).putShort(hdr.length.toShort).put(hdr.getBytes("US-ASCII"))
    data.foreach(bb.putDouble(_): Unit)
    bb.array()

  private def storedNpz(members: Seq[(String, Array[Byte])]): Array[Byte] =
    val out = new java.io.ByteArrayOutputStream
    val zos = new java.util.zip.ZipOutputStream(out)
    members.foreach { (n, b) =>
      val e = new java.util.zip.ZipEntry(n + ".npy")
      val crc = new java.util.zip.CRC32; crc.update(b)
      e.setMethod(java.util.zip.ZipEntry.STORED); e.setSize(b.length.toLong); e.setCompressedSize(b.length.toLong); e.setCrc(crc.getValue)
      e.setTime(0L)
      zos.putNextEntry(e); zos.write(b); zos.closeEntry()
    }
    zos.close()
    out.toByteArray

  tmp.test("end to end with a non-identity trial permutation: bridge returns input (event) order") { dir =>
    val trials = 6
    val ev = Vector(3, 0, 5, 1, 4, 2)
    val chrono = for v <- 0 until 2; k <- 0 until trials yield 10.0 * v + k + 0.25
    val npzBytes = storedNpz(Seq(
      "d_beta_data" -> npy(Seq(2, trials), chrono),
      "trial_event_index" -> npy(Seq(trials), ev.map(_.toDouble))
    ))
    val inSha = sha(Files.readAllBytes(fixtureNpz))
    val side =
      s"""{"output":{"npz_sha256":"${sha(npzBytes)}"},"input":{"npz_sha256":"$inSha"},
         |"meta":{"from_generator_sha256":"${GlmSinglePins.FromGeneratorSha256}","glmsingle_commit":"${GlmSinglePins.GlmSingleCommit}","realized_pool_size":7},
         |"timing":{"cpu_s":1.5,"wall_s":2.0}}""".stripMargin
    Files.write(dir.resolve("emit.npz"), npzBytes)
    Files.writeString(dir.resolve("emit.json"), side)
    val s = stub(dir, s"""mkdir -p "$$PWD/out"; cp ${dir.resolve("emit.npz")} "$$PWD/out/$Fixture.glmsingle.npz"; cp ${dir.resolve("emit.json")} "$$PWD/out/$Fixture.glmsingle.meta.json"; exit 0""")
    val events = new Events
    val o = GlmSingleBridge.run(inputs, config(s, 60.0, events)).fold(e => fail(e.message), identity)
    assertEquals(o.realizedPoolSize, 7)
    assertEquals(o.timingCpuSeconds, 1.5)
    for v <- 0 until 2; k <- 0 until trials do
      assertEquals(o.voxels(v).toOption.get.values(ev(k)), 10.0 * v + k + 0.25, s"v=$v k=$k")
    assertNotEquals(o.voxels(0).toOption.get.values, chrono.take(trials).toVector)
    assertCleaned(events)
    // a provenance mismatch in the sidecar (wrong input hash) is refused
    Files.writeString(dir.resolve("emit.json"), side.replace(inSha, "0" * 64))
    val r = GlmSingleBridge.run(inputs, config(s, 60.0, new Events))
    assert(r.left.toOption.exists(_.isInstanceOf[GlmSingleRefusal.ProvenanceMismatch]), s"$r")
  }

  // ---- failure, timeout and cleanup -------------------------------------------------------------------------
  tmp.test("timeout: refusal, whole process group killed, scratch wiped and detached") { dir =>
    val pidFile = dir.resolve("pids")
    val s = stub(dir,
      s"""mkdir -p "$$PWD/out"; echo planted > "$$PWD/out/partial.bin"; echo x > "$$TMPDIR/t.bin"
         |sleep 300 &
         |echo $$! >> $pidFile
         |echo $$$$ >> $pidFile
         |sleep 300""".stripMargin)
    val ev = new Events
    val t0 = System.nanoTime()
    val r = GlmSingleBridge.run(inputs, config(s, 1.5, ev))
    val secs = (System.nanoTime() - t0) / 1e9
    assert(r.left.toOption.exists(_.isInstanceOf[GlmSingleRefusal.Timeout]), s"$r")
    assert(secs < 30.0, s"took $secs s")
    val pids = Files.readAllLines(pidFile).asScala.map(_.trim.toLong).toVector
    assertEquals(pids.length, 2)
    pids.foreach(p => assert(!ProcessHandle.of(p).map(_.isAlive).orElse(false), s"pid $p survived the group kill"))
    assert(ev.list.exists { case ScratchEvent.ProcessGroupKilled(_, 0) => true; case _ => false })
    assert(ev.list.collectFirst { case w: ScratchEvent.Wiped => w.files }.exists(_ >= 2))
    assertCleaned(ev, "after timeout")
  }

  tmp.test("forced failure: non-zero exit, Python refusal exit 2 and missing output each clean the scratch") { dir =>
    def one(body: String): (Either[GlmSingleRefusal, GlmSingleOutcome], Events) =
      val ev = new Events
      (GlmSingleBridge.run(inputs, config(stub(dir, body), 60.0, ev)), ev)
    val (r3, e3) = one("""echo secret-ish > "$TMPDIR/leak.bin"; echo 'FAILED: ValueError: two trials in one volume' >&2; exit 3""")
    assert(r3.left.toOption.exists { case GlmSingleRefusal.ChildFailed(3, t) => t.contains("two trials"); case _ => false }, s"$r3")
    assertCleaned(e3, "(exit 3)")
    val (r2, e2) = one("""echo 'REFUSED: tr_aligned_onsets' >&2; exit 2""")
    assert(r2.left.toOption.exists(_.isInstanceOf[GlmSingleRefusal.InputRefusedByPython]), s"$r2")
    assertCleaned(e2, "(exit 2)")
    val (r0, e0) = one("""exit 0""")
    assert(r0.left.toOption.exists(_.isInstanceOf[GlmSingleRefusal.MissingOutput]), s"$r0")
    assertCleaned(e0, "(missing output)")
    assertEquals(GlmSingleRefusal.ChildFailed(3, "x").asVoxels(2), Vector(VoxelOutcome.Failed, VoxelOutcome.Failed))
  }

  tmp.test("the child sees a cleared, scratch-confined environment with pinned threads") { dir =>
    val ev = new Events
    val s = stub(dir, """echo "ENV omp=$OMP_NUM_THREADS mkl=$MKL_NUM_THREADS ob=$OPENBLAS_NUM_THREADS ne=$NUMEXPR_NUM_THREADS tmp=$TMPDIR home=$HOME pwd=$PWD n=$(env | wc -l | tr -d ' ')" >&2; exit 3""")
    val r = GlmSingleBridge.run(inputs, config(s, 60.0, ev))
    val tail = r.left.toOption.collect { case GlmSingleRefusal.ChildFailed(_, t) => t }.getOrElse(fail(s"$r"))
    val sp = ev.scratchPath.get
    def under(key: String, suffix: String): Boolean =
      val v = s"$key=(\\S+)".r.findFirstMatchIn(tail).map(_.group(1)).getOrElse("")
      (v == sp + suffix) || (v == "/private" + sp + suffix)
    assert(tail.contains("omp=1 mkl=1 ob=1 ne=1"), tail)
    assert(under("tmp", "/tmp"), tail)
    assert(under("home", "/home"), tail)
    assert(under("pwd", ""), tail)
    val n = """n=(\d+)""".r.findFirstMatchIn(tail).map(_.group(1).toInt).get
    assert(n < 40, s"environment not minimal: $n variables")
    assertCleaned(ev)
  }

  tmp.test("no RAM scratch on the host: typed refusal and the child is never started") { dir =>
    val marker = dir.resolve("started")
    val s = stub(dir, s"touch $marker")
    val r = GlmSingleBridge.run(inputs, config(s, 60.0, ScratchHook.none, RamScratch.forOs("Plan 9")))
    assert(r.left.toOption.exists(_.isInstanceOf[GlmSingleRefusal.NoRamScratch]), s"$r")
    assert(!Files.exists(marker))
  }

  tmp.test("input, script and provenance mismatches are refused before any scratch exists") { dir =>
    val ev = new Events
    val bad = GlmSingleInputs(fixtureNpz, Some("0" * 64))
    assert(GlmSingleBridge.run(bad, config(stub(dir, "exit 0"), 60.0, ev)).left.toOption.exists(_.isInstanceOf[GlmSingleRefusal.InputHashMismatch]))
    val wrongScript = config(stub(dir, "exit 0"), 60.0, ev).copy(expectedFromGeneratorSha256 = "1" * 64)
    assert(GlmSingleBridge.run(inputs, wrongScript).left.toOption.exists(_.isInstanceOf[GlmSingleRefusal.ScriptHashMismatch]))
    assertEquals(ev.list, Vector.empty[ScratchEvent])
  }

  tmp.test("a scratch that cannot be verified gone overrides the run result; a throwing hook does not stop cleanup") { dir =>
    val real = RamScratch.host()
    val failing: ScratchProvider = h =>
      real.create(h).map { sc =>
        new Scratch:
          def root: Path = sc.root
          def release(): Either[GlmSingleRefusal, Unit] =
            val _ = sc.release()
            Left(GlmSingleRefusal.ScratchResidue("test", "forced", None))
      }
    val r = GlmSingleBridge.run(inputs, config(stub(dir, "exit 3"), 60.0, ScratchHook.none, failing))
    assert(r.left.toOption.exists { case GlmSingleRefusal.ScratchResidue("test", _, Some(prior)) => prior.contains("exited 3"); case _ => false }, s"$r")
    val ev = new Events:
      override def event(e: ScratchEvent): Unit =
        super.event(e)
        if e.isInstanceOf[ScratchEvent.Wiped] then throw new RuntimeException("hook failure")
    val r2 = GlmSingleBridge.run(inputs, config(stub(dir, "exit 3"), 60.0, ev))
    assert(r2.left.toOption.exists(_.isInstanceOf[GlmSingleRefusal.ChildFailed]), s"$r2")
    assertCleaned(ev, "(throwing hook)")
  }

  // ---- review round: custody-cleanup holes --------------------------------------------------------------------
  private def onEvent(ev: Events)(f: PartialFunction[ScratchEvent, Unit]): ScratchHook = new Events:
    override def event(e: ScratchEvent): Unit =
      ev.event(e)
      f.lift(e): Unit

  /** Releases a scratch the test deliberately left behind, with the real tools. */
  private def manualCleanup(path: String, device: String, sc: Scratch): Unit =
    ScratchRegistry.unregister(sc)
    RamScratch.cmd(Seq("/sbin/umount", path)): Unit
    RamScratch.cmd(Seq("/usr/bin/hdiutil", "detach", device)): Unit
    Files.deleteIfExists(Paths.get(path)): Unit

  private def capturing(tools: MacTools, ownerPid: Long = ProcessHandle.current().pid()): (ScratchProvider, java.util.concurrent.atomic.AtomicReference[Scratch]) =
    val ref = new java.util.concurrent.atomic.AtomicReference[Scratch]
    val base = RamScratch.forOsWith("Mac OS X", 128, tools, ownerPid)
    ((h: ScratchHook) => base.create(h).map { sc => ref.set(sc); sc }, ref)

  tmp.test("F1: an Error (OutOfMemoryError) in the output path still detaches the scratch") { dir =>
    val ev = new Events
    val cfg = config(stub(dir, "exit 0"), 60.0, ev).copy(probe = {
      case "read-output" => throw new OutOfMemoryError("injected")
      case _             => ()
    })
    // munit's `intercept` does not catch VirtualMachineError, so catch it by hand.
    val thrown =
      try { GlmSingleBridge.run(inputs, cfg); None }
      catch case e: OutOfMemoryError => Some(e)
    assert(thrown.exists(_.getMessage == "injected"), "the Error must propagate after cleanup")
    assertCleaned(ev, "after OutOfMemoryError")
    assertEquals(ScratchRegistry.liveCount, 0)
  }

  tmp.test("F2a: an interrupt arriving during release does not stop teardown, and the flag is restored") { dir =>
    val ev = new Events
    val hook = onEvent(ev) { case _: ScratchEvent.Wiped => Thread.currentThread().interrupt() }
    val r = GlmSingleBridge.run(inputs, config(stub(dir, "exit 3"), 60.0, hook))
    assert(r.left.toOption.exists(_.isInstanceOf[GlmSingleRefusal.ChildFailed]), s"$r")
    assert(Thread.interrupted(), "interrupt flag was swallowed")
    assertCleaned(ev, "(interrupt in release)")
  }

  tmp.test("F2b: an interrupt while the child runs kills it, releases, refuses Interrupted and keeps the flag") { dir =>
    val ev = new Events
    val hook = onEvent(ev) { case _: ScratchEvent.ChildStarted => Thread.currentThread().interrupt() }
    val t0 = System.nanoTime()
    val r = GlmSingleBridge.run(inputs, config(stub(dir, "sleep 300"), 120.0, hook))
    assertEquals(r.left.toOption, Some(GlmSingleRefusal.Interrupted))
    assert((System.nanoTime() - t0) / 1e9 < 60.0)
    assert(Thread.interrupted(), "interrupt flag was swallowed")
    assertCleaned(ev, "(interrupt in run)")
  }

  test("F2c: a hung tool is killed at its timeout and reported, not waited on forever") {
    val t0 = System.nanoTime()
    val r = RamScratch.cmd(Seq("/bin/sleep", "300"), 1L)
    assertEquals(r.exit, -1)
    assert((System.nanoTime() - t0) / 1e9 < 15.0)
  }

  tmp.test("F2c: a hanging umount is reported as residue after the remaining steps ran; then the leftovers are removed") { dir =>
    val hang = dir.resolve("hang-umount")
    Files.writeString(hang, "#!/bin/sh\nsleep 300\n")
    Files.setPosixFilePermissions(hang, PosixFilePermissions.fromString("rwxr-xr-x"))
    val (prov, ref) = capturing(MacTools(umount = hang.toString, cmdTimeoutSeconds = 2L, retries = 2, retryPauseMillis = 50L))
    val ev = new Events
    val r = GlmSingleBridge.run(inputs, config(stub(dir, "exit 3"), 60.0, ev, prov))
    assert(r.left.toOption.exists { case GlmSingleRefusal.ScratchResidue(s, _, Some(_)) => s.contains("unmount"); case _ => false }, s"$r")
    assert(ev.list.exists { case ScratchEvent.StepFailed("detach", _) => true; case _ => false }, "detach was not attempted after the failed unmount")
    assert(ev.list.contains(ScratchEvent.Verified(pathGone = false, deviceGone = false)))
    manualCleanup(ev.scratchPath.get, ev.device.get, ref.get())
    assert(!Files.exists(Paths.get(ev.device.get)))
  }

  tmp.test("F2c: a failing unmount still runs detach, remove and verify, and reports ScratchResidue") { dir =>
    val (prov, ref) = capturing(MacTools(umount = "/usr/bin/false", retries = 2, retryPauseMillis = 20L))
    val ev = new Events
    val r = GlmSingleBridge.run(inputs, config(stub(dir, "exit 3"), 60.0, ev, prov))
    assert(r.left.toOption.exists(_.isInstanceOf[GlmSingleRefusal.ScratchResidue]), s"$r")
    val names = ev.list.collect { case ScratchEvent.StepFailed(s, _) => s }
    assert(names.contains("unmount") && names.contains("detach") && names.contains("verify"), names.toString)
    manualCleanup(ev.scratchPath.get, ev.device.get, ref.get())
  }

  tmp.test("F3: a refused attempt still reports exit, wall and CPU (a timed-out CPU burner is not free)") { dir =>
    val burn = stub(dir, "i=0; while :; do i=$((i+1)); done")
    val run = GlmSingleBridge.runAttempt(inputs, config(burn, 1.5, ScratchHook.none))
    assert(run.result.left.toOption.exists(_.isInstanceOf[GlmSingleRefusal.Timeout]))
    assert(run.attempt.timedOut && run.attempt.exitCode.isEmpty)
    assert(run.attempt.wallSeconds >= 1.4, s"${run.attempt.wallSeconds}")
    assert(run.attempt.guardCpuSeconds > 0.5, s"guard CPU ${run.attempt.guardCpuSeconds}")
    val fail3 = stub(dir, "i=0; while [ $i -lt 60000 ]; do i=$((i+1)); done; exit 3")
    val r3 = GlmSingleBridge.runAttempt(inputs, config(fail3, 60.0, ScratchHook.none))
    assertEquals(r3.attempt.exitCode, Some(3))
    assert(r3.attempt.rusageCpuSeconds.exists(_ > 0.0), s"${r3.attempt}")
    assert(r3.attempt.guardCpuSeconds > 0.0)
    assertEquals(GlmSingleBridge.runAttempt(inputs, config(stub(dir, "exit 0"), 60.0, ScratchHook.none)).attempt.exitCode, Some(0))
  }

  tmp.test("F3/F4: a successful attempt reports both CPU figures and the output hash; guard is rusage, timing is the sidecar") { _ =>
    val py = venvPython
    assume(py.isDefined, "GLMsingle virtualenv not found (set PHRF_GLMSINGLE_PYTHON)")
    val run = GlmSingleBridge.runAttempt(inputs, config(py.get, RealRunTimeout, ScratchHook.none))
    val o = run.result.fold(e => fail(e.message), identity)
    assertEquals(run.attempt.outputNpzSha256, Some(FixtureOutputSha))
    assertEquals(run.attempt.sidecarCpuSeconds, Some(o.timingCpuSeconds))
    assertEquals(o.guardCpuSeconds, run.attempt.rusageCpuSeconds.get)
    assert(o.guardCpuSeconds > o.timingCpuSeconds)
  }

  tmp.test("F5: the startup sweep releases a deliberately leaked scratch of a dead owner, and nothing else") { _ =>
    val dead = { val p = new ProcessBuilder("/usr/bin/true").start(); p.waitFor(); p.pid() }
    val ev = new Events
    val leaked = RamScratch.forOsWith("Mac OS X", 128, MacTools(), dead).create(ev).fold(r => fail(r.message), identity)
    ScratchRegistry.unregister(leaked) // simulate a dead JVM: no hook will run
    val leakedPath = Paths.get(ev.scratchPath.get).toRealPath().toString // `mount(8)` reports the real path
    val leakedDev = ev.device.get
    Files.writeString(leaked.root.resolve("home/leftover.bin"), "x")
    // a live owner's scratch and a foreign (not phrfcmp-named) RAM volume must be left alone
    val liveEv = new Events
    val live = RamScratch.host().create(liveEv).fold(r => fail(r.message), identity)
    val foreign = Files.createTempDirectory("other-ram-")
    val fdev = RamScratch.cmd(Seq("/usr/bin/hdiutil", "attach", "-nomount", "ram://4096")).out.trim
    assert(fdev.startsWith("/dev/disk"), fdev)
    RamScratch.cmd(Seq("/sbin/newfs_hfs", fdev)): Unit
    assertEquals(RamScratch.cmd(Seq("/sbin/mount", "-t", "hfs", "-o", "nobrowse", fdev, foreign.toString)).exit, 0)
    // an empty stale mountpoint
    val stale = Files.createTempDirectory(s"phrfcmp-ram-$dead-")
    val staleReal = stale.toRealPath().toString
    try
      val swept = RamScratch.sweepStaleWith("Mac OS X", ScratchHook.none, MacTools())
      val byPath = swept.map(x => x.path -> x).toMap
      assertEquals(byPath(leakedPath).outcome, "released")
      assertEquals(byPath(leakedPath).device, Some(leakedDev))
      assertEquals(byPath(staleReal).outcome, "removed empty stale mountpoint")
      assert(!Files.exists(Paths.get(leakedPath)) && !Files.exists(Paths.get(ev.scratchPath.get)) && !Files.exists(Paths.get(leakedDev)))
      assert(!Files.exists(stale))
      assert(!swept.exists(x => x.path == liveEv.scratchPath.get || x.path.contains("other-ram-")), "swept something that is not a stale phrfcmp scratch")
      assert(Files.exists(Paths.get(liveEv.device.get)) && Files.exists(Paths.get(fdev)))
    finally
      assertEquals(live.release().isRight, true)
      RamScratch.cmd(Seq("/sbin/umount", foreign.toString)): Unit
      RamScratch.cmd(Seq("/usr/bin/hdiutil", "detach", fdev)): Unit
      Files.deleteIfExists(foreign): Unit
      Files.deleteIfExists(stale): Unit
  }

  tmp.test("F5: the JVM shutdown path kills the child's process group and releases a live scratch") { _ =>
    val ev = new Events
    val sc = RamScratch.host().create(ev).fold(r => fail(r.message), identity)
    val pr = GlmSingleBridge.startGroupLeader(Seq("/bin/sh", "-c", "sleep 300 & sleep 300"), sc.root, Map("PATH" -> "/usr/bin:/bin"))
    ScratchRegistry.setGroup(sc, pr.pid())
    Thread.sleep(300L)
    ScratchRegistry.shutdownAll()
    assert(pr.waitFor(10, TimeUnit.SECONDS), "group leader survived the shutdown path")
    assertCleaned(ev, "(shutdown hook)")
    assertEquals(ScratchRegistry.liveCount, 0)
    assertEquals(RamScratch.cmd(Seq("/usr/bin/pgrep", "-g", pr.pid().toString)).exit, 1)
  }

  tmp.test("F6/F7: group survivors after the kill retries refuse with residue and the unmount is not forced") { dir =>
    val pidFile = dir.resolve("pids")
    val s = stub(dir, s"""sleep 300 &
         |echo $$! >> $pidFile
         |sleep 300""".stripMargin)
    val noKill: GroupKiller = (_, _) => ()
    val ev = new Events
    val cfg = config(s, 1.0, ev).copy(killer = noKill)
    val run = GlmSingleBridge.runAttempt(inputs, cfg)
    try
      assert(run.attempt.groupSurvivors > 0, s"${run.attempt}")
      assert(run.result.left.toOption.exists { case GlmSingleRefusal.ScratchResidue("kill", _, _) => true; case _ => false }, s"${run.result}")
      assert(ev.list.exists { case ScratchEvent.ProcessGroupKilled(_, n) => n > 0; case _ => false })
    finally
      ScratchRegistry.shutdownAll() // the real kill, then a real release
    assertCleaned(ev, "(after the survivors were killed for real)")
    Files.readAllLines(pidFile).asScala.map(_.trim.toLong).foreach(p => assert(!ProcessHandle.of(p).map(_.isAlive).orElse(false)))
  }

  tmp.test("F6: the volume is nosuid/nodev/nobrowse, root mode 0700 before the child starts; mdutil state recorded") { dir =>
    val seen = new java.util.concurrent.atomic.AtomicReference[String]("")
    val ev = new Events
    val hook = onEvent(ev) {
      case _: ScratchEvent.ChildStarted =>
        val root = Paths.get(ev.scratchPath.get)
        val mode = PosixFilePermissions.toString(Files.getPosixFilePermissions(root))
        val mnt = RamScratch.cmd(Seq("/sbin/mount")).out.linesIterator.filter(_.contains(root.getFileName.toString)).mkString
        val md = RamScratch.cmd(Seq("/usr/bin/mdutil", "-s", root.toString)).out.replace('\n', ' ')
        seen.set(s"mode=$mode; mount=[$mnt]; mdutil -s: $md")
    }
    GlmSingleBridge.run(inputs, config(stub(dir, "exit 3"), 60.0, hook))
    println(s"[S6 mount] ${seen.get}")
    assert(seen.get.startsWith("mode=rwx------"), seen.get)
    for o <- Seq("nosuid", "nodev", "nobrowse") do assert(seen.get.contains(o), seen.get)
  }

  tmp.test("F6: an unparseable sidecar's parser message (which can quote result bytes) is not carried") { dir =>
    val npzBytes = storedNpz(Seq("d_beta_data" -> npy(Seq(1, 2), Seq(1.0, 2.0)), "trial_event_index" -> npy(Seq(2), Seq(0.0, 1.0))))
    Files.write(dir.resolve("emit.npz"), npzBytes)
    Files.writeString(dir.resolve("emit.json"), "{\"beta\": SECRETBETA123 ")
    val s = stub(dir, s"""mkdir -p "$$PWD/out"; cp ${dir.resolve("emit.npz")} "$$PWD/out/$Fixture.glmsingle.npz"; cp ${dir.resolve("emit.json")} "$$PWD/out/$Fixture.glmsingle.meta.json"; exit 0""")
    val r = GlmSingleBridge.run(inputs, config(s, 60.0, ScratchHook.none))
    val m = r.left.toOption.getOrElse(fail(s"$r")).message
    assert(m.contains("not parseable") && !m.contains("SECRETBETA"), m)
  }

  tmp.test("a registry failure during setup (JVM shutting down) releases the attached device") { _ =>
    val ev = new Events
    ScratchRegistry.registerFault = Some(new IllegalStateException("Shutdown in progress"))
    val thrown =
      try { RamScratch.host().create(ev); None }
      catch case e: IllegalStateException => Some(e)
      finally ScratchRegistry.registerFault = None
    assert(thrown.isDefined, "the failure must surface")
    val dev = ev.device.getOrElse(fail("no device was attached"))
    assert(!Files.exists(Paths.get(dev)), s"RAM device $dev survives")
    assert(ev.list.exists { case ScratchEvent.Detached(_) => true; case _ => false })
    assertEquals(ScratchRegistry.liveCount, 0)
  }

  // ---- review of S7 (M5, CPU loss, L1) ------------------------------------------------------------------------
  private def alive(pid: Long): Boolean = ProcessHandle.of(pid).map(_.isAlive).orElse(false)

  private def awaitLines(f: Path, n: Int, seconds: Double): Vector[Long] =
    val deadline = System.nanoTime() + (seconds * 1e9).toLong
    def read = Try(Files.readAllLines(f).asScala.map(_.trim).filter(_.nonEmpty).map(_.toLong).toVector).getOrElse(Vector.empty)
    while read.length < n && System.nanoTime() < deadline do Thread.sleep(20L)
    read

  private def pgidOf(pid: Long): Option[Long] =
    RamScratch.cmd(Seq("/bin/ps", "-o", "pgid=", "-p", pid.toString)).out.trim.toLongOption

  tmp.test("M5: when the runner-side parent is SIGKILLed, the watchdog kills the child's own process group within the bound") { dir =>
    val pids = dir.resolve("pids")
    val wpidFile = dir.resolve("wpid")
    // The payload records its pid and a background grandchild's pid, then sleeps (stands in for GLMsingle).
    val payload = s"echo $$$$ >> $pids; sleep 300 & echo $$! >> $pids; sleep 300"
    // An intermediate parent stands in for the runner JVM: it starts the real launcher with ITS pid as the
    // guarded parent (`$$` in sh), records the launcher's pid and waits. Argument $$0 is the watchdog script.
    val intermediate = new ProcessBuilder(
      "/bin/sh", "-c", s"""/usr/bin/perl -e "$$0" $$$$ -- "$$@" & echo $$! > $wpidFile; wait""",
      GlmSingleBridge.WatchdogScript, "/bin/sh", "-c", payload
    ).redirectErrorStream(true).redirectOutput(dir.resolve("intermediate.log").toFile).start()
    var launcher = -1L
    try
      launcher = awaitLines(wpidFile, 1, 10.0).headOption.getOrElse(fail("launcher pid not recorded"))
      val members = awaitLines(pids, 2, 10.0)
      assertEquals(members.length, 2, "payload did not start")
      // The child group is the launcher's own group, not the intermediate's (the sanctioned exception).
      assertEquals(pgidOf(launcher), Some(launcher))
      members.foreach(p => assertEquals(pgidOf(p), Some(launcher), s"pid $p is not in the launcher's group"))
      assertNotEquals(pgidOf(intermediate.pid()), Some(launcher))
      assert(alive(launcher) && members.forall(alive), "group not running before the parent is killed")
      // SIGKILL the parent (what the session does to the runner's group); nothing can be caught.
      val t0 = System.nanoTime()
      intermediate.destroyForcibly(): Unit
      assert(intermediate.waitFor(10, TimeUnit.SECONDS))
      val all = launcher +: members
      val deadline = t0 + 5_000_000_000L
      while all.exists(alive) && System.nanoTime() < deadline do Thread.sleep(10L)
      val secs = (System.nanoTime() - t0) / 1e9
      assert(!all.exists(alive), f"survivors after the parent's death: ${all.filter(alive)} (after $secs%.2f s)")
      assertEquals(RamScratch.cmd(Seq("/usr/bin/pgrep", "-g", launcher.toString)).exit, 1, "the child's process group still has members")
      println(f"[S6 watchdog] child group gone $secs%.3f s after the parent was SIGKILLed (poll ${GlmSingleBridge.WatchdogPollSeconds}%.2f s)")
      assert(secs < 2.0, f"group outlived its parent by $secs%.2f s (bound 2 s)")
    finally
      // Only our own launcher's group; never a broad kill.
      if launcher > 0 && RamScratch.cmd(Seq("/usr/bin/pgrep", "-g", launcher.toString)).exit == 0 then
        RamScratch.cmd(Seq("/bin/kill", "-KILL", "--", s"-$launcher")): Unit
      if intermediate.isAlive then intermediate.destroyForcibly(): Unit
  }

  tmp.test("M5: a launcher whose guarded parent is already gone never runs the command") { dir =>
    val marker = dir.resolve("ran")
    val dead = { val p = new ProcessBuilder("/usr/bin/true").start(); p.waitFor(); p.pid() }
    val pr = new ProcessBuilder(GlmSingleBridge.watchdogCommand(dead, Seq("/usr/bin/touch", marker.toString))*).start()
    assert(pr.waitFor(10, TimeUnit.SECONDS))
    assertEquals(pr.exitValue(), 137)
    assert(!Files.exists(marker), "the command ran under an orphaned launcher")
  }

  tmp.test("M5: under the watchdog, exit status propagates and background stragglers are killed when the command exits") { dir =>
    val pids = dir.resolve("pids")
    val pr = GlmSingleBridge.startGroupLeader(Seq("/bin/sh", "-c", s"sleep 300 & echo $$! >> $pids; exit 5"), dir, Map("PATH" -> "/usr/bin:/bin"))
    assert(pr.waitFor(10, TimeUnit.SECONDS))
    assertEquals(pr.exitValue(), 5)
    val straggler = awaitLines(pids, 1, 5.0).head
    assert(!alive(straggler), s"straggler $straggler survived the command's exit")
    assertEquals(RamScratch.cmd(Seq("/usr/bin/pgrep", "-g", pr.pid().toString)).exit, 1)
  }

  tmp.test("CPU loss: an exception after the child ran keeps its exit, wall and CPU in the attempt") { dir =>
    val burn = stub(dir, "i=0; while [ $i -lt 60000 ]; do i=$((i+1)); done; exit 0")
    val ev = new Events
    val cfg = config(burn, 60.0, ev).copy(probe = {
      case "read-output" => throw new IllegalStateException("injected")
      case _             => ()
    })
    val run = GlmSingleBridge.runAttempt(inputs, cfg)
    assertEquals(run.result.left.toOption, Some(GlmSingleRefusal.LaunchFailed("IllegalStateException")))
    assertEquals(run.attempt.exitCode, Some(0))
    assert(run.attempt.wallSeconds > 0.0, s"${run.attempt}")
    assert(run.attempt.rusageCpuSeconds.exists(_ > 0.0), s"${run.attempt}")
    assert(run.attempt.guardCpuSeconds > 0.0, s"${run.attempt}")
    assertEquals(run.attempt.groupSurvivors, 0)
    assertCleaned(ev, "(exception after the child ran)")
  }

  test("L1: tool children started by RamScratch.cmd never see the root-pipe variables") {
    RamScratch.inheritedEnvForTest = Map("PHRF_ROOT_FD" -> "7", "PHRF_ROOT_ACK_FD" -> "8", "PHRF_CMD_CONTROL" -> "kept")
    val out =
      try RamScratch.cmd(Seq("/usr/bin/env"))
      finally RamScratch.inheritedEnvForTest = Map.empty
    assertEquals(out.exit, 0)
    val names = out.out.linesIterator.map(_.takeWhile(_ != '=')).toSet
    assert(names.contains("PHRF_CMD_CONTROL"), "control: the seeded environment did not reach the child")
    assert(!names.contains("PHRF_ROOT_FD"), "PHRF_ROOT_FD reached a cmd child")
    assert(!names.contains("PHRF_ROOT_ACK_FD"), "PHRF_ROOT_ACK_FD reached a cmd child")
  }

  test("sweep on Linux is skipped unless /dev/shm is a tmpfs mount (not testable on this host)") {
    // On macOS the Linux branch is unreachable; this only checks that the guard exists and answers false here.
    if !System.getProperty("os.name", "").toLowerCase.contains("linux") then assertEquals(RamScratch.shmIsTmpfs(), false)
    assertEquals(RamScratch.sweepStaleWith("Linux", ScratchHook.none, MacTools()).isEmpty || RamScratch.shmIsTmpfs(), true)
  }
