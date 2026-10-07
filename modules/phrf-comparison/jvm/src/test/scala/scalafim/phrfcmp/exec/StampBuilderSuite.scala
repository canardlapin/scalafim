package scalafim.phrfcmp.exec

import java.nio.file.{Files, Path}

class StampBuilderSuite extends munit.FunSuite:

  private def fieldMap(s: PilotStamp): Map[String, String] = s.fields.toMap

  private def sh(dir: Path, args: String*): Unit =
    val p = new ProcessBuilder(args*).directory(dir.toFile).redirectErrorStream(true).start()
    val out = new String(p.getInputStream.readAllBytes())
    assertEquals(p.waitFor(), 0, out)

  private def commit(repo: Path): Unit =
    sh(repo, "git", "add", "-A")
    sh(repo, "git", "-c", "user.name=t", "-c", "user.email=t@example.invalid", "commit", "-q", "-m", "x")

  private final class Repo:
    val dir: Path = Files.createTempDirectory("phrf-s7-stamp-")
    sh(dir, "git", "init", "-q")
    Files.writeString(dir.resolve("manifest.json"), "{\"v\":0}")
    Files.writeString(dir.resolve("from_generator.py"), "print('converter')\n")
    Files.writeString(dir.resolve("parity.json"), "{\"can\":1}")
    commit(dir)

  private def cell(s: String): CellId = CellId.parse(s).fold(sys.error, identity)
  private def arm(s: String): ArmId = ArmId.parse(s).fold(sys.error, identity)
  private val plan = PilotPlan(Vector(PilotCell(cell("C-1"), Vector(arm("can")))), 20)

  private def inputs(r: Repo, prefix: Option[String] = None, timeout: Int = 600, p: PilotPlan = plan): StampInputs =
    StampInputs(
      repo = r.dir,
      requireCleanTree = true,
      frozenManifests = Vector("v0" -> r.dir.resolve("manifest.json")),
      generatorCodeSha256 = "b4ba7ac9",
      fromGeneratorPy = r.dir.resolve("from_generator.py"),
      fromGeneratorPrefix = prefix,
      glmsinglePin = "glmsingle@deadbeef lock=1234",
      parityReceipts = Vector("can" -> r.dir.resolve("parity.json")),
      plan = p,
      glmsingleTimeoutSeconds = timeout
    )

  private def stamp(i: StampInputs, props: Iterable[String] = Nil): PilotStamp =
    StampBuilder.build(i, props).fold(r => fail(r.message), identity)

  test("stamp binds git state, hashes, pin, java, plan, timeout and thread pinning") {
    val r = new Repo
    val s = stamp(inputs(r))
    val keys = s.fields.map(_._1).toSet
    Set("git_sha", "git_clean", "manifest:v0:sha256", "generator_code_sha256", "from_generator_py_sha256", "glmsingle_pin",
      "parity:can:sha256", "java_version", "java_vendor", "redirected_builds", "pilot_plan_sha256", "glmsingle_timeout_seconds",
      "thread_pinning").foreach(k => assert(keys.contains(k), s"missing $k"))
    assertEquals(fieldMap(s)("git_clean"), "true")
    assertEquals(fieldMap(s)("glmsingle_timeout_seconds"), "600")
    assert(!keys.exists(k => k.contains("ceiling") || k.contains("soft") || k.contains("hard") || k.contains("threads_parallel")))
    assertEquals(stamp(inputs(r)), s)
  }

  test("each bound input changes the stamp (the first difference names it)") {
    val r = new Repo
    val base = stamp(inputs(r))
    assertEquals(base.firstDifference(stamp(inputs(r, timeout = 601))), Some("glmsingle_timeout_seconds"))
    val bigger = plan.copy(datasets = 21)
    assertEquals(base.firstDifference(stamp(inputs(r, p = bigger))), Some("pilot_plan_sha256"))
    assertEquals(base.firstDifference(stamp(inputs(r).copy(glmsinglePin = "other"))), Some("glmsingle_pin"))
    assertEquals(base.firstDifference(stamp(inputs(r).copy(generatorCodeSha256 = "x"))), Some("generator_code_sha256"))
    Files.writeString(r.dir.resolve("manifest.json"), "{\"v\":1}")
    commit(r.dir)
    val d = base.firstDifference(stamp(inputs(r))).get
    assert(d == "git_sha" || d == "manifest:v0:sha256")
    assert(fieldMap(base)("manifest:v0:sha256") != fieldMap(stamp(inputs(r)))("manifest:v0:sha256"))
  }

  test("dirty tree refuses when required and is flagged otherwise") {
    val r = new Repo
    Files.writeString(r.dir.resolve("scratch.txt"), "x")
    val refused = StampBuilder.build(inputs(r), Nil)
    assertEquals(refused.left.toOption.map(_.isInstanceOf[PilotRefusal.DirtyWorktree]), Some(true))
    val lax = StampBuilder.build(inputs(r).copy(requireCleanTree = false), Nil).toOption.get
    assertEquals(fieldMap(lax)("git_clean"), "false")
  }

  test("any scalafim.*.build property refuses a redirected build") {
    val r = new Repo
    val refused = StampBuilder.build(inputs(r), Seq("scalafim.gale.build", "user.dir"))
    assertEquals(refused.left.toOption, Some(PilotRefusal.RedirectedBuild(Vector("scalafim.gale.build"))))
    assertEquals(StampBuilder.redirectedBuildProperties(Seq("scalafim.image4s.build", "scalafim.other", "x.build")), Vector("scalafim.image4s.build"))
  }

  test("from_generator.py must carry the frozen hash prefix") {
    val r = new Repo
    val refused = StampBuilder.build(inputs(r, prefix = Some(StampBuilder.FromGeneratorPrefix)), Nil)
    assertEquals(refused.left.toOption.map(_.isInstanceOf[PilotRefusal.FrozenFileChanged]), Some(true))
    val sha = Fs.sha256File(r.dir.resolve("from_generator.py"))
    assert(StampBuilder.build(inputs(r, prefix = Some(sha.take(8))), Nil).isRight)
  }

  test("a missing frozen file refuses") {
    val r = new Repo
    val bad = inputs(r).copy(parityReceipts = Vector("rlss" -> r.dir.resolve("nope.json")))
    assert(StampBuilder.build(bad, Nil).isLeft)
  }

  test("stamp records the GLMsingle interpreter, its environment, the host architecture and the lockfile hash") {
    val r = new Repo
    val py = Files.createTempFile("phrf-s7-py", "")
    Files.writeString(py, "#!/bin/sh\n")
    val lock = r.dir.resolve("requirements.lock")
    Files.writeString(lock, "numpy==1.0\n")
    commit(r.dir)
    val s = stamp(inputs(r).copy(glmSinglePython = Some(py), lockfiles = Vector("glmsingle" -> lock)))
    val m = s.fields.toMap
    assertEquals(m("glmsingle_python_sha256"), Fs.sha256File(py))
    assertEquals(m("glmsingle_python_path"), py.toRealPath().toString)
    assertEquals(m("glmsingle_env"), scalafim.phrfcmp.run.GlmSingleEnv.description)
    assertEquals(m("lockfile:glmsingle:sha256"), Fs.sha256File(lock))
    Set("host_jvm_arch", "host_uname_m", "host_translated", "os").foreach(k => assert(m.contains(k), k))
    assertNotEquals(m("host_uname_m"), "")
    Files.writeString(py, "#!/bin/sh\necho changed\n")
    assertEquals(s.firstDifference(stamp(inputs(r).copy(glmSinglePython = Some(py), lockfiles = Vector("glmsingle" -> lock)))), Some("glmsingle_python_sha256"))
    assert(StampBuilder.build(inputs(r).copy(glmSinglePython = Some(r.dir.resolve("no-such-python"))), Nil).isLeft)
  }
