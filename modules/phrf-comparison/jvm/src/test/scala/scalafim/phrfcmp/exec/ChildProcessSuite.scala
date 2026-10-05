package scalafim.phrfcmp.exec

class ChildProcessSuite extends munit.FunSuite:

  test("timeout kills a sleeping child from the JVM") {
    val r = ChildProcess.run(Seq("sleep", "30"), timeoutSeconds = 0.5)
    assert(r.timedOut)
    assertEquals(r.exitCode, None)
    assert(r.wallSeconds < 10.0, s"took ${r.wallSeconds}s; the child was not killed")
  }

  test("timeout kills the whole process tree, not just the direct child") {
    val r = ChildProcess.run(Seq("sh", "-c", "sleep 30 & echo $!; wait"), timeoutSeconds = 1.0)
    assert(r.timedOut)
    val pid = r.stdout.trim.linesIterator.next().trim.toLong
    val deadline = System.nanoTime() + 5_000_000_000L
    def alive = ProcessHandle.of(pid).map(_.isAlive).orElse(false)
    while alive && System.nanoTime() < deadline do Thread.sleep(20)
    assert(!alive, s"grandchild $pid survived the timeout")
  }

  test("a child that finishes in time reports its exit code, output, and pinned thread environment") {
    val r = ChildProcess.run(Seq("sh", "-c", "echo $OMP_NUM_THREADS $MKL_NUM_THREADS $OPENBLAS_NUM_THREADS; exit 3"), timeoutSeconds = 20.0)
    assert(!r.timedOut)
    assertEquals(r.exitCode, Some(3))
    assertEquals(r.stdout.trim, "1 1 1")
    assert(r.cpuSeconds >= 0.0)
  }

  test("L1: a child never sees the root pipe variables, even when the caller's environment names them") {
    val leaky = ThreadPinning.Environment ++ Map(RootIntake.RootVar -> "5", RootIntake.AckVar -> "6")
    val r = ChildProcess.run(Seq("sh", "-c", "echo ${PHRF_ROOT_FD-unset}/${PHRF_ROOT_ACK_FD-unset}"), timeoutSeconds = 20.0, env = leaky)
    assertEquals(r.stdout.trim, "unset/unset")
  }

  test("L1/F8: every process spawn site under exec/ and in run/GlmSingleBridge scrubs or clears the environment (source scan)") {
    val main = Repo.root.resolve("modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp")
    val files = Fs.listFiles(main.resolve("exec")).filter(_.toString.endsWith(".scala")) :+ main.resolve("run/GlmSingleBridge.scala")
    assert(files.forall(java.nio.file.Files.isRegularFile(_)), s"missing sources: $files")
    val Spawn = """new\s+ProcessBuilder\s*\(""".r
    val Assigned = """val\s+(\w+)\s*=\s*$""".r
    val sites = files.flatMap { f =>
      val src = java.nio.file.Files.readString(f)
      assert(!src.contains("getRuntime.exec") && !src.contains("getRuntime().exec"), s"${f.getFileName}: Runtime.exec bypasses the scrub")
      Spawn.findAllMatchIn(src).map { m =>
        val before = src.substring(0, m.start)
        val line = before.substring(before.lastIndexOf('\n') + 1)
        val wrapped = before.endsWith("ChildEnvironment.scrub(")
        // `val pb = new ProcessBuilder(...)`: before the first start of `pb`, either scrub it or clear its environment
        val assignedAndSafe = Assigned.findFirstMatchIn(line).exists { a =>
          val name = a.group(1)
          val after = src.substring(m.end)
          val plain = after.indexOf(s"$name.start()")
          val viaScrub = after.indexOf(s"ChildEnvironment.scrub($name).start()")
          if viaScrub >= 0 && (plain < 0 || viaScrub < plain) then true // scrubbed in the start expression itself
          else
            plain >= 0 && {
              val region = after.substring(0, plain)
              region.contains(s"ChildEnvironment.scrub($name)") || region.contains(s"$name.environment().clear()")
            }
        }
        assert(wrapped || assignedAndSafe, s"${f.getFileName}: spawn site at offset ${m.start} neither scrubs nor clears the environment")
        f.getFileName.toString
      }.toVector
    }
    assertEquals(sites.sorted, Vector("ChildProcess.scala", "GlmSingleBridge.scala", "GlmSingleBridge.scala", "StampBuilder.scala", "StampBuilder.scala"), "the known spawn sites")
    // the cleared-environment site (the GLMsingle group leader) gets only this whitelist, which never names the root pipe
    val env = scalafim.phrfcmp.run.GlmSingleEnv.forScratch(java.nio.file.Path.of("/scratch"), java.nio.file.Path.of("/py"))
    assert(env.keySet.intersect(ChildEnvironment.Scrubbed).isEmpty, env.keySet.toString)
  }

  test("thread pinning description is stamped content") {
    assertEquals(
      ThreadPinning.description,
      "threads=1;MKL_NUM_THREADS=1;OMP_NUM_THREADS=1;OPENBLAS_NUM_THREADS=1"
    )
  }
