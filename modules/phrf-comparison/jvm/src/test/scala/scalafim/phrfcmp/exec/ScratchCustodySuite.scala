package scalafim.phrfcmp.exec

import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger

import scalafim.phrfcmp.run.ScratchEvent

class ScratchCustodySuite extends munit.FunSuite:

  private final class Capture extends CustodySink:
    var events = Vector.empty[(String, Map[String, String])]
    def record(event: String, fields: Vector[(String, String)]): Unit = events :+= ((event, fields.toMap))

  private val all: Vector[ScratchEvent] = Vector(
    ScratchEvent.Created("ram", "/Volumes/phrfcmp-ram-1-ab"),
    ScratchEvent.DeviceAttached("/dev/disk9", 262144L),
    ScratchEvent.Formatted("/dev/disk9", "HFS+"),
    ScratchEvent.Mounted("/Volumes/phrfcmp-ram-1-ab", "noindex"),
    ScratchEvent.IndexingGuard("mdutil off"),
    ScratchEvent.ChildStarted(100L, 100L),
    ScratchEvent.ProcessGroupKilled(100L, 0),
    ScratchEvent.Wiped(37, 123456789L, 0),
    ScratchEvent.Unmounted("/Volumes/phrfcmp-ram-1-ab"),
    ScratchEvent.Detached("/dev/disk9"),
    ScratchEvent.Removed("/Volumes/phrfcmp-ram-1-ab"),
    ScratchEvent.Verified(true, true),
    ScratchEvent.StepFailed("hdiutil", "boom\nwith\u0000control"),
    ScratchEvent.StaleSwept("/Volumes/phrfcmp-ram-9-zz", Some("/dev/disk8"), 9L, "detached")
  )

  test("every scratch event is mapped to a custody event, including StaleSwept") {
    val c = new Capture
    val hook = new ScratchCustodyHook(c)
    all.foreach(hook.event)
    assertEquals(c.events.length, all.length)
    assertEquals(c.events.map(_._1).distinct.length, all.length)
    val stale = c.events.last
    assertEquals(stale._1, "scratch-stale-swept")
    assertEquals(stale._2, Map("path" -> "/Volumes/phrfcmp-ram-9-zz", "device" -> "/dev/disk8", "owner_pid" -> "9", "outcome" -> "detached"))
  }

  test("Wiped logs the residue only: no file count and no byte count of the wiped result") {
    val c = new Capture
    new ScratchCustodyHook(c).event(ScratchEvent.Wiped(37, 123456789L, 2))
    assertEquals(c.events.head._2, Map("residue" -> "2"))
    assert(!c.events.toString.contains("123456789"))
  }

  test("control characters are replaced and a throwing sink never propagates") {
    val c = new Capture
    new ScratchCustodyHook(c).event(ScratchEvent.StepFailed("s", "a\nb\u0000c"))
    assertEquals(c.events.head._2("detail"), "a?b?c")
    val bad: CustodySink = (_, _) => throw new IllegalStateException("sink down")
    new ScratchCustodyHook(bad).event(ScratchEvent.Removed("/x"))
  }

  test("chained log: appends verify, restart continues the chain, rewriting or truncating the middle is detected") {
    val path = Files.createTempDirectory("phrf-s7-custody-").resolve("runner.log.jsonl")
    val log = new ChainedCustodyLog(path, () => 1L)
    val hook = new ScratchCustodyHook(log)
    all.take(5).foreach(hook.event)
    val head = log.headHash
    assertEquals(ChainedCustodyLog.verify(path), Right((5L, head)))
    val log2 = new ChainedCustodyLog(path, () => 2L)
    new ScratchCustodyHook(log2).event(ScratchEvent.Removed("/y"))
    assertEquals(ChainedCustodyLog.verify(path).map(_._1), Right(6L))
    val lines = Files.readAllLines(path)
    // rewrite a field in the middle
    val tampered = new java.util.ArrayList[String](lines)
    tampered.set(2, lines.get(2).replace("scratch-formatted", "scratch-xxxxxxxxx"))
    Files.write(path, tampered)
    assert(ChainedCustodyLog.verify(path).isLeft)
    // drop a middle line
    val dropped = new java.util.ArrayList[String](lines)
    dropped.remove(3)
    Files.write(path, dropped)
    assert(ChainedCustodyLog.verify(path).isLeft)
    // a log that does not verify cannot be appended to
    intercept[IllegalStateException](new ChainedCustodyLog(path))
  }

  test("the runner sweeps stale scratch once at start, and a throwing sweep does not stop the run") {
    val owner = TestOwner.random()
    val out = Files.createTempDirectory("phrf-s7-sweep-")
    val plan = PilotPlan(Vector(PilotCell(CellId.parse("C0").toOption.get, Vector(ArmId.parse("a").toOption.get))), 2)
    val store = SealedStore.open(out.resolve("sealed"), owner.recipient, owner.fingerprint).toOption.get
    val n = new AtomicInteger(0)
    val arms: ArmRunner = _ => ArmResult.Done()
    val r = new PilotRunner(plan, out, PilotStamp(Vector("k" -> "v")), store, new PilotRoot(1L), arms, sweepStale = () => { n.incrementAndGet(); throw new IllegalStateException("sweep") })
    assert(r.run().isRight)
    assertEquals(n.get(), 1)
  }

  test("the real start-of-run sweep reports through the hook and does not throw on this host") {
    val c = new Capture
    ScratchCustody.sweepAtStart(c)()
    c.events.foreach(e => assertEquals(e._1, "scratch-stale-swept"))
  }
