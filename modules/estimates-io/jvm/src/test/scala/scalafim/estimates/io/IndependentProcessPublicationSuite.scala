package scalafim.estimates.io

import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.*

class IndependentProcessPublicationSuite extends munit.FunSuite:
  private val probe = "scalafim.estimates.io.IndependentProcessPublicationProbe"
  private val probeResource = "scalafim/estimates/io/IndependentProcessPublicationProbe$.class"
  private val javaExecutable = Path.of(System.getProperty("java.home"), "bin", "java").toString
  private val maximumCapturedOutput = 64 * 1024

  private def classpath: String =
    def urls(loader: ClassLoader | Null): Vector[java.net.URL] = loader match
      case null => Vector.empty
      case value: URLClassLoader => value.getURLs.toVector ++ urls(value.getParent)
      case value => urls(value.getParent)
    val values = urls(getClass.getClassLoader).distinct
    require(getClass.getClassLoader.getResource(probeResource) != null, s"test classloader cannot resolve $probeResource")
    require(values.nonEmpty, "test classloader exposes no URL closure")
    values.map(url => Path.of(url.toURI).toString).mkString(java.io.File.pathSeparator)

  private def digest(value: String): String =
    MessageDigest.getInstance("SHA-256").digest(value.getBytes(java.nio.charset.StandardCharsets.UTF_8)).map("%02x".format(_)).mkString

  private def root(name: String): Path = Files.createTempDirectory(s"independent-process-publication-$name-")

  private final class Capture:
    private val buffer = new StringBuilder
    private var dropped = 0
    def append(line: String): Unit = synchronized:
      val text = line + "\n"
      val available = maximumCapturedOutput - buffer.length
      if available > 0 then buffer.append(text.take(available))
      dropped += Math.max(0, text.length - available)
    def text: String = synchronized(s"$buffer[dropped=$dropped]")

  private final case class OwnedChild(process: Process, capture: Capture, reader: Thread):
    def cleanup(): Unit =
      if process.isAlive then process.destroyForcibly()
      if !process.waitFor(5, TimeUnit.SECONDS) then throw new IllegalStateException(s"owned child ${process.pid()} did not terminate")
      reader.join(5000)
      if reader.isAlive then throw new IllegalStateException(s"owned child reader ${process.pid()} did not terminate")
    def result: (Long, Int, String) =
      if process.isAlive || reader.isAlive then throw new IllegalStateException(s"owned child ${process.pid()} is not stopped")
      (process.pid(), process.exitValue(), capture.text)

  private def cleanup(owned: Iterable[OwnedChild]): Unit =
    var failure: Option[Throwable] = None
    owned.foreach: child =>
      try child.cleanup()
      catch
        case error: Throwable =>
          failure match
            case Some(first) => first.addSuppressed(error)
            case None => failure = Some(error)
    failure.foreach: error =>
      throw error

  private def start(args: String*): OwnedChild =
    val capture = new Capture
    val process = new ProcessBuilder((Vector(javaExecutable, "-cp", classpath, probe) ++ args).toArray*).redirectErrorStream(true).start()
    val reader = new Thread(() =>
      val source = scala.io.Source.fromInputStream(process.getInputStream)
      try source.getLines().foreach(capture.append)
      finally source.close())
    try
      reader.start()
      OwnedChild(process, capture, reader)
    catch
      case error: Throwable =>
        if process.isAlive then process.destroyForcibly()
        if !process.waitFor(5, TimeUnit.SECONDS) then
          error.addSuppressed(new IllegalStateException(s"owned child ${process.pid()} did not terminate after start failure"))
        throw error

  private def run(args: String*): (Long, Int, String) =
    val owned = ArrayBuffer.empty[OwnedChild]
    try
      val child = start(args*)
      owned += child
      if !child.process.waitFor(30, TimeUnit.SECONDS) then fail(s"owned child timed out: ${args.mkString(" ")}")
      child.reader.join(5000)
      child.result
    finally cleanup(owned)

  private def race(root: Path, left: String, right: String): Vector[(Long, Int, String)] =
    val readyLeft = root.resolve(s"$left.ready")
    val readyRight = root.resolve(s"$right.ready")
    val go = root.resolve("go")
    val owned = ArrayBuffer.empty[OwnedChild]
    try
      val first = start("publish", root.toString, left, readyLeft.toString, go.toString)
      owned += first
      val second = start("publish", root.toString, right, readyRight.toString, go.toString)
      owned += second
      val deadline = System.nanoTime() + 20.seconds.toNanos
      while (!Files.exists(readyLeft) || !Files.exists(readyRight)) && System.nanoTime() < deadline do Thread.sleep(10)
      if !Files.exists(readyLeft) || !Files.exists(readyRight) then
        fail(s"owned children did not reach the barrier: ${owned.map(child => s"pid=${child.process.pid()} output=${child.capture.text}").mkString("; ")}")
      Files.createFile(go)
      owned.foreach: child =>
        if !child.process.waitFor(30, TimeUnit.SECONDS) then fail(s"owned child ${child.process.pid()} timed out after barrier")
        child.reader.join(5000)
      owned.toVector.map(_.result)
    finally cleanup(owned)

  private def winner(results: Vector[(Long, Int, String)]): String =
    val pass = results.filter(_._3.contains("PUBLISH_PASS"))
    assertEquals(pass.size, 1)
    "label=([a-z-]+)".r.findFirstMatchIn(pass.head._3).map(_.group(1)).getOrElse(fail(s"missing winner label: ${pass.head._3}"))

  test("two independent JVMs race compatible unit publication, then an explicit fresh-process merge preserves both"):
    val store = root("compatible")
    val results = race(store, "compatible-a", "compatible-b")
    assertEquals(results.count(_._2 == 0), 2)
    assertEquals(results.count(_._3.contains("PUBLISH_CONFLICT")), 1)
    val selected = winner(results)
    val observed = run("observe", store.toString, selected)
    assertEquals(observed._2, 0)
    assert(observed._3.contains(s"OBSERVE_PASS label=$selected") && observed._3.contains("digest="))
    val merged = run("merge", store.toString)
    assertEquals(merged._2, 0)
    assert(merged._3.contains("MERGE_PASS") && merged._3.contains("observed="))
    val checked = run("check", store.toString, "compatible-a,compatible-b")
    assertEquals(checked._2, 0)
    assert(checked._3.contains("CHECK_PASS") && checked._3.contains("digest="))
    println(s"PROCESS_RACE_RECEIPT java=$javaExecutable cpSha256=${digest(classpath)} winner=$selected publish=$results observe=$observed merge=$merged check=$checked")

  test("two independent JVMs cannot silently merge conflicting revisions of the same unit"):
    val store = root("conflict")
    val results = race(store, "conflict-a", "conflict-b")
    assertEquals(results.count(_._2 == 0), 2)
    assertEquals(results.count(_._3.contains("PUBLISH_CONFLICT")), 1)
    val selected = winner(results)
    val observed = run("observe", store.toString, selected)
    assertEquals(observed._2, 0)
    assert(observed._3.contains(s"OBSERVE_PASS label=$selected"))
    val loser = if selected == "conflict-a" then "conflict-b" else "conflict-a"
    val retained = run("check", store.toString, selected)
    val rejected = run("check", store.toString, loser)
    assertEquals(retained._2, 0)
    assert(rejected._2 != 0)
    println(s"PROCESS_CONFLICT_RECEIPT java=$javaExecutable cpSha256=${digest(classpath)} winner=$selected loser=$loser publish=$results observe=$observed retained=$retained rejected=$rejected")
