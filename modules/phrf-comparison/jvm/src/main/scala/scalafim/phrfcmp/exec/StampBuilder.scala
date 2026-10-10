package scalafim.phrfcmp.exec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import scalafim.phrfcmp.run.GlmSingleEnv

/** The frozen pilot cell table: cells, their arms, and the dataset counts. Part of the stamp (design 5.2, 5.3). */
object PilotPlan:
  /** A sane bound, so that `maxRetries + 1` attempts never overflows anywhere (third review L3). */
  val MaxRetries: Int = 100

final case class PilotPlan(cells: Vector[PilotCell], datasets: Int, probeDatasets: Int = 2, minDatasets: Int = PartialPilot.MinDatasets, maxRetries: Int = 2):
  require(cells.nonEmpty && cells.map(_.id).distinct.length == cells.length, "cells must be non-empty and distinct")
  require(datasets >= 1 && probeDatasets >= 1 && minDatasets >= 2 && maxRetries >= 0, "positive plan sizes")
  require(maxRetries <= PilotPlan.MaxRetries, s"at most ${PilotPlan.MaxRetries} retries")
  require(datasets <= SealedNames.MaxDatasets, s"at most ${SealedNames.MaxDatasets} datasets (4-digit dNNNN names)")

  /** Canonical rendering, hashed into the stamp. */
  def canonical: String =
    val table = cells.map(c => s"${c.id.value}:${c.arms.map(_.value).mkString(",")}").mkString(";")
    s"cells=$table|datasets=$datasets|probe=$probeDatasets|min=$minDatasets|retries=$maxRetries"

/** Everything the stamp binds. Ceilings are deliberately absent (they do not change results); so is the runner's
  * dataset parallelism (results must not depend on it, F11).
  *
  * @param frozenManifests   name and path of each frozen manifest file (hashed)
  * @param generatorCodeSha256 the frozen generator code hash (string, as bound by ingestion)
  * @param fromGeneratorPy   the GLMsingle converter; refused unless its sha256 starts with `fromGeneratorPrefix`
  * @param glmsinglePin      GLMsingle commit and lockfile identity, as one string
  * @param parityReceipts    name and path of each parity receipt (hashed)
  */
final case class StampInputs(
    repo: Path,
    requireCleanTree: Boolean,
    frozenManifests: Vector[(String, Path)],
    generatorCodeSha256: String,
    fromGeneratorPy: Path,
    fromGeneratorPrefix: Option[String],
    glmsinglePin: String,
    parityReceipts: Vector[(String, Path)],
    plan: PilotPlan,
    glmsingleTimeoutSeconds: Int,
    buildSbt: Option[Path] = None,
    lockfiles: Vector[(String, Path)] = Vector.empty,
    phrfSourceDirs: Vector[Path] = Vector.empty,
    whitelistSchemaVersion: String = "unset",
    dfMappingRuleVersion: String = "unset",
    glmSinglePython: Option[Path] = None,
    extra: Vector[(String, String)] = Vector.empty
)

object StampBuilder:
  /** sha256 prefix of the frozen `from_generator.py`. */
  val FromGeneratorPrefix: String = "c5a8c0dc"

  /** System properties that redirect a source dependency to a local checkout (`scalafim.<dep>.build`). */
  def redirectedBuildProperties(props: Iterable[String] = sys.props.keySet): Vector[String] =
    props.toVector.filter(k => k.startsWith("scalafim.") && k.endsWith(".build")).sorted

  private def git(repo: Path, args: String*): Either[PilotRefusal, String] =
    try
      val p = ChildEnvironment.scrub(new ProcessBuilder(("git" +: "-C" +: repo.toString +: args)*).redirectErrorStream(true)).start()
      val out = new String(p.getInputStream.readAllBytes(), UTF_8).trim
      if p.waitFor() == 0 then Right(out) else Left(PilotRefusal.Failure(s"git ${args.mkString(" ")} failed: $out"))
    catch case e: java.io.IOException => Left(PilotRefusal.Failure(s"git unavailable: ${e.getMessage}"))

  private def hashed(kind: String, files: Vector[(String, Path)]): Either[PilotRefusal, Vector[(String, String)]] =
    files.foldLeft[Either[PilotRefusal, Vector[(String, String)]]](Right(Vector.empty)) { (acc, nf) =>
      acc.flatMap { done =>
        val (name, path) = nf
        if Files.isRegularFile(path) then Right(done :+ (s"$kind:$name:sha256" -> Fs.sha256File(path)))
        else Left(PilotRefusal.FrozenFileChanged(s"$kind:$name", s"missing file $path"))
      }
    }

  /** Digest of every regular file under the given directories (sorted relative path plus content hash). */
  private def sourceDigest(dirs: Vector[Path]): String =
    Fs.sha256(dirs.flatMap(d => Fs.listFiles(d).map(p => s"${d.getFileName}/${d.relativize(p)} ${Fs.sha256File(p)}")).sorted.mkString("\n").getBytes(UTF_8))

  private def probe(cmd: String*): String =
    try
      val p = ChildEnvironment.scrub(new ProcessBuilder(cmd*).redirectErrorStream(true)).start()
      val out = new String(p.getInputStream.readAllBytes(), UTF_8).trim.take(80)
      if p.waitFor() == 0 && out.nonEmpty then out else "unknown"
    catch case _: java.io.IOException => "unknown"

  /** Host architecture (S10 rehearsal item): the JVM's own view, the kernel's, and Rosetta translation on macOS. */
  def hostArchitecture(): Vector[(String, String)] =
    Vector(
      "host_jvm_arch" -> sys.props.getOrElse("os.arch", "unknown"),
      "host_uname_m" -> probe("uname", "-m"),
      "host_translated" -> (if sys.props.getOrElse("os.name", "").toLowerCase.contains("mac") then probe("sysctl", "-n", "sysctl.proc_translated") else "n/a")
    )

  /** Interpreter identity: the resolved path and the SHA-256 of the interpreter file. */
  private def interpreter(python: Option[Path]): Either[PilotRefusal, Vector[(String, String)]] = python match
    case None => Right(Vector.empty)
    case Some(p) =>
      try
        val real = p.toRealPath()
        Right(Vector("glmsingle_python_path" -> real.toString, "glmsingle_python_sha256" -> Fs.sha256File(real), "glmsingle_env" -> GlmSingleEnv.description))
      catch case _: java.io.IOException => Left(PilotRefusal.FrozenFileChanged("glmsingle_python", s"missing interpreter $p"))

  def build(in: StampInputs, props: Iterable[String] = sys.props.keySet): Either[PilotRefusal, PilotStamp] =
    val redirected = redirectedBuildProperties(props)
    for
      _ <- if redirected.nonEmpty then Left(PilotRefusal.RedirectedBuild(redirected)) else Right(())
      head <- git(in.repo, "rev-parse", "HEAD")
      status <- git(in.repo, "status", "--porcelain")
      _ <- if in.requireCleanTree && status.nonEmpty then Left(PilotRefusal.DirtyWorktree(status.linesIterator.take(5).mkString("; "))) else Right(())
      manifests <- hashed("manifest", in.frozenManifests)
      parity <- hashed("parity", in.parityReceipts)
      locks <- hashed("lockfile", in.lockfiles)
      sbt <- hashed("build", in.buildSbt.toVector.map("build.sbt" -> _))
      py <- interpreter(in.glmSinglePython)
      converter <- hashed("converter", Vector("from_generator.py" -> in.fromGeneratorPy))
      _ <- in.fromGeneratorPrefix match
        case Some(prefix) if !converter.head._2.startsWith(prefix) =>
          Left(PilotRefusal.FrozenFileChanged("from_generator.py", s"sha256 ${converter.head._2} does not start with $prefix"))
        case _ => Right(())
    yield PilotStamp(
      Vector(
        "git_sha" -> head,
        "git_clean" -> status.isEmpty.toString
      ) ++ manifests ++ Vector(
        "generator_code_sha256" -> in.generatorCodeSha256,
        "from_generator_py_sha256" -> converter.head._2,
        "glmsingle_pin" -> in.glmsinglePin
      ) ++ parity ++ locks ++ sbt ++ Vector(
        "java_version" -> sys.props.getOrElse("java.version", "unknown"),
        "java_vendor" -> sys.props.getOrElse("java.vendor", "unknown"),
        "os" -> s"${sys.props.getOrElse("os.name", "unknown")} ${sys.props.getOrElse("os.version", "unknown")} ${sys.props.getOrElse("os.arch", "unknown")}",
        "phrf_source_sha256" -> sourceDigest(in.phrfSourceDirs),
        "whitelist_schema_version" -> in.whitelistSchemaVersion,
        "df_mapping_rule_version" -> in.dfMappingRuleVersion,
        "redirected_builds" -> "none",
        "pilot_plan_sha256" -> Fs.sha256(in.plan.canonical.getBytes(UTF_8)),
        "glmsingle_timeout_seconds" -> in.glmsingleTimeoutSeconds.toString,
        "thread_pinning" -> ThreadPinning.description
      ) ++ py ++ hostArchitecture() ++ in.extra
    )
