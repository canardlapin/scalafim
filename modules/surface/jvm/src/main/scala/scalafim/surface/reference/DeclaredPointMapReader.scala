package scalafim.surface.reference

import scalafim.surface.SurfaceError

import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

/** Reads a canonical `templateflow4s.point-map/1` directory: `manifest.json`
  * plus one `stage-<i>-displacement.nii` per displacement stage. The manifest
  * bytes are checked against the caller's expected SHA-256 before parsing;
  * every stage file is read once and verified against the manifest digest and
  * header before it is decoded; a quarantined manifest is refused.
  */
object DeclaredPointMapReader:
  def read(directory: Path, expectedSourceSha256: String, expectedManifestSha256: String): Either[ReferenceError, DeclaredPointMap] =
    for
      bytes <- attempt(directory)(Files.readAllBytes(directory.resolve("manifest.json")))
      manifest <- PointMapManifest.verified(bytes, expectedManifestSha256, parse)
      declared <- DeclaredPointMap.fromManifest(manifest, expectedSourceSha256, name =>
        val file = directory.resolve(name)
        if name.contains('/') || name.contains('\\') || !Files.isRegularFile(file) then None
        else scala.util.Try(Files.readAllBytes(file)).toOption)
    yield declared

  /** Parse the manifest fields ScalaFIM relies on; other fields are ignored. */
  def parse(text: String): Either[String, ManifestFields] =
    try
      val json = ujson.read(text)
      val source = json("source")
      val frames = json("frames")
      val derivation = frames.obj.get("derivation").filterNot(_.isNull).map(_.str).getOrElse("")
      val declaredQuarantine = frames.obj.get("quarantine").filterNot(_.isNull)
        .map(q => PointMapQuarantine(q("archivePath").str, q.obj.get("reason").map(_.str).getOrElse("quarantined")))
      val quarantine = declaredQuarantine.orElse(
        Option.when(derivation.contains("QUARANTINED"))(PointMapQuarantine(source("archivePath").str, derivation)))
      val stages = json("stages").arr.toVector.map: stage =>
        stage("kind").str match
          case "affine" => ManifestStage.AffineEntry(rows(stage("matrix")))
          case "displacement" =>
            require(stage.obj.get("dtype").forall(_.str == "float64"), "displacement dtype must be float64")
            require(stage.obj.get("byteOrder").forall(_.str == "little-endian"), "displacement byte order must be little-endian")
            ManifestStage.DisplacementEntry(stage("file").str, stage("sha256").str, stage("bytes").num.toLong,
              stage("dims").arr.toVector.map(_.num.toInt), rows(stage("voxelToRas")))
          case other => throw new IllegalArgumentException(s"unknown stage kind '$other'")
      Right(ManifestFields(
        json("schema").str,
        PointMapSource(source("archivePath").str, source("sha256").str, source("bytes").num.toLong,
          source.obj.get("catalogRevision").filterNot(_.isNull).map(_.str)),
        frames("input").str,
        frames("output").str,
        derivation,
        quarantine,
        stages))
    catch case NonFatal(error) => Left(SurfaceError.reason(error))

  private def rows(value: ujson.Value): Vector[Double] =
    value.arr.toVector.flatMap(_.arr.toVector.map(_.num))

  private def attempt[A](path: Path)(body: => A): Either[ReferenceError, A] =
    try Right(body)
    catch case NonFatal(error) => Left(ReferenceError.AssetReadFailure(s"$path: ${SurfaceError.reason(error)}"))
