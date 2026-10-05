package scalafim.phrfcmp.ingest

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.util.Try

/** JVM-only file IO around the shared, pure binding. */
object PhrfDatasetLoader:

  /** Load `<dir>/<stem>.npz` and `<dir>/<stem>.manifest.json`, e.g. stem `T-TX-fast__d0003`. */
  def load(dir: Path, stem: String, expect: IngestExpectation): Either[IngestRefusal, BoundDataset] =
    for
      manifest <- read(dir.resolve(s"$stem.manifest.json")).map(new String(_, StandardCharsets.UTF_8))
      npz <- read(dir.resolve(s"$stem.npz"))
      bound <- PhrfDatasetBinding.bind(npz, manifest, expect)
    yield bound

  private def read(p: Path): Either[IngestRefusal, Array[Byte]] =
    Try(Files.readAllBytes(p)).toEither.left.map(e => IngestRefusal.Unreadable(p.toString, String.valueOf(e.getMessage)))
