package scalafim.architecture

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

class ResponseArchiveBoundaryGuardSuite extends munit.FunSuite:
  private lazy val repositoryRoot: Path =
    var current =
      Path.of(System.getProperty("user.dir")).toAbsolutePath.normalize
    while current != null &&
        (
          !Files.isRegularFile(current.resolve("build.sbt")) ||
            !Files.isDirectory(current.resolve("modules"))
        )
    do current = current.getParent
    if current == null then
      fail("could not locate the ScalaFIM repository root")
    current

  test("principal production modules retain one-way import boundaries"):
    assertNoMatches(
      mainSources("latent"),
      raw"(?m)^\s*import\s+scalafim\.archive".r,
      "latent core imports archive"
    )
    assertNoMatches(
      mainSources("latent"),
      raw"(?i)\b(hdf5|zarr|objectkey|archivepath|lnapath|gzip|checksum)\b".r,
      "latent core contains physical-storage vocabulary"
    )
    assertNoMatches(
      mainSources("dataset"),
      raw"(?m)^\s*import\s+scalafim\.(archive|latent)".r,
      "dataset core imports archive or latent"
    )
    assertNoMatches(
      mainSources("archive"),
      raw"(?i)\b(reconstruct|dct|haar|hrbf|transport|boldzip)\b".r,
      "archive core executes scientific reconstruction"
    )

  test("LNA format and cross-domain code have explicit physical owners"):
    val required =
      Vector(
        "modules/archive-lna/shared/src/main/scala/scalafim/archive/lna/LnaModel.scala",
        "modules/archive-lna/jvm/src/main/scala/scalafim/archive/io/LnaArchiveDriver.scala",
        "modules/interop-archived-response/shared/src/main/scala/scalafim/archive/lna/LnaPipeline.scala",
        "modules/interop-archived-response/shared/src/main/scala/scalafim/latent/LatentArchiveRegistry.scala",
        "modules/interop-archived-response/shared/src/main/scala/scalafim/dataset/LatentArchiveDatasetBackend.scala",
        "modules/interop-archived-response/jvm/src/main/scala/scalafim/dataset/io/LnaDataset.scala"
      )
    val missing =
      required.filterNot(path =>
        Files.isRegularFile(repositoryRoot.resolve(path))
      )
    assertEquals(missing, Vector.empty)

    val forbidden =
      Vector(
        "modules/archive/shared/src/main/scala/scalafim/archive/lna/LnaPipeline.scala",
        "modules/latent/shared/src/main/scala/scalafim/latent/LegacyLatentArchiveCodec.scala",
        "modules/interop-archived-response/shared/src/main/scala/scalafim/latent/LegacyLatentArchiveCodec.scala",
        "modules/archive-lna/shared/src/main/scala/scalafim/archive/lna/LegacyLnaManifestTranslator.scala",
        "modules/dataset/shared/src/main/scala/scalafim/dataset/LatentArchiveDatasetBackend.scala",
        "modules/dataset/jvm/src/main/scala/scalafim/dataset/io/LnaDataset.scala"
      )
    val retained =
      forbidden.filter(path =>
        Files.exists(repositoryRoot.resolve(path))
      )
    assertEquals(retained, Vector.empty)

  test("declared build edges match the extracted acyclic graph"):
    val build =
      Files.readString(
        repositoryRoot.resolve("build.sbt"),
        StandardCharsets.UTF_8
      )
    val latent = projectBlock(build, "latent", "latentJS")
    val archive = projectBlock(build, "archive", "archiveJS")
    val archiveLna = projectBlock(build, "archiveLna", "archiveLnaJS")
    val interop =
      projectBlock(
        build,
        "archivedResponseInterop",
        "archivedResponseInteropJS"
      )
    val dataset = projectBlock(build, "dataset", "datasetJS")

    assert(!latent.contains("archive"))
    assert(!dataset.contains("archive"))
    assert(!dataset.contains("latent"))
    assert(!archive.contains("image"))
    assert(archiveLna.contains(".dependsOn(archive, image)"))
    assert(interop.contains("archiveLna"))
    assert(interop.contains("latent"))
    assert(interop.contains("dataset"))

  test("LNA representation policy is explicit immutable and greenfield"):
    val registry =
      "modules/interop-archived-response/shared/src/main/scala/" +
        "scalafim/latent/LatentArchiveRegistry.scala"

    val text =
      Files.readString(
        repositoryRoot.resolve(registry),
        StandardCharsets.UTF_8
    )
    assert(text.contains("trait LatentArchiveBinding"))
    assert(!text.contains("enum LatentArchiveBinding"))
    assert(text.contains("final class LatentArchiveRegistry private"))
    assert(text.contains("def build("))
    assert(text.contains("val standard: LatentArchiveRegistry"))

    assertNoMatches(
      mainSources("interop-archived-response") ++
        mainSources("archive-lna"),
      raw"(?i:\blegacy\b)|\bLegacy[A-Z]\w*".r,
      "response/archive production code contains obsolete-system vocabulary"
    )
    assertNoMatches(
      mainSources("interop-archived-response"),
      raw"(?m)^\s*object\s+LatentArchiveCodec:".r,
      "deprecated central archive codec alias remains"
    )

    val datasetApi =
      Files.readString(
        repositoryRoot.resolve(
          "modules/interop-archived-response/jvm/src/main/scala/" +
            "scalafim/dataset/io/FmriDatasetLna.scala"
        ),
        StandardCharsets.UTF_8
      )
    assert(datasetApi.contains("registry: LatentArchiveRegistry"))

  private def mainSources(
      module: String
  ): Vector[Path] =
    val root = repositoryRoot.resolve("modules").resolve(module)
    val stream = Files.walk(root)
    try
      stream.iterator.asScala
        .filter(path =>
          Files.isRegularFile(path) &&
            path.toString.endsWith(".scala") &&
            path.iterator.asScala
              .map(_.toString)
              .sliding(2)
              .exists(parts =>
                parts.length == 2 &&
                  parts.head == "src" &&
                  parts.last == "main"
              )
        )
        .toVector
        .sortBy(_.toString)
    finally stream.close()

  private def assertNoMatches(
      sources: Vector[Path],
      pattern: Regex,
      label: String
  ): Unit =
    val violations =
      sources.flatMap: path =>
        val text = Files.readString(path, StandardCharsets.UTF_8)
        pattern
          .findAllMatchIn(text)
          .map(found =>
            s"${repositoryRoot.relativize(path)}:${lineAt(text, found.start)}"
          )
          .toVector
    assert(
      violations.isEmpty,
      s"$label:\n${violations.mkString("\n")}"
    )

  private def lineAt(
      text: String,
      offset: Int
  ): Int =
    var line = 1
    var index = 0
    while index < offset do
      if text.charAt(index) == '\n' then line += 1
      index += 1
    line

  private def projectBlock(
      build: String,
      name: String,
      nextName: String
  ): String =
    val start = build.indexOf(s"lazy val $name =")
    val end = build.indexOf(s"lazy val $nextName", start)
    if start < 0 || end < 0 then
      fail(s"could not isolate build block for $name")
    build.substring(start, end)
