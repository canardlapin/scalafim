package scalafim.examples.surfaceview

import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scalafim.surface.io.{FreeSurferSurfaceReader, FreeSurferMorphometryReader}

/** Host-local test transport through existing readers; not a public file format. */
object CorticalMapBrowserFixture:
  def main(args: Array[String]): Unit =
    require(args.length == 2, "expected external fsaverage6 directory and output JSON path")
    val corpus = Path.of(args(0))
    val hashes = Map(
      "lh.pial" -> "e31e4b615abcad1bc5c6b69401bbfcbc1cc00a484257ae9e7c3df080cc68654c",
      "lh.white" -> "9e927bc7ed863e0e4d01035616456e06b170847dfe7b4fc1e46628ed030e598b",
      "lh.inflated" -> "b38f14b0b073df96a9c9daf3de41d4d8f5b32d7c7148dad36e21c44e552dcab8",
      "lh.sulc" -> "8e684b1405e86b6d3fa05e4f26628cb6c2b1080b95982faf24caa7345cfdf7a5")
    hashes.foreach: (file, expected) =>
      val actual = MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(corpus.resolve(file)))
        .map(b => f"${b & 255}%02x").mkString
      require(actual == expected, s"cortical corpus hash mismatch: $file")
    val pial = FreeSurferSurfaceReader.read(corpus.resolve("lh.pial"))
    val white = FreeSurferSurfaceReader.read(corpus.resolve("lh.white"))
    val inflated = FreeSurferSurfaceReader.read(corpus.resolve("lh.inflated"))
    val folding = FreeSurferMorphometryReader.read(corpus.resolve("lh.sulc"), pial, "sulcal depth")
    require(pial.vertexCount == 40962 && pial.faceCount == 81920)
    require(pial.mesh.faceIndices.sameElements(white.mesh.faceIndices) &&
      pial.mesh.faceIndices.sameElements(inflated.mesh.faceIndices))
    val json = s"""{"pial":${pial.mesh.coordinates.mkString("[", ",", "]")},"white":${white.mesh.coordinates.mkString("[", ",", "]")},"inflated":${inflated.mesh.coordinates.mkString("[", ",", "]")},"faces":${pial.mesh.faceIndices.mkString("[", ",", "]")},"folding":${folding.data.mkString("[", ",", "]")},"sourceSha256":{${hashes.toVector.sorted.map((file, hash) => s"\"$file\":\"$hash\"").mkString(",")}}}"""
    Files.writeString(Path.of(args(1)), json)
    println(s"cortical_browser_fixture vertices=${pial.vertexCount} faces=${pial.faceCount}")
