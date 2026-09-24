package scalafim.transform

import java.security.MessageDigest

/** Vendored neurotransform oracles must be byte-identical to the recorded provenance. */
class NeurotransformOracleProvenanceSuite extends munit.FunSuite:
  private val root = "/scalafim/transform/oracle/neurotransform"

  private def bytes(path: String): Array[Byte] =
    val stream = getClass.getResourceAsStream(s"$root/$path")
    assert(stream != null, s"missing vendored oracle resource: $path")
    try stream.readAllBytes()
    finally stream.close()

  private def sha256(data: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(data).map(b => f"${b & 0xff}%02x").mkString

  private val recorded: Vector[(String, String)] =
    val manifest = String(bytes("PROVENANCE.json"), "UTF-8")
    val section = manifest.substring(manifest.indexOf("\"sha256\""))
    "\"([^\"]+)\":\\s*\"([0-9a-f]{64})\"".r
      .findAllMatchIn(section)
      .map(m => m.group(1) -> m.group(2))
      .toVector

  test("provenance records every vendored oracle set"):
    val sets = recorded.map(_._1.takeWhile(_ != '/')).distinct.sorted
    assertEquals(sets, Vector("afni_oracle", "fsl_coef_oracle", "fsl_dense_oracle", "itk_oracle"))
    assert(recorded.size > 100, s"only ${recorded.size} files recorded")

  test("every vendored oracle file matches its recorded SHA-256"):
    val mismatched = recorded.filterNot((path, digest) => sha256(bytes(path)) == digest)
    assertEquals(mismatched.map(_._1), Vector.empty)
