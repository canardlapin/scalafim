package scalafim.surface

/** Outside `scalafim.surface.reference`, no API pairs manifest bytes with a
  * caller-supplied parser or builds a manifest from caller-supplied fields.
  */
class PointMapManifestAccessSuite extends munit.FunSuite:
  test("manifests cannot be forged from outside the reference package"):
    val viaParser = compileErrors(
      """scalafim.surface.reference.PointMapManifest.verified(Array.emptyByteArray, "0" * 64, _ => Left("forged"))""")
    assert(viaParser.contains("verified") && viaParser.contains("cannot be accessed"), viaParser)
    val viaApply = compileErrors("""scalafim.surface.reference.PointMapManifest(null, null)""")
    assert(viaApply.contains("does not take parameters"), viaApply)
    val viaNew = compileErrors("""new scalafim.surface.reference.PointMapManifest(null, null)""")
    assert(viaNew.contains("cannot be accessed"), viaNew)
    val viaCopy = compileErrors(
      """(??? : scalafim.surface.reference.PointMapManifest).copy(fields = (??? : scalafim.surface.reference.ManifestFields))""")
    assert(viaCopy.contains("cannot be accessed") || viaCopy.contains("copy"), viaCopy)
    assert(viaCopy.nonEmpty, "copy must not re-pair a verified digest with other fields")
