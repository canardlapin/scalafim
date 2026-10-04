package scalafim.atlas.io

import java.nio.file.{Files, Path}
import scalafim.atlas.*

class FslAtlasLoaderSuite extends munit.FunSuite:
  private def xml(kind: String, labels: String): String =
    s"""<atlas><header><name>Fixture</name><shortname>FIX</shortname><type>$kind</type>
      <images><imagefile>/fixture-prob-2mm</imagefile><summaryimagefile>/fixture-summary-2mm</summaryimagefile></images>
      </header><data>$labels</data></atlas>"""
  private def right[E, A](v: Either[E, A]): A = v.fold(e => fail(e.toString), identity)

  private def assertSourceReferencesCoherent(atlas: VolumeAtlas): Unit =
    val sourceIds = atlas.provenance.sourceArtifacts.map(_.id).toSet
    val loadedIds = atlas.provenance.derivation.collect:
      case DerivationStep.Loaded(artifactId) => artifactId
    assert(loadedIds.forall(sourceIds.contains),
      s"loaded artifacts absent from sources: ${loadedIds.filterNot(sourceIds.contains).mkString(", ")}")
    assert(atlas.provenance.labels.labelTableArtifactId.forall(sourceIds.contains),
      s"label-table artifact absent from sources: ${atlas.provenance.labels.labelTableArtifactId}")

  test("probability-channel offset and literal label ids have distinct semantics"):
    val labels = """<label index="0">Background or first channel</label><label index="2">Left hippocampus</label>"""
    val probability = right(FslAtlasLoader.parse(xml("Probabilistic", labels)))
    assertEquals(probability.regions.map(_.id.value), Vector(1, 3))
    assertEquals(probability.regions.last.hemisphere, Some(Hemisphere.Left))
    val literal = right(FslAtlasLoader.parse(xml("Label", labels)))
    assertEquals(literal.regions.map(_.id.value), Vector(2))
    assertEquals(literal.summaries, Vector("/fixture-summary-2mm"))

  test("FSL XML admits the optional shortname used by the pinned Juelich descriptor"):
    val withoutShortName = xml("Probabilistic", """<label index="0">A</label>""")
      .replace("<shortname>FIX</shortname>", "")
    assertEquals(right(FslAtlasLoader.parse(withoutShortName)).name, "Fixture")

  test("malformed, duplicate, empty, unsupported and external entity XML fails explicitly"):
    val cases = Vector(
      xml("Probabilistic", """<label index="0">A</label><label index="0">B</label>"""),
      xml("Label", """<label index="-1">A</label>"""),
      xml("Probabilistic", """<label index="2147483647">A</label>"""),
      xml("Probabilistic", """<label index="0"> </label>"""),
      xml("Unknown", """<label index="1">A</label>"""),
      "<atlas>",
      """<!DOCTYPE atlas [<!ENTITY x SYSTEM "file:///does-not-exist">]><atlas>&x;</atlas>"""
    )
    cases.foreach(text => assert(FslAtlasLoader.parse(text).isLeft))

  test("local paths must match expected source bytes"):
    val path = Files.createTempFile("fsl-integrity-", ".xml")
    try
      Files.writeString(path, xml("Label", """<label index="1">A</label>"""))
      assert(PinnedVolumeLoader.verify(FslAtlasRequest(FslAtlasFamily.Julich).xml, path).isLeft)
      assert(FslAtlasLoader.loadFromPaths(FslAtlasRequest(FslAtlasFamily.Julich), path, path).isLeft)
    finally Files.deleteIfExists(path)

  test("optional source-backed qualification loads pinned families with coverage and provenance"):
    sys.props.get("scalafim.atlas.fsl.fixtureRoot") match
      case None => assume(false, "source-backed FSL qualification is separate from offline tests")
      case Some(root) =>
        FslAtlasFamily.values.foreach: family =>
          val request = FslAtlasRequest(family)
          val atlas = right(FslAtlasLoader.loadFromPaths(request, Path.of(root, request.xml.fileName),
            Path.of(root, request.volume.fileName)))
          assert(!atlas.realization.support.isEmpty)
          assertEquals(atlas.realization.parcelDomain.size, atlas.regions.size)
          assertEquals(atlas.provenance.sourceArtifacts.map(_.digest.get.value).toSet,
            Set(request.xml.sha256, request.volume.sha256))
          assertSourceReferencesCoherent(atlas)
          right(atlas.realization.validateNeuropublishProjection(atlas.realization.neuropublishProjection))
