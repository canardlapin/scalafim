package scalafim.atlas.io

import java.nio.file.Path
import scalafim.atlas.*

class JulichVisualLoaderSuite extends munit.FunSuite:
  private def right[E, A](v: Either[E, A]): A = v.fold(e => fail(e.toString), identity)

  private def assertSourceReferencesCoherent(atlas: VolumeAtlas): Unit =
    val sourceIds = atlas.provenance.sourceArtifacts.map(_.id).toSet
    val loadedIds = atlas.provenance.derivation.collect:
      case DerivationStep.Loaded(artifactId) => artifactId
    assert(loadedIds.forall(sourceIds.contains),
      s"loaded artifacts absent from sources: ${loadedIds.filterNot(sourceIds.contains).mkString(", ")}")
    assert(atlas.provenance.labels.labelTableArtifactId.forall(sourceIds.contains),
      s"label-table artifact absent from sources: ${atlas.provenance.labels.labelTableArtifactId}")

  test("optional source-backed qualification selects the anatomical visual parcels from pinned Julich"):
    sys.props.get("scalafim.atlas.fsl.fixtureRoot") match
      case None => assume(false, "source-backed Julich qualification is separate from offline tests")
      case Some(root) =>
        val request = JulichVisualRequest.source
        val xml = Path.of(root, request.xml.fileName)
        val volume = Path.of(root, request.volume.fileName)
        val atlas = right(JulichVisualLoader.loadFromPaths(xml, volume))
        assert(!atlas.realization.support.isEmpty)
        assertEquals(atlas.realization.parcelDomain.size, atlas.regions.size)
        assert(atlas.regions.regions.forall(region => JulichVisualRequest.area(region).nonEmpty))
        assertEquals(atlas.regions.regions.flatMap(JulichVisualRequest.area).toSet,
          Set("V1", "V2", "V3V", "V4", "V5"))
        assertEquals(atlas.regions.regions.flatMap(_.hemisphere).toSet,
          Set(Hemisphere.Left, Hemisphere.Right))
        assertEquals(atlas.provenance.sourceArtifacts.map(_.digest.get.value).toSet,
          Set(request.xml.sha256, request.volume.sha256))
        assertSourceReferencesCoherent(atlas)
        right(atlas.realization.validateNeuropublishProjection(atlas.realization.neuropublishProjection))
        assert(JulichVisualLoader.loadFromPaths(volume, xml).isLeft)
