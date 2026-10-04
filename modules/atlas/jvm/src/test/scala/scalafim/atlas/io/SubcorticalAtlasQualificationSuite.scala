package scalafim.atlas.io

import java.nio.file.Path
import scalafim.atlas.*

class SubcorticalAtlasQualificationSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)

  private def coherent(atlas: VolumeAtlas): Unit =
    val sourceIds = atlas.provenance.sourceArtifacts.map(_.id).toSet
    assert(atlas.provenance.derivation.collect { case DerivationStep.Loaded(id) => id }.forall(sourceIds))
    assert(atlas.provenance.labels.labelTableArtifactId.forall(sourceIds))

  private def qualify(family: SubcorticalAtlasFamily, root: String): VolumeAtlas =
    val request = SubcorticalAtlasRequest(family)
    val atlas = right(SubcorticalAtlasLoader.loadFromPaths(request,
      Path.of(root, request.volume.fileName), Path.of(root, request.labels.fileName)))
    assert(!atlas.realization.support.isEmpty)
    assertEquals(atlas.realization.parcelDomain.size, atlas.regions.size)
    coherent(atlas)
    val expected = family match
      case SubcorticalAtlasFamily.Cit168 => (16, Vector(182, 218, 182), true)
      case SubcorticalAtlasFamily.HcpThalamic => (14, Vector(197, 233, 189), true)
      case SubcorticalAtlasFamily.Mdtb10 => (10, Vector(141, 95, 87), true)
      case SubcorticalAtlasFamily.HcpHippocampusAmygdala => (4, Vector(113, 136, 113), false)
    assertEquals(atlas.regions.size, expected._1)
    assertEquals(atlas.realization.domain.grid.shape.toVector, expected._2)
    assertEquals(atlas.ref.details.confidence, Confidence.Uncertain)
    assertEquals(atlas.provenance.derivation.exists(_.isInstanceOf[DerivationStep.DeclaredDescriptor]), expected._3)
    if expected._3 then assertEquals(atlas.ref.coordSpace.value, s"pinned-atlas-${request.volume.sha256}")
    right(atlas.realization.validateNeuropublishProjection(atlas.realization.neuropublishProjection))
    atlas

  private def withRoot(run: String => Unit): Unit =
    sys.props.get("scalafim.subcortical.fixtureRoot") match
      case None => assume(false, "native source qualification requires explicit fixture root")
      case Some(root) => run(root)

  test("optional native CIT168 qualification"):
    withRoot: root =>
      assert(qualify(SubcorticalAtlasFamily.Cit168, root).regions.regions.forall(_.hemisphere.isEmpty))

  test("optional native HCP thalamic qualification"):
    withRoot: root =>
      qualify(SubcorticalAtlasFamily.HcpThalamic, root)

  test("optional native MDTB10 qualification"):
    withRoot: root =>
      qualify(SubcorticalAtlasFamily.Mdtb10, root)

  test("optional native HCP ROI qualification"):
    withRoot: root =>
      val atlas = qualify(SubcorticalAtlasFamily.HcpHippocampusAmygdala, root)
      assertEquals(atlas.regions.ids.map(_.value).toSet, Set(17, 18, 53, 54))
