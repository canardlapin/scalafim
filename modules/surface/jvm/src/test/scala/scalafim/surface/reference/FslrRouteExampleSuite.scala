package scalafim.surface.reference

import scalafim.examples.reference.FslrRouteExample
import scalafim.surface.{Hemisphere, SurfaceKind, VertexId}
import java.nio.file.{Files, Path}

class FslrRouteExampleSuite extends munit.FunSuite:
  private val manifest = Path.of(getClass.getResource("/fslr-route-example/manifest.json").toURI)

  test("ordinary byte-verified APIs retain zero, explain nonfinite missingness and bind the wall"):
    val result = FslrRouteExample.synthetic(manifest)
    assertEquals(result.mapped.valueAt(VertexId(0)), Some(0.0))
    assertEquals(result.mapped.coverageAt(VertexId(1)), Some(VertexCoverage.NoSupport))
    assertEquals(result.mapped.valueAt(VertexId(2)), Some(2.0))
    assertEquals(result.mapped.coverageAt(VertexId(3)), Some(VertexCoverage.MedialWall))
    val nonfinite = result.route.inspect(result.source, VertexId(1)).toOption.get
    assert(nonfinite.samples.head.contribution.isInstanceOf[Contribution.NonFinite])
    assertEquals(result.route.disclosure.medialWallAsset.map(_.label), Some("cortex.label.gii"))
    assert(result.displays.forall(_.mapped eq result.mapped))
    assertEquals(result.receipt("displayPreservesMappedObject").bool, true)
    assertEquals(result.route.disclosure.qualification, RouteQualification.NumericalContract)

  test("a route rejects another declared template on the same voxel grid"):
    val result = FslrRouteExample.synthetic(manifest)
    val declaration = result.source.declaration
    val other = FrameDeclaration.make(TemplateFrame.unsafe("MNI152NLin6Asym", "other-release"),
      declaration.basis, declaration.asset).toOption.get
    // The same grid and values never make a different declared frame admissible.
    val different = DeclaredVolumeReader.readNifti(manifest.getParent.resolve("source.nii"), other).toOption.get
    assert(result.route.map(different).left.exists(_.isInstanceOf[RouteError.SourceFrameMismatch]))

  test("the locked inflated display cannot enter the anatomical sampling chain"):
    val result = FslrRouteExample.synthetic(manifest)
    val path = manifest.getParent.resolve("inflated.surf.gii")
    val bytes = Files.readAllBytes(path)
    val asset = DataAsset.make("inflated.surf.gii", AssetSha256.of(bytes).value).toOption.get
    val declaration = FrameDeclaration.make(result.source.frame, result.source.declaration.basis, asset).toOption.get
    val anatomy = DeclaredSurfaceReader.read(path, declaration, Hemisphere.Left, SurfaceKind.Midthickness)
      .flatMap(surface => SamplingAnatomy.make(result.route.anatomy.reference, AnatomicalGeometry.Midthickness(surface)))
    assert(anatomy.left.exists(_.isInstanceOf[ReferenceError.DeclarationConflict]), anatomy.toString)
