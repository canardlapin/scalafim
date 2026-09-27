package scalafim.image.view

import image4s.geometry.Affine
import image4s.geometry.D3
import scalafim.image.*
import scalafim.image.world.{DatasetNamespace, GeometryDigest, ReferenceAcquisition, SubjectId, TemplateName, WorldSpace}

class LayerAlignmentSuite extends munit.FunSuite:
  private def volumeIn(space: SomeSampleSpace, label: String): SomeScalarVolume[Double] =
    val typed = SampleSpaces.requireVolumeD3(space).fold(error => fail(error.message), identity)
    SomeScalarVolume.unsafeCopyFromCanonicalArray(
      PrimitiveBuffers.tabulate[Double](typed.grid.shape.product)(_.toDouble),
      typed,
      label
    )

  private def layer(id: String, volume: SomeScalarVolume[Double], mapping: LayerMapping = LayerMapping.WorldAligned) =
    SliceLayer(
      LayerId.unsafe(id),
      volume,
      SliceSampling.Linear(),
      ScalarColorizer(DisplayWindow.unsafe(0.0, 10.0)),
      mapping = mapping
    )

  private val unresolved = SampleSpaces(Vector(3, 3, 3))

  private def inTemplate(name: String): SomeSampleSpace =
    SampleSpaces
      .inWorld(SampleSpaces(Vector(3, 3, 3)), WorldSpace.Template(TemplateName.unsafe(name)))
      .fold(error => fail(error.message), identity)

  test("layers carry their own frame and a checked alignment to the reference"):
    val reference = volumeIn(inTemplate("MNI152NLin2009cAsym"), "reference")
    val overlay = volumeIn(inTemplate("MNI152NLin2009cAsym"), "overlay")
    val model = ViewerModel
      .make(reference.grid, Vector(layer("reference", reference), layer("overlay", overlay)))
      .fold(error => fail(error.message), identity)

    assert(model.layers(1).frame.sameRuntimeOwnerAs(overlay.grid.frame))
    assert(!model.layers(1).frame.sameRuntimeOwnerAs(reference.grid.frame), clue = "distinct owners, one persistent key")
    model.alignment(LayerId.unsafe("overlay")) match
      case Some(LayerAlignment.SharedWorld(_)) => ()
      case other                                 => fail(s"expected a shared-world alignment, got $other")

  test("a world-aligned layer in another world is rejected"):
    val reference = volumeIn(inTemplate("MNI152NLin2009cAsym"), "reference")
    val other = volumeIn(inTemplate("fsaverage"), "other")
    ViewerModel.make(reference.grid, Vector(layer("reference", reference), layer("other", other))) match
      case Left(ImageViewError.LayerFrameMismatch(id, _)) => assertEquals(id, LayerId.unsafe("other"))
      case result                                         => fail(s"expected a frame mismatch, got $result")

  test("two subjects' native volumes are different worlds and do not overlay without a transform"):
    def native(subject: String): SomeSampleSpace =
      val world =
        for
          namespace <- DatasetNamespace("ds")
          id <- SubjectId(subject)
          digest <- GeometryDigest(Vector(3, 3, 3), Vector.tabulate(12)(i => if i % 5 == 0 then 1.0 else 0.0), 1, 1)
          reference <- ReferenceAcquisition(Map("suffix" -> "T1w"), digest)
        yield WorldSpace.SubjectNative(namespace, id, None, reference)
      world
        .flatMap(w => SampleSpaces.inWorld(SampleSpaces(Vector(3, 3, 3)), w).left.map(e => fail(e.message)))
        .fold(error => fail(error.message), identity)
    val first = volumeIn(native("sub-01"), "sub-01")
    val sameSubject = volumeIn(native("sub-01"), "sub-01 again")
    val second = volumeIn(native("sub-02"), "sub-02")
    assert(ViewerModel.make(first.grid, Vector(layer("a", first), layer("b", sameSubject))).isRight)
    ViewerModel.make(first.grid, Vector(layer("a", first), layer("b", second))) match
      case Left(ImageViewError.LayerFrameMismatch(id, _)) => assertEquals(id, LayerId.unsafe("b"))
      case result                                         => fail(s"expected a frame mismatch, got $result")

  test("a pullback layer must map the reference frame to its own frame"):
    val reference = volumeIn(unresolved, "reference")
    val source = volumeIn(SampleSpaces(Vector(3, 3, 3)), "source")
    val referenceGrid = GridSpec.fromGrid(reference.grid)
    val sourceGrid = GridSpec.fromGrid(source.grid)
    val good = SpatialPullbacks.affine(sourceGrid, referenceGrid, Affine.identity[D3])
    val backwards = SpatialPullbacks.affine(referenceGrid, sourceGrid, Affine.identity[D3])

    val model = ViewerModel
      .make(reference.grid, Vector(layer("reference", reference), layer("mapped", source, LayerMapping.Pullback(good))))
      .fold(error => fail(error.message), identity)
    model.alignment(LayerId.unsafe("mapped")) match
      case Some(LayerAlignment.Mapped(_)) => ()
      case other                          => fail(s"expected a mapped alignment, got $other")

    ViewerModel.make(
      reference.grid,
      Vector(layer("reference", reference), layer("mapped", source, LayerMapping.Pullback(backwards)))
    ) match
      case Left(ImageViewError.LayerMappingMismatch(id, _)) => assertEquals(id, LayerId.unsafe("mapped"))
      case result                                           => fail(s"expected a mapping mismatch, got $result")
