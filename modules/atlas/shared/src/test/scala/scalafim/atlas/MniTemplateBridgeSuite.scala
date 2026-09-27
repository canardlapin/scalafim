package scalafim.atlas

import image4s.geometry.{Affine, D3}
import reframe4s.field.CoordinateBoundaryPolicy
import scalafim.image.world.Spaces as WorldFrames
import scalafim.transform.{AssetRef, PushAvailability, TransformProvenance, WorldTransform}
import scalafim.transform.field.DenseLattice
import scalafim.transform.itk.{ItkHdf5Component, ItkHdf5File}

/** Admission of the TemplateFlow bridge and pullback-only routes, without the 200 MB composite (see the JVM
  * `MniTemplateBridgeFilesSuite` for the real file against ITK).
  */
class MniTemplateBridgeSuite extends munit.FunSuite:
  private def value[A](result: Either[AtlasError, A]): A =
    result.fold(error => fail(error.message), identity)

  private val marker = ItkHdf5Component(0, "CompositeTransform_double_3_3", IArray.empty[Double], IArray.empty[Double])
  private val affine =
    ItkHdf5Component(1, "AffineTransform_double_3_3", IArray(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0), IArray(0.0, 0.0, 0.0))

  /** A 2 x 2 x 2 zero displacement field whose LPS lattice starts at `origin`. */
  private def field(origin: (Double, Double, Double), size: Int = 2): ItkHdf5Component =
    ItkHdf5Component(
      2,
      "DisplacementFieldTransform_double_3_3",
      IArray.fill(3 * size * size * size)(0.0),
      IArray(size.toDouble, size.toDouble, size.toDouble, origin._1, origin._2, origin._3, 1.0, 1.0, 1.0, -1.0, 0.0, 0.0, 0.0, -1.0, 0.0, 0.0, 0.0, 1.0)
    )

  private def refusal(file: ItkHdf5File, asset: AssetRef): String =
    MniTemplateBridge.fromItk(file, asset) match
      case Left(AtlasError.TemplateAssetRefused(_, reason)) => reason
      case other                                            => fail(s"expected a refusal, got $other")

  private val forwardSha = TemplateFlowXfm.Mni6ToMni2009c.sha256

  test("only the inspected TemplateFlow composite is admitted, by the SHA-256 of its bytes"):
    val file = ItkHdf5File(Vector(marker, affine, field((96.0, 132.0, -78.0))))
    assert(refusal(file, AssetRef("unhashed.h5", None)).contains("no SHA-256"))
    assert(refusal(file, AssetRef("other.h5", Some("00" * 32))).contains("not an inspected TemplateFlow composite"))
    val reverse = refusal(file, AssetRef("reverse.h5", Some(TemplateFlowXfm.Mni2009cToMni6.sha256)))
    assert(reverse.contains("named to pull Mni6ToMni2009c but measured to pull Mni2009cToMni6"), reverse)
    assertEquals(TemplateFlowXfm.bySha256(forwardSha.toUpperCase), Some(TemplateFlowXfm.Mni6ToMni2009c))

  test("the admitted digest must still hold an affine and a field on the 2009c res-01 lattice"):
    val asset = AssetRef("forward.h5", Some(forwardSha))
    assert(refusal(ItkHdf5File(Vector(marker, affine)), asset).contains("one affine and one displacement field"))
    // right layout, wrong lattice: the first voxel is not the 2009c res-01 corner, and the size is 2^3
    val lattice = refusal(ItkHdf5File(Vector(marker, affine, field((91.0, 126.0, -72.0)))), asset)
    assert(lattice.contains("is not MNI152NLin2009cAsym res-01"), lattice)
    assert(MniTemplateBridge.checkLayout(ItkHdf5File(Vector(marker, affine, field((96.0, 132.0, -78.0)))), TemplateFlowXfm.Mni6ToMni2009c).isLeft)

  test("stock MNI grids are TemplateFlow's, in their templates' frames, with persistent ids"):
    val grids = Vector(
      ("templateflow:tpl-MNI152NLin2009cAsym_res-01", TemplateGrids.mni2009cRes1.grid.persistentId, TemplateGrids.mni2009cRes1.dims, TemplateGrids.mni2009cRes1.affine, Vector(193, 229, 193), (1.0, -96.0, -132.0, -78.0)),
      ("templateflow:tpl-MNI152NLin2009cAsym_res-02", TemplateGrids.mni2009cRes2.grid.persistentId, TemplateGrids.mni2009cRes2.dims, TemplateGrids.mni2009cRes2.affine, Vector(97, 115, 97), (2.0, -96.5, -132.5, -78.5)),
      ("templateflow:tpl-MNI152NLin6Asym_res-01", TemplateGrids.mni6Res1.grid.persistentId, TemplateGrids.mni6Res1.dims, TemplateGrids.mni6Res1.affine, Vector(182, 218, 182), (1.0, -91.0, -126.0, -72.0)),
      ("templateflow:tpl-MNI152NLin6Asym_res-02", TemplateGrids.mni6Res2.grid.persistentId, TemplateGrids.mni6Res2.dims, TemplateGrids.mni6Res2.affine, Vector(91, 109, 91), (2.0, -90.0, -126.0, -72.0))
    )
    grids.foreach:
      case (id, persistent, dims, affine, expectedDims, (spacing, x, y, z)) =>
        assertEquals(persistent.map(_.value), Some(id))
        assertEquals(dims, expectedDims, id)
        val m = affine.rowMajor
        Vector(m(0), m(5), m(10)).foreach(v => assertEqualsDouble(v, spacing, 0.0, id))
        Vector(m(1), m(2), m(4), m(6), m(8), m(9)).foreach(v => assertEqualsDouble(v, 0.0, 0.0, id))
        assertEqualsDouble(m(3), x, 0.0, id)
        assertEqualsDouble(m(7), y, 0.0, id)
        assertEqualsDouble(m(11), z, 0.0, id)
    assert(TemplateGrids.mni2009cRes1.frame eq WorldFrames.MNI152NLin2009cAsym)
    assert(TemplateGrids.mni6Res2.frame eq WorldFrames.MNI152NLin6Asym)

  /** A 6Asym -> 2009c dense warp on a small 2009c lattice whose pullback shifts points by (+1, -2, +0.5) mm. */
  private def shiftWarp(push: Boolean): TransformAsset =
    val lattice = Affine
      .fromRowMajor[D3](Vector(4.0, 0.0, 0.0, -20.0, 0.0, 4.0, 0.0, -20.0, 0.0, 0.0, 4.0, -20.0, 0.0, 0.0, 0.0, 1.0))
      .fold(error => fail(error.message), identity)
    def dense[S <: image4s.geometry.Frame[D3], T <: image4s.geometry.Frame[D3]](target: T, source: S, shift: Vector[Double]) =
      DenseLattice
        .pullback(target, source, Vector(11, 11, 11), lattice, CoordinateBoundaryPolicy.Reject)((i, j, k) =>
          Vector(-20.0 + 4.0 * i + shift(0), -20.0 + 4.0 * j + shift(1), -20.0 + 4.0 * k + shift(2))
        )
        .fold(error => fail(error.message), identity)
    val pull = dense[WorldFrames.Mni6, WorldFrames.Mni2009c](WorldFrames.MNI152NLin2009cAsym, WorldFrames.MNI152NLin6Asym, Vector(1.0, -2.0, 0.5))
    val availability =
      if push then
        PushAvailability.FromAsset(
          dense[WorldFrames.Mni2009c, WorldFrames.Mni6](WorldFrames.MNI152NLin6Asym, WorldFrames.MNI152NLin2009cAsym, Vector(-1.0, 2.0, -0.5)),
          AssetRef("test inverse", None)
        )
      else PushAvailability.Unavailable[WorldFrames.Mni6, WorldFrames.Mni2009c]()
    TransformAsset(WorldTransform.Mapped(pull, availability, TransformProvenance.constructed("test shift warp")), s"test-shift-warp|push=$push")

  private def registry(asset: TransformAsset): Vector[TransformStep] =
    SpaceTransforms.manifest.map: step =>
      if step.from == SpaceId.MNI152NLin6Asym && step.to == SpaceId.MNI152NLin2009cAsym then step.withAsset(asset) else step

  test("a dense warp without a forward map is pullback-executable only"):
    val plan = value(SpaceTransforms.plan(SpaceId.MNI152NLin6Asym, SpaceId.MNI152NLin2009cAsym, DataKind.Voxel, registry(shiftWarp(push = false))))
    assertEquals(plan.status, TransformStatus.Available)
    assert(!plan.isExecutable)
    plan.executability match
      case Left(AtlasError.TransformNotExecutable(_, _, reason)) => assert(reason.contains("pullback only"), reason)
      case other                                                 => fail(s"expected a pullback-only refusal, got $other")
    assert(plan.transform(Vector(Point3D.Origin)).isLeft)
    val pulled = value(plan.pullPoints(Vector(Point3D(3.0, 4.0, 5.0)))).head
    assertEqualsDouble(pulled.x, 4.0, 1e-12)
    assertEqualsDouble(pulled.y, 2.0, 1e-12)
    assertEqualsDouble(pulled.z, 5.5, 1e-12)
    // a pull-only warp cannot be routed backwards; the planned reverse step stays the route
    val reverse = value(SpaceTransforms.plan(SpaceId.MNI152NLin2009cAsym, SpaceId.MNI152NLin6Asym, DataKind.Voxel, registry(shiftWarp(push = false))))
    assert(!reverse.usedInverses)
    assert(reverse.pullbackExecutability.isLeft)

  test("a supplied inverse makes the warp push points and admits routing through its inverse"):
    val withPush = registry(shiftWarp(push = true))
    val plan = value(SpaceTransforms.plan(SpaceId.MNI152NLin6Asym, SpaceId.MNI152NLin2009cAsym, DataKind.Voxel, withPush))
    assert(plan.isExecutable, plan.executability.toString)
    val pushed = value(plan.transform(Vector(Point3D(4.0, 2.0, 5.5)))).head
    assertEqualsDouble(pushed.x, 3.0, 1e-12)
    assertEqualsDouble(pushed.y, 4.0, 1e-12)
    assertEqualsDouble(pushed.z, 5.0, 1e-12)
    val graph = value(SpaceTransforms.graph(withPush))
    val edge = graph.graph.morphisms.find(_.id.value.contains("MNI152NLin6Asym->MNI152NLin2009cAsym")).getOrElse(fail("missing edge"))
    assertEquals(edge.inverse, scalafim.spatial.Inverse.Provided("inverse asset", 1.0))
    // without the planned reverse step, 2009c -> 6Asym runs the warp backwards through its supplied inverse
    val noReverse = withPush.filterNot(step => step.from == SpaceId.MNI152NLin2009cAsym && step.to == SpaceId.MNI152NLin6Asym)
    val reverse = value(SpaceTransforms.plan(SpaceId.MNI152NLin2009cAsym, SpaceId.MNI152NLin6Asym, DataKind.Voxel, noReverse))
    assert(reverse.usedInverses)
    val back = value(reverse.transform(Vector(Point3D(3.0, 4.0, 5.0)))).head
    assertEqualsDouble(back.x, 4.0, 1e-12)
    assertEqualsDouble(back.y, 2.0, 1e-12)
    assertEqualsDouble(back.z, 5.5, 1e-12)

  test("the standard manifest names the real TemplateFlow file and why the reverse step is planned"):
    val steps = SpaceTransforms.manifest.filter(_.backend == TransformBackend.TemplateFlowAnts)
    assertEquals(steps.map(step => (step.from, step.to)), Vector((SpaceId.MNI152NLin6Asym, SpaceId.MNI152NLin2009cAsym), (SpaceId.MNI152NLin2009cAsym, SpaceId.MNI152NLin6Asym)))
    steps.foreach(step => assertEquals(step.dataFiles, Vector("tpl-MNI152NLin2009cAsym/tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5")))
    assert(steps(1).notes.exists(_.contains("is refused")), steps(1).notes.toString)
