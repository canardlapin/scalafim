package scalafim.image

import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid
import scala.compiletime.testing.typeCheckErrors
import scalafim.image.world.{Placed, Spaces}

class GridSpecFrameSuite extends munit.FunSuite:
  private val shape = SpatialDims(3, 4, 5)

  private val affine: Affine[D3] =
    ProviderSpaces.affine(
      Vector(
        Vector(2.0, 0.0, 0.0, 10.0),
        Vector(0.0, 3.0, 0.0, 20.0),
        Vector(0.0, 0.0, 4.0, 30.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def isTypeMismatch(errors: List[scala.compiletime.testing.Error]): Boolean =
    errors.nonEmpty && errors.forall(_.message.contains("Required"))

  private def assertVoxel(actual: VoxelPoint, expected: VoxelPoint): Unit =
    assertEqualsDouble(actual.x, expected.x, 1e-12)
    assertEqualsDouble(actual.y, expected.y, 1e-12)
    assertEqualsDouble(actual.z, expected.z, 1e-12)

  private def mniGrid: GridSpec[Spaces.Mni2009c] =
    GridSpec.in(Spaces.MNI152NLin2009cAsym)(shape, affine).fold(error => fail(error.message), identity)

  test("a grid built in a static frame carries that frame in its type"):
    val grid: GridSpec[Spaces.Mni2009c] = mniGrid
    assert(grid.frame eq Spaces.MNI152NLin2009cAsym)
    assertEquals(grid.dims, Vector(3, 4, 5))

  test("typed voxel/world entry points round-trip through frame-owned points"):
    val grid = mniGrid
    val point = grid.pointAt(VoxelPoint(1.0, 2.0, 3.0)).fold(error => fail(error.message), identity)
    assertEqualsDouble(point.x, 12.0, 1e-12)
    assertEqualsDouble(point.y, 26.0, 1e-12)
    assertEqualsDouble(point.z, 42.0, 1e-12)
    assertEquals(point.toWorldPoint, WorldPoint(12.0, 26.0, 42.0))
    assertVoxel(grid.voxelAt(point).fold(error => fail(error.message), identity), VoxelPoint(1.0, 2.0, 3.0))

    val bound = grid.claimUnchecked(WorldPoint(12.0, 26.0, 42.0)).fold(error => fail(error.message), identity)
    assertVoxel(grid.voxelAt(bound).fold(error => fail(error.message), identity), VoxelPoint(1.0, 2.0, 3.0))

  test("WorldPoint.claimUnchecked claims a coordinate for exactly the named frame"):
    val point = WorldPoint(1.0, 2.0, 3.0).claimUnchecked(Spaces.MNI152NLin2009cAsym).fold(error => fail(error.message), identity)
    assert(point.frame eq Spaces.MNI152NLin2009cAsym)
    assertEquals(WorldPoint.of(point), WorldPoint(1.0, 2.0, 3.0))

  test("a GridSpec in one frame cannot be passed where another frame's grid is required"):
    val errors = typeCheckErrors(
      """
        import scalafim.image.GridSpec
        import scalafim.image.world.Spaces
        def needsMni(grid: GridSpec[Spaces.Mni2009c]): Int = grid.nVoxels
        val fs: GridSpec[Spaces.FsAverage] = ???
        needsMni(fs)
      """
    )
    assert(isTypeMismatch(errors), clue = s"a fsaverage grid must not typecheck as an MNI grid: $errors")
    val control = typeCheckErrors(
      """
        import scalafim.image.GridSpec
        import scalafim.image.world.Spaces
        def needsMni(grid: GridSpec[Spaces.Mni2009c]): Int = grid.nVoxels
        val mni: GridSpec[Spaces.Mni2009c] = ???
        needsMni(mni)
      """
    )
    assertEquals(control, Nil)

  test("a runtime-framed grid cannot be used as a statically framed one without binding"):
    val errors = typeCheckErrors(
      """
        import scalafim.image.GridSpec
        import scalafim.image.world.Spaces
        val dynamic: GridSpec[?] = GridSpec.identity(Vector(2, 2, 2))
        val mni: GridSpec[Spaces.Mni2009c] = dynamic
      """
    )
    assert(isTypeMismatch(errors), clue = s"an existential grid must not widen to a static frame: $errors")

  test("a pullback between two frames cannot drive a plan between two others"):
    val errors = typeCheckErrors(
      """
        import image4s.geometry.Affine
        import image4s.geometry.D3
        import scalafim.image.{GridSpec, Resample, ResamplingPlan, SpatialPullbacks}
        import scalafim.image.world.Spaces
        val mni: GridSpec[Spaces.Mni2009c] = ???
        val fs: GridSpec[Spaces.FsAverage] = ???
        val mniToFs = SpatialPullbacks.affine(fs, mni, Affine.identity[D3])
        ResamplingPlan.make(mni, fs, mniToFs, Resample.Method.Linear)
      """
    )
    assert(isTypeMismatch(errors), clue = s"pullback endpoints must match the plan's frames: $errors")
    val control = typeCheckErrors(
      """
        import image4s.geometry.Affine
        import image4s.geometry.D3
        import scalafim.image.{GridSpec, Resample, ResamplingPlan, SpatialPullbacks}
        import scalafim.image.world.Spaces
        val mni: GridSpec[Spaces.Mni2009c] = ???
        val fs: GridSpec[Spaces.FsAverage] = ???
        val fsToMni = SpatialPullbacks.affine(mni, fs, Affine.identity[D3])
        ResamplingPlan.make(mni, fs, fsToMni, Resample.Method.Linear)
      """
    )
    assertEquals(control, Nil)

  test("a frame-owned point cannot be passed as another frame's point"):
    val errors = typeCheckErrors(
      """
        import image4s.geometry.{D3, Point}
        import scalafim.image.GridSpec
        import scalafim.image.world.Spaces
        val fs: GridSpec[Spaces.FsAverage] = ???
        val mniPoint: Point[Spaces.Mni2009c, D3] = ???
        fs.voxelAt(mniPoint)
      """
    )
    assert(isTypeMismatch(errors), clue = s"a native point must not be accepted by an MNI grid: $errors")

  test("Placed[GridSpec] binds a decoded-frame grid to the static template frame"):
    val decodedFrame = scalafim.image.world.FrameCatalog.frame(
      scalafim.image.world.FrameCatalog.worldOf(Spaces.MNI152NLin2009cAsym).toOption.get
    )
    assert(!decodedFrame.sameRuntimeOwnerAs(Spaces.MNI152NLin2009cAsym))
    val decoded = GridSpec.in(decodedFrame)(shape, affine).fold(error => fail(error.message), identity)
    val placed = Placed[GridSpec](decodedFrame)(decoded).fold(error => fail(error.message), identity)

    val bound: GridSpec[Spaces.Mni2009c] =
      placed.bindTo(Spaces.MNI152NLin2009cAsym).fold(error => fail(error.message), identity)
    assert(bound.frame eq Spaces.MNI152NLin2009cAsym)
    assertEquals(bound.dims, decoded.dims)
    assertEquals(bound.affine, decoded.affine)

    assert(placed.bindTo(Spaces.fsaverage).isLeft, clue = "binding to a different world must fail")

  test("Placed.of packages a runtime-decoded GridSpec[?] and binds it to the static template frame"):
    val decodedFrame = scalafim.image.world.FrameCatalog.frame(
      scalafim.image.world.FrameCatalog.worldOf(Spaces.MNI152NLin2009cAsym).toOption.get
    )
    val decoded: GridSpec[?] = GridSpec.in(decodedFrame)(shape, affine).fold(error => fail(error.message), identity)
    val placed = Placed.of(decoded.frame)(decoded).fold(error => fail(error.message), identity)
    assert(placed.frame eq decoded.frame)
    val bound: GridSpec[Spaces.Mni2009c] =
      placed.bindTo(Spaces.MNI152NLin2009cAsym).fold(error => fail(error.message), identity)
    assertEquals(bound.affine, decoded.affine)
    assert(placed.bindTo(Spaces.fsaverage).isLeft)

  test("Placed.of checks the value's runtime frame owner when its static frame type is wide"):
    val subjectFrame = scalafim.image.world.FrameCatalog.frame(scalafim.image.world.WorldSpace.declare("subject").toOption.get)
    val wide: GridSpec[Frame[D3]] =
      GridSpec.fromGrid(Grid.forFrame[D3, Frame[D3]](subjectFrame)(shape.toVector, affine).fold(error => fail(error.message), identity))
    val mni: Frame[D3] = Spaces.MNI152NLin2009cAsym
    assert(Placed.of[Frame[D3], GridSpec](mni)(wide).isLeft, clue = "a subject grid must not be packaged as an MNI grid")
    assert(Placed.of[Frame[D3], GridSpec](subjectFrame)(wide).isRight)

  test("worldAligned relates grids of one world space and refuses grids of different ones"):
    val mni = mniGrid
    val otherMniOwner = GridSpec
      .in(scalafim.image.world.FrameCatalog.frame(scalafim.image.world.FrameCatalog.worldOf(Spaces.MNI152NLin2009cAsym).toOption.get))(shape, affine)
      .fold(error => fail(error.message), identity)
    assert(SpatialPullbacks.worldAligned(mni, otherMniOwner).isRight, clue = "one world, two runtime owners")
    val fs = GridSpec.in(Spaces.fsaverage)(shape, affine).fold(error => fail(error.message), identity)
    SpatialPullbacks.worldAligned(mni, fs) match
      case Left(SpatialPullbackError.WorldMismatch(_)) => ()
      case other                                      => fail(s"expected a world mismatch, got $other")
    val unresolved = GridSpec(shape.toVector, affine)
    assert(SpatialPullbacks.worldAligned(mni, unresolved).isLeft, clue = "an unresolved grid is not in MNI space")

  test("legacy world-aligned resampling surfaces a world mismatch instead of resampling across worlds"):
    val subjectA = SampleSpaces.inWorld(SampleSpaces(Vector(3, 3, 3)), scalafim.image.world.WorldSpace.declare("sub-01").toOption.get)
      .fold(error => fail(error.message), identity)
    val subjectB = SampleSpaces.inWorld(SampleSpaces(Vector(3, 3, 3)), scalafim.image.world.WorldSpace.declare("sub-02").toOption.get)
      .fold(error => fail(error.message), identity)
    val volume = SomeScalarVolume.unsafeCopyFromCanonicalArray(Array.tabulate(27)(_.toDouble), subjectA, "a")
    assertEqualsDouble(Resample.trilinear(volume, subjectA)(1, 1, 1), volume(1, 1, 1), 1e-12)
    intercept[IllegalArgumentException](Resample.trilinear(volume, subjectB))

  test("typed plans resample within one static frame"):
    val grid = mniGrid
    val plan = ResamplingPlan.identity(grid, Resample.Method.Nearest).fold(error => fail(error.message), identity)
    val typed: ResamplingPlan[Spaces.Mni2009c, Spaces.Mni2009c] = plan
    assert(typed.pullback.source eq Spaces.MNI152NLin2009cAsym)

  test("bind checks a runtime pullback's endpoints against the grids"):
    val sourceSpace = SampleSpaces(Vector(2, 2, 2))
    val world = SampleSpaces.worldOf(sourceSpace).fold(e => fail(e.message), identity)
    val targetSpace = SampleSpaces.inWorld(SampleSpaces(Vector(2, 2, 2)), world).fold(e => fail(e.message), identity)
    val source = GridSpec.fromSpace(sourceSpace)
    val target = GridSpec.fromSpace(targetSpace)
    val unrelated = GridSpec.identity(Vector(2, 2, 2))
    val pullback = SpatialPullbacks.worldAligned(source, target).fold(error => fail(error.message), identity)
    assert(ResamplingPlan.bind(source, target, pullback, Resample.Method.Linear).isRight)
    assert(ResamplingPlan.bind(unrelated, target, pullback, Resample.Method.Linear).isLeft)

  test("SampleSpaces.regular is the typed form of the spacing/origin vector constructor"):
    val typed = SampleSpaces
      .regular(Vector(4, 3, 2, 5), VoxelSpacing.unsafe(2.0, 3.0, 4.0), WorldPoint(10.0, 20.0, 30.0))
      .fold(error => fail(error.message), identity)
    val vectors = SampleSpaces(
      Vector(4, 3, 2, 5),
      spacing = Some(Vector(2.0, 3.0, 4.0)),
      origin = Some(Vector(10.0, 20.0, 30.0))
    )
    assertEquals(typed.logicalShape, vectors.logicalShape)
    assertEquals(typed.grid.indexToFrame.rowMajor, vectors.grid.indexToFrame.rowMajor)
    assert(VoxelSpacing.make(1.0, 0.0, 1.0).isLeft, clue = "zero spacing must be rejected")
    assert(SampleSpaces.regular(Vector(4, 3), VoxelSpacing.OneMillimetre).isLeft, clue = "regular spaces are D3")
