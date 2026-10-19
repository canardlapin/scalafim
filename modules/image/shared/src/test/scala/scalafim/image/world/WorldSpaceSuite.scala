package scalafim.image.world

import neuroim4s.spaces.*
import image4s.geometry.{D3, Frame}
import scalafim.image.{SampleSpaceError, SampleSpaces}

class WorldSpaceSuite extends munit.FunSuite:
  private def ok[A](result: Either[SpaceError, A]): A =
    result.fold(error => fail(error.message), identity)

  test("sample spaces relabel from unresolved into a world, but never across worlds"):
    val unresolved = SampleSpaces.make(Vector(4, 5, 6)).fold(e => fail(e.message), identity)
    assert(SampleSpaces.worldOf(unresolved).exists(_.isInstanceOf[WorldSpace.Unresolved]))
    val independentlyBuilt = SampleSpaces.make(Vector(4, 5, 6)).fold(e => fail(e.message), identity)
    assertNotEquals(unresolved.grid.persistentId, independentlyBuilt.grid.persistentId)
    val left = SampleSpaces.requireD3(unresolved).fold(e => fail(e.message), identity)
    val right = SampleSpaces.requireD3(independentlyBuilt).fold(e => fail(e.message), identity)
    assert(Frame.align[D3](left.grid.frame, right.grid.frame).isLeft)
    val mni = ok(WorldSpace.template("MNI152NLin2009cAsym"))
    val placed = SampleSpaces.inWorld(unresolved, mni).fold(e => fail(e.message), identity)
    assertEquals(SampleSpaces.worldOf(placed), Right(mni))
    assertEquals(placed.grid.shape, unresolved.grid.shape)
    assertEquals(placed.grid.indexToFrame.rowMajor, unresolved.grid.indexToFrame.rowMajor)
    assertEquals(SampleSpaces.inWorld(placed, mni).map(SampleSpaces.worldOf), Right(Right(mni)))
    SampleSpaces.inWorld(placed, ok(WorldSpace.template("MNI152NLin6Asym"))) match
      case Left(SampleSpaceError.WorldRelabel(_, _)) => ()
      case other                                     => fail(s"expected a relabel refusal, got $other")

  test("downsampling and deobliquing preserve unknown and known source world scopes"):
    import scalafim.image.{Deoblique, Downsample, SomeScalarVolume, SomeScalarSeries}
    Vector(WorldSpace.freshUnresolved(), ok(WorldSpace.template("MNI152NLin2009cAsym"))).foreach: world =>
      val bare = SampleSpaces(Vector(4, 4, 4))
      val source = SampleSpaces.inWorld(bare, world).fold(e => fail(e.message), identity)
      val volume = SomeScalarVolume.unsafeCopyFromCanonicalArray(Array.fill(64)(1.0), source)
      val down = Downsample.toDims(volume, Vector(2, 2, 2))
      assertEquals(SampleSpaces.worldOf(down.space), Right(world))
      assert(down.grid.frame.sameRuntimeOwnerAs(volume.grid.frame))
      assert(down.grid.persistentId.nonEmpty)
      assertEquals(SampleSpaces.worldOf(Deoblique.target(source)), Right(world))
      val seriesSpace = SampleSpaces.inWorld(SampleSpaces(Vector(4, 4, 4, 2)), world).fold(e => fail(e.message), identity)
      val series = SomeScalarSeries.unsafeCopyFromCanonicalArray(Array.fill(128)(1.0), seriesSpace)
      val downSeries = Downsample.toDims(series, Vector(2, 2, 2))
      assertEquals(SampleSpaces.worldOf(downSeries.space), Right(world))
      assert(downSeries.grid.frame.sameRuntimeOwnerAs(series.grid.frame))
      assert(downSeries.grid.persistentId.nonEmpty)

  test("world relabelling refuses ephemeral LPS and non-millimetre coordinates"):
    import image4s.{NonSpatialAxes, SampleSpace}
    import image4s.geometry.{Affine, CoordinateConvention, Grid, LengthUnit}
    val world = ok(WorldSpace.template("MNI152NLin2009cAsym"))
    Vector((LengthUnit.Meter, CoordinateConvention.RAS), (LengthUnit.Millimeter, CoordinateConvention.LPS)).foreach: (unit, convention) =>
      val frame = Frame.named[D3]("foreign coordinate units", unit = unit, convention = convention).fold(e => fail(e.message), identity)
      val grid = Grid.in(frame)(Vector(2, 2, 2), Affine.identity[D3]).fold(e => fail(e.message), identity)
      assert(SampleSpaces.inWorld(SampleSpace.create(grid, NonSpatialAxes.empty), world).isLeft)

  test("generic nearest-neighbour resampling refuses independent unknown worlds"):
    import scalafim.image.{Resample, SomeLabelVolume, SomeLabelSeries}
    val source = SampleSpaces(Vector(2, 2, 2))
    val foreign = SampleSpaces(Vector(2, 2, 2))
    val labels = SomeLabelVolume.unsafeCopyFromCanonicalArray(Array.fill(8)(1), source)
    intercept[IllegalArgumentException](Resample.nearest(labels, foreign, 0))
    val world = SampleSpaces.worldOf(source).fold(e => fail(e.message), identity)
    val target = SampleSpaces.inWorld(foreign, world).fold(e => fail(e.message), identity)
    assertEquals(Resample.nearest(labels, target, 0).copyToCanonicalArray.toVector, Vector.fill(8)(1))
    val seriesSpace = SampleSpaces.inWorld(SampleSpaces(Vector(2, 2, 2, 2)), world).fold(e => fail(e.message), identity)
    val series = SomeLabelSeries.unsafeCopyFromCanonicalArray(Array.fill(16)(1), seriesSpace)
    intercept[IllegalArgumentException](Resample.nearest(series, foreign, 0))
    assertEquals(Resample.nearest(series, target, 0).copyToCanonicalArray.toVector, Vector.fill(16)(1))
