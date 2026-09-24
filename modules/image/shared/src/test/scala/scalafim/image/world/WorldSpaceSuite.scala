package scalafim.image.world

import image4s.geometry.{D3, Frame, FrameRegistry}
import scalafim.image.{SampleSpaceError, SampleSpaces}

class WorldSpaceSuite extends munit.FunSuite:
  private def ok[A](result: Either[SpaceError, A]): A =
    result.fold(error => fail(error.message), identity)

  private val identityAffine = Vector(1.0, 0, 0, -10, 0, 1, 0, -20, 0, 0, 1, -30, 0, 0, 0, 1)
  private val bold = ok(GeometryDigest(Vector(64, 64, 32), identityAffine, 1, 1))
  private val otherGeometry = ok(GeometryDigest(Vector(64, 64, 32), identityAffine.updated(3, -10.5), 1, 1))

  private def native(ns: String, sub: String, entities: Map[String, String], geometry: GeometryDigest = bold) =
    WorldSpace.SubjectNative(ok(DatasetNamespace(ns)), ok(SubjectId(sub)), Some(ok(SessionId("01"))), ReferenceAcquisition(entities, geometry))

  test("the same subject label in two datasets names two world spaces"):
    val a = native("doi:10.18112/openneuro.ds000001", "sub-01", Map("task" -> "rest"))
    val b = native("doi:10.18112/openneuro.ds000002", "sub-01", Map("task" -> "rest"))
    assertNotEquals(FrameCatalog.frameId(a), FrameCatalog.frameId(b))

  test("two boldrefs in one session are distinct unless task, run and geometry all agree"):
    val run1 = native("ds", "sub-01", Map("task" -> "rest", "run" -> "1"))
    val run2 = native("ds", "sub-01", Map("task" -> "rest", "run" -> "2"))
    val moved = native("ds", "sub-01", Map("task" -> "rest", "run" -> "1"), otherGeometry)
    assertNotEquals(FrameCatalog.frameId(run1), FrameCatalog.frameId(run2))
    assertNotEquals(FrameCatalog.frameId(run1), FrameCatalog.frameId(moved))
    assertEquals(FrameCatalog.frameId(run1), FrameCatalog.frameId(native("ds", "sub-01", Map("run" -> "1", "task" -> "rest"))))

  test("declared spaces are fresh even when their labels match"):
    val first = ok(WorldSpace.declare("my-space"))
    val second = ok(WorldSpace.declare("my-space"))
    assertNotEquals(first, second)
    assertNotEquals(FrameCatalog.frameId(first), FrameCatalog.frameId(second))

  test("the unresolved space keeps the historical shared frame id"):
    assertEquals(FrameCatalog.frameId(WorldSpace.Unresolved).value, "scalafim-ras-d3")

  test("encoding round-trips every world space, including awkward characters"):
    val spaces = Vector(
      ok(WorldSpace.template("MNI152NLin2009cAsym")),
      native("ns:with:colons%and-dashes_", "sub-01|x", Map("task" -> "a-b_c|d", "acq" -> "é"), otherGeometry),
      WorldSpace.SubjectTkRas(ok(DatasetNamespace("ds")), ok(SubjectId("bert")), ReferenceAcquisition(Map.empty, bold)),
      ok(WorldSpace.declare("label: with % specials")),
      WorldSpace.Unresolved
    )
    spaces.foreach: space =>
      assertEquals(WorldSpace.decode(WorldSpace.encode(space)), Right(space))

  test("independently created frames of one world space align; different spaces do not"):
    val mni = ok(WorldSpace.template("MNI152NLin2009cAsym"))
    val first = FrameCatalog.frame(mni)
    val second = FrameCatalog.frame(mni)
    assert(!first.sameRuntimeOwnerAs(second))
    assert(Frame.align[D3](first, second).isRight)
    assert(Frame.align[D3](first, FrameCatalog.frame(ok(WorldSpace.template("MNI152NLin6Asym")))).isLeft)
    assertEquals(FrameCatalog.worldOf(second), Right(mni))

  test("a persisted world frame restores through the provider registry"):
    val tkras = WorldSpace.SubjectTkRas(ok(DatasetNamespace("ds")), ok(SubjectId("bert")), ReferenceAcquisition(Map.empty, bold))
    val live = FrameCatalog.frame(tkras)
    val registry = live.record.flatMap(_ => FrameRegistry.empty.register(live)).fold(e => fail(e.message), identity)
    val restored = Frame.restore[D3](FrameCatalog.record(tkras), registry).fold(e => fail(e.message), identity)
    assert(restored.frame.sameRuntimeOwnerAs(live))
    assertEquals(FrameCatalog.worldOf(restored.frame), Right(tkras))

  test("ephemeral and non-RAS frames are not world frames"):
    val ephemeral = Frame.named[D3]("scratch").fold(e => fail(e.message), identity)
    assert(FrameCatalog.worldOf(ephemeral).isLeft)

  test("sample spaces relabel from unresolved into a world, but never across worlds"):
    val unresolved = SampleSpaces.make(Vector(4, 5, 6)).fold(e => fail(e.message), identity)
    assertEquals(SampleSpaces.worldOf(unresolved), Right(WorldSpace.Unresolved))
    val mni = ok(WorldSpace.template("MNI152NLin2009cAsym"))
    val placed = SampleSpaces.inWorld(unresolved, mni).fold(e => fail(e.message), identity)
    assertEquals(SampleSpaces.worldOf(placed), Right(mni))
    assertEquals(placed.grid.shape, unresolved.grid.shape)
    assertEquals(placed.grid.indexToFrame.rowMajor, unresolved.grid.indexToFrame.rowMajor)
    assertEquals(SampleSpaces.inWorld(placed, mni).map(SampleSpaces.worldOf), Right(Right(mni)))
    SampleSpaces.inWorld(placed, ok(WorldSpace.template("MNI152NLin6Asym"))) match
      case Left(SampleSpaceError.WorldRelabel(_, _)) => ()
      case other                                     => fail(s"expected a relabel refusal, got $other")
