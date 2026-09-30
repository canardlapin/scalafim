package scalafim.surface.view

import image4s.geometry.{Affine, D3, Frame, Point}
import reframe4s.lie.FramedAffine
import scalafim.image.world.*
import scalafim.surface.*

/** STP P8.02: the linked surface <-> volume cursor goes through a typed world link. */
class SurfaceVolumeCursorSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def affine(values: Double*): Affine[D3] = ok(Affine.fromRowMajor[D3](values.toVector))

  private def close(actual: Point[?, D3], expected: Vector[Double], tol: Double = 1e-12)(using munit.Location): Unit =
    (0 until 3).foreach(axis => assertEqualsDouble(actual.coordinates(axis), expected(axis), tol))

  private val namespace = ok(DatasetNamespace("ds-cursor"))
  private val subject = ok(SubjectId("01"))
  private val reference = ok(ReferenceAcquisition(Map("acq" -> "mprage"), ok(GeometryDigest(Vector(64, 72, 50), Vector.fill(12)(1.0), 1, 1))))
  private val tkRas = FrameCatalog.frame(WorldSpace.SubjectTkRas(namespace, subject, reference))
  private val scanner = FrameCatalog.frame(WorldSpace.SubjectNative(namespace, subject, None, reference))
  private val mni = Spaces.MNI152NLin2009cAsym

  private val mesh = TriangleMesh.fromRows(
    Vector(Vector(0.0, 0.0, 0.0), Vector(10.0, 0.0, 0.0), Vector(0.0, 10.0, 0.0), Vector(0.0, 0.0, 10.0)),
    Vector((0, 1, 2), (0, 1, 3), (0, 2, 3), (1, 2, 3))
  )
  private val geometry = SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.White)
  private val surfaceId = SurfaceId.unsafe("lh.white")
  private val white = ok(FramedSurface.in(tkRas)(geometry, SurfacePlacement.StoredCoordinates))
  private val radius = SurfaceLinkRadius.unsafe(2.0)

  /** MNI -> tkRAS pullback (tkRAS = MNI + (5, -3, 1)); push withheld, as for a composite whose warp has no inverse. */
  private val pullOnly =
    ok(WorldLink.pullback[tkRas.type, mni.type](
      FramedAffine.betweenFrames[mni.type, tkRas.type, D3](mni, tkRas)(affine(1, 0, 0, 5, 0, 1, 0, -3, 0, 0, 1, 1, 0, 0, 0, 1)),
      None,
      "fMRIPrep composite without inverse warp"
    ))

  test("a tkRAS surface links to an MNI volume through the pullback; the forward direction is a typed error"):
    val cursor = ok(SurfaceVolumeCursor.make(surfaceId, white, pullOnly))
    val hit = ok(cursor.toSurface(ok(Point.in(mni)(-4.5, 13.5, -0.5)), radius))
    assertEquals(hit.selection, SurfaceSelection(surfaceId, VertexId(2)))
    close(hit.vertex, Vector(0.0, 10.0, 0.0))
    assert(hit.vertex.belongsTo(tkRas))
    assertEqualsDouble(hit.distance, math.sqrt(0.5 * 0.5 + 0.5 * 0.5 + 0.5 * 0.5), 1e-12)
    assertEquals(
      cursor.toVolume(hit.selection),
      Left(SurfaceCursorError.Link(WorldLinkError.DirectionUnavailable(LinkDirection.LeftToRight, "fMRIPrep composite without inverse warp")))
    )

  test("with a forward map the cursor round-trips vertex -> volume -> vertex"):
    val pull = FramedAffine.betweenFrames[mni.type, tkRas.type, D3](mni, tkRas)(affine(1, 0, 0, 5, 0, 1, 0, -3, 0, 0, 1, 1, 0, 0, 0, 1))
    val link = ok(WorldLink.pullback[tkRas.type, mni.type](pull, Some(pull.inverse), "affine registration"))
    val cursor = ok(SurfaceVolumeCursor.make(surfaceId, white, link))
    val selection = SurfaceSelection(surfaceId, VertexId(1))
    val inMni = ok(cursor.toVolume(selection))
    assert(inMni.belongsTo(mni))
    close(inMni, Vector(5.0, 3.0, -1.0))
    val back = ok(cursor.toSurface(inMni, radius))
    assertEquals(back.selection, selection)
    assertEqualsDouble(back.distance, 0.0, 1e-12)

  test("one world links directly under an explicit alignment"):
    val scannerSurface = ok(white.toScanner(scanner, FreeSurferVolumeGeometry(Vector(64, 72, 50), affine(-1, 0, 0, 32, 0, 0, 1, -36, 0, -1, 0, 25, 0, 0, 0, 1))))
    val volumeFrame = FrameCatalog.frame(WorldSpace.SubjectNative(namespace, subject, None, reference))
    assert(!volumeFrame.sameRuntimeOwnerAs(scanner), clue = "the volume's frame is another owner of the same world")
    val cursor = ok(SurfaceVolumeCursor.make(surfaceId, scannerSurface, ok(WorldLink.shared[scanner.type, volumeFrame.type](scanner, volumeFrame))))
    val selection = SurfaceSelection(surfaceId, VertexId(3))
    val inVolume = ok(cursor.toVolume(selection))
    assert(inVolume.belongsTo(volumeFrame))
    assertEquals(inVolume.coordinates, ok(scannerSurface.vertex(VertexId(3))).coordinates)
    assertEquals(ok(cursor.toSurface(inVolume, radius)).selection, selection)

  test("the cursor refuses another world's link, another surface's selection, and points beyond the radius"):
    val scannerLink = ok(WorldLink.shared[scanner.type, scanner.type](scanner, scanner))
    SurfaceVolumeCursor.make(surfaceId, white, scannerLink.asInstanceOf[WorldLink[tkRas.type, scanner.type]]) match
      case Left(SurfaceCursorError.Link(WorldLinkError.FrameMismatch(_))) => ()
      case other => fail(s"expected a tkRAS surface to refuse a scanner-space link, got $other")
    val cursor = ok(SurfaceVolumeCursor.make(surfaceId, white, pullOnly))
    val stranger = SurfaceId.unsafe("rh.white")
    assertEquals(cursor.toVolume(SurfaceSelection(stranger, VertexId(0))), Left(SurfaceCursorError.WrongSurface(surfaceId, stranger)))
    cursor.toSurface(ok(Point.in(mni)(-50.0, 0.0, 0.0)), radius) match
      case Left(SurfaceCursorError.BeyondRadius(distance, maximum)) =>
        assertEqualsDouble(distance, math.sqrt(45.0 * 45.0 + 3.0 * 3.0 + 1.0), 1e-9)
        assertEqualsDouble(maximum, 2.0, 0.0)
      case other => fail(s"expected the point to lie beyond the radius, got $other")
