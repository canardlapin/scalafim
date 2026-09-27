package scalafim.surface

import image4s.geometry.{Affine, D3, Frame}
import scalafim.image.world.*

class FramedSurfaceSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def affine(values: Double*): Affine[D3] =
    ok(Affine.fromRowMajor[D3](values.toVector))

  private val namespace = ok(DatasetNamespace("ds-ribbon"))
  private val subject = ok(SubjectId("01"))
  private val reference = ReferenceAcquisition(Map("acq" -> "mprage"), ok(GeometryDigest(Vector(64, 72, 50), Vector.fill(12)(1.0), 1, 1)))

  private val scannerWorld = WorldSpace.SubjectNative(namespace, subject, None, reference)
  private val tkRasWorld = WorldSpace.SubjectTkRas(namespace, subject, reference)

  /** An oblique (non-LIA) volume, so tkRAS -> scanner rotates as well as translates. */
  private val volume = FreeSurferVolumeGeometry(Vector(64, 72, 50), affine(0.9, -0.2, 0.1, 12.0, 0.15, 1.1, 0.05, -30.0, -0.1, 0.02, 2.4, 8.5, 0, 0, 0, 1))

  private val mesh = TriangleMesh.fromRows(
    Vector(Vector(1.0, 2.0, 3.0), Vector(-4.0, 5.5, 0.25), Vector(7.0, -1.0, 2.0), Vector(0.0, 0.0, -6.0)),
    Vector((0, 1, 2), (0, 2, 3), (0, 3, 1), (1, 3, 2))
  )
  private val geometry = SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.White, affine(1, 0, 0, 10, 0, 1, 0, -20, 0, 0, 1, 30, 0, 0, 0, 1))

  private def applyTo(a: Affine[D3], xyz: Vector[Double]): Vector[Double] =
    ok(a(xyz))

  test("stored coordinates and the geometry's surface-to-world placement are distinct declarations"):
    val frame = FrameCatalog.frame(scannerWorld)
    val stored = ok(FramedSurface.in(frame)(geometry, SurfacePlacement.StoredCoordinates))
    val placed = ok(FramedSurface.in(frame)(geometry, SurfacePlacement.SurfaceToWorld))
    assertEquals(ok(stored.vertex(VertexId(1))).coordinates, Vector(-4.0, 5.5, 0.25))
    assertEquals(ok(placed.vertex(VertexId(1))).coordinates, Vector(6.0, -14.5, 30.25))
    assert(ok(placed.vertex(VertexId(0))).belongsTo(frame))
    assertEquals(placed.vertex(VertexId(4)), Left(SurfaceFrameError.VertexOutOfRange(4, 4)))
    assertEquals(placed.hemisphere, Hemisphere.Left)
    assertEquals(placed.world, Right(scannerWorld))

  test("tkRAS surfaces reach scanner RAS through Norig * inverse(Torig) and back"):
    val tk = FrameCatalog.frame(tkRasWorld)
    val scanner = FrameCatalog.frame(scannerWorld)
    val white = ok(FramedSurface.in(tk)(geometry, SurfacePlacement.StoredCoordinates))
    val moved: FramedSurface[scanner.type] = ok(white.toScanner(scanner, volume))
    (0 until mesh.vertexCount).foreach: i =>
      val expected = applyTo(volume.tkrToScanner, mesh.vertex(VertexId(i)).toVector)
      val actual = ok(moved.vertex(VertexId(i))).coordinates
      (0 until 3).foreach(axis => assertEqualsDouble(actual(axis), expected(axis), 1e-12))
    assert(moved.hasSameTopology(white))
    val back = ok(moved.toTkRas(tk, volume))
    (0 until mesh.vertexCount).foreach: i =>
      (0 until 3).foreach(axis => assertEqualsDouble(back.coordinates(3 * i + axis), mesh.coordinates(3 * i + axis), 1e-9))

  test("tkRAS -> scanner transport requires a tkRAS source and a same-subject scanner target"):
    val tk = FrameCatalog.frame(tkRasWorld)
    val scanner = FrameCatalog.frame(scannerWorld)
    val otherSubject = FrameCatalog.frame(WorldSpace.SubjectNative(namespace, ok(SubjectId("02")), None, reference))
    val tkSurface = ok(FramedSurface.in(tk)(geometry, SurfacePlacement.StoredCoordinates))
    val scannerSurface = ok(FramedSurface.in(scanner)(geometry, SurfacePlacement.StoredCoordinates))
    assert(tkSurface.toScanner(otherSubject, volume).left.exists(_.isInstanceOf[SurfaceFrameError.SubjectMismatch]))
    assert(tkSurface.toScanner(Spaces.fsaverage, volume).left.exists(_.isInstanceOf[SurfaceFrameError.WrongWorld]))
    assert(scannerSurface.toScanner(scanner, volume).left.exists(_.isInstanceOf[SurfaceFrameError.WrongWorld]))
    val ephemeral = ok(Frame.named[D3]("scratch"))
    assert(tkSurface.toScanner(ephemeral, volume).left.exists(_.isInstanceOf[SurfaceFrameError.Space]))

  test("surfaces in different frames cannot be substituted for one another"):
    val errors = compileErrors(
      """
      val tk = FrameCatalog.frame(tkRasWorld)
      val scanner = FrameCatalog.frame(scannerWorld)
      val white = FramedSurface.in(tk)(geometry, SurfacePlacement.StoredCoordinates).toOption.get
      val wrong: FramedSurface[scanner.type] = white
      """
    )
    assert(errors.contains("Found:"), errors)

  test("a surface placed in a runtime world binds to the static template frame with the same key"):
    val placed = ok(FramedSurface.inWorld(WorldSpace.Template(TemplateName.unsafe("fsaverage")))(geometry, SurfacePlacement.StoredCoordinates))
    val bound: FramedSurface[Spaces.FsAverage] = ok(placed.bindTo(Spaces.fsaverage))
    assert(bound.frame.sameRuntimeOwnerAs(Spaces.fsaverage))
    assertEquals(bound.coordinates.toVector, mesh.coordinates.toVector)
    assert(placed.bindTo(Spaces.fsLR).isLeft)
