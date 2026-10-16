package scalafim.surface.view

import image4s.geometry.{D3, Frame, Vec}

class SurfaceCameraFrustumSuite extends munit.FunSuite:
  private val frame = Frame.named[D3]("explicit-camera").toOption.get
  private def point(values: Vector[Double]) = SurfaceCameraPose.pointIn[frame.type](frame, values).toOption.get
  private def vector(values: Vector[Double]) = Vec.fromVector[D3](frame, values).toOption.get
  private val pose = SurfaceCameraPose.make(point(Vector(0, 0, 0)), point(Vector(0, 0, 4))).toOption.get

  test("explicit perspective packet preserves pose up and depth planes"):
    val packet = SurfaceCameraPacket.lookAt(pose, vector(Vector(0, 1, 0)),
      SurfaceCameraFrustum.Perspective(60, 1.5, .2, 50)).toOption.get
    assertEqualsDouble(packet.viewMatrix(0), 1.0, 1e-6)
    assertEqualsDouble(packet.viewMatrix(5), 1.0, 1e-6)
    assertEqualsDouble(packet.projectionMatrix(10), -1.0080321, 1e-6)
    assertEqualsDouble(packet.directionZ, 1.0, 0.0)

  test("explicit orthographic frustum and invalid up are checked"):
    val packet = SurfaceCameraPacket.lookAt(pose, vector(Vector(0, 1, 0)),
      SurfaceCameraFrustum.Orthographic(-2, 6, -3, 1, .5, 10)).toOption.get
    assertEqualsDouble(packet.projectionMatrix(0), .25, 1e-6)
    assertEqualsDouble(packet.projectionMatrix(3), -.5, 1e-6)
    assert(SurfaceCameraPacket.lookAt(pose, vector(Vector(0, 0, 1)),
      SurfaceCameraFrustum.Orthographic(-1, 1, -1, 1, .1, 10)).isLeft)

  test("rotated up and asymmetric bounds project an independently known landmark"):
    val packet = SurfaceCameraPacket.lookAt(pose, vector(Vector(1, 0, 0)),
      SurfaceCameraFrustum.Orthographic(-2, 6, -3, 1, .5, 10)).toOption.get
    // +X up and an eye on +Z puts world (1,2,0) at camera (-2,1,-4),
    // the top-left corner of this asymmetric frustum.
    val world = Vector(1.0, 2.0, 0.0, 1.0)
    val camera = Vector.tabulate(4)(r => (0 until 4).map(c => packet.viewMatrix(r * 4 + c) * world(c)).sum)
    val clip = Vector.tabulate(4)(r => (0 until 4).map(c => packet.projectionMatrix(r * 4 + c) * camera(c)).sum)
    assertEqualsDouble(clip(0) / clip(3), -1.0, 1e-6)
    assertEqualsDouble(clip(1) / clip(3), 1.0, 1e-6)

  test("invalid and unrepresentable frusta fail before producing a packet"):
    val up = vector(Vector(0, 1, 0))
    val invalid = Vector(
      SurfaceCameraFrustum.Perspective(0, 1, .1, 10),
      SurfaceCameraFrustum.Perspective(35, Double.NaN, .1, 10),
      SurfaceCameraFrustum.Perspective(35, 1, 10, 1),
      SurfaceCameraFrustum.Orthographic(1, -1, -1, 1, .1, 10),
      SurfaceCameraFrustum.Orthographic(-1, 1, 0, 0, .1, 10),
      SurfaceCameraFrustum.Orthographic(-1e-300, 1e-300, -1, 1, .1, 10)
    )
    invalid.foreach(f => assert(SurfaceCameraPacket.lookAt(pose, up, f).isLeft))

  test("widened frame types cannot mix an up vector from another frame"):
    val other: Frame[D3] = Frame.named[D3]("another-camera-frame").toOption.get
    val widened: Frame[D3] = frame
    val target = SurfaceCameraPose.pointIn(widened, Vector(0, 0, 0)).toOption.get
    val eye = SurfaceCameraPose.pointIn(widened, Vector(0, 0, 4)).toOption.get
    val widenedPose = SurfaceCameraPose.make[Frame[D3]](target, eye).toOption.get
    val rawUp = Vec.fromVector[D3](other, Vector(0, 1, 0)).toOption.get
    val up = Frame.alignOwners[D3, other.type, Frame[D3]](other, other).flatMap(_.vectorToRight(rawUp)).toOption.get
    assert(SurfaceCameraPacket.lookAt[Frame[D3]](widenedPose, up,
      SurfaceCameraFrustum.Perspective(35, 1, .1, 10)).isLeft)

  test("depth coefficients that collapse at Float precision are rejected"):
    val up = vector(Vector(0, 1, 0))
    for frustum <- Vector(
        SurfaceCameraFrustum.Perspective(60, 1, .01, 1e6),
        SurfaceCameraFrustum.Orthographic(-1, 1, -1, 1, 1, 1e100),
        SurfaceCameraFrustum.Orthographic(-1, 1, -1, 1, .01, 1e6)) do
      assertEquals(SurfaceCameraPacket.lookAt(pose, up, frustum), Left(SurfaceCameraFrustumError.UnrepresentableProjection))
