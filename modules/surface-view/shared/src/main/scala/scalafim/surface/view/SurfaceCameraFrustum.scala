package scalafim.surface.view

import image4s.geometry.GeometryError

/** Explicit render-camera projection for low-level composition clients.
  * Unlike [[SurfaceCamera]], this does not fit a surface or choose depth planes.
  */
enum SurfaceCameraFrustum:
  case Perspective(fieldOfViewDegrees: Double, aspect: Double, near: Double, far: Double)
  case Orthographic(left: Double, right: Double, bottom: Double, top: Double, near: Double, far: Double)

enum SurfaceCameraFrustumError:
  case InvalidPerspective(fieldOfViewDegrees: Double, aspect: Double, near: Double, far: Double)
  case InvalidOrthographic(left: Double, right: Double, bottom: Double, top: Double, near: Double, far: Double)
  case InvalidUp(x: Double, y: Double, z: Double)
  case FrameMismatch(error: GeometryError)
  case UnrepresentableProjection
  case UnrepresentablePose

  def message: String = this match
    case InvalidPerspective(fov, aspect, near, far) => s"invalid perspective frustum: fov=$fov, aspect=$aspect, near=$near, far=$far"
    case InvalidOrthographic(left, right, bottom, top, near, far) => s"invalid orthographic frustum: [$left,$right] x [$bottom,$top], near=$near, far=$far"
    case InvalidUp(x, y, z) => s"camera up must be finite, nonzero and nonparallel to the viewing direction: ($x,$y,$z)"
    case FrameMismatch(error) => s"camera up and pose must share a frame: ${error.message}"
    case UnrepresentableProjection => "camera projection cannot be represented by finite nondegenerate Float coefficients"
    case UnrepresentablePose => "camera pose cannot be represented by finite Float coefficients"
