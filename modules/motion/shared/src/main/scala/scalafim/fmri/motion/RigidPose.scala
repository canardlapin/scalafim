package scalafim.fmri.motion

import scalafim.image.{Affine, DMat}

final case class RigidPose private (
    translation: Translation3Mm,
    rotation: EulerZYXRad
):
  require(RigidPose.isFinite6(tx, ty, tz, rx, ry, rz), "RigidPose values must be finite")

  def tx: Double = translation.tx
  def ty: Double = translation.ty
  def tz: Double = translation.tz
  def rx: Double = rotation.rx
  def ry: Double = rotation.ry
  def rz: Double = rotation.rz

  def toMatrix: DMat =
    val cx = math.cos(rx)
    val sx = math.sin(rx)
    val cy = math.cos(ry)
    val sy = math.sin(ry)
    val cz = math.cos(rz)
    val sz = math.sin(rz)

    DMat.fromRows(
      Vector(
        Vector(cz * cy, cz * sy * sx - sz * cx, cz * sy * cx + sz * sx, tx),
        Vector(sz * cy, sz * sy * sx + cz * cx, sz * sy * cx - cz * sx, ty),
        Vector(-sy, cy * sx, cy * cx, tz),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  def inverse: Either[MotionError, RigidPose] =
    DMat.invert(toMatrix).left.map(MotionError.SingularTransform.apply).flatMap(matrix => RigidPose.fromMatrix(matrix))

  def compose(next: RigidPose): Either[MotionError, RigidPose] =
    RigidPose.fromMatrix(Affine.multiply(toMatrix, next.toMatrix))

  def -(that: RigidPose): RigidPose =
    RigidPose.unsafe(
      Translation3Mm.unsafe(tx - that.tx, ty - that.ty, tz - that.tz),
      EulerZYXRad.unsafe(rx - that.rx, ry - that.ry, rz - that.rz)
    )

object RigidPose:
  val identity: RigidPose =
    unsafe(Translation3Mm.zero, EulerZYXRad.zero)

  def make(
      translation: Translation3Mm,
      rotation: EulerZYXRad
  ): RigidPose =
    unsafe(translation, rotation)

  def make(
      tx: Double,
      ty: Double,
      tz: Double,
      rx: Double,
      ry: Double,
      rz: Double
  ): Either[MotionError, RigidPose] =
    for
      translation <- Translation3Mm.make(tx, ty, tz)
      rotation <- EulerZYXRad.make(rx, ry, rz)
    yield unsafe(translation, rotation)

  def unsafe(
      translation: Translation3Mm,
      rotation: EulerZYXRad
  ): RigidPose =
    new RigidPose(translation, rotation)

  def unsafe(
      tx: Double,
      ty: Double,
      tz: Double,
      rx: Double,
      ry: Double,
      rz: Double
  ): RigidPose =
    unsafe(Translation3Mm.unsafe(tx, ty, tz), EulerZYXRad.unsafe(rx, ry, rz))

  def fromMatrix(matrix: DMat, tolerance: Double = 1e-8): Either[MotionError, RigidPose] =
    MotionError.validateFiniteMatrix(matrix).flatMap { _ =>
      val lastOk =
        math.abs(matrix(3, 0)) <= tolerance &&
          math.abs(matrix(3, 1)) <= tolerance &&
          math.abs(matrix(3, 2)) <= tolerance &&
          math.abs(matrix(3, 3) - 1.0) <= tolerance
      if !lastOk then Left(MotionError.InvalidMatrix("last homogeneous row must be [0, 0, 0, 1]"))
      else if !rotationLooksOrthonormal(matrix, tolerance = 1e-5) then
        Left(MotionError.InvalidMatrix("upper-left 3x3 block is not an orthonormal rotation"))
      else
        val r00 = matrix(0, 0)
        val r10 = matrix(1, 0)
        val r20 = matrix(2, 0)
        val r21 = matrix(2, 1)
        val r22 = matrix(2, 2)
        val r11 = matrix(1, 1)
        val r12 = matrix(1, 2)

        val ry = math.asin(clamp(-r20, -1.0, 1.0))
        val cy = math.cos(ry)
        val (rx, rz) =
          if math.abs(cy) > 1e-8 then
            (math.atan2(r21, r22), math.atan2(r10, r00))
          else
            (math.atan2(-r12, r11), 0.0)
        Right(unsafe(matrix(0, 3), matrix(1, 3), matrix(2, 3), rx, ry, rz))
    }

  private[motion] def isFinite6(values: Double*): Boolean =
    values.forall(_.isFinite)

  private[motion] def wrapPi(a: Double): Double =
    val twoPi = 2.0 * math.Pi
    var out = a
    while out > math.Pi do out -= twoPi
    while out < -math.Pi do out += twoPi
    out

  private def clamp(x: Double, low: Double, high: Double): Double =
    math.max(low, math.min(high, x))

  private def rotationLooksOrthonormal(matrix: DMat, tolerance: Double): Boolean =
    var r = 0
    while r < 3 do
      var c = 0
      while c < 3 do
        var sum = 0.0
        var k = 0
        while k < 3 do
          sum += matrix(k, r) * matrix(k, c)
          k += 1
        val expected = if r == c then 1.0 else 0.0
        if math.abs(sum - expected) > tolerance then return false
        c += 1
      r += 1
    true
