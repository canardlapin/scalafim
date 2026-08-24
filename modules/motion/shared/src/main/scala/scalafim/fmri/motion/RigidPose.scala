package scalafim.fmri.motion

import image4s.geometry.Affine
import image4s.geometry.D3

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

  def toAffine: Affine[D3] =
    val cx = math.cos(rx)
    val sx = math.sin(rx)
    val cy = math.cos(ry)
    val sy = math.sin(ry)
    val cz = math.cos(rz)
    val sz = math.sin(rz)

    Affine.fromRowMajor[D3](
      Vector(
        cz * cy, cz * sy * sx - sz * cx, cz * sy * cx + sz * sx, tx,
        sz * cy, sz * sy * sx + cz * cx, sz * sy * cx - cz * sx, ty,
        -sy, cy * sx, cy * cx, tz,
        0.0, 0.0, 0.0, 1.0
      )
    ).fold(
      error => throw new IllegalStateException(error.message),
      identity
    )

  def inverse: Either[MotionError, RigidPose] =
    RigidPose.fromAffine(toAffine.inverse)

  /** Apply this rigid pose first and `next` second. */
  def andThen(next: RigidPose): Either[MotionError, RigidPose] =
    toAffine
      .andThen(next.toAffine)
      .left
      .map(error => MotionError.InvalidMatrix(error.message))
      .flatMap(RigidPose.fromAffine(_))

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

  def fromAffine(affine: Affine[D3], tolerance: Double = 1e-5): Either[MotionError, RigidPose] =
    val matrix = affine.matrix
    if !rotationLooksOrthonormal(affine, tolerance) then
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

  private def rotationLooksOrthonormal(affine: Affine[D3], tolerance: Double): Boolean =
    val matrix = affine.matrix
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
