package scalafim.surface

import scalafim.image.SpatialPoint

type Point3D = SpatialPoint

object Point3D:
  val Zero: Point3D =
    SpatialPoint.Origin

  def apply(x: Double, y: Double, z: Double): Point3D =
    SpatialPoint(x, y, z)

  def fromVector(values: Vector[Double]): Point3D =
    SpatialPoint.unsafeFromVector(values, "Point3D")

extension (p: Point3D)
  def +(that: Point3D): Point3D =
    Point3D(p.x + that.x, p.y + that.y, p.z + that.z)

  def -(that: Point3D): Point3D =
    Point3D(p.x - that.x, p.y - that.y, p.z - that.z)

  def *(scale: Double): Point3D =
    Point3D(p.x * scale, p.y * scale, p.z * scale)

  def dot(that: Point3D): Double =
    p.x * that.x + p.y * that.y + p.z * that.z

  def cross(that: Point3D): Point3D =
    Point3D(
      p.y * that.z - p.z * that.y,
      p.z * that.x - p.x * that.z,
      p.x * that.y - p.y * that.x
    )

  def norm: Double =
    math.sqrt(p.dot(p))

  def normalized: Point3D =
    val n = p.norm
    if n == 0.0 then Point3D.Zero else p * (1.0 / n)

final case class Triangle(a: VertexId, b: VertexId, c: VertexId):
  def vertices: (VertexId, VertexId, VertexId) =
    (a, b, c)
