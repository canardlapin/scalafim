package scalafim.atlas

import scalafim.image.Affine3D

/** A face of a labelled voxel cell. Axis is 0/1/2; side is -1/+1. */
final case class AtlasBoundaryFace(x: Int, y: Int, z: Int, axis: Int, side: Int):
  require(axis >= 0 && axis < 3 && (side == -1 || side == 1))

final case class AtlasBoundaryHit(region: AtlasRegionMetadata, distanceMm: Double,
    nearestPoint: Point3D, face: AtlasBoundaryFace)

/** Every labelled region with a boundary within the inclusive search radius, sorted by
  * (distance, region ID). Equal-distance regions are retained, never collapsed by name.
  * For one region, equal-distance faces choose (z,y,x,axis,side) order.
  */
final case class AtlasBoundaryResult(input: Point3D, coordinateSpace: AnySpaceId,
    radiusMm: Double, hits: Vector[AtlasBoundaryHit], visitedVoxels: Int)

object AtlasBoundaryQuery:
  /** Distance to the union of exposed voxel-cell faces, not labelled voxel centres.
    * Cells extend +/- 0.5 grid units. A face is exposed when its neighbor has a
    * different label or is outside the atlas; shared interfaces belong to both labels.
    * The complete affine (including shear) defines the world-mm metric. No implicit
    * space normalization/registration is performed. Excessive searches fail closed.
    */
  def queryEither(atlas: VolumeAtlas, point: Point3D, fromSpace: AnySpaceId,
      radiusMm: Double, maxVisitedVoxels: Int = 1000000,
      cancelled: () => Boolean = () => false): Either[AtlasError, AtlasBoundaryResult] =
    def invalid(message: String) = Left(AtlasError.InvalidQuery(message))
    if cancelled() then invalid("Atlas boundary query cancelled")
    else if fromSpace.value != atlas.ref.coordSpace.value then invalid("Boundary query requires the exact atlas coordinate space")
    else if !Vector(point.x, point.y, point.z).forall(_.isFinite) || !radiusMm.isFinite || radiusMm < 0 then
      invalid("Boundary query requires finite coordinates and a finite non-negative radius")
    else if maxVisitedVoxels <= 0 then invalid("Boundary query voxel budget must be positive")
    else if atlas.space.dims.size != 3 then invalid("Boundary query requires a three-dimensional atlas")
    else Affine3D.make(atlas.space.trans).left.map(e => AtlasError.InvalidQuery(e.message)).flatMap(affine =>
      queryAffine(atlas, point, fromSpace, radiusMm, maxVisitedVoxels, cancelled, affine))

  private def queryAffine(atlas: VolumeAtlas, point: Point3D, fromSpace: AnySpaceId,
      radiusMm: Double, maxVisitedVoxels: Int, cancelled: () => Boolean,
      affine: Affine3D): Either[AtlasError, AtlasBoundaryResult] =
      def invalid(message: String) = Left(AtlasError.InvalidQuery(message))
      val matrix = affine.matrix
      val inverse = affine.inverse
      val coordinates = Array(point.x, point.y, point.z)
      val lower = new Array[Int](3)
      val upper = new Array[Int](3)
      val dims = atlas.space.spatialDims
      val inclusiveRadius = java.lang.Math.nextAfter(radiusMm, Double.PositiveInfinity)
      var axis = 0
      var empty = false
      while axis < 3 do
        val center = inverse(axis, 0) * point.x + inverse(axis, 1) * point.y + inverse(axis, 2) * point.z + inverse(axis, 3)
        // A world ball projects to this interval in each voxel axis; +/-0.5 admits
        // every cell intersecting that ball, including sheared parallelepipeds.
        val extent = inclusiveRadius * math.hypot(math.hypot(inverse(axis, 0), inverse(axis, 1)), inverse(axis, 2)) + 0.5
        if !center.isFinite || !extent.isFinite then return invalid("Boundary query affine coordinates overflow")
        // One outward representable step preserves exact-radius contacts at an
        // integer cell bound without introducing a scale-independent tolerance.
        val outerExtent = java.lang.Math.nextAfter(extent, Double.PositiveInfinity)
        val lo = math.ceil(java.lang.Math.nextAfter(
          java.lang.Math.nextAfter(center, Double.NegativeInfinity) - outerExtent, Double.NegativeInfinity))
        val hi = math.floor(java.lang.Math.nextAfter(
          java.lang.Math.nextAfter(center, Double.PositiveInfinity) + outerExtent, Double.PositiveInfinity))
        if hi < 0 || lo > dims(axis) - 1 then empty = true
        lower(axis) = math.max(0.0, math.min(dims(axis).toDouble, lo)).toInt
        upper(axis) = math.max(-1.0, math.min(dims(axis) - 1.0, hi)).toInt
        axis += 1
      val count = (0 until 3).map(a => math.max(0, upper(a) - lower(a) + 1).toDouble).product
      if empty || count == 0 then Right(AtlasBoundaryResult(point, fromSpace, radiusMm, Vector.empty, 0))
      else if count > maxVisitedVoxels then invalid("Boundary query exceeds the declared voxel budget")
      else
        val faces = Array.tabulate(3) { a =>
          val u = (a + 1) % 3
          val v = (a + 2) % 3
          new FaceMetric(matrix(0,u), matrix(1,u), matrix(2,u), matrix(0,v), matrix(1,v), matrix(2,v))
        }
        if faces.exists(!_.valid) then invalid("Boundary query refuses numerically degenerate affine faces")
        else
          val labels = atlas.labelVolume
          def label(x: Int, y: Int, z: Int): Int =
            if x < 0 || y < 0 || z < 0 || x >= dims(0) || y >= dims(1) || z >= dims(2) then 0
            else labels.linear(x + dims(0) * (y + dims(1) * z))
          val best = scala.collection.mutable.Map.empty[Int, AtlasBoundaryHit]
          val closest = new Array[Double](3)
          var visited = 0
          var z = lower(2)
          while z <= upper(2) do
            var y = lower(1)
            while y <= upper(1) do
              var x = lower(0)
              while x <= upper(0) do
                if cancelled() then return invalid("Atlas boundary query cancelled")
                visited += 1
                val id = label(x,y,z)
                if id != 0 then
                  axis = 0
                  while axis < 3 do
                    var side = -1
                    while side <= 1 do
                      val neighbor = label(x + (if axis == 0 then side else 0),
                        y + (if axis == 1 then side else 0), z + (if axis == 2 then side else 0))
                      if neighbor != id then
                        val gx = x - 0.5 + (if axis == 0 && side == 1 then 1 else 0)
                        val gy = y - 0.5 + (if axis == 1 && side == 1 then 1 else 0)
                        val gz = z - 0.5 + (if axis == 2 && side == 1 then 1 else 0)
                        val ox = matrix(0,0)*gx + matrix(0,1)*gy + matrix(0,2)*gz + matrix(0,3)
                        val oy = matrix(1,0)*gx + matrix(1,1)*gy + matrix(1,2)*gz + matrix(1,3)
                        val oz = matrix(2,0)*gx + matrix(2,1)*gy + matrix(2,2)*gz + matrix(2,3)
                        val distance = faces(axis).closest(coordinates(0)-ox, coordinates(1)-oy, coordinates(2)-oz, closest)
                        if !distance.isFinite then return invalid("Boundary query world distance overflow")
                        if distance <= inclusiveRadius && best.get(id).forall(_.distanceMm > distance) then
                          best.update(id, AtlasBoundaryHit(atlas.region(RegionId(id)).get, distance,
                            Point3D(ox+closest(0), oy+closest(1), oz+closest(2)), AtlasBoundaryFace(x,y,z,axis,side)))
                      side += 2
                    axis += 1
                x += 1
              y += 1
            z += 1
          if cancelled() then invalid("Atlas boundary query cancelled")
          else Right(AtlasBoundaryResult(point, fromSpace, radiusMm,
            best.valuesIterator.toVector.sortBy(h => (h.distanceMm, h.region.id.value)), visited))

  /** Closest point on a parallelogram: feasible plane projection or one of four
    * segment projections. Unit edge vectors avoid squaring physical scale in the
    * 2D face equations. No mesh construction or per-face scratch allocation.
    */
  private final class FaceMetric(ux: Double, uy: Double, uz: Double, vx: Double, vy: Double, vz: Double):
    private val ul = math.hypot(math.hypot(ux,uy),uz)
    private val vl = math.hypot(math.hypot(vx,vy),vz)
    private val ax = ux/ul; private val ay = uy/ul; private val az = uz/ul
    private val bx = vx/vl; private val by = vy/vl; private val bz = vz/vl
    private val cosine = ax*bx + ay*by + az*bz
    private val determinant = 1 - cosine*cosine
    val valid: Boolean = ul.isFinite && vl.isFinite && ul > 0 && vl > 0 && determinant > 1e-12

    def closest(qx: Double, qy: Double, qz: Double, out: Array[Double]): Double =
      val du = qx*ax + qy*ay + qz*az
      val dv = qx*bx + qy*by + qz*bz
      val a = (du-cosine*dv)/determinant
      val b = (dv-cosine*du)/determinant
      var best = Double.PositiveInfinity
      def consider(x: Double, y: Double, z: Double): Unit =
        val distance = math.hypot(math.hypot(qx-x,qy-y),qz-z)
        if distance < best then
          best = distance; out(0)=x; out(1)=y; out(2)=z
      def clamp(value: Double, length: Double): Double = math.max(0, math.min(length,value))
      if a >= 0 && a <= ul && b >= 0 && b <= vl then consider(a*ax+b*bx,a*ay+b*by,a*az+b*bz)
      val edge0 = clamp(du,ul)
      consider(edge0*ax,edge0*ay,edge0*az)
      val edge1 = clamp(du-vl*cosine,ul)
      consider(vx+edge1*ax,vy+edge1*ay,vz+edge1*az)
      val edge2 = clamp(dv,vl)
      consider(edge2*bx,edge2*by,edge2*bz)
      val edge3 = clamp(dv-ul*cosine,vl)
      consider(ux+edge3*bx,uy+edge3*by,uz+edge3*bz)
      best
