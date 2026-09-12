package scalafim.atlas

import scalafim.image.{Affine3D, DMat}

/** A face of a labelled voxel cell. Axis is 0/1/2; side is -1/+1. */
final case class AtlasBoundaryFace(x: Int, y: Int, z: Int, axis: Int, side: Int):
  require(axis >= 0 && axis < 3 && (side == -1 || side == 1))

final case class AtlasBoundaryHit(region: AtlasRegionMetadata, distanceMm: Double,
    nearestPoint: Point3D, face: AtlasBoundaryFace, distanceErrorBoundMm: Double,
    nearestPointErrorBoundMm: Double, radiusAdmission: AtlasBoundaryRadiusAdmission)

enum AtlasBoundaryRadiusAdmission:
  case DefinitelyWithin, NumericallyBorderline

final case class AtlasBoundaryDiscovery(residualInfinityNorm: Double,
    inverseErrorInfinityNorm: Double, paddingVoxels: Double)

private[atlas] final case class AtlasBoundaryInverseBound(residual: Double, errorNorm: Double)

/** Every labelled region with a boundary within the inclusive search radius, sorted by
  * (distance, region ID). Equal-distance regions are retained, never collapsed by name.
  * For one region, equal-distance faces choose (z,y,x,axis,side) order.
  */
final case class AtlasBoundaryResult(input: Point3D, coordinateSpace: AnySpaceId,
    radiusMm: Double, hits: Vector[AtlasBoundaryHit], visitedVoxels: Int,
    distanceErrorBoundMm: Double, discovery: AtlasBoundaryDiscovery)

object AtlasBoundaryQuery:
  private def down(value: Double): Double = java.lang.Math.nextAfter(value, Double.NegativeInfinity)
  private def up(value: Double): Double = java.lang.Math.nextAfter(value, Double.PositiveInfinity)
  private val unitRoundoff = math.ulp(1.0) / 2
  private val gamma128 = 128 * unitRoundoff / (1 - 128 * unitRoundoff)

  /** A posteriori inverse bound over the stored forward matrix, using outward
    * intervals for R=I-A B. Neumann's bound gives ||A^-1-B|| <= ||B||rho/(1-rho).
    */
  private[atlas] def discoveryInverseBound(a: DMat, b: DMat): Either[AtlasError, AtlasBoundaryInverseBound] =
    var residual = 0.0
    var inverseNorm = 0.0
    var row = 0
    while row < 3 do
      var rowResidual = 0.0
      var rowInverse = 0.0
      var col = 0
      while col < 3 do
        var lower = 0.0
        var upper = 0.0
        var k = 0
        while k < 3 do
          val product = a(row,k) * b(k,col)
          lower = down(lower + down(product))
          upper = up(upper + up(product))
          k += 1
        val target = if row == col then 1.0 else 0.0
        rowResidual = up(rowResidual + math.max(math.abs(down(target-upper)),math.abs(up(target-lower))))
        rowInverse = up(rowInverse + math.abs(b(row,col)))
        col += 1
      residual = math.max(residual,rowResidual)
      inverseNorm = math.max(inverseNorm,rowInverse)
      row += 1
    if !residual.isFinite || !inverseNorm.isFinite || residual >= 0.125 then
      Left(AtlasError.InvalidQuery("Boundary query cannot bound the computed affine inverse residual"))
    else
      val error = up(up(inverseNorm * residual) / down(1-residual))
      if !error.isFinite then Left(AtlasError.InvalidQuery("Boundary query inverse error bound overflow"))
      else Right(AtlasBoundaryInverseBound(residual,error))

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
      val inverseBound = discoveryInverseBound(matrix,inverse) match
        case Left(error) => return Left(error)
        case Right(value) => value
      val faces = Array.tabulate(3) { a =>
        val u = (a + 1) % 3
        val v = (a + 2) % 3
        new FaceMetric(matrix(0,u), matrix(1,u), matrix(2,u), matrix(0,v), matrix(1,v), matrix(2,v))
      }
      if faces.exists(!_.valid) then return invalid("Boundary query refuses numerically degenerate affine faces")
      val worldBounds = Array.tabulate(3) { r =>
        var bound = math.abs(matrix(r,3))
        var c = 0
        while c < 3 do
          bound = up(bound + up(math.abs(matrix(r,c)) * (dims(c)-0.5)))
          c += 1
        bound
      }
      val scale = up(up(math.hypot(up(math.hypot(point.x,point.y)),point.z)) +
        up(math.hypot(up(math.hypot(worldBounds(0),worldBounds(1))),worldBounds(2))))
      // gamma128 covers the coordinate/normalized-face arithmetic chain; the
      // normalized Gram determinant sin²(theta) accounts for face conditioning.
      val determinantLower = down(faces.map(_.gramDeterminant).min - 32*unitRoundoff)
      val distanceError = up(up(gamma128 * scale) / determinantLower)
      val resolutionBudget = math.max(1e-12, atlas.space.spacing.min * 1e-8)
      if !distanceError.isFinite || distanceError > resolutionBudget then
        return invalid("Boundary distance arithmetic envelope exceeds local voxel resolution")
      val inclusiveRadius = up(radiusMm + distanceError)
      // A borderline admitted measured minimum may have true distance r+2e.
      // Search one additional envelope beyond that so every competing true
      // minimum can contribute to the global witness uncertainty set.
      val discoveryRadius = up(radiusMm + up(3*distanceError))
      val relativeLower = Array.tabulate(3)(i => down(coordinates(i)-matrix(i,3)))
      val relativeUpper = Array.tabulate(3)(i => up(coordinates(i)-matrix(i,3)))
      val relativeNorm = (0 until 3).map(i => math.max(math.abs(relativeLower(i)),math.abs(relativeUpper(i)))).max
      val inverseCenterError = up(inverseBound.errorNorm * relativeNorm)
      var discoveryPadding = 0.0
      var axis = 0
      var empty = false
      while axis < 3 do
        // Bound each multiplication and addition, not merely the final dot-product
        // result: large cancelling terms can lose many ulps of the small centre.
        var centerLower = 0.0
        var centerUpper = 0.0
        var term = 0
        while term < 3 do
          val first = inverse(axis,term) * relativeLower(term)
          val second = inverse(axis,term) * relativeUpper(term)
          centerLower = down(centerLower + down(math.min(first,second)))
          centerUpper = up(centerUpper + up(math.max(first,second)))
          term += 1
        centerLower = down(centerLower-inverseCenterError)
        centerUpper = up(centerUpper+inverseCenterError)
        // A world ball projects to this interval in each voxel axis; +/-0.5 admits
        // every cell intersecting that ball, including sheared parallelepipeds.
        val normUpper = up(up(math.hypot(up(math.hypot(inverse(axis,0),inverse(axis,1))),inverse(axis,2))) + inverseBound.errorNorm)
        val extentUpper = up(up(discoveryRadius * normUpper) + 0.5)
        if !centerLower.isFinite || !centerUpper.isFinite || !extentUpper.isFinite then
          return invalid("Boundary query affine coordinates overflow")
        discoveryPadding = math.max(discoveryPadding, up(up((centerUpper-centerLower)/2) + up(discoveryRadius*inverseBound.errorNorm)))
        if !discoveryPadding.isFinite || discoveryPadding > 0.01 then
          return invalid("Boundary discovery uncertainty exceeds one hundredth of a voxel")
        val lo = math.ceil(down(centerLower - extentUpper))
        val hi = math.floor(up(centerUpper + extentUpper))
        if hi < 0 || lo > dims(axis) - 1 then empty = true
        lower(axis) = math.max(0.0, math.min(dims(axis).toDouble, lo)).toInt
        upper(axis) = math.max(-1.0, math.min(dims(axis) - 1.0, hi)).toInt
        axis += 1
      val count = (0 until 3).map(a => math.max(0, upper(a) - lower(a) + 1).toDouble).product
      val discovery = AtlasBoundaryDiscovery(inverseBound.residual,inverseBound.errorNorm,discoveryPadding)
      if empty || count == 0 then Right(AtlasBoundaryResult(point, fromSpace, radiusMm, Vector.empty, 0,distanceError,discovery))
      else if count > maxVisitedVoxels then invalid("Boundary query exceeds the declared voxel budget")
      else
          val labels = atlas.labelVolume
          def label(x: Int, y: Int, z: Int): Int =
            if x < 0 || y < 0 || z < 0 || x >= dims(0) || y >= dims(1) || z >= dims(2) then 0
            else labels.linear(x + dims(0) * (y + dims(1) * z))
          val best = scala.collection.mutable.Map.empty[Int, AtlasBoundaryHit]
          val witnessSets = scala.collection.mutable.Map.empty[Int, WitnessCandidates]
          val closest = new Array[Double](4)
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
                        val distance = faces(axis).closest(coordinates(0)-ox, coordinates(1)-oy, coordinates(2)-oz, closest,distanceError)
                        if !distance.isFinite then return invalid("Boundary query world distance overflow")
                        val witness = Point3D(ox+closest(0),oy+closest(1),oz+closest(2))
                        witnessSets.getOrElseUpdate(id,new WitnessCandidates).include(distance,distanceError,witness,closest(3))
                        if best.get(id).forall(_.distanceMm > distance) then
                          best.update(id, AtlasBoundaryHit(atlas.region(RegionId(id)).get, distance,
                            witness, AtlasBoundaryFace(x,y,z,axis,side), distanceError,closest(3),
                            if up(distance+distanceError) <= radiusMm then AtlasBoundaryRadiusAdmission.DefinitelyWithin
                            else AtlasBoundaryRadiusAdmission.NumericallyBorderline))
                      side += 2
                    axis += 1
                x += 1
              y += 1
            z += 1
          if cancelled() then invalid("Atlas boundary query cancelled")
          else
            val hits = best.valuesIterator.filter(_.distanceMm <= inclusiveRadius).toVector
              .sortBy(h => (h.distanceMm,h.region.id.value)).map { hit =>
                hit.copy(nearestPointErrorBoundMm = witnessSets(hit.region.id.value).bound(hit.nearestPoint,hit.nearestPointErrorBoundMm))
              }
            if hits.exists(h => !h.nearestPointErrorBoundMm.isFinite || h.nearestPointErrorBoundMm > resolutionBudget) then
              invalid("Boundary nearest-witness ambiguity exceeds local voxel resolution")
            else Right(AtlasBoundaryResult(point, fromSpace, radiusMm,hits,visited,distanceError,discovery))

  /** Covers every face whose outward distance lower bound can attain the minimum
    * upper bound. Obsolete candidates may remain, conservatively enlarging the box;
    * reset only when the new upper bound is below every previous lower bound.
    */
  private final class WitnessCandidates:
    private var minimumLower = Double.PositiveInfinity
    private var minimumUpper = Double.PositiveInfinity
    private val lower = Array.fill(3)(Double.PositiveInfinity)
    private val upper = Array.fill(3)(Double.NegativeInfinity)
    private var localBound = 0.0
    def include(distance: Double,error: Double,point: Point3D,witnessError: Double): Unit =
      val lo = down(distance-error)
      val hi = up(distance+error)
      if hi < minimumLower then
        var i = 0
        while i < 3 do
          lower(i)=Double.PositiveInfinity; upper(i)=Double.NegativeInfinity
          i += 1
        localBound = 0
      if lo <= minimumUpper then
        lower(0)=math.min(lower(0),point.x); upper(0)=math.max(upper(0),point.x)
        lower(1)=math.min(lower(1),point.y); upper(1)=math.max(upper(1),point.y)
        lower(2)=math.min(lower(2),point.z); upper(2)=math.max(upper(2),point.z)
        localBound=math.max(localBound,witnessError)
      minimumLower=math.min(minimumLower,lo)
      minimumUpper=math.min(minimumUpper,hi)
    def bound(selected: Point3D,selectedError: Double): Double =
      def delta(value: Double,axis: Int): Double = up(math.max(math.abs(value-lower(axis)),math.abs(value-upper(axis))))
      up(up(math.hypot(up(math.hypot(delta(selected.x,0),delta(selected.y,1))),delta(selected.z,2))) + up(localBound+selectedError))

  /** Closest point on a parallelogram: feasible plane projection or one of four
    * segment projections. Unit edge vectors avoid squaring physical scale in the
    * 2D face equations. No mesh construction; the caller supplies coordinate scratch.
    */
  private final class FaceMetric(ux: Double, uy: Double, uz: Double, vx: Double, vy: Double, vz: Double):
    private val ul = math.hypot(math.hypot(ux,uy),uz)
    private val vl = math.hypot(math.hypot(vx,vy),vz)
    private val ax = ux/ul; private val ay = uy/ul; private val az = uz/ul
    private val bx = vx/vl; private val by = vy/vl; private val bz = vz/vl
    private val cosine = ax*bx + ay*by + az*bz
    private val determinant = 1 - cosine*cosine
    val gramDeterminant: Double = determinant
    val valid: Boolean = ul.isFinite && vl.isFinite && ul > 0 && vl > 0 && determinant > 1e-12

    def closest(qx: Double, qy: Double, qz: Double, out: Array[Double],error: Double): Double =
      val du = qx*ax + qy*ay + qz*az
      val dv = qx*bx + qy*by + qz*bz
      val a = (du-cosine*dv)/determinant
      val b = (dv-cosine*du)/determinant
      def clamp(value: Double, length: Double): Double = math.max(0, math.min(length,value))
      val structurallyOrthogonal = (ux == 0 || vx == 0) && (uy == 0 || vy == 0) && (uz == 0 || vz == 0)
      if structurallyOrthogonal || (a > error && a < ul-error && b > error && b < vl-error) then
        val first = if structurallyOrthogonal then clamp(du,ul) else a
        val second = if structurallyOrthogonal then clamp(dv,vl) else b
        out(0)=first*ax+second*bx; out(1)=first*ay+second*by; out(2)=first*az+second*bz
        out(3)=error
        return math.hypot(math.hypot(qx-out(0),qy-out(1)),qz-out(2))
      var best = Double.PositiveInfinity
      def consider(x: Double, y: Double, z: Double): Unit =
        val distance = math.hypot(math.hypot(qx-x,qy-y),qz-z)
        if distance < best then
          best = distance; out(0)=x; out(1)=y; out(2)=z
      if a >= 0 && a <= ul && b >= 0 && b <= vl then consider(a*ax+b*bx,a*ay+b*by,a*az+b*bz)
      val edge0 = clamp(du,ul)
      consider(edge0*ax,edge0*ay,edge0*az)
      val edge1 = clamp(du-vl*cosine,ul)
      consider(vx+edge1*ax,vy+edge1*ay,vz+edge1*az)
      val edge2 = clamp(dv,vl)
      consider(edge2*bx,edge2*by,edge2*bz)
      val edge3 = clamp(dv-ul*cosine,vl)
      consider(ux+edge3*bx,uy+edge3*by,uz+edge3*bz)
      // If candidate ordering is unresolved, distance accuracy does not imply
      // coordinate accuracy. For a feasible point z and the convex face projection
      // p*, ||z-p*||² <= ||q-z||²-d*². Include feasibility/coordinate roundoff e:
      // ||z-q|| <= d+2e, d* >= max(0,d-e), then add e for the emitted point.
      val squared = if best >= error then up(up(6*best*error)+up(3*error*error))
        else up(up(best+2*error)*up(best+2*error))
      out(3)=up(error+up(math.sqrt(squared)))
      best
