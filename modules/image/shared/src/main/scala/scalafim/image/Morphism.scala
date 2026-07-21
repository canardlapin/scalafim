package scalafim.image

import scala.annotation.targetName

opaque type SpatialDomainId = String

object SpatialDomainId:
  def apply(value: String): SpatialDomainId =
    val out = value.trim
    require(out.nonEmpty, "SpatialDomainId must be non-empty")
    out

  extension (id: SpatialDomainId)
    def value: String = id

enum MorphismKind:
  case Identity, Affine3D, DenseDisplacementField, DenseCoordinateField, Path

enum InverseKind:
  case Exact, Approximate, Adjoint, Unavailable

  def hasGeometricInverse: Boolean =
    this match
      case Exact | Approximate => true
      case _ => false

  def hasAdjoint: Boolean =
    this != Unavailable

enum JacobianMode:
  case Pullback, Pushforward

enum DenseFieldKind:
  case Displacement, AbsoluteCoordinates

enum MorphismError:
  case DomainMismatch(leftTarget: SpatialDomainId, rightSource: SpatialDomainId)
  case EmptyPath
  case SingularMatrix(reason: String)
  case SingularJacobian(index: Int, reason: String)
  case NonInvertible(kind: MorphismKind, inverseKind: InverseKind)
  case DenseFieldShapeMismatch(expected: Vector[Int], actual: Vector[Int])
  case NonFiniteFieldValue(index: Int)
  case UnsupportedInterpolation(method: Resample.Method)
  case InvalidInverseParameters(reason: String)

  def message: String =
    this match
      case DomainMismatch(leftTarget, rightSource) =>
        s"cannot compose morphisms: left target '${leftTarget.value}' != right source '${rightSource.value}'"
      case EmptyPath =>
        "morphism path must contain at least one step"
      case SingularMatrix(reason) =>
        s"morphism matrix is singular: $reason"
      case SingularJacobian(index, reason) =>
        s"jacobian at index $index is singular: $reason"
      case NonInvertible(kind, inverseKind) =>
        s"$kind morphism does not have a geometric inverse (inverseKind=$inverseKind)"
      case DenseFieldShapeMismatch(expected, actual) =>
        s"dense field shape mismatch: expected $expected, actual $actual"
      case NonFiniteFieldValue(index) =>
        s"dense field value at linear index $index is not finite"
      case UnsupportedInterpolation(method) =>
        s"dense field morphism does not support interpolation method $method"
      case InvalidInverseParameters(reason) =>
        s"invalid dense field inverse parameters: $reason"

sealed trait SpatialMorphism:
  def source: SpatialDomainId
  def target: SpatialDomainId
  def kind: MorphismKind
  def cost: Double
  def methodTag: String
  def inverseKind: InverseKind

  final def transform(point: Vector[Double]): Vector[Double] =
    transform(Vector(point)).head

  final def transform(point: SpatialPoint): SpatialPoint =
    transformPoints(Vector(point)).head

  final def transform(point: WorldPoint): WorldPoint =
    transformWorldPoints(Vector(point)).head

  @targetName("transformMany")
  final def transform(points: Vector[Vector[Double]]): Vector[Vector[Double]] =
    transformWorldPoints(
      SpatialMorphism.worldPointsFromCoords(points, "morphism coordinate")
    ).map(_.toVector)

  final def transformPoints(points: Vector[SpatialPoint]): Vector[SpatialPoint] =
    transformWorldPoints(points.map(WorldPoint.fromSpatialPoint)).map(_.toSpatialPoint)

  def transformWorldPoints(points: Vector[WorldPoint]): Vector[WorldPoint]

  /** Internal primitive bridge for allocation-sensitive callers. Morphisms
    * without a specialized implementation retain the public typed behavior.
    */
  private[image] def transformWorldCoordinatesInto(
      inputX: Array[Double],
      inputY: Array[Double],
      inputZ: Array[Double],
      outputX: Array[Double],
      outputY: Array[Double],
      outputZ: Array[Double]
  ): Unit =
    require(
      inputX.length == inputY.length && inputX.length == inputZ.length,
      "morphism input coordinate buffers must have equal lengths"
    )
    require(
      outputX.length >= inputX.length && outputY.length >= inputX.length && outputZ.length >= inputX.length,
      "morphism output coordinate buffers are too small"
    )
    val points = Vector.tabulate(inputX.length) { index =>
      WorldPoint(inputX(index), inputY(index), inputZ(index))
    }
    val transformed = transformWorldPoints(points)
    require(transformed.length == inputX.length, "spatial morphism must preserve mapped point count")
    var index = 0
    while index < transformed.length do
      val point = transformed(index)
      outputX(index) = point.x
      outputY(index) = point.y
      outputZ(index) = point.z
      index += 1

  final def jacobian(
      coords: Vector[Vector[Double]],
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, JacobianField] =
    jacobianAtWorld(SpatialMorphism.worldPointsFromCoords(coords, "jacobian coordinate"), mode)

  final def jacobianAt(
      point: SpatialPoint,
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, DMat] =
    jacobianAt(Vector(point), mode).map(_(0))

  final def jacobianAt(points: Vector[SpatialPoint]): Either[MorphismError, JacobianField] =
    jacobianAt(points, JacobianMode.Pullback)

  final def jacobianAt(
      points: Vector[SpatialPoint],
      mode: JacobianMode
  ): Either[MorphismError, JacobianField] =
    jacobianAtWorld(points.map(WorldPoint.fromSpatialPoint), mode)

  final def jacobianAtWorld(
      point: WorldPoint,
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, DMat] =
    jacobianAtWorld(Vector(point), mode).map(_(0))

  final def jacobianAtWorld(points: Vector[WorldPoint]): Either[MorphismError, JacobianField] =
    jacobianAtWorld(points, JacobianMode.Pullback)

  def jacobianAtWorld(
      points: Vector[WorldPoint],
      mode: JacobianMode
  ): Either[MorphismError, JacobianField]

  def jacobianDet(
      coords: Vector[Vector[Double]],
      log: Boolean = false,
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, Vector[Double]] =
    jacobian(coords, mode).map { field =>
      val dets = field.determinants
      if log then dets.map(d => math.log(math.abs(d))) else dets
    }

  final def jacobianDetAt(
      point: SpatialPoint,
      log: Boolean = false,
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, Double] =
    jacobianDetAt(Vector(point), log, mode).map(_.head)

  final def jacobianDetAt(points: Vector[SpatialPoint]): Either[MorphismError, Vector[Double]] =
    jacobianDetAt(points, log = false, mode = JacobianMode.Pullback)

  final def jacobianDetAt(
      points: Vector[SpatialPoint],
      log: Boolean
  ): Either[MorphismError, Vector[Double]] =
    jacobianDetAt(points, log, JacobianMode.Pullback)

  final def jacobianDetAt(
      points: Vector[SpatialPoint],
      log: Boolean,
      mode: JacobianMode
  ): Either[MorphismError, Vector[Double]] =
    jacobianDetAtWorld(points.map(WorldPoint.fromSpatialPoint), log, mode)

  final def jacobianDetAtWorld(
      point: WorldPoint,
      log: Boolean = false,
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, Double] =
    jacobianDetAtWorld(Vector(point), log, mode).map(_.head)

  final def jacobianDetAtWorld(points: Vector[WorldPoint]): Either[MorphismError, Vector[Double]] =
    jacobianDetAtWorld(points, log = false, mode = JacobianMode.Pullback)

  final def jacobianDetAtWorld(
      points: Vector[WorldPoint],
      log: Boolean
  ): Either[MorphismError, Vector[Double]] =
    jacobianDetAtWorld(points, log, JacobianMode.Pullback)

  final def jacobianDetAtWorld(
      points: Vector[WorldPoint],
      log: Boolean,
      mode: JacobianMode
  ): Either[MorphismError, Vector[Double]] =
    jacobianAtWorld(points, mode).map { field =>
      val dets = field.determinants
      if log then dets.map(d => math.log(math.abs(d))) else dets
    }

  def invert: Either[MorphismError, SpatialMorphism]

  final def andThen(next: SpatialMorphism): Either[MorphismError, SpatialMorphism] =
    SpatialMorphism.compose(this, next)

final case class IdentityMorphism(domain: SpatialDomainId) extends SpatialMorphism:
  def source: SpatialDomainId = domain
  def target: SpatialDomainId = domain
  def kind: MorphismKind = MorphismKind.Identity
  def cost: Double = 0.0
  def methodTag: String = "identity"
  def inverseKind: InverseKind = InverseKind.Exact

  def transformWorldPoints(points: Vector[WorldPoint]): Vector[WorldPoint] =
    points

  def jacobianAtWorld(
      points: Vector[WorldPoint],
      mode: JacobianMode
  ): Either[MorphismError, JacobianField] =
    Right(JacobianField.constantAtWorld(points, DMat.eye(3), mode))

  def invert: Either[MorphismError, SpatialMorphism] =
    Right(this)

object IdentityMorphism:
  @targetName("fromString")
  def apply(domain: String): IdentityMorphism =
    IdentityMorphism(SpatialDomainId(domain))

final case class Affine3DMorphism private (
    source: SpatialDomainId,
    target: SpatialDomainId,
    matrix: DMat,
    cost: Double,
    methodTag: String
) extends SpatialMorphism:
  require(cost.isFinite && cost >= 0.0, "affine morphism cost must be finite and non-negative")
  Affine3DMorphism.requireAffine3D(matrix)

  def kind: MorphismKind = MorphismKind.Affine3D
  def inverseKind: InverseKind = InverseKind.Exact

  def transformWorldPoints(points: Vector[WorldPoint]): Vector[WorldPoint] =
    points.map { point =>
      WorldPoint.unsafeFromVector(Affine.applyAffine(matrix, point.toVector), "transformed world point")
    }

  def jacobianAtWorld(
      points: Vector[WorldPoint],
      mode: JacobianMode
  ): Either[MorphismError, JacobianField] =
    val linear = Affine3DMorphism.linearPart(matrix)
    val j =
      mode match
        case JacobianMode.Pullback => Right(linear)
        case JacobianMode.Pushforward =>
          DMat.invert(linear).left.map(MorphismError.SingularMatrix.apply)
    j.map(mat => JacobianField.constantAtWorld(points, mat, mode))

  def invert: Either[MorphismError, SpatialMorphism] =
    DMat.invert(matrix) match
      case Left(msg) => Left(MorphismError.SingularMatrix(msg))
      case Right(inv) =>
        Right(Affine3DMorphism.unsafe(target, source, inv, cost, methodTag))

object Affine3DMorphism:
  def apply(
      source: String,
      target: String,
      matrix: DMat,
      cost: Double = 1.0,
      methodTag: String = "anatomical"
  ): Either[MorphismError, Affine3DMorphism] =
    make(SpatialDomainId(source), SpatialDomainId(target), matrix, cost, methodTag)

  def make(
      source: SpatialDomainId,
      target: SpatialDomainId,
      matrix: DMat,
      cost: Double = 1.0,
      methodTag: String = "anatomical"
  ): Either[MorphismError, Affine3DMorphism] =
    requireAffine3D(matrix)
    DMat.invert(matrix) match
      case Left(msg) => Left(MorphismError.SingularMatrix(msg))
      case Right(_) => Right(unsafe(source, target, matrix, cost, methodTag))

  def unsafe(
      source: SpatialDomainId,
      target: SpatialDomainId,
      matrix: DMat,
      cost: Double = 1.0,
      methodTag: String = "anatomical"
  ): Affine3DMorphism =
    new Affine3DMorphism(source, target, matrix, cost, methodTag)

  private[image] def requireAffine3D(matrix: DMat): Unit =
    require(matrix.rows == 4 && matrix.cols == 4, "Affine3DMorphism requires a 4x4 matrix")
    var i = 0
    while i < matrix.data.length do
      require(matrix.data(i).isFinite, "Affine3DMorphism matrix values must be finite")
      i += 1

  private[image] def linearPart(matrix: DMat): DMat =
    DMat.fromRows(
      Vector.tabulate(3)(r => Vector.tabulate(3)(c => matrix(r, c)))
    )

final case class DenseFieldMorphism private (
    source: SpatialDomainId,
    target: SpatialDomainId,
    grid: GridSpec,
    field: NDArray[Double],
    fieldKind: DenseFieldKind,
    interpolation: Resample.Method,
    cost: Double,
    methodTag: String
) extends SpatialMorphism:
  require(field.ndim == 4, "dense field must be 4D")
  require(field.shape == (grid.dims :+ 3), "dense field shape must be grid dims plus vector components")
  require(cost.isFinite && cost >= 0.0, "dense field morphism cost must be finite and non-negative")

  private lazy val inverseGridAffine: DMat =
    DMat.invert(grid.affine).fold(
      reason => throw new IllegalArgumentException(reason),
      identity
    )

  def kind: MorphismKind =
    fieldKind match
      case DenseFieldKind.Displacement => MorphismKind.DenseDisplacementField
      case DenseFieldKind.AbsoluteCoordinates => MorphismKind.DenseCoordinateField

  def inverseKind: InverseKind = InverseKind.Unavailable

  def transformWorldPoints(points: Vector[WorldPoint]): Vector[WorldPoint] =
    val sampled = sampleFieldAtWorldPoints(points)
    fieldKind match
      case DenseFieldKind.Displacement =>
        Vector.tabulate(points.length) { i =>
          val point = points(i)
          val offset = sampled(i)
          WorldPoint(point.x + offset(0), point.y + offset(1), point.z + offset(2))
        }
      case DenseFieldKind.AbsoluteCoordinates =>
        sampled.map(point => WorldPoint.unsafeFromVector(point, "dense field transformed world point"))

  private[image] override def transformWorldCoordinatesInto(
      inputX: Array[Double],
      inputY: Array[Double],
      inputZ: Array[Double],
      outputX: Array[Double],
      outputY: Array[Double],
      outputZ: Array[Double]
  ): Unit =
    interpolation match
      case Resample.Method.Cubic =>
        super.transformWorldCoordinatesInto(inputX, inputY, inputZ, outputX, outputY, outputZ)
      case Resample.Method.Nearest | Resample.Method.Linear =>
        require(
          inputX.length == inputY.length && inputX.length == inputZ.length,
          "morphism input coordinate buffers must have equal lengths"
        )
        require(
          outputX.length >= inputX.length && outputY.length >= inputX.length && outputZ.length >= inputX.length,
          "morphism output coordinate buffers are too small"
        )
        var index = 0
        while index < inputX.length do
          val worldX = inputX(index)
          val worldY = inputY(index)
          val worldZ = inputZ(index)
          val voxelX = affineCoordinate(inverseGridAffine, 0, worldX, worldY, worldZ)
          val voxelY = affineCoordinate(inverseGridAffine, 1, worldX, worldY, worldZ)
          val voxelZ = affineCoordinate(inverseGridAffine, 2, worldX, worldY, worldZ)
          var sampledX = 0.0
          var sampledY = 0.0
          var sampledZ = 0.0
          interpolation match
            case Resample.Method.Nearest =>
              val xi = math.round(voxelX).toInt
              val yi = math.round(voxelY).toInt
              val zi = math.round(voxelZ).toInt
              if fieldInBounds(xi, yi, zi) then
                val base = fieldIndex(xi, yi, zi)
                sampledX = field.data(base)
                sampledY = field.data(base + grid.nVoxels)
                sampledZ = field.data(base + 2 * grid.nVoxels)
              else if fieldKind == DenseFieldKind.AbsoluteCoordinates then
                sampledX = worldX
                sampledY = worldY
                sampledZ = worldZ
            case Resample.Method.Linear =>
              val outsideWeight = linearOutsideWeight(voxelX, voxelY, voxelZ)
              sampledX = sampleLinearComponent(voxelX, voxelY, voxelZ, worldX, 0, outsideWeight)
              sampledY = sampleLinearComponent(voxelX, voxelY, voxelZ, worldY, 1, outsideWeight)
              sampledZ = sampleLinearComponent(voxelX, voxelY, voxelZ, worldZ, 2, outsideWeight)
            case Resample.Method.Cubic =>
              throw new IllegalStateException("cubic dense fields use the typed fallback path")
          fieldKind match
            case DenseFieldKind.Displacement =>
              outputX(index) = worldX + sampledX
              outputY(index) = worldY + sampledY
              outputZ(index) = worldZ + sampledZ
            case DenseFieldKind.AbsoluteCoordinates =>
              outputX(index) = sampledX
              outputY(index) = sampledY
              outputZ(index) = sampledZ
          index += 1

  def jacobianAtWorld(
      points: Vector[WorldPoint],
      mode: JacobianMode
  ): Either[MorphismError, JacobianField] =
    val pullback =
      JacobianField.atWorld(
        points,
        points.map(point => numericJacobian(point, step = 1e-3)),
        JacobianMode.Pullback
      )
    if mode == JacobianMode.Pullback then Right(pullback) else pullback.invert

  def invert: Either[MorphismError, SpatialMorphism] =
    Left(MorphismError.NonInvertible(kind, inverseKind))

  private def numericJacobian(point: WorldPoint, step: Double): DMat =
    DMat.fromRows(
      Vector.tabulate(3) { row =>
        Vector.tabulate(3) { col =>
          val plus = shiftAxis(point, col, step)
          val minus = shiftAxis(point, col, -step)
          (transform(plus).toVector(row) - transform(minus).toVector(row)) / (2.0 * step)
        }
      }
    )

  def interpolationPlanAtWorldPoints(points: Vector[WorldPoint]): Either[MorphismError, DenseFieldInterpolationPlan] =
    DenseFieldInterpolationPlan.fromWorldPoints(grid, points, interpolation)

  def interpolationPlan(points: Vector[Vector[Double]]): Either[MorphismError, DenseFieldInterpolationPlan] =
    DenseFieldInterpolationPlan.make(grid, points, interpolation)

  def approximateInverse(
      inverseGrid: GridSpec,
      options: DenseFieldInverseOptions = DenseFieldInverseOptions()
  ): Either[MorphismError, DenseFieldInverseResult] =
    DenseFieldInverse.approximate(this, inverseGrid, options)

  private def sampleFieldAtWorldPoints(points: Vector[WorldPoint]): Vector[Vector[Double]] =
    val plan =
      interpolationPlanAtWorldPoints(points).fold(
        err => throw new IllegalArgumentException(err.message),
        identity
      )
    plan.sampleUnsafe(field, outsidePolicy)

  private def shiftAxis(point: WorldPoint, axis: Int, offset: Double): WorldPoint =
    axis match
      case 0 => WorldPoint(point.x + offset, point.y, point.z)
      case 1 => WorldPoint(point.x, point.y + offset, point.z)
      case _ => WorldPoint(point.x, point.y, point.z + offset)

  private def outsidePolicy: DenseFieldOutside =
    fieldKind match
      case DenseFieldKind.Displacement => DenseFieldOutside.Zero
      case DenseFieldKind.AbsoluteCoordinates => DenseFieldOutside.QueryPoint

  private inline def affineCoordinate(
      matrix: DMat,
      row: Int,
      x: Double,
      y: Double,
      z: Double
  ): Double =
    var sum = matrix(row, 3)
    sum += matrix(row, 0) * x
    sum += matrix(row, 1) * y
    sum += matrix(row, 2) * z
    sum

  private def linearOutsideWeight(
      x: Double,
      y: Double,
      z: Double
  ): Double =
    val x0 = math.floor(x).toInt
    val y0 = math.floor(y).toInt
    val z0 = math.floor(z).toInt
    val xd = x - x0
    val yd = y - y0
    val zd = z - z0
    var outsideWeight = 0.0
    var dz = 0
    while dz <= 1 do
      val wz = if dz == 0 then 1.0 - zd else zd
      var dy = 0
      while dy <= 1 do
        val wy = if dy == 0 then 1.0 - yd else yd
        var dx = 0
        while dx <= 1 do
          val wx = if dx == 0 then 1.0 - xd else xd
          val weight = wx * wy * wz
          if weight != 0.0 && !fieldInBounds(x0 + dx, y0 + dy, z0 + dz) then
            outsideWeight += weight
          dx += 1
        dy += 1
      dz += 1
    outsideWeight

  private def sampleLinearComponent(
      x: Double,
      y: Double,
      z: Double,
      outsideValue: Double,
      component: Int,
      outsideWeight: Double
  ): Double =
    val x0 = math.floor(x).toInt
    val y0 = math.floor(y).toInt
    val z0 = math.floor(z).toInt
    val xd = x - x0
    val yd = y - y0
    val zd = z - z0
    var sum =
      if fieldKind == DenseFieldKind.Displacement then 0.0
      else outsideWeight * outsideValue
    val componentOffset = component * grid.nVoxels
    var dz = 0
    while dz <= 1 do
      val wz = if dz == 0 then 1.0 - zd else zd
      val zi = z0 + dz
      var dy = 0
      while dy <= 1 do
        val wy = if dy == 0 then 1.0 - yd else yd
        val yi = y0 + dy
        var dx = 0
        while dx <= 1 do
          val wx = if dx == 0 then 1.0 - xd else xd
          val xi = x0 + dx
          val weight = wx * wy * wz
          if weight != 0.0 && fieldInBounds(xi, yi, zi) then
            sum += weight * field.data(fieldIndex(xi, yi, zi) + componentOffset)
          dx += 1
        dy += 1
      dz += 1
    sum

  private inline def fieldInBounds(x: Int, y: Int, z: Int): Boolean =
    x >= 0 && x < grid.shape.x &&
      y >= 0 && y < grid.shape.y &&
      z >= 0 && z < grid.shape.z

  private inline def fieldIndex(x: Int, y: Int, z: Int): Int =
    x + y * grid.shape.x + z * grid.shape.x * grid.shape.y

object DenseFieldMorphism:
  def displacement(
      source: SpatialDomainId,
      target: SpatialDomainId,
      grid: GridSpec,
      field: NDArray[Double],
      interpolation: Resample.Method = Resample.Method.Linear,
      cost: Double = 10.0,
      methodTag: String = "dense-displacement"
  ): Either[MorphismError, DenseFieldMorphism] =
    make(source, target, grid, field, DenseFieldKind.Displacement, interpolation, cost, methodTag)

  def coordinates(
      source: SpatialDomainId,
      target: SpatialDomainId,
      grid: GridSpec,
      field: NDArray[Double],
      interpolation: Resample.Method = Resample.Method.Linear,
      cost: Double = 10.0,
      methodTag: String = "dense-coordinate"
  ): Either[MorphismError, DenseFieldMorphism] =
    make(source, target, grid, field, DenseFieldKind.AbsoluteCoordinates, interpolation, cost, methodTag)

  def make(
      source: SpatialDomainId,
      target: SpatialDomainId,
      grid: GridSpec,
      field: NDArray[Double],
      fieldKind: DenseFieldKind,
      interpolation: Resample.Method = Resample.Method.Linear,
      cost: Double = 10.0,
      methodTag: String = "dense-field"
  ): Either[MorphismError, DenseFieldMorphism] =
    validate(grid, field, interpolation).map { _ =>
      unsafe(source, target, grid, field, fieldKind, interpolation, cost, methodTag)
    }

  private[image] def unsafe(
      source: SpatialDomainId,
      target: SpatialDomainId,
      grid: GridSpec,
      field: NDArray[Double],
      fieldKind: DenseFieldKind,
      interpolation: Resample.Method = Resample.Method.Linear,
      cost: Double = 10.0,
      methodTag: String = "dense-field"
  ): DenseFieldMorphism =
    new DenseFieldMorphism(source, target, grid, field, fieldKind, interpolation, cost, methodTag)

  private def validate(
      grid: GridSpec,
      field: NDArray[Double],
      interpolation: Resample.Method
  ): Either[MorphismError, Unit] =
    for
      _ <- DenseFieldInterpolationPlan.validateField(grid, field)
      _ <- DenseFieldInterpolationPlan.validateMethod(interpolation)
      _ <- DMat.invert(grid.affine).left.map(MorphismError.SingularMatrix.apply)
    yield ()

final case class MorphismPath private (steps: Vector[SpatialMorphism]) extends SpatialMorphism:
  require(steps.nonEmpty, "morphism path must contain at least one step")

  def source: SpatialDomainId = steps.head.source
  def target: SpatialDomainId = steps.last.target
  def kind: MorphismKind = MorphismKind.Path
  def cost: Double = steps.map(_.cost).sum
  def methodTag: String = "path"
  def inverseKind: InverseKind =
    if steps.forall(_.inverseKind == InverseKind.Exact) then InverseKind.Exact
    else if steps.forall(_.inverseKind.hasGeometricInverse) then InverseKind.Approximate
    else InverseKind.Unavailable

  def transformWorldPoints(points: Vector[WorldPoint]): Vector[WorldPoint] =
    var out = points
    var i = steps.length - 1
    while i >= 0 do
      out = steps(i).transformWorldPoints(out)
      i -= 1
    out

  def jacobianAtWorld(
      points: Vector[WorldPoint],
      mode: JacobianMode
  ): Either[MorphismError, JacobianField] =
    var currentPoints = points
    var result = Option.empty[JacobianField]
    var error = Option.empty[MorphismError]
    var i = steps.length - 1
    while i >= 0 && error.isEmpty do
      steps(i).jacobianAtWorld(currentPoints, JacobianMode.Pullback) match
        case Left(err) => error = Some(err)
        case Right(j) =>
          result = result match
            case None => Some(j)
            case Some(acc) => Some(JacobianField.multiply(j, acc))
          if i > 0 then currentPoints = steps(i).transformWorldPoints(currentPoints)
      i -= 1

    error match
      case Some(err) => Left(err)
      case None =>
        val pullback = result.getOrElse(JacobianField.constantAtWorld(points, DMat.eye(3), JacobianMode.Pullback))
        val field = pullback.withWorldPointsAndMode(points, mode)
        if mode == JacobianMode.Pullback then Right(field) else field.invert

  def invert: Either[MorphismError, SpatialMorphism] =
    if !inverseKind.hasGeometricInverse then Left(MorphismError.NonInvertible(kind, inverseKind))
    else
      val inverted = Vector.newBuilder[SpatialMorphism]
      var error = Option.empty[MorphismError]
      var i = steps.length - 1
      while i >= 0 && error.isEmpty do
        steps(i).invert match
          case Left(err) => error = Some(err)
          case Right(inv) => inverted += inv
        i -= 1

      error match
        case Some(err) => Left(err)
        case None => SpatialMorphism.path(inverted.result())

object MorphismPath:
  def make(steps: Vector[SpatialMorphism]): Either[MorphismError, MorphismPath] =
    if steps.isEmpty then Left(MorphismError.EmptyPath)
    else
      var i = 0
      var error = Option.empty[MorphismError]
      while i < steps.length - 1 && error.isEmpty do
        val left = steps(i)
        val right = steps(i + 1)
        if left.target != right.source then
          error = Some(MorphismError.DomainMismatch(left.target, right.source))
        i += 1

      error match
        case Some(err) => Left(err)
        case None => Right(new MorphismPath(steps))

object SpatialMorphism:
  def compose(left: SpatialMorphism, right: SpatialMorphism): Either[MorphismError, SpatialMorphism] =
    if left.target != right.source then Left(MorphismError.DomainMismatch(left.target, right.source))
    else
      (left, right) match
        case (_: IdentityMorphism, _) => Right(right)
        case (_, _: IdentityMorphism) => Right(left)
        case (l: Affine3DMorphism, r: Affine3DMorphism) =>
          Right(
            Affine3DMorphism.unsafe(
              l.source,
              r.target,
              Affine.multiply(l.matrix, r.matrix),
              l.cost + r.cost,
              l.methodTag
            )
          )
        case _ =>
          path(flatten(left) ++ flatten(right))

  def path(steps: Vector[SpatialMorphism]): Either[MorphismError, SpatialMorphism] =
    MorphismPath.make(steps).map { path =>
      if path.steps.length == 1 then path.steps.head else path
    }

  private def flatten(morphism: SpatialMorphism): Vector[SpatialMorphism] =
    morphism match
      case path: MorphismPath => path.steps
      case other => Vector(other)

  private[image] def validateCoords(coords: Vector[Vector[Double]]): Unit =
    coords.foreach { point =>
      require(point.length == 3, "coordinates must be 3D")
      require(point.forall(_.isFinite), "coordinates must be finite")
    }

  private[image] def worldPointsFromCoords(coords: Vector[Vector[Double]], label: String): Vector[WorldPoint] =
    validateCoords(coords)
    coords.map(point => WorldPoint.unsafeFromVector(point, label))

final case class JacobianField(
    coords: Vector[Vector[Double]],
    matrices: Vector[DMat],
    mode: JacobianMode
):
  require(coords.length == matrices.length, "coords and matrices length mismatch")
  SpatialMorphism.validateCoords(coords)
  matrices.foreach { matrix =>
    require(matrix.rows == 3 && matrix.cols == 3, "jacobian matrices must be 3x3")
  }

  def size: Int = matrices.length

  def apply(index: Int): DMat =
    matrices(index)

  def determinants: Vector[Double] =
    matrices.map(Matrix3.det)

  def logAbsDeterminants: Vector[Double] =
    determinants.map(d => math.log(math.abs(d)))

  def invert: Either[MorphismError, JacobianField] =
    val out = Vector.newBuilder[DMat]
    var error = Option.empty[MorphismError]
    var i = 0
    while i < matrices.length && error.isEmpty do
      DMat.invert(matrices(i)) match
        case Left(msg) => error = Some(MorphismError.SingularJacobian(i, msg))
        case Right(inv) => out += inv
      i += 1

    error match
      case Some(err) => Left(err)
      case None =>
        val nextMode =
          mode match
            case JacobianMode.Pullback => JacobianMode.Pushforward
            case JacobianMode.Pushforward => JacobianMode.Pullback
        Right(JacobianField(coords, out.result(), nextMode))

  private[image] def withCoordsAndMode(newCoords: Vector[Vector[Double]], newMode: JacobianMode): JacobianField =
    JacobianField(newCoords, matrices, newMode)

  private[image] def withWorldPointsAndMode(points: Vector[WorldPoint], newMode: JacobianMode): JacobianField =
    withCoordsAndMode(points.map(_.toVector), newMode)

object JacobianField:
  def atWorld(points: Vector[WorldPoint], matrices: Vector[DMat], mode: JacobianMode): JacobianField =
    JacobianField(points.map(_.toVector), matrices, mode)

  def constantAtWorld(points: Vector[WorldPoint], matrix: DMat, mode: JacobianMode): JacobianField =
    atWorld(points, Vector.fill(points.length)(matrix), mode)

  def constant(coords: Vector[Vector[Double]], matrix: DMat, mode: JacobianMode): JacobianField =
    JacobianField(coords, Vector.fill(coords.length)(matrix), mode)

  def multiply(left: JacobianField, right: JacobianField): JacobianField =
    require(left.size == right.size, "jacobian fields must have the same length")
    val mats =
      Vector.tabulate(left.size) { i =>
        Affine.multiply(left.matrices(i), right.matrices(i))
      }
    JacobianField(right.coords, mats, left.mode)

private object Matrix3:
  def det(matrix: DMat): Double =
    require(matrix.rows == 3 && matrix.cols == 3, "det requires a 3x3 matrix")
    val a = matrix(0, 0)
    val b = matrix(0, 1)
    val c = matrix(0, 2)
    val d = matrix(1, 0)
    val e = matrix(1, 1)
    val f = matrix(1, 2)
    val g = matrix(2, 0)
    val h = matrix(2, 1)
    val i = matrix(2, 2)
    a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
