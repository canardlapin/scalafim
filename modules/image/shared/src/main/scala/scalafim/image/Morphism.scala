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
    SpatialPoint.unsafeFromVector(transform(point.toVector), "transformed point")

  final def transform(point: WorldPoint): WorldPoint =
    WorldPoint.unsafeFromVector(transform(point.toVector), "transformed world point")

  @targetName("transformMany")
  def transform(points: Vector[Vector[Double]]): Vector[Vector[Double]]

  final def transformPoints(points: Vector[SpatialPoint]): Vector[SpatialPoint] =
    transform(points.map(_.toVector)).map(point => SpatialPoint.unsafeFromVector(point, "transformed point"))

  final def transformWorldPoints(points: Vector[WorldPoint]): Vector[WorldPoint] =
    transform(points.map(_.toVector)).map(point => WorldPoint.unsafeFromVector(point, "transformed world point"))

  def jacobian(
      coords: Vector[Vector[Double]],
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, JacobianField]

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
    jacobian(points.map(_.toVector), mode)

  final def jacobianAtWorld(
      point: WorldPoint,
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, DMat] =
    jacobianAtWorld(Vector(point), mode).map(_(0))

  final def jacobianAtWorld(points: Vector[WorldPoint]): Either[MorphismError, JacobianField] =
    jacobianAtWorld(points, JacobianMode.Pullback)

  final def jacobianAtWorld(
      points: Vector[WorldPoint],
      mode: JacobianMode
  ): Either[MorphismError, JacobianField] =
    jacobian(points.map(_.toVector), mode)

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
    jacobianDet(points.map(_.toVector), log, mode)

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
    jacobianDet(points.map(_.toVector), log, mode)

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

  @targetName("transformMany")
  def transform(points: Vector[Vector[Double]]): Vector[Vector[Double]] =
    SpatialMorphism.validateCoords(points)
    points

  def jacobian(
      coords: Vector[Vector[Double]],
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, JacobianField] =
    SpatialMorphism.validateCoords(coords)
    Right(JacobianField.constant(coords, DMat.eye(3), mode))

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

  @targetName("transformMany")
  def transform(points: Vector[Vector[Double]]): Vector[Vector[Double]] =
    SpatialMorphism.validateCoords(points)
    points.map(point => Affine.applyAffine(matrix, point))

  def jacobian(
      coords: Vector[Vector[Double]],
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, JacobianField] =
    SpatialMorphism.validateCoords(coords)
    val linear = Affine3DMorphism.linearPart(matrix)
    val j =
      mode match
        case JacobianMode.Pullback => Right(linear)
        case JacobianMode.Pushforward =>
          DMat.invert(linear).left.map(MorphismError.SingularMatrix.apply)
    j.map(mat => JacobianField.constant(coords, mat, mode))

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

  def kind: MorphismKind =
    fieldKind match
      case DenseFieldKind.Displacement => MorphismKind.DenseDisplacementField
      case DenseFieldKind.AbsoluteCoordinates => MorphismKind.DenseCoordinateField

  def inverseKind: InverseKind = InverseKind.Unavailable

  @targetName("transformMany")
  def transform(points: Vector[Vector[Double]]): Vector[Vector[Double]] =
    SpatialMorphism.validateCoords(points)
    val sampled = sampleField(points)
    fieldKind match
      case DenseFieldKind.Displacement =>
        Vector.tabulate(points.length) { i =>
          Vector.tabulate(3)(axis => points(i)(axis) + sampled(i)(axis))
        }
      case DenseFieldKind.AbsoluteCoordinates =>
        sampled

  def jacobian(
      coords: Vector[Vector[Double]],
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, JacobianField] =
    SpatialMorphism.validateCoords(coords)
    val pullback =
      JacobianField(
        coords,
        coords.map(point => numericJacobian(point, step = 1e-3)),
        JacobianMode.Pullback
      )
    if mode == JacobianMode.Pullback then Right(pullback) else pullback.invert

  def invert: Either[MorphismError, SpatialMorphism] =
    Left(MorphismError.NonInvertible(kind, inverseKind))

  private def transformOne(point: Vector[Double]): Vector[Double] =
    transform(Vector(point)).head

  private def numericJacobian(point: Vector[Double], step: Double): DMat =
    DMat.fromRows(
      Vector.tabulate(3) { row =>
        Vector.tabulate(3) { col =>
          val plus = point.updated(col, point(col) + step)
          val minus = point.updated(col, point(col) - step)
          (transformOne(plus)(row) - transformOne(minus)(row)) / (2.0 * step)
        }
      }
    )

  def interpolationPlan(points: Vector[Vector[Double]]): Either[MorphismError, DenseFieldInterpolationPlan] =
    DenseFieldInterpolationPlan.make(grid, points, interpolation)

  def interpolationPlanAtWorldPoints(points: Vector[WorldPoint]): Either[MorphismError, DenseFieldInterpolationPlan] =
    DenseFieldInterpolationPlan.fromWorldPoints(grid, points, interpolation)

  def approximateInverse(
      inverseGrid: GridSpec,
      options: DenseFieldInverseOptions = DenseFieldInverseOptions()
  ): Either[MorphismError, DenseFieldInverseResult] =
    DenseFieldInverse.approximate(this, inverseGrid, options)

  private def sampleField(points: Vector[Vector[Double]]): Vector[Vector[Double]] =
    val plan =
      interpolationPlan(points).fold(
        err => throw new IllegalArgumentException(err.message),
        identity
      )
    plan.sampleUnsafe(field, outsidePolicy)

  private def outsidePolicy: DenseFieldOutside =
    fieldKind match
      case DenseFieldKind.Displacement => DenseFieldOutside.Zero
      case DenseFieldKind.AbsoluteCoordinates => DenseFieldOutside.QueryPoint

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

  @targetName("transformMany")
  def transform(points: Vector[Vector[Double]]): Vector[Vector[Double]] =
    SpatialMorphism.validateCoords(points)
    var out = points
    var i = steps.length - 1
    while i >= 0 do
      out = steps(i).transform(out)
      i -= 1
    out

  def jacobian(
      coords: Vector[Vector[Double]],
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, JacobianField] =
    SpatialMorphism.validateCoords(coords)
    var currentCoords = coords
    var result = Option.empty[JacobianField]
    var error = Option.empty[MorphismError]
    var i = steps.length - 1
    while i >= 0 && error.isEmpty do
      steps(i).jacobian(currentCoords, JacobianMode.Pullback) match
        case Left(err) => error = Some(err)
        case Right(j) =>
          result = result match
            case None => Some(j)
            case Some(acc) => Some(JacobianField.multiply(j, acc))
          if i > 0 then currentCoords = steps(i).transform(currentCoords)
      i -= 1

    error match
      case Some(err) => Left(err)
      case None =>
        val pullback = result.getOrElse(JacobianField.constant(coords, DMat.eye(3), JacobianMode.Pullback))
        val field = pullback.withCoordsAndMode(coords, mode)
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

object JacobianField:
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
