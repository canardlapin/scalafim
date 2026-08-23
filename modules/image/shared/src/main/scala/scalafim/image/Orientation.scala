package scalafim.image

enum OrientationError:
  case UnknownAxisAbbreviation(value: String)
  case NonSpatialAxis(axis: Axis)
  case DuplicateAnatomicalAxes(axes: Vector[AnatomicalAxis])
  case Expected3Axes(actual: Int)
  case MissingAxisDirection(index: Int)
  case MatrixTooSmall(rows: Int, cols: Int)
  case SingularMatrix
  case InvalidAxisCode(code: Int)

  def message: String =
    this match
      case UnknownAxisAbbreviation(value) =>
        s"unknown axis abbreviation: '$value'"
      case NonSpatialAxis(axis) =>
        s"axis is not a spatial anatomical axis: $axis"
      case DuplicateAnatomicalAxes(axes) =>
        val codes = axes.map(a => math.abs(a.code)).mkString(",")
        s"axes must be orthogonal (one each of x/y/z): got $codes"
      case Expected3Axes(actual) =>
        s"orient must be length 3; got $actual"
      case MissingAxisDirection(index) =>
        s"axis $index has no direction"
      case MatrixTooSmall(rows, cols) =>
        s"pmat must be at least 3x3; got ${rows}x$cols"
      case SingularMatrix =>
        "invalid matrix input, determinant is 0"
      case InvalidAxisCode(code) =>
        s"invalid axis code: $code"

enum AnatomicalAxis(val abbrev: String, val axis: Axis, val code: Int):
  case L extends AnatomicalAxis("L", Axis.LeftRight, 1)
  case R extends AnatomicalAxis("R", Axis.RightLeft, -1)
  case P extends AnatomicalAxis("P", Axis.PosteriorAnterior, 2)
  case A extends AnatomicalAxis("A", Axis.AnteriorPosterior, -2)
  case I extends AnatomicalAxis("I", Axis.InferiorSuperior, 3)
  case S extends AnatomicalAxis("S", Axis.SuperiorInferior, -3)

object AnatomicalAxis:
  private val byAbbrev: Map[String, AnatomicalAxis] =
    Vector(
      "L" -> L,
      "LEFT" -> L,
      "R" -> R,
      "RIGHT" -> R,
      "P" -> P,
      "POSTERIOR" -> P,
      "A" -> A,
      "ANTERIOR" -> A,
      "I" -> I,
      "INFERIOR" -> I,
      "S" -> S,
      "SUPERIOR" -> S
    ).toMap

  def fromAbbrev(value: String): Either[OrientationError, AnatomicalAxis] =
    byAbbrev.get(value.trim.toUpperCase).toRight(OrientationError.UnknownAxisAbbreviation(value))

  def fromAxis(axis: Axis): Either[OrientationError, AnatomicalAxis] =
    axis match
      case Axis.LeftRight => Right(L)
      case Axis.RightLeft => Right(R)
      case Axis.PosteriorAnterior => Right(P)
      case Axis.AnteriorPosterior => Right(A)
      case Axis.InferiorSuperior => Right(I)
      case Axis.SuperiorInferior => Right(S)
      case other => Left(OrientationError.NonSpatialAxis(other))

final case class Orientation3D private (
  first: AnatomicalAxis,
  second: AnatomicalAxis,
  third: AnatomicalAxis
):
  def axes: Vector[AnatomicalAxis] =
    Vector(first, second, third)

  def axisSet: AxisSet =
    AxisSet(axes.map(_.axis)*)

object Orientation3D:
  val LPI: Orientation3D =
    unsafe(AnatomicalAxis.L, AnatomicalAxis.P, AnatomicalAxis.I)

  def make(
    first: AnatomicalAxis,
    second: AnatomicalAxis,
    third: AnatomicalAxis
  ): Either[OrientationError, Orientation3D] =
    val axes = Vector(first, second, third)
    val spatialCodes = axes.map(a => math.abs(a.code))
    if spatialCodes.distinct.length != 3 then Left(OrientationError.DuplicateAnatomicalAxes(axes))
    else Right(new Orientation3D(first, second, third))

  def unsafe(
    first: AnatomicalAxis,
    second: AnatomicalAxis,
    third: AnatomicalAxis
  ): Orientation3D =
    make(first, second, third).fold(err => throw new IllegalArgumentException(err.message), identity)

  def fromStrings(values: Seq[String]): Either[OrientationError, Orientation3D] =
    if values.length != 3 then Left(OrientationError.Expected3Axes(values.length))
    else
      for
        first <- AnatomicalAxis.fromAbbrev(values(0))
        second <- AnatomicalAxis.fromAbbrev(values(1))
        third <- AnatomicalAxis.fromAbbrev(values(2))
        orientation <- make(first, second, third)
      yield orientation

object Orientation:

  def findAnatomy3D(axis1: String = "L", axis2: String = "P", axis3: String = "I"): AxisSet =
    findAnatomy3DEither(axis1, axis2, axis3).fold(err => throw new IllegalArgumentException(err.message), identity)

  def findAnatomy3DEither(
    axis1: String = "L",
    axis2: String = "P",
    axis3: String = "I"
  ): Either[OrientationError, AxisSet] =
    Orientation3D.fromStrings(Seq(axis1, axis2, axis3)).map(_.axisSet)

  def findAnatomy3D(orientation: Orientation3D): AxisSet =
    orientation.axisSet

  def findAnatomy3D(
    axis1: AnatomicalAxis,
    axis2: AnatomicalAxis,
    axis3: AnatomicalAxis
  ): Either[OrientationError, AxisSet] =
    Orientation3D.make(axis1, axis2, axis3).map(_.axisSet)

  def permMat3D(axes: AxisSet): DMat =
    permMat3DEither(axes).fold(err => throw new IllegalArgumentException(err.message), identity)

  def permMat3D(orientation: Orientation3D): DMat =
    permMat3D(orientation.axisSet)

  def permMat3DEither(axes: AxisSet): Either[OrientationError, DMat] =
    if axes.ndim < 3 then Left(OrientationError.Expected3Axes(axes.ndim))
    else
      val a0 = axes(0).direction.toRight(OrientationError.MissingAxisDirection(0))
      val a1 = axes(1).direction.toRight(OrientationError.MissingAxisDirection(1))
      val a2 = axes(2).direction.toRight(OrientationError.MissingAxisDirection(2))

      for
        v0 <- a0
        v1 <- a1
        v2 <- a2
      yield
        val rows = Vector.tabulate(3) { r =>
          Vector(v0(r), v1(r), v2(r))
        }
        DMat.fromRows(rows)

  def findAnatomy(pmat: DMat, tol: Double = 1e-10): AxisSet =
    findAnatomyEither(pmat, tol).fold(err => throw new IllegalArgumentException(err.message), identity)

  def findAnatomyEither(pmat: DMat, tol: Double = 1e-10): Either[OrientationError, AxisSet] =
    if pmat.rows < 3 || pmat.cols < 3 then Left(OrientationError.MatrixTooSmall(pmat.rows, pmat.cols))
    else
      findAnatomyUnchecked(pmat, tol)

  private def findAnatomyUnchecked(pmat: DMat, tol: Double): Either[OrientationError, AxisSet] =

    def col(c: Int): Array[Double] =
      Array(pmat(0, c), pmat(1, c), pmat(2, c))

    var icol = col(0)
    var jcol = col(1)
    var kcol = col(2)

    icol = normalize(icol, tol)
    jcol = normalize(jcol, tol)
    jcol = orthogonalize(icol, jcol, tol)

    val knorm = norm(kcol)
    kcol =
      if knorm == 0.0 then cross(icol, jcol)
      else scale(kcol, 1.0 / knorm)

    kcol = orthogonalize(icol, kcol, tol)
    kcol = orthogonalize(jcol, kcol, tol)

    val q = Array(icol, jcol, kcol) // columns
    val detQ = det3x3Cols(q)
    if math.abs(detQ) <= tol then
      Left(OrientationError.SingularMatrix)
    else
      var vbest = Double.NegativeInfinity
      var ibest = 1
      var jbest = 2
      var kbest = 3
      var pbest = 1
      var qbest = 1
      var rbest = 1

      var i = 1
      while i <= 3 do
        var j = 1
        while j <= 3 do
          if i != j then
            var k = 1
            while k <= 3 do
              if k != i && k != j then
                var p = -1
                while p <= 1 do
                  if p != 0 then
                    var qq = -1
                    while qq <= 1 do
                      if qq != 0 then
                        var r = -1
                        while r <= 1 do
                          if r != 0 then
                            val detP = detPerm(p, i, qq, j, r, k)
                            if detP * detQ > 0.0 then
                              val crit = p * q(0)(i - 1) + qq * q(1)(j - 1) + r * q(2)(k - 1)
                              if crit > vbest then
                                vbest = crit
                                ibest = i; jbest = j; kbest = k
                                pbest = p; qbest = qq; rbest = r
                          r += 1
                      qq += 1
                  p += 1
              k += 1
          j += 1
        i += 1

      for
        ax1 <- axisFromCode(ibest * pbest)
        ax2 <- axisFromCode(jbest * qbest)
        ax3 <- axisFromCode(kbest * rbest)
      yield AxisSet(ax1.axis, ax2.axis, ax3.axis)

  def reorient(space: NeuroSpace, orient: Seq[String]): NeuroSpace =
    val orientation =
      Orientation3D.fromStrings(orient).fold(err => throw new IllegalArgumentException(err.message), identity)
    reorient(space, orientation)

  def reorient(space: NeuroSpace, orientation: Orientation3D): NeuroSpace =
    val anat = orientation.axisSet
    val pmat = permMat3D(orientation)

    val old = space.trans
    require(old.rows >= 4 && old.cols >= 4, "space.trans must be 4x4")

    val top = Array.ofDim[Double](3, 4)
    var r = 0
    while r < 3 do
      var c = 0
      while c < 4 do
        top(r)(c) = old(r, c)
        c += 1
      r += 1

    // tx = t(pmat_new) %*% trans(x)[1:3, ]
    val newTop = Array.ofDim[Double](3, 4)
    r = 0
    while r < 3 do
      var c = 0
      while c < 4 do
        var s = 0
        var sum = 0.0
        while s < 3 do
          sum += pmat(s, r) * top(s)(c) // transpose(pmat)(r,s) = pmat(s,r)
          s += 1
        newTop(r)(c) = sum
        c += 1
      r += 1

    val rows = Vector(
      Vector(newTop(0)(0), newTop(0)(1), newTop(0)(2), newTop(0)(3)),
      Vector(newTop(1)(0), newTop(1)(1), newTop(1)(2), newTop(1)(3)),
      Vector(newTop(2)(0), newTop(2)(1), newTop(2)(2), newTop(2)(3)),
      Vector(0.0, 0.0, 0.0, 1.0)
    )
    val tx = DMat.fromRows(rows)
    val newOrigin = Vector(tx(0, 3), tx(1, 3), tx(2, 3))

    val newAxes = AxisSet((anat.axes ++ space.axes.additionalAxes)*)
    NeuroSpace(
      dims = space.dims,
      spacing = Some(space.spacing),
      origin = Some(newOrigin),
      axes = Some(newAxes),
      trans = Some(tx)
    )

  def reorient[A](vol: NeuroVol[A], orient: Seq[String])(using
      MigrationValueSemantics[A]
  ): NeuroVol[A] =
    vol.copy(space = reorient(vol.space, orient))

  def reorient[A](vol: NeuroVol[A], orientation: Orientation3D)(using
      MigrationValueSemantics[A]
  ): NeuroVol[A] =
    vol.copy(space = reorient(vol.space, orientation))

  @scala.annotation.targetName("reorientNeuroVecAxes")
  def reorient[A](vec: NeuroVec[A], orient: Seq[String])(using
      MigrationValueSemantics[A]
  ): NeuroVec[A] =
    vec.copy(space = reorient(vec.space, orient))

  @scala.annotation.targetName("reorientNeuroVecOrientation")
  def reorient[A](vec: NeuroVec[A], orientation: Orientation3D)(using
      MigrationValueSemantics[A]
  ): NeuroVec[A] =
    vec.copy(space = reorient(vec.space, orientation))

  def reorient(cvol: ClusteredNeuroVol, orient: Seq[String]): ClusteredNeuroVol =
    cvol.copy(mask = reorient(cvol.mask, orient))

  def reorient(cvol: ClusteredNeuroVol, orientation: Orientation3D): ClusteredNeuroVol =
    cvol.copy(mask = reorient(cvol.mask, orientation))

  private def axisFromCode(code: Int): Either[OrientationError, AnatomicalAxis] =
    code match
      case 1 => Right(AnatomicalAxis.L)
      case -1 => Right(AnatomicalAxis.R)
      case 2 => Right(AnatomicalAxis.P)
      case -2 => Right(AnatomicalAxis.A)
      case 3 => Right(AnatomicalAxis.I)
      case -3 => Right(AnatomicalAxis.S)
      case other => Left(OrientationError.InvalidAxisCode(other))

  private def dot(a: Array[Double], b: Array[Double]): Double =
    a(0) * b(0) + a(1) * b(1) + a(2) * b(2)

  private def norm(a: Array[Double]): Double =
    math.sqrt(dot(a, a))

  private def scale(a: Array[Double], s: Double): Array[Double] =
    Array(a(0) * s, a(1) * s, a(2) * s)

  private def normalize(a: Array[Double], tol: Double): Array[Double] =
    val n = norm(a)
    if n <= tol then a else scale(a, 1.0 / n)

  private def orthogonalize(col1: Array[Double], col2: Array[Double], tol: Double): Array[Double] =
    val dotp = dot(col1, col2)
    if math.abs(dotp) > 1e-4 then
      val out = Array(col2(0) - dotp * col1(0), col2(1) - dotp * col1(1), col2(2) - dotp * col1(2))
      normalize(out, tol)
    else col2

  private def cross(a: Array[Double], b: Array[Double]): Array[Double] =
    Array(
      a(1) * b(2) - a(2) * b(1),
      a(2) * b(0) - a(0) * b(2),
      a(0) * b(1) - a(1) * b(0)
    )

  // determinant for 3x3 matrix stored as columns: q(0..2) are columns, each len 3.
  private def det3x3Cols(q: Array[Array[Double]]): Double =
    val a00 = q(0)(0); val a01 = q(1)(0); val a02 = q(2)(0)
    val a10 = q(0)(1); val a11 = q(1)(1); val a12 = q(2)(1)
    val a20 = q(0)(2); val a21 = q(1)(2); val a22 = q(2)(2)
    a00 * (a11 * a22 - a12 * a21) - a01 * (a10 * a22 - a12 * a20) + a02 * (a10 * a21 - a11 * a20)

  // determinant of P where P is a signed permutation matrix defined by:
  // row1 selects column i with sign p, row2 selects column j with sign q, row3 selects column k with sign r
  private def detPerm(p: Int, i: Int, q: Int, j: Int, r: Int, k: Int): Double =
    val perm =
      if (i == 1 && j == 2 && k == 3) || (i == 2 && j == 3 && k == 1) || (i == 3 && j == 1 && k == 2) then 1
      else -1
    (p * q * r * perm).toDouble
