package scalafim.transform.itk

import scalafim.transform.{TransformIoError, UnsupportedFormat}

/** One ITK transform as stored: its registered type name (e.g. `AffineTransform_double_3_3`), parameters and fixed
  * parameters, exactly as written.
  */
final case class ItkEntry(typeName: String, parameters: Vector[Double], fixedParameters: Vector[Double]) derives CanEqual:
  /** The type without its precision/dimension suffix, e.g. `AffineTransform`. */
  def kind: String =
    typeName.split('_').headOption.getOrElse(typeName)

  def isComposite: Boolean =
    kind == "CompositeTransform"

/** An ITK transform file's content: one transform, or a `CompositeTransform` marker followed by its components in
  * file order. ITK applies composite components last-first: a point goes through the final entry first.
  */
final case class ItkTransformFile(entries: Vector[ItkEntry]) derives CanEqual:
  /** Components in file order, without the composite marker. */
  def components: Vector[ItkEntry] =
    entries.filterNot(_.isComposite)

  def isComposite: Boolean =
    entries.headOption.exists(_.isComposite)

/** A linear ITK transform in ITK's own matrix-offset form, in LPS millimetres: `y = matrix * x + offset`. */
final case class ItkMatrixOffset(matrix: Vector[Double], offset: Vector[Double]) derives CanEqual:
  def rowMajor4: Vector[Double] =
    Vector(
      matrix(0), matrix(1), matrix(2), offset(0),
      matrix(3), matrix(4), matrix(5), offset(1),
      matrix(6), matrix(7), matrix(8), offset(2),
      0.0, 0.0, 0.0, 1.0
    )

/** ITK's linear parameterisations, reproducing each class's `ComputeMatrix`/`ComputeOffset` (ITK 5). */
object ItkLinear:
  def matrixOffset(entry: ItkEntry): Either[TransformIoError, ItkMatrixOffset] =
    val p = entry.parameters
    val f = entry.fixedParameters
    def need(n: Int): Either[TransformIoError, Unit] =
      if p.size == n then Right(()) else Left(malformed(entry, s"expected $n parameters, got ${p.size}"))
    def center: Either[TransformIoError, Vector[Double]] =
      if f.isEmpty then Right(Vector(0.0, 0.0, 0.0))
      else if f.size >= 3 then Right(f.take(3))
      else Left(malformed(entry, s"expected a 3-vector centre in the fixed parameters, got ${f.size} values"))
    val finite =
      if (p ++ f).forall(_.isFinite) then Right(()) else Left(malformed(entry, "parameters must be finite"))
    finite.flatMap: _ =>
      entry.kind match
        case "AffineTransform" | "MatrixOffsetTransformBase" | "Rigid3DTransform" =>
          for
            _ <- need(12)
            c <- center
          yield centred(p.take(9), p.drop(9), c)
        case "Euler3DTransform" =>
          for
            _ <- need(6)
            c <- center
          yield
            val computeZyx = f.size >= 4 && f(3) != 0.0
            centred(euler(p(0), p(1), p(2), computeZyx), p.drop(3), c)
        case "VersorRigid3DTransform" =>
          for
            _ <- need(6)
            c <- center
            r <- versor(entry, p(0), p(1), p(2))
          yield centred(r, p.slice(3, 6), c)
        case "Similarity3DTransform" =>
          for
            _ <- need(7)
            c <- center
            r <- versor(entry, p(0), p(1), p(2))
          yield centred(r.map(_ * p(6)), p.slice(3, 6), c)
        case "ScaleSkewVersor3DTransform" =>
          for
            _ <- need(15)
            c <- center
            r <- versor(entry, p(0), p(1), p(2))
          yield
            // itk::ScaleSkewVersor3DTransform::ComputeMatrix (ITK 5.4) adds scale and skew to the rotation:
            // M = R + diag(scale - 1) + skew, skew ordered {xy, xz, yx, yz, zx, zy}.
            val (s, k) = (p.slice(6, 9), p.slice(9, 15))
            val additive = Vector(s(0) - 1.0, k(0), k(1), k(2), s(1) - 1.0, k(3), k(4), k(5), s(2) - 1.0)
            centred(r.zip(additive).map(_ + _), p.slice(3, 6), c)
        case "TranslationTransform" =>
          need(3).map(_ => ItkMatrixOffset(identity, p))
        case "IdentityTransform" =>
          Right(ItkMatrixOffset(identity, Vector(0.0, 0.0, 0.0)))
        case "BSplineTransform" | "BSplineDeformableTransform" =>
          Left(TransformIoError.Unsupported(UnsupportedFormat.ItkBSpline, s"${entry.typeName} is out of scope"))
        case _ =>
          Left(TransformIoError.UnsupportedItkTransform(entry.typeName))

  def isLinear(entry: ItkEntry): Boolean =
    Set(
      "AffineTransform", "MatrixOffsetTransformBase", "Rigid3DTransform", "Euler3DTransform", "VersorRigid3DTransform",
      "Similarity3DTransform", "ScaleSkewVersor3DTransform", "TranslationTransform", "IdentityTransform"
    ).contains(entry.kind)

  private val identity = Vector(1.0, 0, 0, 0, 1.0, 0, 0, 0, 1.0)

  /** ITK MatrixOffsetTransformBase: offset = translation + centre - matrix * centre. */
  private def centred(m: Vector[Double], translation: Vector[Double], c: Vector[Double]): ItkMatrixOffset =
    val offset = Vector.tabulate(3)(r => translation(r) + c(r) - (m(3 * r) * c(0) + m(3 * r + 1) * c(1) + m(3 * r + 2) * c(2)))
    ItkMatrixOffset(m, offset)

  /** itk::Euler3DTransform::ComputeMatrix: Rz*Rx*Ry by default, Rz*Ry*Rx when ComputeZYX. */
  private def euler(ax: Double, ay: Double, az: Double, computeZyx: Boolean): Vector[Double] =
    val (cx, sx, cy, sy, cz, sz) = (math.cos(ax), math.sin(ax), math.cos(ay), math.sin(ay), math.cos(az), math.sin(az))
    val rx = Vector(1.0, 0, 0, 0, cx, -sx, 0, sx, cx)
    val ry = Vector(cy, 0, sy, 0, 1.0, 0, -sy, 0, cy)
    val rz = Vector(cz, -sz, 0, sz, cz, 0, 0, 0, 1.0)
    if computeZyx then multiply(multiply(rz, ry), rx) else multiply(multiply(rz, rx), ry)

  /** itk::Versor: the parameters are the vector part of a unit quaternion; the scalar part is sqrt(1 - |v|^2). */
  private def versor(entry: ItkEntry, x: Double, y: Double, z: Double): Either[TransformIoError, Vector[Double]] =
    val norm2 = x * x + y * y + z * z
    if norm2 > 1.0 + 1e-12 then Left(malformed(entry, s"versor norm ${math.sqrt(norm2)} exceeds 1"))
    else
      val w = math.sqrt(math.max(0.0, 1.0 - norm2))
      Right(
        Vector(
          1 - 2 * (y * y + z * z), 2 * (x * y - z * w), 2 * (x * z + y * w),
          2 * (x * y + z * w), 1 - 2 * (x * x + z * z), 2 * (y * z - x * w),
          2 * (x * z - y * w), 2 * (y * z + x * w), 1 - 2 * (x * x + y * y)
        )
      )

  private def multiply(a: Vector[Double], b: Vector[Double]): Vector[Double] =
    Vector.tabulate(9)(i => (0 until 3).map(k => a(3 * (i / 3) + k) * b(3 * k + i % 3)).sum)

  private def malformed(entry: ItkEntry, reason: String): TransformIoError =
    TransformIoError.Malformed(entry.typeName, reason)
