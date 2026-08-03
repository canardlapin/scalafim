package scalafim.fmri.hrf

import scalafim.fmri.hrf.linalg.{Mat, Vec}

enum BasisError:
  case DimensionMismatch(basis: String, expected: Int, actual: Int)
  case ForeignCoefficients(expected: String, actual: String)
  case NotInvertible(detail: String)
  case NonFiniteValue(label: String, index: Int, value: Double)

  def message: String =
    this match
      case DimensionMismatch(basis, expected, actual) =>
        s"basis '$basis' has $expected columns, got $actual coefficients"
      case ForeignCoefficients(expected, actual) =>
        s"coefficients belong to basis '$actual', not '$expected'"
      case NotInvertible(detail) =>
        s"basis transform is not invertible: $detail"
      case NonFiniteValue(label, index, value) =>
        s"`$label` value at index ${index + 1} must be finite, got $value"

/** A response basis: a kernel whose values span a finite response space.
  *
  * The point of wrapping an [[Hrf]] this way is the abstract `Space` member.
  * Because `Space` is path-dependent, coefficients obtained from one basis
  * cannot be silently applied to another — even when both happen to have the
  * same number of columns. `withCoefficients` on a bare `Hrf` only checks the
  * length, so a three-column SPMG3 coefficient vector applies cleanly to a
  * three-column B-spline basis and produces a meaningless kernel.
  *
  * {{{
  * val basis = ResponseBasis.of(Hrfs.bspline(nBasis = 6))
  * val beta: BasisCoefficients[basis.Space] = basis.coefficients(fitted).toOption.get
  * val fittedHrf: Hrf = basis.reconstruct(beta)
  * }}}
  */
trait ResponseBasis:
  /** Identifies this basis at the type level. Distinct per `val`. */
  type Space

  def kernel: Hrf

  final def name: String = kernel.name
  final def dimension: Int = kernel.nbasis
  final def span: Seconds = kernel.span
  final def support: Support = kernel.support

  /** Column labels, following the module's `_b01`-style convention. */
  def labels: Vector[String] =
    if dimension == 1 then Vector(name)
    else Vector.tabulate(dimension)(i => f"${name}_b${i + 1}%02d")

  /** Lift a raw coefficient vector into this basis's dual space. */
  def coefficients(values: Seq[Double]): Either[BasisError, BasisCoefficients[Space]] =
    if values.length != dimension then Left(BasisError.DimensionMismatch(name, dimension, values.length))
    else
      val xs = values.toVector
      var i = 0
      while i < xs.length do
        if !xs(i).isFinite then return Left(BasisError.NonFiniteValue("coefficients", i, xs(i)))
        i += 1
      Right(BasisCoefficients.unsafe(xs))

  /** Contract the basis with a covector: `h_beta(l) = sum_j beta_j phi_j(l)`.
    *
    * This is the operation that turns a fitted voxel- or parcel-wise HRF back
    * into an ordinary scalar kernel, and it commutes with rendering — so a
    * basis-valued design block can be convolved once and contracted per voxel
    * afterwards, rather than convolving per voxel.
    */
  def reconstruct(coefficients: BasisCoefficients[Space]): Hrf =
    HrfCombinators.withCoefficientsUnchecked(kernel, coefficients.toArray, None)

object ResponseBasis:
  /** Wrap a kernel as a basis with a fresh, distinct `Space`. */
  def of(hrf: Hrf): ResponseBasis { type Space = hrf.type } =
    new ResponseBasis:
      type Space = hrf.type
      def kernel: Hrf = hrf

/** Coefficients in the dual of a basis space.
  *
  * Basis *values* live in `H`; fitted coefficients live in `H*`. Both are
  * stored as vectors of doubles, and the Euclidean dot product hides the
  * difference — until a basis is rescaled or rotated, at which point values and
  * coefficients transform contragrediently. [[BasisTransform]] is what keeps
  * them consistent.
  */
opaque type BasisCoefficients[S] = Vector[Double]

object BasisCoefficients:
  private[hrf] def unsafe[S](values: Vector[Double]): BasisCoefficients[S] = values

  extension [S](c: BasisCoefficients[S])
    def values: Vector[Double] = c
    def toArray: Array[Double] = c.toArray
    def dimension: Int = c.length

    def scaled(factor: Double): BasisCoefficients[S] =
      c.map(_ * factor)

    def combine(other: BasisCoefficients[S])(f: (Double, Double) => Double): BasisCoefficients[S] =
      c.zip(other).map(f.tupled)

/** An invertible change of basis coordinates.
  *
  * Normalization, whitening, QR orthogonalization, rotation and rescaling all
  * change basis coordinates without changing the space the basis spans. In an
  * unpenalized GLM the invariant object is the spanned column space, not the
  * raw coefficients — so these operations are a gauge symmetry, and they must
  * never happen silently.
  *
  * Two things have to travel with the new basis:
  *
  *   - '''coefficients''', contragrediently: if `Phi' = A Phi` then
  *     `beta' = beta A^-1`, so that `beta'(Phi') = beta(Phi)`;
  *   - '''penalties''', because a quadratic form on coefficient space is not
  *     invariant. Applying the same diagonal ridge after a non-orthogonal
  *     change of basis defines a *different model*.
  *
  * This module currently only produces diagonal transforms (from `normalize`),
  * which is why [[Diagonal]] is the sole case; the interface is shaped so a
  * general `Mat` case can be added without disturbing callers.
  */
enum BasisTransform:
  /** Independent per-column rescaling: `Phi'_j = Phi_j / scales_j`. */
  case Diagonal(scales: Vector[Double])

  /** Identity — coordinates unchanged. */
  case Identity(columns: Int)

  def dimension: Int =
    this match
      case Diagonal(scales) => scales.length
      case Identity(d)      => d

  /** Whether this transform preserves orthogonality, and so leaves a diagonal
    * penalty meaningful up to rescaling.
    */
  def isDiagonal: Boolean = true

  /** Transport coefficients so the reconstructed kernel is unchanged.
    *
    * Values divide by `scales`, so coefficients multiply by them.
    */
  def transportCoefficients[S](coefficients: BasisCoefficients[S]): Either[BasisError, BasisCoefficients[S]] =
    this match
      case Identity(_) => Right(coefficients)
      case Diagonal(scales) =>
        if coefficients.dimension != scales.length then
          Left(BasisError.DimensionMismatch("transform", scales.length, coefficients.dimension))
        else Right(BasisCoefficients.unsafe(coefficients.values.zip(scales).map(_ * _)))

  /** Transport a quadratic penalty `Q` into the new coordinates.
    *
    * For values scaled by `1/s_j`, coefficients scale by `s_j`, so
    * `Q' = S Q S` with `S = diag(scales)`.
    */
  def transportPenalty(penalty: Mat): Either[BasisError, Mat] =
    this match
      case Identity(_) => Right(penalty)
      case Diagonal(scales) =>
        val n = scales.length
        if penalty.rows != n || penalty.cols != n then
          Left(BasisError.DimensionMismatch("penalty", n, penalty.rows))
        else
          val out = new Array[Double](n * n)
          var r = 0
          while r < n do
            var c = 0
            while c < n do
              out(r * n + c) = scales(r) * penalty(r, c) * scales(c)
              c += 1
            r += 1
          Right(Mat.unsafe(n, n, out))

  def inverse: Either[BasisError, BasisTransform] =
    this match
      case Identity(d) => Right(Identity(d))
      case Diagonal(scales) =>
        scales.zipWithIndex.find { case (s, _) => math.abs(s) <= 1e-12 } match
          case Some((s, i)) => Left(BasisError.NotInvertible(s"scale $s at column ${i + 1}"))
          case None         => Right(Diagonal(scales.map(1.0 / _)))

/** A basis paired with the transform that produced it.
  *
  * Returned by gauge-changing operations so the caller can transport whatever
  * they already hold — coefficients, contrasts, penalties — instead of
  * discovering later that their units moved.
  */
final case class TransformedBasis(basis: Hrf, transform: BasisTransform):
  def transportPenalty(penalty: Mat): Either[BasisError, Mat] =
    transform.transportPenalty(penalty)
