package scalafim.fmri.hrf

import scalafim.fmri.hrf.linalg.Mat

enum BasisError:
  case DimensionMismatch(basis: String, expected: Int, actual: Int)
  case ForeignCoefficients(expected: String, actual: String)
  case NotInvertible(detail: String)
  case NonFiniteValue(label: String, index: Int, value: Double)
  case BasisIdentityFailure(detail: String)
  case UnknownRole(role: String)
  case InvalidFunctional(detail: String)
  case InvalidDiscretization(detail: String)

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
      case BasisIdentityFailure(detail) =>
        s"invalid basis identity: $detail"
      case UnknownRole(role) =>
        s"basis does not contain an element with role '$role'"
      case InvalidFunctional(detail) =>
        s"invalid response functional: $detail"
      case InvalidDiscretization(detail) =>
        s"invalid response-functional discretization: $detail"

/** Errors raised while deriving stable identities for basis elements. */
enum BasisIdentityError:
  case InvalidId(value: String)
  case InvalidIndex(index: Int)
  case CardinalityMismatch(basis: String, expected: Int, actual: Int)
  case DuplicateId(value: String)
  case DuplicateLabel(value: String)
  case NonContiguousIndices(basis: String)

  def message: String =
    this match
      case InvalidId(value) =>
        s"basis element id must be non-empty and contain no control characters, got '$value'"
      case InvalidIndex(index) =>
        s"basis element index must be >= 1, got $index"
      case CardinalityMismatch(basis, expected, actual) =>
        s"basis '$basis' inferred $actual semantic elements, expected $expected"
      case DuplicateId(value) =>
        s"basis element id '$value' is repeated"
      case DuplicateLabel(value) =>
        s"basis element label '$value' is repeated"
      case NonContiguousIndices(basis) =>
        s"basis '$basis' element indices must be exactly 1..nbasis"

/** Semantic identity of one coordinate in a response basis.
  *
  * The role is deliberately richer than a rendered column label. In
  * particular, FIR bins retain their temporal interval and derivative columns
  * remain distinguishable even if a caller later changes display names.
  */
enum BasisRole:
  case Canonical
  case TemporalDerivative
  case DispersionDerivative
  case FirBin(index: Int, from: Seconds, until: Seconds)
  case Spline(index: Int)
  case Tent(index: Int)
  case Fourier(index: Int)
  case Sine(index: Int)
  case Daguerre(index: Int)
  case Custom(index: Int, label: String)
  case Generic(index: Int)

  def stableLabel: String =
    this match
      case Canonical => "canonical"
      case TemporalDerivative => "temporal-derivative"
      case DispersionDerivative => "dispersion-derivative"
      case FirBin(index, from, until) =>
        s"fir-bin-${index}%02d-${from.value}..${until.value}"
      case Spline(index) => s"spline-${index}%02d"
      case Tent(index) => s"tent-${index}%02d"
      case Fourier(index) => s"fourier-${index}%02d"
      case Sine(index) => s"sine-${index}%02d"
      case Daguerre(index) => s"daguerre-${index}%02d"
      case Custom(index, label) => s"custom-${index}%02d-$label"
      case Generic(index) => s"basis-${index}%02d"

  def oneBasedIndex: Int =
    this match
      case Canonical => 1
      case TemporalDerivative => 2
      case DispersionDerivative => 3
      case FirBin(index, _, _) => index
      case Spline(index) => index
      case Tent(index) => index
      case Fourier(index) => index
      case Sine(index) => index
      case Daguerre(index) => index
      case Custom(index, _) => index
      case Generic(index) => index

opaque type BasisElementId = String

object BasisElementId:
  def from(value: String): Either[BasisIdentityError, BasisElementId] =
    if value.trim.isEmpty || value.exists(_.isControl) then Left(BasisIdentityError.InvalidId(value))
    else Right(value)

  def apply(value: String): BasisElementId =
    from(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  inline def unsafe(value: String): BasisElementId = value

  extension (id: BasisElementId)
    inline def value: String = id

/** A basis coordinate with stable semantic metadata. */
final case class BasisElement(
    id: BasisElementId,
    index: Int,
    role: BasisRole,
    label: String
):
  require(index >= 1, s"basis element index must be >= 1, got $index")
  require(label.trim.nonEmpty, "basis element label must be non-empty")

object BasisElement:
  /** Derive semantic coordinates from the descriptor, never from display labels. */
  def infer(hrf: Hrf): Either[BasisIdentityError, Vector[BasisElement]] =
    val descriptor = hrf.descriptor
    val roles = rolesFor(descriptor, hrf.nbasis)
    if roles.length != hrf.nbasis then
      Left(BasisIdentityError.CardinalityMismatch(hrf.name, hrf.nbasis, roles.length))
    else
      val labels =
        if hrf.nbasis == 1 then Vector(hrf.name)
        else Vector.tabulate(hrf.nbasis)(i => f"${hrf.name}_b${i + 1}%02d")
      Right(
        roles.zipWithIndex.map { case (role, offset) =>
          val index = offset + 1
          val id = BasisElementId.unsafe(
            s"${descriptor.canonicalId}|${role.stableLabel}|$index"
          )
          BasisElement(id, index, role, labels(index - 1))
        }
      )

  /** Validate caller-supplied identities at the custom-basis boundary. */
  def validate(
      basis: String,
      expected: Int,
      elements: Vector[BasisElement]
  ): Either[BasisIdentityError, Unit] =
    if elements.length != expected then
      Left(BasisIdentityError.CardinalityMismatch(basis, expected, elements.length))
    else if elements.exists(element => element.id.value.trim.isEmpty || element.id.value.exists(_.isControl)) then
      Left(BasisIdentityError.InvalidId(elements.find(element => element.id.value.trim.isEmpty || element.id.value.exists(_.isControl)).get.id.value))
    else if elements.map(_.id.value).distinct.length != elements.length then
      Left(BasisIdentityError.DuplicateId(elements.groupBy(_.id.value).collectFirst { case (id, values) if values.length > 1 => id }.get))
    else if elements.map(_.label).distinct.length != elements.length then
      Left(BasisIdentityError.DuplicateLabel(elements.groupBy(_.label).collectFirst { case (label, values) if values.length > 1 => label }.get))
    else if elements.map(_.index) != (1 to expected).toVector then
      Left(BasisIdentityError.NonContiguousIndices(basis))
    else Right(())

  private def rolesFor(descriptor: HrfDescriptor, nBasis: Int): Vector[BasisRole] =
    descriptor.integration match
      case IntegrationPolicy.SpmgTemporalDeriv(_) if nBasis == 1 =>
        Vector(BasisRole.TemporalDerivative)
      case IntegrationPolicy.SpmgDispersionDeriv(_) if nBasis == 1 =>
        Vector(BasisRole.DispersionDerivative)
      case _ =>
        descriptor.family match
          case HrfFamily.Known(HrfKind.Spmg1) if nBasis == 1 =>
            Vector(BasisRole.Canonical)
          case HrfFamily.Known(HrfKind.Spmg2) if nBasis == 2 =>
            Vector(BasisRole.Canonical, BasisRole.TemporalDerivative)
          case HrfFamily.Known(HrfKind.Spmg3) if nBasis == 3 =>
            Vector(BasisRole.Canonical, BasisRole.TemporalDerivative, BasisRole.DispersionDerivative)
          case HrfFamily.Known(HrfKind.Fir) =>
            firRoles(descriptor, nBasis)
          case HrfFamily.Known(HrfKind.Bspline) =>
            Vector.tabulate(nBasis)(i => BasisRole.Spline(i + 1))
          case HrfFamily.Known(HrfKind.Tent) =>
            Vector.tabulate(nBasis)(i => BasisRole.Tent(i + 1))
          case HrfFamily.Known(HrfKind.Fourier) =>
            Vector.tabulate(nBasis)(i => BasisRole.Fourier(i + 1))
          case HrfFamily.Known(HrfKind.Sine) =>
            Vector.tabulate(nBasis)(i => BasisRole.Sine(i + 1))
          case HrfFamily.Known(HrfKind.Daguerre) =>
            Vector.tabulate(nBasis)(i => BasisRole.Daguerre(i + 1))
          case HrfFamily.Composite(_) =>
            componentRoles(descriptor.components, nBasis)
          case HrfFamily.Derived(_) | HrfFamily.Custom(_) | HrfFamily.Known(_) =>
            descriptor.params match
              case HrfParams.Coefficients(_, _) if nBasis == 1 => Vector(BasisRole.Custom(1, "coefficient"))
              case HrfParams.Empirical(_) if nBasis == 1 => Vector(BasisRole.Custom(1, "empirical"))
              case _ => Vector.tabulate(nBasis)(i => BasisRole.Generic(i + 1))

  private def firRoles(descriptor: HrfDescriptor, nBasis: Int): Vector[BasisRole] =
    val breaks = descriptor.integration match
      case IntegrationPolicy.PiecewisePolynomial(values, _) if values.length == nBasis + 1 => values
      case _ =>
        Vector.tabulate(nBasis + 1)(i => Seconds(i.toDouble * descriptor.span.value / nBasis.toDouble))
    Vector.tabulate(nBasis) { i =>
      BasisRole.FirBin(i + 1, breaks(i), breaks(i + 1))
    }

  private def componentRoles(components: Vector[HrfDescriptor], nBasis: Int): Vector[BasisRole] =
    val flattened = components.flatMap { component =>
      rolesFor(component, component.nbasis)
    }
    if flattened.length == nBasis then flattened else Vector.tabulate(nBasis)(i => BasisRole.Generic(i + 1))

enum ResponseFunctional:
  case At(lag: Seconds)
  case WindowMean(from: Seconds, until: Seconds)
  case WindowIntegral(from: Seconds, until: Seconds)

/** Units make it impossible to confuse a point response with an integrated response. */
enum ResponseUnits:
  case ResponseValue
  case ResponseIntegral

enum FunctionalDiscretization:
  case Exact
  case Trapezoid(maxStep: PositiveSeconds)

final case class FunctionalDiscretizationReceipt(
    policy: FunctionalDiscretization,
    samples: Int,
    effectiveStep: Option[Seconds]
)

/** Linear weights over one basis for a response-level functional. */
final case class ResponseFunctionalWeights[S](
    functional: ResponseFunctional,
    weights: BasisCoefficients[S],
    units: ResponseUnits,
    receipt: FunctionalDiscretizationReceipt
):
  def values: Vector[Double] = weights.values

/** Nonlinear summaries intentionally have no conversion to coefficient weights. */
enum NonlinearResponseSummary:
  case PeakLatency(from: Seconds, until: Seconds)

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

  /** Structural basis elements, with a typed failure for malformed custom HRFs. */
  final def elementsValidated: Either[BasisIdentityError, Vector[BasisElement]] =
    kernel.basisElementsValidated

  final def elements: Vector[BasisElement] =
    kernel.basisElements

  /** Resolve a semantic role without consulting a rendered label or index. */
  final def element(role: BasisRole): Either[BasisError, BasisElement] =
    elementsValidated
      .left
      .map(error => BasisError.BasisIdentityFailure(error.message))
      .flatMap(_.find(_.role == role).toRight(BasisError.UnknownRole(role.stableLabel)))

  /** Column labels, following the module's `_b01`-style convention. */
  def labels: Vector[String] =
    if dimension == 1 then Vector(name)
    else Vector.tabulate(dimension)(i => f"${name}_b${i + 1}%02d")

  /** Compile a point or window functional into weights over this basis.
    *
    * `Exact` windows use the primitive registered by the HRF descriptor. A
    * basis without such a primitive must choose an explicit trapezoid policy;
    * there is no hidden default sampling grid.
    */
  def responseFunctional(functional: ResponseFunctional): Either[BasisError, ResponseFunctionalWeights[Space]] =
    responseFunctional(functional, FunctionalDiscretization.Exact)

  def responseFunctional(
      functional: ResponseFunctional,
      discretization: FunctionalDiscretization
  ): Either[BasisError, ResponseFunctionalWeights[Space]] =
    functional match
      case ResponseFunctional.At(lag) =>
        if !lag.value.isFinite || lag.value < 0.0 then
          Left(BasisError.InvalidFunctional(s"point lag must be finite and >= 0, got ${lag.value}"))
        else
          val values = kernel(Lag.ofSeconds(lag)).data
          finiteWeights(values).map { vector =>
            ResponseFunctionalWeights(
              functional,
              BasisCoefficients.unsafe(vector),
              ResponseUnits.ResponseValue,
              FunctionalDiscretizationReceipt(discretization, samples = 1, effectiveStep = None)
            )
          }

      case ResponseFunctional.WindowMean(from, until) =>
        windowWeights(from, until, discretization, divideByWidth = true)

      case ResponseFunctional.WindowIntegral(from, until) =>
        windowWeights(from, until, discretization, divideByWidth = false)

  private def windowWeights(
      from: Seconds,
      until: Seconds,
      discretization: FunctionalDiscretization,
      divideByWidth: Boolean
  ): Either[BasisError, ResponseFunctionalWeights[Space]] =
    val width = until.value - from.value
    if !from.value.isFinite || !until.value.isFinite || from.value < 0.0 || width <= 0.0 then
      Left(BasisError.InvalidFunctional(s"window must satisfy 0 <= from < until, got [${from.value}, ${until.value})"))
    else
      discretization match
        case FunctionalDiscretization.Exact =>
          Primitive.definiteIntegral(kernel, Lag(from.value), Lag(until.value)) match
            case None =>
              Left(BasisError.InvalidDiscretization("exact integration is not registered for this basis"))
            case Some(integral) =>
              finiteWeights(integral).map { values =>
                val scaled = if divideByWidth then values.map(_ / width) else values
                ResponseFunctionalWeights(
                  if divideByWidth then ResponseFunctional.WindowMean(from, until)
                  else ResponseFunctional.WindowIntegral(from, until),
                  BasisCoefficients.unsafe(scaled),
                  if divideByWidth then ResponseUnits.ResponseValue else ResponseUnits.ResponseIntegral,
                  FunctionalDiscretizationReceipt(FunctionalDiscretization.Exact, samples = 0, effectiveStep = None)
                )
              }

        case policy @ FunctionalDiscretization.Trapezoid(maxStep) =>
          if !maxStep.value.isFinite || maxStep.value <= 0.0 then
            Left(BasisError.InvalidDiscretization(s"trapezoid maxStep must be finite and > 0, got ${maxStep.value}"))
          else
            val intervals = math.max(1, math.ceil(width / maxStep.value).toInt)
            val dt = width / intervals.toDouble
            val integral = Array.fill(dimension)(0.0)
            var i = 0
            while i < intervals do
              val left = Lag(from.value + i.toDouble * dt)
              val right = Lag(from.value + (i + 1).toDouble * dt)
              val y0 = kernel(left).data
              val y1 = kernel(right).data
              var j = 0
              while j < dimension do
                integral(j) += 0.5 * dt * (y0(j) + y1(j))
                j += 1
              i += 1
            val scaled = if divideByWidth then integral.map(_ / width).toVector else integral.toVector
            finiteWeights(scaled).map { values =>
              ResponseFunctionalWeights(
                if divideByWidth then ResponseFunctional.WindowMean(from, until)
                else ResponseFunctional.WindowIntegral(from, until),
                BasisCoefficients.unsafe(values),
                if divideByWidth then ResponseUnits.ResponseValue else ResponseUnits.ResponseIntegral,
                FunctionalDiscretizationReceipt(policy, samples = intervals + 1, effectiveStep = Some(Seconds(dt)))
              )
            }

  private def finiteWeights(values: Array[Double]): Either[BasisError, Vector[Double]] =
    finiteWeights(values.toVector)

  private def finiteWeights(values: Vector[Double]): Either[BasisError, Vector[Double]] =
    var i = 0
    while i < values.length do
      if !values(i).isFinite then return Left(BasisError.NonFiniteValue("response functional", i, values(i)))
      i += 1
    if values.length != dimension then Left(BasisError.DimensionMismatch(name, dimension, values.length))
    else Right(values)

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
  * This module currently produces diagonal transforms (from `normalize`) and
  * explicit permutations. Both keep the coordinate change visible to callers;
  * a future dense linear transform can extend this same contract.
  */
enum BasisTransform:
  /** Independent per-column rescaling: `Phi'_j = Phi_j / scales_j`. */
  case Diagonal(scales: Vector[Double])

  /** Identity — coordinates unchanged. */
  case Identity(columns: Int)

  /** Reorder basis values by selecting old coordinate `order(newIndex)`. */
  case Permutation(order: Vector[Int])

  def dimension: Int =
    this match
      case Diagonal(scales) => scales.length
      case Identity(d)      => d
      case Permutation(order) => order.length

  /** Whether this transform preserves orthogonality, and so leaves a diagonal
    * penalty meaningful up to rescaling.
    */
  def isDiagonal: Boolean =
    this match
      case Diagonal(_) | Identity(_) => true
      case Permutation(_)            => false

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
      case Permutation(order) =>
        validatePermutation(order).flatMap { _ =>
          if coefficients.dimension != order.length then
            Left(BasisError.DimensionMismatch("transform", order.length, coefficients.dimension))
          else Right(BasisCoefficients.unsafe(order.map(i => coefficients.values(i))))
        }

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
      case Permutation(order) =>
        validatePermutation(order).flatMap:
          _ =>
            val n = order.length
            if penalty.rows != n || penalty.cols != n then
              Left(BasisError.DimensionMismatch("penalty", n, penalty.rows))
            else
              val out = new Array[Double](n * n)
              var r = 0
              while r < n do
                var c = 0
                while c < n do
                  out(r * n + c) = penalty(order(r), order(c))
                  c += 1
                r += 1
              Right(Mat.unsafe(n, n, out))

  /** Transport a covariance matrix for coefficient estimates into the new
    * coordinates.  The covariance follows the coefficient map, so a
    * diagonal rescaling uses `S Q S` and a permutation reorders both axes.
    * Keeping this beside coefficient and penalty transport prevents callers
    * from accidentally applying the value-space map to inferential geometry.
    */
  def transportCovariance(covariance: Mat): Either[BasisError, Mat] =
    validateSquare(covariance, "covariance").flatMap { _ =>
      this match
        case Identity(_) => Right(covariance)
        case Diagonal(scales) =>
          val n = scales.length
          if covariance.rows != n then
            Left(BasisError.DimensionMismatch("covariance", n, covariance.rows))
          else
            val out = new Array[Double](n * n)
            var r = 0
            while r < n do
              var c = 0
              while c < n do
                out(r * n + c) = scales(r) * covariance(r, c) * scales(c)
                c += 1
              r += 1
            Right(Mat.unsafe(n, n, out))
        case Permutation(order) =>
          validatePermutation(order).flatMap { _ =>
            val n = order.length
            if covariance.rows != n then
              Left(BasisError.DimensionMismatch("covariance", n, covariance.rows))
            else
              val out = new Array[Double](n * n)
              var r = 0
              while r < n do
                var c = 0
                while c < n do
                  out(r * n + c) = covariance(order(r), order(c))
                  c += 1
                r += 1
              Right(Mat.unsafe(n, n, out))
          }
    }

  /** Transport a coefficient-space covector, such as a contrast or a
    * response-functional weight, into the transformed coordinates. */
  def transportLinearWeights(weights: Vector[Double]): Either[BasisError, Vector[Double]] =
    this match
      case Identity(columns) =>
        if weights.length != columns then Left(BasisError.DimensionMismatch("transform", columns, weights.length))
        else Right(weights)
      case Diagonal(scales) =>
        if weights.length != scales.length then
          Left(BasisError.DimensionMismatch("transform", scales.length, weights.length))
        else if scales.exists(s => !s.isFinite || math.abs(s) <= 1e-12) then
          Left(BasisError.NotInvertible("linear weights cannot be transported through a non-finite or zero scale"))
        else Right(weights.zip(scales).map { case (weight, scale) => weight / scale })
      case Permutation(order) =>
        validatePermutation(order).flatMap { _ =>
          if weights.length != order.length then
            Left(BasisError.DimensionMismatch("transform", order.length, weights.length))
          else Right(order.map(weights(_)))
        }

  def inverse: Either[BasisError, BasisTransform] =
    this match
      case Identity(d) => Right(Identity(d))
      case Diagonal(scales) =>
        scales.zipWithIndex.find { case (s, _) => math.abs(s) <= 1e-12 } match
          case Some((s, i)) => Left(BasisError.NotInvertible(s"scale $s at column ${i + 1}"))
          case None         => Right(Diagonal(scales.map(1.0 / _)))
      case Permutation(order) =>
        validatePermutation(order).map:
          _ =>
            val inverse = Array.fill(order.length)(0)
            var i = 0
            while i < order.length do
              inverse(order(i)) = i
              i += 1
            Permutation(inverse.toVector)

  private def validatePermutation(order: Vector[Int]): Either[BasisError, Unit] =
    if order.isEmpty || order.sorted != (0 until order.length).toVector then
      Left(BasisError.NotInvertible(s"order must be a permutation of 0 until ${order.length}"))
    else Right(())

  private def validateSquare(matrix: Mat, label: String): Either[BasisError, Unit] =
    if matrix.rows <= 0 || matrix.rows != matrix.cols || matrix.rows != dimension then
      Left(BasisError.DimensionMismatch(label, dimension, matrix.rows))
    else if matrix.data.exists(value => !value.isFinite) then
      Left(BasisError.NonFiniteValue(label, matrix.data.indexWhere(value => !value.isFinite), matrix.data.find(value => !value.isFinite).get))
    else Right(())

/** A basis paired with the transform that produced it.
  *
  * Returned by gauge-changing operations so the caller can transport whatever
  * they already hold — coefficients, contrasts, penalties — instead of
  * discovering later that their units moved.
  */
final case class TransformedBasis(basis: Hrf, transform: BasisTransform):
  def transportPenalty(penalty: Mat): Either[BasisError, Mat] =
    transform.transportPenalty(penalty)

  def transportCovariance(covariance: Mat): Either[BasisError, Mat] =
    transform.transportCovariance(covariance)

  def transportLinearWeights(weights: Vector[Double]): Either[BasisError, Vector[Double]] =
    transform.transportLinearWeights(weights)
