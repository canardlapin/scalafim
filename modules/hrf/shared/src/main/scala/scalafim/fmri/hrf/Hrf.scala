package scalafim.fmri.hrf

/** A hemodynamic response function: a causal kernel on temporal displacement.
  *
  * The argument is a [[Lag]] — the displacement from an event onset — and not a
  * point on a run's time line. The two are separate types precisely so that
  * handing a kernel an absolute onset does not compile.
  *
  * Two invariants hold for every kernel and are enforced here rather than
  * trusted to each of the ~17 constructors:
  *
  *   - '''causality''': `h(lag) = 0` for `lag < 0`;
  *   - '''support''': `h(lag) = 0` beyond a [[Support.Compact]] horizon.
  *
  * Implementors provide [[evaluateInSupport]], which is called only with a lag
  * that already satisfies both. A kernel therefore cannot accidentally respond
  * before its event.
  */
trait Hrf:
  def name: String
  def nbasis: Int
  def span: Seconds

  /** Where this kernel may be non-zero. Defaults to the conservative choice. */
  def support: Support = Support.Unbounded

  /** Symbolic provenance: family, parameters, derivative and penalty policy.
    *
    * This is the typed record of *what kernel this is*, and it is what `Deriv`
    * and `Penalty` dispatch on. Parameters live in the [[HrfParams]] ADT; there
    * is deliberately no untyped side channel.
    */
  def descriptor: HrfDescriptor =
    HrfDescriptor.custom(name, nbasis, span)
  def basis: BasisCount =
    descriptor.basis

  /** Structural coordinates for this response space.
    *
    * The validated form is available to trust-boundary code. The total form
    * keeps ordinary kernel use ergonomic while still failing immediately if a
    * custom descriptor cannot account for its declared cardinality.
    */
  def basisElementsValidated: Either[BasisIdentityError, Vector[BasisElement]] =
    BasisElement.infer(this)

  def basisElements: Vector[BasisElement] =
    basisElementsValidated.fold(error => throw new IllegalArgumentException(error.message), values => values)

  /** Evaluate at a lag already known to be causal and within support. */
  protected def evaluateInSupport(lag: Lag): scalafim.fmri.hrf.linalg.Vec

  final def apply(lag: Lag): scalafim.fmri.hrf.linalg.Vec =
    if !lag.isCausal || !support.containsNonNegative(lag) then
      scalafim.fmri.hrf.linalg.Vec.zeros(nbasis)
    else evaluateInSupport(lag)

  /** Alias for [[apply]]. */
  final def at(lag: Lag): scalafim.fmri.hrf.linalg.Vec = apply(lag)

  final def eval(grid: IterableOnce[Lag]): scalafim.fmri.hrf.linalg.Mat =
    val it = grid.iterator
    val buf = Vector.newBuilder[scalafim.fmri.hrf.linalg.Vec]
    while it.hasNext do buf += apply(it.next())
    val rows = buf.result()
    if rows.isEmpty then scalafim.fmri.hrf.linalg.Mat.zeros(0, nbasis)
    else
      val out = new Array[Double](rows.size * nbasis)
      var r = 0
      while r < rows.size do
        val v = rows(r).data
        require(v.length == nbasis, s"HRF '$name' returned ${v.length} basis values, expected $nbasis")
        System.arraycopy(v, 0, out, r * nbasis, nbasis)
        r += 1
      scalafim.fmri.hrf.linalg.Mat.unsafe(rows.size, nbasis, out)

  /** Sample on a lag axis given as plain doubles. */
  final def evalDoubles(grid: IterableOnce[Double]): scalafim.fmri.hrf.linalg.Mat =
    eval(grid.iterator.map(Lag(_)))

  final def evalScalar(grid: IterableOnce[Lag]): Array[Double] =
    require(descriptor.isScalar, s"evalScalar only valid for nbasis=1, got $nbasis")
    val it = grid.iterator
    val out = new scala.collection.mutable.ArrayBuffer[Double]()
    while it.hasNext do out += apply(it.next()).data(0)
    out.toArray

trait ScalarHrf extends Hrf:
  final def nbasis: Int = 1

  /** Evaluate at a lag already known to be causal and within support. */
  def scalarAt(lag: Lag): Double

  final protected def evaluateInSupport(lag: Lag): scalafim.fmri.hrf.linalg.Vec =
    scalafim.fmri.hrf.linalg.Vec.unsafe(Array(scalarAt(lag)))

object ScalarHrf:
  def from(hrf: Hrf): Either[HrfSpecError, ScalarHrf] =
    if hrf.nbasis != 1 then Left(HrfSpecError.ExpectedScalar(hrf.name, hrf.nbasis))
    else
      hrf match
        case scalar: ScalarHrf => Right(scalar)
        case other =>
          Right(
            new ScalarHrf:
              def name: String = other.name
              def span: Seconds = other.span
              override def support: Support = other.support
              override def descriptor: HrfDescriptor = other.descriptor
              def scalarAt(lag: Lag): Double = other(lag).data(0)
          )

object Hrf:
  /** Attach caller-supplied identities to a custom kernel after validating
    * cardinality, ordering, ids, and labels.  Inferred identities remain the
    * default for built-ins; custom bases use this boundary when generated
    * names are not sufficient scientific provenance.
    */
  def withBasisElements(
      hrf: Hrf,
      elements: Vector[BasisElement]
  ): Either[BasisIdentityError, Hrf] =
    BasisElement.validate(hrf.name, hrf.nbasis, elements).map { _ =>
      new Hrf:
        def name: String = hrf.name
        def nbasis: Int = hrf.nbasis
        def span: Seconds = hrf.span
        override def support: Support = hrf.support
        override def descriptor: HrfDescriptor = hrf.descriptor
        override def basisElementsValidated: Either[BasisIdentityError, Vector[BasisElement]] = Right(elements)
        protected def evaluateInSupport(lag: Lag): scalafim.fmri.hrf.linalg.Vec = hrf(lag)
    }

  /** Convenience constructor for explicitly identified multi-basis kernels. */
  def multiWithBasisElements(
      name: String,
      elements: Vector[BasisElement],
      span: Seconds = Seconds(24.0),
      descriptor: Option[HrfDescriptor] = None,
      support: Support = Support.Unbounded
  )(f: Lag => Array[Double]): Either[BasisIdentityError, Hrf] =
    if elements.isEmpty then Left(BasisIdentityError.CardinalityMismatch(name, 1, 0))
    else withBasisElements(multi(name, elements.length, span, descriptor, support)(f), elements)

  def of(
      name: String,
      nbasis: Int = 1,
      span: Seconds = Seconds(24.0),
      descriptor: Option[HrfDescriptor] = None,
      support: Support = Support.Unbounded
  )(f: Lag => scalafim.fmri.hrf.linalg.Vec): Hrf =
    val basis0 = BasisCount(nbasis)
    val name0 = name
    val span0 = span
    val support0 = support
    val descriptor0 = descriptor.getOrElse(HrfDescriptor.custom(name0, basis0.value, span0))
    require(descriptor0.nbasis == basis0.value, s"descriptor basis ${descriptor0.nbasis} != nbasis ${basis0.value}")
    new Hrf:
      def name: String = name0
      def nbasis: Int = basis0.value
      def span: Seconds = span0
      override def support: Support = support0
      override def descriptor: HrfDescriptor = descriptor0
      protected def evaluateInSupport(lag: Lag): scalafim.fmri.hrf.linalg.Vec = f(lag)

  def scalar(
      name: String,
      span: Seconds = Seconds(24.0),
      descriptor: Option[HrfDescriptor] = None,
      support: Support = Support.Unbounded
  )(f: Lag => Double): ScalarHrf =
    val name0 = name
    val span0 = span
    val support0 = support
    val descriptor0 = descriptor.getOrElse(HrfDescriptor.custom(name0, 1, span0))
    require(descriptor0.isScalar, s"scalar HRF descriptor must have nbasis=1, got ${descriptor0.nbasis}")
    new ScalarHrf:
      def name: String = name0
      def span: Seconds = span0
      override def support: Support = support0
      override def descriptor: HrfDescriptor = descriptor0
      def scalarAt(lag: Lag): Double = f(lag)

  def multi(
      name: String,
      nbasis: Int,
      span: Seconds = Seconds(24.0),
      descriptor: Option[HrfDescriptor] = None,
      support: Support = Support.Unbounded
  )(f: Lag => Array[Double]): Hrf =
    of(name, nbasis = nbasis, span = span, descriptor = descriptor, support = support)(t =>
      scalafim.fmri.hrf.linalg.Vec.unsafe(f(t))
    )
