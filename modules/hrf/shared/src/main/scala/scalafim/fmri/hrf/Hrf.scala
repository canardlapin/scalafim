package scalafim.fmri.hrf

trait Hrf extends (Seconds => scalafim.fmri.hrf.linalg.Vec):
  def name: String
  def nbasis: Int
  def span: Seconds
  def params: Map[String, Any] = Map.empty
  def descriptor: HrfDescriptor =
    HrfDescriptor.custom(name, nbasis, span, HrfParams.Legacy(params))
  def basis: BasisCount =
    descriptor.basis

  protected def eval1(t: Seconds): scalafim.fmri.hrf.linalg.Vec

  final def apply(t: Seconds): scalafim.fmri.hrf.linalg.Vec = eval1(t)

  final def eval(grid: IterableOnce[Seconds]): scalafim.fmri.hrf.linalg.Mat =
    val it = grid.iterator
    val buf = Vector.newBuilder[scalafim.fmri.hrf.linalg.Vec]
    while it.hasNext do buf += eval1(it.next())
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

  final def evalDoubles(grid: IterableOnce[Double]): scalafim.fmri.hrf.linalg.Mat =
    eval(grid.iterator.map(Seconds(_)))

  final def evalScalar(grid: IterableOnce[Seconds]): Array[Double] =
    require(descriptor.isScalar, s"evalScalar only valid for nbasis=1, got $nbasis")
    val it = grid.iterator
    val out = new scala.collection.mutable.ArrayBuffer[Double]()
    while it.hasNext do out += eval1(it.next()).data(0)
    out.toArray

trait ScalarHrf extends Hrf:
  final def nbasis: Int = 1

  def scalarAt(t: Seconds): Double

  final protected def eval1(t: Seconds): scalafim.fmri.hrf.linalg.Vec =
    scalafim.fmri.hrf.linalg.Vec.unsafe(Array(scalarAt(t)))

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
              override def params: Map[String, Any] = other.params
              override def descriptor: HrfDescriptor = other.descriptor
              def scalarAt(t: Seconds): Double = other(t).data(0)
          )

object Hrf:
  def of(
      name: String,
      nbasis: Int = 1,
      span: Seconds = Seconds(24.0),
      params: Map[String, Any] = Map.empty,
      descriptor: Option[HrfDescriptor] = None
  )(f: Seconds => scalafim.fmri.hrf.linalg.Vec): Hrf =
    val basis0 = BasisCount(nbasis)
    val name0 = name
    val span0 = span
    val params0 = params
    val descriptor0 = descriptor.getOrElse(HrfDescriptor.custom(name0, basis0.value, span0, HrfParams.Legacy(params0)))
    require(descriptor0.nbasis == basis0.value, s"descriptor basis ${descriptor0.nbasis} != nbasis ${basis0.value}")
    new Hrf:
      def name: String = name0
      def nbasis: Int = basis0.value
      def span: Seconds = span0
      override def params: Map[String, Any] =
        val legacy = descriptor0.legacyParams
        if legacy.nonEmpty then legacy else params0
      override def descriptor: HrfDescriptor = descriptor0
      protected def eval1(t: Seconds): scalafim.fmri.hrf.linalg.Vec = f(t)

  def scalar(
      name: String,
      span: Seconds = Seconds(24.0),
      params: Map[String, Any] = Map.empty,
      descriptor: Option[HrfDescriptor] = None
  )(f: Seconds => Double): ScalarHrf =
    val name0 = name
    val span0 = span
    val params0 = params
    val descriptor0 = descriptor.getOrElse(HrfDescriptor.custom(name0, 1, span0, HrfParams.Legacy(params0)))
    require(descriptor0.isScalar, s"scalar HRF descriptor must have nbasis=1, got ${descriptor0.nbasis}")
    new ScalarHrf:
      def name: String = name0
      def span: Seconds = span0
      override def params: Map[String, Any] =
        val legacy = descriptor0.legacyParams
        if legacy.nonEmpty then legacy else params0
      override def descriptor: HrfDescriptor = descriptor0
      def scalarAt(t: Seconds): Double = f(t)

  def multi(
      name: String,
      nbasis: Int,
      span: Seconds = Seconds(24.0),
      params: Map[String, Any] = Map.empty,
      descriptor: Option[HrfDescriptor] = None
  )(f: Seconds => Array[Double]): Hrf =
    of(name, nbasis = nbasis, span = span, params = params, descriptor = descriptor)(t => scalafim.fmri.hrf.linalg.Vec.unsafe(f(t)))
