package scalafim.fmri.hrf

trait Hrf extends (Seconds => scalafim.fmri.hrf.linalg.Vec):
  def name: String
  def nbasis: Int
  def span: Seconds
  def params: Map[String, Any] = Map.empty

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
    require(nbasis == 1, s"evalScalar only valid for nbasis=1, got $nbasis")
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
              def scalarAt(t: Seconds): Double = other(t).data(0)
          )

object Hrf:
  def of(
      name: String,
      nbasis: Int = 1,
      span: Seconds = Seconds(24.0),
      params: Map[String, Any] = Map.empty
  )(f: Seconds => scalafim.fmri.hrf.linalg.Vec): Hrf =
    require(nbasis >= 1, "nbasis must be >= 1")
    val name0 = name
    val nbasis0 = nbasis
    val span0 = span
    val params0 = params
    new Hrf:
      def name: String = name0
      def nbasis: Int = nbasis0
      def span: Seconds = span0
      override def params: Map[String, Any] = params0
      protected def eval1(t: Seconds): scalafim.fmri.hrf.linalg.Vec = f(t)

  def scalar(
      name: String,
      span: Seconds = Seconds(24.0),
      params: Map[String, Any] = Map.empty
  )(f: Seconds => Double): ScalarHrf =
    val name0 = name
    val span0 = span
    val params0 = params
    new ScalarHrf:
      def name: String = name0
      def span: Seconds = span0
      override def params: Map[String, Any] = params0
      def scalarAt(t: Seconds): Double = f(t)

  def multi(
      name: String,
      nbasis: Int,
      span: Seconds = Seconds(24.0),
      params: Map[String, Any] = Map.empty
  )(f: Seconds => Array[Double]): Hrf =
    of(name, nbasis = nbasis, span = span, params = params)(t => scalafim.fmri.hrf.linalg.Vec.unsafe(f(t)))
