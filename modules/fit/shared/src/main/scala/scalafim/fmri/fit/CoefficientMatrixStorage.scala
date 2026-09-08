package scalafim.fmri.fit

import gale.linalg.{DMat, DVec, Matrix, CholeskyOptions}

/** Immutable coefficient-matrix fields. Logical voxel count is independent of
  * retained numeric storage; indexed access may compute one small matrix.
  */
private[fit] sealed abstract class CoefficientMatrixStorage extends IndexedSeq[DMat]:
  def predictors: Int
  def retainedDoubleCount: Long

private[fit] object CoefficientMatrixStorage:
  private final class Scaled(base: DMat, scales: DVec) extends CoefficientMatrixStorage:
    val predictors: Int = base.rows
    def length: Int = scales.length
    def retainedDoubleCount: Long = predictors.toLong * predictors + scales.length
    def apply(index: Int): DMat =
      val scale = scales(index)
      Matrix.tabulate(predictors, predictors)((r, c) => base(r, c) * scale)

  private final class InverseSum(val components: Vector[IndexedSeq[DMat]], val predictors: Int)
      extends CoefficientMatrixStorage:
    def length: Int = components.head.length
    def retainedDoubleCount: Long = components.map(retained).sum
    def apply(index: Int): DMat =
      inverse(sumAt(components, index)).fold(e => throw IllegalArgumentException(e.message), identity)

  private final class Selected(parent: IndexedSeq[DMat], positions: Vector[Int]) extends CoefficientMatrixStorage:
    val predictors: Int = parent.head.rows
    def length: Int = positions.length
    def retainedDoubleCount: Long = retained(parent)
    def apply(index: Int): DMat = parent(positions(index))

  private final class Concatenated(parts: Vector[IndexedSeq[DMat]]) extends CoefficientMatrixStorage:
    val predictors: Int = parts.head.head.rows
    private val ends = parts.scanLeft(0)((n, p) => Math.addExact(n, p.length)).tail
    def length: Int = ends.last
    def retainedDoubleCount: Long = parts.map(retained).sum
    def apply(index: Int): DMat =
      require(index >= 0 && index < length, "coefficient matrix index out of bounds")
      val part = ends.indexWhere(index < _)
      parts(part)(index - (if part == 0 then 0 else ends(part - 1)))

  def retained(values: IndexedSeq[DMat]): Long = values match
    case structured: CoefficientMatrixStorage => structured.retainedDoubleCount
    case _ => values.iterator.map(m => m.rows.toLong * m.cols).sum

  def valid(values: IndexedSeq[DMat], predictors: Int): Boolean = values match
    case structured: CoefficientMatrixStorage => structured.predictors == predictors && predictors > 0
    case _ => predictors > 0 && values.forall(m => m.rows == predictors && m.cols == predictors && allFinite(m))

  def scaled(base: DMat, scales: Vector[Double]): Either[FitError, IndexedSeq[DMat]] =
    if base.rows <= 0 || base.rows != base.cols || scales.isEmpty then
      Left(FitError.InvalidFitAxis("scaled coefficient precision", "nonempty square base and voxel scales required"))
    else if !allFinite(base) || !scales.forall(s => s > 0.0 && s.isFinite) then
      Left(FitError.NonFiniteInput("scaled coefficient precision requires finite base and positive finite scales"))
    else
      val maximum = scales.max
      var row = 0
      while row < base.rows do
        var col = 0
        while col < base.cols do
          if !(base(row, col) * maximum).isFinite then return Left(FitError.NonFiniteInput("scaled coefficient precision"))
          col += 1
        row += 1
      Right(new Scaled(Matrix.tabulate(base.rows, base.cols)(base.apply), DVec.fromSeq(scales)))

  def inverseSum(components: Vector[IndexedSeq[DMat]]): Either[FitError, IndexedSeq[DMat]] =
    if components.isEmpty || components.exists(_.isEmpty) then
      Left(FitError.InvalidFitAxis("coefficient precision sum", "nonempty components required"))
    else
      val predictors = components.head.head.rows
      val count = components.head.length
      if !components.forall(c => c.length == count && valid(c, predictors)) then
        Left(FitError.InvalidFitAxis("coefficient precision sum", "component voxel counts and square predictor shapes must agree"))
      else
        // Snapshot arbitrary callers; our closed immutable representations already own their data.
        val frozen = components.map {
          case owned: CoefficientMatrixStorage => owned
          case values => values.map(m => Matrix.tabulate(m.rows, m.cols)(m.apply)).toVector
        }
        var voxel = 0
        while voxel < count do
          inverse(sumAt(frozen, voxel)) match
            case Left(error) => return Left(FitError.InvalidFitAxis("coefficient precision sum", s"voxel $voxel: ${error.message}"))
            case Right(_) => ()
          voxel += 1
        Right(new InverseSum(frozen, predictors))

  /** Parents have already been validated by CoefficientCovariance. */
  def selected(values: IndexedSeq[DMat], positions: Vector[Int]): IndexedSeq[DMat] = new Selected(values, positions)
  def concatenated(parts: Vector[IndexedSeq[DMat]]): IndexedSeq[DMat] = new Concatenated(parts)

  def sumAt(components: Vector[IndexedSeq[DMat]], voxel: Int): DMat =
    val matrices = components.map(_(voxel))
    val size = matrices.head.rows
    val out = Matrix.newBuilder(size, size)
    matrices.foreach { matrix =>
      var r = 0
      while r < size do
        var c = 0
        while c < size do
          out(r, c) = out(r, c) + matrix(r, c)
          c += 1
        r += 1
    }
    out.result()

  private def inverse(matrix: DMat): Either[FitError, DMat] =
    val scale = (0 until matrix.rows).map(i => math.abs(matrix(i, i))).max
    matrix.cholesky(CholeskyOptions(math.max(1e-12, scale * 1e-12)))
      .left.map(e => FitError.InvalidFitAxis("coefficient precision sum", e.getMessage))
      .flatMap(_.solve(Matrix.eye(matrix.rows)).left.map(e => FitError.InvalidFitAxis("coefficient precision sum", e.getMessage)))
      .flatMap(value => if allFinite(value) then Right(value) else Left(FitError.NonFiniteInput("inverse coefficient precision")))

  private def allFinite(matrix: DMat): Boolean =
    var r = 0
    while r < matrix.rows do
      var c = 0
      while c < matrix.cols do
        if !matrix(r, c).isFinite then return false
        c += 1
      r += 1
    true
