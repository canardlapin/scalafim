package scalafim.fmri.group

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.linalg.DoubleMatrix

/** The second-level design: `[subjects × terms]` with named terms.
  *
  * Deliberately an explicit, typed matrix rather than a re-implementation of R's
  * `model.matrix` non-standard evaluation. Common designs are provided as
  * combinators; arbitrary designs go through `fromMatrix`.
  */
final case class GroupDesign private (matrix: DoubleMatrix, termNames: Vector[String]):
  require(matrix.cols == termNames.length, "term names must match design columns")

  def subjects: Int = matrix.rows
  def terms: Int = matrix.cols
  def hasTerm(name: String): Boolean = termNames.contains(name)

object GroupDesign:

  private val InterceptName = "(Intercept)"

  def fromMatrix(matrix: DoubleMatrix, termNames: Vector[String]): Either[GroupError, GroupDesign] =
    if matrix.rows == 0 || matrix.cols == 0 then Left(GroupError.EmptyDesign)
    else if matrix.cols != termNames.length then
      Left(GroupError.ContrastMismatch(matrix.cols, termNames.length))
    else if termNames.distinct.length != termNames.length then
      Left(GroupError.DuplicateTerms(termNames.diff(termNames.distinct)))
    else if !allFinite(matrix) then
      Left(GroupError.NonFiniteData("group design"))
    else Right(new GroupDesign(matrix, termNames))

  /** One-sample / intercept-only design: a single column of ones. Yields the
    * one-sample group t-test (OLS) or the classic meta-analysis (weighted).
    */
  def intercept(nSubjects: Int): GroupDesign =
    require(nSubjects > 0, "need at least one subject")
    new GroupDesign(DoubleMatrix.unsafe(nSubjects, 1, Array.fill(nSubjects)(1.0)), Vector(InterceptName))

  /** Two-sample design: intercept plus a 0/1 indicator for the non-reference
    * level (reference is the first level encountered). The indicator term is
    * named after the non-reference level, so a contrast on it is the group
    * difference.
    */
  def twoSample(groupLabels: Vector[String]): Either[GroupError, GroupDesign] =
    if groupLabels.isEmpty then Left(GroupError.EmptyDesign)
    else
      val levels = groupLabels.distinct
      if levels.length != 2 then Left(GroupError.ContrastMismatch(2, levels.length))
      else
        val other = levels(1)
        val n = groupLabels.length
        val data = new Array[Double](n * 2)
        var i = 0
        while i < n do
          data(i * 2) = 1.0
          data(i * 2 + 1) = if groupLabels(i) == other then 1.0 else 0.0
          i += 1
        Right(new GroupDesign(DoubleMatrix.unsafe(n, 2, data), Vector(InterceptName, other)))

  /** Covariate / meta-regression design from a typed `DataTable`. Numeric
    * columns become terms, optionally prefixed by an intercept column. Missing
    * or non-numeric columns are reported as typed errors (the constructor is
    * total — it never throws for ordinary user input).
    */
  def covariates(
      table: DataTable,
      columns: Vector[String],
      intercept: Boolean = true
  ): Either[GroupError, GroupDesign] =
    columns.find(c => !table.contains(c)) match
      case Some(c) => Left(GroupError.UnknownColumn(c))
      case None =>
        columns.find(c => !isNumeric(table.column(c))) match
          case Some(c) => Left(GroupError.NonNumericColumn(c))
          case None =>
            val n = table.nrows
            val columnData = columns.map(c => table.doubles(c))
            val termNames = (if intercept then Vector(InterceptName) else Vector.empty) ++ columns
            val p = termNames.length
            if n == 0 || p == 0 then Left(GroupError.EmptyDesign)
            else
              val data = new Array[Double](n * p)
              var row = 0
              while row < n do
                var col = 0
                if intercept then
                  data(row * p) = 1.0
                  col = 1
                var c = 0
                while c < columns.length do
                  data(row * p + col + c) = columnData(c)(row)
                  c += 1
                row += 1
              fromMatrix(DoubleMatrix.unsafe(n, p, data), termNames)

  private def isNumeric(column: Column): Boolean =
    column match
      case Column.Doubles(_) | Column.Ints(_) => true
      case _                                  => false

  private def allFinite(m: DoubleMatrix): Boolean =
    val data = m.copyData
    var i = 0
    var ok = true
    while ok && i < data.length do
      if !data(i).isFinite then ok = false
      i += 1
    ok
