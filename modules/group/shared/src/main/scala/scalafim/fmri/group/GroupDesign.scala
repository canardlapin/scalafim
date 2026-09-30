package scalafim.fmri.group

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.design.{ColumnId, DesignError}
import scalafim.fmri.design.data.DataTable

/** The second-level design: `[subjects × terms]` with named terms.
  *
  * Deliberately an explicit, typed matrix rather than a re-implementation of R's
  * `model.matrix` non-standard evaluation. Common designs are provided as
  * combinators; arbitrary designs go through `fromMatrix`.
  */
final case class GroupDesign private (matrix: DMat, designTerms: Vector[DesignTermName]):
  require(matrix.cols == designTerms.length, "term names must match design columns")

  def subjects: Int = matrix.rows
  def terms: Int = matrix.cols
  def termNames: Vector[String] = designTerms.map(_.value)
  def hasTerm(name: String): Boolean = termNames.contains(name)
  def hasDesignTerm(name: DesignTermName): Boolean = designTerms.contains(name)

object GroupDesign:

  private val InterceptName = DesignTermName.Intercept

  def fromMatrix(matrix: DMat, termNames: Vector[String]): Either[GroupError, GroupDesign] =
    parseTermNames(termNames).flatMap(fromTypedMatrix(matrix, _))

  def fromTypedMatrix(matrix: DMat, termNames: Vector[DesignTermName]): Either[GroupError, GroupDesign] =
    if matrix.rows == 0 || matrix.cols == 0 then Left(GroupError.EmptyDesign)
    else if matrix.cols != termNames.length then
      Left(GroupError.contrastMismatch(matrix.cols, termNames.length))
    else if termNames.distinct.length != termNames.length then
      Left(GroupError.DuplicateTerms(termNames.diff(termNames.distinct).map(_.value)))
    else if !allFinite(matrix) then
      Left(GroupError.NonFiniteData("group design"))
    else Right(new GroupDesign(matrix, termNames))

  /** One-sample / intercept-only design: a single column of ones. Yields the
    * one-sample group t-test (OLS) or the classic meta-analysis (weighted).
    */
  def intercept(nSubjects: Int): GroupDesign =
    require(nSubjects > 0, "need at least one subject")
    val matrix = Matrix.newBuilder(nSubjects, 1)
    matrix.fill(1.0)
    new GroupDesign(matrix.result(), Vector(InterceptName))

  /** Two-sample design: intercept plus a 0/1 indicator for the non-reference
    * level (reference is the first level encountered). The indicator term is
    * named after the non-reference level, so a contrast on it is the group
    * difference.
    */
  def twoSample(groupLabels: Vector[String]): Either[GroupError, GroupDesign] =
    if groupLabels.isEmpty then Left(GroupError.EmptyDesign)
    else parseTermNames(groupLabels).flatMap { labels =>
      val levels = labels.distinct
      if levels.length != 2 then Left(GroupError.contrastMismatch(2, levels.length))
      else
        val other = levels(1)
        val matrix = Matrix.newBuilder(labels.length, 2)
        var i = 0
        while i < labels.length do
          matrix(i, 0) = 1.0
          matrix(i, 1) = if labels(i) == other then 1.0 else 0.0
          i += 1
        fromTypedMatrix(matrix.result(), Vector(InterceptName, other))
    }

  /** Covariate / meta-regression design from a typed `DataTable`. Numeric
    * columns become terms, optionally prefixed by an intercept column. Missing
    * or non-numeric columns are reported as typed errors (the constructor is
    * total — it never throws for ordinary user input).
    */
  def covariates(
      table: DataTable,
      columns: Vector[String],
      intercept: InterceptPolicy = InterceptPolicy.Include
  ): Either[GroupError, GroupDesign] =
    parseCovariates(columns).flatMap(typedCovariates(table, _, intercept))

  def covariates(
      table: DataTable,
      columns: Vector[String],
      intercept: Boolean
  ): Either[GroupError, GroupDesign] =
    covariates(table, columns, InterceptPolicy.fromBoolean(intercept))

  def typedCovariates(
      table: DataTable,
      columns: Vector[CovariateName],
      intercept: InterceptPolicy
  ): Either[GroupError, GroupDesign] =
    numericColumns(table, columns).flatMap { columnData =>
      val n = table.nrows
      val termNames = (if intercept.include then Vector(InterceptName) else Vector.empty) ++ columns.map(c => DesignTermName.unsafe(c.value))
      val p = termNames.length
      if n == 0 || p == 0 then Left(GroupError.EmptyDesign)
      else
        val matrix = Matrix.newBuilder(n, p)
        var row = 0
        while row < n do
          var col = 0
          if intercept.include then
            matrix(row, 0) = 1.0
            col = 1
          var c = 0
          while c < columns.length do
            matrix(row, col + c) = columnData(c)(row)
            c += 1
          row += 1
        fromTypedMatrix(matrix.result(), termNames)
    }

  /** Read every covariate column as numeric, reporting the first that a design cannot use.
    *
    * Missing columns are reported ahead of non-numeric ones so that a caller
    * naming several bad columns hears about the absent one first.
    */
  private def numericColumns(
      table: DataTable,
      columns: Vector[CovariateName]
  ): Either[GroupError, Vector[Vector[Double]]] =
    val reads = columns.map(c => c.value -> ColumnId(c.value).flatMap(table.get[Double]))

    def firstNaming(name: String => GroupError)(pf: PartialFunction[DesignError, Unit]): Option[GroupError] =
      reads.collectFirst { case (column, Left(error)) if pf.isDefinedAt(error) => name(column) }

    val absent = firstNaming(GroupError.UnknownColumn.apply) {
      case DesignError.MissingColumn(_) | DesignError.InvalidId(_, _, _) => ()
    }
    val unusable = firstNaming(GroupError.NonNumericColumn.apply) { case _ => () }

    absent.orElse(unusable) match
      case Some(error) => Left(error)
      case None        => Right(reads.collect { case (_, Right(values)) => values })

  private def allFinite(m: DMat): Boolean =
    var row = 0
    var ok = true
    while ok && row < m.rows do
      var col = 0
      while ok && col < m.cols do
        if !m(row, col).isFinite then ok = false
        col += 1
      row += 1
    ok

  private def parseTermNames(names: Vector[String]): Either[GroupError, Vector[DesignTermName]] =
    names.foldLeft[Either[GroupError, Vector[DesignTermName]]](Right(Vector.empty)) {
      case (Left(err), _) => Left(err)
      case (Right(acc), name) => DesignTermName(name).map(acc :+ _)
    }

  private def parseCovariates(names: Vector[String]): Either[GroupError, Vector[CovariateName]] =
    names.foldLeft[Either[GroupError, Vector[CovariateName]]](Right(Vector.empty)) {
      case (Left(err), _) => Left(err)
      case (Right(acc), name) => CovariateName(name).map(acc :+ _)
    }
