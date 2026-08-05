package scalafim.fmri.design.event

import scalafim.fmri.design.{ConditionId, DesignError, EventId, FactorId, FactorLevelSet, ModulatorId, Names}
import scalafim.fmri.design.basis.ParametricBasis
import scalafim.fmri.hrf.linalg.Mat

sealed trait Event:
  def varName: String
  def nEvents: Int
  def isContinuous: Boolean

  /** Tokens used by `EventTerm.conditions` to build condition tags. */
  def conditionTokens: Vector[String]

  def eventId: EventId =
    EventId.unsafe(varName)

  def factorId: Option[FactorId] =
    None

  def conditionIds: Vector[ConditionId] =
    conditionTokens.map(ConditionId.unsafe)

final case class CategoricalEvent(varName: String, codes: Vector[Int], levels: Vector[String]) extends Event:
  val nEvents: Int = codes.length
  val isContinuous: Boolean = false
  val conditionTokens: Vector[String] =
    levels.map(lvl => Names.levelToken(varName, lvl))
  override def factorId: Option[FactorId] =
    Some(FactorId.unsafe(varName))

final case class ContinuousEvent(
    varName: String,
    value: Mat,
    columnTags: Vector[String],
    basis: Option[ParametricBasis] = None,
    /** Per-column scientific identities for an additive modulator family.
      *
      * An empty vector preserves the legacy single-source interpretation. A
      * populated vector is exact: one identity for every numerical column.
      */
    columnModulators: Vector[ModulatorId] = Vector.empty
) extends Event:
  require(
    columnModulators.isEmpty || columnModulators.length == value.cols,
    "continuous-event modulator identities must align with numerical columns"
  )

  val nEvents: Int = value.rows
  val isContinuous: Boolean = true
  val conditionTokens: Vector[String] = columnTags

  /** Resolve one semantic modulator identity per numerical column. */
  def modulatorIds: Vector[ModulatorId] =
    if columnModulators.nonEmpty then columnModulators
    else
      val id = ModulatorId.unsafe(basis.map(_.argName).getOrElse(varName))
      Vector.fill(value.cols)(id)

object Event:

  /** Convenience constructor for one self-contained table.
    *
    * Levels are inferred deterministically from the observed values.  This is
    * not a schema authority for independently loaded runs or subjects; those
    * workflows must declare a [[FactorLevelRegistry]] and use
    * [[factorWithLevels]] so absent levels and ordering remain stable.
    */
  def factor(values: Seq[String], name: String): CategoricalEvent =
    val varName = Names.sanitize(name, allowDot = true)
    val levels = values.toVector.distinct.sorted
    val idx = levels.zipWithIndex.toMap
    val codes = values.iterator.map(v => idx(v)).toVector
    CategoricalEvent(varName, codes, levels)

  /** Construct a categorical event against a caller-declared, stable level set.
    *
    * Unlike [[factor]], this does not infer level order from the observed rows;
    * it also rejects observations that are not in the declaration before any
    * design matrix is built.
    */
  def factorWithLevels(
      values: Seq[String],
      name: String,
      levelSet: FactorLevelSet
  ): Either[DesignError, CategoricalEvent] =
    val varName = Names.sanitize(name, allowDot = true)
    val factor = FactorId.unsafe(varName)
    if factor.value != levelSet.factor.value then
      Left(DesignError.InvalidSchema(s"declared factor '${levelSet.factor.value}' does not match column '$varName'"))
    else
      val codes = Vector.newBuilder[Int]
      var error: Option[DesignError] = None
      val iterator = values.iterator
      while iterator.hasNext && error.isEmpty do
        val value = iterator.next()
        levelSet.indexOf(value) match
          case Some(index) => codes += index
          case None        => error = Some(DesignError.UnknownFactorLevel(varName, value, levelSet.values))
      error match
        case Some(value) => Left(value)
        case None        => Right(CategoricalEvent(varName, codes.result(), levelSet.values))

  def variable(values: Seq[Double], name: String): ContinuousEvent =
    val varName = Names.sanitize(name, allowDot = true)
    val xs = values.toVector
    val out = xs.toArray
    ContinuousEvent(
      varName = varName,
      value = Mat.unsafe(xs.length, 1, out),
      columnTags = Vector(Names.continuousToken(varName)),
      basis = None
    )

  def matrix(values: Mat, name: String, colNames: Option[Seq[String]] = None): ContinuousEvent =
    val varName = Names.sanitize(name, allowDot = true)
    val tags =
      colNames match
        case Some(ns) if ns.nonEmpty && ns.distinct.size == ns.size =>
          Names.sanitizeAll(ns, allowDot = true)
        case _ =>
          (1 to values.cols).map(j => Names.featureSuffix(j, values.cols)).toVector

    ContinuousEvent(varName, values, tags, basis = None)

  /** Construct sibling parametric-modulator columns without turning them into
    * an interaction. The order is scientifically consequential and is
    * preserved exactly for subsequent ordered policy application.
    */
  def modulatorFamily(
      columns: Vector[(ModulatorId, Vector[Double])],
      name: String = "modulators"
  ): Either[DesignError, ContinuousEvent] =
    columns.headOption match
      case None =>
        Left(DesignError.FormulaBinding("modulators(...) requires at least one numeric column"))
      case Some(_) if columns.map(_._1).distinct.length != columns.length =>
        Left(DesignError.FormulaBinding("modulators(...) column identities must be distinct"))
      case Some((_, firstValues)) =>
        val rows = firstValues.length
        columns.find(_._2.length != rows) match
          case Some((id, values)) =>
            Left(
              DesignError.InvalidSchema(
                s"modulator '${id.value}' has ${values.length} rows but expected $rows"
              )
            )
          case None =>
            val out = new Array[Double](rows * columns.length)
            var row = 0
            while row < rows do
              var column = 0
              while column < columns.length do
                out(row * columns.length + column) = columns(column)._2(row)
                column += 1
              row += 1
            val ids = columns.map(_._1)
            Right(
              ContinuousEvent(
                varName = Names.sanitize(name, allowDot = true),
                value = Mat.unsafe(rows, columns.length, out),
                columnTags = ids.map(id => Names.continuousToken(id.value)),
                basis = None,
                columnModulators = ids
              )
            )

  def basis(basis: ParametricBasis, name: Option[String] = None): ContinuousEvent =
    val varName = Names.sanitize(name.getOrElse(basis.name), allowDot = true)
    ContinuousEvent(varName, basis.y, basis.columns, basis = Some(basis))
