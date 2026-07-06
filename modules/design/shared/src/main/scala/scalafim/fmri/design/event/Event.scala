package scalafim.fmri.design.event

import scalafim.fmri.design.Names
import scalafim.fmri.design.basis.ParametricBasis
import scalafim.fmri.hrf.linalg.Mat

sealed trait Event:
  def varName: String
  def nEvents: Int
  def isContinuous: Boolean

  /** Tokens used by `EventTerm.conditions` to build condition tags. */
  def conditionTokens: Vector[String]

final case class CategoricalEvent(varName: String, codes: Vector[Int], levels: Vector[String]) extends Event:
  val nEvents: Int = codes.length
  val isContinuous: Boolean = false
  val conditionTokens: Vector[String] =
    levels.map(lvl => Names.levelToken(varName, lvl))

final case class ContinuousEvent(
    varName: String,
    value: Mat,
    columnTags: Vector[String],
    basis: Option[ParametricBasis] = None
) extends Event:
  val nEvents: Int = value.rows
  val isContinuous: Boolean = true
  val conditionTokens: Vector[String] = columnTags

object Event:

  def factor(values: Seq[String], name: String): CategoricalEvent =
    val varName = Names.sanitize(name, allowDot = true)
    val levels = values.toVector.distinct.sorted
    val idx = levels.zipWithIndex.toMap
    val codes = values.iterator.map(v => idx(v)).toVector
    CategoricalEvent(varName, codes, levels)

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

  def basis(basis: ParametricBasis, name: Option[String] = None): ContinuousEvent =
    val varName = Names.sanitize(name.getOrElse(basis.name), allowDot = true)
    ContinuousEvent(varName, basis.y, basis.columns, basis = Some(basis))
