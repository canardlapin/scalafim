package scalafim.fmri.design.contrast

import scalafim.fmri.design.contrast.ContrastWeights.Predicate
import scalafim.fmri.design.event.ConvolvedTerm

import scala.collection.immutable.VectorMap
import scala.util.matching.Regex

sealed trait ContrastSpec:
  def name: String
  def weights(term: ConvolvedTerm): ContrastWeights

  infix def -(other: ContrastSpec): ContrastSpec =
    ContrastSpec.Difference(name = s"$name:${other.name}", left = this, right = other)

object ContrastSpec:

  final case class Pair(
      name: String,
      A: Predicate,
      B: Predicate,
      where: Predicate = _ => true,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ) extends ContrastSpec:
    def weights(term: ConvolvedTerm): ContrastWeights =
      ConvolvedContrastWeights.pair(term, name, A, B, where = where, basis = basis, basisWeights = basisWeights)

  final case class UnitContrast(
      name: String,
      where: Predicate = _ => true,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ) extends ContrastSpec:
    def weights(term: ConvolvedTerm): ContrastWeights =
      ConvolvedContrastWeights.unit(term, name, where = where, basis = basis, basisWeights = basisWeights)

  final case class Poly(
      name: String,
      degree: Int,
      value: Cell => Double,
      where: Predicate = _ => true,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ) extends ContrastSpec:
    def weights(term: ConvolvedTerm): ContrastWeights =
      ConvolvedContrastWeights.poly(
        term,
        name,
        degree = degree,
        value = value,
        where = where,
        basis = basis,
        basisWeights = basisWeights
      )

  final case class Mask(
      name: String,
      A: Vector[Boolean],
      B: Option[Vector[Boolean]] = None,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ) extends ContrastSpec:
    def weights(term: ConvolvedTerm): ContrastWeights =
      ConvolvedContrastWeights.mask(
        term,
        name,
        maskA = A,
        maskB = B,
        basis = basis,
        basisWeights = basisWeights
      )

  final case class Difference(
      name: String,
      left: ContrastSpec,
      right: ContrastSpec
  ) extends ContrastSpec:
    def weights(term: ConvolvedTerm): ContrastWeights =
      val l = left.weights(term)
      val r = right.weights(term)
      require(l.condNames == r.condNames, s"Difference contrast '$name' requires matching condition names")
      require(l.weights.cols == r.weights.cols, s"Difference contrast '$name' requires matching contrast column counts")
      val out = new Array[Double](l.weights.rows * l.weights.cols)
      var i = 0
      while i < out.length do
        out(i) = l.weights.data(i) - r.weights.data(i)
        i += 1
      val names =
        if l.weights.cols == 1 then Vector(name)
        else (1 to l.weights.cols).map(j => s"${name}_$j").toVector
      ContrastWeights(
        name = name,
        condNames = l.condNames,
        contrastNames = names,
        weights = scalafim.fmri.hrf.linalg.Mat.unsafe(l.weights.rows, l.weights.cols, out),
        selectedCondNames = (l.selectedCondNames ++ r.selectedCondNames).distinct
      )

  final case class Oneway(
      name: String,
      factor: String,
      where: Predicate = _ => true,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ) extends ContrastSpec:
    def weights(term: ConvolvedTerm): ContrastWeights =
      ConvolvedContrastWeights.oneway(
        term,
        name,
        factor = factor,
        where = where,
        basis = basis,
        basisWeights = basisWeights
      )

  final case class Interaction(
      name: String,
      factor1: String,
      factor2: String,
      where: Predicate = _ => true,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ) extends ContrastSpec:
    def weights(term: ConvolvedTerm): ContrastWeights =
      ConvolvedContrastWeights.interaction(
        term,
        name,
        factor1 = factor1,
        factor2 = factor2,
        where = where,
        basis = basis,
        basisWeights = basisWeights
      )

  final case class Column(
      name: String,
      patternA: Regex,
      patternB: Option[Regex] = None
  ) extends ContrastSpec:
    def weights(term: ConvolvedTerm): ContrastWeights =
      ConvolvedContrastWeights.column(term, name, patternA = patternA, patternB = patternB)

  final case class ContrastSet(contrasts: Vector[ContrastSpec]):
    def weights(term: ConvolvedTerm): VectorMap[String, ContrastWeights] =
      VectorMap.from(contrasts.map(c => c.name -> c.weights(term)))

    def ++(other: ContrastSet): ContrastSet = ContrastSet(contrasts ++ other.contrasts)
    def :+(c: ContrastSpec): ContrastSet = ContrastSet(contrasts :+ c)

  object ContrastSet:
    def apply(contrasts: ContrastSpec*): ContrastSet = ContrastSet(contrasts.toVector)

  def oneAgainstAllContrasts(
      levels: Seq[String],
      facName: String,
      where: Predicate = _ => true,
      namePrefix: String = "con",
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): ContrastSet =
    val levs = levels.toVector
    require(levs.length >= 2, "oneAgainstAllContrasts requires at least two levels")
    require(levs.distinct.length == levs.length, "levels must be distinct")
    ContrastSet(
      levs.map { lev =>
        Pair(
          name = s"${namePrefix}_${lev}_vs_other",
          A = cell => cell(facName) == lev,
          B = cell => cell(facName) != lev,
          where = where,
          basis = basis,
          basisWeights = basisWeights
        )
      }
    )

  def pairwiseContrasts(
      levels: Seq[String],
      facName: String,
      where: Predicate = _ => true,
      namePrefix: String = "con",
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): ContrastSet =
    val levs = levels.toVector
    require(levs.length >= 2, "pairwiseContrasts requires at least two levels")
    require(levs.distinct.length == levs.length, "levels must be distinct")

    val out = Vector.newBuilder[ContrastSpec]
    var i = 0
    while i < levs.length - 1 do
      var j = i + 1
      while j < levs.length do
        val a = levs(i)
        val b = levs(j)
        out += Pair(
          name = s"${namePrefix}_${a}_${b}",
          A = cell => cell(facName) == a,
          B = cell => cell(facName) == b,
          where = where,
          basis = basis,
          basisWeights = basisWeights
        )
        j += 1
      i += 1

    ContrastSet(out.result())

  def slidingWindowContrasts(
      levels: Seq[String],
      facName: String,
      windowSize: Int = 2,
      where: Predicate = _ => true,
      namePrefix: String = "win",
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): ContrastSet =
    require(windowSize >= 1, "'windowSize' must be >= 1")
    val levs = levels.toVector
    require(levs.length >= 2, "slidingWindowContrasts requires at least two levels")
    require(levs.distinct.length == levs.length, "levels must be distinct")
    require(2 * windowSize <= levs.length, "'windowSize' too large: requires 2*windowSize <= levels.length")

    val nCon = levs.length - 2 * windowSize + 1
    val out = Vector.newBuilder[ContrastSpec]
    var i = 0
    while i < nCon do
      val aLevels = levs.slice(i, i + windowSize)
      val bLevels = levs.slice(i + windowSize, i + 2 * windowSize)
      val aSet = aLevels.toSet
      val bSet = bLevels.toSet
      val name = s"${namePrefix}_${aLevels.mkString("-")}_vs_${bLevels.mkString("-")}"

      out += Pair(
        name = name,
        A = cell => aSet.contains(cell(facName)),
        B = cell => bSet.contains(cell(facName)),
        where = where,
        basis = basis,
        basisWeights = basisWeights
      )
      i += 1

    ContrastSet(out.result())

  /** Translate legacy regex patterns to the current column naming scheme.
    *
    * Mirrors `translate_legacy_pattern()` from the R package:
    * - `Var[Level]` -> `Var.Level`
    * - `:basis[<k>]` -> `_b<k>` (preserves trailing `$`)
    * - standalone `:` -> `_`
    */
  def translateLegacyPattern(pattern: String): String =
    if pattern == null then throw new IllegalArgumentException("pattern must not be null")
    // Apply basis rewrite first so `basis[2]` doesn't get transformed into `basis.2`.
    val step1 = pattern.replaceAll(":basis\\[(\\d+)\\](\\$?)$", "_b$1$2")
    val step2 = step1.replaceAll("([A-Za-z0-9_\\.]+)\\[([^\\]]+)\\]", "$1.$2")
    val out = new StringBuilder(step2.length)
    var i = 0
    while i < step2.length do
      val c = step2.charAt(i)
      if c == ':' then
        val prevIsColon = i > 0 && step2.charAt(i - 1) == ':'
        val nextIsColon = i + 1 < step2.length && step2.charAt(i + 1) == ':'
        if !prevIsColon && !nextIsColon then out.append('_') else out.append(':')
      else out.append(c)
      i += 1
    out.result()
