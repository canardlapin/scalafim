package scalafim.fmri.design.contrast

import scalafim.fmri.design.contrast.ContrastWeights.Predicate
import scalafim.fmri.design.event.{ConvolvedTerm, EventTerm}
import scalafim.fmri.hrf.linalg.Mat

import scala.util.matching.Regex

object ConvolvedContrastWeights:

  def pair(
      term: ConvolvedTerm,
      name: String,
      A: Predicate,
      B: Predicate,
      where: Predicate = _ => true,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): ContrastWeights =
    val cw0 = ContrastWeights.pair(
      term = term.term,
      name = name,
      A = A,
      B = B,
      where = where,
      nbasis = term.hrf.nbasis,
      basis = basis,
      basisWeights = basisWeights
    )
    alignToConvolved(cw0, term)

  def unit(
      term: ConvolvedTerm,
      name: String,
      where: Predicate = _ => true,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): ContrastWeights =
    val cw0 = ContrastWeights.unit(
      term = term.term,
      name = name,
      where = where,
      nbasis = term.hrf.nbasis,
      basis = basis,
      basisWeights = basisWeights
    )
    alignToConvolved(cw0, term)

  def poly(
      term: ConvolvedTerm,
      name: String,
      degree: Int,
      value: Cell => Double,
      where: Predicate = _ => true,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): ContrastWeights =
    val cw0 = ContrastWeights.poly(
      term = term.term,
      name = name,
      degree = degree,
      value = value,
      where = where,
      nbasis = term.hrf.nbasis,
      basis = basis,
      basisWeights = basisWeights
    )
    alignToConvolved(cw0, term)

  def mask(
      term: ConvolvedTerm,
      name: String,
      maskA: Seq[Boolean],
      maskB: Option[Seq[Boolean]] = None,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): ContrastWeights =
    val cw0 = ContrastWeights.mask(
      term = term.term,
      name = name,
      maskA = maskA,
      maskB = maskB,
      nbasis = term.hrf.nbasis,
      basis = basis,
      basisWeights = basisWeights
    )
    alignToConvolved(cw0, term)

  def oneAgainstAllContrasts(
      term: ConvolvedTerm,
      levels: Seq[String],
      facName: String,
      where: Predicate = _ => true,
      namePrefix: String = "con",
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): Vector[ContrastWeights] =
    val levs = levels.toVector
    require(levs.length >= 2, "oneAgainstAllContrasts requires at least two levels")
    require(levs.distinct.length == levs.length, "levels must be distinct")

    levs.map { lev =>
      pair(
        term = term,
        name = s"${namePrefix}_${lev}_vs_other",
        A = cell => cell(facName) == lev,
        B = cell => cell(facName) != lev,
        where = where,
        basis = basis,
        basisWeights = basisWeights
      )
    }

  def pairwiseContrasts(
      term: ConvolvedTerm,
      levels: Seq[String],
      facName: String,
      where: Predicate = _ => true,
      namePrefix: String = "con",
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): Vector[ContrastWeights] =
    val levs = levels.toVector
    require(levs.length >= 2, "pairwiseContrasts requires at least two levels")
    require(levs.distinct.length == levs.length, "levels must be distinct")

    val out = Vector.newBuilder[ContrastWeights]
    var i = 0
    while i < levs.length - 1 do
      var j = i + 1
      while j < levs.length do
        val a = levs(i)
        val b = levs(j)
        out += pair(
          term = term,
          name = s"${namePrefix}_${a}_${b}",
          A = cell => cell(facName) == a,
          B = cell => cell(facName) == b,
          where = where,
          basis = basis,
          basisWeights = basisWeights
        )
        j += 1
      i += 1
    out.result()

  def slidingWindowContrasts(
      term: ConvolvedTerm,
      levels: Seq[String],
      facName: String,
      windowSize: Int = 2,
      where: Predicate = _ => true,
      namePrefix: String = "win",
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): Vector[ContrastWeights] =
    require(windowSize >= 1, "'windowSize' must be >= 1")
    val levs = levels.toVector
    require(levs.length >= 2, "slidingWindowContrasts requires at least two levels")
    require(levs.distinct.length == levs.length, "levels must be distinct")
    require(2 * windowSize <= levs.length, "'windowSize' too large: requires 2*windowSize <= levels.length")

    val nCon = levs.length - 2 * windowSize + 1
    val out = Vector.newBuilder[ContrastWeights]
    var i = 0
    while i < nCon do
      val aLevels = levs.slice(i, i + windowSize)
      val bLevels = levs.slice(i + windowSize, i + 2 * windowSize)
      val aSet = aLevels.toSet
      val bSet = bLevels.toSet
      val name = s"${namePrefix}_${aLevels.mkString("-")}_vs_${bLevels.mkString("-")}"

      out += pair(
        term = term,
        name = name,
        A = cell => aSet.contains(cell(facName)),
        B = cell => bSet.contains(cell(facName)),
        where = where,
        basis = basis,
        basisWeights = basisWeights
      )
      i += 1
    out.result()

  def column(
      term: ConvolvedTerm,
      name: String,
      patternA: Regex,
      patternB: Option[Regex] = None
  ): ContrastWeights =
    val names = term.columnNames
    val maskA = names.map(n => patternA.findFirstIn(n).nonEmpty).toArray
    val maskB = patternB.map(re => names.map(n => re.findFirstIn(n).nonEmpty).toArray)
    val w = ContrastWeights.calculateMaskWeights(names, maskA, maskB)

    val selected = names.indices.iterator.filter(i => w(i) != 0.0).map(names).toVector
    ContrastWeights(
      name = name,
      condNames = names,
      contrastNames = Vector(name),
      weights = Mat.unsafe(names.length, 1, w),
      selectedCondNames = selected
    )

  def oneway(
      term: ConvolvedTerm,
      name: String,
      factor: String,
      where: Predicate = _ => true,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): ContrastWeights =
    val (baseMat, outCols) = mainEffectWeights(term.term, name, factor, where)

    val baseCondNames = term.term.conditions
    val (condNames, expanded) = ContrastWeights.expandWeights(baseMat, baseCondNames, term.hrf.nbasis)
    val (filtered, selected0) = ContrastWeights.applyBasisFiltering(expanded, baseCondNames, term.hrf.nbasis, basis, basisWeights)

    val cw0 = ContrastWeights(
      name = name,
      condNames = condNames,
      contrastNames = (1 to outCols).map(j => s"${name}_${j}").toVector,
      weights = filtered,
      selectedCondNames = selected0
    )
    alignToConvolved(cw0, term)

  def interaction(
      term: ConvolvedTerm,
      name: String,
      factor1: String,
      factor2: String,
      where: Predicate = _ => true,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): ContrastWeights =
    val (baseMat, outCols) = interactionWeights(term.term, name, factor1, factor2, where)

    val baseCondNames = term.term.conditions
    val (condNames, expanded) = ContrastWeights.expandWeights(baseMat, baseCondNames, term.hrf.nbasis)
    val (filtered, selected0) = ContrastWeights.applyBasisFiltering(expanded, baseCondNames, term.hrf.nbasis, basis, basisWeights)

    val cw0 = ContrastWeights(
      name = name,
      condNames = condNames,
      contrastNames = (1 to outCols).map(j => s"${name}_${j}").toVector,
      weights = filtered,
      selectedCondNames = selected0
    )
    alignToConvolved(cw0, term)

  private def mainEffectWeights(term: EventTerm, contrastName: String, factor: String, where: Predicate): (Mat, Int) =
    val cats = ContrastWeights.categoricalOnly(term, contrastName)
    val baseCondNames = term.conditions

    val observed = TermCells.from(term, dropEmpty = true)
    val fi = observed.vars.indexOf(factor)
    require(fi >= 0, s"Contrast '$contrastName': factor '$factor' not found in term")

    val relevant = observed.rows.filter(row => where(Cell(observed.vars, row.levels)))
    val levs = relevant.map(_.levels(fi)).distinct.sorted
    val k = levs.length
    require(k >= 2, s"Need at least 2 levels for main effect on '$factor'")

    val outCols = k - 1
    val levIndex = levs.zipWithIndex.toMap

    val out = new Array[Double](baseCondNames.length * outCols)
    var i = 0
    while i < relevant.length do
      val row = relevant(i)
      val idx = ContrastWeights.cellIndex(cats, row.levels)
      if idx >= 0 then
        val li = levIndex(row.levels(fi))
        val h = helmertRow(li, k)
        System.arraycopy(h, 0, out, idx * outCols, outCols)
      i += 1

    (Mat.unsafe(baseCondNames.length, outCols, out), outCols)

  private def interactionWeights(
      term: EventTerm,
      contrastName: String,
      factor1: String,
      factor2: String,
      where: Predicate
  ): (Mat, Int) =
    val cats = ContrastWeights.categoricalOnly(term, contrastName)
    val baseCondNames = term.conditions

    val observed = TermCells.from(term, dropEmpty = true)
    val f1i = observed.vars.indexOf(factor1)
    val f2i = observed.vars.indexOf(factor2)
    require(f1i >= 0 && f2i >= 0, s"Contrast '$contrastName': both factors must be present in term")
    require(f1i != f2i, s"Contrast '$contrastName': factors must be distinct")

    val relevant = observed.rows.filter(row => where(Cell(observed.vars, row.levels)))
    val levs1 = relevant.map(_.levels(f1i)).distinct.sorted
    val levs2 = relevant.map(_.levels(f2i)).distinct.sorted
    val k1 = levs1.length
    val k2 = levs2.length
    require(k1 >= 2 && k2 >= 2, "Each factor needs >= 2 levels for interaction")

    val a = k1 - 1
    val b = k2 - 1
    val outCols = a * b

    val levIndex1 = levs1.zipWithIndex.toMap
    val levIndex2 = levs2.zipWithIndex.toMap

    val out = new Array[Double](baseCondNames.length * outCols)
    var i = 0
    while i < relevant.length do
      val row = relevant(i)
      val idx = ContrastWeights.cellIndex(cats, row.levels)
      if idx >= 0 then
        val h1 = helmertRow(levIndex1(row.levels(f1i)), k1)
        val h2 = helmertRow(levIndex2(row.levels(f2i)), k2)

        var c = 0
        var i1 = 0
        while i1 < a do
          var i2 = 0
          while i2 < b do
            out(idx * outCols + c) = h1(i1) * h2(i2)
            c += 1
            i2 += 1
          i1 += 1
      i += 1

    (Mat.unsafe(baseCondNames.length, outCols, out), outCols)

  private def helmertRow(levelIndex: Int, k: Int): Array[Double] =
    require(k >= 2, "k must be >= 2 for Helmert coding")
    require(levelIndex >= 0 && levelIndex < k, "levelIndex out of range")
    val out = new Array[Double](k - 1)
    var j = 0
    while j < k - 1 do
      if levelIndex <= j then out(j) = -1.0
      else if levelIndex == j + 1 then out(j) = (j + 1).toDouble
      else out(j) = 0.0
      j += 1
    out

  private def alignToConvolved(cw: ContrastWeights, term: ConvolvedTerm): ContrastWeights =
    val cols = term.columnNames
    val prefix = term.term.termTag.map(_ + "_")
    val idx = cw.condNames.zipWithIndex.toMap
    val out = new Array[Double](cols.length * cw.weights.cols)

    var rOut = 0
    while rOut < cols.length do
      val raw = cols(rOut)
      val key =
        prefix match
          case Some(p) if raw.startsWith(p) => raw.drop(p.length)
          case _                            => raw

      idx.get(key).foreach { rIn =>
        System.arraycopy(cw.weights.data, rIn * cw.weights.cols, out, rOut * cw.weights.cols, cw.weights.cols)
      }
      rOut += 1

    val selected =
      val base = cw.selectedCondNames
      val withPrefix = prefix match
        case None    => base
        case Some(p) => base.map(p + _)
      val colSet = cols.toSet
      withPrefix.filter(colSet)

    ContrastWeights(
      name = cw.name,
      condNames = cols,
      contrastNames = cw.contrastNames,
      weights = Mat.unsafe(cols.length, cw.weights.cols, out),
      selectedCondNames = selected
    )
