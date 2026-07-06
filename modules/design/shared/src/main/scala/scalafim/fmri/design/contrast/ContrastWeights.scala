package scalafim.fmri.design.contrast

import scalafim.fmri.design.Names
import scalafim.fmri.design.basis.ParametricBasis
import scalafim.fmri.design.event.{CategoricalEvent, EventTerm}
import scalafim.fmri.hrf.linalg.Mat

final case class ContrastWeights(
    name: String,
    condNames: Vector[String],
    contrastNames: Vector[String],
    weights: Mat,
    selectedCondNames: Vector[String]
):
  require(weights.rows == condNames.length, "weights/condNames row mismatch")
  require(weights.cols == contrastNames.length, "weights/contrastNames col mismatch")

  def embedIn(allColumns: Vector[String]): ContrastWeights =
    val idx = condNames.zipWithIndex.toMap
    val out = new Array[Double](allColumns.length * weights.cols)
    var rOut = 0
    while rOut < allColumns.length do
      idx.get(allColumns(rOut)).foreach { rIn =>
        System.arraycopy(weights.data, rIn * weights.cols, out, rOut * weights.cols, weights.cols)
      }
      rOut += 1
    ContrastWeights(
      name = name,
      condNames = allColumns,
      contrastNames = contrastNames,
      weights = Mat.unsafe(allColumns.length, weights.cols, out),
      selectedCondNames = selectedCondNames.filter(allColumns.toSet)
    )

object ContrastWeights:

  type Predicate = Cell => Boolean

  def pair(
      term: EventTerm,
      name: String,
      A: Predicate,
      B: Predicate,
      where: Predicate = _ => true,
      nbasis: Int = 1,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): ContrastWeights =
    val cats = categoricalOnly(term, name)
    val baseCondNames = term.conditions

    val observed = TermCells.from(term, dropEmpty = true)
    val maskA = Array.fill(baseCondNames.length)(false)
    val maskB = Array.fill(baseCondNames.length)(false)

    observed.rows.foreach { row =>
      val cell = Cell(observed.vars, row.levels)
      if where(cell) then
        val idx = cellIndex(cats, row.levels)
        if idx >= 0 then
          if A(cell) then maskA(idx) = true
          if B(cell) then maskB(idx) = true
    }

    val baseW = calculateMaskWeights(baseCondNames, maskA, Some(maskB))
    val baseMat = Mat.unsafe(baseW.length, 1, baseW)
    val (condNames, expanded) = expandWeights(baseMat, baseCondNames, nbasis)
    val (filtered, selected) = applyBasisFiltering(expanded, baseCondNames, nbasis, basis, basisWeights)

    ContrastWeights(
      name = name,
      condNames = condNames,
      contrastNames = Vector(name),
      weights = filtered,
      selectedCondNames = selected
    )

  def unit(
      term: EventTerm,
      name: String,
      where: Predicate = _ => true,
      nbasis: Int = 1,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): ContrastWeights =
    val cats = categoricalOnly(term, name)
    val baseCondNames = term.conditions

    val observed = TermCells.from(term, dropEmpty = true)
    val maskA = Array.fill(baseCondNames.length)(false)

    observed.rows.foreach { row =>
      val cell = Cell(observed.vars, row.levels)
      if where(cell) then
        val idx = cellIndex(cats, row.levels)
        if idx >= 0 then maskA(idx) = true
    }

    val baseW = calculateMaskWeights(baseCondNames, maskA, None)
    val baseMat = Mat.unsafe(baseW.length, 1, baseW)
    val (condNames, expanded) = expandWeights(baseMat, baseCondNames, nbasis)
    val (filtered, selected) = applyBasisFiltering(expanded, baseCondNames, nbasis, basis, basisWeights)

    ContrastWeights(
      name = name,
      condNames = condNames,
      contrastNames = Vector(name),
      weights = filtered,
      selectedCondNames = selected
    )

  def poly(
      term: EventTerm,
      name: String,
      degree: Int,
      value: Cell => Double,
      where: Predicate = _ => true,
      nbasis: Int = 1,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): ContrastWeights =
    require(degree >= 1, "'degree' must be >= 1")
    val cats = categoricalOnly(term, name)
    val baseCondNames = term.conditions

    val observed = TermCells.from(term, dropEmpty = true)
    val relevant = observed.rows.filter { row =>
      val cell = Cell(observed.vars, row.levels)
      where(cell)
    }

    val vals = relevant.map(row => value(Cell(observed.vars, row.levels))).toVector
    require(vals.distinct.length > degree, s"Polynomial degree ($degree) too high for unique values (${vals.distinct.length})")

    val pvals =
      if vals.isEmpty then Mat.zeros(0, degree)
      else ParametricBasis.Poly.fit(vals, degree = degree, argName = "x").y

    val out = new Array[Double](baseCondNames.length * degree)
    var i = 0
    while i < relevant.length do
      val idx = cellIndex(cats, relevant(i).levels)
      if idx >= 0 then
        var d = 0
        while d < degree do
          out(idx * degree + d) = pvals.data(i * degree + d)
          d += 1
      i += 1

    val colNames = (1 to degree).map(j => s"${name}_${j}").toVector
    val baseMat = Mat.unsafe(baseCondNames.length, degree, out)

    val (condNames, expanded) = expandWeights(baseMat, baseCondNames, nbasis)
    val (filtered, selected) = applyBasisFiltering(expanded, baseCondNames, nbasis, basis, basisWeights)

    ContrastWeights(
      name = name,
      condNames = condNames,
      contrastNames = colNames,
      weights = filtered,
      selectedCondNames = selected
    )

  def mask(
      term: EventTerm,
      name: String,
      maskA: Seq[Boolean],
      maskB: Option[Seq[Boolean]] = None,
      nbasis: Int = 1,
      basis: Option[Seq[Int]] = None,
      basisWeights: Option[Seq[Double]] = None
  ): ContrastWeights =
    val baseCondNames = term.conditions
    val a = maskA.toArray
    val b = maskB.map(_.toArray)
    val baseW = calculateMaskWeights(baseCondNames, a, b)
    val baseMat = Mat.unsafe(baseW.length, 1, baseW)
    val (condNames, expanded) = expandWeights(baseMat, baseCondNames, nbasis)
    val (filtered, selected) = applyBasisFiltering(expanded, baseCondNames, nbasis, basis, basisWeights)

    ContrastWeights(
      name = name,
      condNames = condNames,
      contrastNames = Vector(name),
      weights = filtered,
      selectedCondNames = selected
    )

  def oneAgainstAllContrasts(
      term: EventTerm,
      levels: Seq[String],
      facName: String,
      where: Predicate = _ => true,
      nbasis: Int = 1,
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
        nbasis = nbasis,
        basis = basis,
        basisWeights = basisWeights
      )
    }

  def pairwiseContrasts(
      term: EventTerm,
      levels: Seq[String],
      facName: String,
      where: Predicate = _ => true,
      nbasis: Int = 1,
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
          nbasis = nbasis,
          basis = basis,
          basisWeights = basisWeights
        )
        j += 1
      i += 1
    out.result()

  def slidingWindowContrasts(
      term: EventTerm,
      levels: Seq[String],
      facName: String,
      windowSize: Int = 2,
      where: Predicate = _ => true,
      nbasis: Int = 1,
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
        nbasis = nbasis,
        basis = basis,
        basisWeights = basisWeights
      )
      i += 1
    out.result()

  private[contrast] def categoricalOnly(term: EventTerm, contrastName: String): Vector[CategoricalEvent] =
    val cats = term.events.collect { case c: CategoricalEvent => c }
    if cats.isEmpty then throw new IllegalArgumentException(s"Contrast '$contrastName': term has no categorical cells")
    if term.events.exists(_.isContinuous) then
      throw new IllegalArgumentException(s"Contrast '$contrastName': categorical-only contrasts require categorical-only terms for now")
    cats

  private[contrast] def calculateMaskWeights(
      names: Vector[String],
      maskA: Array[Boolean],
      maskB: Option[Array[Boolean]]
  ): Array[Double] =
    require(maskA.length == names.length, "maskA length mismatch")
    maskB.foreach(m => require(m.length == names.length, "maskB length mismatch"))

    val nA = maskA.count(identity)
    val nB = maskB.map(_.count(identity)).getOrElse(0)

    if nA == 0 && (maskB.isEmpty || nB == 0) then
      throw new IllegalArgumentException("No conditions selected by the provided mask(s)")

    maskB.foreach { mb =>
      var i = 0
      while i < names.length do
        if maskA(i) && mb(i) then throw new IllegalArgumentException("Masks for group A and group B overlap")
        i += 1
    }

    val out = new Array[Double](names.length)
    if nA > 0 then
      val w = 1.0 / nA.toDouble
      var i = 0
      while i < names.length do
        if maskA(i) then out(i) = w
        i += 1

    maskB.foreach { mb =>
      if nB > 0 then
        val w = -1.0 / nB.toDouble
        var i = 0
        while i < names.length do
          if mb(i) then out(i) = w
          i += 1
    }

    out

  private[contrast] def cellIndex(cats: Vector[CategoricalEvent], levels: Vector[String]): Int =
    if levels.length != cats.length then return -1
    var idx = 0
    var mult = 1
    var j = 0
    while j < cats.length do
      val cat = cats(j)
      val code = cat.levels.indexOf(levels(j))
      if code < 0 then return -1
      idx += code * mult
      mult *= cat.levels.length
      j += 1
    idx

  private[contrast] def expandWeights(weights: Mat, baseCondNames: Vector[String], nbasis: Int): (Vector[String], Mat) =
    require(nbasis >= 1, "nbasis must be >= 1")
    if nbasis == 1 then (baseCondNames, weights)
    else
      val expandedNames = Names.addBasis(baseCondNames, nbasis)
      val nConds = baseCondNames.length
      val k = weights.cols
      val out = new Array[Double](expandedNames.length * k)
      var basis = 0
      while basis < nbasis do
        var cond = 0
        while cond < nConds do
          val dstRow = basis * nConds + cond
          System.arraycopy(weights.data, cond * k, out, dstRow * k, k)
          cond += 1
        basis += 1
      (expandedNames, Mat.unsafe(expandedNames.length, k, out))

  private[contrast] def applyBasisFiltering(
      expandedWeights: Mat,
      baseCondNames: Vector[String],
      nbasis: Int,
      basis: Option[Seq[Int]],
      basisWeights: Option[Seq[Double]]
  ): (Mat, Vector[String]) =
    require(nbasis >= 1, "nbasis must be >= 1")
    if nbasis == 1 then (expandedWeights, baseCondNames)
    else
      val nConds = baseCondNames.length
      val k = expandedWeights.cols
      require(expandedWeights.rows == nConds * nbasis, "expandedWeights row mismatch for nbasis")

      val selectedBases0: Option[Vector[Int]] =
        basis match
          case None => None
          case Some(bs) =>
            val b0 = bs.toVector
            require(b0.nonEmpty, "basis must be non-empty when provided")
            b0.foreach(b => require(b >= 1 && b <= nbasis, s"basis index out of range: $b"))
            Some(b0.distinct.sorted.map(_ - 1))

      val bw: Option[Array[Double]] =
        basisWeights match
          case None => None
          case Some(ws) =>
            val nSelected = selectedBases0.map(_.length).getOrElse(nbasis)
            require(ws.length == nSelected, s"basisWeights length (${ws.length}) must match selected bases ($nSelected)")
            val arr = ws.toArray
            require(arr.forall(_.isFinite), "basisWeights must be finite")
            require(arr.forall(_ >= 0.0), "basisWeights must be non-negative")
            val s = arr.sum
            require(s > 0.0, "basisWeights must sum to a positive value")
            var i = 0
            while i < arr.length do
              arr(i) = arr(i) / s
              i += 1
            Some(arr)

      val out = expandedWeights.data.clone

      var basisIdx = 0
      while basisIdx < nbasis do
        val keepBasis = selectedBases0.forall(_.contains(basisIdx))
        var cond = 0
        while cond < nConds do
          val row = basisIdx * nConds + cond
          if !keepBasis then
            var col = 0
            while col < k do
              out(row * k + col) = 0.0
              col += 1
          else
            bw.foreach { w =>
              val pos =
                selectedBases0 match
                  case None => basisIdx
                  case Some(bs) => bs.indexOf(basisIdx)
              if pos >= 0 then
                var col = 0
                while col < k do
                  out(row * k + col) = expandedWeights.data(row * k + col) * w(pos)
                  col += 1
              else
                var col = 0
                while col < k do
                  out(row * k + col) = 0.0
                  col += 1
            }
          cond += 1
        basisIdx += 1

      val selectedNames =
        selectedBases0 match
          case None =>
            Names.addBasis(baseCondNames, nbasis)
          case Some(bs) =>
            bs.flatMap(b => baseCondNames.map(_ + Names.basisSuffix(b + 1, nbasis)))

      (Mat.unsafe(expandedWeights.rows, expandedWeights.cols, out), selectedNames)
