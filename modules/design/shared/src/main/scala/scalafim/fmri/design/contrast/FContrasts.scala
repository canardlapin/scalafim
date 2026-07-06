package scalafim.fmri.design.contrast

import scalafim.fmri.design.Names
import scalafim.fmri.design.event.{CategoricalEvent, ConvolvedTerm, EventModel, EventTerm}
import scalafim.fmri.hrf.linalg.Mat

import scala.collection.immutable.VectorMap

object FContrasts:

  def forEventTerm(term: EventTerm, maxInter: Int = 4): VectorMap[String, ContrastWeights] =
    val cats = term.events.collect { case c: CategoricalEvent => c }
    require(cats.nonEmpty, s"No categorical variables found in term for FContrasts (termTag=${term.termTag.getOrElse("none")})")

    cats.foreach { c =>
      require(c.levels.length >= 2, s"Need at least 2 levels for contrasts (variable '${c.varName}')")
    }

    val baseCondNames = conditionTags(cats)
    val C = cats.map(c => ones(c.levels.length))
    val D = cats.map(c => contrSum(c.levels.length))

    val out = VectorMap.newBuilder[String, ContrastWeights]

    // Main effects
    var i = 0
    while i < cats.length do
      val mats = C.updated(i, D(i))
      val w = kronAll(mats)
      out += cats(i).varName -> ContrastWeights(
        name = cats(i).varName,
        condNames = baseCondNames,
        contrastNames = genericCols(w.cols),
        weights = w,
        selectedCondNames = baseCondNames
      )
      i += 1

    // Interactions (up to maxInter)
    if cats.length > 1 && cats.length <= maxInter then
      var k = 2
      while k <= cats.length do
        combinations(cats.length, k).foreach { ix =>
          val mats = ix.foldLeft(C) { (acc, j) => acc.updated(j, D(j)) }
          val w = kronAll(mats)
          val name = ix.map(cats(_).varName).mkString(":")
          out += name -> ContrastWeights(
            name = name,
            condNames = baseCondNames,
            contrastNames = genericCols(w.cols),
            weights = w,
            selectedCondNames = baseCondNames
          )
        }
        k += 1

    out.result()

  def forConvolvedTerm(term: ConvolvedTerm, maxInter: Int = 4): VectorMap[String, ContrastWeights] =
    val local = forEventTerm(term.term, maxInter = maxInter)
    if local.isEmpty then VectorMap.empty
    else
      val nb = term.hrf.nbasis

      val out = VectorMap.newBuilder[String, ContrastWeights]
      local.foreach { case (name, cw) =>
        val (baseCondNames, lifted) = liftToTermConditions(term.term, cw)
        val (_, expanded) = ContrastWeights.expandWeights(lifted, baseCondNames, nbasis = nb)
        val fullNames = Names.makeColumnNames(term.term.termTag, baseCondNames, nb)
        val full = ContrastWeights(
          name = name,
          condNames = fullNames,
          contrastNames = cw.contrastNames,
          weights = expanded,
          selectedCondNames = fullNames
        )
        out += name -> full.embedIn(term.columnNames)
      }
      out.result()

  private def liftToTermConditions(term: EventTerm, cw: ContrastWeights): (Vector[String], Mat) =
    val eventRows = expandGrid(term.events.map(_.conditionTokens).filter(_.nonEmpty))
    val allCondNames = eventRows.map(Names.makeCondTag)
    val catPositions =
      term.events.zipWithIndex.collect { case (_: CategoricalEvent, i) => i }.toVector
    val catIndex = cw.condNames.zipWithIndex.toMap

    val out = new Array[Double](eventRows.length * cw.weights.cols)
    var r = 0
    while r < eventRows.length do
      val catTag = Names.makeCondTag(catPositions.map(eventRows(r)))
      val srcRow = catIndex.getOrElse(catTag, throw new IllegalArgumentException(s"F contrast row '$catTag' not found in categorical condition names"))
      System.arraycopy(cw.weights.data, srcRow * cw.weights.cols, out, r * cw.weights.cols, cw.weights.cols)
      r += 1

    (allCondNames, Mat.unsafe(eventRows.length, cw.weights.cols, out))

  extension (model: EventModel)

    /** Flattened F-contrast weights embedded in the full design matrix columns.
      *
      * Keys follow the R convention: `"<termKey>#<effectName>"`.
      */
    def fContrasts(maxInter: Int = 4): VectorMap[String, ContrastWeights] =
      val out = VectorMap.newBuilder[String, ContrastWeights]
      var i = 0
      while i < model.terms.length do
        val (termKey, term) = model.terms(i)
        term match
          case ct: ConvolvedTerm =>
            val local = forConvolvedTerm(ct, maxInter = maxInter)
            local.foreach { case (name, cw) =>
              val key = s"$termKey#$name"
              out += key -> cw.embedIn(model.columnNames)
            }
          case _ => ()
        i += 1
      out.result()

  private def genericCols(n: Int): Vector[String] =
    (1 to n).iterator.map(j => s"c$j").toVector

  private def ones(n: Int): Mat =
    Mat.unsafe(n, 1, Array.fill(n)(1.0))

  private def contrSum(n: Int): Mat =
    require(n >= 2, "contrSum requires n >= 2")
    val cols = n - 1
    val out = new Array[Double](n * cols)
    var i = 0
    while i < cols do
      out(i * cols + i) = 1.0
      i += 1
    val last = n - 1
    i = 0
    while i < cols do
      out(last * cols + i) = -1.0
      i += 1
    Mat.unsafe(n, cols, out)

  /** Standard Kronecker product with row-major output; rows/cols of `b` vary fastest. */
  private def kron(a: Mat, b: Mat): Mat =
    val outRows = a.rows * b.rows
    val outCols = a.cols * b.cols
    val out = new Array[Double](outRows * outCols)

    var ra = 0
    while ra < a.rows do
      var rb = 0
      while rb < b.rows do
        val rOut = ra * b.rows + rb
        var ca = 0
        while ca < a.cols do
          val av = a.data(ra * a.cols + ca)
          var cb = 0
          while cb < b.cols do
            val cOut = ca * b.cols + cb
            out(rOut * outCols + cOut) = av * b.data(rb * b.cols + cb)
            cb += 1
          ca += 1
        rb += 1
      ra += 1
    Mat.unsafe(outRows, outCols, out)

  /** Kronecker product over factor matrices in event order (first factor varies fastest). */
  private def kronAll(mats: Vector[Mat]): Mat =
    require(mats.nonEmpty, "kronAll requires non-empty input")
    mats.reverse.reduceLeft(kron)

  private def conditionTags(cats: Vector[CategoricalEvent]): Vector[String] =
    val tokenLists = cats.map(_.conditionTokens)
    if tokenLists.isEmpty then Vector.empty
    else expandGrid(tokenLists).map(Names.makeCondTag)

  private def expandGrid(levelLists: Vector[Vector[String]]): Vector[Vector[String]] =
    levelLists.foldLeft(Vector(Vector.empty[String])) { (acc, levels) =>
      val out = Vector.newBuilder[Vector[String]]
      levels.foreach(lvl => acc.foreach(row => out += (row :+ lvl)))
      out.result()
    }

  private def combinations(n: Int, k: Int): Vector[Vector[Int]] =
    require(n >= 0, "n must be >= 0")
    require(k >= 0, "k must be >= 0")
    if k == 0 then Vector(Vector.empty)
    else if k > n then Vector.empty
    else
      val out = Vector.newBuilder[Vector[Int]]
      val buf = new Array[Int](k)

      def rec(start: Int, depth: Int): Unit =
        if depth == k then out += buf.toVector
        else
          var i = start
          while i <= n - (k - depth) do
            buf(depth) = i
            rec(i + 1, depth + 1)
            i += 1

      rec(0, 0)
      out.result()
