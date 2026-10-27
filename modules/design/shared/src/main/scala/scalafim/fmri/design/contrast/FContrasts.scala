package scalafim.fmri.design.contrast

import scalafim.fmri.design.Names
import scalafim.fmri.design.event.{CategoricalEvent, ConvolvedTerm, EventModel, EventTerm}
import scalafim.fmri.hrf.linalg.Mat

import scala.collection.immutable.VectorMap

object FContrasts:

  def forEventTerm(term: EventTerm, maxInter: Int = 4): VectorMap[String, ContrastWeights] =
    unsafe(forEventTermEither(term, maxInter = maxInter))

  def forEventTermEither(term: EventTerm, maxInter: Int = 4): Either[ContrastError, VectorMap[String, ContrastWeights]] =
    localForEventTerm(term, maxInter).map(values => VectorMap.from(values.iterator.map { case (key, weights) => key -> weights.toLegacy }))

  private def localForEventTerm(term: EventTerm, maxInter: Int): Either[ContrastError, VectorMap[String, LocalContrastWeights]] =
    val context = term.termTag.getOrElse("FContrasts")
    val cats = term.events.collect { case c: CategoricalEvent => c }
    if cats.isEmpty then return Left(ContrastError.NoCategoricalCells(context))

    cats.find(_.levels.length < 2) match
      case Some(c) => return Left(ContrastError.InsufficientLevels(context, c.varName, c.levels.length))
      case None    => ()

    val baseCondNames = conditionTags(cats)
    val C = cats.map(c => ones(c.levels.length))
    val D = cats.map(c => contrSum(c.levels.length))

    val out = VectorMap.newBuilder[String, LocalContrastWeights]

    // Main effects
    var i = 0
    while i < cats.length do
      val mats = C.updated(i, D(i))
      val w = kronAll(mats)
      out += cats(i).varName -> TypedContrastWeights[LocalContrastAxis](
        id = ContrastId.unsafe(cats(i).varName),
        rowNames = baseCondNames,
        contrastNames = genericCols(w.cols),
        weights = w,
        selectedRowNames = baseCondNames
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
          out += name -> TypedContrastWeights[LocalContrastAxis](
            id = ContrastId.unsafe(name),
            rowNames = baseCondNames,
            contrastNames = genericCols(w.cols),
            weights = w,
            selectedRowNames = baseCondNames
          )
        }
        k += 1

    Right(out.result())

  def forConvolvedTerm(term: ConvolvedTerm, maxInter: Int = 4): VectorMap[String, ContrastWeights] =
    unsafe(forConvolvedTermEither(term, maxInter = maxInter))

  def forConvolvedTermEither(term: ConvolvedTerm, maxInter: Int = 4): Either[ContrastError, VectorMap[String, ContrastWeights]] =
    compiledForConvolvedTerm(term, maxInter).map(values => VectorMap.from(values.iterator.map { case (key, c) => key -> c.toLegacy }))

  def compiledForConvolvedTerm(term: ConvolvedTerm, maxInter: Int = 4): Either[ContrastError, VectorMap[String, CompiledContrast]] =
    if term.hasHeterogeneousBasis then
      Left(ContrastError.HeterogeneousBasisUnsupported(term.term.termTag.getOrElse("FContrasts"), term.basisWidths))
    else
      for
        local <- localForEventTerm(term.term, maxInter)
        out <- local.foldLeft(Right(VectorMap.empty): Either[ContrastError, VectorMap[String, CompiledContrast]]) {
          case (acc, (name, cw)) =>
            for
              out0 <- acc
              lifted <- liftLocalToTermConditions(term.term, cw)
            yield
              val (baseCondNames, liftedWeights) = lifted
              val (_, expanded) = ContrastWeights.expandWeights(liftedWeights, baseCondNames, nbasis = term.hrf.nbasis)
              val fullNames = Names.makeColumnNames(term.term.termTag, baseCondNames, term.hrf.nbasis)
              val full = TypedContrastWeights[LocalContrastAxis](
                id = ContrastId.unsafe(name), rowNames = fullNames,
                contrastNames = cw.contrastNames, weights = expanded, selectedRowNames = fullNames)
              out0.updated(name, CompiledContrast(ContrastId.unsafe(name), Some(EffectId.unsafe(name)),
                ContrastSource.GeneratedF, full.embedIn(term.columnNames)))
        }
      yield out

  private[design] def liftToTermConditionsEither(term: EventTerm, cw: ContrastWeights): Either[ContrastError, (Vector[String], Mat)] =
    liftLocalToTermConditions(term, TypedContrastWeights.fromLegacyLocal(cw))

  private def liftLocalToTermConditions(term: EventTerm, cw: LocalContrastWeights): Either[ContrastError, (Vector[String], Mat)] =
    val eventRows = expandGrid(term.events.map(_.conditionTokens).filter(_.nonEmpty))
    val allCondNames = eventRows.map(Names.makeCondTag)
    val catPositions =
      term.events.zipWithIndex.collect { case (_: CategoricalEvent, i) => i }.toVector
    val catIndex = cw.rowNames.zipWithIndex.toMap

    val out = new Array[Double](eventRows.length * cw.weights.cols)
    var r = 0
    while r < eventRows.length do
      val catTag = Names.makeCondTag(catPositions.map(eventRows(r)))
      val srcRow =
        catIndex.get(catTag) match
          case Some(i) => i
          case None    => return Left(ContrastError.MissingConditionRow(cw.id.value, catTag, cw.rowNames))
      System.arraycopy(cw.weights.data, srcRow * cw.weights.cols, out, r * cw.weights.cols, cw.weights.cols)
      r += 1

    Right((allCondNames, Mat.unsafe(eventRows.length, cw.weights.cols, out)))

  extension (model: EventModel)

    /** Flattened F-contrast weights embedded in the full design matrix columns.
      *
      * Keys follow the R convention: `"<termKey>#<effectName>"`.
      */
    def fContrasts(maxInter: Int = 4): VectorMap[String, ContrastWeights] =
      unsafe(fContrastsEither(maxInter = maxInter))

    def fContrastsEither(maxInter: Int = 4): Either[ContrastError, VectorMap[String, ContrastWeights]] =
      compiledFContrasts(maxInter).map(values => VectorMap.from(values.iterator.map { case (key, c) => key -> c.toLegacy }))

    def compiledFContrasts(maxInter: Int = 4): Either[ContrastError, VectorMap[String, CompiledContrast]] =
      model.terms.foldLeft(Right(VectorMap.empty): Either[ContrastError, VectorMap[String, CompiledContrast]]) {
        case (acc, (termKey, term)) =>
          term match
            case ct: ConvolvedTerm =>
              for
                out <- acc
                local <- FContrasts.compiledForConvolvedTerm(ct, maxInter = maxInter)
              yield
                local.foldLeft(out) { case (out0, (name, compiled)) =>
                  val key = s"$termKey#$name"
                  out0.updated(key, compiled.copy(id = ContrastId.unsafe(key),
                    weights = compiled.weights.embedIn(model.columnNames)))
                }
            case _ => acc
      }

  private def unsafe[A](result: Either[ContrastError, A]): A =
    result.fold(error => throw new IllegalArgumentException(error.message), identity)

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
