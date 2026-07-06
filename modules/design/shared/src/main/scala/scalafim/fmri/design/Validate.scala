package scalafim.fmri.design

import scalafim.fmri.design.Residualize.HasDesignMatrix
import scalafim.fmri.design.contrast.ContrastWeights
import scalafim.fmri.design.linalg.QrDecomposition
import scalafim.fmri.hrf.linalg.Mat

object Validate:

  enum ContrastType:
    case T, F

  final case class ContrastValidation(
      name: String,
      contrastType: ContrastType,
      estimable: Boolean,
      sumToZero: Boolean,
      orthogonalToIntercept: Boolean,
      fullRank: Option[Boolean],
      nonzeroWeights: Int
  )

  final case class CollinearityPair(regressor1: String, regressor2: String, r: Double)
  final case class CollinearityResult(ok: Boolean, pairs: Vector[CollinearityPair])

  val DefaultTol: Double = 1e-8
  val DefaultRankTol: Double = 1e-7
  val DefaultCollinearityThreshold: Double = 0.9

  def validateContrasts(
      design: Mat,
      columnNames: Vector[String],
      weights: Vector[Double]
  ): Vector[ContrastValidation] =
    validateContrasts(
      design = design,
      columnNames = columnNames,
      weights = Mat.unsafe(weights.length, 1, weights.toArray)
    )

  def validateContrasts(
      design: Mat,
      columnNames: Vector[String],
      weights: Vector[Double],
      name: String
  ): Vector[ContrastValidation] =
    validateContrasts(
      design = design,
      columnNames = columnNames,
      weights = Mat.unsafe(weights.length, 1, weights.toArray),
      name = name
    )

  def validateContrasts(
      design: Mat,
      columnNames: Vector[String],
      weights: Mat,
      name: String = "contrast",
      weightRowNames: Option[Vector[String]] = None,
      tol: Double = DefaultTol,
      rankTol: Double = DefaultRankTol
  ): Vector[ContrastValidation] =
    require(columnNames.length == design.cols, "validateContrasts: columnNames length must match design.cols")
    require(tol >= 0.0 && rankTol >= 0.0, "validateContrasts: tolerances must be non-negative")

    val wAligned =
      if weights.rows == design.cols then weights
      else
        weightRowNames match
          case None =>
            throw new IllegalArgumentException(
              s"validateContrasts: weights.rows=${weights.rows} but design.cols=${design.cols}; provide weightRowNames for name-based alignment"
            )
          case Some(rownames) =>
            alignWeightsToDesign(columnNames, weights, rownames)

    val contrastNames = defaultContrastNames(name, wAligned.cols)
    validateAlignedWeights(design, columnNames, wAligned, contrastNames, tol = tol, rankTol = rankTol).sortBy(_.name)

  def validateContrasts(
      design: Mat,
      columnNames: Vector[String],
      contrast: ContrastWeights
  ): Vector[ContrastValidation] =
    validateContrasts(design, columnNames, contrast, tol = DefaultTol, rankTol = DefaultRankTol)

  def validateContrasts(
      design: Mat,
      columnNames: Vector[String],
      contrast: ContrastWeights,
      tol: Double,
      rankTol: Double
  ): Vector[ContrastValidation] =
    require(columnNames.length == design.cols, "validateContrasts: columnNames length must match design.cols")
    require(tol >= 0.0 && rankTol >= 0.0, "validateContrasts: tolerances must be non-negative")

    val aligned = contrast.embedIn(columnNames)
    val names =
      if aligned.contrastNames.length == aligned.weights.cols then aligned.contrastNames
      else defaultContrastNames(aligned.name, aligned.weights.cols)

    validateAlignedWeights(design, columnNames, aligned.weights, names, tol = tol, rankTol = rankTol).sortBy(_.name)

  def validateContrasts(
      design: Mat,
      columnNames: Vector[String],
      contrasts: IterableOnce[ContrastWeights]
  ): Vector[ContrastValidation] =
    validateContrasts(design, columnNames, contrasts, tol = DefaultTol, rankTol = DefaultRankTol)

  def validateContrasts(
      design: Mat,
      columnNames: Vector[String],
      contrasts: IterableOnce[ContrastWeights],
      tol: Double,
      rankTol: Double
  ): Vector[ContrastValidation] =
    contrasts.iterator.flatMap(cw => validateContrasts(design, columnNames, cw, tol, rankTol)).toVector.sortBy(_.name)

  def checkCollinearity(
      design: Mat,
      columnNames: Vector[String],
      threshold: Double = DefaultCollinearityThreshold
  ): CollinearityResult =
    require(columnNames.length == design.cols, "checkCollinearity: columnNames length must match design.cols")
    require(threshold >= 0.0 && threshold <= 1.0, "checkCollinearity: threshold must be in [0, 1]")

    val keep0 = (0 until design.cols).filterNot(i => isInterceptLike(columnNames(i))).toVector
    if keep0.isEmpty then return CollinearityResult(ok = true, pairs = Vector.empty)

    if design.rows < 2 then return CollinearityResult(ok = true, pairs = Vector.empty)

    val means0 = new Array[Double](keep0.length)
    val varSum0 = new Array[Double](keep0.length)

    var i = 0
    while i < keep0.length do
      val col = keep0(i)
      var sum = 0.0
      var r = 0
      while r < design.rows do
        sum += design.data(r * design.cols + col)
        r += 1
      val mean = sum / design.rows.toDouble
      means0(i) = mean

      var ss = 0.0
      r = 0
      while r < design.rows do
        val d = design.data(r * design.cols + col) - mean
        ss += d * d
        r += 1
      varSum0(i) = ss
      i += 1

    val keep = Vector.newBuilder[Int]
    val means = Vector.newBuilder[Double]
    val varSum = Vector.newBuilder[Double]
    i = 0
    while i < keep0.length do
      val ss = varSum0(i)
      if isFiniteDouble(ss) && ss > 0.0 then
        keep += keep0(i)
        means += means0(i)
        varSum += ss
      i += 1

    val keepCols = keep.result()
    val keepMeans = means.result()
    val keepVarSum = varSum.result()

    if keepCols.length < 2 then return CollinearityResult(ok = true, pairs = Vector.empty)

    val pairs = Vector.newBuilder[CollinearityPair]
    var a = 0
    while a < keepCols.length - 1 do
      var b = a + 1
      while b < keepCols.length do
        val colA = keepCols(a)
        val colB = keepCols(b)
        val meanA = keepMeans(a)
        val meanB = keepMeans(b)
        val denom = math.sqrt(keepVarSum(a) * keepVarSum(b))

        var cov = 0.0
        var r = 0
        while r < design.rows do
          cov +=
            (design.data(r * design.cols + colA) - meanA) *
              (design.data(r * design.cols + colB) - meanB)
          r += 1

        val corr = cov / denom
        if isFiniteDouble(corr) && math.abs(corr) > threshold then
          pairs += CollinearityPair(columnNames(colA), columnNames(colB), corr)

        b += 1
      a += 1

    val out = pairs.result()
    CollinearityResult(ok = out.isEmpty, pairs = out)

  extension [A](model: A)(using ev: HasDesignMatrix[A])
    def validateContrasts(weights: Mat): Vector[ContrastValidation] =
      Validate.validateContrasts(ev.designMatrix(model), ev.columnNames(model), weights)

    def validateContrasts(
        weights: Mat,
        name: String,
        weightRowNames: Option[Vector[String]],
        tol: Double,
        rankTol: Double
    ): Vector[ContrastValidation] =
      Validate.validateContrasts(
        design = ev.designMatrix(model),
        columnNames = ev.columnNames(model),
        weights = weights,
        name = name,
        weightRowNames = weightRowNames,
        tol = tol,
        rankTol = rankTol
      )

    def validateContrasts(contrast: ContrastWeights): Vector[ContrastValidation] =
      Validate.validateContrasts(ev.designMatrix(model), ev.columnNames(model), contrast)

    def validateContrasts(contrast: ContrastWeights, tol: Double, rankTol: Double): Vector[ContrastValidation] =
      Validate.validateContrasts(ev.designMatrix(model), ev.columnNames(model), contrast, tol, rankTol)

    def validateContrasts(contrasts: IterableOnce[ContrastWeights]): Vector[ContrastValidation] =
      Validate.validateContrasts(ev.designMatrix(model), ev.columnNames(model), contrasts)

    def validateContrasts(contrasts: IterableOnce[ContrastWeights], tol: Double, rankTol: Double): Vector[ContrastValidation] =
      Validate.validateContrasts(ev.designMatrix(model), ev.columnNames(model), contrasts, tol, rankTol)

    def checkCollinearity: CollinearityResult =
      Validate.checkCollinearity(ev.designMatrix(model), ev.columnNames(model))

    def checkCollinearity(threshold: Double): CollinearityResult =
      Validate.checkCollinearity(ev.designMatrix(model), ev.columnNames(model), threshold = threshold)

  private def defaultContrastNames(base: String, cols: Int): Vector[String] =
    if cols <= 1 then Vector(base) else (1 to cols).map(j => s"$base#$j").toVector

  private def validateAlignedWeights(
      design: Mat,
      columnNames: Vector[String],
      weights: Mat,
      contrastNames: Vector[String],
      tol: Double,
      rankTol: Double
  ): Vector[ContrastValidation] =
    require(weights.rows == design.cols, "internal: aligned weights row mismatch")
    require(contrastNames.length == weights.cols, "internal: contrastNames length mismatch")

    val rankX = matrixRank(design, tol = rankTol)
    val interceptIx = interceptIndices(columnNames)
    val ctype = if weights.cols > 1 then ContrastType.F else ContrastType.T

    val fullRank =
      if ctype == ContrastType.F then Some(matrixRank(weights, tol = rankTol) == weights.cols) else None

    val out = Vector.newBuilder[ContrastValidation]

    var j = 0
    while j < weights.cols do
      val cvec = columnAsArray(weights, j)

      var sum = 0.0
      var nonzero = 0
      var orth = true

      var i = 0
      while i < cvec.length do
        val v = cvec(i)
        sum += v
        if math.abs(v) > tol then nonzero += 1
        if orth && interceptIx.contains(i) && math.abs(v) >= tol then orth = false
        i += 1

      val estimable = isEstimableVec(design, rankX = rankX, cvec = cvec, tol = rankTol)

      out += ContrastValidation(
        name = contrastNames(j),
        contrastType = ctype,
        estimable = estimable,
        sumToZero = math.abs(sum) < tol,
        orthogonalToIntercept = orth,
        fullRank = fullRank,
        nonzeroWeights = nonzero
      )
      j += 1

    out.result()

  private def matrixRank(m: Mat, tol: Double): Int =
    QrDecomposition.decompose(m.data, rows = m.rows, cols = m.cols, pivoting = true, tol = tol).rank

  private def isEstimableVec(design: Mat, rankX: Int, cvec: Array[Double], tol: Double): Boolean =
    if cvec.length != design.cols then false
    else
      val aug = new Array[Double]((design.rows + 1) * design.cols)
      System.arraycopy(design.data, 0, aug, 0, design.data.length)
      System.arraycopy(cvec, 0, aug, design.data.length, design.cols)
      val rankAug = QrDecomposition.decompose(aug, rows = design.rows + 1, cols = design.cols, pivoting = true, tol = tol).rank
      rankAug == rankX

  private def columnAsArray(m: Mat, c: Int): Array[Double] =
    require(c >= 0 && c < m.cols, "columnAsArray: column index out of range")
    val out = new Array[Double](m.rows)
    var r = 0
    while r < m.rows do
      out(r) = m.data(r * m.cols + c)
      r += 1
    out

  private def alignWeightsToDesign(designColNames: Vector[String], weights: Mat, rowNames: Vector[String]): Mat =
    require(rowNames.length == weights.rows, "alignWeightsToDesign: rowNames length must match weights.rows")
    require(rowNames.distinct.length == rowNames.length, "alignWeightsToDesign: rowNames must be unique")

    val index = designColNames.zipWithIndex.toMap
    val out = new Array[Double](designColNames.length * weights.cols)

    var matched = 0
    var rIn = 0
    while rIn < rowNames.length do
      index.get(rowNames(rIn)).foreach { rOut =>
        System.arraycopy(weights.data, rIn * weights.cols, out, rOut * weights.cols, weights.cols)
        matched += 1
      }
      rIn += 1

    if matched == 0 then
      throw new IllegalArgumentException("alignWeightsToDesign: no rowNames matched any design column names")

    Mat.unsafe(designColNames.length, weights.cols, out)

  private def interceptIndices(columnNames: Vector[String]): Set[Int] =
    columnNames.iterator.zipWithIndex.collect { case (n, i) if isInterceptLike(n) => i }.toSet

  private def isInterceptLike(name: String): Boolean =
    name == "(Intercept)" ||
      name == "Intercept" ||
      name == "constant" ||
      name == "const" ||
      name.startsWith("constant_") ||
      name.startsWith("const_") ||
      name.startsWith("base_constant")

  private def isFiniteDouble(x: Double): Boolean =
    !x.isNaN && !x.isInfinity
