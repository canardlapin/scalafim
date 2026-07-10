package scalafim.fmri.design.basis

import scalafim.fmri.design.Names
import scalafim.fmri.hrf.linalg.Mat

trait ParametricBasis:
  def argName: String
  def name: String
  def basisClass: String
  def y: Mat
  def columns: Vector[String]
  final def nbasis: Int = y.cols
  def registryKeys: Vector[String] = Vector(basisClass)

  def subset(mask: Vector[Boolean]): ParametricBasis

enum BasisDegeneracyKind:
  case AllNonFinite, RepairedNonFinite, ZeroVariance

final case class BasisDiagnostic(
    kind: BasisDegeneracyKind,
    basisClass: String,
    column: String,
    message: String,
    group: Option[String] = None
)

enum BasisDegeneracyPolicy:
  case Compatible, Report, Strict

enum BasisFitError:
  case Degenerate(diagnostics: Vector[BasisDiagnostic])

  def message: String =
    this match
      case Degenerate(diagnostics) =>
        diagnostics.map(_.message).mkString("; ")

final case class BasisFit[+A <: ParametricBasis](basis: A, diagnostics: Vector[BasisDiagnostic])

object ParametricBasis:
  private val FallbackScale = 1e-6

  private final case class ScaleMoments(center: Double, rawScale: Double)

  private final case class ScaledColumn(
      center: Double,
      scale: Double,
      values: Array[Double],
      diagnostics: Vector[BasisDiagnostic]
  )

  private def finishFit[A <: ParametricBasis](
      basis: A,
      diagnostics: Vector[BasisDiagnostic],
      policy: BasisDegeneracyPolicy
  ): Either[BasisFitError, BasisFit[A]] =
    val reported =
      policy match
        case BasisDegeneracyPolicy.Compatible => Vector.empty
        case BasisDegeneracyPolicy.Report     => diagnostics
        case BasisDegeneracyPolicy.Strict     => diagnostics

    policy match
      case BasisDegeneracyPolicy.Strict if diagnostics.nonEmpty =>
        Left(BasisFitError.Degenerate(diagnostics))
      case _ =>
        Right(BasisFit(basis, reported))

  private def meanSd(clean: Vector[Double]): ScaleMoments =
    val mean = if clean.isEmpty then Double.NaN else clean.sum / clean.length.toDouble
    val sd =
      if clean.length <= 1 then Double.NaN
      else
        val v = clean.map(a => (a - mean) * (a - mean)).sum / (clean.length - 1).toDouble
        math.sqrt(v)
    ScaleMoments(mean, sd)

  private def repairedScale(
      xs: Vector[Double],
      clean: Vector[Double],
      moments: ScaleMoments,
      basisClass: String,
      column: String,
      scaleName: String,
      group: Option[String] = None
  ): ScaledColumn =
    val scale =
      if moments.rawScale.isFinite && moments.rawScale != 0.0 then moments.rawScale
      else FallbackScale
    val out = new Array[Double](xs.length)
    var i = 0
    while i < xs.length do
      val v = (xs(i) - moments.center) / scale
      out(i) = if v.isFinite then v else 0.0
      i += 1

    ScaledColumn(
      center = moments.center,
      scale = scale,
      values = out,
      diagnostics = scaleDiagnostics(xs, clean, moments.rawScale, basisClass, column, scaleName, group)
    )

  private def scaleDiagnostics(
      xs: Vector[Double],
      clean: Vector[Double],
      rawScale: Double,
      basisClass: String,
      column: String,
      scaleName: String,
      group: Option[String]
  ): Vector[BasisDiagnostic] =
    val out = Vector.newBuilder[BasisDiagnostic]
    val groupText = group.map(g => s" for group '$g'").getOrElse("")
    val prefix = s"$basisClass basis column '$column'$groupText"

    if xs.nonEmpty && clean.isEmpty then
      out += BasisDiagnostic(
        BasisDegeneracyKind.AllNonFinite,
        basisClass,
        column,
        s"$prefix has all non-finite values; repaired output uses 0.0",
        group
      )
    else if xs.exists(!_.isFinite) then
      out += BasisDiagnostic(
        BasisDegeneracyKind.RepairedNonFinite,
        basisClass,
        column,
        s"$prefix has non-finite values; repaired affected rows to 0.0",
        group
      )

    if clean.nonEmpty && (!rawScale.isFinite || rawScale == 0.0) then
      out += BasisDiagnostic(
        BasisDegeneracyKind.ZeroVariance,
        basisClass,
        column,
        s"$prefix has zero variance; using fallback $scaleName $FallbackScale",
        group
      )

    out.result()

  final case class Ident(y: Mat, varNames: Vector[String]) extends ParametricBasis:
    val argName: String = varNames.mkString("_")
    val name: String = argName
    val basisClass: String = "Ident"
    val columns: Vector[String] = varNames.map(Names.continuousToken)
    def subset(mask: Vector[Boolean]): ParametricBasis =
      val keep = maskToIndices(mask, y.rows)
      val out = new Array[Double](keep.length * y.cols)
      var rOut = 0
      while rOut < keep.length do
        val rIn = keep(rOut)
        System.arraycopy(y.data, rIn * y.cols, out, rOut * y.cols, y.cols)
        rOut += 1
      Ident(Mat.unsafe(keep.length, y.cols, out), varNames)

  final case class PolyCoefs(alpha: Vector[Double], norm2: Vector[Double])

  final case class Poly(x: Vector[Double], degree: Int, argName: String, y: Mat, coefs: PolyCoefs) extends ParametricBasis:
    val name: String = s"poly_${argName}"
    val basisClass: String = "Poly"
    val columns: Vector[String] =
      Names.zeroPad(1 to degree, degree).map(Names.continuousToken)

    def subset(mask: Vector[Boolean]): ParametricBasis =
      val keep = maskToIndices(mask, x.length)
      val x2 = keep.map(x).toVector
      val out = new Array[Double](keep.length * y.cols)
      var rOut = 0
      while rOut < keep.length do
        val rIn = keep(rOut)
        System.arraycopy(y.data, rIn * y.cols, out, rOut * y.cols, y.cols)
        rOut += 1
      Poly(x2, degree, argName, Mat.unsafe(keep.length, y.cols, out), coefs)

  object Poly:
    def fit(x: Seq[Double], degree: Int, argName: String): Poly =
      require(degree >= 1, "'degree' must be at least 1")
      val xs = x.toVector
      require(xs.forall(_.isFinite), "missing values are not allowed in 'poly'")
      require(degree < xs.distinct.length, "'degree' must be less than number of unique points")

      val n = xs.length
      val xbar = xs.sum / n.toDouble
      val xc = xs.map(_ - xbar)

      // X = outer(xc, 0:degree, `^`)
      val p = degree + 1
      val X = new Array[Double](n * p)
      var i = 0
      while i < n do
        var p = 0
        var v = 1.0
        while p <= degree do
          X(i * (degree + 1) + p) = v
          v *= xc(i)
          p += 1
        i += 1

      val (q, diagR) = householderQrReduced(X, rows = n, cols = degree + 1)

      // Z = Q %*% diag(diag(R)) (R parity: qr.qy(QR, diag(diagR)))
      val Z = new Array[Double](n * (degree + 1))
      var col = 0
      while col <= degree do
        val d = diagR(col)
        i = 0
        while i < n do
          Z(i * (degree + 1) + col) = q(i * (degree + 1) + col) * d
          i += 1
        col += 1

      val norm2ColArr = Array.fill(degree + 1)(0.0)
      val numArr = Array.fill(degree + 1)(0.0)
      i = 0
      while i < n do
        val xci = xc(i)
        var j = 0
        while j <= degree do
          val z = Z(i * (degree + 1) + j)
          val z2 = z * z
          norm2ColArr(j) += z2
          numArr(j) += xci * z2
          j += 1
        i += 1

      val alphaAll = Array.ofDim[Double](degree + 1)
      var j = 0
      while j <= degree do
        alphaAll(j) = numArr(j) / norm2ColArr(j) + xbar
        j += 1
      val alpha = alphaAll.take(degree).toVector
      val norm2Stored = (Vector(1.0) ++ norm2ColArr.toVector)

      val out = new Array[Double](n * degree)
      i = 0
      while i < n do
        var j = 1 // drop intercept column
        while j <= degree do
          val z = Z(i * (degree + 1) + j) / math.sqrt(norm2ColArr(j))
          out(i * degree + (j - 1)) = z
          j += 1
        i += 1

      Poly(
        x = xs,
        degree = degree,
        argName = argName,
        y = Mat.unsafe(n, degree, out),
        coefs = PolyCoefs(alpha = alpha, norm2 = norm2Stored)
      )

    private final case class Householder(v: Array[Double], beta: Double)

    private def householderQrReduced(a0: Array[Double], rows: Int, cols: Int): (Array[Double], Array[Double]) =
      require(rows >= 0 && cols >= 0, "rows/cols must be non-negative")
      require(a0.length == rows * cols, "data length mismatch")
      require(rows >= cols, "reduced QR expects rows >= cols")

      val a = a0.clone
      val diagR = new Array[Double](cols)
      val reflectors = new Array[Householder](cols)

      var k = 0
      while k < cols do
        // x = a[k:, k]
        var norm2 = 0.0
        var r = k
        while r < rows do
          val v = a(r * cols + k)
          norm2 += v * v
          r += 1

        val norm = math.sqrt(norm2)
        if norm == 0.0 then
          reflectors(k) = Householder(Array.emptyDoubleArray, 0.0)
          diagR(k) = 0.0
        else
          val x0 = a(k * cols + k)
          val alpha = if x0 >= 0.0 then -norm else norm

          val len = rows - k
          val v = new Array[Double](len)
          v(0) = x0 - alpha
          r = k + 1
          var i = 1
          while r < rows do
            v(i) = a(r * cols + k)
            r += 1
            i += 1

          var vTv = 0.0
          i = 0
          while i < len do
            vTv += v(i) * v(i)
            i += 1
          val beta = if vTv == 0.0 then 0.0 else 2.0 / vTv
          reflectors(k) = Householder(v, beta)

          // Apply Hk to a[k:, k:].
          var c = k
          while c < cols do
            var dot = 0.0
            i = 0
            r = k
            while r < rows do
              dot += v(i) * a(r * cols + c)
              i += 1
              r += 1
            val s = beta * dot
            i = 0
            r = k
            while r < rows do
              a(r * cols + c) -= s * v(i)
              i += 1
              r += 1
            c += 1

          diagR(k) = a(k * cols + k)

        k += 1

      // Build reduced Q by applying reflectors to I(n,p).
      val q = new Array[Double](rows * cols)
      var rr = 0
      while rr < rows do
        var cc = 0
        while cc < cols do
          q(rr * cols + cc) = if rr == cc then 1.0 else 0.0
          cc += 1
        rr += 1

      // To form Q (not Qᵀ), apply the stored Householder reflectors in reverse.
      k = cols - 1
      while k >= 0 do
        val Householder(v, beta) = reflectors(k)
        if beta != 0.0 then
          val len = v.length
          var cc = 0
          while cc < cols do
            var dot = 0.0
            var i = 0
            while i < len do
              val row = k + i
              dot += v(i) * q(row * cols + cc)
              i += 1
            val s = beta * dot
            i = 0
            while i < len do
              val row = k + i
              q(row * cols + cc) -= s * v(i)
              i += 1
            cc += 1
        k -= 1

      (q, diagR)

    def predict(coefs: PolyCoefs, newData: Seq[Double], degree: Int, colNamePrefix: String): Mat =
      val x = newData.toVector
      val n = x.length
      if n == 0 then return Mat.zeros(0, degree)

      val alpha = coefs.alpha
      val norm2 = coefs.norm2
      require(alpha.length == degree, "coefs.alpha length mismatch")
      require(norm2.length == degree + 2, "coefs.norm2 length mismatch")

      val Z = Array.fill(n * (degree + 1))(1.0)
      // column 2 (1-based) => index 1 (0-based)
      var i = 0
      while i < n do
        Z(i * (degree + 1) + 1) = x(i) - alpha(0)
        i += 1

      var d = 2
      while d <= degree do
        val ratio = norm2(d) / norm2(d - 1)
        i = 0
        while i < n do
          val idx = i * (degree + 1)
          val prev = Z(idx + (d - 1))
          val prevPrev = Z(idx + (d - 2))
          Z(idx + d) = (x(i) - alpha(d - 1)) * prev - ratio * prevPrev
          i += 1
        d += 1

      val out = new Array[Double](n * degree)
      i = 0
      while i < n do
        var j = 1
        while j <= degree do
          val denom = math.sqrt(norm2(j + 1))
          out(i * degree + (j - 1)) = Z(i * (degree + 1) + j) / denom
          j += 1
        i += 1
      Mat.unsafe(n, degree, out)

  final case class Standardized(x: Vector[Double], argName: String, mean: Double, sd: Double, y: Mat) extends ParametricBasis:
    val name: String = s"std_${argName}"
    val basisClass: String = "Standardized"
    val columns: Vector[String] = Vector(Names.continuousToken(name))
    def subset(mask: Vector[Boolean]): ParametricBasis =
      val keep = maskToIndices(mask, x.length)
      val x2 = keep.map(x).toVector
      val out = keep.map(i => y.data(i)).toArray
      Standardized(x2, argName, mean, sd, Mat.unsafe(out.length, 1, out))

  object Standardized:
    def fit(x: Seq[Double], argName: String): Standardized =
      fitWithDiagnostics(x, argName, BasisDegeneracyPolicy.Compatible)
        .fold(err => throw new IllegalArgumentException(err.message), _.basis)

    def fitWithDiagnostics(
        x: Seq[Double],
        argName: String,
        policy: BasisDegeneracyPolicy = BasisDegeneracyPolicy.Report
    ): Either[BasisFitError, BasisFit[Standardized]] =
      val xs = x.toVector
      val clean = xs.filter(_.isFinite)
      val column = Names.continuousToken(s"std_${argName}")
      val scaled = repairedScale(xs, clean, meanSd(clean), "Standardized", column, "sd")
      val basis = Standardized(xs, argName, scaled.center, scaled.scale, Mat.unsafe(xs.length, 1, scaled.values))
      finishFit(basis, scaled.diagnostics, policy)

  final case class Scale(x: Vector[Double], argName: String, mean: Double, sd: Double, y: Mat) extends ParametricBasis:
    val name: String = s"z_${argName}"
    val basisClass: String = "Scale"
    val columns: Vector[String] = Vector(Names.continuousToken(name))
    def subset(mask: Vector[Boolean]): ParametricBasis =
      val keep = maskToIndices(mask, x.length)
      val x2 = keep.map(x).toVector
      val out = keep.map(i => y.data(i)).toArray
      Scale(x2, argName, mean, sd, Mat.unsafe(out.length, 1, out))

  object Scale:
    def fit(x: Seq[Double], argName: String): Scale =
      fitWithDiagnostics(x, argName, BasisDegeneracyPolicy.Compatible)
        .fold(err => throw new IllegalArgumentException(err.message), _.basis)

    def fitWithDiagnostics(
        x: Seq[Double],
        argName: String,
        policy: BasisDegeneracyPolicy = BasisDegeneracyPolicy.Report
    ): Either[BasisFitError, BasisFit[Scale]] =
      val xs = x.toVector
      val clean = xs.filter(_.isFinite)
      val column = Names.continuousToken(s"z_${argName}")
      val scaled = repairedScale(xs, clean, meanSd(clean), "Scale", column, "sd")
      val basis = Scale(xs, argName, scaled.center, scaled.scale, Mat.unsafe(xs.length, 1, scaled.values))
      finishFit(basis, scaled.diagnostics, policy)

  final case class ScaleWithin(
      x: Vector[Double],
      group: Vector[String],
      argName: String,
      groupName: String,
      means: Map[String, Double],
      sds: Map[String, Double],
      y: Mat
  ) extends ParametricBasis:
    val name: String = s"z_${argName}_by_${groupName}"
    val basisClass: String = "ScaleWithin"
    val columns: Vector[String] = Vector(Names.continuousToken(name))
    def subset(mask: Vector[Boolean]): ParametricBasis =
      val keep = maskToIndices(mask, x.length)
      val x2 = keep.map(x).toVector
      val g2 = keep.map(group).toVector
      val out = keep.map(i => y.data(i)).toArray
      ScaleWithin(x2, g2, argName, groupName, means, sds, Mat.unsafe(out.length, 1, out))

  object ScaleWithin:
    def fit(x: Seq[Double], group: Seq[String], argName: String, groupName: String): ScaleWithin =
      fitWithDiagnostics(x, group, argName, groupName, BasisDegeneracyPolicy.Compatible)
        .fold(err => throw new IllegalArgumentException(err.message), _.basis)

    def fitWithDiagnostics(
        x: Seq[Double],
        group: Seq[String],
        argName: String,
        groupName: String,
        policy: BasisDegeneracyPolicy = BasisDegeneracyPolicy.Report
    ): Either[BasisFitError, BasisFit[ScaleWithin]] =
      val xs = x.toVector
      val gs = group.toVector
      require(xs.length == gs.length, "length(x) must equal length(group)")

      val byGroup: Map[String, Vector[Int]] =
        gs.indices.groupBy(gs).view.mapValues(_.toVector).toMap

      val column = Names.continuousToken(s"z_${argName}_by_${groupName}")
      val diagnostics = Vector.newBuilder[BasisDiagnostic]

      val groupScaled: Map[String, ScaledColumn] =
        byGroup.view.mapValues { idxs =>
          val clean = idxs.iterator.map(xs).filter(_.isFinite).toVector
          val scaled = repairedScale(
            idxs.map(xs),
            clean,
            meanSd(clean),
            "ScaleWithin",
            column,
            "sd",
            group = Some(gs(idxs.head))
          )
          diagnostics ++= scaled.diagnostics
          scaled
        }.toMap

      val sds: Map[String, Double] =
        groupScaled.view.mapValues(_.scale).toMap

      val means: Map[String, Double] =
        groupScaled.view.mapValues(_.center).toMap

      val out = new Array[Double](xs.length)
      var i = 0
      while i < xs.length do
        val g = gs(i)
        val mu = means.getOrElse(g, Double.NaN)
        val sd = sds.getOrElse(g, Double.NaN)
        val v = (xs(i) - mu) / sd
        out(i) = if v.isFinite then v else 0.0
        i += 1

      val basis = ScaleWithin(xs, gs, argName, groupName, means, sds, Mat.unsafe(xs.length, 1, out))
      finishFit(basis, diagnostics.result(), policy)

  final case class RobustScale(x: Vector[Double], argName: String, median: Double, mad: Double, y: Mat) extends ParametricBasis:
    val name: String = s"robz_${argName}"
    val basisClass: String = "RobustScale"
    val columns: Vector[String] = Vector(Names.continuousToken(name))
    def subset(mask: Vector[Boolean]): ParametricBasis =
      val keep = maskToIndices(mask, x.length)
      val x2 = keep.map(x).toVector
      val out = keep.map(i => y.data(i)).toArray
      RobustScale(x2, argName, median, mad, Mat.unsafe(out.length, 1, out))

  object RobustScale:
    private val MadConstant = 1.4826

    def fit(x: Seq[Double], argName: String): RobustScale =
      fitWithDiagnostics(x, argName, BasisDegeneracyPolicy.Compatible)
        .fold(err => throw new IllegalArgumentException(err.message), _.basis)

    def fitWithDiagnostics(
        x: Seq[Double],
        argName: String,
        policy: BasisDegeneracyPolicy = BasisDegeneracyPolicy.Report
    ): Either[BasisFitError, BasisFit[RobustScale]] =
      val xs = x.toVector
      val clean = xs.filter(_.isFinite)
      val med = median(clean)
      val mad0 =
        if clean.isEmpty then Double.NaN
        else
          val absDevs = clean.map(a => math.abs(a - med))
          median(absDevs) * MadConstant
      val column = Names.continuousToken(s"robz_${argName}")
      val scaled = repairedScale(xs, clean, ScaleMoments(med, mad0), "RobustScale", column, "MAD")
      val basis = RobustScale(xs, argName, scaled.center, scaled.scale, Mat.unsafe(xs.length, 1, scaled.values))
      finishFit(basis, scaled.diagnostics, policy)

  final case class BSpline(x: Vector[Double], degree: Int, argName: String, y: Mat, boundary: (Double, Double)) extends ParametricBasis:
    val name: String = s"bs_${argName}"
    val basisClass: String = "BSpline"
    val columns: Vector[String] =
      Names.zeroPad(1 to degree, degree).map(Names.continuousToken)

    def subset(mask: Vector[Boolean]): ParametricBasis =
      val keep = maskToIndices(mask, x.length)
      val x2 = keep.map(x).toVector
      val out = new Array[Double](keep.length * y.cols)
      var rOut = 0
      while rOut < keep.length do
        val rIn = keep(rOut)
        System.arraycopy(y.data, rIn * y.cols, out, rOut * y.cols, y.cols)
        rOut += 1
      BSpline(x2, degree, argName, Mat.unsafe(keep.length, y.cols, out), boundary)

  object BSpline:
    def fit(x: Seq[Double], degree: Int, argName: String): BSpline =
      require(degree >= 1, "'degree' must be at least 1")
      val xs = x.toVector
      require(xs.forall(_.isFinite), "missing values are not allowed in 'BSpline'")
      val min = xs.min
      val max = xs.max

      val ord = degree + 1
      val knots = Array.fill(ord)(min) ++ Array.fill(ord)(max)
      val nBasisWithIntercept = knots.length - degree - 1 // = degree + 1
      require(nBasisWithIntercept == degree + 1)

      val out = new Array[Double](xs.length * degree)
      var i = 0
      while i < xs.length do
        val b = bsplineAt(xs(i), knots, degree).drop(1) // intercept = FALSE
        var j = 0
        while j < degree do
          out(i * degree + j) = b(j)
          j += 1
        i += 1

      BSpline(xs, degree, argName, Mat.unsafe(xs.length, degree, out), (min, max))

    // Cox–de Boor recursion (ported from scalafim.fmri.hrf).
    private def bsplineAt(x: Double, knots: Array[Double], degree: Int): Array[Double] =
      val nBasis = knots.length - degree - 1
      val lastSpan =
        if x == knots.last then
          var i = nBasis - 1
          while i >= 0 && !(knots(i) < knots(i + 1)) do i -= 1
          if i >= 0 then i else nBasis - 1
        else -1
      val n0 = new Array[Double](nBasis)
      var i = 0
      while i < nBasis do
        val k0 = knots(i)
        val k1 = knots(i + 1)
        n0(i) =
          if (x >= k0 && x < k1) 1.0
          else if (lastSpan >= 0 && i == lastSpan) 1.0
          else 0.0
        i += 1

      var p = 1
      var prev = n0
      while p <= degree do
        val next = new Array[Double](nBasis)
        i = 0
        while i < nBasis do
          val leftDen = knots(i + p) - knots(i)
          val rightDen = knots(i + p + 1) - knots(i + 1)
          val left =
            if leftDen == 0.0 then 0.0
            else (x - knots(i)) / leftDen * prev(i)
          val right =
            if rightDen == 0.0 || i + 1 >= nBasis then 0.0
            else (knots(i + p + 1) - x) / rightDen * prev(i + 1)
          next(i) = left + right
          i += 1
        prev = next
        p += 1
      prev

  final case class NSpline(
      x: Vector[Double],
      df: Int,
      argName: String,
      y: Mat,
      knots: Vector[Double],
      boundary: (Double, Double),
      intercept: Boolean
  ) extends ParametricBasis:
    val name: String = s"ns_${argName}"
    val basisClass: String = "NSpline"
    val columns: Vector[String] =
      Names.zeroPad(1 to df, df).map(Names.continuousToken)

    def subset(mask: Vector[Boolean]): ParametricBasis =
      val keep = maskToIndices(mask, x.length)
      val x2 = keep.map(x).toVector
      val out = new Array[Double](keep.length * y.cols)
      var rOut = 0
      while rOut < keep.length do
        val rIn = keep(rOut)
        System.arraycopy(y.data, rIn * y.cols, out, rOut * y.cols, y.cols)
        rOut += 1
      NSpline(x2, df, argName, Mat.unsafe(keep.length, y.cols, out), knots, boundary, intercept)

  object NSpline:
    def fit(
        x: Seq[Double],
        df: Int,
        argName: String,
        knots: Option[Seq[Double]] = None,
        intercept: Boolean = false,
        boundaryKnots: Option[(Double, Double)] = None
    ): NSpline =
      require(df >= 1, "'df' must be at least 1")
      val xs = x.toVector
      require(xs.forall(_.isFinite), "missing values are not allowed in 'NSpline'")

      val b0 =
        boundaryKnots match
          case Some((a, b)) => (math.min(a, b), math.max(a, b))
          case None =>
            if xs.isEmpty then (0.0, 1.0)
            else (xs.min, xs.max)

      val df0 =
        val minDf = if intercept then 2 else 1
        if df < minDf then minDf else df

      val iknots =
        knots match
          case Some(ks) =>
            val k0 = ks.toVector
            require(k0.forall(_.isFinite), "non-finite knots")
            k0.sorted
          case None =>
            val nIknots0 = df0 - 1 - (if intercept then 1 else 0)
            val nIknots = math.max(0, nIknots0)
            if nIknots == 0 then Vector.empty
            else
              val inside = xs.filter(v => v >= b0._1 && v <= b0._2)
              val xsQ = if inside.nonEmpty then inside else xs
              val probs = (1 to nIknots).iterator.map(i => i.toDouble / (nIknots + 1).toDouble).toVector
              adjustBoundaryKnots(quantileType7(xsQ, probs), b0)

      val y = evaluate(xs, knots = iknots, boundary = b0, intercept = intercept)
      NSpline(xs, df0, argName, y, iknots, b0, intercept)

    def predict(basis: NSpline, newData: Seq[Double]): Mat =
      val xs = newData.toVector
      if xs.isEmpty then Mat.zeros(0, basis.y.cols)
      else
        require(xs.forall(_.isFinite), "missing values are not allowed in 'NSpline'")
        evaluate(xs, knots = basis.knots, boundary = basis.boundary, intercept = basis.intercept)

    private def evaluate(x: Vector[Double], knots: Vector[Double], boundary: (Double, Double), intercept: Boolean): Mat =
      val bLeft = boundary._1
      val bRight = boundary._2
      require(bLeft <= bRight, "Boundary.knots must be ordered")
      val degree = 3
      val ord = degree + 1

      val aknots = (Vector.fill(ord)(bLeft) ++ Vector.fill(ord)(bRight) ++ knots).sorted.toArray
      val nBasis = aknots.length - degree - 1

      val basis0 = new Array[Double](x.length * nBasis)

      var i = 0
      while i < x.length do
        val xi = x(i)
        val outsideLeft = xi < bLeft
        val outsideRight = xi > bRight
        val row =
          if outsideLeft || outsideRight then
            val pivot = if outsideLeft then bLeft else bRight
            val b = splineDesignAt(aknots, pivot, degree = degree, deriv = 0)
            val d1 = splineDesignAt(aknots, pivot, degree = degree, deriv = 1)
            val dx = xi - pivot
            val out = new Array[Double](nBasis)
            var j = 0
            while j < nBasis do
              out(j) = b(j) + dx * d1(j)
              j += 1
            out
          else splineDesignAt(aknots, xi, degree = degree, deriv = 0)

        System.arraycopy(row, 0, basis0, i * nBasis, nBasis)
        i += 1

      val const0 = new Array[Double](2 * nBasis)
      val cL = splineDesignAt(aknots, bLeft, degree = degree, deriv = 2)
      val cR = splineDesignAt(aknots, bRight, degree = degree, deriv = 2)
      System.arraycopy(cL, 0, const0, 0, nBasis)
      System.arraycopy(cR, 0, const0, nBasis, nBasis)

      // Drop intercept column (like R's `if (!intercept) basis <- basis[, -1]`).
      val (basis1, const1, m1) =
        if intercept then (basis0, const0, nBasis)
        else
          val m = nBasis - 1
          val bOut = new Array[Double](x.length * m)
          i = 0
          while i < x.length do
            System.arraycopy(basis0, i * nBasis + 1, bOut, i * m, m)
            i += 1
          val cOut = new Array[Double](2 * m)
          System.arraycopy(const0, 1, cOut, 0, m)
          System.arraycopy(const0, nBasis + 1, cOut, m, m)
          (bOut, cOut, m)

      // QR of t(const) (m1 x 2), then project and drop constraint directions.
      val ct = new Array[Double](m1 * 2)
      var r = 0
      while r < m1 do
        ct(r * 2) = const1(r)
        ct(r * 2 + 1) = const1(m1 + r)
        r += 1

      val qr = scalafim.fmri.design.linalg.QrDecomposition.decompose(ct, rows = m1, cols = 2, pivoting = false)

      // y = t(basis1) => (m1 x n)
      val n = x.length
      val y = new Array[Double](m1 * n)
      i = 0
      while i < n do
        var j = 0
        while j < m1 do
          y(j * n + i) = basis1(i * m1 + j)
          j += 1
        i += 1

      qr.applyQtInPlace(y, yCols = n)

      val outCols = m1 - 2
      require(outCols >= 0, "ns basis must have non-negative columns")
      val out = new Array[Double](n * outCols)
      i = 0
      while i < n do
        var j = 0
        while j < outCols do
          out(i * outCols + j) = y((j + 2) * n + i)
          j += 1
        i += 1

      Mat.unsafe(n, outCols, out)

    private def quantileType7(x: Vector[Double], probs: Vector[Double]): Vector[Double] =
      if x.isEmpty then Vector.empty
      else
        val s = x.sorted
        val n = s.length

        def q(p: Double): Double =
          if p <= 0.0 then s.head
          else if p >= 1.0 then s.last
          else
            val h = (n - 1).toDouble * p + 1.0
            val j = math.floor(h).toInt // 1-based
            val g = h - j.toDouble
            val xj = s(j - 1)
            val xj1 = s(math.min(j, n - 1))
            xj + g * (xj1 - xj)

        probs.map(q)

    private def adjustBoundaryKnots(knots: Vector[Double], boundary: (Double, Double)): Vector[Double] =
      if knots.isEmpty then knots
      else
        val (bLeft, bRight) = boundary
        var out = knots

        val minK = out.min
        val maxK = out.max

        if minK == bLeft then
          val gt = out.filter(_ > bLeft)
          if gt.isEmpty then throw new IllegalArgumentException("all interior knots match left boundary knot")
          val delta = (gt.min - bLeft) / 8.0
          out = out.map(k => if k == bLeft then k + delta else k)

        if maxK == bRight then
          val lt = out.filter(_ < bRight)
          if lt.isEmpty then throw new IllegalArgumentException("all interior knots match right boundary knot")
          val delta = (bRight - lt.max) / 8.0
          out = out.map(k => if k == bRight then k - delta else k)

        out

    private def splineDesignAt(knots: Array[Double], x: Double, degree: Int, deriv: Int): Array[Double] =
      require(degree == 3, "NSpline only supports cubic splines (degree=3)")
      deriv match
        case 0 => bsplineAt(x, knots, degree = 3)
        case 1 => bsplineDeriv1At(x, knots)
        case 2 => bsplineDeriv2At(x, knots)
        case other =>
          throw new IllegalArgumentException(s"Unsupported derivative order: $other")

    private def bsplineDeriv1At(x: Double, knots: Array[Double]): Array[Double] =
      val degree = 3
      val b2 = bsplineAt(x, knots, degree = 2) // length = nBasis + 1
      val nBasis = knots.length - degree - 1
      val out = new Array[Double](nBasis)
      var i = 0
      while i < nBasis do
        val leftDen = knots(i + degree) - knots(i)
        val rightDen = knots(i + degree + 1) - knots(i + 1)
        val left = if leftDen == 0.0 then 0.0 else degree.toDouble / leftDen * b2(i)
        val right =
          if rightDen == 0.0 then 0.0
          else degree.toDouble / rightDen * b2(i + 1)
        out(i) = left - right
        i += 1
      out

    private def bsplineDeriv2At(x: Double, knots: Array[Double]): Array[Double] =
      val degree = 3
      val b1 = bsplineAt(x, knots, degree = 1) // length = nBasis + 2
      val nBasis = knots.length - degree - 1

      // First derivative of quadratic splines (degree=2), length = nBasis + 1
      val d2 = new Array[Double](nBasis + 1)
      var i = 0
      while i < nBasis + 1 do
        val p = 2
        val leftDen = knots(i + p) - knots(i)
        val rightDen = knots(i + p + 1) - knots(i + 1)
        val left = if leftDen == 0.0 then 0.0 else p.toDouble / leftDen * b1(i)
        val right = if rightDen == 0.0 then 0.0 else p.toDouble / rightDen * b1(i + 1)
        d2(i) = left - right
        i += 1

      // Second derivative of cubic splines from d/dx of degree=2 derivative.
      val out = new Array[Double](nBasis)
      i = 0
      while i < nBasis do
        val leftDen = knots(i + degree) - knots(i)
        val rightDen = knots(i + degree + 1) - knots(i + 1)
        val left = if leftDen == 0.0 then 0.0 else degree.toDouble / leftDen * d2(i)
        val right = if rightDen == 0.0 then 0.0 else degree.toDouble / rightDen * d2(i + 1)
        out(i) = left - right
        i += 1
      out

    // Cox–de Boor recursion (ported from scalafim.fmri.hrf).
    private def bsplineAt(x: Double, knots: Array[Double], degree: Int): Array[Double] =
      val nBasis = knots.length - degree - 1
      val lastSpan =
        if x == knots.last then
          var i = nBasis - 1
          while i >= 0 && !(knots(i) < knots(i + 1)) do i -= 1
          if i >= 0 then i else nBasis - 1
        else -1
      val n0 = new Array[Double](nBasis)
      var i = 0
      while i < nBasis do
        val k0 = knots(i)
        val k1 = knots(i + 1)
        n0(i) =
          if (x >= k0 && x < k1) 1.0
          else if (lastSpan >= 0 && i == lastSpan) 1.0
          else 0.0
        i += 1

      var p = 1
      var prev = n0
      while p <= degree do
        val next = new Array[Double](nBasis)
        i = 0
        while i < nBasis do
          val leftDen = knots(i + p) - knots(i)
          val rightDen = knots(i + p + 1) - knots(i + 1)
          val left =
            if leftDen == 0.0 then 0.0
            else (x - knots(i)) / leftDen * prev(i)
          val right =
            if rightDen == 0.0 || i + 1 >= nBasis then 0.0
            else (knots(i + p + 1) - x) / rightDen * prev(i + 1)
          next(i) = left + right
          i += 1
        prev = next
        p += 1
      prev

  private def maskToIndices(mask: Vector[Boolean], n: Int): Array[Int] =
    require(mask.length == n, s"subset length (${mask.length}) must match n ($n)")
    val buf = scala.collection.mutable.ArrayBuffer.empty[Int]
    var i = 0
    while i < mask.length do
      if mask(i) then buf += i
      i += 1
    buf.toArray

  private def median(xs: Seq[Double]): Double =
    if xs.isEmpty then Double.NaN
    else
      val s = xs.sorted
      val n = s.length
      if n % 2 == 1 then s(n / 2)
      else
        val a = s(n / 2 - 1)
        val b = s(n / 2)
        (a + b) / 2.0
