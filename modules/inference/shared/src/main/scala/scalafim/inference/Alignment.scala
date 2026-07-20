package scalafim.inference

import scalafim.linalg.DecompositionRank
import scalafim.linalg.DenseSvdSolver
import scalafim.linalg.DoubleMatrix
import scalafim.linalg.LinalgSolvers

enum MatchingMetric:
  case AbsoluteInnerProduct
  case Cosine

enum MatchingMethod:
  case Hungarian

final case class ComponentMatch private[inference] (
    permutation: Vector[Int],
    score: Double,
    margin: Double,
    ambiguous: Boolean,
    method: MatchingMethod
)

final case class AlignmentResult(
    matching: ComponentMatch,
    aligned: DoubleMatrix
)

object ComponentAlignment:
  def align(
      reference: DoubleMatrix,
      replicate: DoubleMatrix,
      metric: MatchingMetric = MatchingMetric.AbsoluteInnerProduct,
      ambiguityTolerance: Double = 1e-8
  ): Either[InferenceError, AlignmentResult] =
    for
      matching <- matchComponents(reference, replicate, metric, ambiguityTolerance)
      permuted <- permute(replicate, matching.permutation)
      aligned <- alignSigns(reference, permuted)
    yield AlignmentResult(matching, aligned)

  def matchComponents(
      reference: DoubleMatrix,
      replicate: DoubleMatrix,
      metric: MatchingMetric = MatchingMetric.AbsoluteInnerProduct,
      ambiguityTolerance: Double = 1e-8
  ): Either[InferenceError, ComponentMatch] =
    for
      _ <- validatePair(reference, replicate)
      _ <-
        if ambiguityTolerance.isFinite && ambiguityTolerance >= 0.0 then Right(())
        else Left(InferenceError.InvalidTolerance("matching ambiguity tolerance", ambiguityTolerance))
    yield
      val scores = scoreMatrix(reference, replicate, metric)
      val permutation = hungarianMaximum(scores, reference.cols)
      var total = 0.0
      var margin = Double.PositiveInfinity
      var row = 0
      while row < reference.cols do
        val assigned = scores(row * reference.cols + permutation(row))
        total += assigned
        var alternative = Double.NegativeInfinity
        var col = 0
        while col < reference.cols do
          if col != permutation(row) then alternative = Math.max(alternative, scores(row * reference.cols + col))
          col += 1
        if alternative.isFinite then margin = Math.min(margin, assigned - alternative)
        row += 1
      ComponentMatch(
        permutation,
        total,
        margin,
        margin.isFinite && margin <= ambiguityTolerance,
        MatchingMethod.Hungarian
      )

  def permute(matrix: DoubleMatrix, permutation: Vector[Int]): Either[InferenceError, DoubleMatrix] =
    if permutation.length != matrix.cols then
      Left(InferenceError.InvalidReplicatePlan(
        s"component permutation length ${permutation.length} does not match ${matrix.cols} columns"
      ))
    else if permutation.sorted != Vector.range(0, matrix.cols) then
      Left(InferenceError.InvalidReplicatePlan("component permutation must contain each column exactly once"))
    else
      val out = new Array[Double](matrix.rows * matrix.cols)
      var row = 0
      while row < matrix.rows do
        var col = 0
        while col < matrix.cols do
          out(row * matrix.cols + col) = matrix(row, permutation(col))
          col += 1
        row += 1
      Right(DoubleMatrix.unsafe(matrix.rows, matrix.cols, out))

  def alignSigns(
      reference: DoubleMatrix,
      matched: DoubleMatrix
  ): Either[InferenceError, DoubleMatrix] =
    validatePair(reference, matched).map { _ =>
      val signs = new Array[Double](reference.cols)
      var col = 0
      while col < reference.cols do
        var inner = 0.0
        var row = 0
        while row < reference.rows do
          inner += reference(row, col) * matched(row, col)
          row += 1
        signs(col) = if inner < 0.0 then -1.0 else 1.0
        col += 1
      val out = matched.copyData
      var row = 0
      while row < matched.rows do
        col = 0
        while col < matched.cols do
          out(row * matched.cols + col) *= signs(col)
          col += 1
        row += 1
      DoubleMatrix.unsafe(matched.rows, matched.cols, out)
    }

  private def validatePair(left: DoubleMatrix, right: DoubleMatrix): Either[InferenceError, Unit] =
    if left.rows != right.rows then
      Left(InferenceError.InvalidReplicatePlan(
        s"alignment matrices differ: ${left.rows}x${left.cols} and ${right.rows}x${right.cols}"
      ))
    else if left.cols != right.cols then Left(InferenceError.RankLoss(left.cols, right.cols))
    else if left.cols <= 0 then Left(InferenceError.InvalidCount("alignment columns", left.cols))
    else
      val values = left.copyData ++ right.copyData
      var i = 0
      while i < values.length do
        if !values(i).isFinite then return Left(InferenceError.NonFiniteStatistic(s"alignment entry $i", values(i)))
        i += 1
      Right(())

  private def scoreMatrix(
      reference: DoubleMatrix,
      replicate: DoubleMatrix,
      metric: MatchingMetric
  ): Array[Double] =
    val k = reference.cols
    val scores = new Array[Double](k * k)
    var ref = 0
    while ref < k do
      var rep = 0
      while rep < k do
        var inner = 0.0
        var refNorm = 0.0
        var repNorm = 0.0
        var row = 0
        while row < reference.rows do
          val x = reference(row, ref)
          val y = replicate(row, rep)
          inner += x * y
          refNorm += x * x
          repNorm += y * y
          row += 1
        val absolute = Math.abs(inner)
        scores(ref * k + rep) =
          metric match
            case MatchingMetric.AbsoluteInnerProduct => absolute
            case MatchingMetric.Cosine =>
              val denominator = Math.sqrt(refNorm * repNorm)
              if denominator > 0.0 then absolute / denominator else 0.0
        rep += 1
      ref += 1
    scores

  private def hungarianMaximum(scores: Array[Double], size: Int): Vector[Int] =
    val u = new Array[Double](size + 1)
    val v = new Array[Double](size + 1)
    val p = new Array[Int](size + 1)
    val way = new Array[Int](size + 1)
    var i = 1
    while i <= size do
      p(0) = i
      var j0 = 0
      val minv = Array.fill(size + 1)(Double.PositiveInfinity)
      val used = Array.fill(size + 1)(false)
      var searching = true
      while searching do
        used(j0) = true
        val i0 = p(j0)
        var delta = Double.PositiveInfinity
        var j1 = 0
        var j = 1
        while j <= size do
          if !used(j) then
            val current = -scores((i0 - 1) * size + (j - 1)) - u(i0) - v(j)
            if current < minv(j) then
              minv(j) = current
              way(j) = j0
            if minv(j) < delta then
              delta = minv(j)
              j1 = j
          j += 1
        j = 0
        while j <= size do
          if used(j) then
            u(p(j)) += delta
            v(j) -= delta
          else minv(j) -= delta
          j += 1
        j0 = j1
        searching = p(j0) != 0
      var augmenting = true
      while augmenting do
        val j1 = way(j0)
        p(j0) = p(j1)
        j0 = j1
        augmenting = j0 != 0
      i += 1
    val permutation = new Array[Int](size)
    var col = 1
    while col <= size do
      permutation(p(col) - 1) = col - 1
      col += 1
    permutation.toVector

final case class PrincipalAngles private (values: Vector[Double])

object PrincipalAngles:
  def between(
      left: DoubleMatrix,
      right: DoubleMatrix,
      solver: DenseSvdSolver = LinalgSolvers.denseSvd,
      rankTolerance: Double = 1e-10
  ): Either[InferenceError, PrincipalAngles] =
    if left.rows != right.rows then
      Left(InferenceError.RowCountMismatch("principal-angle bases", left.rows, right.rows))
    else if left.cols <= 0 || right.cols <= 0 then
      Left(InferenceError.InvalidCount("principal-angle basis columns", Math.min(left.cols, right.cols)))
    else
      for
        qLeft <- orthonormalBasis(left, solver, rankTolerance)
        qRight <- orthonormalBasis(right, solver, rankTolerance)
        cross = DoubleMatrix.transposeMultiply(qLeft, qRight)
        rank <- DecompositionRank.bounded(Math.min(qLeft.cols, qRight.cols), Math.min(cross.rows, cross.cols))
          .left.map(error => InferenceError.NumericalFailure("principal-angle rank", error.message))
        fit <- solver.decompose(cross, rank)
          .left.map(error => InferenceError.NumericalFailure("principal-angle SVD", error.message))
      yield PrincipalAngles(Vector.tabulate(fit.singularValues.length) { index =>
        Math.acos(Math.max(-1.0, Math.min(1.0, fit.singularValues(index))))
      })

  private def orthonormalBasis(
      matrix: DoubleMatrix,
      solver: DenseSvdSolver,
      tolerance: Double
  ): Either[InferenceError, DoubleMatrix] =
    if !tolerance.isFinite || tolerance < 0.0 then
      Left(InferenceError.InvalidTolerance("principal-angle rank tolerance", tolerance))
    else
      DecompositionRank.bounded(Math.min(matrix.rows, matrix.cols), Math.min(matrix.rows, matrix.cols))
        .left.map(error => InferenceError.NumericalFailure("subspace rank", error.message))
        .flatMap { rank =>
          solver.decompose(matrix, rank)
            .left.map(error => InferenceError.NumericalFailure("subspace SVD", error.message))
        }
        .flatMap { fit =>
          val scale = if fit.singularValues.length > 0 then fit.singularValues(0) else 0.0
          var kept = 0
          while kept < fit.singularValues.length && fit.singularValues(kept) > tolerance * Math.max(1.0, scale) do
            kept += 1
          if kept < matrix.cols then Left(InferenceError.RankLoss(matrix.cols, kept))
          else
            val out = new Array[Double](matrix.rows * kept)
            var row = 0
            while row < matrix.rows do
              var col = 0
              while col < kept do
                out(row * kept + col) = fit.u(row, col)
                col += 1
              row += 1
            Right(DoubleMatrix.unsafe(matrix.rows, kept, out))
        }
