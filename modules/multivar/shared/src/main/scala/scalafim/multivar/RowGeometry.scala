package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

enum RowWhiteningMode:
  case Identity
  case GroupedIdentity
  case BlockCholesky

enum EffectScope:
  case Between
  case Within
  case Mixed
  case Ungrouped

enum ExchangeabilityScheme:
  case BetweenSubject
  case WithinSubject
  case WholeSubject
  case Rows

enum EffectScale:
  case Whitened
  case Processed
  case Original

type RowMetricMode = RowWhiteningMode

object RowMetricMode:
  val Identity: RowWhiteningMode =
    RowWhiteningMode.Identity
  val GroupedIdentity: RowWhiteningMode =
    RowWhiteningMode.GroupedIdentity
  val BlockCholesky: RowWhiteningMode =
    RowWhiteningMode.BlockCholesky

trait RowWhitening:
  def rows: Int
  def mode: RowWhiteningMode
  def blocks: Vector[IndexSet]

  def whiten(input: DMat): Either[MultivarError, DMat]

  def unwhiten(input: DMat): Either[MultivarError, DMat]

  def solve(input: DMat): Either[MultivarError, DMat]

type RowMetric = RowWhitening

final case class IdentityRowWhitening private[multivar] (
    rows: Int,
    mode: RowWhiteningMode,
    blocks: Vector[IndexSet]
) extends RowWhitening:
  require(rows > 0, "row whitening rows must be positive")

  override def whiten(input: DMat): Either[MultivarError, DMat] =
    RowGeometryOps.requireRows("row whitening", input, rows).map(_ => input)

  override def unwhiten(input: DMat): Either[MultivarError, DMat] =
    RowGeometryOps.requireRows("row unwhitening", input, rows).map(_ => input)

  override def solve(input: DMat): Either[MultivarError, DMat] =
    RowGeometryOps.requireRows("row whitening solve", input, rows).map(_ => input)

final case class BlockCholeskyRowWhitening private[multivar] (
    rows: Int,
    blocks: Vector[IndexSet],
    upperCholesky: Vector[DMat],
    tolerance: Double
) extends RowWhitening:
  require(rows > 0, "row whitening rows must be positive")
  require(blocks.length == upperCholesky.length, "row whitening block/cholesky counts must match")

  override def mode: RowWhiteningMode =
    RowWhiteningMode.BlockCholesky

  override def whiten(input: DMat): Either[MultivarError, DMat] =
    applyBlocks(input, RowGeometryOps.solveTransposeUpper(_, _, tolerance))

  override def unwhiten(input: DMat): Either[MultivarError, DMat] =
    applyBlocks(input, RowGeometryOps.multiplyTransposeUpper)

  override def solve(input: DMat): Either[MultivarError, DMat] =
    for
      whitened <- whiten(input)
      solved <- applyBlocks(whitened, RowGeometryOps.solveUpper(_, _, tolerance))
    yield solved

  private def applyBlocks(
      input: DMat,
      op: (DMat, DMat) => Either[MultivarError, DMat]
  ): Either[MultivarError, DMat] =
    RowGeometryOps.requireRows("row whitening block operation", input, rows).flatMap { _ =>
      val out = new Array[Double](input.rows * input.cols)
      var blockIndex = 0
      var error = Option.empty[MultivarError]
      while blockIndex < blocks.length && error.isEmpty do
        val block = blocks(blockIndex)
        val local = RowGeometryOps.selectRows(input, block.indices)
        op(upperCholesky(blockIndex), local) match
          case Left(value) => error = Some(value)
          case Right(blockOut) =>
            RowGeometryOps.writeRows(out, input.cols, block.indices, blockOut)
        blockIndex += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(GaleNumerics.matrixFromRowMajor(input.rows, input.cols, out))
    }

object RowWhitening:
  def identity(rows: Int): Either[MultivarError, RowWhitening] =
    if rows <= 0 then Left(MultivarError.InvalidDimension("row whitening rows", rows))
    else Right(new IdentityRowWhitening(rows, RowWhiteningMode.Identity, Vector.empty))

  def groupedIdentity(rows: Int, blocks: Vector[IndexSet]): Either[MultivarError, RowWhitening] =
    if rows <= 0 then Left(MultivarError.InvalidDimension("row whitening rows", rows))
    else
      validateBlocks(rows, blocks).map { checked =>
        new IdentityRowWhitening(rows, RowWhiteningMode.GroupedIdentity, checked)
      }

  def blockCholesky(
      rows: Int,
      blocks: Vector[IndexSet],
      upperCholesky: Vector[DMat],
      tolerance: Double = 1e-12
  ): Either[MultivarError, RowWhitening] =
    if rows <= 0 then Left(MultivarError.InvalidDimension("row whitening rows", rows))
    else if blocks.length != upperCholesky.length then
      Left(MultivarError.InvalidRowGeometry("row whitening block and Cholesky counts differ"))
    else
      for
        _ <- RowGeometryOps.requireTolerance("row whitening Cholesky tolerance", tolerance)
        checked <- validateBlocks(rows, blocks)
        _ <- validateCholesky(checked, upperCholesky, tolerance)
      yield new BlockCholeskyRowWhitening(rows, checked, upperCholesky, tolerance)

  private def validateBlocks(rows: Int, blocks: Vector[IndexSet]): Either[MultivarError, Vector[IndexSet]] =
    if blocks.isEmpty then Left(MultivarError.InvalidRowGeometry("row whitening block list must be non-empty"))
    else
      val seen = Array.fill(rows)(false)
      var blockIndex = 0
      var error = Option.empty[MultivarError]
      while blockIndex < blocks.length && error.isEmpty do
        val block = blocks(blockIndex)
        if block.axis != IndexAxis.Row && block.axis != IndexAxis.Sample then
          error = Some(MultivarError.InvalidRowGeometry("row whitening blocks must use row or sample indices"))
        else
          block.requireWithin(rows) match
            case Left(value) => error = Some(value)
            case Right(checked) =>
              val indices = checked.indices
              var i = 0
              while i < indices.length && error.isEmpty do
                val row = indices(i)
                if seen(row) then error = Some(MultivarError.DuplicateIndex(IndexAxis.Row, row))
                else seen(row) = true
                i += 1
        blockIndex += 1

      var row = 0
      while row < rows && error.isEmpty do
        if !seen(row) then error = Some(MultivarError.InvalidRowGeometry(s"row whitening blocks do not cover row $row"))
        row += 1

      error match
        case Some(value) => Left(value)
        case None        => Right(blocks)

  private def validateCholesky(
      blocks: Vector[IndexSet],
      upperCholesky: Vector[DMat],
      tolerance: Double
  ): Either[MultivarError, Unit] =
    var blockIndex = 0
    var error = Option.empty[MultivarError]
    while blockIndex < blocks.length && error.isEmpty do
      val block = blocks(blockIndex)
      val chol = upperCholesky(blockIndex)
      if chol.rows != chol.cols || chol.rows != block.length then
        error = Some(
          MultivarError.InvalidRowGeometry(
            s"Cholesky block ${chol.rows}x${chol.cols} does not match row block length ${block.length}"
          )
        )
      else
        MatrixOps.checkFinite("row whitening Cholesky block", chol) match
          case Left(value) => error = Some(value)
          case Right(_) =>
            var row = 0
            while row < chol.rows && error.isEmpty do
              var col = 0
              while col < row && error.isEmpty do
                if Math.abs(chol(row, col)) > tolerance then
                  error = Some(MultivarError.InvalidRowGeometry("row whitening Cholesky factors must be upper triangular"))
                col += 1
              if chol(row, row) <= tolerance then
                error = Some(MultivarError.SingularRowMetric("row whitening Cholesky factor has a non-positive diagonal"))
              row += 1
      blockIndex += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(())

object RowMetric:
  def identity(rows: Int): Either[MultivarError, RowWhitening] =
    RowWhitening.identity(rows)

  def groupedIdentity(rows: Int, blocks: Vector[IndexSet]): Either[MultivarError, RowWhitening] =
    RowWhitening.groupedIdentity(rows, blocks)

  def blockCholesky(
      rows: Int,
      blocks: Vector[IndexSet],
      upperCholesky: Vector[DMat],
      tolerance: Double = 1e-12
  ): Either[MultivarError, RowWhitening] =
    RowWhitening.blockCholesky(rows, blocks, upperCholesky, tolerance)

final case class RowProjector private (
    matrix: DMat,
    rank: Int,
    basis: Option[DMat]
):
  require(matrix.rows == matrix.cols, "row projector matrix must be square")
  require(rank >= 0, "row projector rank must be non-negative")

  def rows: Int =
    matrix.rows

  def project(input: DMat): Either[MultivarError, DMat] =
    if input.rows != rows then
      Left(MultivarError.MatrixShapeMismatch(s"row projector expected ${rows} rows, got ${input.rows}"))
    else Right(GaleNumerics.multiply(matrix, input))

  def difference(nuisance: RowProjector, tolerance: Double = 1e-8): Either[MultivarError, RowProjector] =
    if nuisance.rows != rows then
      Left(MultivarError.MatrixShapeMismatch(s"row projector sizes differ: $rows vs ${nuisance.rows}"))
    else
      RowGeometryOps.requireTolerance("row projector difference tolerance", tolerance).flatMap { _ =>
        val nested = GaleNumerics.multiply(matrix, nuisance.matrix)
        if !RowGeometryOps.close(nested, nuisance.matrix, tolerance) then
          Left(MultivarError.InvalidRowGeometry("nuisance projector is not nested in full projector"))
        else RowProjector.fromMatrix(RowGeometryOps.subtract(matrix, nuisance.matrix), tolerance)
      }

  def complement: RowProjector =
    val out = new Array[Double](rows * rows)
    var row = 0
    while row < rows do
      var col = 0
      while col < rows do
        out(row * rows + col) = (if row == col then 1.0 else 0.0) - matrix(row, col)
        col += 1
      row += 1
    RowProjector(GaleNumerics.matrixFromRowMajor(rows, rows, out), rows - rank, None)

object RowProjector:
  def zero(rows: Int): Either[MultivarError, RowProjector] =
    if rows <= 0 then Left(MultivarError.InvalidDimension("row projector rows", rows))
    else Right(RowProjector(DMat.zeros(rows, rows), 0, Some(DMat.zeros(rows, 0))))

  /** Orthogonal projector onto the column span of `design`.
    *
    * The basis is orthonormalized via the eigendecomposition of the Gram matrix
    * `design' design`, which squares the condition number of `design`. Threshold
    * semantics: Gram eigenvalues at or below `tolerance * max(1, lambdaMax)` are
    * truncated, i.e. directions whose relative singular value falls below
    * `sqrt(tolerance)` (about 1e-5 at the default tolerance) drop out of the span.
    * Near-collinear design columns therefore lose rank earlier than users of a
    * QR-based orthonormalization (e.g. R's `qr()`) may expect. Future work: route
    * this through a QR/Householder factorization in `linalg` to recover
    * singular-value-level rank sensitivity.
    */
  def orthogonal(
      design: DMat,
      solver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      tolerance: Double = 1e-10
  ): Either[MultivarError, RowProjector] =
    if design.rows <= 0 then Left(MultivarError.InvalidDimension("row projector rows", design.rows))
    else
      RowGeometryOps.requireTolerance("row projector eigen tolerance", tolerance).flatMap { _ =>
        if design.cols == 0 then zero(design.rows)
        else
          for
            _ <- MatrixOps.checkFinite("row design matrix", design)
            gram = GaleNumerics.crossProduct(design)
            eigen <- LinalgErrorAdapter.adapt(solver.decompose(gram))
          yield
            val maxValue =
              if eigen.values.length == 0 then 0.0
              else Math.max(1.0, Math.abs(eigen.values(0)))
            var rank = 0
            while rank < eigen.values.length && eigen.values(rank) > tolerance * maxValue do
              rank += 1

            if rank == 0 then RowProjector(DMat.zeros(design.rows, design.rows), 0, Some(DMat.zeros(design.rows, 0)))
            else
              val rawBasis = designRightMultiply(design, MatrixOps.takeColumns(eigen.vectors, rank))
              val basisData = rawBasis.copyData
              var col = 0
              while col < rank do
                val scale = 1.0 / Math.sqrt(Math.max(eigen.values(col), tolerance))
                var row = 0
                while row < rawBasis.rows do
                  basisData(row * rank + col) *= scale
                  row += 1
                col += 1
              val basis = GaleNumerics.matrixFromRowMajor(design.rows, rank, basisData)
              val projector = GaleNumerics.multiply(basis, basis.transpose)
              RowProjector(projector, rank, Some(basis))
      }

  def fromMatrix(matrix: DMat, tolerance: Double = 1e-8): Either[MultivarError, RowProjector] =
    if matrix.rows <= 0 then Left(MultivarError.InvalidDimension("row projector rows", matrix.rows))
    else if matrix.rows != matrix.cols then Left(MultivarError.MatrixShapeMismatch("row projector matrix must be square"))
    else
      for
        _ <- RowGeometryOps.requireTolerance("row projector matrix tolerance", tolerance)
        _ <- MatrixOps.checkFinite("row projector matrix", matrix)
        _ <- MatrixOps.checkSymmetric(matrix, tolerance)
        squared = GaleNumerics.multiply(matrix, matrix)
        _ <-
          if RowGeometryOps.close(squared, matrix, tolerance) then Right(())
          else Left(MultivarError.InvalidRowGeometry("row projector matrix must be idempotent"))
      yield
        val trace = RowGeometryOps.trace(matrix)
        val rank = Math.rint(trace).toInt
        RowProjector(matrix, rank, None)

  private def designRightMultiply(design: DMat, weights: DMat): DMat =
    GaleNumerics.multiply(design, weights)

final case class EffectTerm(
    label: String,
    effectColumns: Vector[Int],
    nuisanceColumns: Vector[Int],
    df: Int,
    scope: EffectScope,
    exchangeability: ExchangeabilityScheme
):
  require(label.nonEmpty, "effect term label must be non-empty")
  require(effectColumns.nonEmpty, "effect term must have at least one design column")
  require(df >= 0, "effect term degrees of freedom must be non-negative")

object EffectTerm:
  def of(
      label: String,
      effectColumns: Iterable[Int],
      nuisanceColumns: Iterable[Int],
      df: Int,
      scope: EffectScope = EffectScope.Ungrouped,
      exchangeability: ExchangeabilityScheme = ExchangeabilityScheme.Rows,
      designColumns: Option[Int] = None
  ): Either[MultivarError, EffectTerm] =
    for
      checkedLabel <- Identifier.validate("effect term", label)
      effect <- RowGeometryOps.validateColumnList("effect columns", effectColumns, allowEmpty = false, designColumns)
      nuisance <- RowGeometryOps.validateColumnList("nuisance columns", nuisanceColumns, allowEmpty = true, designColumns)
      _ <- RowGeometryOps.requireDisjoint(effect, nuisance)
      _ <- if df >= 0 then Right(()) else Left(MultivarError.InvalidRowGeometry("effect term df must be non-negative"))
    yield EffectTerm(checkedLabel, effect, nuisance, df, scope, exchangeability)

final case class EffectTermFit(
    term: EffectTerm,
    rowWhitening: RowWhitening,
    termProjector: RowProjector,
    nuisanceProjector: RowProjector,
    fullProjector: RowProjector
):
  require(termProjector.rows == rowWhitening.rows, "term projector rows must match row whitening")
  require(nuisanceProjector.rows == rowWhitening.rows, "nuisance projector rows must match row whitening")
  require(fullProjector.rows == rowWhitening.rows, "full projector rows must match row whitening")

  def rowMetric: RowWhitening =
    rowWhitening

  def effectMatrix(response: DMat): Either[MultivarError, DMat] =
    for
      whitened <- rowWhitening.whiten(response)
      projected <- termProjector.project(whitened)
    yield projected

object EffectTermFit:
  def fromDesign(
      label: String,
      design: DMat,
      effectColumns: Iterable[Int],
      rowWhitening: RowWhitening,
      scope: EffectScope = EffectScope.Ungrouped,
      exchangeability: ExchangeabilityScheme = ExchangeabilityScheme.Rows,
      solver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      tolerance: Double = 1e-10
  ): Either[MultivarError, EffectTermFit] =
    if design.rows != rowWhitening.rows then
      Left(MultivarError.MatrixShapeMismatch(s"design has ${design.rows} rows but row whitening has ${rowWhitening.rows}"))
    else
      for
        effect <- RowGeometryOps.validateColumnList("effect columns", effectColumns, allowEmpty = false, Some(design.cols))
        nuisance = (0 until design.cols).filterNot(effect.contains).toVector
        designW <- rowWhitening.whiten(design)
        effectDesign = RowGeometryOps.selectColumns(designW, effect)
        nuisanceDesign = RowGeometryOps.selectColumns(designW, nuisance)
        fullDesign = RowGeometryOps.concatColumns(nuisanceDesign, effectDesign)
        nuisanceProjector <- RowProjector.orthogonal(nuisanceDesign, solver, tolerance)
        fullProjector <- RowProjector.orthogonal(fullDesign, solver, tolerance)
        termProjector <- fullProjector.difference(nuisanceProjector, Math.sqrt(tolerance))
        term <- EffectTerm.of(
          label,
          effect,
          nuisance,
          termProjector.rank,
          scope,
          exchangeability,
          Some(design.cols)
        )
      yield EffectTermFit(term, rowWhitening, termProjector, nuisanceProjector, fullProjector)

final case class EffectModelFit(
    rowWhitening: RowWhitening,
    design: DMat,
    designWhitened: DMat,
    modelProjector: RowProjector,
    terms: Vector[EffectTermFit]
):
  def rowMetric: RowWhitening =
    rowWhitening

  def term(label: String): Option[EffectTermFit] =
    terms.find(_.term.label == label)

object EffectModelFit:
  def fromTerms(
      design: DMat,
      rowWhitening: RowWhitening,
      terms: Vector[(String, Iterable[Int])],
      solver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      tolerance: Double = 1e-10
  ): Either[MultivarError, EffectModelFit] =
    if design.rows != rowWhitening.rows then
      Left(MultivarError.MatrixShapeMismatch(s"design has ${design.rows} rows but row whitening has ${rowWhitening.rows}"))
    else
      for
        designW <- rowWhitening.whiten(design)
        modelProjector <- RowProjector.orthogonal(designW, solver, tolerance)
        termFits <- MatrixOps.traverse(terms) { case (label, columns) =>
          EffectTermFit.fromDesign(label, design, columns, rowWhitening, solver = solver, tolerance = tolerance)
        }
      yield EffectModelFit(rowWhitening, design, designW, modelProjector, termFits)

final case class EffectOperator private (
    termFit: EffectTermFit,
    loadings: DMat,
    scores: DMat,
    singularValues: DVec,
    preprocessor: FittedPreprocessor,
    basis: DMat,
    basisEffectMatrix: DMat,
    effectMatrixWhitened: DMat,
    fittedContribution: DMat
):
  require(loadings.cols == scores.cols, "effect loadings and scores must have the same component count")
  require(singularValues.length == loadings.cols, "singular values must match component count")
  require(scores.rows == termFit.rowWhitening.rows, "effect scores must match row whitening rows")
  require(loadings.rows == preprocessor.inputCols, "effect loadings must match response feature count")
  require(effectMatrixWhitened.rows == scores.rows, "whitened effect matrix rows must match scores")
  require(effectMatrixWhitened.cols == loadings.rows, "whitened effect matrix columns must match loadings")
  require(fittedContribution.rows == scores.rows, "fitted contribution rows must match scores")
  require(fittedContribution.cols == loadings.rows, "fitted contribution columns must match loadings")

  def rank: Int =
    singularValues.length

  def reconstruct(scale: EffectScale = EffectScale.Processed): Either[MultivarError, DMat] =
    scale match
      case EffectScale.Whitened =>
        Right(effectMatrixWhitened)
      case EffectScale.Processed =>
        Right(fittedContribution)
      case EffectScale.Original =>
        EffectOperator.inverseContribution(preprocessor, fittedContribution)

  /** Truncate to the leading `components` factors.
    *
    * Takes a validated `Int` rather than `ComponentCount` because zero is a
    * legitimate truncation target here (an empty operator whose reconstruction is
    * exactly zero), while `ComponentCount` is strictly positive. Negative or
    * over-rank requests return a typed `InvalidComponentRequest`.
    */
  def truncate(components: Int): Either[MultivarError, EffectOperator] =
    if components < 0 || components > rank then Left(MultivarError.InvalidComponentRequest(components, rank))
    else if components == rank then Right(this)
    else
      EffectOperator.fromParts(
        termFit,
        MatrixOps.takeColumns(loadings, components),
        MatrixOps.takeColumns(scores, components),
        MatrixOps.takeVector(singularValues, components),
        preprocessor,
        basis,
        basisEffectMatrix
      )

object EffectOperator:
  def fit(
      termFit: EffectTermFit,
      response: MatrixView,
      preprocessor: FittedPreprocessor,
      basis: DMat,
      components: Option[ComponentCount] = None,
      solver: SvdSolver = DenseSolvers.svd,
      tolerance: Double = 1e-10
  ): Either[MultivarError, EffectOperator] =
    if response.rows != termFit.rowWhitening.rows then
      Left(MultivarError.MatrixShapeMismatch(s"response has ${response.rows} rows but row whitening has ${termFit.rowWhitening.rows}"))
    else if response.cols != preprocessor.inputCols then
      Left(MultivarError.MatrixShapeMismatch(s"response has ${response.cols} columns but preprocessor expects ${preprocessor.inputCols}"))
    else if basis.rows != response.cols then
      Left(MultivarError.MatrixShapeMismatch(s"basis has ${basis.rows} rows but response has ${response.cols} columns"))
    else
      for
        _ <- RowGeometryOps.requireTolerance("effect operator SVD tolerance", tolerance)
        _ <- MatrixOps.checkFinite("effect basis", basis)
        _ <- RowGeometryOps.requireOrthonormalColumns("effect basis", basis, Math.sqrt(tolerance))
        responseBasis <- response.rightMultiply(basis)
        responseBasisW <- termFit.rowWhitening.whiten(responseBasis)
        basisEffect <- termFit.termProjector.project(responseBasisW)
        rankBound = Math.min(Math.min(termFit.term.df, basis.cols), Math.min(basisEffect.rows, basisEffect.cols))
        requested = components.map(_.value).getOrElse(rankBound)
        out <-
          if requested > rankBound then Left(MultivarError.InvalidComponentRequest(requested, rankBound))
          else if rankBound <= 0 || requested == 0 then
            empty(termFit, response.cols, response.rows, preprocessor, basis, basisEffect)
          else
            solver.decompose(MatrixView.dense(basisEffect), ComponentCount.unsafe(rankBound)).flatMap { svd =>
              val maxValue =
                if svd.singularValues.length == 0 then 0.0
                else Math.max(1.0, svd.singularValues(0))
              var available = 0
              while available < svd.singularValues.length && svd.singularValues(available) > tolerance * maxValue do
                available += 1
              val keep = Math.min(requested, available)
              if keep == 0 then empty(termFit, response.cols, response.rows, preprocessor, basis, basisEffect)
              else
                val basisLoadings = MatrixOps.takeColumns(svd.v, keep)
                val loadings = GaleNumerics.multiply(basis, basisLoadings)
                val scores = MatrixOps.scaleColumns(MatrixOps.takeColumns(svd.u, keep), MatrixOps.takeVector(svd.singularValues, keep))
                fromParts(termFit, loadings, scores, MatrixOps.takeVector(svd.singularValues, keep), preprocessor, basis, basisEffect)
            }
      yield out

  private def empty(
      termFit: EffectTermFit,
      featureCols: Int,
      responseRows: Int,
      preprocessor: FittedPreprocessor,
      basis: DMat,
      basisEffectMatrix: DMat
  ): Either[MultivarError, EffectOperator] =
    fromParts(
      termFit,
      DMat.zeros(featureCols, 0),
      DMat.zeros(responseRows, 0),
      DVec.zeros(0),
      preprocessor,
      basis,
      basisEffectMatrix
    )

  private def fromParts(
      termFit: EffectTermFit,
      loadings: DMat,
      scores: DMat,
      singularValues: DVec,
      preprocessor: FittedPreprocessor,
      basis: DMat,
      basisEffectMatrix: DMat
  ): Either[MultivarError, EffectOperator] =
    if loadings.cols != scores.cols || loadings.cols != singularValues.length then
      Left(MultivarError.MatrixShapeMismatch("effect operator component dimensions do not agree"))
    else if loadings.rows != preprocessor.inputCols then
      Left(MultivarError.MatrixShapeMismatch("effect operator loadings do not match preprocessor feature count"))
    else if scores.rows != termFit.rowWhitening.rows then
      Left(MultivarError.MatrixShapeMismatch("effect operator scores do not match row whitening rows"))
    else
      val whitened =
        if singularValues.length == 0 then DMat.zeros(scores.rows, loadings.rows)
        else GaleNumerics.multiply(scores, loadings.transpose)
      termFit.rowWhitening.unwhiten(whitened).map { fitted =>
        EffectOperator(
          termFit,
          loadings,
          scores,
          singularValues,
          preprocessor,
          basis,
          basisEffectMatrix,
          whitened,
          fitted
        )
      }

  private def inverseContribution(
      preprocessor: FittedPreprocessor,
      processed: DMat
  ): Either[MultivarError, DMat] =
    val zero = DMat.zeros(processed.rows, processed.cols)
    for
      original <- preprocessor.inverseTransform(MatrixView.dense(processed), policy = StoragePolicy.AllowDense)
      originalZero <- preprocessor.inverseTransform(MatrixView.dense(zero), policy = StoragePolicy.AllowDense)
      originalDense <- original.toDense(StoragePolicy.AllowDense)
      zeroDense <- originalZero.toDense(StoragePolicy.AllowDense)
    yield RowGeometryOps.subtract(originalDense, zeroDense)

private[multivar] object RowGeometryOps:
  def requireTolerance(role: String, tolerance: Double): Either[MultivarError, Unit] =
    if tolerance.isFinite && tolerance >= 0.0 then Right(())
    else Left(MultivarError.InvalidTolerance(role, tolerance))

  def requireRows(role: String, input: DMat, expected: Int): Either[MultivarError, Unit] =
    if input.rows == expected then Right(())
    else Left(MultivarError.MatrixShapeMismatch(s"$role expected $expected rows, got ${input.rows}"))

  def validateColumnList(
      role: String,
      columns: Iterable[Int],
      allowEmpty: Boolean,
      limit: Option[Int]
  ): Either[MultivarError, Vector[Int]] =
    val values = columns.toVector
    if values.isEmpty && !allowEmpty then Left(MultivarError.EmptyIndexSet(IndexAxis.Column))
    else
      val seen = scala.collection.mutable.HashSet.empty[Int]
      var i = 0
      var error = Option.empty[MultivarError]
      while i < values.length && error.isEmpty do
        val col = values(i)
        if col < 0 then error = Some(MultivarError.IndexOutOfBounds(IndexAxis.Column, col, 0))
        else
          limit match
            case Some(cols) if col >= cols => error = Some(MultivarError.IndexOutOfBounds(IndexAxis.Column, col, cols))
            case _ =>
              if seen.contains(col) then error = Some(MultivarError.DuplicateIndex(IndexAxis.Column, col))
              else seen += col
        i += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(values)

  def requireDisjoint(left: Vector[Int], right: Vector[Int]): Either[MultivarError, Unit] =
    val rightSet = right.toSet
    left.find(rightSet.contains) match
      case Some(value) => Left(MultivarError.DuplicateIndex(IndexAxis.Column, value))
      case None        => Right(())

  /** Fails when the columns of `basis` are not orthonormal within `tolerance`,
    * comparing the b x b Gram matrix `basis' basis` against the identity.
    */
  def requireOrthonormalColumns(
      context: String,
      basis: DMat,
      tolerance: Double
  ): Either[MultivarError, Unit] =
    val gram = GaleNumerics.crossProduct(basis)
    var row = 0
    var error = Option.empty[MultivarError]
    while row < gram.rows && error.isEmpty do
      var col = 0
      while col < gram.cols && error.isEmpty do
        val expected = if row == col then 1.0 else 0.0
        val value = gram(row, col)
        // Fail closed: a NaN Gram entry must reject, so never compare with `>`.
        if !(Math.abs(value - expected) <= tolerance) then
          error = Some(MultivarError.NonOrthonormalBasis(context, row, col, value))
        col += 1
      row += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(())

  def selectRows(matrix: DMat, rows: IndexedSeq[Int]): DMat =
    val out = new Array[Double](rows.length * matrix.cols)
    val data = matrix.copyData
    var outRow = 0
    while outRow < rows.length do
      val sourceRow = rows(outRow)
      System.arraycopy(data, sourceRow * matrix.cols, out, outRow * matrix.cols, matrix.cols)
      outRow += 1
    GaleNumerics.matrixFromRowMajor(rows.length, matrix.cols, out)

  def writeRows(out: Array[Double], cols: Int, rows: IndexedSeq[Int], values: DMat): Unit =
    require(rows.length == values.rows, "row write length must match value rows")
    val data = values.copyData
    var localRow = 0
    while localRow < rows.length do
      val targetRow = rows(localRow)
      System.arraycopy(data, localRow * cols, out, targetRow * cols, cols)
      localRow += 1

  def selectColumns(matrix: DMat, columns: IndexedSeq[Int]): DMat =
    val out = new Array[Double](matrix.rows * columns.length)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < columns.length do
        out(row * columns.length + col) = matrix(row, columns(col))
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(matrix.rows, columns.length, out)

  def concatColumns(left: DMat, right: DMat): DMat =
    require(left.rows == right.rows, "column concatenation requires equal row counts")
    val cols = left.cols + right.cols
    val out = new Array[Double](left.rows * cols)
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        out(row * cols + col) = left(row, col)
        col += 1
      col = 0
      while col < right.cols do
        out(row * cols + left.cols + col) = right(row, col)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(left.rows, cols, out)

  def subtract(left: DMat, right: DMat): DMat =
    MatrixOps.subtract(left, right)

  def trace(matrix: DMat): Double =
    require(matrix.rows == matrix.cols, "trace requires a square matrix")
    var out = 0.0
    var i = 0
    while i < matrix.rows do
      out += matrix(i, i)
      i += 1
    out

  def close(left: DMat, right: DMat, tolerance: Double): Boolean =
    if left.rows != right.rows || left.cols != right.cols then false
    else
      val leftData = left.copyData
      val rightData = right.copyData
      var i = 0
      var ok = true
      while i < leftData.length && ok do
        ok = Math.abs(leftData(i) - rightData(i)) <= tolerance
        i += 1
      ok

  def solveTransposeUpper(
      upper: DMat,
      input: DMat,
      tolerance: Double
  ): Either[MultivarError, DMat] =
    if upper.rows != upper.cols || upper.rows != input.rows then
      Left(MultivarError.MatrixShapeMismatch("upper-transpose triangular solve shape mismatch"))
    else
      val out = new Array[Double](input.rows * input.cols)
      var col = 0
      var error = Option.empty[MultivarError]
      while col < input.cols && error.isEmpty do
        var row = 0
        while row < input.rows && error.isEmpty do
          var sum = input(row, col)
          var k = 0
          while k < row do
            sum -= upper(k, row) * out(k * input.cols + col)
            k += 1
          val diag = upper(row, row)
          if Math.abs(diag) <= tolerance then error = Some(MultivarError.SingularRowMetric("row whitening triangular solve is singular"))
          else out(row * input.cols + col) = sum / diag
          row += 1
        col += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(GaleNumerics.matrixFromRowMajor(input.rows, input.cols, out))

  def solveUpper(
      upper: DMat,
      input: DMat,
      tolerance: Double
  ): Either[MultivarError, DMat] =
    if upper.rows != upper.cols || upper.rows != input.rows then
      Left(MultivarError.MatrixShapeMismatch("upper triangular solve shape mismatch"))
    else
      val out = new Array[Double](input.rows * input.cols)
      var col = 0
      var error = Option.empty[MultivarError]
      while col < input.cols && error.isEmpty do
        var row = input.rows - 1
        while row >= 0 && error.isEmpty do
          var sum = input(row, col)
          var k = row + 1
          while k < input.rows do
            sum -= upper(row, k) * out(k * input.cols + col)
            k += 1
          val diag = upper(row, row)
          if Math.abs(diag) <= tolerance then error = Some(MultivarError.SingularRowMetric("row whitening triangular solve is singular"))
          else out(row * input.cols + col) = sum / diag
          row -= 1
        col += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(GaleNumerics.matrixFromRowMajor(input.rows, input.cols, out))

  def multiplyTransposeUpper(upper: DMat, input: DMat): Either[MultivarError, DMat] =
    if upper.rows != upper.cols || upper.rows != input.rows then
      Left(MultivarError.MatrixShapeMismatch("upper-transpose multiply shape mismatch"))
    else
      val out = new Array[Double](input.rows * input.cols)
      var row = 0
      while row < input.rows do
        var k = 0
        while k <= row do
          val value = upper(k, row)
          var col = 0
          while col < input.cols do
            out(row * input.cols + col) += value * input(k, col)
            col += 1
          k += 1
        row += 1
      Right(GaleNumerics.matrixFromRowMajor(input.rows, input.cols, out))
