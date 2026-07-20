package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

/** Backend selection for the generalized matrix decomposition. */
enum GmdBackend:
  /** Reference path: eigendecomposition of the smaller metric-whitened Gram matrix. */
  case Eigen(rankTolerance: Double = 1e-12)
  /** Matrix-free generalized power iteration with implicit deflation; never forms a
    * metric square root, so sparse data and sparse metrics stay sparse. `threshold`
    * is the convergence tolerance on the scale-free alignment residual;
    * `rankTolerance` is the relative rank/degeneracy cutoff (as in Eigen).
    */
  case Deflation(
      threshold: Double = 1e-6,
      maxIterations: Int = 500,
      seed: Long = GmdBackend.DefaultSeed,
      rankTolerance: Double = 1e-12
  )
  /** Deterministic heuristic between Eigen and Deflation. */
  case Auto

object GmdBackend:
  private[multivar] val DefaultSeed: Long = 0x9E3779B97F4A7C15L
  private[multivar] val EigenDimCap: Int = 800

  /** Resolve Auto to a concrete backend; Eigen and Deflation pass through. */
  private[multivar] def resolve(
      backend: GmdBackend,
      x: MatrixView,
      rowMetric: MvMetric,
      colMetric: MvMetric,
      policy: StoragePolicy
  ): GmdBackend =
    backend match
      case resolved @ (Eigen(_) | Deflation(_, _, _, _)) =>
        resolved
      case Auto =>
        val primal = x.rows >= x.cols || x.storage != StorageKind.Dense
        val sqrtSide = if primal then colMetric else rowMetric
        val gramSide = if primal then rowMetric else colMetric
        val allowDense = policy == StoragePolicy.AllowDense
        val gramFeasible = x.storage == StorageKind.Dense || gramSide.isDiagonal || allowDense
        val sqrtFeasible = sqrtSide match
          case MvMetric.SparseSymmetric(_, _) => allowDense
          case _                              => true
        if !gramFeasible || !sqrtFeasible then Deflation()
        else if Math.min(x.rows, x.cols) <= EigenDimCap then Eigen()
        else Deflation()

  private[multivar] def label(backend: GmdBackend): String =
    backend match
      case Eigen(_)               => "eigen"
      case Deflation(_, _, _, _)  => "deflation"
      case Auto                   => "auto"

  private[multivar] def tolerance(backend: GmdBackend): Option[Double] =
    backend match
      case Eigen(rankTolerance)              => Some(rankTolerance)
      case Deflation(threshold, _, _, _)     => Some(threshold)
      case Auto                              => None

/** Generalized SVD factors: X ~ ou * diag(d) * ov', with ou' M ou = I and ov' A ov = I. */
final case class GmdResult(ou: DoubleMatrix, d: DoubleVector, ov: DoubleMatrix):
  require(ou.cols == d.length, "left generalized vectors must match singular value count")
  require(ov.cols == d.length, "right generalized vectors must match singular value count")

private[multivar] final case class GmdDecomposition(result: GmdResult, totalVariance: Double)

/** A fitted generalized PCA: metric-orthonormal factors plus a standard BiProjection.
  *
  * `projection.scores` follow the module contract (`map.forward(x)`, projectable onto
  * new rows); the R genpca training scores `M ou diag(d)` are exposed separately as
  * `metricScores` because the row metric is tied to the training rows.
  */
final case class GenPcaFit private[multivar] (
    result: GmdResult,
    projection: BiProjection,
    rowMetric: MvMetric,
    colMetric: MvMetric,
    preprocessor: FittedPreprocessor,
    u: DoubleMatrix,
    v: DoubleMatrix,
    totalVariance: Double
):
  def ou: DoubleMatrix = result.ou
  def ov: DoubleMatrix = result.ov
  def d: DoubleVector = result.d
  def componentCount: Int = result.d.length

  /** R genpca training scores s = M ou diag(d); defined only for the training rows. */
  lazy val metricScores: DoubleMatrix =
    MetricOperator.scaleColumnsDense(u, result.d)

  /** Proportion of generalized variance per component: d_k^2 / tr(X' M X A). */
  lazy val propV: DoubleVector =
    val out = new Array[Double](result.d.length)
    if totalVariance > 0.0 then
      var i = 0
      while i < out.length do
        out(i) = result.d(i) * result.d(i) / totalVariance
        i += 1
    DoubleVector.unsafe(out)

  def project(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    projection.project(input)

  /** Low-rank reconstruction on the original scale: ou_k diag(d_k) ov_k' inverse-preprocessed. */
  def reconstruct(components: Option[ComponentCount] = None): Either[MultivarError, DoubleMatrix] =
    val k = components.map(_.value).getOrElse(componentCount)
    if k > componentCount then Left(MultivarError.InvalidComponentRequest(k, componentCount))
    else
      val scaled = MetricOperator.scaleColumnsDense(
        MatrixOps.takeColumns(result.ou, k),
        MatrixOps.takeVector(result.d, k)
      )
      val lowRank = DoubleMatrix.multiply(scaled, MatrixOps.takeColumns(result.ov, k).transpose)
      preprocessor
        .inverseTransform(MatrixView.dense(lowRank), policy = StoragePolicy.AllowDense)
        .flatMap(_.toDense(StoragePolicy.AllowDense))

  /** Keep the leading k components; scores and factors are sliced, nothing is refit. */
  def truncate(components: ComponentCount): Either[MultivarError, GenPcaFit] =
    val k = components.value
    if k > componentCount then Left(MultivarError.InvalidComponentRequest(k, componentCount))
    else
      val truncatedResult = GmdResult(
        MatrixOps.takeColumns(result.ou, k),
        MatrixOps.takeVector(result.d, k),
        MatrixOps.takeColumns(result.ov, k)
      )
      val truncatedV = MatrixOps.takeColumns(v, k)
      val previousLatent = projection.map.codomain
      val latent = MvSpace(previousLatent.id, previousLatent.role, Dimension.unsafe(k))
      MatrixMap.from(projection.map.domain, latent, truncatedV, preprocessor).map { map =>
        val previousDiagnostics = projection.diagnostics
        val diagnostics = ProjectionDiagnostics(
          method = previousDiagnostics.map(_.method).getOrElse("genpca"),
          components = components,
          effectiveComponents = k,
          singularValues = Some(truncatedResult.d),
          backend = previousDiagnostics.flatMap(_.backend),
          storagePolicy = previousDiagnostics.flatMap(_.storagePolicy),
          tolerance = previousDiagnostics.flatMap(_.tolerance)
        )
        GenPcaFit(
          truncatedResult,
          BiProjection(
            map,
            MatrixOps.takeColumns(projection.scores, k),
            scale = Some(ComponentScale(truncatedResult.d)),
            diagnostics = Some(diagnostics)
          ),
          rowMetric,
          colMetric,
          preprocessor,
          MatrixOps.takeColumns(u, k),
          truncatedV,
          totalVariance
        )
      }

object GenPca:
  def fit(
      diagram: DualityDiagram,
      components: ComponentCount
  ): Either[MultivarError, GenPcaFit] =
    fit(
      diagram,
      components,
      PreprocessSpec.Center,
      GmdBackend.Auto,
      StoragePolicy.AllowDense,
      DenseSolvers.symmetricEigen,
      DenseSolvers.svd
    )

  def fit(
      diagram: DualityDiagram,
      components: ComponentCount,
      preproc: PreprocessSpec,
      backend: GmdBackend,
      policy: StoragePolicy,
      eigenSolver: SymmetricEigenSolver,
      svdSolver: SvdSolver
  ): Either[MultivarError, GenPcaFit] =
    val limit = Math.min(diagram.rows, diagram.cols)
    if components.value > limit then Left(MultivarError.InvalidComponentRequest(components.value, limit))
    else
      fitResolved(
        diagram.table,
        components,
        diagram.rowMetric,
        diagram.columnMetric,
        preproc,
        backend,
        policy,
        eigenSolver,
        svdSolver,
        diagram.columnSpace,
        method = "genpca",
        latentId = "genpca.latent"
      )

  /** Fit a generalized PCA of `x` under a row metric M (n x n) and column metric A (p x p).
    *
    * `None` metrics mean unweighted (identity). Fewer than `components` factors are
    * returned when the generalized spectrum ranks below the request.
    *
    * This compatibility overload creates anonymous row and column spaces. New code
    * should construct a [[SemanticDualityDiagram]] or use
    * [[Unsafe.genPcaFromArrays]] with an explicit provenance reason.
    */
  @deprecated(
    "Construct SemanticDualityDiagram and use SemanticGenPca.fit, or call Unsafe.genPcaFromArrays with an explicit reason",
    "0.1.0"
  )
  def fit(
      x: MatrixView,
      components: ComponentCount,
      rowMetric: Option[MvMetric] = None,
      colMetric: Option[MvMetric] = None,
      preproc: PreprocessSpec = PreprocessSpec.Center,
      backend: GmdBackend = GmdBackend.Auto,
      policy: StoragePolicy = StoragePolicy.AllowDense,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      svdSolver: SvdSolver = DenseSolvers.svd
  ): Either[MultivarError, GenPcaFit] =
    Unsafe.genPcaFromArrays(
      x,
      components,
      reason = "deprecated GenPca.fit raw-array compatibility overload",
      rowMetric = rowMetric,
      colMetric = colMetric,
      preproc = preproc,
      backend = backend,
      policy = policy,
      eigenSolver = eigenSolver,
      svdSolver = svdSolver
    )

  private[multivar] def fitRawUnsafe(
      x: MatrixView,
      components: ComponentCount,
      rowMetric: Option[MvMetric],
      colMetric: Option[MvMetric],
      preproc: PreprocessSpec,
      backend: GmdBackend,
      policy: StoragePolicy,
      eigenSolver: SymmetricEigenSolver,
      svdSolver: SvdSolver
  ): Either[MultivarError, GenPcaFit] =
    val limit = Math.min(x.rows, x.cols)
    val rm = rowMetric.getOrElse(MvMetric.unsafeIdentity(x.rows))
    val cm = colMetric.getOrElse(MvMetric.unsafeIdentity(x.cols))
    if components.value > limit then Left(MultivarError.InvalidComponentRequest(components.value, limit))
    else if rm.dim != x.rows then Left(MultivarError.MetricShapeMismatch(IndexAxis.Row, x.rows, rm.dim))
    else if cm.dim != x.cols then Left(MultivarError.MetricShapeMismatch(IndexAxis.Column, x.cols, cm.dim))
    else
      fitResolved(
        x,
        components,
        rm,
        cm,
        preproc,
        backend,
        policy,
        eigenSolver,
        svdSolver,
        MvSpace(SpaceId.unsafe("genpca.observed"), SpaceRole.Observed, Dimension.unsafe(x.cols)),
        method = "genpca",
        latentId = "genpca.latent"
      )

  private[multivar] def identityBackend(
      x: MatrixView,
      components: ComponentCount,
      preproc: PreprocessSpec,
      svdSolver: SvdSolver,
      observedSpace: MvSpace,
      method: String,
      latentId: String = "genpca.latent",
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, GenPcaFit] =
    val limit = Math.min(x.rows, x.cols)
    if components.value > limit then Left(MultivarError.InvalidComponentRequest(components.value, limit))
    else
      fitResolved(
        x,
        components,
        MvMetric.unsafeIdentity(x.rows),
        MvMetric.unsafeIdentity(x.cols),
        preproc,
        GmdBackend.Auto,
        policy,
        DenseSolvers.symmetricEigen,
        svdSolver,
        observedSpace,
        method,
        latentId
      )

  private def fitResolved(
      x: MatrixView,
      components: ComponentCount,
      rm: MvMetric,
      cm: MvMetric,
      preproc: PreprocessSpec,
      backend: GmdBackend,
      policy: StoragePolicy,
      eigenSolver: SymmetricEigenSolver,
      svdSolver: SvdSolver,
      observedSpace: MvSpace,
      method: String,
      latentId: String
  ): Either[MultivarError, GenPcaFit] =
    for
      fitted <- preproc.fit(x)
      transformed <- fitted.transform(x)
      out <- {
        val resolvedBackend = GmdBackend.resolve(backend, transformed, rm, cm, policy)
        // The identity Gram-SVD shortcut is only what the eigen primal branch would
        // compute anyway (a p x p Gram), so it must not shadow an explicitly resolved
        // deflation backend and must leave wide data to the n x n dual branch.
        val identityShortcut =
          rm.isIdentity && cm.isIdentity &&
            (resolvedBackend match
              case GmdBackend.Eigen(_) => transformed.rows >= transformed.cols
              case _                   => false)
        if identityShortcut then
          identityFit(x, transformed, fitted, rm, cm, components, svdSolver, observedSpace, policy, method, latentId)
        else
          val engine = resolvedBackend match
            case GmdBackend.Eigen(rankTolerance) =>
              EigenGmd(rankTolerance)
            case GmdBackend.Deflation(threshold, maxIterations, seed, rankTolerance) =>
              DeflationGmd(threshold, maxIterations, seed, rankTolerance)
            case GmdBackend.Auto =>
              EigenGmd()
          engine
            .decompose(transformed, rm, cm, components.value, eigenSolver, policy)
            .flatMap(
              assemble(
                x,
                fitted,
                rm,
                cm,
                _,
                components,
                observedSpace,
                backend = Some(GmdBackend.label(resolvedBackend)),
                storagePolicy = Some(policy),
                tolerance = GmdBackend.tolerance(resolvedBackend),
                method = method,
                latentId = latentId
              )
            )
      }
    yield out

  /** Both metrics identity: exactly the Pca/Svd path, adapted into GMD factors. */
  private def identityFit(
      x: MatrixView,
      transformed: MatrixView,
      fitted: FittedPreprocessor,
      rm: MvMetric,
      cm: MvMetric,
      components: ComponentCount,
      svdSolver: SvdSolver,
      observedSpace: MvSpace,
      policy: StoragePolicy,
      method: String,
      latentId: String
  ): Either[MultivarError, GenPcaFit] =
    for
      svd <- svdSolver.decompose(transformed, components)
      stats <- transformed.columnStats
      fit <- assemble(
        x,
        fitted,
        rm,
        cm,
        GmdDecomposition(
          GmdResult(svd.u, svd.singularValues, svd.v),
          totalVariance = totalVarianceOf(stats)
        ),
        components,
        observedSpace,
        backend = Some("identity-svd"),
        storagePolicy = Some(policy),
        tolerance = None,
        method = method,
        latentId = latentId
      )
    yield fit

  private def assemble(
      x: MatrixView,
      fitted: FittedPreprocessor,
      rm: MvMetric,
      cm: MvMetric,
      decomposition: GmdDecomposition,
      requested: ComponentCount,
      observedSpace: MvSpace,
      backend: Option[String],
      storagePolicy: Option[StoragePolicy],
      tolerance: Option[Double],
      method: String,
      latentId: String
  ): Either[MultivarError, GenPcaFit] =
    val result = decomposition.result
    val k = result.d.length
    if k <= 0 then Left(MultivarError.SolverFailed("no generalized components survived the rank tolerance"))
    else
      for
        u <- rm.matvec(result.ou)
        v <- cm.matvec(result.ov)
        domain = observedSpace
        latent = MvSpace(SpaceId.unsafe(latentId), SpaceRole.Latent, Dimension.unsafe(k))
        map <- MatrixMap.from(domain, latent, v, fitted)
        scores <- map.forward(x)
      yield
        val diagnostics = ProjectionDiagnostics(
          method = method,
          components = requested,
          effectiveComponents = k,
          singularValues = Some(result.d),
          backend = backend,
          storagePolicy = storagePolicy,
          tolerance = tolerance
        )
        GenPcaFit(
          result,
          BiProjection(map, scores, scale = Some(ComponentScale(result.d)), diagnostics = Some(diagnostics)),
          rm,
          cm,
          fitted,
          u,
          v,
          decomposition.totalVariance
        )

  /** Total sum of squares from column stats, preferring the cancellation-free
    * decomposition sumSq = centeredSS + n * mean^2 when centered sums are available.
    * Lazily shifted views (a sparse table under Center/Standardize) propagate raw
    * second moments with catastrophic cancellation at large column means, while
    * their centered sums propagate exactly.
    */
  private def totalVarianceOf(stats: ColumnStats): Double =
    stats.centeredSumSquares match
      case Some(centered) if stats.count > 0 =>
        var acc = 0.0
        var col = 0
        while col < centered.length do
          val mean = stats.sums(col) / stats.count
          acc += centered(col) + stats.count * mean * mean
          col += 1
        acc
      case _ =>
        var acc = 0.0
        var i = 0
        while i < stats.sumSquares.length do
          acc += stats.sumSquares(i)
          i += 1
        acc

private[multivar] trait GmdEngine:
  def decompose(
      x: MatrixView,
      rowMetric: MvMetric,
      colMetric: MvMetric,
      components: Int,
      eigenSolver: SymmetricEigenSolver,
      policy: StoragePolicy
  ): Either[MultivarError, GmdDecomposition]

/** Reference backend: eigendecomposition of the metric-whitened Gram on the smaller side.
  *
  * Primal (n >= p, and always for non-dense data): eigen of A^{1/2} (X'MX) A^{1/2};
  * dual otherwise: eigen of M^{1/2} (XAX') M^{1/2}. Only the smaller-side metric is
  * ever square-rooted.
  */
private[multivar] final case class EigenGmd(rankTolerance: Double = 1e-12) extends GmdEngine:
  override def decompose(
      x: MatrixView,
      rowMetric: MvMetric,
      colMetric: MvMetric,
      components: Int,
      eigenSolver: SymmetricEigenSolver,
      policy: StoragePolicy
  ): Either[MultivarError, GmdDecomposition] =
    val primal = x.rows >= x.cols || x.storage != StorageKind.Dense
    if primal then decomposePrimal(x, rowMetric, colMetric, components, eigenSolver, policy)
    else decomposeDual(x, rowMetric, colMetric, components, eigenSolver, policy)

  private def decomposePrimal(
      x: MatrixView,
      rowMetric: MvMetric,
      colMetric: MvMetric,
      components: Int,
      eigenSolver: SymmetricEigenSolver,
      policy: StoragePolicy
  ): Either[MultivarError, GmdDecomposition] =
    for
      gram <- DualityKernels.rowGram(x, rowMetric, policy)
      roots <- MetricSqrt.factor(colMetric, eigenSolver, rankTolerance, policy, "column metric")
      target = DualityKernels.symmetrize(roots.half.applyLeft(roots.half.applyRight(gram)))
      eigen <- LinalgErrorAdapter.adapt(eigenSolver.decompose(target))
      totalVariance <- colMetric.contract(gram)
      decomposition <- {
        val cutoff = cutoffFor(eigen.values)
        val kEff = keptComponents(eigen.values, cutoff, components)
        if kEff <= 0 then Left(MultivarError.SolverFailed("no generalized components above tolerance"))
        else
          val ov = roots.pinvHalf.applyLeft(MatrixOps.takeColumns(eigen.vectors, kEff))
          colMetric.matvec(ov).flatMap { w =>
            val gw = DoubleMatrix.multiply(gram, w)
            val norms = new Array[Double](kEff)
            var finalK = kEff
            var j = 0
            while j < finalK do
              var acc = 0.0
              var i = 0
              while i < w.rows do
                acc += w(i, j) * gw(i, j)
                i += 1
              if acc > cutoff then norms(j) = Math.sqrt(acc)
              else finalK = j
              j += 1
            if finalK <= 0 then Left(MultivarError.SolverFailed("no generalized components above tolerance"))
            else
              x.rightMultiply(MatrixOps.takeColumns(w, finalK)).map { xw =>
                val ouData = xw.copyData
                var row = 0
                while row < xw.rows do
                  var col = 0
                  while col < finalK do
                    ouData(row * finalK + col) /= norms(col)
                    col += 1
                  row += 1
                val d = new Array[Double](finalK)
                var c = 0
                while c < finalK do
                  d(c) = Math.sqrt(Math.max(eigen.values(c), 0.0))
                  c += 1
                GmdDecomposition(
                  GmdResult(
                    DoubleMatrix.unsafe(xw.rows, finalK, ouData),
                    DoubleVector.unsafe(d),
                    MatrixOps.takeColumns(ov, finalK)
                  ),
                  totalVariance
                )
              }
          }
      }
    yield decomposition

  private def decomposeDual(
      x: MatrixView,
      rowMetric: MvMetric,
      colMetric: MvMetric,
      components: Int,
      eigenSolver: SymmetricEigenSolver,
      policy: StoragePolicy
  ): Either[MultivarError, GmdDecomposition] =
    for
      gram <- DualityKernels.colGram(x, colMetric, policy)
      roots <- MetricSqrt.factor(rowMetric, eigenSolver, rankTolerance, policy, "row metric")
      target = DualityKernels.symmetrize(roots.half.applyLeft(roots.half.applyRight(gram)))
      eigen <- LinalgErrorAdapter.adapt(eigenSolver.decompose(target))
      totalVariance <- rowMetric.contract(gram)
      decomposition <- {
        val cutoff = cutoffFor(eigen.values)
        val kEff = keptComponents(eigen.values, cutoff, components)
        if kEff <= 0 then Left(MultivarError.SolverFailed("no generalized components above tolerance"))
        else
          val ou = roots.pinvHalf.applyLeft(MatrixOps.takeColumns(eigen.vectors, kEff))
          rowMetric.matvec(ou).flatMap { mu =>
            x.transposeMultiply(DenseMatrixView(mu)).flatMap { xtmu =>
              val d = new Array[Double](kEff)
              var c = 0
              while c < kEff do
                d(c) = Math.sqrt(Math.max(eigen.values(c), 0.0))
                c += 1
              // ov = X' M ou D^{-1}, renormalized per column in the A metric.
              val ovData = xtmu.copyData
              var finalK = kEff
              var metricError = Option.empty[MultivarError]
              var col = 0
              while col < finalK && metricError.isEmpty do
                val column = new Array[Double](xtmu.rows)
                var row = 0
                while row < xtmu.rows do
                  column(row) = ovData(row * kEff + col) / d(col)
                  row += 1
                // The quad norm guards rank deficiency: for a true component the
                // GMD normalization makes it ~1 by construction, while a dual
                // eigenvector mapped into a null column direction gives ~0. It is
                // dimensionless, so it is compared against the relative tolerance
                // itself — never against the eigenvalue-scaled spectrum cutoff.
                colMetric.quadNorm(DoubleVector.unsafe(column)) match
                  case Right(quad) if quad > rankTolerance =>
                    val inv = 1.0 / Math.sqrt(quad)
                    row = 0
                    while row < xtmu.rows do
                      ovData(row * kEff + col) = column(row) * inv
                      row += 1
                  case Right(_) =>
                    // Below the rank tolerance: truncate here, as the primal path does.
                    finalK = col
                  case Left(error) =>
                    metricError = Some(error)
                col += 1
              if metricError.isDefined then Left(metricError.get)
              else if finalK <= 0 then Left(MultivarError.SolverFailed("no generalized components above tolerance"))
              else
                Right(
                  GmdDecomposition(
                    GmdResult(
                      MatrixOps.takeColumns(ou, finalK),
                      MatrixOps.takeVector(DoubleVector.unsafe(d), finalK),
                      MatrixOps.takeColumns(DoubleMatrix.unsafe(xtmu.rows, kEff, ovData), finalK)
                    ),
                    totalVariance
                  )
                )
            }
          }
      }
    yield decomposition

  /** Rank cutoff for the data spectrum, purely relative to its leading eigenvalue so
    * small-scale data keeps its full rank; metric-side spectra keep their absolute
    * floor inside MetricSqrt, where weights are expected to be O(1).
    */
  private def cutoffFor(values: DoubleVector): Double =
    rankTolerance * Math.max(values(0), 0.0)

  private def keptComponents(values: DoubleVector, cutoff: Double, requested: Int): Int =
    var kept = 0
    while kept < values.length && values(kept) > cutoff do kept += 1
    Math.min(requested, kept)
