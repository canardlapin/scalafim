package scalafim.fmri.group

import gale.linalg.{DMat, DVec, Matrix, Vec}

import scala.collection.immutable.VectorMap

/** The interpreter: folds a pure `GroupModel` into typed `GroupResult`s. Total —
  * every failure is a `GroupError` value, never an exception.
  *
  * The estimator dispatch is the whole story: `Unweighted` runs amortized OLS;
  * `InverseVariance` runs weighted least squares with `w = 1/var`;
  * `RandomEffects` runs it once to estimate `tau^2`, then reweights with
  * `w* = 1/(var + tau^2)` and runs it again.
  */
object GroupEngine:

  def fit[V <: VarianceCapability](model: GroupModel[V]): Either[GroupError, GroupResult] =
    if model.weighting.requiresVariance && !model.data.hasVariances then
      Left(GroupError.MissingVariances(model.weighting.label))
    else if model.data.nSubjects <= model.design.terms then
      Left(GroupError.InsufficientSubjects(model.data.nSubjects, model.design.terms))
    else
      val design = model.design.matrix
      val termNames = model.design.termNames
      val space = model.data.space
      model.data.responses
        .foldLeft[Either[GroupError, VectorMap[String, GroupFit]]](Right(VectorMap.empty)) {
          case (Left(err), _) => Left(err)
          case (Right(fits), (name, response)) =>
            fitContrast(name, response, design, termNames, model.weighting, space).map(fit => fits.updated(name, fit))
        }
        .map(fits => GroupResult(model.data.subjects, termNames, space, model.weighting, fits))

  private def fitContrast(
      name: String,
      response: GroupResponse[? <: VarianceCapability],
      design: DMat,
      termNames: Vector[String],
      weighting: GroupWeighting,
      space: GroupSpace
  ): Either[GroupError, GroupFit] =
    val df = DegreesOfFreedom.unsafe(response.nSubjects - termNames.length)
    weighting match
      case GroupWeighting.Unweighted =>
        GroupGlm.ols(design, response.effects).map { pieces =>
          GroupFit(
            contrast = FirstLevelContrastName.unsafe(name),
            termNames = termNames,
            coefficients = pieces.coefficients,
            standardErrors = pieces.standardErrors,
            covariance = GroupCovariance.Shared(pieces.inverse, pieces.residualVariance),
            statistic = GroupStatistic.unsafeStudentT(pieces.residualDf),
            heterogeneity = None,
            space = space
          )
        }

      case GroupWeighting.InverseVariance =>
        response.requireVariances(weighting).map { weighted =>
          fitInverseVariance(name, weighted, design, termNames, df, space)
        }

      case GroupWeighting.RandomEffects(tau) =>
        response.requireVariances(weighting).map { weighted =>
          fitRandomEffects(name, weighted, design, termNames, tau, df, space)
        }

  private def fitInverseVariance(
      name: String,
      response: GroupResponse.WithVariances,
      design: DMat,
      termNames: Vector[String],
      df: DegreesOfFreedom,
      space: GroupSpace
  ): GroupFit =
    val pieces = GroupGlm.wls(design, response.effects, reciprocal(response.varianceMatrix))
    // Fixed effects assume no between-subject variance; keep it NaN-consistent
    // with Q on singular samples.
    metaFit(name, termNames, pieces, tau2 = fixedEffectsTau2(pieces.q), qSource = pieces.q, df = df, space = space)

  private def fitRandomEffects(
      name: String,
      response: GroupResponse.WithVariances,
      design: DMat,
      termNames: Vector[String],
      tau: TauEstimator,
      df: DegreesOfFreedom,
      space: GroupSpace
  ): GroupFit =
    val variances = response.varianceMatrix
    val fixedPieces = GroupGlm.wls(design, response.effects, reciprocal(variances))
    val tau2 = estimateTau2(tau, fixedPieces, df)
    val randomPieces = GroupGlm.wls(design, response.effects, reweight(variances, tau2))
    // Coefficients from the reweighted fit; heterogeneity from the fixed-effects pass.
    metaFit(name, termNames, randomPieces, tau2 = tau2, qSource = fixedPieces.q, df = df, space = space)

  private def metaFit(
      name: String,
      termNames: Vector[String],
      pieces: GroupGlm.WlsPieces,
      tau2: DVec,
      qSource: DVec,
      df: DegreesOfFreedom,
      space: GroupSpace
  ): GroupFit =
    GroupFit(
      contrast = FirstLevelContrastName.unsafe(name),
      termNames = termNames,
      coefficients = pieces.coefficients,
      standardErrors = pieces.standardErrors,
      covariance = GroupCovariance.PerSample(pieces.terms, pieces.covariance),
      statistic = GroupStatistic.Normal,
      heterogeneity = Some(heterogeneity(qSource, tau2, df)),
      space = space
    )

  /** Dispatch the between-subject variance estimator. Exhaustive on `TauEstimator`
    * so a future estimator is a compile error here, not a silent DL fallback.
    */
  private def estimateTau2(tau: TauEstimator, pieces: GroupGlm.WlsPieces, df: DegreesOfFreedom): DVec =
    tau match
      case TauEstimator.DerSimonianLaird => derSimonianLaird(pieces, df)

  /** DerSimonian–Laird between-subject variance per sample: `max(0, (Q − df)/C)`. */
  private def derSimonianLaird(pieces: GroupGlm.WlsPieces, df: DegreesOfFreedom): DVec =
    val n = pieces.q.length
    val out = Vec.newBuilder(n)
    var s = 0
    while s < n do
      val q = pieces.q(s)
      val c = pieces.c(s)
      out(s) =
        if !q.isFinite || !c.isFinite then Double.NaN
        else if c <= 0.0 then 0.0
        else math.max(0.0, (q - df.value) / c)
      s += 1
    out.result()

  private def fixedEffectsTau2(q: DVec): DVec =
    val n = q.length
    val out = Vec.newBuilder(n)
    var s = 0
    while s < n do
      out(s) = if q(s).isFinite then 0.0 else Double.NaN
      s += 1
    out.result()

  private def heterogeneity(q: DVec, tau2: DVec, df: DegreesOfFreedom): Heterogeneity =
    val n = q.length
    val i2 = Vec.newBuilder(n)
    val qCopy = Vec.newBuilder(n)
    var s = 0
    while s < n do
      val qs = q(s)
      qCopy(s) = qs
      i2(s) =
        if !qs.isFinite then Double.NaN
        else if qs <= 0.0 then 0.0
        else math.max(0.0, (qs - df.value) / qs)
      s += 1
    Heterogeneity(tau2, qCopy.result(), i2.result())

  private def reciprocal(m: DMat): DMat =
    val out = Matrix.newBuilder(m.rows, m.cols)
    var row = 0
    while row < m.rows do
      var col = 0
      while col < m.cols do
        out(row, col) = 1.0 / m(row, col)
        col += 1
      row += 1
    out.result()

  private def reweight(variances: DMat, tau2: DVec): DMat =
    val n = variances.rows
    val samples = variances.cols
    val out = Matrix.newBuilder(n, samples)
    var i = 0
    while i < n do
      var s = 0
      while s < samples do
        out(i, s) = 1.0 / (variances(i, s) + tau2(s))
        s += 1
      i += 1
    out.result()
