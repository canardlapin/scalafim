package scalafim.fmri.group

import gale.linalg.{DVec, Vec}
import scala.collection.immutable.VectorMap

/** Interprets a group model, preparing and checking the shared design once.
  * Global failures are Left values. Partial weighted maps carry identified
  * sample failures through term and contrast results.
  */
object GroupEngine:
  def fit[V <: VarianceCapability](model: GroupModel[V]): Either[GroupError, GroupResult] =
    if model.weighting.requiresVariance && !model.data.hasVariances then
      Left(GroupError.MissingVariances(model.weighting.label))
    else
      GroupGlm.prepare(model.design.matrix).flatMap { prepared =>
        val termNames = model.design.termNames
        val space = model.data.space
        model.data.responses
          .foldLeft[Either[GroupError, VectorMap[String, GroupFit]]](Right(VectorMap.empty)) {
            case (Left(err), _) => Left(err)
            case (Right(fits), (name, response)) =>
              fitContrast(name, response, prepared, termNames, model.weighting, space).map(fit => fits.updated(name, fit))
          }
          .map(fits => GroupResult(model.data.subjects, termNames, space, model.weighting, fits))
      }

  private def fitContrast(
      name: String, response: GroupResponse[? <: VarianceCapability],
      prepared: GroupGlm.Prepared, termNames: Vector[String],
      weighting: GroupWeighting, space: GroupSpace
  ): Either[GroupError, GroupFit] =
    val df = DegreesOfFreedom.unsafe(response.nSubjects - termNames.length)
    weighting match
      case GroupWeighting.Unweighted =>
        GroupGlm.olsPrepared(prepared, response.effects).map { pieces =>
          GroupFit(
            FirstLevelContrastName.unsafe(name), termNames, pieces.coefficients, pieces.standardErrors,
            GroupCovariance.Shared(pieces.inverse, pieces.residualVariance),
            GroupStatistic.unsafeStudentT(pieces.residualDf), None, space
          )
        }
      case GroupWeighting.InverseVariance =>
        response.requireVariances(weighting).flatMap { weighted =>
          GroupGlm.meta(prepared, weighted.effects, weighted.varianceMatrix, None)
            .map(metaFit(name, termNames, _, df, GroupStatistic.Normal, space))
        }
      case GroupWeighting.RandomEffects(tau, inference) =>
        response.requireVariances(weighting).flatMap { weighted =>
          val statistic = inference match
            case MetaInference.Normal => GroupStatistic.Normal
            case MetaInference.ModifiedKnappHartung => GroupStatistic.StudentT(df)
          GroupGlm.meta(prepared, weighted.effects, weighted.varianceMatrix, Some(tau -> inference))
            .map(metaFit(name, termNames, _, df, statistic, space))
        }

  private def metaFit(
      name: String, termNames: Vector[String], pieces: GroupGlm.MetaPieces,
      df: DegreesOfFreedom, statistic: GroupStatistic, space: GroupSpace
  ): GroupFit =
    val fit = pieces.fit
    GroupFit(
      FirstLevelContrastName.unsafe(name), termNames, fit.coefficients, fit.standardErrors,
      GroupCovariance.PerSample(fit.terms, fit.covariance), statistic,
      Some(heterogeneity(pieces.fixedQ, pieces.tau2, df)), space, fit.failures
    )

  private def heterogeneity(q: DVec, tau2: DVec, df: DegreesOfFreedom): Heterogeneity =
    val i2 = Vec.newBuilder(q.length)
    var s = 0
    while s < q.length do
      val qs = q(s)
      i2(s) = if !qs.isFinite then Double.NaN else if qs <= 0.0 then 0.0 else math.max(0.0, (qs - df.value) / qs)
      s += 1
    Heterogeneity(tau2, q, i2.result())
