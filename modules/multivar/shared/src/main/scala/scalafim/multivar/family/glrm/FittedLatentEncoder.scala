package scalafim.multivar
package family.glrm

import scalafim.multivar.core.*
import scalafim.multivar.contract.*
import scalafim.multivar.optimization.*

import gale.linalg.DMat
import gale.linalg.DVec
import scalafim.linalg.DoubleMatrix
import scalafim.linalg.FirstOrderCertificate
import scalafim.linalg.FirstOrderConfig
import scalafim.linalg.FirstOrderError
import scalafim.linalg.FirstOrderSolvers
import scalafim.linalg.FirstOrderStoppingStatus
import scalafim.linalg.GaleMatrixBridge
import scalafim.linalg.ProximalTerm
import scalafim.linalg.SmoothObjective

enum LatentEncodingError:
  case InvalidDefinition(detail: String)
  case EmptySupport
  case UnseenCategoricalLevel(feature: GlrmFeatureId, value: Double, levels: CategoryLevels)
  case UnsupportedLoss(feature: GlrmFeatureId, loss: EntryLoss, detail: String)
  case CensoredObservation(feature: Int, interval: CensoringInterval)
  case NonIdentifiableCode(detail: String)
  case Observation(error: GeneralizedLowRankError)
  case Solver(error: FirstOrderError)
  case Guarantee(error: OptimizationGuaranteeError)
  case Multivar(error: MultivarError)

  def message: String =
    this match
      case InvalidDefinition(detail) => detail
      case EmptySupport => "partial latent encoding requires at least one point-observed feature"
      case UnseenCategoricalLevel(feature, value, levels) =>
        s"feature ${feature.value} has unseen categorical level index $value; fitted levels are ${levels.values.mkString(", ")}"
      case UnsupportedLoss(feature, loss, detail) =>
        s"feature ${feature.value} loss $loss is unsupported by the current latent encoder: $detail"
      case CensoredObservation(feature, interval) =>
        s"feature index $feature is $interval; latent encoding requires an explicit censoring likelihood"
      case NonIdentifiableCode(detail) => s"latent code is non-identifiable: $detail"
      case Observation(error) => error.message
      case Solver(error) => error.message
      case Guarantee(error) => error.message
      case Multivar(error) => error.message

final case class LatentEncodingSupport(
    features: IndexSet,
    observedCount: Int,
    totalWeight: Double,
    patternIdentity: ValueIdentity
):
  require(observedCount == features.length)
  require(totalWeight.isFinite && totalWeight > 0.0)

enum LatentCodeUniqueness:
  case UniqueByStrongConvexity(modulus: StrongConvexityModulus)
  case NotCertified(reason: String)

final class FittedLatentCode[Latent <: SemanticSpace] private (
    val latentSpace: SpaceEvidence[Latent],
    val values: DVec,
    val valueIdentity: ValueIdentity
)

object FittedLatentCode:
  private[multivar] def from[Latent <: SemanticSpace](
      latentSpace: SpaceEvidence[Latent],
      values: DVec,
      valueIdentity: ValueIdentity
  ): Either[LatentEncodingError, FittedLatentCode[Latent]] =
    if values.length != latentSpace.dimension then
      Left(
        LatentEncodingError.InvalidDefinition(
          s"latent code requires ${latentSpace.dimension} values, got ${values.length}"
        )
      )
    else
      var index = 0
      var failure = Option.empty[(Int, Double)]
      while index < values.length && failure.isEmpty do
        if !values(index).isFinite then failure = Some(index -> values(index))
        index += 1
      failure match
        case Some((position, value)) =>
          Left(LatentEncodingError.InvalidDefinition(s"latent code value $position is not finite: $value"))
        case None => Right(new FittedLatentCode(latentSpace, values, valueIdentity))

final case class DecodedLatentFeature(
    feature: GlrmFeatureId,
    prediction: DecodedPrediction
)

final case class LatentEncodingObjective(
    observedEntryLoss: Double,
    rowPenalty: Double
):
  require(observedEntryLoss.isFinite && observedEntryLoss >= 0.0)
  require(rowPenalty.isFinite && rowPenalty >= 0.0)

  def total: Double = observedEntryLoss + rowPenalty

final case class LatentEncodingCertificate(
    proxGradientResidual: Double,
    objectiveChange: Double,
    iterations: Int,
    lipschitzUpperBound: Double,
    numerical: FirstOrderCertificate
):
  require(proxGradientResidual.isFinite && proxGradientResidual >= 0.0)
  require(objectiveChange.isFinite && objectiveChange >= 0.0)
  require(iterations >= 0)
  require(lipschitzUpperBound.isFinite && lipschitzUpperBound > 0.0)

final case class FittedLatentEncoding[Latent <: SemanticSpace](
    code: FittedLatentCode[Latent],
    support: LatentEncodingSupport,
    objective: LatentEncodingObjective,
    decoded: Vector[DecodedLatentFeature],
    certificate: LatentEncodingCertificate,
    uniqueness: LatentCodeUniqueness,
    achievedGuarantee: AchievedOptimizationGuarantee,
    resultIdentity: ValueIdentity
)

/** Nonlinear new-row inference against a frozen GLRM decoder.
  *
  * This is deliberately unrelated to [[FittedProjection]]: the code is the
  * solution of a convex observed-entry problem, not a matrix multiplication by
  * a fitted linear frame.
  */
final class FittedLatentEncoder[
    Feature <: SemanticSpace,
    Latent <: SemanticSpace
] private (
    val decoder: FeatureDecoder[Feature, Latent],
    val rowPenalties: Vector[GlrmFactorPenaltyTerm],
    val config: FirstOrderConfig,
    val encoderIdentity: ValueIdentity
):
  def encode[Rows <: SemanticSpace](
      observations: ObservationPattern[Rows, Feature],
      initial: Option[DVec] = None
  ): Either[LatentEncodingError, FittedLatentEncoding[Latent]] =
    if observations.rowSpace.dimension != 1 then
      Left(LatentEncodingError.InvalidDefinition("partial latent encoding accepts exactly one new row"))
    else if observations.featureSpace.descriptor != decoder.layout.featureSpace.descriptor then
      Left(LatentEncodingError.InvalidDefinition("partial observations use a foreign feature domain"))
    else
      for
        entries <- encodingEntries(observations)
        _ <- if entries.nonEmpty then Right(()) else Left(LatentEncodingError.EmptySupport)
        initialCode <- initialMatrix(initial)
        ridgeWeight = penaltyWeight(GlrmFactorPenalty.SquaredFrobenius)
        l1Weight = penaltyWeight(GlrmFactorPenalty.ElementwiseL1)
        lipschitz = encodingLipschitz(entries, ridgeWeight)
        _ <-
          if lipschitz > 0.0 && lipschitz.isFinite then Right(())
          else
            Left(
              LatentEncodingError.NonIdentifiableCode(
                "the observed decoder slices are zero and no strongly convex row penalty is present"
              )
            )
        smooth = new EncodingSmoothObjective(decoder, entries, ridgeWeight, lipschitz)
        direct = new EncodingL1Term(decoder.latentSpace.dimension, l1Weight)
        solution <- FirstOrderSolvers
          .proximalGradient(smooth, direct, GaleMatrixBridge.fromGale(initialCode), config)
          .left
          .map(LatentEncodingError.Solver.apply)
        codeMatrix = GaleMatrixBridge.toGale(solution.primal)
        codeValues = glrmEncodingColumn(codeMatrix)
        resultIdentity = ValueIdentity.derived(
          "fitted-latent-encoding",
          encoderIdentity,
          observations.valueIdentity
        )
        code <- FittedLatentCode.from(decoder.latentSpace, codeValues, resultIdentity)
        observedLoss <- smooth.entryValue(codeMatrix).left.map(LatentEncodingError.Observation.apply)
        rowPenalty = glrmEncodingPenalty(codeMatrix, ridgeWeight, l1Weight)
        objective = LatentEncodingObjective(observedLoss, rowPenalty)
        _ <-
          if Math.abs(objective.total - solution.objective) <= 1e-7 * Math.max(1.0, objective.total) then Right(())
          else
            Left(
              LatentEncodingError.InvalidDefinition(
                s"latent encoding objective ${objective.total} does not bind solver objective ${solution.objective}"
              )
            )
        decoded <- decodeAll(observations.rowSpace, codeMatrix, resultIdentity)
        support = LatentEncodingSupport(
          IndexSet.unsafe(entries.map(_.feature)),
          entries.length,
          entries.map(_.weight).sum,
          observations.valueIdentity
        )
        uniqueness <- uniquenessStatus(entries, ridgeWeight)
        residual <- NonNegativeProofBound
          .residual(solution.certificate.primalResidual)
          .left
          .map(LatentEncodingError.Guarantee.apply)
        bindings <- OptimizationIdentityBindings
          .from(
            MathematicalContractCatalog.generalizedLowRankModel.id,
            encoderIdentity,
            observations.valueIdentity,
            ObservationMaskIdentity.Observed(observations.valueIdentity),
            (Vector(decoder.valueIdentity, decoder.layout.valueIdentity) ++ rowPenalties.map(_.valueIdentity)).distinct,
            Vector(FittedLatentEncoder.latentCodeParameter),
            resultIdentity
          )
          .left
          .map(LatentEncodingError.Guarantee.apply)
        termination = solution.status match
          case FirstOrderStoppingStatus.Converged => NumericalTermination.Converged
          case FirstOrderStoppingStatus.IterationLimit => NumericalTermination.IterationLimit
        evidence <- SemanticOptimizationEvidence
          .from(bindings, termination, stationarity = Some(residual))
          .left
          .map(LatentEncodingError.Guarantee.apply)
        admittedClaim = solution.status match
          case FirstOrderStoppingStatus.Converged => OptimizationClaimClass.Stationary
          case FirstOrderStoppingStatus.IterationLimit => OptimizationClaimClass.Unresolved
        achievedGuarantee <- OptimizationGuaranteeAdmission
          .admit(
            MathematicalContractCatalog.generalizedLowRankModel,
            admittedClaim,
            OptimizationAssumptions.empty(bindings),
            Set.empty,
            evidence
          )
          .left
          .map(LatentEncodingError.Guarantee.apply)
        certificate = LatentEncodingCertificate(
          solution.certificate.primalResidual,
          solution.certificate.objectiveChange,
          solution.certificate.iterations,
          lipschitz,
          solution.certificate
        )
      yield
        FittedLatentEncoding(
          code,
          support,
          objective,
          decoded,
          certificate,
          uniqueness,
          achievedGuarantee,
          resultIdentity
        )

  private def encodingEntries[Rows <: SemanticSpace](
      observations: ObservationPattern[Rows, Feature]
  ): Either[LatentEncodingError, Vector[EncodingEntry]] =
    var feature = 0
    var result = Vector.empty[EncodingEntry]
    var failure = Option.empty[LatentEncodingError]
    while feature < observations.featureSpace.dimension && failure.isEmpty do
      observations.cellUnsafe(0, feature) match
        case ObservationCell.Observed(value, weight) =>
          val specification = decoder.layout.featureUnsafe(feature)
          specification.loss.curvatureUpperBound match
            case None =>
              failure = Some(
                LatentEncodingError.UnsupportedLoss(
                  specification.id,
                  specification.loss,
                  "the portable fixed-step solver requires a global gradient-Lipschitz bound"
                )
              )
            case Some(curvature) if specification.loss == EntryLoss.OrdinalCumulativeLogit =>
              failure = Some(
                LatentEncodingError.UnsupportedLoss(
                  specification.id,
                  specification.loss,
                  "ordered cumulative logits require a projected natural-parameter solver"
                )
              )
            case Some(curvature) =>
              FeatureEmbedding.from(specification.id, specification.domain, row = 0, value) match
                case Left(error) =>
                  specification.domain match
                    case FeatureDomain.Categorical(levels) =>
                      failure = Some(LatentEncodingError.UnseenCategoricalLevel(specification.id, value, levels))
                    case _ => failure = Some(LatentEncodingError.Observation(error))
                case Right(embedding) =>
                  result = result :+ EncodingEntry(feature, specification, embedding, weight.value, curvature)
        case ObservationCell.Missing(_) | ObservationCell.StructurallyInapplicable(_) => ()
        case ObservationCell.Censored(interval, _) =>
          failure = Some(LatentEncodingError.CensoredObservation(feature, interval))
      feature += 1
    failure.toLeft(result)

  private def initialMatrix(initial: Option[DVec]): Either[LatentEncodingError, DMat] =
    initial match
      case None => Right(DMat.zeros(decoder.latentSpace.dimension, 1))
      case Some(values) if values.length != decoder.latentSpace.dimension =>
        Left(
          LatentEncodingError.InvalidDefinition(
            s"initial latent code requires ${decoder.latentSpace.dimension} values, got ${values.length}"
          )
        )
      case Some(values) =>
        val data = new Array[Double](values.length)
        var index = 0
        var failure = Option.empty[(Int, Double)]
        while index < values.length && failure.isEmpty do
          data(index) = values(index)
          if !data(index).isFinite then failure = Some(index -> data(index))
          index += 1
        failure match
          case Some((position, value)) =>
            Left(LatentEncodingError.InvalidDefinition(s"initial latent code $position is not finite: $value"))
          case None => Right(GaleNumerics.matrixFromRowMajor(values.length, 1, data))

  private def penaltyWeight(functional: GlrmFactorPenalty): Double =
    rowPenalties.filter(_.functional == functional).map(_.weight.value).sum

  private def encodingLipschitz(entries: Vector[EncodingEntry], ridgeWeight: Double): Double =
    var result = ridgeWeight
    var index = 0
    while index < entries.length do
      val entry = entries(index)
      result += entry.weight * entry.curvature * decoderSliceSquaredNorm(entry.feature)
      index += 1
    result

  private def decoderSliceSquaredNorm(feature: Int): Double =
    val offset = decoder.layout.offset(feature)
    val width = decoder.layout.width(feature)
    var result = 0.0
    var latent = 0
    while latent < decoder.latentSpace.dimension do
      var output = 0
      while output < width do
        val value = decoder.values(latent, offset + output)
        result += value * value
        output += 1
      latent += 1
    result

  private def decodeAll[Rows <: SemanticSpace](
      rowSpace: SpaceEvidence[Rows],
      code: DMat,
      resultIdentity: ValueIdentity
  ): Either[LatentEncodingError, Vector[DecodedLatentFeature]] =
    val rowValues = new Array[Double](decoder.latentSpace.dimension)
    var latent = 0
    while latent < decoder.latentSpace.dimension do
      rowValues(latent) = code(latent, 0)
      latent += 1
    for
      rowCodes <- GlrmRowCodes
        .from(
          rowSpace,
          decoder.latentSpace,
          GaleNumerics.matrixFromRowMajor(1, decoder.latentSpace.dimension, rowValues),
          ValueIdentity.derived("encoded-row-codes", resultIdentity)
        )
        .left
        .map(LatentEncodingError.Observation.apply)
      factors <- GlrmFactors.from(rowCodes, decoder).left.map(LatentEncodingError.Observation.apply)
      decoded <- glrmEncodingTraverse(Vector.range(0, decoder.layout.features.length)): feature =>
        factors
          .decoded(0, feature)
          .left
          .map(LatentEncodingError.Observation.apply)
          .map(DecodedLatentFeature(decoder.layout.featureUnsafe(feature).id, _))
    yield decoded

  private def uniquenessStatus(
      entries: Vector[EncodingEntry],
      ridgeWeight: Double
  ): Either[LatentEncodingError, LatentCodeUniqueness] =
    if ridgeWeight > 0.0 then
      PositiveProofConstant
        .strongConvexity(ridgeWeight)
        .left
        .map(LatentEncodingError.Guarantee.apply)
        .map(LatentCodeUniqueness.UniqueByStrongConvexity.apply)
    else
      val gramValues = Array.fill(decoder.latentSpace.dimension * decoder.latentSpace.dimension)(0.0)
      var quadraticCount = 0
      var index = 0
      while index < entries.length do
        val entry = entries(index)
        if entry.specification.loss == EntryLoss.Quadratic then
          quadraticCount += 1
          val offset = decoder.layout.offset(entry.feature)
          var row = 0
          while row < decoder.latentSpace.dimension do
            var column = 0
            while column < decoder.latentSpace.dimension do
              gramValues(row * decoder.latentSpace.dimension + column) +=
                entry.weight * decoder.values(row, offset) * decoder.values(column, offset)
              column += 1
            row += 1
        index += 1
      if quadraticCount == 0 then Right(LatentCodeUniqueness.NotCertified("no globally strongly convex code term"))
      else
        val gram = GaleNumerics.matrixFromRowMajor(
          decoder.latentSpace.dimension,
          decoder.latentSpace.dimension,
          gramValues
        )
        DenseSolvers.symmetricEigen
          .decompose(gram)
          .left
          .map(error => LatentEncodingError.Multivar(LinalgErrorAdapter.toMultivarError(error)))
          .flatMap: spectrum =>
            val minimum = spectrum.values(spectrum.values.length - 1)
            val scale = Math.max(1.0, spectrum.values(0))
            if minimum > 1e-10 * scale then
              PositiveProofConstant
                .strongConvexity(minimum)
                .left
                .map(LatentEncodingError.Guarantee.apply)
                .map(LatentCodeUniqueness.UniqueByStrongConvexity.apply)
            else
              Right(
                LatentCodeUniqueness.NotCertified(
                  s"observed quadratic decoder rank is deficient; minimum Gram eigenvalue is $minimum"
                )
              )

object FittedLatentEncoder:
  private val latentCodeParameter: ParameterId = ParameterId.unsafe("latent-code")

  def from[Feature <: SemanticSpace, Latent <: SemanticSpace](
      decoder: FeatureDecoder[Feature, Latent],
      rowPenalties: Vector[GlrmFactorPenaltyTerm] = Vector.empty,
      config: FirstOrderConfig = FirstOrderConfig.portable
  ): Either[LatentEncodingError, FittedLatentEncoder[Feature, Latent]] =
    rowPenalties.find(_.target != GlrmFactorTarget.RowCodes) match
      case Some(term) =>
        Left(
          LatentEncodingError.InvalidDefinition(
            s"latent encoding accepts only row-code penalties, got ${term.target}"
          )
        )
      case None if rowPenalties.map(_.valueIdentity).distinct.length != rowPenalties.length =>
        Left(LatentEncodingError.InvalidDefinition("latent-encoding row penalty identities must be distinct"))
      case None =>
        val identity = ValueIdentity.derived(
          "fitted-latent-encoder",
          (Vector(decoder.valueIdentity, decoder.layout.valueIdentity) ++ rowPenalties.map(_.valueIdentity))*
        )
        Right(new FittedLatentEncoder(decoder, rowPenalties, config, identity))

private final case class EncodingEntry(
    feature: Int,
    specification: GlrmFeatureSpec,
    embedding: FeatureEmbedding,
    weight: Double,
    curvature: Double
)

private final class EncodingSmoothObjective[
    Feature <: SemanticSpace,
    Latent <: SemanticSpace
](
    decoder: FeatureDecoder[Feature, Latent],
    entries: Vector[EncodingEntry],
    ridgeWeight: Double,
    val lipschitz: Double
) extends SmoothObjective:
  val variableRows: Int = decoder.latentSpace.dimension

  def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
    val code = GaleMatrixBridge.toGale(at)
    entryValue(code)
      .left
      .map(error => FirstOrderError.OracleFailure("latent entry loss", error.message))
      .map(_ + 0.5 * ridgeWeight * glrmEncodingSquaredNorm(code))

  def gradient(at: DoubleMatrix): Either[FirstOrderError, DoubleMatrix] =
    val code = GaleMatrixBridge.toGale(at)
    val result = Array.fill(variableRows)(0.0)
    var entryIndex = 0
    var failure = Option.empty[FirstOrderError]
    while entryIndex < entries.length && failure.isEmpty do
      val entry = entries(entryIndex)
      val natural = decoder.naturalFromCode(code, entry.feature)
      entry.specification.loss.naturalGradient(entry.specification.id, entry.embedding, natural) match
        case Left(error) => failure = Some(FirstOrderError.OracleFailure("latent entry gradient", error.message))
        case Right(naturalGradient) =>
          val offset = decoder.layout.offset(entry.feature)
          var latent = 0
          while latent < variableRows do
            var output = 0
            while output < naturalGradient.length do
              result(latent) += entry.weight * decoder.values(latent, offset + output) * naturalGradient(output)
              output += 1
            latent += 1
      entryIndex += 1
    failure match
      case Some(error) => Left(error)
      case None =>
        var latent = 0
        while latent < variableRows do
          result(latent) += ridgeWeight * code(latent, 0)
          latent += 1
        Right(
          GaleMatrixBridge.fromGale(
            GaleNumerics.matrixFromRowMajor(variableRows, 1, result)
          )
        )

  def entryValue(code: DMat): Either[GeneralizedLowRankError, Double] =
    var result = 0.0
    var index = 0
    var failure = Option.empty[GeneralizedLowRankError]
    while index < entries.length && failure.isEmpty do
      val entry = entries(index)
      entry.specification.loss.value(
        entry.specification.id,
        entry.embedding,
        decoder.naturalFromCode(code, entry.feature)
      ) match
        case Left(error) => failure = Some(error)
        case Right(value) => result += entry.weight * value
      index += 1
    failure.toLeft(result)

private final class EncodingL1Term(
    val variableRows: Int,
    weight: Double
) extends ProximalTerm:
  def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
    val data = at.dataArray
    var result = 0.0
    var index = 0
    while index < data.length do
      result += Math.abs(data(index))
      index += 1
    Right(weight * result)

  def proximal(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix] =
    val input = at.dataArray
    val output = new Array[Double](input.length)
    val threshold = step * weight
    var index = 0
    while index < input.length do
      output(index) = Math.copySign(Math.max(0.0, Math.abs(input(index)) - threshold), input(index))
      index += 1
    DoubleMatrix
      .fromRowMajor(scalafim.linalg.MatrixShape.unsafe(at.rows, at.cols), output)
      .left
      .map(error => FirstOrderError.InvalidConfiguration(error.message))

private def glrmEncodingPenalty(code: DMat, ridgeWeight: Double, l1Weight: Double): Double =
  var l1 = 0.0
  var squared = 0.0
  var row = 0
  while row < code.rows do
    val value = code(row, 0)
    l1 += Math.abs(value)
    squared += value * value
    row += 1
  l1Weight * l1 + 0.5 * ridgeWeight * squared

private def glrmEncodingSquaredNorm(code: DMat): Double =
  var result = 0.0
  var row = 0
  while row < code.rows do
    result += code(row, 0) * code(row, 0)
    row += 1
  result

private def glrmEncodingColumn(code: DMat): DVec =
  val values = new Array[Double](code.rows)
  var row = 0
  while row < code.rows do
    values(row) = code(row, 0)
    row += 1
  GaleNumerics.vectorFromArray(values)

private def glrmEncodingTraverse[A, B](
    values: Vector[A]
)(
    function: A => Either[LatentEncodingError, B]
): Either[LatentEncodingError, Vector[B]] =
  values.foldLeft[Either[LatentEncodingError, Vector[B]]](Right(Vector.empty)): (result, value) =>
    for
      accumulated <- result
      current <- function(value)
    yield accumulated :+ current
