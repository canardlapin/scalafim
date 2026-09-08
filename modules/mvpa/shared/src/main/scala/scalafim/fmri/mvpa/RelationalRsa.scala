package scalafim.fmri.mvpa

import gale.linalg.CholeskyOptions
import gale.linalg.DMat
import gale.linalg.Matrix
import multivar.core.SemanticSpace

opaque type SecondOrderModelName = String

object SecondOrderModelName:
  def apply(value: String): Either[RelationalRsaError, SecondOrderModelName] =
    ScientificIdentityText
      .lowerIdentifier("second-order model name", value)
      .left
      .map(RelationalRsaError.Identity.apply)

  private[mvpa] def unsafe(value: String): SecondOrderModelName =
    value

  extension (name: SecondOrderModelName) inline def value: String = name

sealed trait SignalModelRole
sealed trait NuisanceModelRole

enum RelationalRsaError:
  case Identity(error: ScientificIdentityError)
  case Query(error: RelationalQueryError)
  case Fit(error: RelationalFitError)
  case ModelLengthMismatch(expected: Int, actual: Int)
  case NonFiniteModelValue(position: Int, value: Double)
  case ModelDomainMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case ModelWitnessMismatch
  case EmptySignalModels
  case EmptyNuisanceModels
  case DuplicateModelName(name: SecondOrderModelName)
  case ConstantVector(label: String)
  case RankDeficientComparison(detail: String)

  def message: String =
    this match
      case Identity(error)                       => error.message
      case Query(error)                          => error.message
      case Fit(error)                            => error.message
      case ModelLengthMismatch(expected, actual) =>
        s"second-order model contains $actual pair values, expected $expected"
      case NonFiniteModelValue(position, value) =>
        s"second-order model contains non-finite value $value at pair position $position"
      case ModelDomainMismatch(expected, actual) =>
        s"second-order model pair domain ${actual.value} does not match fit domain ${expected.value}"
      case ModelWitnessMismatch =>
        "second-order model and fit use different nominal pair witnesses"
      case EmptySignalModels =>
        "second-order regression requires at least one signal model"
      case EmptyNuisanceModels =>
        "partial second-order comparison requires at least one nuisance model"
      case DuplicateModelName(name) =>
        s"second-order model name '${name.value}' is duplicated"
      case ConstantVector(label) =>
        s"$label is constant, so its correlation is undefined"
      case RankDeficientComparison(detail) =>
        s"second-order comparison design is rank deficient: $detail"

/** Pair-axis-bound model with a statically visible scientific role. */
final class SecondOrderModel[
    Role,
    E <: SemanticSpace,
    EK
] private (
    val name: SecondOrderModelName,
    val domain: WithinPairDomain[E, EK],
    val values: Column[domain.pairAxis.Id, Double],
    val roleLabel: String,
    val identity: ScientificComponentFingerprint
)

object SecondOrderModel:
  private val Kind = EstimandKind.unsafe("second-order-model")

  def signal[E <: SemanticSpace, EK](
      domain: WithinPairDomain[E, EK],
      name: SecondOrderModelName,
      values: Seq[Double]
  ): Either[RelationalRsaError, SecondOrderModel[SignalModelRole, E, EK]] =
    build[SignalModelRole, E, EK](domain, name, values, "signal")

  def nuisance[E <: SemanticSpace, EK](
      domain: WithinPairDomain[E, EK],
      name: SecondOrderModelName,
      values: Seq[Double]
  ): Either[RelationalRsaError, SecondOrderModel[NuisanceModelRole, E, EK]] =
    build[NuisanceModelRole, E, EK](domain, name, values, "nuisance")

  private def build[Role, E <: SemanticSpace, EK](
      domain: WithinPairDomain[E, EK],
      name: SecondOrderModelName,
      input: Seq[Double],
      role: String
  ): Either[RelationalRsaError, SecondOrderModel[Role, E, EK]] =
    val values = input.toVector
    if values.length != domain.size then Left(RelationalRsaError.ModelLengthMismatch(domain.size, values.length))
    else
      var position = 0
      while position < values.length do
        if !values(position).isFinite then
          return Left(RelationalRsaError.NonFiniteModelValue(position, values(position)))
        position += 1
      val writer = CanonicalWriter()
      writer.string("scalafim-second-order-model/v1")
      writer.string(domain.identity.value)
      writer.string(name.value)
      writer.string(role)
      writer.int(values.length)
      values.foreach(writer.double)
      for
        column <- Column(domain.pairAxis, values).left
          .map(error => RelationalRsaError.Query(RelationalQueryError.Column(error)))
        component <- EstimandIdentity(
          Kind,
          Vector(
            "domain" -> domain.identity.value,
            "name" -> name.value,
            "role" -> role,
            "values" -> AxisDigest.sha256Hex(writer.result())
          )
        ).left.map(RelationalRsaError.Identity.apply)
      yield new SecondOrderModel(
        name,
        domain,
        column,
        role,
        component.fingerprint
      )

final class RankRsaEstimate private[mvpa] (
    val fit: ScientificComponentFingerprint,
    val model: ScientificComponentFingerprint,
    val estimand: EstimandIdentity,
    val correlation: Double
)

final class PearsonRsaEstimate private[mvpa] (
    val fit: ScientificComponentFingerprint,
    val model: ScientificComponentFingerprint,
    val estimand: EstimandIdentity,
    val correlation: Double
)

final class PartialRsaEstimate private[mvpa] (
    val fit: ScientificComponentFingerprint,
    val signal: ScientificComponentFingerprint,
    val nuisance: Vector[ScientificComponentFingerprint],
    val estimand: EstimandIdentity,
    val partialCorrelation: Double
)

enum RsaCoefficientRole:
  case Intercept
  case Signal
  case Nuisance

final case class RsaCoefficient(
    name: String,
    role: RsaCoefficientRole,
    value: Double
)

final class RegressionRsaEstimate private[mvpa] (
    val fit: ScientificComponentFingerprint,
    val signals: Vector[ScientificComponentFingerprint],
    val nuisance: Vector[ScientificComponentFingerprint],
    val estimand: EstimandIdentity,
    val coefficients: Vector[RsaCoefficient],
    val residualSumSquares: Double
)

object RelationalRsa:
  private val PearsonKind = EstimandKind.unsafe("pearson-rsa")
  private val RankKind = EstimandKind.unsafe("rank-rsa")
  private val PartialKind = EstimandKind.unsafe("partial-rsa")
  private val RegressionKind = EstimandKind.unsafe("regression-rsa")

  def pearson[
      P <: SemanticSpace,
      E <: SemanticSpace,
      L <: SemanticSpace,
      EK,
      LK
  ](
      fit: CompatibleSecondOrderFit[P, E, L, EK, LK],
      signal: SecondOrderModel[SignalModelRole, E, EK]
  ): Either[RelationalRsaError, PearsonRsaEstimate] =
    for
      _ <- validateModel(fit, signal)
      observed <- fit.rdm.left.map(RelationalRsaError.Fit.apply)
      correlation <- pearson(
        observed.distances.toVector,
        signal.values.toVector,
        "observed distances",
        "signal distances"
      )
      identity <- EstimandIdentity(
        PearsonKind,
        Vector(
          "fit" -> fit.identity.value,
          "model" -> signal.identity.value,
          "second-order-estimand" ->
            "pearson-correlation-of-signed-canonical-pair-distances"
        )
      ).left.map(RelationalRsaError.Identity.apply)
    yield new PearsonRsaEstimate(
      fit.identity,
      signal.identity,
      identity,
      correlation
    )

  def rank[
      P <: SemanticSpace,
      E <: SemanticSpace,
      L <: SemanticSpace,
      EK,
      LK
  ](
      fit: CompatibleSecondOrderFit[P, E, L, EK, LK],
      signal: SecondOrderModel[SignalModelRole, E, EK]
  ): Either[RelationalRsaError, RankRsaEstimate] =
    for
      _ <- validateModel(fit, signal)
      observed <- fit.rdm.left.map(RelationalRsaError.Fit.apply)
      observedRanks = averageRanks(observed.distances.toVector)
      modelRanks = averageRanks(signal.values.toVector)
      correlation <- pearson(observedRanks, modelRanks, "observed ranks", "signal ranks")
      identity <- EstimandIdentity(
        RankKind,
        Vector(
          "fit" -> fit.identity.value,
          "model" -> signal.identity.value,
          "second-order-estimand" -> "spearman-correlation-of-canonical-pair-distances"
        )
      ).left.map(RelationalRsaError.Identity.apply)
    yield new RankRsaEstimate(
      fit.identity,
      signal.identity,
      identity,
      correlation
    )

  def partial[
      P <: SemanticSpace,
      E <: SemanticSpace,
      L <: SemanticSpace,
      EK,
      LK
  ](
      fit: CompatibleSecondOrderFit[P, E, L, EK, LK],
      signal: SecondOrderModel[SignalModelRole, E, EK],
      nuisance: Vector[SecondOrderModel[NuisanceModelRole, E, EK]]
  ): Either[RelationalRsaError, PartialRsaEstimate] =
    if nuisance.isEmpty then Left(RelationalRsaError.EmptyNuisanceModels)
    else
      for
        _ <- validateModels(fit, signal +: nuisance)
        _ <- validateNames(signal +: nuisance)
        observed <- fit.rdm.left.map(RelationalRsaError.Fit.apply)
        nuisanceDesign = designMatrix(nuisance.map(_.values.toVector), intercept = true)
        observedResidual <- residualize(observed.distances.toVector, nuisanceDesign)
        signalResidual <- residualize(signal.values.toVector, nuisanceDesign)
        correlation <- pearson(
          observedResidual,
          signalResidual,
          "nuisance-residualized observed distances",
          "nuisance-residualized signal distances"
        )
        identity <- EstimandIdentity(
          PartialKind,
          Vector(
            "fit" -> fit.identity.value,
            "nuisance" -> modelDigest(nuisance),
            "second-order-estimand" -> "pearson-correlation-after-intercept-and-nuisance-residualization",
            "signal" -> signal.identity.value
          )
        ).left.map(RelationalRsaError.Identity.apply)
      yield new PartialRsaEstimate(
        fit.identity,
        signal.identity,
        nuisance.map(_.identity),
        identity,
        correlation
      )

  def regression[
      P <: SemanticSpace,
      E <: SemanticSpace,
      L <: SemanticSpace,
      EK,
      LK
  ](
      fit: CompatibleSecondOrderFit[P, E, L, EK, LK],
      signals: Vector[SecondOrderModel[SignalModelRole, E, EK]],
      nuisance: Vector[SecondOrderModel[NuisanceModelRole, E, EK]],
      intercept: Boolean
  ): Either[RelationalRsaError, RegressionRsaEstimate] =
    if signals.isEmpty then Left(RelationalRsaError.EmptySignalModels)
    else
      val all: Vector[SecondOrderModel[?, E, EK]] = signals ++ nuisance
      for
        _ <- validateModels(fit, all)
        _ <- validateNames(all)
        observed <- fit.rdm.left.map(RelationalRsaError.Fit.apply)
        design = designMatrix((signals ++ nuisance).map(_.values.toVector), intercept)
        coefficients <- ordinaryLeastSquares(observed.distances.toVector, design)
        fitted = multiply(design, coefficients)
        residualSumSquares = observed.distances.toVector
          .zip(fitted)
          .map: (actual, predicted) =>
            val residual = actual - predicted
            residual * residual
          .sum
        identity <- EstimandIdentity(
          RegressionKind,
          Vector(
            "fit" -> fit.identity.value,
            "intercept" -> intercept.toString,
            "nuisance" -> modelDigest(nuisance),
            "second-order-estimand" -> "ordinary-least-squares-on-signed-canonical-pair-distances",
            "signals" -> modelDigest(signals)
          )
        ).left.map(RelationalRsaError.Identity.apply)
      yield
        val output = Vector.newBuilder[RsaCoefficient]
        var offset = 0
        if intercept then
          output += RsaCoefficient("intercept", RsaCoefficientRole.Intercept, coefficients(0))
          offset = 1
        signals.zipWithIndex.foreach: (model, position) =>
          output += RsaCoefficient(
            model.name.value,
            RsaCoefficientRole.Signal,
            coefficients(offset + position)
          )
        offset += signals.length
        nuisance.zipWithIndex.foreach: (model, position) =>
          output += RsaCoefficient(
            model.name.value,
            RsaCoefficientRole.Nuisance,
            coefficients(offset + position)
          )
        new RegressionRsaEstimate(
          fit.identity,
          signals.map(_.identity),
          nuisance.map(_.identity),
          identity,
          output.result(),
          residualSumSquares
        )

  private def validateModel[
      P <: SemanticSpace,
      E <: SemanticSpace,
      L <: SemanticSpace,
      EK,
      LK,
      R
  ](
      fit: CompatibleSecondOrderFit[P, E, L, EK, LK],
      model: SecondOrderModel[R, E, EK]
  ): Either[RelationalRsaError, Unit] =
    if model.domain.pairAxis.identity != fit.definition.domain.pairAxis.identity then
      Left(
        RelationalRsaError.ModelDomainMismatch(
          fit.definition.domain.pairAxis.identity.fingerprint,
          model.domain.pairAxis.identity.fingerprint
        )
      )
    else if !(model.domain.pairAxis.evidence eq fit.definition.domain.pairAxis.evidence) then
      Left(RelationalRsaError.ModelWitnessMismatch)
    else Right(())

  private def validateModels[
      P <: SemanticSpace,
      E <: SemanticSpace,
      L <: SemanticSpace,
      EK,
      LK
  ](
      fit: CompatibleSecondOrderFit[P, E, L, EK, LK],
      models: Vector[SecondOrderModel[?, E, EK]]
  ): Either[RelationalRsaError, Unit] =
    var position = 0
    while position < models.length do
      validateModel(fit, models(position)) match
        case Left(error) => return Left(error)
        case Right(_)    => ()
      position += 1
    Right(())

  private def validateNames[E <: SemanticSpace, EK](
      models: Vector[SecondOrderModel[?, E, EK]]
  ): Either[RelationalRsaError, Unit] =
    val seen = scala.collection.mutable.HashSet.empty[SecondOrderModelName]
    var position = 0
    while position < models.length do
      val name = models(position).name
      if seen.contains(name) then return Left(RelationalRsaError.DuplicateModelName(name))
      seen += name
      position += 1
    Right(())

  private def averageRanks(values: Vector[Double]): Vector[Double] =
    val sorted = values.zipWithIndex.sortBy(_._1)
    val output = new Array[Double](values.length)
    var start = 0
    while start < sorted.length do
      var end = start + 1
      while end < sorted.length && sorted(end)._1 == sorted(start)._1 do end += 1
      val rank = (start + 1 + end).toDouble / 2.0
      var position = start
      while position < end do
        output(sorted(position)._2) = rank
        position += 1
      start = end
    output.toVector

  private def pearson(
      left: Vector[Double],
      right: Vector[Double],
      leftLabel: String,
      rightLabel: String
  ): Either[RelationalRsaError, Double] =
    val leftMean = left.sum / left.length.toDouble
    val rightMean = right.sum / right.length.toDouble
    var numerator = 0.0
    var leftSumSquares = 0.0
    var rightSumSquares = 0.0
    var position = 0
    while position < left.length do
      val centeredLeft = left(position) - leftMean
      val centeredRight = right(position) - rightMean
      numerator += centeredLeft * centeredRight
      leftSumSquares += centeredLeft * centeredLeft
      rightSumSquares += centeredRight * centeredRight
      position += 1
    if leftSumSquares == 0.0 then Left(RelationalRsaError.ConstantVector(leftLabel))
    else if rightSumSquares == 0.0 then Left(RelationalRsaError.ConstantVector(rightLabel))
    else Right(numerator / math.sqrt(leftSumSquares * rightSumSquares))

  private def designMatrix(
      columns: Vector[Vector[Double]],
      intercept: Boolean
  ): DMat =
    val rows = columns.head.length
    val output = Matrix.newBuilder(rows, columns.length + (if intercept then 1 else 0))
    var row = 0
    while row < rows do
      var offset = 0
      if intercept then
        output(row, 0) = 1.0
        offset = 1
      var column = 0
      while column < columns.length do
        output(row, offset + column) = columns(column)(row)
        column += 1
      row += 1
    output.result()

  private def residualize(
      values: Vector[Double],
      design: DMat
  ): Either[RelationalRsaError, Vector[Double]] =
    ordinaryLeastSquares(values, design).map: coefficients =>
      values.zip(multiply(design, coefficients)).map(_ - _)

  private def ordinaryLeastSquares(
      values: Vector[Double],
      design: DMat
  ): Either[RelationalRsaError, Vector[Double]] =
    val normal = Matrix.newBuilder(design.cols, design.cols)
    val rhs = Matrix.newBuilder(design.cols, 1)
    var left = 0
    while left < design.cols do
      var right = 0
      while right < design.cols do
        var sum = 0.0
        var row = 0
        while row < design.rows do
          sum += design(row, left) * design(row, right)
          row += 1
        normal(left, right) = sum
        right += 1
      var response = 0.0
      var row = 0
      while row < design.rows do
        response += design(row, left) * values(row)
        row += 1
      rhs(left, 0) = response
      left += 1
    normal
      .result()
      .cholesky(CholeskyOptions(1e-12))
      .left
      .map(error => RelationalRsaError.RankDeficientComparison(error.getMessage))
      .flatMap(_.solve(rhs.result()).left.map(error => RelationalRsaError.RankDeficientComparison(error.getMessage)))
      .map(solution => Vector.tabulate(solution.rows)(row => solution(row, 0)))

  private def multiply(design: DMat, coefficients: Vector[Double]): Vector[Double] =
    Vector.tabulate(design.rows): row =>
      var sum = 0.0
      var column = 0
      while column < design.cols do
        sum += design(row, column) * coefficients(column)
        column += 1
      sum

  private def modelDigest[E <: SemanticSpace, EK](
      models: Vector[SecondOrderModel[?, E, EK]]
  ): String =
    val writer = CanonicalWriter()
    writer.string("scalafim-rsa-model-bank/v1")
    writer.int(models.length)
    models.foreach(model => writer.string(model.identity.value))
    AxisDigest.sha256Hex(writer.result())
