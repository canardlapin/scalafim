package scalafim.multivar
package optimization

import scalafim.multivar.core.*

import gale.linalg.DMat

sealed trait ChartOperatorRole extends OperatorRoleTag

opaque type TightFrameBound = Double

object TightFrameBound:
  def apply(value: Double): Either[ChartError, TightFrameBound] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(ChartError.InvalidDefinition(s"tight-frame bound must be finite and positive, got $value"))

  extension (value: TightFrameBound)
    inline def doubleValue: Double = value

enum ChartKind:
  case Identity
  case Orthogonal(proof: ValueIdentity)
  case TightFrame(bound: TightFrameBound, proof: ValueIdentity)
  case Selection(indices: IndexSet)
  case General

enum ChartError:
  case InvalidDefinition(reason: String)
  case FeatureIdentityMismatch(expected: ValueIdentity, actual: ValueIdentity)
  case GroupOverlapUnsupported(groups: ValueIdentity)
  case UnsupportedDirectLowering(kind: ChartKind)
  case FunctionalMismatch(expected: String, actual: FunctionalKind)
  case SetMismatch(actual: FeasibleSetKind)
  case Semantic(error: SemanticError)

  def message: String =
    this match
      case InvalidDefinition(reason) => reason
      case FeatureIdentityMismatch(expected, actual) =>
        s"feature chart identity ${expected.stableKey} does not match group identity ${actual.stableKey}"
      case GroupOverlapUnsupported(groups) =>
        s"overlapping group structure ${groups.stableKey} requires split lowering"
      case UnsupportedDirectLowering(kind) => s"chart kind $kind has no valid direct separable lowering"
      case FunctionalMismatch(expected, actual) => s"expected $expected functional, got $actual"
      case SetMismatch(actual) => s"feasible set $actual has no direct chart projection lowering"
      case Semantic(error) => error.message

final case class ChartLawCertificate private (
    kind: ChartKind,
    forwardIdentity: ValueIdentity,
    synthesisIdentity: ValueIdentity,
    residual: Double,
    scale: Double,
    context: CertificateContext
)

object ChartLawCertificate:
  private[multivar] def certified(
      kind: ChartKind,
      forwardIdentity: ValueIdentity,
      synthesisIdentity: ValueIdentity,
      residual: Double,
      scale: Double,
      context: CertificateContext
  ): ChartLawCertificate =
    new ChartLawCertificate(kind, forwardIdentity, synthesisIdentity, residual, scale, context)

/** Stable coordinates for chart-dependent claims. `synthesis` is explicit: an
  * algebraic dual alone is not a coordinate inverse.
  */
final class FeatureChart[Feature <: SemanticSpace, Coordinates <: SemanticSpace] private (
    val featureSpace: SpaceEvidence[Feature],
    val coordinateSpace: SpaceEvidence[Coordinates],
    val featureIds: Vector[String],
    val valueIdentity: ValueIdentity,
    val kind: ChartKind,
    val forward: Op[Dual[Feature], Primal[Coordinates], ChartOperatorRole, UncheckedEvidence],
    val synthesis: Op[Primal[Coordinates], Dual[Feature], ChartOperatorRole, UncheckedEvidence],
    val lawCertificate: Option[ChartLawCertificate]
):
  def target(parameter: ParameterId): TargetExpression =
    TargetExpression
      .linear(parameter, s"feature-chart:${valueIdentity.stableKey}", forward)
      .fold(error => throw new IllegalStateException(error.message), identity)

object FeatureChart:
  def identity[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      featureSpace: SpaceEvidence[Feature],
      coordinateSpace: SpaceEvidence[Coordinates],
      featureIds: Vector[String],
      valueIdentity: ValueIdentity
  ): Either[ChartError, FeatureChart[Feature, Coordinates]] =
    if featureSpace.dimension != coordinateSpace.dimension then
      Left(ChartError.InvalidDefinition("identity chart requires equal feature and coordinate dimensions"))
    else build(featureSpace, coordinateSpace, featureIds, valueIdentity, ChartKind.Identity, DMat.eye(featureSpace.dimension), DMat.eye(featureSpace.dimension))

  def selection[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      featureSpace: SpaceEvidence[Feature],
      coordinateSpace: SpaceEvidence[Coordinates],
      allFeatureIds: Vector[String],
      indices: IndexSet,
      valueIdentity: ValueIdentity
  ): Either[ChartError, FeatureChart[Feature, Coordinates]] =
    if allFeatureIds.length != featureSpace.dimension then
      Left(ChartError.InvalidDefinition("source feature ids must match the feature space"))
    else if indices.indices.exists(index => index < 0 || index >= featureSpace.dimension) then
      Left(ChartError.InvalidDefinition("selection chart index lies outside the feature space"))
    else if coordinateSpace.dimension != indices.length then
      Left(ChartError.InvalidDefinition("selection chart coordinate dimension must equal selected feature count"))
    else
      val forward = new Array[Double](coordinateSpace.dimension * featureSpace.dimension)
      val synthesis = new Array[Double](featureSpace.dimension * coordinateSpace.dimension)
      var coordinate = 0
      while coordinate < indices.length do
        val feature = indices.indices(coordinate)
        forward(coordinate * featureSpace.dimension + feature) = 1.0
        synthesis(feature * coordinateSpace.dimension + coordinate) = 1.0
        coordinate += 1
      build(
        featureSpace,
        coordinateSpace,
        indices.indices.map(allFeatureIds),
        valueIdentity,
        ChartKind.Selection(indices),
        GaleNumerics.matrixFromRowMajor(coordinateSpace.dimension, featureSpace.dimension, forward),
        GaleNumerics.matrixFromRowMajor(featureSpace.dimension, coordinateSpace.dimension, synthesis)
      )

  def certified[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      featureSpace: SpaceEvidence[Feature],
      coordinateSpace: SpaceEvidence[Coordinates],
      featureIds: Vector[String],
      valueIdentity: ValueIdentity,
      kind: ChartKind,
      forward: Op[Dual[Feature], Primal[Coordinates], ? <: OperatorRoleTag, ? <: OperatorEvidence],
      synthesis: Op[Primal[Coordinates], Dual[Feature], ? <: OperatorRoleTag, ? <: OperatorEvidence],
      context: CertificateContext = CertificateContext.portableFloat64
  ): Either[ChartError, FeatureChart[Feature, Coordinates]] =
    kind match
      case ChartKind.Identity | ChartKind.Selection(_) =>
        Left(ChartError.InvalidDefinition("identity and selection charts must use their proof-by-construction factories"))
      case ChartKind.Orthogonal(proof) if proof != forward.valueIdentity =>
        Left(ChartError.InvalidDefinition("orthogonal chart proof must bind the forward map identity"))
      case ChartKind.TightFrame(_, proof) if proof != forward.valueIdentity =>
        Left(ChartError.InvalidDefinition("tight-frame proof must bind the forward map identity"))
      case _ =>
        for
          _ <- validateMetadata(featureIds, coordinateSpace.dimension)
          law <- certifyLaw(kind, forward, synthesis, context)
        yield
          new FeatureChart(
            featureSpace,
            coordinateSpace,
            featureIds,
            valueIdentity,
            kind,
            eraseForward(forward),
            eraseSynthesis(synthesis),
            law
          )

  private def build[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      featureSpace: SpaceEvidence[Feature],
      coordinateSpace: SpaceEvidence[Coordinates],
      featureIds: Vector[String],
      valueIdentity: ValueIdentity,
      kind: ChartKind,
      forwardMatrix: DMat,
      synthesisMatrix: DMat
  ): Either[ChartError, FeatureChart[Feature, Coordinates]] =
    for
      _ <- validateMetadata(featureIds, coordinateSpace.dimension)
      forward <- Op
        .fromDense(
          forwardMatrix,
          CoordinateEvidence.dual(featureSpace),
          CoordinateEvidence.primal(coordinateSpace),
          OperatorRoleWitness.derived[ChartOperatorRole](OperatorRole.ConstraintMap),
          ValueIdentity.derived("feature-chart-forward", valueIdentity)
        )
        .left
        .map(ChartError.Semantic.apply)
      synthesis <- Op
        .fromDense(
          synthesisMatrix,
          CoordinateEvidence.primal(coordinateSpace),
          CoordinateEvidence.dual(featureSpace),
          OperatorRoleWitness.derived[ChartOperatorRole](OperatorRole.ConstraintMap),
          ValueIdentity.derived("feature-chart-synthesis", valueIdentity)
        )
        .left
        .map(ChartError.Semantic.apply)
    yield new FeatureChart(featureSpace, coordinateSpace, featureIds, valueIdentity, kind, forward, synthesis, None)

  private def certifyLaw[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      kind: ChartKind,
      forward: Op[Dual[Feature], Primal[Coordinates], ? <: OperatorRoleTag, ? <: OperatorEvidence],
      synthesis: Op[Primal[Coordinates], Dual[Feature], ? <: OperatorRoleTag, ? <: OperatorEvidence],
      context: CertificateContext
  ): Either[ChartError, Option[ChartLawCertificate]] =
    kind match
      case ChartKind.General => Right(None)
      case ChartKind.Orthogonal(_) | ChartKind.TightFrame(_, _) =>
        for
          forwardDense <- forward.toDense.left.map(ChartError.Semantic.apply)
          synthesisDense <- synthesis.toDense.left.map(ChartError.Semantic.apply)
          bound = kind match
            case ChartKind.Orthogonal(_) => 1.0
            case ChartKind.TightFrame(value, _) => value.doubleValue
            case _ => 1.0
          requiresSquare = kind match
            case ChartKind.Orthogonal(_) => true
            case _ => false
          _ <-
            if requiresSquare && forwardDense.rows != forwardDense.cols then
              Left(ChartError.InvalidDefinition("orthogonal chart must be square"))
            else Right(())
          adjointResidual = frobeniusDifference(synthesisDense, forwardDense.t)
          coisometry = GaleNumerics.multiply(forwardDense, synthesisDense)
          expected = MatrixOps.scale(DMat.eye(forwardDense.rows), bound)
          lawResidual = frobeniusDifference(coisometry, expected)
          residual = Math.max(adjointResidual, lawResidual)
          scale = Math.max(1.0, Math.max(frobenius(synthesisDense), Math.max(frobenius(forwardDense), frobenius(expected))))
          _ <-
            if residual <= context.tolerance.threshold(scale) then Right(())
            else Left(
              ChartError.InvalidDefinition(
                s"$kind chart law residual $residual exceeds threshold ${context.tolerance.threshold(scale)}"
              )
            )
        yield Some(
          ChartLawCertificate.certified(
            kind,
            forward.valueIdentity,
            synthesis.valueIdentity,
            residual,
            scale,
            context
          )
        )
      case ChartKind.Identity | ChartKind.Selection(_) =>
        Left(ChartError.InvalidDefinition("identity and selection charts must use their proof-by-construction factories"))

  private def frobenius(value: DMat): Double =
    var total = 0.0
    var row = 0
    while row < value.rows do
      var column = 0
      while column < value.cols do
        val entry = value(row, column)
        total += entry * entry
        column += 1
      row += 1
    Math.sqrt(total)

  private def frobeniusDifference(left: DMat, right: DMat): Double =
    var total = 0.0
    var row = 0
    while row < left.rows do
      var column = 0
      while column < left.cols do
        val delta = left(row, column) - right(row, column)
        total += delta * delta
        column += 1
      row += 1
    Math.sqrt(total)

  private def validateMetadata(featureIds: Vector[String], expected: Int): Either[ChartError, Unit] =
    val clean = featureIds.map(_.trim)
    if clean.length != expected then Left(ChartError.InvalidDefinition("feature ids must match chart coordinates"))
    else if clean.exists(_.isEmpty) || clean.distinct.length != clean.length then
      Left(ChartError.InvalidDefinition("feature ids must be non-empty and unique"))
    else Right(())

  private def eraseForward[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      value: Op[Dual[Feature], Primal[Coordinates], ? <: OperatorRoleTag, ? <: OperatorEvidence]
  ): Op[Dual[Feature], Primal[Coordinates], ChartOperatorRole, UncheckedEvidence] =
    erase(value)

  private def eraseSynthesis[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      value: Op[Primal[Coordinates], Dual[Feature], ? <: OperatorRoleTag, ? <: OperatorEvidence]
  ): Op[Primal[Coordinates], Dual[Feature], ChartOperatorRole, UncheckedEvidence] =
    erase(value)

  private def erase[From <: Coordinate, To <: Coordinate](
      value: Op[From, To, ? <: OperatorRoleTag, ? <: OperatorEvidence]
  ): Op[From, To, ChartOperatorRole, UncheckedEvidence] =
    val identity = ValueIdentity.derived("feature-chart-map", value.valueIdentity)
    new Op(
      value.kernel,
      value.domain,
      value.codomain,
      OperatorRoleWitness.derived[ChartOperatorRole](OperatorRole.ConstraintMap),
      OperatorCertificate.unchecked(identity),
      identity,
      value.provenance.append(SemanticProvenanceEvent.Derived("feature-chart-map", Vector(value.valueIdentity)))
    )

final case class CoordinateGroup(id: String, indices: IndexSet):
  require(id.trim.nonEmpty, "coordinate group id must be non-empty")

enum GroupOverlap:
  case Disjoint
  case Overlapping

final class GroupStructure private (
    val chartIdentity: ValueIdentity,
    val featureSpaceId: SpaceId,
    val coordinateDimension: Int,
    val groups: Vector[CoordinateGroup],
    val overlap: GroupOverlap,
    val valueIdentity: ValueIdentity
)

object GroupStructure:
  def from[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      chart: FeatureChart[Feature, Coordinates],
      groups: Vector[CoordinateGroup],
      valueIdentity: ValueIdentity
  ): Either[ChartError, GroupStructure] =
    val coordinateDimension = chart.coordinateSpace.dimension
    if coordinateDimension <= 0 || groups.isEmpty then
      Left(ChartError.InvalidDefinition("group structure requires positive coordinates and non-empty groups"))
    else if groups.map(_.id.trim).distinct.length != groups.length then
      Left(ChartError.InvalidDefinition("group ids must be unique"))
    else if groups.exists(_.indices.requireWithin(coordinateDimension).isLeft) then
      Left(ChartError.InvalidDefinition("group index lies outside chart coordinates"))
    else
      val flattened = groups.flatMap(_.indices.indices)
      val overlap = if flattened.distinct.length == flattened.length then GroupOverlap.Disjoint else GroupOverlap.Overlapping
      Right(
        new GroupStructure(
          chart.valueIdentity,
          chart.featureSpace.descriptor.id,
          coordinateDimension,
          groups,
          overlap,
          valueIdentity
        )
      )

enum DirectProximalKind:
  case ElementwiseL1
  case FeatureRowsL21
  case DisjointGroups(groups: GroupStructure)
  case SparseGroup(l1Fraction: UnitFraction, groups: GroupStructure)
  case ElasticNet(l1Fraction: UnitFraction)

final class DirectProximalPlan[Feature <: SemanticSpace, Coordinates <: SemanticSpace] private (
    val original: PenaltyTerm,
    val chart: FeatureChart[Feature, Coordinates],
    val kind: DirectProximalKind
):
  def apply(parameter: DMat, step: PenaltyWeight): Either[ChartError, DMat] =
    chart.forward(parameter).left.map(ChartError.Semantic.apply).flatMap: coordinates =>
      val threshold = original.weight.value * step.value
      chart.kind match
        case ChartKind.General => Left(ChartError.UnsupportedDirectLowering(chart.kind))
        case ChartKind.Identity | ChartKind.Orthogonal(_) =>
          chart.synthesis(prox(coordinates, threshold)).left.map(ChartError.Semantic.apply)
        case ChartKind.Selection(_) =>
          for
            update <- chart.synthesis(MatrixOps.subtract(prox(coordinates, threshold), coordinates)).left.map(ChartError.Semantic.apply)
          yield MatrixOps.subtract(parameter, MatrixOps.scale(update, -1.0))
        case ChartKind.TightFrame(bound, _) =>
          for
            update <- chart
              .synthesis(MatrixOps.subtract(prox(coordinates, threshold * bound.doubleValue), coordinates))
              .left
              .map(ChartError.Semantic.apply)
          yield MatrixOps.subtract(parameter, MatrixOps.scale(update, -1.0 / bound.doubleValue))

  private def prox(value: DMat, threshold: Double): DMat =
    kind match
      case DirectProximalKind.ElementwiseL1 => ProximalMath.softThreshold(value, threshold)
      case DirectProximalKind.FeatureRowsL21 => ProximalMath.groupRows(value, threshold)
      case DirectProximalKind.DisjointGroups(groups) => ProximalMath.groups(value, groups, threshold)
      case DirectProximalKind.SparseGroup(fraction, groups) =>
        val sparse = ProximalMath.softThreshold(value, threshold * fraction.value)
        ProximalMath.groups(sparse, groups, threshold * (1.0 - fraction.value))
      case DirectProximalKind.ElasticNet(fraction) =>
        val sparse = ProximalMath.softThreshold(value, threshold * fraction.value)
        MatrixOps.scale(sparse, 1.0 / (1.0 + threshold * (1.0 - fraction.value)))

object DirectProximalPlan:
  def from[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      original: PenaltyTerm,
      chart: FeatureChart[Feature, Coordinates],
      kind: DirectProximalKind
  ): Either[ChartError, DirectProximalPlan[Feature, Coordinates]] =
    for
      _ <- validateChart(chart)
      _ <- validateFunctional(original.functional, kind)
      _ <- validateGroups(chart, kind)
      _ <- validateTarget(original, chart)
    yield new DirectProximalPlan(original, chart, kind)

  private def validateChart[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      chart: FeatureChart[Feature, Coordinates]
  ): Either[ChartError, Unit] =
    chart.kind match
      case ChartKind.General => Left(ChartError.UnsupportedDirectLowering(chart.kind))
      case ChartKind.Orthogonal(_) | ChartKind.TightFrame(_, _) if chart.lawCertificate.isEmpty =>
        Left(ChartError.InvalidDefinition(s"${chart.kind} chart requires a numerical law certificate"))
      case _ => Right(())

  private def validateFunctional(actual: FunctionalKind, expected: DirectProximalKind): Either[ChartError, Unit] =
    val valid =
      (actual, expected) match
        case (FunctionalKind.L1, DirectProximalKind.ElementwiseL1) => true
        case (FunctionalKind.GroupL21, DirectProximalKind.FeatureRowsL21) => true
        case (FunctionalKind.GroupL2(identity), DirectProximalKind.DisjointGroups(groups)) => identity == groups.valueIdentity
        case (FunctionalKind.SparseGroup(fraction, identity), DirectProximalKind.SparseGroup(expectedFraction, groups)) =>
          fraction == expectedFraction && identity == groups.valueIdentity
        case (FunctionalKind.ElasticNet(fraction), DirectProximalKind.ElasticNet(expectedFraction)) => fraction == expectedFraction
        case _ => false
    if valid then Right(()) else Left(ChartError.FunctionalMismatch(expected.toString, actual))

  private def validateGroups[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      chart: FeatureChart[Feature, Coordinates],
      kind: DirectProximalKind
  ): Either[ChartError, Unit] =
    kind match
      case DirectProximalKind.DisjointGroups(groups) => validateGroupStructure(chart, groups)
      case DirectProximalKind.SparseGroup(_, groups) => validateGroupStructure(chart, groups)
      case _ => Right(())

  private def validateGroupStructure[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      chart: FeatureChart[Feature, Coordinates],
      groups: GroupStructure
  ): Either[ChartError, Unit] =
    if groups.chartIdentity != chart.valueIdentity || groups.featureSpaceId != chart.featureSpace.descriptor.id then
      Left(ChartError.FeatureIdentityMismatch(chart.valueIdentity, groups.chartIdentity))
    else if groups.overlap == GroupOverlap.Overlapping then Left(ChartError.GroupOverlapUnsupported(groups.valueIdentity))
    else Right(())

  private def validateTarget[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      original: PenaltyTerm,
      chart: FeatureChart[Feature, Coordinates]
  ): Either[ChartError, Unit] =
    if original.target.operators.contains(chart.forward.valueIdentity) then Right(())
    else Left(ChartError.InvalidDefinition("penalty target must be the supplied feature chart"))

final class DirectProjectionPlan[Feature <: SemanticSpace, Coordinates <: SemanticSpace] private (
    val original: ConstraintTerm,
    val chart: FeatureChart[Feature, Coordinates]
):
  def apply(parameter: DMat): Either[ChartError, DMat] =
    chart.forward(parameter).left.map(ChartError.Semantic.apply).flatMap: coordinates =>
      val projected = ProximalMath.project(coordinates, original.feasibleSet)
      chart.kind match
        case ChartKind.Identity | ChartKind.Orthogonal(_) =>
          chart.synthesis(projected).left.map(ChartError.Semantic.apply)
        case ChartKind.Selection(_) =>
          for
            update <- chart.synthesis(MatrixOps.subtract(projected, coordinates)).left.map(ChartError.Semantic.apply)
          yield MatrixOps.subtract(parameter, MatrixOps.scale(update, -1.0))
        case other => Left(ChartError.UnsupportedDirectLowering(other))

object DirectProjectionPlan:
  def from[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      original: ConstraintTerm,
      chart: FeatureChart[Feature, Coordinates]
  ): Either[ChartError, DirectProjectionPlan[Feature, Coordinates]] =
    val supported =
      original.feasibleSet match
        case FeasibleSetKind.NonnegativeOrthant | FeasibleSetKind.Simplex | FeasibleSetKind.Monotone(_) |
            FeasibleSetKind.Box(_) => true
        case _ => false
    if !supported then Left(ChartError.SetMismatch(original.feasibleSet))
    else if !original.target.operators.contains(chart.forward.valueIdentity) then
      Left(ChartError.InvalidDefinition("constraint target must be the supplied feature chart"))
    else
      chart.kind match
        case ChartKind.Identity | ChartKind.Orthogonal(_) | ChartKind.Selection(_) =>
          Right(new DirectProjectionPlan(original, chart))
        case other => Left(ChartError.UnsupportedDirectLowering(other))

private[multivar] object ProximalMath:
  def softThreshold(value: DMat, threshold: Double): DMat =
    map(value): current =>
      Math.signum(current) * Math.max(0.0, Math.abs(current) - threshold)

  def groupRows(value: DMat, threshold: Double): DMat =
    val output = copyData(value)
    var row = 0
    while row < value.rows do
      var normSquared = 0.0
      var column = 0
      while column < value.cols do
        normSquared += value(row, column) * value(row, column)
        column += 1
      val norm = Math.sqrt(normSquared)
      val factor = if norm == 0.0 then 0.0 else Math.max(0.0, 1.0 - threshold / norm)
      column = 0
      while column < value.cols do
        output(row * value.cols + column) *= factor
        column += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(value.rows, value.cols, output)

  def groups(value: DMat, structure: GroupStructure, threshold: Double): DMat =
    val output = copyData(value)
    structure.groups.foreach: group =>
      var normSquared = 0.0
      group.indices.indices.foreach: row =>
        var column = 0
        while column < value.cols do
          normSquared += value(row, column) * value(row, column)
          column += 1
      val norm = Math.sqrt(normSquared)
      val factor = if norm == 0.0 then 0.0 else Math.max(0.0, 1.0 - threshold / norm)
      group.indices.indices.foreach: row =>
        var column = 0
        while column < value.cols do
          output(row * value.cols + column) *= factor
          column += 1
    GaleNumerics.matrixFromRowMajor(value.rows, value.cols, output)

  def project(value: DMat, feasibleSet: FeasibleSetKind): DMat =
    feasibleSet match
      case FeasibleSetKind.NonnegativeOrthant => map(value)(Math.max(0.0, _))
      case FeasibleSetKind.Box(bounds) => map(value)(current => Math.max(bounds.lower, Math.min(bounds.upper, current)))
      case FeasibleSetKind.Simplex => projectColumns(value, simplex)
      case FeasibleSetKind.Monotone(_) => projectColumns(value, isotonic)
      case other => throw new IllegalArgumentException(s"unsupported direct projection $other")

  private def simplex(values: Array[Double]): Array[Double] =
    val sorted = values.sorted.reverse
    var cumulative = 0.0
    var rho = -1
    var index = 0
    while index < sorted.length do
      cumulative += sorted(index)
      val threshold = (cumulative - 1.0) / (index + 1).toDouble
      if sorted(index) > threshold then rho = index
      index += 1
    val theta = (sorted.take(rho + 1).sum - 1.0) / (rho + 1).toDouble
    values.map(value => Math.max(0.0, value - theta))

  private def isotonic(values: Array[Double]): Array[Double] =
    val means = new Array[Double](values.length)
    val weights = new Array[Int](values.length)
    var blocks = 0
    var index = 0
    while index < values.length do
      means(blocks) = values(index)
      weights(blocks) = 1
      blocks += 1
      while blocks > 1 && means(blocks - 2) > means(blocks - 1) do
        val totalWeight = weights(blocks - 2) + weights(blocks - 1)
        means(blocks - 2) =
          (means(blocks - 2) * weights(blocks - 2) + means(blocks - 1) * weights(blocks - 1)) / totalWeight.toDouble
        weights(blocks - 2) = totalWeight
        blocks -= 1
      index += 1
    val output = new Array[Double](values.length)
    var offset = 0
    var block = 0
    while block < blocks do
      var current = 0
      while current < weights(block) do
        output(offset) = means(block)
        offset += 1
        current += 1
      block += 1
    output

  private def projectColumns(value: DMat, projection: Array[Double] => Array[Double]): DMat =
    val output = copyData(value)
    var column = 0
    while column < value.cols do
      val current = new Array[Double](value.rows)
      var row = 0
      while row < value.rows do
        current(row) = value(row, column)
        row += 1
      val projected = projection(current)
      row = 0
      while row < value.rows do
        output(row * value.cols + column) = projected(row)
        row += 1
      column += 1
    GaleNumerics.matrixFromRowMajor(value.rows, value.cols, output)

  private def map(value: DMat)(function: Double => Double): DMat =
    val output = copyData(value)
    var index = 0
    while index < output.length do
      output(index) = function(output(index))
      index += 1
    GaleNumerics.matrixFromRowMajor(value.rows, value.cols, output)

  private def copyData(value: DMat): Array[Double] =
    val output = new Array[Double](value.rows * value.cols)
    var row = 0
    while row < value.rows do
      var column = 0
      while column < value.cols do
        output(row * value.cols + column) = value(row, column)
        column += 1
      row += 1
    output
