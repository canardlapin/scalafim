package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.Matrix
import multivar.core.OperatorRepresentation
import multivar.core.SemanticSpace

enum RelationalCompilationError:
  case Evidence(error: EvidenceTableError)
  case Relation(error: RelationError)
  case Pairing(error: PairingDesignError)
  case DesignLeftMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case DesignRightMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case DesignWitnessMismatch(boundary: String)
  case MetricAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case MetricWitnessMismatch
  case MissingProjectedPartition(partition: PartitionId)
  case InvalidReducerWeight(value: Double)
  case NonFiniteResult(row: Int, column: Int, value: Double)
  case InvalidReceipt(detail: String)

  def message: String =
    this match
      case Evidence(error)                      => error.message
      case Relation(error)                      => error.message
      case Pairing(error)                       => error.message
      case DesignLeftMismatch(expected, actual) =>
        s"pairing left axis ${actual.value} does not match relation partitions ${expected.value}"
      case DesignRightMismatch(expected, actual) =>
        s"pairing right axis ${actual.value} does not match relation partitions ${expected.value}"
      case DesignWitnessMismatch(boundary) =>
        s"pairing $boundary axis and relations use different nominal witnesses"
      case MetricAxisMismatch(expected, actual) =>
        s"local neural query ${actual.value} does not match measurement output ${expected.value}"
      case MetricWitnessMismatch =>
        "local neural query and measurement output use different nominal witnesses"
      case MissingProjectedPartition(partition) =>
        s"pairing references partition '${partition.value}' without projected relation evidence"
      case InvalidReducerWeight(value) =>
        s"pairing reducer has invalid total weight $value"
      case NonFiniteResult(row, column, value) =>
        s"relational result contains non-finite value $value at ($row,$column)"
      case InvalidReceipt(detail) =>
        s"invalid relational computation receipt: $detail"

enum RelationalProjectionMode:
  case OperatorProjection
  case ExplicitLocalMaterialization

  def label: String =
    this match
      case OperatorProjection           => "operator-projection"
      case ExplicitLocalMaterialization => "explicit-local-materialization"

final case class PairEdgeReceipt(
    left: PartitionId,
    right: PartitionId,
    weight: Double
)

final case class PartitionProjectionReceipt(
    partition: PartitionId,
    sourceRepresentation: OperatorRepresentation,
    sourceEffects: Int,
    sourceNeuralCoordinates: Int,
    localNeuralCoordinates: Int,
    projectedCells: Long
)

final class RelationalComputationReceipt private (
    val projectionMode: RelationalProjectionMode,
    val design: DesignIdentity,
    val measurement: MeasurementIdentity,
    val neuralQuery: ScientificComponentFingerprint,
    val edges: Vector[PairEdgeReceipt],
    val reducer: PairingReducer,
    val reducerDenominator: Double,
    val projections: Vector[PartitionProjectionReceipt],
    val operatorApplications: Long,
    val materializations: Vector[MaterializationReceipt],
    val outputCells: Long,
    val avoidedFullRelationCells: Long,
    val measurementComposedBeforeProjection: Boolean
):
  def materializedCells: Long =
    materializations.map(_.elements).sum

object RelationalComputationReceipt:
  private[mvpa] def apply(
      projectionMode: RelationalProjectionMode,
      design: DesignIdentity,
      measurement: MeasurementIdentity,
      neuralQuery: ScientificComponentFingerprint,
      edges: Vector[PairEdgeReceipt],
      reducer: PairingReducer,
      reducerDenominator: Double,
      projections: Vector[PartitionProjectionReceipt],
      operatorApplications: Long,
      materializations: Vector[MaterializationReceipt],
      outputCells: Long,
      avoidedFullRelationCells: Long
  ): Either[RelationalCompilationError, RelationalComputationReceipt] =
    if edges.isEmpty then Left(RelationalCompilationError.InvalidReceipt("edge ledger is empty"))
    else if projections.isEmpty then Left(RelationalCompilationError.InvalidReceipt("projection ledger is empty"))
    else if !reducerDenominator.isFinite || reducerDenominator <= 0.0 then
      Left(RelationalCompilationError.InvalidReceipt("reducer denominator must be finite and positive"))
    else if operatorApplications < 0L || outputCells <= 0L || avoidedFullRelationCells < 0L then
      Left(RelationalCompilationError.InvalidReceipt("work counts must be non-negative and output must be non-empty"))
    else
      Right(
        new RelationalComputationReceipt(
          projectionMode,
          design,
          measurement,
          neuralQuery,
          edges,
          reducer,
          reducerDenominator,
          projections,
          operatorApplications,
          materializations,
          outputCells,
          avoidedFullRelationCells,
          measurementComposedBeforeProjection = true
        )
      )

private[mvpa] final class PairedEffectForm[
    Effects <: SemanticSpace,
    EffectKey
] private (
    val leftEffects: AxisRef.Aux[EffectKey, Effects],
    val rightEffects: AxisRef.Aux[EffectKey, Effects],
    val value: DMat,
    val receipt: RelationalComputationReceipt
)

private[mvpa] object PairedEffectForm:
  private[mvpa] def apply[E <: SemanticSpace, K](
      effects: AxisRef.Aux[K, E],
      value: DMat,
      receipt: RelationalComputationReceipt
  ): Either[RelationalCompilationError, PairedEffectForm[E, K]] =
    if value.rows != effects.size || value.cols != effects.size then
      Left(
        RelationalCompilationError.InvalidReceipt(
          s"effect form is ${value.rows}x${value.cols}, expected ${effects.size}x${effects.size}"
        )
      )
    else validateFinite(value).map(_ => new PairedEffectForm(effects, effects, value, receipt))

private[mvpa] object RelationalCompiler:
  def effectForm[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      LK,
      C <: RelationCapabilities[N, NK]
  ](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK],
      metric: NeuralQuery[measurement.local.Id, LK],
      materialization: MaterializationPolicy = MaterializationPolicy.Reject
  ): Either[RelationalCompilationError, PairedEffectForm[E, EK]] =
    for
      _ <- validateBindings(source, design, measurement, metric)
      projected <- projectRelations(source, design, measurement, materialization)
      metricValue <- resolveMetric(metric)
      denominator <- reducerDenominator(design)
      form <- reduceEffectForm(
        source.effects.size,
        measurement.local.size,
        projected.values,
        design,
        metricValue,
        denominator
      )
      _ <- validateFinite(form)
      receipt <- receipt(
        design,
        measurement,
        metric,
        projected,
        metricValue.operatorApplications,
        form.rows.toLong * form.cols.toLong,
        denominator
      )
      result <- PairedEffectForm(source.effects, form, receipt)
    yield result

  private final case class ProjectedRelations(
      values: Map[PartitionId, DMat],
      receipts: Vector[PartitionProjectionReceipt],
      materializations: Vector[MaterializationReceipt],
      operatorApplications: Long,
      projectionMode: RelationalProjectionMode
  )

  private final case class MetricValue(
      value: Option[DMat],
      operatorApplications: Long
  )

  private def validateBindings[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      LK,
      C <: RelationCapabilities[N, NK]
  ](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK],
      metric: NeuralQuery[measurement.local.Id, LK]
  ): Either[RelationalCompilationError, Unit] =
    if design.left.axis.identity != source.partitions.axis.identity then
      Left(
        RelationalCompilationError.DesignLeftMismatch(
          source.partitions.axis.identity.fingerprint,
          design.left.axis.identity.fingerprint
        )
      )
    else if design.right.axis.identity != source.partitions.axis.identity then
      Left(
        RelationalCompilationError.DesignRightMismatch(
          source.partitions.axis.identity.fingerprint,
          design.right.axis.identity.fingerprint
        )
      )
    else if !(design.left.axis.evidence eq source.partitions.axis.evidence) then
      Left(RelationalCompilationError.DesignWitnessMismatch("left"))
    else if !(design.right.axis.evidence eq source.partitions.axis.evidence) then
      Left(RelationalCompilationError.DesignWitnessMismatch("right"))
    else
      for
        _ <- design
          .validateSource(source.identity, source.partitions)
          .left
          .map(RelationalCompilationError.Pairing.apply)
        _ <-
          if metric.neural.identity != measurement.local.identity then
            Left(
              RelationalCompilationError.MetricAxisMismatch(
                measurement.local.identity.fingerprint,
                metric.neural.identity.fingerprint
              )
            )
          else if !(metric.neural.evidence eq measurement.local.evidence) then
            Left(RelationalCompilationError.MetricWitnessMismatch)
          else Right(())
        _ <- source
          .relation(source.partitionKeys.head)
          .left
          .map(RelationalCompilationError.Relation.apply)
          .flatMap(_.estimate.measureColumns(measurement).left.map(RelationalCompilationError.Evidence.apply))
          .map(_ => ())
      yield ()

  private def projectRelations[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      LK,
      C <: RelationCapabilities[N, NK]
  ](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK],
      policy: MaterializationPolicy
  ): Either[RelationalCompilationError, ProjectedRelations] =
    val needed = design.edges.iterator.flatMap(edge => Iterator(edge.left, edge.right)).toSet
    val partitions = source.partitionKeys.filter(needed.contains)
    val values = Map.newBuilder[PartitionId, DMat]
    val receipts = Vector.newBuilder[PartitionProjectionReceipt]
    val materializations = Vector.newBuilder[MaterializationReceipt]
    var applications = 0L
    var position = 0
    while position < partitions.length do
      val partition = partitions(position)
      val relation = source.relation(partition) match
        case Left(error)  => return Left(RelationalCompilationError.Relation(error))
        case Right(value) => value
      val measured = relation.estimate.measureColumns(measurement) match
        case Left(error)  => return Left(RelationalCompilationError.Evidence(error))
        case Right(value) => value
      val projected = policy match
        case MaterializationPolicy.Reject =>
          measured.transposeMultiply(DMat.eye(source.effects.size)) match
            case Left(error)  => return Left(RelationalCompilationError.Evidence(error))
            case Right(value) => value.t
        case allowed @ MaterializationPolicy.Allow(_) =>
          measured.materialize(allowed) match
            case Left(error)  => return Left(RelationalCompilationError.Evidence(error))
            case Right(value) =>
              materializations += value.receipt
              value.value
      applications += 1L
      values += partition -> projected
      receipts += PartitionProjectionReceipt(
        partition,
        relation.estimate.representation,
        source.effects.size,
        source.neuralAxis.size,
        measurement.local.size,
        source.effects.size.toLong * measurement.local.size.toLong
      )
      position += 1
    Right(
      ProjectedRelations(
        values.result(),
        receipts.result(),
        materializations.result(),
        applications,
        policy match
          case MaterializationPolicy.Reject   => RelationalProjectionMode.OperatorProjection
          case MaterializationPolicy.Allow(_) => RelationalProjectionMode.ExplicitLocalMaterialization
      )
    )

  private def resolveMetric[N <: SemanticSpace, K](
      metric: NeuralQuery[N, K]
  ): Either[RelationalCompilationError, MetricValue] =
    metric.kind match
      case NeuralQueryKind.IdentityInnerProduct => Right(MetricValue(None, 0L))
      case NeuralQueryKind.FixedPrecision       =>
        metric.form
          .rightMultiply(DMat.eye(metric.neural.size))
          .left
          .map(RelationalCompilationError.Evidence.apply)
          .map(value => MetricValue(Some(value), 1L))

  private def reducerDenominator[P <: SemanticSpace](
      design: PairingDesign[P, P]
  ): Either[RelationalCompilationError, Double] =
    design.reducer match
      case PairingReducer.WeightedSum  => Right(1.0)
      case PairingReducer.WeightedMean =>
        val total = design.edges.map(_.weight).sum
        if !total.isFinite || total <= 0.0 then Left(RelationalCompilationError.InvalidReducerWeight(total))
        else Right(total)

  private def reduceEffectForm[P <: SemanticSpace](
      effects: Int,
      local: Int,
      projected: Map[PartitionId, DMat],
      design: PairingDesign[P, P],
      metric: MetricValue,
      denominator: Double
  ): Either[RelationalCompilationError, DMat] =
    val output = Matrix.newBuilder(effects, effects)
    var edgePosition = 0
    while edgePosition < design.edges.length do
      val edge = design.edges(edgePosition)
      val left = projected.get(edge.left) match
        case None        => return Left(RelationalCompilationError.MissingProjectedPartition(edge.left))
        case Some(value) => value
      val right = projected.get(edge.right) match
        case None        => return Left(RelationalCompilationError.MissingProjectedPartition(edge.right))
        case Some(value) => value
      var leftEffect = 0
      while leftEffect < effects do
        var rightEffect = 0
        while rightEffect < effects do
          val contribution = bilinearRows(
            left,
            leftEffect,
            right,
            rightEffect,
            local,
            metric.value
          )
          output(leftEffect, rightEffect) += edge.weight * contribution / denominator
          rightEffect += 1
        leftEffect += 1
      edgePosition += 1
    Right(output.result())

  private def receipt[
      P <: SemanticSpace,
      N <: SemanticSpace,
      NK,
      LK
  ](
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK],
      metric: NeuralQuery[measurement.local.Id, LK],
      projected: ProjectedRelations,
      extraApplications: Long,
      outputCells: Long,
      denominator: Double
  ): Either[RelationalCompilationError, RelationalComputationReceipt] =
    val avoided = projected.receipts.map: value =>
      value.sourceEffects.toLong *
        (value.sourceNeuralCoordinates - value.localNeuralCoordinates).max(0).toLong
    RelationalComputationReceipt(
      projected.projectionMode,
      design.identity,
      measurement.identity,
      metric.identity,
      design.edges.map(edge => PairEdgeReceipt(edge.left, edge.right, edge.weight)),
      design.reducer,
      denominator,
      projected.receipts,
      projected.operatorApplications + extraApplications,
      projected.materializations,
      outputCells,
      avoided.sum
    )

  private def bilinearRows(
      left: DMat,
      leftRow: Int,
      right: DMat,
      rightRow: Int,
      dimension: Int,
      metric: Option[DMat]
  ): Double =
    metric match
      case None =>
        var sum = 0.0
        var coordinate = 0
        while coordinate < dimension do
          sum += left(leftRow, coordinate) * right(rightRow, coordinate)
          coordinate += 1
        sum
      case Some(form) =>
        var sum = 0.0
        var row = 0
        while row < dimension do
          var transformed = 0.0
          var column = 0
          while column < dimension do
            transformed += form(row, column) * right(rightRow, column)
            column += 1
          sum += left(leftRow, row) * transformed
          row += 1
        sum

private def validateFinite(
    value: DMat
): Either[RelationalCompilationError, Unit] =
  var row = 0
  while row < value.rows do
    var column = 0
    while column < value.cols do
      if !value(row, column).isFinite then
        return Left(
          RelationalCompilationError.NonFiniteResult(
            row,
            column,
            value(row, column)
          )
        )
      column += 1
    row += 1
  Right(())
