package scalafim.spatial

import scalafim.image.{SpatialAxis, VoxelCoord}
import scalafim.linalg.DoubleMatrix

enum FieldApiError:
  case Spatial(error: SpatialError)
  case Plan(error: ViewPlanError)
  case Demand(error: DemandError)
  case DomainDefinitionMismatch(id: DomainId)
  case MaterializedShapeMismatch(
    expectedRows: Int,
    actualRows: Int,
    expectedObservations: Int,
    actualObservations: Int
  )

  def message: String =
    this match
      case Spatial(error) =>
        error.message
      case Plan(error) =>
        error.message
      case Demand(error) =>
        error.message
      case DomainDefinitionMismatch(id) =>
        s"domain ${id.value} does not match the definition registered in the spatial graph"
      case MaterializedShapeMismatch(expectedRows, actualRows, expectedObservations, actualObservations) =>
        s"materialized field expected ${expectedRows}x$expectedObservations values, got ${actualRows}x$actualObservations"

extension (field: Field)
  def explain: FieldExplanation =
    FieldExplanation.from(field)

  def to(
    target: Domain,
    routing: RoutingPolicy = RoutingPolicy.Shortest,
    sampling: SamplingPolicy = SamplingPolicy.Trilinear,
    allowInverses: Boolean = false
  )(using graph: SpatialGraph): Either[FieldApiError, Field] =
    reexpress(field, target, routing, sampling, allowInverses)

  def in(
    target: Domain,
    routing: RoutingPolicy = RoutingPolicy.Shortest,
    sampling: SamplingPolicy = SamplingPolicy.Trilinear,
    allowInverses: Boolean = false
  )(using graph: SpatialGraph): Either[FieldApiError, Field] =
    reexpress(field, target, routing, sampling, allowInverses)

  def select(requested: FieldDemand)(using graph: SpatialGraph): Either[FieldApiError, Field] =
    for
      domain <- graph.domain(field.domain).left.map(error => FieldApiError.Spatial(error))
      nextPlan <- field.plan.select(requested, domain).left.map(error => FieldApiError.Plan(error))
    yield Field.withPlan(field, nextPlan)

  def rows(indices: Int*)(using graph: SpatialGraph): Either[FieldApiError, Field] =
    field.select(FieldDemand.rows(indices.toVector))

  def vertices(indices: Int*)(using graph: SpatialGraph): Either[FieldApiError, Field] =
    field.select(FieldDemand.vertices(indices.toVector))

  def voxels(coords: VoxelCoord*)(using graph: SpatialGraph): Either[FieldApiError, Field] =
    field.select(FieldDemand.voxels(coords.toVector))

  def roi(indices: Int*)(using graph: SpatialGraph): Either[FieldApiError, Field] =
    field.select(FieldDemand.roi(indices.toVector))

  def slice(axis: SpatialAxis, index: Int)(using graph: SpatialGraph): Either[FieldApiError, Field] =
    field.select(FieldDemand.slice(axis, index))

  def mask(included: Vector[Boolean])(using graph: SpatialGraph): Either[FieldApiError, Field] =
    field.select(FieldDemand.mask(included))

  def region(region: VoxelRegion)(using graph: SpatialGraph): Either[FieldApiError, Field] =
    field.select(FieldDemand.region(region))

  def timeBlock(start: Int, length: Int)(using graph: SpatialGraph): Either[FieldApiError, Field] =
    TimeBlock
      .build(start, length)
      .left
      .map(error => FieldApiError.Demand(error))
      .flatMap(block => field.select(FieldDemand.time(block)))

  def value(using runtime: FieldRuntime): Either[FieldApiError, DoubleMatrix] =
    runtime.data(field).left.map(error => FieldApiError.Spatial(error))

  def materialize(using runtime: FieldRuntime): Either[FieldApiError, Field] =
    runtime
      .materialize(field)
      .left
      .map {
        case SpatialError.FieldMaterializedShapeMismatch(expectedRows, actualRows, expectedObservations, actualObservations) =>
          FieldApiError.MaterializedShapeMismatch(
            expectedRows,
            actualRows,
            expectedObservations,
            actualObservations
          )
        case SpatialError.FieldShapeMismatch(_, actual) =>
          FieldApiError.MaterializedShapeMismatch(
            field.sampleCount,
            actual,
            field.observations,
            field.observations
          )
        case SpatialError.FieldObservationMismatch(_, actual) =>
          FieldApiError.MaterializedShapeMismatch(
            field.sampleCount,
            field.sampleCount,
            field.observations,
            actual
          )
        case error =>
          FieldApiError.Spatial(error)
      }

private def reexpress(
  field: Field,
  target: Domain,
  routing: RoutingPolicy,
  sampling: SamplingPolicy,
  allowInverses: Boolean
)(using graph: SpatialGraph): Either[FieldApiError, Field] =
  for
    _ <- graph.domain(field.domain).left.map(error => FieldApiError.Spatial(error))
    registered <- graph.domain(target.id).left.map(error => FieldApiError.Spatial(error))
    _ <-
      if registered == target then Right(())
      else Left(FieldApiError.DomainDefinitionMismatch(target.id))
    nextPlan <- field.plan
      .reexpress(target.id, target.nElements, routing, sampling, allowInverses)
      .left
      .map(error => FieldApiError.Plan(error))
    _ <- graph
      .path(field.domain, target.id, routing, allowInverses)
      .left
      .map(error => FieldApiError.Spatial(error))
  yield Field.withPlan(field, nextPlan)
