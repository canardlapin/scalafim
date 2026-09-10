package scalafim.estimates

enum ResponseCoordinate:
  case NotApplicable
  case FirInterval(origin: String, startSeconds: Double, endSeconds: Double)
  case Sample(origin: String, seconds: Double)

  private[estimates] def valid: Boolean = this match
    case NotApplicable => true
    case FirInterval(origin, start, end) => Invariants.text(origin) && start.isFinite && end.isFinite && start < end
    case Sample(origin, seconds) => Invariants.text(origin) && seconds.isFinite

enum EstimandKind:
  case Coefficient, LinearContrast, BasisReadout, Hypothesis

final case class EstimandDefinition(
    id: EstimandId,
    label: String,
    kind: EstimandKind,
    units: String,
    normalization: String,
    definition: String,
    response: ResponseCoordinate = ResponseCoordinate.NotApplicable,
    unitScope: Option[UnitId] = None
):
  require(Vector(label, units, normalization, definition).forall(Invariants.text))
  require(response.valid, "response coordinates must be finite physical coordinates")

final case class EstimandCatalog(model: ModelRevisionId, entries: Vector[EstimandDefinition]):
  require(Invariants.unique(entries.map(_.id)), "catalog identities must be nonempty and unique; labels may repeat")
  def entry(id: EstimandId): Option[EstimandDefinition] = entries.find(_.id == id)

/** Exact realized operator: rows are output components, columns follow columnIds.
  * Vector ownership makes metadata immutable even if a producer reuses buffers.
  */
final case class EstimandBinding(
    estimand: EstimandId,
    columnIds: Vector[ColumnId],
    rows: Int,
    weights: Vector[Double]
):
  require(Invariants.unique(columnIds))
  require(rows > 0 && rows.toLong * columnIds.size == weights.size.toLong)
  require(weights.forall(_.isFinite))
