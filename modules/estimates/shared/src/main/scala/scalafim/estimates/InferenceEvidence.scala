package scalafim.estimates

/** Applicability of native coefficient inference, not a rank/subspace proof. */
final case class CoefficientInferenceEvidence(
    observation: ObservationId,
    columns: Vector[ColumnId],
    inferableColumns: Vector[ColumnId],
    scopeLabel: String,
    method: ScientificFact,
    conditioning: ScientificFact
):
  require(Invariants.unique(columns))
  require(inferableColumns.distinct.size == inferableColumns.size && inferableColumns.forall(columns.contains))
  require(Invariants.text(scopeLabel) && method.valid && conditioning.valid)

enum InferenceStatusCode(val code: Byte):
  case OutsideSupport extends InferenceStatusCode(0)
  case Unrecorded extends InferenceStatusCode(1)
  case Estimable extends InferenceStatusCode(2)
  case AllZero extends InferenceStatusCode(3)
  case Constant extends InferenceStatusCode(4)
  case NonFinite extends InferenceStatusCode(5)
  case NoObservedResponses extends InferenceStatusCode(6)
  case InsufficientResidualDegreesOfFreedom extends InferenceStatusCode(7)
  case RankDeficientObservedDesign extends InferenceStatusCode(8)
  case ZeroResidualVariance extends InferenceStatusCode(9)

object InferenceStatusCode:
  def fromCode(code: Byte): Either[EstimateError, InferenceStatusCode] =
    values.find(_.code == code).toRight(EstimateError.Invalid(s"unknown inference status code ${code & 0xff}"))

enum InferenceStatusScope:
  case Fit(observation: ObservationId)
  case Hypothesis(observation: ObservationId, hypothesisId: EstimandId)

  def observationId: ObservationId = this match
    case Fit(observation) => observation
    case Hypothesis(observation, _) => observation

/** Metadata depends on columns and declared planes, never the sample count. */
final case class InferenceEvidence(
    coefficients: Vector[CoefficientInferenceEvidence],
    planes: Vector[InferenceStatusScope]
):
  require(Invariants.unique(planes))
  require(coefficients.map(_.observation).distinct.size == coefficients.size)

  private[estimates] def agrees(unit: EstimateUnit): Boolean =
    val coefficientsAgree = coefficients.forall: evidence =>
      val realized = unit.products.filter(p => p.kind == ProductKind.Effect && p.observations.contains(evidence.observation))
        .flatMap(_.targets.estimands).distinct.filter(id => unit.catalog.entry(id).exists(_.kind == EstimandKind.Coefficient))
      realized.nonEmpty && unit.observations.exists(_.id == evidence.observation) &&
        realized.forall(id => unit.bindings.find(_.estimand == id).exists(_.columnIds == evidence.columns))
    coefficientsAgree && planes.forall: plane =>
      unit.observations.exists(_.id == plane.observationId) && (plane match
        case InferenceStatusScope.Fit(_) => true
        case InferenceStatusScope.Hypothesis(observation, hypothesis) =>
          unit.catalog.entry(hypothesis).exists(_.kind == EstimandKind.Hypothesis) &&
            unit.statistics.exists(s => unit.products.exists(p => p.id == s.product &&
              p.observations.contains(observation) && p.targets.estimands.contains(hypothesis))))

/** Plane-major, then selected physical sample order; arrays are borrowed. */
final case class InferenceStatusSelection(planes: Vector[InferenceStatusScope], samples: Vector[Int]):
  require(Invariants.unique(planes) && Invariants.unique(samples))
  def cells: Long = planes.size.toLong * samples.size.toLong

final case class InferenceStatusReceipt(selection: InferenceStatusSelection, cells: Int)

object InferenceStatusValidation:
  def check(unit: EstimateUnit, selection: InferenceStatusSelection, capacity: Int,
      maximumCells: Int): Either[EstimateError, InferenceEvidence] =
    unit.inferenceEvidence.toRight(EstimateError.Unsupported("unit has no recorded inference evidence")).flatMap: evidence =>
      if !selection.planes.forall(evidence.planes.contains) then Left(EstimateError.Invalid("unknown inference status plane"))
      else if selection.samples.exists(i => i < 0 || i >= unit.domain.sampleCount) then Left(EstimateError.Invalid("unknown sample"))
      else if selection.cells > maximumCells then Left(EstimateError.Invalid("inference status exceeds maximum block cells"))
      else if selection.cells > capacity then Left(EstimateError.Invalid("status array capacity is too small"))
      else Right(evidence)

trait InferenceEvidenceSource extends EstimateSource:
  /** A failure/cancellation leaves the destination unpublished scratch. */
  def readInferenceStatus(selection: InferenceStatusSelection, codes: Array[Byte],
      cancelled: () => Boolean = () => false): Either[EstimateError, InferenceStatusReceipt]

trait InferenceEvidenceSink extends EstimateSink:
  def writeInferenceStatus(selection: InferenceStatusSelection,
      codes: Array[Byte]): Either[EstimateError, InferenceStatusReceipt]
