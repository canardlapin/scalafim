package scalafim.multivar.ir

enum UnknownFieldPolicy:
  case Reject

final case class SchemaVersion(major: Int, minor: Int):
  require(major >= 0 && minor >= 0, "schema version components must be non-negative")

object SchemaVersion:
  val v0_1: SchemaVersion = SchemaVersion(0, 1)

final case class SchemaHeader(
    name: String,
    version: SchemaVersion,
    unknownFields: UnknownFieldPolicy
)

enum SpaceRoleIr:
  case Samples
  case Observed
  case Latent
  case Kernel
  case Block

final case class SpaceIr(id: String, role: SpaceRoleIr, dimension: Int)

enum VarianceIr:
  case Primal
  case Dual

final case class CoordinateIr(spaceId: String, variance: VarianceIr)

enum OperatorRoleIr:
  case Table
  case LinearMap
  case RowMap
  case RowLink
  case LinearConstraint

enum PayloadIr:
  case InlineDense(
      override val rows: Int,
      override val columns: Int,
      values: Vector[Double],
      override val sha256: String
  )
  case InlineSparse(
      override val rows: Int,
      override val columns: Int,
      rowIndices: Vector[Int],
      columnIndices: Vector[Int],
      values: Vector[Double],
      override val sha256: String
  )
  case External(
      uri: String,
      mediaType: String,
      override val rows: Int,
      override val columns: Int,
      override val sha256: String
  )

  def rows: Int =
    this match
      case InlineDense(rows, _, _, _) => rows
      case InlineSparse(rows, _, _, _, _, _) => rows
      case External(_, _, rows, _, _) => rows

  def columns: Int =
    this match
      case InlineDense(_, columns, _, _) => columns
      case InlineSparse(_, columns, _, _, _, _) => columns
      case External(_, _, _, columns, _) => columns

  def sha256: String =
    this match
      case InlineDense(_, _, _, sha256) => sha256
      case InlineSparse(_, _, _, _, _, sha256) => sha256
      case External(_, _, _, _, sha256) => sha256

final case class OperatorIr(
    id: String,
    role: OperatorRoleIr,
    domain: CoordinateIr,
    codomain: CoordinateIr,
    representation: String,
    payload: PayloadIr,
    valueIdentity: String,
    provenance: Vector[ProvenanceEventIr]
)

enum FormRoleIr:
  case Bilinear
  case Symmetric
  case SemiMetric
  case Metric
  case Indefinite
  case Cometric
  case Penalty
  case Kernel
  case Covariance
  case Precision

enum FormStructureIr:
  case General
  case Symmetric

enum PositivityIr:
  case Unchecked
  case Psd
  case Spd
  case Indefinite

enum EvidenceStatusIr:
  case Unchecked
  case Certified
  case Assumed

enum ScaleSemanticsIr:
  case AbsoluteMetric
  case NormalizedMetric
  case ShapeMetric(gaugeId: String)

final case class ToleranceIr(absolute: Double, relative: Double)

final case class CertificateIr(
    property: String,
    valueIdentity: String,
    tolerance: ToleranceIr,
    norm: String,
    method: String,
    precision: String,
    backend: String,
    regularization: Option[String],
    residual: Option[Double]
)

final case class FormIr(
    id: String,
    role: FormRoleIr,
    spaceId: String,
    operatorId: String,
    structure: FormStructureIr,
    positivity: PositivityIr,
    evidence: EvidenceStatusIr,
    scaleSemantics: ScaleSemanticsIr,
    certificates: Vector[CertificateIr]
)

enum MeasureNormalizationIr:
  case UnitMass(originalMass: Double)

final case class MeasureIr(
    id: String,
    spaceId: String,
    weights: PayloadIr,
    normalization: MeasureNormalizationIr,
    valueIdentity: String
)

enum CenteringIr:
  case None
  case ByMeasure(measureId: String)
  case Orthogonal(rowFormId: String, derivedMeasureId: Option[String])
  case AlreadyCentered(measureId: String, certificate: CertificateIr)

enum SingularityPolicyIr:
  case Reject
  case RestrictToSupport(threshold: Double)
  case Regularize(amount: Double)
  case Quotient(threshold: Double)

enum MissingnessIr:
  case Complete
  case MissingCells(maskPayloadId: String)

final case class DiagramIr(
    id: String,
    tableOperatorId: String,
    rowFormId: String,
    columnFormId: String,
    measureId: Option[String],
    centering: CenteringIr,
    rowSingularity: SingularityPolicyIr,
    columnSingularity: SingularityPolicyIr,
    missingness: MissingnessIr,
    normalization: String,
    provenance: Vector[ProvenanceEventIr]
)

enum RelationshipKindIr:
  case SameEntityEvidence
  case ExactBijection
  case PartialInjection
  case IncidenceMap
  case AggregationMap
  case NonnegativeCoupling
  case ProbabilisticCoupling
  case SignedRowLink
  case IncidenceInducedLink
  case HubInducedLink

enum CardinalityIr:
  case OneToOne
  case OneToMany
  case ManyToOne
  case ManyToMany

enum RelationshipNormalizationIr:
  case Unnormalized
  case UnitMass
  case SourceMassPreserving
  case TargetMassPreserving
  case DoublyStochastic
  case ConditionalOnSource
  case ConditionalOnTarget

enum AlignmentOriginIr:
  case ObservedKeys
  case ExternallySupplied
  case UnsafeAssumption

final case class RelationshipSupportIr(
    matchedMass: Double,
    unmatchedSourceMass: Double,
    unmatchedTargetMass: Double,
    uncertainMass: Double,
    excludedMass: Double,
    structuralZeroCount: Int,
    cardinality: CardinalityIr,
    duplicateKeys: Vector[String],
    everySourceRepresented: Boolean,
    everyTargetRepresented: Boolean
)

final case class MarginalsIr(
    left: PayloadIr,
    right: PayloadIr,
    totalMass: Double
)

final case class RelationshipIr(
    id: String,
    kind: RelationshipKindIr,
    operatorId: String,
    support: RelationshipSupportIr,
    normalization: RelationshipNormalizationIr,
    marginals: Option[MarginalsIr],
    origin: AlignmentOriginIr,
    provenance: Vector[ProvenanceEventIr]
)

enum ObjectiveFormulaIr:
  case PairwiseAssociation
  case QuadraticDisagreement
  case HardScoreAgreement

enum ObjectiveNormalizationIr:
  case DirectSumMetricOrthonormal
  case PerViewMetricOrthonormal
  case ConstraintOnly

enum SolverFormulationIr:
  case SymmetricFeatureEigen
  case QuadraticPenalty
  case LinearEquality

enum SolvedToReportedIr:
  case Identical
  case Scale(factor: Double)
  case Transform(description: String)

final case class ObjectiveIr(
    id: String,
    formula: ObjectiveFormulaIr,
    renderedFormula: String,
    constraints: Vector[String],
    normalization: ObjectiveNormalizationIr,
    solverFormulation: SolverFormulationIr,
    solvedToReported: SolvedToReportedIr
)

enum ProvenanceEventIr:
  case Source(label: String)
  case Adapted(adapter: String)
  case Derived(operation: String, inputs: Vector[String])
  case Certified(property: String, method: String)
  case UnsafeAssumption(property: String, reason: String)

final case class MultivarIrDocument(
    schema: SchemaHeader,
    spaces: Vector[SpaceIr],
    operators: Vector[OperatorIr],
    forms: Vector[FormIr],
    measures: Vector[MeasureIr],
    diagrams: Vector[DiagramIr],
    relationships: Vector[RelationshipIr],
    objectives: Vector[ObjectiveIr]
)

object MultivarIrDocument:
  def empty: MultivarIrDocument =
    MultivarIrDocument(
      SchemaHeader("scalafim-multivar-ir", SchemaVersion.v0_1, UnknownFieldPolicy.Reject),
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty
    )

enum RejectionCategory:
  case DomainCodomainMismatch
  case UncertifiedPositivity
  case UnsupportedSingularity
  case IncompatibleAlignmentKind
  case PayloadTampered
  case SchemaVersionMismatch
  case UnknownField
  case Malformed

final case class IrError(category: RejectionCategory, path: String, detail: String):
  def message: String = s"$path: $detail"
