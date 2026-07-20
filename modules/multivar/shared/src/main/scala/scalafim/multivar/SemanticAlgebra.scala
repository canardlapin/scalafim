package scalafim.multivar

import scalafim.linalg.BlockDiagonalLinearMap
import scalafim.linalg.BlockLinearMap
import scalafim.linalg.CsrMatrix
import scalafim.linalg.DoubleMatrix
import scalafim.linalg.LinearMap
import scalafim.linalg.LinearMapError

/** Stable identity for an immutable numerical value or externally versioned object. */
opaque type ValueId = String

object ValueId:
  def apply(value: String): Either[SemanticError, ValueId] =
    Identifier.validate("value id", value).left.map(SemanticError.MultivarFailure.apply)

  def unsafe(value: String): ValueId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ValueId)
    inline def value: String = id

/** Structural identity of derived values. Adjoint and composition normalize the
  * duality laws instead of inventing unrelated object ids for derived operators.
  */
enum ValueIdentity:
  case Source(id: ValueId)
  case Adjoint(of: ValueIdentity)
  case Composition(first: ValueIdentity, second: ValueIdentity)
  case Derived(operation: String, inputs: Vector[ValueIdentity])

  def star: ValueIdentity =
    this match
      case ValueIdentity.Adjoint(of) => of
      case ValueIdentity.Composition(first, second) =>
        ValueIdentity.Composition(second.star, first.star)
      case value => ValueIdentity.Adjoint(value)

  def stableKey: String =
    this match
      case ValueIdentity.Source(id) => id.value
      case ValueIdentity.Adjoint(of) => s"star-${of.stableKey}"
      case ValueIdentity.Composition(first, second) =>
        s"compose-${first.stableKey}-${second.stableKey}"
      case ValueIdentity.Derived(operation, inputs) =>
        val cleanOperation = operation.map { character =>
          if character.isLetterOrDigit || character == '.' || character == '_' || character == '-' then character
          else '-'
        }
        s"$cleanOperation-${inputs.map(_.stableKey).mkString("-")}"

object ValueIdentity:
  def source(id: ValueId): ValueIdentity =
    ValueIdentity.Source(id)

  def compose(first: ValueIdentity, second: ValueIdentity): ValueIdentity =
    ValueIdentity.Composition(first, second)

  def derived(operation: String, inputs: ValueIdentity*): ValueIdentity =
    ValueIdentity.Derived(operation, inputs.toVector)

/** Phantom root for nominal scientific spaces. Dimensions deliberately remain runtime values. */
sealed trait SemanticSpace

/** A scoped Scala witness for a language-neutral [[MvSpace]]. Distinct refs have
  * distinct member types even when their serialized descriptors have equal dimensions.
  * Runtime compatibility is always decided from the stable descriptor, never object identity.
  */
final class SpaceRef private (val descriptor: MvSpace):
  sealed trait Id extends SemanticSpace

  val evidence: SpaceEvidence[Id] =
    SpaceEvidence.unsafe(descriptor)

object SpaceRef:
  def apply(descriptor: MvSpace): SpaceRef =
    new SpaceRef(descriptor)

  def of(id: String, role: SpaceRole, dimension: Int): Either[MultivarError, SpaceRef] =
    MvSpace.of(id, role, dimension).map(new SpaceRef(_))

final class SpaceEvidence[S <: SemanticSpace] private (val descriptor: MvSpace):
  def id: SpaceId =
    descriptor.id

  def dimension: Int =
    descriptor.size

object SpaceEvidence:
  private[multivar] def unsafe[S <: SemanticSpace](descriptor: MvSpace): SpaceEvidence[S] =
    new SpaceEvidence(descriptor)

enum CoordinateVariance:
  case Primal
  case Dual

sealed trait Coordinate
sealed trait Primal[S <: SemanticSpace] extends Coordinate
sealed trait Dual[S <: SemanticSpace] extends Coordinate

type DualOf[C <: Coordinate] <: Coordinate = C match
  case Primal[s] => Dual[s]
  case Dual[s]   => Primal[s]

final case class CoordinateDescriptor(space: MvSpace, variance: CoordinateVariance):
  def dimension: Int =
    space.size

final class CoordinateEvidence[C <: Coordinate] private[multivar] (
    val descriptor: CoordinateDescriptor
):
  def dimension: Int =
    descriptor.dimension

  private[multivar] def star: CoordinateEvidence[DualOf[C]] =
    val variance =
      descriptor.variance match
        case CoordinateVariance.Primal => CoordinateVariance.Dual
        case CoordinateVariance.Dual   => CoordinateVariance.Primal
    new CoordinateEvidence[DualOf[C]](descriptor.copy(variance = variance))

object CoordinateEvidence:
  def primal[S <: SemanticSpace](space: SpaceEvidence[S]): CoordinateEvidence[Primal[S]] =
    new CoordinateEvidence(CoordinateDescriptor(space.descriptor, CoordinateVariance.Primal))

  def dual[S <: SemanticSpace](space: SpaceEvidence[S]): CoordinateEvidence[Dual[S]] =
    new CoordinateEvidence(CoordinateDescriptor(space.descriptor, CoordinateVariance.Dual))

enum OperatorRepresentation:
  case Dense
  case Sparse
  case Diagonal
  case Block
  case LowRank
  case Kronecker
  case LazyAffine
  case MatrixFree

  def label: String =
    this match
      case Dense       => "dense"
      case Sparse      => "sparse"
      case Diagonal    => "diagonal"
      case Block       => "block"
      case LowRank     => "low-rank"
      case Kronecker   => "kronecker"
      case LazyAffine  => "lazy-affine"
      case MatrixFree  => "matrix-free"

object OperatorRepresentation:
  private[multivar] def fromLinearMap(operator: LinearMap): OperatorRepresentation =
    operator match
      case _: CsrMatrix              => OperatorRepresentation.Sparse
      case _: BlockDiagonalLinearMap => OperatorRepresentation.Block
      case _: BlockLinearMap         => OperatorRepresentation.Block
      case _                         => OperatorRepresentation.MatrixFree

  private[multivar] def fromMatrixView(view: MatrixView): OperatorRepresentation =
    view.storage match
      case StorageKind.Dense      => OperatorRepresentation.Dense
      case StorageKind.Sparse     => OperatorRepresentation.Sparse
      case StorageKind.LazyAffine => OperatorRepresentation.LazyAffine
      case StorageKind.Operator   => OperatorRepresentation.MatrixFree

enum SemanticProvenanceEvent:
  case Source(label: String)
  case Adapted(adapter: String)
  case Derived(operation: String, inputs: Vector[ValueIdentity])
  case Certified(property: String, method: String)
  case UnsafeAssumption(property: String, reason: String)

final case class SemanticProvenance(events: Vector[SemanticProvenanceEvent]):
  def append(event: SemanticProvenanceEvent): SemanticProvenance =
    copy(events = events :+ event)

  def ++(other: SemanticProvenance): SemanticProvenance =
    SemanticProvenance(events ++ other.events)

object SemanticProvenance:
  def source(label: String): SemanticProvenance =
    SemanticProvenance(Vector(SemanticProvenanceEvent.Source(label)))

enum SemanticError:
  case MultivarFailure(error: MultivarError)
  case LinearMapFailure(error: LinearMapError)
  case OperatorShapeMismatch(expectedRows: Int, expectedCols: Int, actualRows: Int, actualCols: Int)
  case CoordinateMismatch(role: String, expected: CoordinateDescriptor, actual: CoordinateDescriptor)
  case CertificateRejected(property: String, detail: String)
  case CertificateValueMismatch(expected: ValueIdentity, actual: ValueIdentity)
  case InvalidCertificateMetadata(detail: String)

  def message: String =
    this match
      case MultivarFailure(error) => error.message
      case LinearMapFailure(error) => error.message
      case OperatorShapeMismatch(expectedRows, expectedCols, actualRows, actualCols) =>
        s"semantic operator expected ${expectedRows}x${expectedCols} storage, got ${actualRows}x${actualCols}"
      case CoordinateMismatch(role, expected, actual) =>
        s"$role coordinate ${actual.space.id.value}:${actual.variance} does not match " +
          s"${expected.space.id.value}:${expected.variance}"
      case CertificateRejected(property, detail) =>
        s"$property certificate rejected: $detail"
      case CertificateValueMismatch(expected, actual) =>
        s"certificate belongs to $actual, but the operator identity is $expected"
      case InvalidCertificateMetadata(detail) =>
        detail

final case class LinDescriptor(
    domain: CoordinateDescriptor,
    codomain: CoordinateDescriptor,
    representation: OperatorRepresentation,
    valueIdentity: ValueIdentity
)

/** A directed semantic map backed by the linalg operator substrate.
  *
  * `From` and `To` carry nominal space plus primal/dual orientation. The numerical
  * handle remains private so raw operators cannot cross the semantic boundary by accident.
  */
final class Lin[From <: Coordinate, To <: Coordinate] private[multivar] (
    private[multivar] val kernel: SemanticKernel,
    val domain: CoordinateEvidence[From],
    val codomain: CoordinateEvidence[To],
    val valueIdentity: ValueIdentity,
    val provenance: SemanticProvenance
):
  val descriptor: LinDescriptor =
    LinDescriptor(domain.descriptor, codomain.descriptor, kernel.representation, valueIdentity)

  def rows: Int =
    codomain.dimension

  def cols: Int =
    domain.dimension

  def apply(input: DoubleMatrix): Either[SemanticError, DoubleMatrix] =
    kernel.forward(input)

  def andThen[Next <: Coordinate](next: Lin[To, Next]): Lin[From, Next] =
    new Lin(
      SemanticKernel.compose(kernel, next.kernel),
      domain,
      next.codomain,
      ValueIdentity.compose(valueIdentity, next.valueIdentity),
      (provenance ++ next.provenance).append(
        SemanticProvenanceEvent.Derived(
          "compose",
          Vector(valueIdentity, next.valueIdentity)
        )
      )
    )

  def star: Lin[DualOf[To], DualOf[From]] =
    new Lin(
      kernel.adjoint,
      codomain.star,
      domain.star,
      valueIdentity.star,
      provenance.append(SemanticProvenanceEvent.Derived("adjoint", Vector(valueIdentity)))
    )

object Lin:
  def fromDenseMatrix[From <: Coordinate, To <: Coordinate](
      matrix: DoubleMatrix,
      domain: CoordinateEvidence[From],
      codomain: CoordinateEvidence[To],
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance = SemanticProvenance.source("dense-linear-map")
  ): Either[SemanticError, Lin[From, To]] =
    fromKernel(DenseMatrixKernel(matrix), domain, codomain, valueIdentity, provenance)

  def fromLinearMap[From <: Coordinate, To <: Coordinate](
      operator: LinearMap,
      domain: CoordinateEvidence[From],
      codomain: CoordinateEvidence[To],
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance = SemanticProvenance.source("linalg-linear-map")
  ): Either[SemanticError, Lin[From, To]] =
    fromKernel(LinearMapKernel(operator), domain, codomain, valueIdentity, provenance)

  /** Runtime boundary used by language-neutral decoding. The declared descriptors
    * must agree with the caller's expected nominal coordinates before storage is accepted.
    */
  def decode[From <: Coordinate, To <: Coordinate](
      operator: LinearMap,
      expectedDomain: CoordinateEvidence[From],
      expectedCodomain: CoordinateEvidence[To],
      declaredDomain: CoordinateDescriptor,
      declaredCodomain: CoordinateDescriptor,
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[SemanticError, Lin[From, To]] =
    if expectedDomain.descriptor != declaredDomain then
      Left(SemanticError.CoordinateMismatch("domain", expectedDomain.descriptor, declaredDomain))
    else if expectedCodomain.descriptor != declaredCodomain then
      Left(SemanticError.CoordinateMismatch("codomain", expectedCodomain.descriptor, declaredCodomain))
    else fromLinearMap(operator, expectedDomain, expectedCodomain, valueIdentity, provenance)

  private[multivar] def fromKernel[From <: Coordinate, To <: Coordinate](
      kernel: SemanticKernel,
      domain: CoordinateEvidence[From],
      codomain: CoordinateEvidence[To],
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[SemanticError, Lin[From, To]] =
    if kernel.rows != codomain.dimension || kernel.cols != domain.dimension then
      Left(
        SemanticError.OperatorShapeMismatch(
          codomain.dimension,
          domain.dimension,
          kernel.rows,
          kernel.cols
        )
      )
    else Right(new Lin(kernel, domain, codomain, valueIdentity, provenance))

type Table[Rows <: SemanticSpace, Columns <: SemanticSpace] = Lin[Dual[Columns], Primal[Rows]]

object Table:
  def fromMatrixView[Rows <: SemanticSpace, Columns <: SemanticSpace](
      view: MatrixView,
      rows: SpaceEvidence[Rows],
      columns: SpaceEvidence[Columns],
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance = SemanticProvenance.source("matrix-view-table")
  ): Either[SemanticError, Table[Rows, Columns]] =
    Lin.fromKernel(
      MatrixViewKernel(view),
      CoordinateEvidence.dual(columns),
      CoordinateEvidence.primal(rows),
      valueIdentity,
      provenance.append(SemanticProvenanceEvent.Adapted("MatrixView"))
    )

private[multivar] trait SemanticKernel:
  def rows: Int
  def cols: Int
  def representation: OperatorRepresentation
  def linearMap: LinearMap
  def forward(input: DoubleMatrix): Either[SemanticError, DoubleMatrix]
  def adjoint: SemanticKernel

private[multivar] final case class LinearMapKernel(operator: LinearMap) extends SemanticKernel:
  override def rows: Int = operator.rows
  override def cols: Int = operator.cols
  override def representation: OperatorRepresentation =
    OperatorRepresentation.fromLinearMap(operator)
  override def linearMap: LinearMap = operator
  override def forward(input: DoubleMatrix): Either[SemanticError, DoubleMatrix] =
    operator.forward(input).left.map(SemanticError.LinearMapFailure.apply)
  override def adjoint: SemanticKernel =
    LinearMapKernel(operator.adjoint)

private[multivar] final case class DenseMatrixKernel(matrix: DoubleMatrix) extends SemanticKernel:
  private val adapted = DenseMatrixLinearMap(matrix)

  override def rows: Int = matrix.rows
  override def cols: Int = matrix.cols
  override def representation: OperatorRepresentation = OperatorRepresentation.Dense
  override def linearMap: LinearMap = adapted
  override def forward(input: DoubleMatrix): Either[SemanticError, DoubleMatrix] =
    adapted.forward(input).left.map(SemanticError.LinearMapFailure.apply)
  override def adjoint: SemanticKernel =
    DenseMatrixKernel(matrix.transpose)

private[multivar] final case class MatrixViewKernel(view: MatrixView) extends SemanticKernel:
  private val adapted = MatrixViewLinearMap(view)

  override def rows: Int = view.rows
  override def cols: Int = view.cols
  override def representation: OperatorRepresentation =
    OperatorRepresentation.fromMatrixView(view)
  override def linearMap: LinearMap = adapted
  override def forward(input: DoubleMatrix): Either[SemanticError, DoubleMatrix] =
    view.rightMultiply(input).left.map(SemanticError.MultivarFailure.apply)
  override def adjoint: SemanticKernel =
    MatrixViewKernel(view.transposeView)

private[multivar] final case class ComposedSemanticKernel(
    first: SemanticKernel,
    second: SemanticKernel,
    linearMap: LinearMap
) extends SemanticKernel:
  override def rows: Int = second.rows
  override def cols: Int = first.cols
  override def representation: OperatorRepresentation =
    (first.representation, second.representation) match
      case (OperatorRepresentation.Block, OperatorRepresentation.Block) => OperatorRepresentation.Block
      case _                                                            => OperatorRepresentation.MatrixFree
  override def forward(input: DoubleMatrix): Either[SemanticError, DoubleMatrix] =
    first.forward(input).flatMap(second.forward)
  override def adjoint: SemanticKernel =
    SemanticKernel.compose(second.adjoint, first.adjoint)

private[multivar] object SemanticKernel:
  def compose(first: SemanticKernel, second: SemanticKernel): SemanticKernel =
    val composed = LinearMap.compose(first.linearMap, second.linearMap).fold(
      error => throw new IllegalStateException(error.message),
      identity
    )
    ComposedSemanticKernel(first, second, composed)

private final case class MatrixViewLinearMap(view: MatrixView) extends LinearMap:
  override def rows: Int = view.rows
  override def cols: Int = view.cols

  override def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    if input.rows != cols then Left(LinearMapError.DimensionMismatch(cols, input.rows))
    else view.rightMultiply(input).left.map(error => LinearMapError.OperatorApplicationFailed(error.message))

  override def adjoint: LinearMap =
    MatrixViewLinearMap(view.transposeView)

private final case class DenseMatrixLinearMap(matrix: DoubleMatrix) extends LinearMap:
  override def rows: Int = matrix.rows
  override def cols: Int = matrix.cols

  override def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    if input.rows != cols then Left(LinearMapError.DimensionMismatch(cols, input.rows))
    else Right(DoubleMatrix.multiply(matrix, input))

  override def adjoint: LinearMap =
    DenseMatrixLinearMap(matrix.transpose)
