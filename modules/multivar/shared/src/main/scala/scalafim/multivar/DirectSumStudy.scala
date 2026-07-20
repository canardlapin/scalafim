package scalafim.multivar

import scalafim.linalg.LinearMap
import scalafim.linalg.LinearMapBlock

enum DirectSumError:
  case Semantic(error: SemanticError)
  case Alignment(error: AlignmentError)
  case Multivar(error: MultivarError)
  case InvalidStudy(detail: String)
  case UnknownView(id: BlockId)
  case IncompatibleRelationship(detail: String)

  def message: String =
    this match
      case Semantic(error) => error.message
      case Alignment(error) => error.message
      case Multivar(error) => error.message
      case InvalidStudy(detail) => detail
      case UnknownView(id) => s"unknown direct-sum view '${id.value}'"
      case IncompatibleRelationship(detail) => detail

sealed trait CompleteStudyView:
  type Rows <: SemanticSpace
  type Columns <: SemanticSpace
  def id: BlockId
  def diagram: SemanticDualityDiagram[Rows, Columns, CompleteCells]

object CompleteStudyView:
  def apply[R <: SemanticSpace, C <: SemanticSpace](
      viewId: BlockId,
      value: SemanticDualityDiagram[R, C, CompleteCells]
  ): CompleteStudyView { type Rows = R; type Columns = C } =
    new CompleteStudyView:
      type Rows = R
      type Columns = C
      override val id: BlockId = viewId
      override val diagram: SemanticDualityDiagram[R, C, CompleteCells] = value

final case class DirectSumBlock(
    id: BlockId,
    rowSpace: MvSpace,
    featureSpace: MvSpace,
    rowOffset: Int,
    featureOffset: Int
)

final case class PsdConstructionCertificate(
    value: ValueIdentity,
    inputs: Vector[ValueIdentity],
    construction: String,
    proof: String
)

final class DirectSumColumnGeometry[S <: SemanticSpace] private[multivar] (
    val operator: Lin[Primal[S], Dual[S]],
    val space: SpaceEvidence[S],
    val blockGeometries: Vector[DiagramGeometry[?]],
    val psdCertificate: PsdConstructionCertificate,
    val isSpd: Boolean
)

final class DirectSumStudy private (
    val studyId: ValueId,
    val views: Vector[CompleteStudyView],
    val rowSpace: SpaceRef,
    val featureSpace: SpaceRef,
    val blocks: Vector[DirectSumBlock]
)(
    val table: Table[rowSpace.Id, featureSpace.Id],
    val columnGeometry: DirectSumColumnGeometry[featureSpace.Id],
    val provenance: SemanticProvenance
):
  def viewIndex(id: BlockId): Either[DirectSumError, Int] =
    val index = blocks.indexWhere(_.id == id)
    if index < 0 then Left(DirectSumError.UnknownView(id)) else Right(index)

  def block(id: BlockId): Either[DirectSumError, DirectSumBlock] =
    viewIndex(id).map(blocks)

object DirectSumStudy:
  def from(
      studyId: ValueId,
      views: Vector[CompleteStudyView],
      provenance: SemanticProvenance = SemanticProvenance.source("direct-sum-study")
  ): Either[DirectSumError, DirectSumStudy] =
    if views.isEmpty then Left(DirectSumError.InvalidStudy("direct-sum study requires at least one view"))
    else if views.map(_.id).distinct.length != views.length then
      Left(DirectSumError.InvalidStudy("direct-sum study view ids must be unique"))
    else
      val rowDimension = views.map(_.diagram.core.rowGeometry.space.dimension).sum
      val featureDimension = views.map(_.diagram.core.columnGeometry.space.dimension).sum
      val rowSpace = SpaceRef(
        MvSpace(SpaceId.unsafe(s"${studyId.value}.rows"), SpaceRole.Block, Dimension.unsafe(rowDimension))
      )
      val featureSpace = SpaceRef(
        MvSpace(SpaceId.unsafe(s"${studyId.value}.features"), SpaceRole.Block, Dimension.unsafe(featureDimension))
      )
      val blocks = describeBlocks(views)
      for
        tableMap <- LinearMap
          .blockDiag(views.map(_.diagram.core.table.kernel.linearMap))
          .left
          .map(error => DirectSumError.Semantic(SemanticError.LinearMapFailure(error)))
        tableIdentity = ValueIdentity.Derived("direct-sum-table", views.map(_.diagram.core.table.valueIdentity))
        table <- Lin
          .fromLinearMap[Dual[featureSpace.Id], Primal[rowSpace.Id]](
            tableMap,
            CoordinateEvidence.dual(featureSpace.evidence),
            CoordinateEvidence.primal(rowSpace.evidence),
            tableIdentity,
            provenance
          )
          .left
          .map(DirectSumError.Semantic.apply)
        geometryMap <- LinearMap
          .blockDiag(views.map(_.diagram.core.columnGeometry.operator.kernel.linearMap))
          .left
          .map(error => DirectSumError.Semantic(SemanticError.LinearMapFailure(error)))
        geometryIdentity = ValueIdentity.Derived(
          "direct-sum-column-geometry",
          views.map(_.diagram.core.columnGeometry.operator.valueIdentity)
        )
        geometryOperator <- Lin
          .fromLinearMap[Primal[featureSpace.Id], Dual[featureSpace.Id]](
            geometryMap,
            CoordinateEvidence.primal(featureSpace.evidence),
            CoordinateEvidence.dual(featureSpace.evidence),
            geometryIdentity,
            provenance
          )
          .left
          .map(DirectSumError.Semantic.apply)
      yield
        val geometries = views.map(_.diagram.core.columnGeometry)
        val directGeometry = new DirectSumColumnGeometry(
          geometryOperator,
          featureSpace.evidence,
          geometries,
          PsdConstructionCertificate(
            geometryIdentity,
            geometries.map(_.operator.valueIdentity),
            "block-diagonal",
            "a block diagonal operator with certified PSD blocks is PSD"
          ),
          geometries.forall(_.isSpd)
        )
        new DirectSumStudy(studyId, views, rowSpace, featureSpace, blocks)(
          table,
          directGeometry,
          provenance.append(
            SemanticProvenanceEvent.Derived(
              "direct-sum-study",
              Vector(tableIdentity, geometryIdentity)
            )
          )
        )

  private def describeBlocks(views: Vector[CompleteStudyView]): Vector[DirectSumBlock] =
    val out = Vector.newBuilder[DirectSumBlock]
    var rowOffset = 0
    var featureOffset = 0
    var index = 0
    while index < views.length do
      val view = views(index)
      val rows = view.diagram.core.rowGeometry.space.descriptor
      val features = view.diagram.core.columnGeometry.space.descriptor
      out += DirectSumBlock(view.id, rows, features, rowOffset, featureOffset)
      rowOffset += rows.size
      featureOffset += features.size
      index += 1
    out.result()

final case class DirectSumRowBlock(
    rowBlock: Int,
    columnBlock: Int,
    operator: scalafim.linalg.LinearMap,
    valueIdentity: ValueIdentity
)

private[multivar] object DirectSumOperators:
  def rowOperator(
      study: DirectSumStudy,
      blocks: Vector[DirectSumRowBlock],
      operation: String,
      provenance: SemanticProvenance
  ): Either[DirectSumError, Lin[Primal[study.rowSpace.Id], Dual[study.rowSpace.Id]]] =
    val sizes = study.blocks.map(_.rowSpace.size)
    for
      blockMap <- LinearMap
        .blockMatrix(
          sizes,
          sizes,
          blocks.map(block => LinearMapBlock(block.rowBlock, block.columnBlock, block.operator))
        )
        .left
        .map(error => DirectSumError.Semantic(SemanticError.LinearMapFailure(error)))
      identity = ValueIdentity.Derived(operation, blocks.map(_.valueIdentity))
      operator <- Lin
        .fromLinearMap[Primal[study.rowSpace.Id], Dual[study.rowSpace.Id]](
          blockMap,
          CoordinateEvidence.primal(study.rowSpace.evidence),
          CoordinateEvidence.dual(study.rowSpace.evidence),
          identity,
          provenance
        )
        .left
        .map(DirectSumError.Semantic.apply)
    yield operator

  def independentRowGeometry(
      study: DirectSumStudy
  ): Either[DirectSumError, RowGeometry[study.rowSpace.Id]] =
    val blocks = study.views.zipWithIndex.map { case (view, index) =>
      DirectSumRowBlock(
        index,
        index,
        view.diagram.core.rowGeometry.operator.kernel.linearMap,
        view.diagram.core.rowGeometry.operator.valueIdentity
      )
    }
    rowOperator(study, blocks, "independent-row-geometry", study.provenance).map { operator =>
      new RowGeometry(
        operator,
        PsdConstructionCertificate(
          operator.valueIdentity,
          blocks.map(_.valueIdentity),
          "block-diagonal",
          "independent certified PSD row geometries form a PSD direct-sum row geometry"
        )
      )
    }

final class RowGeometry[S <: SemanticSpace] private[multivar] (
    val operator: Lin[Primal[S], Dual[S]],
    val psdCertificate: PsdConstructionCertificate
)

final class ConstraintPenalty[S <: SemanticSpace] private[multivar] (
    val operator: Lin[Primal[S], Dual[S]],
    val psdCertificate: PsdConstructionCertificate
)

final class SymmetricObjectiveForm[S <: SemanticSpace] private[multivar] (
    val operator: Lin[Primal[S], Dual[S]],
    val adjointCertificates: Vector[AdjointConsistencyCertificate],
    val potentiallyIndefinite: Boolean,
    val provenance: SemanticProvenance
)
