package scalafim.multivar

import gale.linalg.CholeskyOptions
import gale.linalg.DMat
import gale.linalg.DoubleLinearOperator

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
    val operator: Op[Primal[S], Dual[S], MetricOperatorRole, UncheckedEvidence],
    val space: SpaceEvidence[S],
    val blockGeometries: Vector[DiagramGeometry[?]],
    val psdCertificate: PsdConstructionCertificate,
    val certifiedMetric: Option[OpMetric[S, CertifiedSpd]],
    val cometric: Option[OpCometric[S, CertifiedSpd]]
):
  def isSpd: Boolean =
    certifiedMetric.nonEmpty && cometric.nonEmpty

final class DirectSumStudy private (
    val studyId: ValueId,
    val views: Vector[CompleteStudyView],
    val rowSpace: SpaceRef,
    val featureSpace: SpaceRef,
    val blocks: Vector[DirectSumBlock]
)(
    val table: OpTable[rowSpace.Id, featureSpace.Id, UncheckedEvidence],
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
        tableMap <- GaleOperators
          .blockDiagonal(views.map(_.diagram.core.table.kernel.linearMap))
          .left
          .map(error => DirectSumError.Semantic(SemanticError.LinearMapFailure(error)))
        tableIdentity = ValueIdentity.Derived("direct-sum-table", views.map(_.diagram.core.table.valueIdentity))
        table <- Op
          .fromLinearMap(
            tableMap,
            CoordinateEvidence.dual(featureSpace.evidence),
            CoordinateEvidence.primal(rowSpace.evidence),
            OperatorRoleWitness.table,
            tableIdentity,
            provenance
          )
          .left
          .map(DirectSumError.Semantic.apply)
        geometryMap <- GaleOperators
          .blockDiagonal(views.map(_.diagram.core.columnGeometry.operator.kernel.linearMap))
          .left
          .map(error => DirectSumError.Semantic(SemanticError.LinearMapFailure(error)))
        geometryIdentity = ValueIdentity.Derived(
          "direct-sum-column-geometry",
          views.map(_.diagram.core.columnGeometry.operator.valueIdentity)
        )
        geometryOperator <- Op
          .fromLinearMap(
            geometryMap,
            CoordinateEvidence.primal(featureSpace.evidence),
            CoordinateEvidence.dual(featureSpace.evidence),
            OperatorRoleWitness.metric,
            geometryIdentity,
            provenance
          )
          .left
          .map(DirectSumError.Semantic.apply)
        spdPair <- certifiedGeometry(
          featureSpace.evidence,
          views.map(_.diagram.core.columnGeometry),
          geometryOperator,
          provenance
        )
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
          spdPair.map(_._1),
          spdPair.map(_._2)
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

  private def certifiedGeometry[S <: SemanticSpace](
      space: SpaceEvidence[S],
      geometries: Vector[DiagramGeometry[?]],
      metric: Op[Primal[S], Dual[S], MetricOperatorRole, UncheckedEvidence],
      provenance: SemanticProvenance
  ): Either[DirectSumError, Option[(OpMetric[S, CertifiedSpd], OpCometric[S, CertifiedSpd])]] =
    if !geometries.forall(_.isSpd) then Right(None)
    else
      for
        blockCertificates <- geometries.foldLeft[Either[DirectSumError, Vector[NumericalCertificate]]](
          Right(Vector.empty)
        ): (result, geometry) =>
          result.flatMap: current =>
            geometry.certificates.find(_.claim.isInstanceOf[CertificateClaim.PositiveDefinite]) match
              case Some(value) => Right(current :+ value)
              case None => Left(DirectSumError.InvalidStudy("SPD block geometry is missing its bound certificate"))
        metricCertificate <- DirectSumOperatorCertificates.spd(
          metric.valueIdentity,
          blockCertificates,
          "block-diagonal-spd"
        )
        certifiedMetric <- Op
          .certifiedSpd(metric, metricCertificate)
          .left
          .map(DirectSumError.Semantic.apply)
        inverseBlocks <- traverseGeometries(geometries)(inverseBlock)
        inverseMap <- GaleOperators
          .blockDiagonal(inverseBlocks.map(_.operator))
          .left
          .map(error => DirectSumError.Semantic(SemanticError.LinearMapFailure(error)))
        inverseIdentity = ValueIdentity.derived("direct-sum-column-cometric", inverseBlocks.map(_.identity)*)
        uncheckedCometric <- Op
          .fromLinearMap(
            inverseMap,
            CoordinateEvidence.dual(space),
            CoordinateEvidence.primal(space),
            OperatorRoleWitness.cometric,
            inverseIdentity,
            provenance.append(
              SemanticProvenanceEvent.Derived("block-diagonal-cometric", inverseBlocks.map(_.identity))
            )
          )
          .left
          .map(DirectSumError.Semantic.apply)
        cometricCertificate <- DirectSumOperatorCertificates.spd(
          inverseIdentity,
          inverseBlocks.map(_.certificate),
          "block-diagonal-cometric-spd"
        )
        cometric <- Op
          .certifiedSpd(uncheckedCometric, cometricCertificate)
          .left
          .map(DirectSumError.Semantic.apply)
      yield Some(certifiedMetric -> cometric)

  private final case class InverseBlock(
      operator: DoubleLinearOperator,
      identity: ValueIdentity,
      certificate: NumericalCertificate
  )

  private def inverseBlock(geometry: DiagramGeometry[?]): Either[DirectSumError, InverseBlock] =
    inverseBlockTyped(geometry)

  private def inverseBlockTyped[S <: SemanticSpace](
      geometry: DiagramGeometry[S]
  ): Either[DirectSumError, InverseBlock] =
    val identity = ValueIdentity.derived("inverse", geometry.operator.valueIdentity)
    for
      dense <- geometry
        .operator(DMat.eye(geometry.space.dimension))
        .left
        .map(DirectSumError.Semantic.apply)
      factor <- dense
        .cholesky(CholeskyOptions())
        .left
        .map(error => DirectSumError.Multivar(LinalgErrorAdapter.toMultivarError(error)))
      inverse <- factor
        .solve(DMat.eye(dense.rows))
        .left
        .map(error => DirectSumError.Multivar(LinalgErrorAdapter.toMultivarError(error)))
      linear <- Lin
        .fromDenseMatrix(
          MatrixOps.symmetrize(inverse),
          CoordinateEvidence.dual(geometry.space),
          CoordinateEvidence.primal(geometry.space),
          identity,
          geometry.operator.provenance.append(
            SemanticProvenanceEvent.Derived("block-cometric", Vector(geometry.operator.valueIdentity))
          )
        )
        .left
        .map(DirectSumError.Semantic.apply)
      certificate <- FormCertificates
        .spd(linear)
        .left
        .map(DirectSumError.Semantic.apply)
    yield InverseBlock(linear.kernel.linearMap, identity, certificate.runtime)

  private def traverseGeometries[A](
      values: Vector[DiagramGeometry[?]]
  )(
      function: DiagramGeometry[?] => Either[DirectSumError, A]
  ): Either[DirectSumError, Vector[A]] =
    values.foldLeft[Either[DirectSumError, Vector[A]]](Right(Vector.empty)): (result, value) =>
      result.flatMap(current => function(value).map(current :+ _))

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
    operator: DoubleLinearOperator,
    valueIdentity: ValueIdentity
)

private[multivar] object DirectSumOperators:
  def rowOperator(
      study: DirectSumStudy,
      blocks: Vector[DirectSumRowBlock],
      operation: String,
      provenance: SemanticProvenance
  ): Either[DirectSumError, OpRowLink[study.rowSpace.Id, study.rowSpace.Id, UncheckedEvidence]] =
    val sizes = study.blocks.map(_.rowSpace.size)
    for
      blockMap <- GaleOperators
        .blockMatrix(
          sizes,
          sizes,
          blocks.map(block => LinearOperatorBlock(block.rowBlock, block.columnBlock, block.operator))
        )
        .left
        .map(error => DirectSumError.Semantic(SemanticError.LinearMapFailure(error)))
      identity = ValueIdentity.Derived(operation, blocks.map(_.valueIdentity))
      operator <- Op
        .fromLinearMap(
          blockMap,
          CoordinateEvidence.primal(study.rowSpace.evidence),
          CoordinateEvidence.dual(study.rowSpace.evidence),
          OperatorRoleWitness.rowLink,
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
    for
      operator <- rowOperator(study, blocks, "independent-row-geometry", study.provenance)
      certificate <- DirectSumOperatorCertificates.psd(
        operator.valueIdentity,
        study.views.flatMap(_.diagram.core.rowGeometry.certificates),
        "block-diagonal-row-geometry"
      )
      certified <- Op.certifiedPsd(operator, certificate).left.map(DirectSumError.Semantic.apply)
    yield
      new RowGeometry(
        certified,
        PsdConstructionCertificate(
          operator.valueIdentity,
          blocks.map(_.valueIdentity),
          "block-diagonal",
          "independent certified PSD row geometries form a PSD direct-sum row geometry"
        )
      )

final class RowGeometry[S <: SemanticSpace] private[multivar] (
    val operator: OpRowLink[S, S, CertifiedPsd],
    val psdCertificate: PsdConstructionCertificate
)

final class ConstraintPenalty[S <: SemanticSpace] private[multivar] (
    val operator: Op[Primal[S], Dual[S], PenaltyOperatorRole, CertifiedPsd],
    val psdCertificate: PsdConstructionCertificate
)

final class SymmetricObjectiveForm[S <: SemanticSpace] private[multivar] (
    val operator: OpRowLink[S, S, CertifiedSymmetric],
    private[multivar] val blocks: Vector[DirectSumRowBlock],
    val adjointCertificates: Vector[AdjointConsistencyCertificate],
    val potentiallyIndefinite: Boolean,
    val provenance: SemanticProvenance
)

private[multivar] object DirectSumOperatorCertificates:
  def spd(
      identity: ValueIdentity,
      inputs: Vector[NumericalCertificate],
      method: String
  ): Either[DirectSumError, Certificate[SpdProperty]] =
    val claims = inputs.collect:
      case NumericalCertificate(_, CertificateClaim.PositiveDefinite(minimum, _, _), _) => minimum
    if inputs.isEmpty || claims.length != inputs.length then
      Left(DirectSumError.InvalidStudy(s"$method requires certified SPD block inputs"))
    else
      context(method).map: current =>
        Certificate.unsafe[SpdProperty](
          identity,
          CertificateClaim.PositiveDefinite(
            claims.min,
            0.0,
            1.0
          ),
          current
        )

  def psd(
      identity: ValueIdentity,
      inputs: Vector[NumericalCertificate],
      method: String
  ): Either[DirectSumError, Certificate[PsdProperty]] =
    if inputs.isEmpty || !inputs.forall(certificate =>
        certificate.claim.isInstanceOf[CertificateClaim.PositiveSemidefinite] ||
          certificate.claim.isInstanceOf[CertificateClaim.PositiveDefinite]
      )
    then Left(DirectSumError.InvalidStudy(s"$method requires certified PSD block inputs"))
    else
      context(method).map: current =>
        Certificate.unsafe[PsdProperty](
          identity,
          CertificateClaim.PositiveSemidefinite(0.0, 0.0, 1.0),
          current
        )

  def symmetric(
      identity: ValueIdentity,
      method: String
  ): Either[DirectSumError, Certificate[SymmetryProperty]] =
    context(method).map: current =>
      Certificate.unsafe[SymmetryProperty](identity, CertificateClaim.Symmetric(0.0, 1.0), current)

  private def context(method: String): Either[DirectSumError, CertificateContext] =
    CertificateContext
      .from(
        CertificateTolerance.strict,
        CertificateNorm.Frobenius,
        method,
        "operator-algebra",
        NumericalPrecision.Float64
      )
      .left
      .map(DirectSumError.Semantic.apply)

sealed trait DirectSumSecondOrderBlock:
  type SourceFeature <: SemanticSpace
  type TargetFeature <: SemanticSpace
  def sourceId: BlockId
  def targetId: BlockId
  def rowBlock: Int
  def columnBlock: Int
  def operator: Op[Dual[TargetFeature], Primal[SourceFeature], CrossOperatorRole, UncheckedEvidence]

  private[multivar] def linearOperator: DoubleLinearOperator =
    operator.kernel.linearMap

object DirectSumSecondOrderBlock:
  private[multivar] def apply[SF <: SemanticSpace, TF <: SemanticSpace](
      source: BlockId,
      target: BlockId,
      outputBlock: Int,
      inputBlock: Int,
      value: Op[Dual[TF], Primal[SF], CrossOperatorRole, UncheckedEvidence]
  ): DirectSumSecondOrderBlock { type SourceFeature = SF; type TargetFeature = TF } =
    new DirectSumSecondOrderBlock:
      type SourceFeature = SF
      type TargetFeature = TF
      override val sourceId: BlockId = source
      override val targetId: BlockId = target
      override val rowBlock: Int = outputBlock
      override val columnBlock: Int = inputBlock
      override val operator: Op[Dual[TF], Primal[SF], CrossOperatorRole, UncheckedEvidence] = value

final case class SecondOrderAssemblyCertificate(
    blockOperators: Vector[ValueIdentity],
    directOperator: ValueIdentity,
    proof: String
)

/** Feature-space association assembled from pairwise `secondOrder` blocks.
  * `direct` is the independent direct-sum expression `X* L X`; retaining both
  * representations makes their equivalence testable without densifying either
  * block assembly.
  */
final class DirectSumAssociationOperator[S <: SemanticSpace] private[multivar] (
    val blocks: Vector[DirectSumSecondOrderBlock],
    val operator: Op[Dual[S], Primal[S], CrossOperatorRole, CertifiedSymmetric],
    val direct: Op[Dual[S], Primal[S], CrossOperatorRole, UncheckedEvidence],
    val certificate: SecondOrderAssemblyCertificate
)

object DirectSumFeatureOperators:
  def association(
      study: DirectSumStudy,
      objective: SymmetricObjectiveForm[study.rowSpace.Id]
  ): Either[DirectSumError, DirectSumAssociationOperator[study.featureSpace.Id]] =
    for
      featureBlocks <- objective.blocks.foldLeft[Either[DirectSumError, Vector[DirectSumSecondOrderBlock]]](
        Right(Vector.empty)
      ): (result, block) =>
        result.flatMap(current => secondOrderBlock(study, block).map(current :+ _))
      blockMap <- GaleOperators
        .blockMatrix(
          study.blocks.map(_.featureSpace.size),
          study.blocks.map(_.featureSpace.size),
          featureBlocks.map(block => LinearOperatorBlock(block.rowBlock, block.columnBlock, block.linearOperator))
        )
        .left
        .map(error => DirectSumError.Semantic(SemanticError.LinearMapFailure(error)))
      identity = ValueIdentity.derived("direct-sum-second-order-blocks", featureBlocks.map(_.operator.valueIdentity)*)
      unchecked <- Op
        .fromLinearMap(
          blockMap,
          CoordinateEvidence.dual(study.featureSpace.evidence),
          CoordinateEvidence.primal(study.featureSpace.evidence),
          OperatorRoleWitness.cross,
          identity,
          objective.provenance.append(
            SemanticProvenanceEvent.Derived(
              "assemble-second-order-blocks",
              featureBlocks.map(_.operator.valueIdentity)
            )
          )
        )
        .left
        .map(DirectSumError.Semantic.apply)
      symmetry <- DirectSumOperatorCertificates.symmetric(identity, "second-order-adjoint-block-pairs")
      certified <- Op.certifiedSymmetric(unchecked, symmetry).left.map(DirectSumError.Semantic.apply)
      direct = OperatorAlgebra.secondOrder(study.table, objective.operator, study.table)
    yield
      new DirectSumAssociationOperator(
        featureBlocks,
        certified,
        direct,
        SecondOrderAssemblyCertificate(
          featureBlocks.map(_.operator.valueIdentity),
          direct.valueIdentity,
          "each feature block is secondOrder(X_s, L_st, X_t); block assembly equals X_oplus* L X_oplus"
        )
      )

  private def secondOrderBlock(
      study: DirectSumStudy,
      block: DirectSumRowBlock
  ): Either[DirectSumError, DirectSumSecondOrderBlock] =
    if block.rowBlock < 0 || block.rowBlock >= study.views.length ||
        block.columnBlock < 0 || block.columnBlock >= study.views.length
    then Left(DirectSumError.InvalidStudy("row-relation block lies outside the direct-sum view grid"))
    else
      val source = study.views(block.rowBlock)
      val target = study.views(block.columnBlock)
      secondOrderBlockTyped(source, target, block)

  private def secondOrderBlockTyped(
      source: CompleteStudyView,
      target: CompleteStudyView,
      block: DirectSumRowBlock
  ): Either[DirectSumError, DirectSumSecondOrderBlock] =
    val sourceTable = Op.fromLin(source.diagram.core.table, OperatorRoleWitness.table)
    val targetTable = Op.fromLin(target.diagram.core.table, OperatorRoleWitness.table)
    for
      relationship <- Op
        .fromLinearMap(
          block.operator,
          CoordinateEvidence.primal(target.diagram.core.rowGeometry.space),
          CoordinateEvidence.dual(source.diagram.core.rowGeometry.space),
          OperatorRoleWitness.rowLink,
          block.valueIdentity,
          (source.diagram.provenance ++ target.diagram.provenance).append(
            SemanticProvenanceEvent.Derived("direct-sum-row-block", Vector(block.valueIdentity))
          )
        )
        .left
        .map(DirectSumError.Semantic.apply)
      secondOrder = OperatorAlgebra.secondOrder(sourceTable, relationship, targetTable)
    yield
      DirectSumSecondOrderBlock(
        source.id,
        target.id,
        block.rowBlock,
        block.columnBlock,
        secondOrder
      )
