package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.LinearMap
import scalafim.linalg.LinearMapBlock

final case class BlockDesignEdge(left: BlockId, right: BlockId, coefficient: Double)

final class BlockDesign private (
    val views: Vector[BlockId],
    val edges: Vector[BlockDesignEdge]
):
  def coefficient(left: BlockId, right: BlockId): Double =
    edges
      .find(edge => (edge.left == left && edge.right == right) || (edge.left == right && edge.right == left))
      .fold(0.0)(_.coefficient)

object BlockDesign:
  def from(
      views: Vector[BlockId],
      edges: Vector[BlockDesignEdge]
  ): Either[DirectSumError, BlockDesign] =
    if views.isEmpty then Left(DirectSumError.InvalidStudy("block design requires at least one view"))
    else if views.distinct.length != views.length then
      Left(DirectSumError.InvalidStudy("block design view ids must be unique"))
    else
      val known = views.toSet
      val canonical = scala.collection.mutable.HashSet.empty[(BlockId, BlockId)]
      var index = 0
      var error = Option.empty[DirectSumError]
      while index < edges.length && error.isEmpty do
        val edge = edges(index)
        val key =
          if edge.left.value <= edge.right.value then (edge.left, edge.right)
          else (edge.right, edge.left)
        if !known.contains(edge.left) then error = Some(DirectSumError.UnknownView(edge.left))
        else if !known.contains(edge.right) then error = Some(DirectSumError.UnknownView(edge.right))
        else if edge.left == edge.right then
          error = Some(DirectSumError.InvalidStudy("association design edges must connect distinct views"))
        else if !edge.coefficient.isFinite then
          error = Some(DirectSumError.InvalidStudy("block design coefficients must be finite"))
        else if canonical.contains(key) then
          error = Some(
            DirectSumError.InvalidStudy(
              s"block design repeats edge '${edge.left.value}'--'${edge.right.value}'"
            )
          )
        else canonical += key
        index += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(new BlockDesign(views, edges))

final case class MarginalConsistencyCertificate(
    relationships: Vector[ValueIdentity],
    residual: Double,
    context: CertificateContext,
    proof: String
)

final case class CycleConsistencyCertificate(
    cycle: Vector[BlockId],
    residual: Double,
    context: CertificateContext,
    proof: String
)

final case class PairwiseCoherenceCertificates(
    adjoint: Vector[AdjointConsistencyCertificate],
    marginal: Option[MarginalConsistencyCertificate],
    cycle: Option[CycleConsistencyCertificate],
    hub: Option[HubFactorizationCertificate],
    globalBlockPsd: Option[GlobalBlockPsdCertificate]
)

sealed trait UndirectedRowRelationship:
  type Left <: SemanticSpace
  type Right <: SemanticSpace
  def leftId: BlockId
  def rightId: BlockId
  def pair: HubLinkedPair[Left, Right]

object UndirectedRowRelationship:
  def apply[L <: SemanticSpace, R <: SemanticSpace](
      left: BlockId,
      right: BlockId,
      linked: HubLinkedPair[L, R]
  ): UndirectedRowRelationship { type Left = L; type Right = R } =
    new UndirectedRowRelationship:
      type Left = L
      type Right = R
      override val leftId: BlockId = left
      override val rightId: BlockId = right
      override val pair: HubLinkedPair[L, R] = linked

final class PairwiseLinkedStudy private (
    val study: DirectSumStudy,
    val relationships: Vector[UndirectedRowRelationship],
    val certificates: PairwiseCoherenceCertificates
)

object PairwiseLinkedStudy:
  def from(
      study: DirectSumStudy,
      relationships: Vector[UndirectedRowRelationship],
      marginal: Option[MarginalConsistencyCertificate] = None,
      cycle: Option[CycleConsistencyCertificate] = None
  ): Either[DirectSumError, PairwiseLinkedStudy] =
    val certificates = relationships.map(_.pair.adjointCertificate)
    validateRelationships(study, relationships).map { _ =>
      new PairwiseLinkedStudy(
        study,
        relationships,
        PairwiseCoherenceCertificates(certificates, marginal, cycle, None, None)
      )
    }

  private def validateRelationships(
      study: DirectSumStudy,
      relationships: Vector[UndirectedRowRelationship]
  ): Either[DirectSumError, Unit] =
    var index = 0
    var error = Option.empty[DirectSumError]
    while index < relationships.length && error.isEmpty do
      val relationship = relationships(index)
      (study.block(relationship.leftId), study.block(relationship.rightId)) match
        case (Left(value), _) => error = Some(value)
        case (_, Left(value)) => error = Some(value)
        case (Right(left), Right(right)) =>
          val forward = relationship.pair.leftToRight.descriptor
          val reverse = relationship.pair.rightToLeft.descriptor
          if forward.codomain != left.rowSpace || forward.domain != right.rowSpace then
            error = Some(DirectSumError.IncompatibleRelationship("forward row link endpoints do not match study views"))
          else if reverse.codomain != right.rowSpace || reverse.domain != left.rowSpace then
            error = Some(DirectSumError.IncompatibleRelationship("reverse row link endpoints do not match study views"))
      index += 1
    error.toLeft(())

sealed trait SameRowViewEvidence[E <: SemanticSpace]:
  type Rows <: SemanticSpace
  def viewId: BlockId
  def evidence: SameEntityEvidence[Rows, E]

object SameRowViewEvidence:
  def apply[R <: SemanticSpace, E <: SemanticSpace](
      id: BlockId,
      value: SameEntityEvidence[R, E]
  ): SameRowViewEvidence[E] { type Rows = R } =
    new SameRowViewEvidence[E]:
      type Rows = R
      override val viewId: BlockId = id
      override val evidence: SameEntityEvidence[R, E] = value

final class SameRowStudy[E <: SemanticSpace] private (
    val study: DirectSumStudy,
    val alignment: EntityAlignedStudy[E]
)

object SameRowStudy:
  def from[E <: SemanticSpace](
      study: DirectSumStudy,
      entitySpace: SpaceEvidence[E],
      entityForm: DiagramGeometry[E],
      evidence: Vector[SameRowViewEvidence[E]]
  ): Either[DirectSumError, SameRowStudy[E]] =
    if evidence.map(_.viewId).toSet != study.blocks.map(_.id).toSet then
      Left(DirectSumError.InvalidStudy("same-row evidence and direct-sum study must name the same views"))
    else
      val maps = evidence.map { item =>
        EntityMapEntry(item.viewId, item.evidence.exactIdentity.rowMap)
      }
      EntityAlignedStudy
        .from(entitySpace, entityForm, maps, SemanticProvenance.source("same-row-study"))
        .left
        .map(DirectSumError.Alignment.apply)
        .map(new SameRowStudy(study, _))

object DirectSumRowForms:
  def independent(study: DirectSumStudy): Either[DirectSumError, RowGeometry[study.rowSpace.Id]] =
    DirectSumOperators.independentRowGeometry(study)

  def sameRowGeometry[E <: SemanticSpace](
      sameRows: SameRowStudy[E]
  ): Either[DirectSumError, RowGeometry[sameRows.study.rowSpace.Id]] =
    hubGeometry(sameRows.study, sameRows.alignment)

  def sameRowAssociation[E <: SemanticSpace](
      sameRows: SameRowStudy[E],
      design: BlockDesign
  ): Either[DirectSumError, SymmetricObjectiveForm[sameRows.study.rowSpace.Id]] =
    hubAssociation(sameRows.study, sameRows.alignment, design)

  def hubGeometry[E <: SemanticSpace](
      study: DirectSumStudy,
      alignment: EntityAlignedStudy[E]
  ): Either[DirectSumError, RowGeometry[study.rowSpace.Id]] =
    validateHubStudy(study, alignment).flatMap { _ =>
      val blocks = Vector.newBuilder[DirectSumRowBlock]
      var leftIndex = 0
      var error = Option.empty[DirectSumError]
      while leftIndex < alignment.views.length && error.isEmpty do
        var rightIndex = 0
        while rightIndex < alignment.views.length && error.isEmpty do
          val left = alignment.views(leftIndex)
          val right = alignment.views(rightIndex)
          HubAlignment.inducedLink(left.map, alignment.entityForm, right.map) match
            case Left(value) => error = Some(DirectSumError.Alignment(value))
            case Right(link) =>
              val studyLeft = study.viewIndex(left.viewId)
              val studyRight = study.viewIndex(right.viewId)
              (studyLeft, studyRight) match
                case (Right(rowBlock), Right(columnBlock)) =>
                  blocks += DirectSumRowBlock(
                    rowBlock,
                    columnBlock,
                    link.operator.kernel.linearMap,
                    link.operator.valueIdentity
                  )
                case (Left(value), _) => error = Some(value)
                case (_, Left(value)) => error = Some(value)
          rightIndex += 1
        leftIndex += 1
      error match
        case Some(value) => Left(value)
        case None =>
          val compiled = blocks.result()
          DirectSumOperators
            .rowOperator(study, compiled, "hub-row-geometry", alignment.provenance)
            .map { operator =>
              new RowGeometry(
                operator,
                PsdConstructionCertificate(
                  operator.valueIdentity,
                  compiled.map(_.valueIdentity),
                  "hub-factorized-block-form",
                  alignment.globalBlockPsdCertificate.proof
                )
              )
            }
    }

  def hubAssociation[E <: SemanticSpace](
      study: DirectSumStudy,
      alignment: EntityAlignedStudy[E],
      design: BlockDesign
  ): Either[DirectSumError, SymmetricObjectiveForm[study.rowSpace.Id]] =
    validateHubStudy(study, alignment).flatMap { _ =>
      if design.views.toSet != study.blocks.map(_.id).toSet then
        Left(DirectSumError.InvalidStudy("block design and direct-sum study must name the same views"))
      else
        val blocks = Vector.newBuilder[DirectSumRowBlock]
        val certificates = Vector.newBuilder[AdjointConsistencyCertificate]
        var index = 0
        var error = Option.empty[DirectSumError]
        while index < design.edges.length && error.isEmpty do
          val edge = design.edges(index)
          (alignment.views.find(_.viewId == edge.left), alignment.views.find(_.viewId == edge.right)) match
            case (Some(left), Some(right)) =>
              HubAlignment.inducedPair(left.map, alignment.entityForm, right.map) match
                case Left(value) => error = Some(DirectSumError.Alignment(value))
                case Right(pair) =>
                  (study.viewIndex(edge.left), study.viewIndex(edge.right)) match
                    case (Right(leftBlock), Right(rightBlock)) =>
                      val forward = LinearMap.scale(pair.leftToRight.operator.kernel.linearMap, edge.coefficient)
                      val reverse = LinearMap.scale(pair.rightToLeft.operator.kernel.linearMap, edge.coefficient)
                      (forward, reverse) match
                        case (Right(forwardMap), Right(reverseMap)) =>
                          blocks += DirectSumRowBlock(
                            leftBlock,
                            rightBlock,
                            forwardMap,
                            ValueIdentity.derived(
                              s"design-scaled-link.${edge.coefficient}",
                              pair.leftToRight.operator.valueIdentity
                            )
                          )
                          blocks += DirectSumRowBlock(
                            rightBlock,
                            leftBlock,
                            reverseMap,
                            ValueIdentity.derived(
                              s"design-scaled-link.${edge.coefficient}",
                              pair.rightToLeft.operator.valueIdentity
                            )
                          )
                          certificates += pair.adjointCertificate
                        case (Left(value), _) =>
                          error = Some(DirectSumError.Semantic(SemanticError.LinearMapFailure(value)))
                        case (_, Left(value)) =>
                          error = Some(DirectSumError.Semantic(SemanticError.LinearMapFailure(value)))
                    case (Left(value), _) => error = Some(value)
                    case (_, Left(value)) => error = Some(value)
            case _ => error = Some(DirectSumError.InvalidStudy("hub alignment is missing a designed view"))
          index += 1
        error match
          case Some(value) => Left(value)
          case None =>
            DirectSumOperators
              .rowOperator(study, blocks.result(), "hub-association-objective", alignment.provenance)
              .map { operator =>
                new SymmetricObjectiveForm(operator, certificates.result(), potentiallyIndefinite = true, alignment.provenance)
              }
    }

  def pairwiseAssociation(
      linked: PairwiseLinkedStudy,
      design: BlockDesign
  ): Either[DirectSumError, SymmetricObjectiveForm[linked.study.rowSpace.Id]] =
    if design.views.toSet != linked.study.blocks.map(_.id).toSet then
      Left(DirectSumError.InvalidStudy("block design and pairwise-linked study must name the same views"))
    else
      val blocks = Vector.newBuilder[DirectSumRowBlock]
      var index = 0
      var error = Option.empty[DirectSumError]
      while index < linked.relationships.length && error.isEmpty do
        val relationship = linked.relationships(index)
        val coefficient = design.coefficient(relationship.leftId, relationship.rightId)
        if coefficient != 0.0 then
          val forward = LinearMap.scale(relationship.pair.leftToRight.operator.kernel.linearMap, coefficient)
          val reverse = LinearMap.scale(relationship.pair.rightToLeft.operator.kernel.linearMap, coefficient)
          (linked.study.viewIndex(relationship.leftId), linked.study.viewIndex(relationship.rightId), forward, reverse) match
            case (Right(leftBlock), Right(rightBlock), Right(forwardMap), Right(reverseMap)) =>
              blocks += DirectSumRowBlock(
                leftBlock,
                rightBlock,
                forwardMap,
                ValueIdentity.derived(
                  s"design-scaled-link.$coefficient",
                  relationship.pair.leftToRight.operator.valueIdentity
                )
              )
              blocks += DirectSumRowBlock(
                rightBlock,
                leftBlock,
                reverseMap,
                ValueIdentity.derived(
                  s"design-scaled-link.$coefficient",
                  relationship.pair.rightToLeft.operator.valueIdentity
                )
              )
            case (Left(value), _, _, _) => error = Some(value)
            case (_, Left(value), _, _) => error = Some(value)
            case (_, _, Left(value), _) =>
              error = Some(DirectSumError.Semantic(SemanticError.LinearMapFailure(value)))
            case (_, _, _, Left(value)) =>
              error = Some(DirectSumError.Semantic(SemanticError.LinearMapFailure(value)))
        index += 1
      error match
        case Some(value) => Left(value)
        case None =>
          DirectSumOperators
            .rowOperator(linked.study, blocks.result(), "pairwise-association-objective", linked.study.provenance)
            .map { operator =>
              new SymmetricObjectiveForm(
                operator,
                linked.certificates.adjoint,
                potentiallyIndefinite = true,
                linked.study.provenance
              )
            }

  private def validateHubStudy[E <: SemanticSpace](
      study: DirectSumStudy,
      alignment: EntityAlignedStudy[E]
  ): Either[DirectSumError, Unit] =
    if alignment.views.map(_.viewId).toSet != study.blocks.map(_.id).toSet then
      Left(DirectSumError.InvalidStudy("hub alignment and direct-sum study must name the same views"))
    else
      var index = 0
      var error = Option.empty[DirectSumError]
      while index < alignment.views.length && error.isEmpty do
        val entry = alignment.views(index)
        study.block(entry.viewId) match
          case Left(value) => error = Some(value)
          case Right(block) =>
            if entry.map.descriptor.domain != block.rowSpace then
              error = Some(
                DirectSumError.IncompatibleRelationship(
                  s"hub map for '${entry.viewId.value}' belongs to a different row space"
                )
              )
        index += 1
      error.toLeft(())

enum ConstraintSemantics:
  case Hard
  case QuadraticPenalty(weight: ValueIdentity)
  case Bounded(weight: ValueIdentity, epsilon: Double)

final class LinearConstraint[S <: SemanticSpace, H <: SemanticSpace] private[multivar] (
    val operator: Lin[Primal[S], Primal[H]],
    val source: SpaceEvidence[S],
    val target: SpaceEvidence[H],
    val formula: String,
    val provenance: SemanticProvenance
):
  def residual(scores: DoubleMatrix): Either[SemanticError, Double] =
    operator(scores).map { values =>
      val data = values.copyData
      var sum = 0.0
      var index = 0
      while index < data.length do
        sum += data(index) * data(index)
        index += 1
      Math.sqrt(sum)
    }

  def hard: HardScoreConstraint[S, H] =
    HardScoreConstraint(this, ConstraintSemantics.Hard)

  def bounded(
      weight: DiagramGeometry[H],
      epsilon: Double
  ): Either[DirectSumError, BoundedScoreConstraint[S, H]] =
    if !epsilon.isFinite || epsilon < 0.0 then
      Left(DirectSumError.InvalidStudy("bounded constraint epsilon must be finite and non-negative"))
    else if weight.space.descriptor != target.descriptor then
      Left(DirectSumError.IncompatibleRelationship("bounded-constraint weight belongs to a different space"))
    else Right(BoundedScoreConstraint(this, weight, epsilon, ConstraintSemantics.Bounded(weight.operator.valueIdentity, epsilon)))

  def quadraticPenalty(
      weight: DiagramGeometry[H]
  ): Either[DirectSumError, ConstraintPenalty[S]] =
    if weight.space.descriptor != target.descriptor then
      Left(DirectSumError.IncompatibleRelationship("constraint weight belongs to a different space"))
    else
      val penalty = operator.andThen(weight.operator).andThen(operator.star)
      Right(
        new ConstraintPenalty(
          penalty,
          PsdConstructionCertificate(
            penalty.valueIdentity,
            Vector(operator.valueIdentity, weight.operator.valueIdentity),
            "B-star-W-B",
            "B* W B is PSD because W is certified PSD"
          )
        )
      )

final case class HardScoreConstraint[S <: SemanticSpace, H <: SemanticSpace](
    constraint: LinearConstraint[S, H],
    semantics: ConstraintSemantics.Hard.type
)

final case class BoundedScoreConstraint[S <: SemanticSpace, H <: SemanticSpace](
    constraint: LinearConstraint[S, H],
    weight: DiagramGeometry[H],
    epsilon: Double,
    semantics: ConstraintSemantics.Bounded
)

object LinearConstraint:
  def pairwiseHubAgreement[E <: SemanticSpace](
      study: DirectSumStudy,
      left: EntityMapEntry[E],
      right: EntityMapEntry[E]
  ): Either[DirectSumError, LinearConstraint[study.rowSpace.Id, E]] =
    for
      leftIndex <- study.viewIndex(left.viewId)
      rightIndex <- study.viewIndex(right.viewId)
      _ <-
        if left.map.descriptor.codomain == right.map.descriptor.codomain then Right(())
        else Left(DirectSumError.IncompatibleRelationship("agreement maps must target one entity space"))
      negativeRight <- LinearMap
        .scale(right.map.operator.kernel.linearMap, -1.0)
        .left
        .map(error => DirectSumError.Semantic(SemanticError.LinearMapFailure(error)))
      blockMap <- LinearMap
        .blockMatrix(
          Vector(left.map.descriptor.codomain.size),
          study.blocks.map(_.rowSpace.size),
          Vector(
            LinearMapBlock(0, leftIndex, left.map.operator.kernel.linearMap),
            LinearMapBlock(0, rightIndex, negativeRight)
          )
        )
        .left
        .map(error => DirectSumError.Semantic(SemanticError.LinearMapFailure(error)))
      identity = ValueIdentity.derived(
        "pairwise-hub-agreement",
        left.map.operator.valueIdentity,
        right.map.operator.valueIdentity
      )
      operator <- Lin
        .fromLinearMap[Primal[study.rowSpace.Id], Primal[E]](
          blockMap,
          CoordinateEvidence.primal(study.rowSpace.evidence),
          left.map.operator.codomain,
          identity,
          (left.map.provenance ++ right.map.provenance).append(
            SemanticProvenanceEvent.Derived(
              "pairwise-hub-agreement",
              Vector(left.map.operator.valueIdentity, right.map.operator.valueIdentity)
            )
          )
        )
        .left
        .map(DirectSumError.Semantic.apply)
    yield
      new LinearConstraint(
        operator,
        study.rowSpace.evidence,
        SpaceEvidence.unsafe(left.map.descriptor.codomain),
        s"P_${left.viewId.value} t_${left.viewId.value} - P_${right.viewId.value} t_${right.viewId.value} = 0",
        operator.provenance
      )

enum ObjectiveFormula:
  case PairwiseAssociation
  case QuadraticDisagreement
  case HardScoreAgreement

enum ObjectiveNormalization:
  case DirectSumMetricOrthonormal
  case PerViewMetricOrthonormal
  case ConstraintOnly

enum SolverFormulation:
  case SymmetricFeatureEigen
  case QuadraticPenalty
  case LinearEquality

enum SolvedToReportedRelationship:
  case Identical
  case Scale(factor: Double)
  case Transform(description: String)

final case class ObjectiveDefinition(
    formula: ObjectiveFormula,
    renderedFormula: String,
    constraints: Vector[String],
    normalization: ObjectiveNormalization,
    solverFormulation: SolverFormulation,
    solvedToReportedRelationship: SolvedToReportedRelationship
)

final case class MaximizeAssociation[S <: SemanticSpace](
    objective: SymmetricObjectiveForm[S],
    definition: ObjectiveDefinition
)

final case class PenalizeDisagreement[S <: SemanticSpace](
    penalty: ConstraintPenalty[S],
    definition: ObjectiveDefinition
)

final case class ConstrainEqualScores[S <: SemanticSpace, H <: SemanticSpace](
    constraint: HardScoreConstraint[S, H],
    definition: ObjectiveDefinition
)
