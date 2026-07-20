package scalafim.multivar

import scalafim.linalg.CsrMatrix
import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

type RowMap[From <: SemanticSpace, To <: SemanticSpace] = Lin[Primal[From], Primal[To]]
type RowLink[Left <: SemanticSpace, Right <: SemanticSpace] = Lin[Primal[Right], Dual[Left]]

enum AlignmentError:
  case Semantic(error: SemanticError)
  case Multivar(error: MultivarError)
  case InvalidRelationship(detail: String)
  case DuplicateAssignment(targetIndex: Int)
  case MarginalMismatch(detail: String)
  case PositionalIdentityRequired

  def message: String =
    this match
      case Semantic(error) => error.message
      case Multivar(error) => error.message
      case InvalidRelationship(detail) => detail
      case DuplicateAssignment(targetIndex) =>
        s"partial injection assigns target index $targetIndex more than once"
      case MarginalMismatch(detail) => detail
      case PositionalIdentityRequired =>
        "legacy same-row reduction requires exact positional identity evidence"

enum RowRelationshipKind:
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

enum CardinalityStructure:
  case OneToOne
  case OneToMany
  case ManyToOne
  case ManyToMany

enum RelationshipNormalization:
  case Unnormalized
  case UnitMass
  case SourceMassPreserving
  case TargetMassPreserving
  case DoublyStochastic
  case ConditionalOnSource
  case ConditionalOnTarget

enum AlignmentOrigin:
  case ObservedKeys
  case ExternallySupplied
  case UnsafeAssumption

final case class RelationshipSupport(
    matchedMass: Double,
    unmatchedSourceMass: Double,
    unmatchedTargetMass: Double,
    uncertainMass: Double,
    excludedMass: Double,
    structuralZeroCount: Int,
    cardinality: CardinalityStructure,
    duplicateKeys: Vector[String],
    everySourceRepresented: Boolean,
    everyTargetRepresented: Boolean
)

final case class CouplingMarginals(
    left: DoubleVector,
    right: DoubleVector,
    totalMass: Double
)

final case class RowRelationshipDescriptor(
    kind: RowRelationshipKind,
    domain: MvSpace,
    codomain: MvSpace,
    orientation: LinDescriptor,
    support: RelationshipSupport,
    normalization: RelationshipNormalization,
    marginals: Option[CouplingMarginals],
    origin: AlignmentOrigin,
    valueIdentity: ValueIdentity
)

final class TypedRowMap[From <: SemanticSpace, To <: SemanticSpace] private[multivar] (
    val operator: RowMap[From, To],
    val matrix: MatrixView,
    val descriptor: RowRelationshipDescriptor,
    val provenance: SemanticProvenance
)

final class TypedRowLink[Left <: SemanticSpace, Right <: SemanticSpace] private[multivar] (
    val operator: RowLink[Left, Right],
    val matrix: MatrixView,
    val descriptor: RowRelationshipDescriptor,
    val provenance: SemanticProvenance
)

final class ExactBijection[From <: SemanticSpace, To <: SemanticSpace] private (
    val rowMap: TypedRowMap[From, To],
    val sourceToTarget: Vector[Int]
):
  def toPartialInjection: PartialInjection[From, To] =
    PartialInjection.fromExact(this)

object ExactBijection:
  def fromPermutation[From <: SemanticSpace, To <: SemanticSpace](
      from: SpaceEvidence[From],
      to: SpaceEvidence[To],
      sourceToTarget: Vector[Int],
      valueIdentity: ValueIdentity,
      duplicateKeys: Vector[String] = Vector.empty,
      origin: AlignmentOrigin = AlignmentOrigin.ObservedKeys,
      provenance: SemanticProvenance = SemanticProvenance.source("exact-row-bijection")
  ): Either[AlignmentError, ExactBijection[From, To]] =
    if from.dimension != to.dimension then
      Left(
        AlignmentError.InvalidRelationship(
          s"exact bijection requires equal cardinality, got ${from.dimension} and ${to.dimension}"
        )
      )
    else if sourceToTarget.length != from.dimension then
      Left(
        AlignmentError.InvalidRelationship(
          s"exact bijection has ${sourceToTarget.length} assignments for ${from.dimension} source rows"
        )
      )
    else
      val seen = scala.collection.mutable.HashSet.empty[Int]
      var source = 0
      var error = Option.empty[AlignmentError]
      while source < sourceToTarget.length && error.isEmpty do
        val target = sourceToTarget(source)
        if target < 0 || target >= to.dimension then
          error = Some(
            AlignmentError.InvalidRelationship(
              s"exact bijection target index $target is outside 0..${to.dimension - 1}"
            )
          )
        else if seen.contains(target) then error = Some(AlignmentError.DuplicateAssignment(target))
        else seen += target
        source += 1
      error match
        case Some(value) => Left(value)
        case None =>
          val entries = sourceToTarget.zipWithIndex.map { case (target, sourceIndex) =>
            (target, sourceIndex, 1.0)
          }
          RelationshipMatrices
            .sparseRowMap(
              from,
              to,
              entries,
              RowRelationshipKind.ExactBijection,
              RelationshipSupport(
                from.dimension.toDouble,
                0.0,
                0.0,
                0.0,
                0.0,
                from.dimension * to.dimension - from.dimension,
                CardinalityStructure.OneToOne,
                duplicateKeys,
                everySourceRepresented = true,
                everyTargetRepresented = true
              ),
              RelationshipNormalization.SourceMassPreserving,
              origin,
              valueIdentity,
              provenance
            )
            .map(new ExactBijection(_, sourceToTarget))

final class PartialInjection[From <: SemanticSpace, To <: SemanticSpace] private (
    val rowMap: TypedRowMap[From, To],
    val assignments: Vector[Option[Int]]
):
  def toIncidenceMap: IncidenceMap[From, To] =
    IncidenceMap.fromPartial(this)

object PartialInjection:
  def fromAssignments[From <: SemanticSpace, To <: SemanticSpace](
      from: SpaceEvidence[From],
      to: SpaceEvidence[To],
      assignments: Vector[Option[Int]],
      valueIdentity: ValueIdentity,
      duplicateKeys: Vector[String] = Vector.empty,
      origin: AlignmentOrigin = AlignmentOrigin.ObservedKeys,
      provenance: SemanticProvenance = SemanticProvenance.source("partial-row-injection")
  ): Either[AlignmentError, PartialInjection[From, To]] =
    if assignments.length != from.dimension then
      Left(
        AlignmentError.InvalidRelationship(
          s"partial injection has ${assignments.length} assignments for ${from.dimension} source rows"
        )
      )
    else
      val seen = scala.collection.mutable.HashSet.empty[Int]
      val entries = Vector.newBuilder[(Int, Int, Double)]
      var source = 0
      var matched = 0
      var error = Option.empty[AlignmentError]
      while source < assignments.length && error.isEmpty do
        assignments(source) match
          case Some(target) if target < 0 || target >= to.dimension =>
            error = Some(
              AlignmentError.InvalidRelationship(
                s"partial injection target index $target is outside 0..${to.dimension - 1}"
              )
            )
          case Some(target) if seen.contains(target) =>
            error = Some(AlignmentError.DuplicateAssignment(target))
          case Some(target) =>
            seen += target
            entries += ((target, source, 1.0))
            matched += 1
          case None => ()
        source += 1
      error match
        case Some(value) => Left(value)
        case None =>
          RelationshipMatrices
            .sparseRowMap(
              from,
              to,
              entries.result(),
              RowRelationshipKind.PartialInjection,
              RelationshipSupport(
                matched.toDouble,
                (from.dimension - matched).toDouble,
                (to.dimension - matched).toDouble,
                0.0,
                0.0,
                from.dimension * to.dimension - matched,
                CardinalityStructure.OneToOne,
                duplicateKeys,
                everySourceRepresented = matched == from.dimension,
                everyTargetRepresented = matched == to.dimension
              ),
              RelationshipNormalization.SourceMassPreserving,
              origin,
              valueIdentity,
              provenance
            )
            .map(new PartialInjection(_, assignments))

  private[multivar] def fromExact[From <: SemanticSpace, To <: SemanticSpace](
      exact: ExactBijection[From, To]
  ): PartialInjection[From, To] =
    val descriptor = exact.rowMap.descriptor.copy(kind = RowRelationshipKind.PartialInjection)
    new PartialInjection(
      new TypedRowMap(
        exact.rowMap.operator,
        exact.rowMap.matrix,
        descriptor,
        exact.rowMap.provenance.append(
          SemanticProvenanceEvent.Derived("exact-to-partial-injection", Vector(exact.rowMap.operator.valueIdentity))
        )
      ),
      exact.sourceToTarget.map(Some(_))
    )

final class IncidenceMap[From <: SemanticSpace, To <: SemanticSpace] private (
    val rowMap: TypedRowMap[From, To],
    val edges: Vector[(Int, Int)]
):
  /** Explicitly lower the target endpoint through its form. The incidence map itself
    * remains a primal-to-primal map and is never silently treated as a row link.
    */
  def toRowLink(
      targetForm: DiagramGeometry[To]
  ): Either[AlignmentError, TypedRowLink[To, From]] =
    val link = rowMap.operator.andThen(targetForm.operator)
    RelationshipMatrices.matrixOf(link).map { matrix =>
      new TypedRowLink(
        link,
        DenseMatrixView(matrix),
        rowMap.descriptor.copy(
          kind = RowRelationshipKind.IncidenceInducedLink,
          domain = rowMap.descriptor.domain,
          codomain = targetForm.space.descriptor,
          orientation = link.descriptor,
          valueIdentity = link.valueIdentity
        ),
        rowMap.provenance.append(
          SemanticProvenanceEvent.Derived(
            "incidence-to-row-link",
            Vector(rowMap.operator.valueIdentity, targetForm.operator.valueIdentity)
          )
        )
      )
    }

object IncidenceMap:
  def fromEdges[From <: SemanticSpace, To <: SemanticSpace](
      from: SpaceEvidence[From],
      to: SpaceEvidence[To],
      edges: Vector[(Int, Int)],
      valueIdentity: ValueIdentity,
      duplicateKeys: Vector[String] = Vector.empty,
      provenance: SemanticProvenance = SemanticProvenance.source("row-incidence-map")
  ): Either[AlignmentError, IncidenceMap[From, To]] =
    if edges.distinct.length != edges.length then
      Left(AlignmentError.InvalidRelationship("incidence edges must be unique"))
    else
      val entries = Vector.newBuilder[(Int, Int, Double)]
      val sourceDegree = Array.fill(from.dimension)(0)
      val targetDegree = Array.fill(to.dimension)(0)
      var index = 0
      var error = Option.empty[AlignmentError]
      while index < edges.length && error.isEmpty do
        val (source, target) = edges(index)
        if source < 0 || source >= from.dimension then
          error = Some(AlignmentError.InvalidRelationship(s"incidence source index $source is out of bounds"))
        else if target < 0 || target >= to.dimension then
          error = Some(AlignmentError.InvalidRelationship(s"incidence target index $target is out of bounds"))
        else
          entries += ((target, source, 1.0))
          sourceDegree(source) += 1
          targetDegree(target) += 1
        index += 1
      error match
        case Some(value) => Left(value)
        case None =>
          val cardinality = RelationshipMatrices.cardinality(sourceDegree, targetDegree)
          val representedSources = sourceDegree.count(_ > 0)
          val representedTargets = targetDegree.count(_ > 0)
          RelationshipMatrices
            .sparseRowMap(
              from,
              to,
              entries.result(),
              RowRelationshipKind.IncidenceMap,
              RelationshipSupport(
                edges.length.toDouble,
                (from.dimension - representedSources).toDouble,
                (to.dimension - representedTargets).toDouble,
                0.0,
                0.0,
                from.dimension * to.dimension - edges.length,
                cardinality,
                duplicateKeys,
                representedSources == from.dimension,
                representedTargets == to.dimension
              ),
              RelationshipNormalization.Unnormalized,
              AlignmentOrigin.ExternallySupplied,
              valueIdentity,
              provenance
            )
            .map(new IncidenceMap(_, edges))

  private[multivar] def fromPartial[From <: SemanticSpace, To <: SemanticSpace](
      partial: PartialInjection[From, To]
  ): IncidenceMap[From, To] =
    val edges = partial.assignments.zipWithIndex.collect { case (Some(target), source) => (source, target) }
    val descriptor = partial.rowMap.descriptor.copy(
      kind = RowRelationshipKind.IncidenceMap,
      normalization = RelationshipNormalization.Unnormalized
    )
    new IncidenceMap(
      new TypedRowMap(
        partial.rowMap.operator,
        partial.rowMap.matrix,
        descriptor,
        partial.rowMap.provenance.append(
          SemanticProvenanceEvent.Derived("partial-injection-to-incidence", Vector(partial.rowMap.operator.valueIdentity))
        )
      ),
      edges
    )

final class AggregationMap[From <: SemanticSpace, To <: SemanticSpace] private (
    val rowMap: TypedRowMap[From, To]
)

object AggregationMap:
  def fromMatrix[From <: SemanticSpace, To <: SemanticSpace](
      from: SpaceEvidence[From],
      to: SpaceEvidence[To],
      matrix: DoubleMatrix,
      normalization: RelationshipNormalization,
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance = SemanticProvenance.source("row-aggregation-map")
  ): Either[AlignmentError, AggregationMap[From, To]] =
    RelationshipMatrices
      .denseRowMap(
        from,
        to,
        matrix,
        requireNonnegative = true,
        RowRelationshipKind.AggregationMap,
        normalization,
        valueIdentity,
        provenance
      )
      .map(new AggregationMap(_))

final class NonnegativeCoupling[Left <: SemanticSpace, Right <: SemanticSpace] private[multivar] (
    val rowLink: TypedRowLink[Left, Right],
    val marginals: CouplingMarginals
):
  def toRowLink: TypedRowLink[Left, Right] =
    rowLink

object NonnegativeCoupling:
  def fromMatrix[Left <: SemanticSpace, Right <: SemanticSpace](
      left: SpaceEvidence[Left],
      right: SpaceEvidence[Right],
      matrix: DoubleMatrix,
      normalization: RelationshipNormalization,
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance = SemanticProvenance.source("nonnegative-row-coupling")
  ): Either[AlignmentError, NonnegativeCoupling[Left, Right]] =
    RelationshipMatrices
      .denseRowLink(
        left,
        right,
        matrix,
        requireNonnegative = true,
        RowRelationshipKind.NonnegativeCoupling,
        normalization,
        valueIdentity,
        provenance
      )
      .map { case (link, marginals) => new NonnegativeCoupling(link, marginals) }

final class ProbabilisticCoupling[Left <: SemanticSpace, Right <: SemanticSpace] private (
    val coupling: NonnegativeCoupling[Left, Right]
):
  def rowLink: TypedRowLink[Left, Right] =
    coupling.rowLink

  def marginals: CouplingMarginals =
    coupling.marginals

object ProbabilisticCoupling:
  def fromNonnegative[Left <: SemanticSpace, Right <: SemanticSpace](
      coupling: NonnegativeCoupling[Left, Right],
      tolerance: Double = 1e-10
  ): Either[AlignmentError, ProbabilisticCoupling[Left, Right]] =
    if !tolerance.isFinite || tolerance < 0.0 then
      Left(AlignmentError.InvalidRelationship(s"coupling tolerance must be finite and non-negative, got $tolerance"))
    else if Math.abs(coupling.marginals.totalMass - 1.0) > tolerance then
      Left(
        AlignmentError.MarginalMismatch(
          s"probabilistic coupling must already have unit mass; got ${coupling.marginals.totalMass}"
        )
      )
    else
      val descriptor = coupling.rowLink.descriptor.copy(
        kind = RowRelationshipKind.ProbabilisticCoupling,
        normalization = RelationshipNormalization.UnitMass
      )
      Right(
        new ProbabilisticCoupling(
          new NonnegativeCoupling(
            new TypedRowLink(
              coupling.rowLink.operator,
              coupling.rowLink.matrix,
              descriptor,
              coupling.rowLink.provenance.append(
                SemanticProvenanceEvent.Derived(
                  "nonnegative-to-probabilistic-coupling",
                  Vector(coupling.rowLink.operator.valueIdentity)
                )
              )
            ),
            coupling.marginals
          )
        )
      )

final class SignedRowLink[Left <: SemanticSpace, Right <: SemanticSpace] private (
    val rowLink: TypedRowLink[Left, Right]
)

object SignedRowLink:
  def fromMatrix[Left <: SemanticSpace, Right <: SemanticSpace](
      left: SpaceEvidence[Left],
      right: SpaceEvidence[Right],
      matrix: DoubleMatrix,
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance = SemanticProvenance.source("signed-row-link")
  ): Either[AlignmentError, SignedRowLink[Left, Right]] =
    RelationshipMatrices
      .denseRowLink(
        left,
        right,
        matrix,
        requireNonnegative = false,
        RowRelationshipKind.SignedRowLink,
        RelationshipNormalization.Unnormalized,
        valueIdentity,
        provenance
      )
      .map { case (link, _) => new SignedRowLink(link) }

final class SameEntityEvidence[Left <: SemanticSpace, Right <: SemanticSpace] private[multivar] (
    val exactIdentity: ExactBijection[Left, Right],
    val entitySpace: MvSpace,
    val keySetIdentity: ValueIdentity,
    val provenance: SemanticProvenance
):
  def toLegacyPair(
      left: MatrixView,
      right: MatrixView,
      rowMetric: Option[MvMetric] = None,
      leftColumnMetric: Option[MvMetric] = None,
      rightColumnMetric: Option[MvMetric] = None,
      leftSpace: Option[MvSpace] = None,
      rightSpace: Option[MvSpace] = None
  ): Either[MultivarError, PairedDualityDiagram] =
    PairedDualityDiagram.fromPositionalUnsafe(
      left,
      right,
      rowMetric,
      leftColumnMetric,
      rightColumnMetric,
      Some(entitySpace),
      leftSpace,
      rightSpace
    )

object SameEntityEvidence:
  def fromVerifiedIdentity[Left <: SemanticSpace, Right <: SemanticSpace](
      left: SpaceEvidence[Left],
      right: SpaceEvidence[Right],
      entitySpace: MvSpace,
      keySetIdentity: ValueIdentity,
      duplicateKeys: Vector[String] = Vector.empty
  ): Either[AlignmentError, SameEntityEvidence[Left, Right]] =
    if duplicateKeys.nonEmpty then
      Left(AlignmentError.InvalidRelationship("verified same-entity evidence cannot contain duplicate keys"))
    else if entitySpace.size != left.dimension || entitySpace.size != right.dimension then
      Left(
        AlignmentError.InvalidRelationship(
          s"entity space has ${entitySpace.size} entities but row spaces have ${left.dimension} and ${right.dimension}"
        )
      )
    else
      ExactBijection
        .fromPermutation(
          left,
          right,
          Vector.tabulate(left.dimension)(identity),
          ValueIdentity.derived("same-entity-identity", keySetIdentity),
          duplicateKeys,
          AlignmentOrigin.ObservedKeys,
          SemanticProvenance.source("verified-same-entity-keys")
        )
        .map(
          new SameEntityEvidence(
            _,
            entitySpace,
            keySetIdentity,
            SemanticProvenance.source("verified-same-entity-evidence")
          )
        )

  private[multivar] def unsafeIdentity[Left <: SemanticSpace, Right <: SemanticSpace](
      left: SpaceEvidence[Left],
      right: SpaceEvidence[Right],
      entitySpace: MvSpace,
      reason: String
  ): Either[AlignmentError, SameEntityEvidence[Left, Right]] =
    if reason.trim.isEmpty then
      Left(AlignmentError.InvalidRelationship("unsafe same-row assumptions require a non-empty reason"))
    else if left.dimension != right.dimension || entitySpace.size != left.dimension then
      Left(AlignmentError.InvalidRelationship("unsafe same-row assumption still requires equal cardinalities"))
    else
      val assumptionIdentity = ValueIdentity.source(
        ValueId.unsafe(s"unsafe.same-rows.${left.id.value}.${right.id.value}")
      )
      ExactBijection
        .fromPermutation(
          left,
          right,
          Vector.tabulate(left.dimension)(identity),
          assumptionIdentity,
          origin = AlignmentOrigin.UnsafeAssumption,
          provenance = SemanticProvenance(
            Vector(SemanticProvenanceEvent.UnsafeAssumption("same-row-identity", reason.trim))
          )
        )
        .map(
          new SameEntityEvidence(
            _,
            entitySpace,
            assumptionIdentity,
            SemanticProvenance(
              Vector(SemanticProvenanceEvent.UnsafeAssumption("same-row-identity", reason.trim))
            )
          )
        )

private[multivar] object RelationshipMatrices:
  def sparseRowMap[From <: SemanticSpace, To <: SemanticSpace](
      from: SpaceEvidence[From],
      to: SpaceEvidence[To],
      entries: Vector[(Int, Int, Double)],
      kind: RowRelationshipKind,
      support: RelationshipSupport,
      normalization: RelationshipNormalization,
      origin: AlignmentOrigin,
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[AlignmentError, TypedRowMap[From, To]] =
    val rowIndices = entries.map(_._1).toArray
    val colIndices = entries.map(_._2).toArray
    val values = entries.map(_._3).toArray
    for
      csr <- CsrMatrix
        .fromTriplets(to.dimension, from.dimension, rowIndices, colIndices, values)
        .left
        .map(error => AlignmentError.InvalidRelationship(error.message))
      view <- SparseMatrixView
        .fromTriplets(to.dimension, from.dimension, rowIndices, colIndices, values)
        .left
        .map(AlignmentError.Multivar.apply)
      operator <- Lin
        .fromLinearMap[Primal[From], Primal[To]](
          csr,
          CoordinateEvidence.primal(from),
          CoordinateEvidence.primal(to),
          valueIdentity,
          provenance
        )
        .left
        .map(AlignmentError.Semantic.apply)
    yield
      new TypedRowMap(
        operator,
        view,
        RowRelationshipDescriptor(
          kind,
          from.descriptor,
          to.descriptor,
          operator.descriptor,
          support,
          normalization,
          None,
          origin,
          valueIdentity
        ),
        provenance
      )

  def denseRowMap[From <: SemanticSpace, To <: SemanticSpace](
      from: SpaceEvidence[From],
      to: SpaceEvidence[To],
      matrix: DoubleMatrix,
      requireNonnegative: Boolean,
      kind: RowRelationshipKind,
      normalization: RelationshipNormalization,
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[AlignmentError, TypedRowMap[From, To]] =
    validateMatrix(matrix, to.dimension, from.dimension, requireNonnegative).flatMap { stats =>
      Lin
        .fromDenseMatrix[Primal[From], Primal[To]](
          matrix,
          CoordinateEvidence.primal(from),
          CoordinateEvidence.primal(to),
          valueIdentity,
          provenance
        )
        .left
        .map(AlignmentError.Semantic.apply)
        .map { operator =>
          new TypedRowMap(
            operator,
            DenseMatrixView(matrix),
            RowRelationshipDescriptor(
              kind,
              from.descriptor,
              to.descriptor,
              operator.descriptor,
              stats.support,
              normalization,
              if requireNonnegative then Some(stats.marginals) else None,
              AlignmentOrigin.ExternallySupplied,
              valueIdentity
            ),
            provenance
          )
        }
    }

  def denseRowLink[Left <: SemanticSpace, Right <: SemanticSpace](
      left: SpaceEvidence[Left],
      right: SpaceEvidence[Right],
      matrix: DoubleMatrix,
      requireNonnegative: Boolean,
      kind: RowRelationshipKind,
      normalization: RelationshipNormalization,
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[AlignmentError, (TypedRowLink[Left, Right], CouplingMarginals)] =
    validateMatrix(matrix, left.dimension, right.dimension, requireNonnegative).flatMap { stats =>
      Lin
        .fromDenseMatrix[Primal[Right], Dual[Left]](
          matrix,
          CoordinateEvidence.primal(right),
          CoordinateEvidence.dual(left),
          valueIdentity,
          provenance
        )
        .left
        .map(AlignmentError.Semantic.apply)
        .map { operator =>
          val link = new TypedRowLink(
            operator,
            DenseMatrixView(matrix),
            RowRelationshipDescriptor(
              kind,
              right.descriptor,
              left.descriptor,
              operator.descriptor,
              stats.support,
              normalization,
              if requireNonnegative then Some(stats.marginals) else None,
              AlignmentOrigin.ExternallySupplied,
              valueIdentity
            ),
            provenance
          )
          (link, stats.marginals)
        }
    }

  def matrixOf[From <: Coordinate, To <: Coordinate](
      operator: Lin[From, To]
  ): Either[AlignmentError, DoubleMatrix] =
    operator(DoubleMatrix.eye(operator.cols)).left.map(AlignmentError.Semantic.apply)

  final case class MatrixStats(
      support: RelationshipSupport,
      marginals: CouplingMarginals
  )

  def validateMatrix(
      matrix: DoubleMatrix,
      expectedRows: Int,
      expectedColumns: Int,
      requireNonnegative: Boolean
  ): Either[AlignmentError, MatrixStats] =
    if matrix.rows != expectedRows || matrix.cols != expectedColumns then
      Left(
        AlignmentError.InvalidRelationship(
          s"row relationship expected ${expectedRows}x${expectedColumns}, got ${matrix.rows}x${matrix.cols}"
        )
      )
    else
      val left = new Array[Double](matrix.rows)
      val right = new Array[Double](matrix.cols)
      val sourceDegree = Array.fill(matrix.cols)(0)
      val targetDegree = Array.fill(matrix.rows)(0)
      var row = 0
      var nonzero = 0
      var total = 0.0
      var absoluteTotal = 0.0
      var error = Option.empty[AlignmentError]
      while row < matrix.rows && error.isEmpty do
        var col = 0
        while col < matrix.cols && error.isEmpty do
          val value = matrix(row, col)
          if !value.isFinite then
            error = Some(AlignmentError.InvalidRelationship(s"row relationship value ($row, $col) is not finite"))
          else if requireNonnegative && value < 0.0 then
            error = Some(AlignmentError.InvalidRelationship(s"row relationship value ($row, $col) is negative: $value"))
          else
            left(row) += value
            right(col) += value
            total += value
            absoluteTotal += Math.abs(value)
            if value != 0.0 then
              targetDegree(row) += 1
              sourceDegree(col) += 1
              nonzero += 1
          col += 1
        row += 1
      error match
        case Some(value) => Left(value)
        case None =>
          val representedSources = sourceDegree.count(_ > 0)
          val representedTargets = targetDegree.count(_ > 0)
          val support = RelationshipSupport(
            matchedMass = absoluteTotal,
            unmatchedSourceMass = (matrix.cols - representedSources).toDouble,
            unmatchedTargetMass = (matrix.rows - representedTargets).toDouble,
            uncertainMass = 0.0,
            excludedMass = 0.0,
            structuralZeroCount = matrix.rows * matrix.cols - nonzero,
            cardinality = cardinality(sourceDegree, targetDegree),
            duplicateKeys = Vector.empty,
            everySourceRepresented = representedSources == matrix.cols,
            everyTargetRepresented = representedTargets == matrix.rows
          )
          Right(
            MatrixStats(
              support,
              CouplingMarginals(DoubleVector.unsafe(left), DoubleVector.unsafe(right), total)
            )
          )

  def cardinality(sourceDegree: Array[Int], targetDegree: Array[Int]): CardinalityStructure =
    val sourceMany = sourceDegree.exists(_ > 1)
    val targetMany = targetDegree.exists(_ > 1)
    (sourceMany, targetMany) match
      case (false, false) => CardinalityStructure.OneToOne
      case (true, false)  => CardinalityStructure.OneToMany
      case (false, true)  => CardinalityStructure.ManyToOne
      case (true, true)   => CardinalityStructure.ManyToMany
