package scalafim.fmri.mvpa

import multivar.core.SemanticSpace

opaque type PartitionId = String

object PartitionId:
  def apply(value: String): Either[PairingDesignError, PartitionId] =
    AxisKey(value).left
      .map(PairingDesignError.InvalidPartitionId.apply)
      .map(_.value)

  private[mvpa] def unsafe(value: String): PartitionId =
    value

  extension (id: PartitionId) inline def value: String = id

  given AxisKeyCodec[PartitionId] with
    override def encode(key: PartitionId): AxisKey = AxisKey.unsafe(key.value)

opaque type IndependenceDeclarationId = String

object IndependenceDeclarationId:
  def apply(value: String): Either[PairingDesignError, IndependenceDeclarationId] =
    ScientificIdentityText
      .lowerIdentifier("independence declaration id", value)
      .left
      .map(PairingDesignError.InvalidIdentity.apply)

  private[mvpa] def unsafe(value: String): IndependenceDeclarationId =
    value

  extension (id: IndependenceDeclarationId) inline def value: String = id

final class PartitionAxis[S <: SemanticSpace] private (
    val name: ScientificAxisName,
    val axis: AxisRef.Aux[PartitionId, S]
):
  def reference: DesignAxisReference =
    DesignAxisReference(name, axis.identity)

object PartitionAxis:
  def apply[S <: SemanticSpace](
      name: ScientificAxisName,
      axis: AxisRef.Aux[PartitionId, S]
  ): Either[PairingDesignError, PartitionAxis[S]] =
    if axis.identity.purpose != AxisPurpose.Partitions then
      Left(
        PairingDesignError.InvalidPartitionPurpose(
          AxisPurpose.Partitions,
          axis.identity.purpose
        )
      )
    else Right(new PartitionAxis(name, axis))

final case class PairingEdge private (
    left: PartitionId,
    right: PartitionId,
    weight: Double
):
  def reversed: PairingEdge =
    new PairingEdge(right, left, weight)

object PairingEdge:
  def apply(
      left: PartitionId,
      right: PartitionId,
      weight: Double = 1.0
  ): Either[PairingDesignError, PairingEdge] =
    if !weight.isFinite || weight == 0.0 then Left(PairingDesignError.InvalidEdgeWeight(left, right, weight))
    else Right(new PairingEdge(left, right, canonicalZero(weight)))

  private def canonicalZero(value: Double): Double =
    if value == 0.0 then 0.0 else value

enum PairingReducer:
  case WeightedMean
  case WeightedSum

  def label: String =
    this match
      case WeightedMean => "weighted-mean"
      case WeightedSum  => "weighted-sum"

enum PairingMode:
  case Custom
  case AllOrdered
  case ForwardOnly
  case ReversedForwardOnly
  case Rectangular

  def label: String =
    this match
      case Custom              => "custom"
      case AllOrdered          => "all-ordered"
      case ForwardOnly         => "forward-only"
      case ReversedForwardOnly => "reversed-forward-only"
      case Rectangular         => "rectangular"

  def reversed: PairingMode =
    this match
      case ForwardOnly         => ReversedForwardOnly
      case ReversedForwardOnly => ForwardOnly
      case other               => other

/** One pair that a source or design owner explicitly declares independent. The pair has no meaning without the evidence
  * identities carried by its [[PartitionIndependenceDeclaration]].
  */
final case class DeclaredIndependentPair(
    left: PartitionId,
    right: PartitionId
):
  def reversed: DeclaredIndependentPair =
    DeclaredIndependentPair(right, left)

/** The exact evidence source and nominal partition witness named by an independence declaration. This is deliberately
  * not inferred from a partition axis: a source or design owner must name the evidence identity.
  */
final class PartitionEvidenceIdentity[S <: SemanticSpace] private (
    val source: ScientificSourceIdentity,
    val partitions: PartitionAxis[S]
)

object PartitionEvidenceIdentity:
  def apply[S <: SemanticSpace](
      source: ScientificSourceIdentity,
      partitions: PartitionAxis[S]
  ): Either[PairingDesignError, PartitionEvidenceIdentity[S]] =
    source.axis(partitions.name) match
      case None =>
        Left(PairingDesignError.MissingPartitionEvidenceAxis(partitions.name))
      case Some(actual) if actual != partitions.axis.identity =>
        Left(
          PairingDesignError.PartitionEvidenceAxisMismatch(
            partitions.name,
            partitions.axis.identity.fingerprint,
            actual.fingerprint
          )
        )
      case Some(_) => Right(new PartitionEvidenceIdentity(source, partitions))

/** An explicit, inspectable declaration that exact partition pairs from exact evidence identities may be treated as
  * independent. It is a declaration, not a statistical certificate: the API makes the scientific assumption visible and
  * prevents it from being inferred from unequal identifiers.
  */
final class PartitionIndependenceDeclaration[
    L <: SemanticSpace,
    R <: SemanticSpace
] private (
    val id: IndependenceDeclarationId,
    val leftEvidence: PartitionEvidenceIdentity[L],
    val rightEvidence: PartitionEvidenceIdentity[R],
    val pairs: Vector[DeclaredIndependentPair],
    val identity: ScientificComponentFingerprint
):
  private val pairSet = pairs.iterator.map(pair => pair.left -> pair.right).toSet

  private def withinOneEvidence: Boolean =
    leftEvidence.source == rightEvidence.source &&
      leftEvidence.partitions.axis.identity == rightEvidence.partitions.axis.identity &&
      (leftEvidence.partitions.axis.evidence eq rightEvidence.partitions.axis.evidence)

  private[mvpa] def reversed: PartitionIndependenceDeclaration[R, L] =
    new PartitionIndependenceDeclaration(
      id,
      rightEvidence,
      leftEvidence,
      pairs.map(_.reversed),
      identity
    )

  private[mvpa] def validateDesign(
      left: PartitionAxis[L],
      right: PartitionAxis[R],
      edges: Vector[PairingEdge]
  ): Either[PairingDesignError, Unit] =
    for
      _ <- validateAxis("left", leftEvidence.partitions, left)
      _ <- validateAxis("right", rightEvidence.partitions, right)
      _ <- edges.find(edge => !attests(edge.left, edge.right)) match
        case Some(edge) => Left(PairingDesignError.UnattestedPair(edge.left, edge.right))
        case None       => Right(())
    yield ()

  private[mvpa] def validateSource(
      source: ScientificSourceIdentity,
      partitions: PartitionAxis[L]
  )(using L =:= R): Either[PairingDesignError, Unit] =
    for
      _ <- validateEvidence("left", leftEvidence, source, partitions)
      _ <- validateEvidence(
        "right",
        rightEvidence,
        source,
        partitions
      )
    yield ()

  private def attests(left: PartitionId, right: PartitionId): Boolean =
    pairSet.contains(left -> right) ||
      (withinOneEvidence && pairSet.contains(right -> left))

  private def validateAxis[Declared <: SemanticSpace, Actual <: SemanticSpace](
      boundary: String,
      declared: PartitionAxis[Declared],
      actual: PartitionAxis[Actual]
  ): Either[PairingDesignError, Unit] =
    if declared.axis.identity != actual.axis.identity then
      Left(
        PairingDesignError.IndependenceAxisMismatch(
          boundary,
          declared.axis.identity.fingerprint,
          actual.axis.identity.fingerprint
        )
      )
    else if !(declared.axis.evidence eq actual.axis.evidence) then
      Left(PairingDesignError.IndependenceWitnessMismatch(boundary))
    else Right(())

  private def validateEvidence[
      Declared <: SemanticSpace,
      Actual <: SemanticSpace
  ](
      boundary: String,
      declared: PartitionEvidenceIdentity[Declared],
      source: ScientificSourceIdentity,
      partitions: PartitionAxis[Actual]
  ): Either[PairingDesignError, Unit] =
    if declared.source != source then
      Left(
        PairingDesignError.IndependenceEvidenceMismatch(
          boundary,
          declared.source.fingerprint,
          source.fingerprint
        )
      )
    else validateAxis(boundary, declared.partitions, partitions)

object PartitionIndependenceDeclaration:
  private val Kind = EstimandKind.unsafe("partition-independence-declaration")

  def apply[L <: SemanticSpace, R <: SemanticSpace](
      id: IndependenceDeclarationId,
      leftEvidence: PartitionEvidenceIdentity[L],
      rightEvidence: PartitionEvidenceIdentity[R],
      pairs: Seq[DeclaredIndependentPair]
  ): Either[PairingDesignError, PartitionIndependenceDeclaration[L, R]] =
    for
      canonical <- validatePairs(leftEvidence, rightEvidence, pairs)
      leftKey = evidenceKey(leftEvidence)
      rightKey = evidenceKey(rightEvidence)
      firstAxis =
        if leftKey <= rightKey then leftEvidence.partitions.axis.identity.fingerprint
        else rightEvidence.partitions.axis.identity.fingerprint
      firstEvidence =
        if leftKey <= rightKey then leftEvidence.source.fingerprint
        else rightEvidence.source.fingerprint
      secondAxis =
        if leftKey <= rightKey then rightEvidence.partitions.axis.identity.fingerprint
        else leftEvidence.partitions.axis.identity.fingerprint
      secondEvidence =
        if leftKey <= rightKey then rightEvidence.source.fingerprint
        else leftEvidence.source.fingerprint
      component <- EstimandIdentity(
        Kind,
        Vector(
          "declaration-id" -> id.value,
          "first-axis" -> firstAxis.value,
          "first-evidence" -> firstEvidence.value,
          "pairs" -> pairDigest(leftEvidence, rightEvidence, canonical),
          "second-axis" -> secondAxis.value,
          "second-evidence" -> secondEvidence.value
        )
      ).left.map(PairingDesignError.InvalidIdentity.apply)
    yield new PartitionIndependenceDeclaration(
      id,
      leftEvidence,
      rightEvidence,
      canonical,
      component.fingerprint
    )

  private def validatePairs[L <: SemanticSpace, R <: SemanticSpace](
      leftEvidence: PartitionEvidenceIdentity[L],
      rightEvidence: PartitionEvidenceIdentity[R],
      input: Seq[DeclaredIndependentPair]
  ): Either[PairingDesignError, Vector[DeclaredIndependentPair]] =
    if input.isEmpty then Left(PairingDesignError.EmptyIndependenceDeclaration)
    else
      val sameEvidence =
        leftEvidence.source == rightEvidence.source &&
          leftEvidence.partitions.axis.identity == rightEvidence.partitions.axis.identity &&
          (leftEvidence.partitions.axis.evidence eq rightEvidence.partitions.axis.evidence)
      val indexed = Vector.newBuilder[(Int, Int, DeclaredIndependentPair)]
      val seen = scala.collection.mutable.HashSet.empty[(PartitionId, PartitionId)]
      val iterator = input.iterator
      while iterator.hasNext do
        val pair = iterator.next()
        val leftPosition = leftEvidence.partitions.axis.positionOf(pair.left) match
          case Some(value) => value
          case None        => return Left(PairingDesignError.UnknownDeclaredLeft(pair.left))
        val rightPosition = rightEvidence.partitions.axis.positionOf(pair.right) match
          case Some(value) => value
          case None        => return Left(PairingDesignError.UnknownDeclaredRight(pair.right))
        if sameEvidence && pair.left == pair.right then
          return Left(PairingDesignError.SelfPairCannotBeDeclaredIndependent(pair.left))
        val canonical =
          if sameEvidence && leftPosition > rightPosition then pair.reversed
          else pair
        val positions =
          if sameEvidence && leftPosition > rightPosition then rightPosition -> leftPosition
          else leftPosition -> rightPosition
        val key = canonical.left -> canonical.right
        if seen.contains(key) then
          return Left(
            PairingDesignError.DuplicateDeclaredPair(canonical.left, canonical.right)
          )
        seen += key
        indexed += ((positions._1, positions._2, canonical))
      Right(indexed.result().sortBy(value => (value._1, value._2)).map(_._3))

  private def pairDigest[L <: SemanticSpace, R <: SemanticSpace](
      leftEvidence: PartitionEvidenceIdentity[L],
      rightEvidence: PartitionEvidenceIdentity[R],
      pairs: Vector[DeclaredIndependentPair]
  ): String =
    val writer = CanonicalWriter()
    writer.string("scalafim-mvpa-partition-independence-declaration/v1")
    val leftKey = evidenceKey(leftEvidence)
    val rightKey = evidenceKey(rightEvidence)
    val canonicalPairs =
      if leftKey <= rightKey then pairs
      else pairs.map(_.reversed)
    writer.string(if leftKey <= rightKey then leftKey else rightKey)
    writer.string(if leftKey <= rightKey then rightKey else leftKey)
    writer.int(canonicalPairs.length)
    canonicalPairs
      .sortBy(pair => (pair.left.value, pair.right.value))
      .foreach: pair =>
        writer.string(pair.left.value)
        writer.string(pair.right.value)
    AxisDigest.sha256Hex(writer.result())

  private def evidenceKey[S <: SemanticSpace](
      evidence: PartitionEvidenceIdentity[S]
  ): String =
    s"${evidence.source.fingerprint.value}:${evidence.partitions.axis.identity.fingerprint.value}"

final class PairingDesign[
    L <: SemanticSpace,
    R <: SemanticSpace
] private (
    val left: PartitionAxis[L],
    val right: PartitionAxis[R],
    val edges: Vector[PairingEdge],
    val reducer: PairingReducer,
    val mode: PairingMode,
    val generalizesOver: GeneralizationAxis,
    val independence: PartitionIndependenceDeclaration[L, R],
    val identity: DesignIdentity,
    val referencedAxes: Vector[DesignAxisReference]
) extends EvidenceDesign:
  def reverse: Either[PairingDesignError, PairingDesign[R, L]] =
    PairingDesign.build(
      right,
      left,
      edges.map(_.reversed),
      reducer,
      mode.reversed,
      generalizesOver,
      independence.reversed
    )

  private[mvpa] def validateSource(
      source: ScientificSourceIdentity,
      partitions: PartitionAxis[L]
  )(using L =:= R): Either[PairingDesignError, Unit] =
    independence.validateSource(source, partitions)

object PairingDesign:
  def apply[L <: SemanticSpace, R <: SemanticSpace](
      left: PartitionAxis[L],
      right: PartitionAxis[R],
      edges: Seq[PairingEdge],
      reducer: PairingReducer,
      generalizesOver: GeneralizationAxis,
      independence: PartitionIndependenceDeclaration[L, R]
  ): Either[PairingDesignError, PairingDesign[L, R]] =
    build(
      left,
      right,
      edges,
      reducer,
      PairingMode.Custom,
      generalizesOver,
      independence
    )

  def allOrdered[S <: SemanticSpace](
      partitions: PartitionAxis[S],
      reducer: PairingReducer,
      generalizesOver: GeneralizationAxis,
      independence: PartitionIndependenceDeclaration[S, S]
  ): Either[PairingDesignError, PairingDesign[S, S]] =
    sameSpace(
      partitions,
      reducer,
      PairingMode.AllOrdered,
      generalizesOver,
      forwardOnly = false,
      independence
    )

  def forwardOnly[S <: SemanticSpace](
      partitions: PartitionAxis[S],
      reducer: PairingReducer,
      generalizesOver: GeneralizationAxis,
      independence: PartitionIndependenceDeclaration[S, S]
  ): Either[PairingDesignError, PairingDesign[S, S]] =
    sameSpace(
      partitions,
      reducer,
      PairingMode.ForwardOnly,
      generalizesOver,
      forwardOnly = true,
      independence
    )

  def rectangular[L <: SemanticSpace, R <: SemanticSpace](
      left: PartitionAxis[L],
      right: PartitionAxis[R],
      reducer: PairingReducer,
      generalizesOver: GeneralizationAxis,
      independence: PartitionIndependenceDeclaration[L, R]
  ): Either[PairingDesignError, PairingDesign[L, R]] =
    val edges = Vector.newBuilder[PairingEdge]
    var failure: Option[PairingDesignError] = None
    var leftPosition = 0
    while leftPosition < left.axis.size && failure.isEmpty do
      var rightPosition = 0
      while rightPosition < right.axis.size && failure.isEmpty do
        PairingEdge(
          left.axis.keys(leftPosition),
          right.axis.keys(rightPosition)
        ) match
          case Left(error) => failure = Some(error)
          case Right(edge) => edges += edge
        rightPosition += 1
      leftPosition += 1
    failure match
      case Some(error) => Left(error)
      case None        =>
        build(
          left,
          right,
          edges.result(),
          reducer,
          PairingMode.Rectangular,
          generalizesOver,
          independence
        )

  private def sameSpace[S <: SemanticSpace](
      partitions: PartitionAxis[S],
      reducer: PairingReducer,
      mode: PairingMode,
      generalizesOver: GeneralizationAxis,
      forwardOnly: Boolean,
      independence: PartitionIndependenceDeclaration[S, S]
  ): Either[PairingDesignError, PairingDesign[S, S]] =
    val edges = Vector.newBuilder[PairingEdge]
    var left = 0
    while left < partitions.axis.size do
      var right = 0
      while right < partitions.axis.size do
        val included =
          if forwardOnly then left < right
          else left != right
        if included then
          PairingEdge(
            partitions.axis.keys(left),
            partitions.axis.keys(right)
          ) match
            case Left(error) => return Left(error)
            case Right(edge) => edges += edge
        right += 1
      left += 1
    build(
      partitions,
      partitions,
      edges.result(),
      reducer,
      mode,
      generalizesOver,
      independence
    )

  private[mvpa] def build[L <: SemanticSpace, R <: SemanticSpace](
      left: PartitionAxis[L],
      right: PartitionAxis[R],
      inputEdges: Seq[PairingEdge],
      reducer: PairingReducer,
      mode: PairingMode,
      generalizesOver: GeneralizationAxis,
      independence: PartitionIndependenceDeclaration[L, R]
  ): Either[PairingDesignError, PairingDesign[L, R]] =
    for
      edges <- validateEdges(left, right, inputEdges, reducer)
      _ <- independence.validateDesign(left, right, edges)
      references <- referencesOf(left, right, generalizesOver)
      identity <- DesignIdentity(
        DesignKind.unsafe("pairing"),
        Vector(
          "edges" -> edgeDigest(edges),
          "generalization-axis" -> generalizesOver.name.value,
          "generalization-space" -> generalizesOver.identity.fingerprint.value,
          "independence-declaration" -> independence.identity.value,
          "independence-declaration-id" -> independence.id.value,
          "left-axis" -> left.name.value,
          "left-space" -> left.axis.identity.fingerprint.value,
          "mode" -> mode.label,
          "reducer" -> reducer.label,
          "right-axis" -> right.name.value,
          "right-space" -> right.axis.identity.fingerprint.value
        )
      ).left.map(PairingDesignError.InvalidIdentity.apply)
    yield new PairingDesign(
      left,
      right,
      edges,
      reducer,
      mode,
      generalizesOver,
      independence,
      identity,
      references
    )

  private def validateEdges[L <: SemanticSpace, R <: SemanticSpace](
      left: PartitionAxis[L],
      right: PartitionAxis[R],
      input: Seq[PairingEdge],
      reducer: PairingReducer
  ): Either[PairingDesignError, Vector[PairingEdge]] =
    if input.isEmpty then Left(PairingDesignError.EmptyEdges)
    else
      val indexed = Vector.newBuilder[(Int, Int, PairingEdge)]
      val seen = scala.collection.mutable.HashSet.empty[(PartitionId, PartitionId)]
      val iterator = input.iterator
      while iterator.hasNext do
        val edge = iterator.next()
        val leftPosition = left.axis.positionOf(edge.left) match
          case Some(value) => value
          case None        => return Left(PairingDesignError.UnknownLeft(edge.left))
        val rightPosition = right.axis.positionOf(edge.right) match
          case Some(value) => value
          case None        => return Left(PairingDesignError.UnknownRight(edge.right))
        val key = edge.left -> edge.right
        if seen.contains(key) then return Left(PairingDesignError.DuplicateEdge(edge.left, edge.right))
        if reducer == PairingReducer.WeightedMean && edge.weight <= 0.0 then
          return Left(
            PairingDesignError.NonPositiveMeanWeight(
              edge.left,
              edge.right,
              edge.weight
            )
          )
        seen += key
        indexed += ((leftPosition, rightPosition, edge))
      Right(indexed.result().sortBy(value => (value._1, value._2)).map(_._3))

  private def referencesOf[L <: SemanticSpace, R <: SemanticSpace](
      left: PartitionAxis[L],
      right: PartitionAxis[R],
      generalization: GeneralizationAxis
  ): Either[PairingDesignError, Vector[DesignAxisReference]] =
    val input = Vector(left.reference, right.reference, generalization.reference)
      .sortBy(_.name.value)
    val output = Vector.newBuilder[DesignAxisReference]
    var index = 0
    while index < input.length do
      val current = input(index)
      if index > 0 && input(index - 1).name == current.name then
        val previous = input(index - 1)
        if previous.identity != current.identity then
          return Left(
            PairingDesignError.ConflictingAxisReference(
              current.name,
              previous.identity.fingerprint,
              current.identity.fingerprint
            )
          )
      else output += current
      index += 1
    Right(output.result())

  private def edgeDigest(edges: Vector[PairingEdge]): String =
    val writer = CanonicalWriter()
    writer.string("scalafim-mvpa-pairing-edges/v1")
    writer.int(edges.length)
    edges.foreach: edge =>
      writer.string(edge.left.value)
      writer.string(edge.right.value)
      writer.double(edge.weight)
    AxisDigest.sha256Hex(writer.result())

final class PairingValues[
    L <: SemanticSpace,
    R <: SemanticSpace,
    +A
] private (
    val design: PairingDesign[L, R],
    val values: Vector[A]
):
  def transpose: Either[PairingDesignError, PairingValues[R, L, A]] =
    design.reverse match
      case Left(error)     => Left(error)
      case Right(reversed) =>
        val byEdge = design.edges
          .zip(values)
          .map: (edge, value) =>
            (edge.left, edge.right) -> value
        val lookup = byEdge.toMap
        val result = Vector.newBuilder[A]
        val iterator = reversed.edges.iterator
        var failure: Option[PairingDesignError] = None
        while iterator.hasNext && failure.isEmpty do
          val edge = iterator.next()
          lookup.get(edge.right -> edge.left) match
            case Some(value) => result += value
            case None        =>
              failure = Some(
                PairingDesignError.MissingTransposedEdge(edge.left, edge.right)
              )
        failure match
          case Some(error) => Left(error)
          case None        => Right(new PairingValues(reversed, result.result()))

object PairingValues:
  def apply[L <: SemanticSpace, R <: SemanticSpace, A](
      design: PairingDesign[L, R],
      values: Seq[A]
  ): Either[PairingDesignError, PairingValues[L, R, A]] =
    val vector = values.toVector
    if vector.length != design.edges.length then
      Left(
        PairingDesignError.ValueCountMismatch(
          design.edges.length,
          vector.length
        )
      )
    else Right(new PairingValues(design, vector))

enum PairingDesignError:
  case InvalidPartitionId(error: AxisIdentityError)
  case InvalidIdentity(error: ScientificIdentityError)
  case InvalidPartitionPurpose(expected: AxisPurpose, actual: AxisPurpose)
  case InvalidEdgeWeight(left: PartitionId, right: PartitionId, value: Double)
  case EmptyEdges
  case UnknownLeft(key: PartitionId)
  case UnknownRight(key: PartitionId)
  case DuplicateEdge(left: PartitionId, right: PartitionId)
  case NonPositiveMeanWeight(left: PartitionId, right: PartitionId, value: Double)
  case MissingPartitionEvidenceAxis(name: ScientificAxisName)
  case PartitionEvidenceAxisMismatch(
      name: ScientificAxisName,
      expected: AxisFingerprint,
      actual: AxisFingerprint
  )
  case EmptyIndependenceDeclaration
  case UnknownDeclaredLeft(key: PartitionId)
  case UnknownDeclaredRight(key: PartitionId)
  case SelfPairCannotBeDeclaredIndependent(key: PartitionId)
  case DuplicateDeclaredPair(left: PartitionId, right: PartitionId)
  case IndependenceAxisMismatch(
      boundary: String,
      expected: AxisFingerprint,
      actual: AxisFingerprint
  )
  case IndependenceWitnessMismatch(boundary: String)
  case IndependenceEvidenceMismatch(
      boundary: String,
      expected: ScientificComponentFingerprint,
      actual: ScientificComponentFingerprint
  )
  case UnattestedPair(left: PartitionId, right: PartitionId)
  case ConflictingAxisReference(
      name: ScientificAxisName,
      first: AxisFingerprint,
      second: AxisFingerprint
  )
  case ValueCountMismatch(expected: Int, actual: Int)
  case MissingTransposedEdge(left: PartitionId, right: PartitionId)

  def message: String =
    this match
      case InvalidPartitionId(error)                 => error.message
      case InvalidIdentity(error)                    => error.message
      case InvalidPartitionPurpose(expected, actual) =>
        s"pairing axis purpose '${actual.value}' is invalid; expected '${expected.value}'"
      case InvalidEdgeWeight(left, right, value) =>
        s"pairing edge ${left.value}->${right.value} has invalid finite non-zero weight $value"
      case EmptyEdges       => "pairing design must contain at least one edge"
      case UnknownLeft(key) =>
        s"pairing edge references unknown left partition '${key.value}'"
      case UnknownRight(key) =>
        s"pairing edge references unknown right partition '${key.value}'"
      case DuplicateEdge(left, right) =>
        s"pairing edge ${left.value}->${right.value} is duplicated"
      case NonPositiveMeanWeight(left, right, value) =>
        s"weighted-mean edge ${left.value}->${right.value} requires a positive weight, obtained $value"
      case MissingPartitionEvidenceAxis(name) =>
        s"evidence source does not identify partition axis '${name.value}'"
      case PartitionEvidenceAxisMismatch(name, expected, actual) =>
        s"evidence source axis '${name.value}' is ${actual.value}, expected ${expected.value}"
      case EmptyIndependenceDeclaration =>
        "partition-independence declaration must contain at least one exact pair"
      case UnknownDeclaredLeft(key) =>
        s"independence declaration references unknown left partition '${key.value}'"
      case UnknownDeclaredRight(key) =>
        s"independence declaration references unknown right partition '${key.value}'"
      case SelfPairCannotBeDeclaredIndependent(key) =>
        s"partition '${key.value}' cannot be declared independent from itself in one evidence source"
      case DuplicateDeclaredPair(left, right) =>
        s"independence declaration repeats pair ${left.value}<->${right.value}"
      case IndependenceAxisMismatch(boundary, expected, actual) =>
        s"independence declaration $boundary axis ${expected.value} does not match pairing axis ${actual.value}"
      case IndependenceWitnessMismatch(boundary) =>
        s"independence declaration and pairing $boundary axis use different nominal witnesses"
      case IndependenceEvidenceMismatch(boundary, expected, actual) =>
        s"independence declaration $boundary evidence ${expected.value} does not match source ${actual.value}"
      case UnattestedPair(left, right) =>
        s"pairing edge ${left.value}->${right.value} has no exact partition-independence declaration"
      case ConflictingAxisReference(name, first, second) =>
        s"design axis '${name.value}' refers to both ${first.value} and ${second.value}"
      case ValueCountMismatch(expected, actual) =>
        s"pairing values contain $actual entries, expected $expected"
      case MissingTransposedEdge(left, right) =>
        s"reversed pairing edge ${left.value}->${right.value} has no source value"
