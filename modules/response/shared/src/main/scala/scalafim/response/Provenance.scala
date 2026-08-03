package scalafim.response

opaque type SourceId = String

object SourceId:
  def fromString(value: String): Either[IdentityError, SourceId] =
    ResponseIdentity.validate("source id", value)

  def unsafe(value: String): SourceId =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: SourceId)
    inline def value: String =
      id

opaque type ProvenanceId = String

object ProvenanceId:
  def fromString(value: String): Either[IdentityError, ProvenanceId] =
    ResponseIdentity.validate("provenance id", value)

  def unsafe(value: String): ProvenanceId =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ProvenanceId)
    inline def value: String =
      id

opaque type OperationId = String

object OperationId:
  def fromString(value: String): Either[IdentityError, OperationId] =
    ResponseIdentity.validate("operation id", value)

  def unsafe(value: String): OperationId =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: OperationId)
    inline def value: String =
      id

enum ProvenanceOperation:
  case SourceRead(source: SourceId)
  case Selection
  case Assembly
  case Adapter(adapter: OperationId)
  case Derived(operation: OperationId)

enum ProvenanceEvidence:
  case Domain(reference: DomainReference)
  case External(reference: DomainReference)
  case NoneDeclared

final case class ProvenanceNode(
    id: ProvenanceId,
    operation: ProvenanceOperation,
    parents: Vector[ProvenanceId],
    evidence: Vector[ProvenanceEvidence]
)

final class Provenance private (
    val nodes: Vector[ProvenanceNode],
    val roots: Vector[ProvenanceId]
):
  private val byId: Map[String, ProvenanceNode] =
    nodes.iterator.map(node => node.id.value -> node).toMap

  def node(id: ProvenanceId): Option[ProvenanceNode] =
    byId.get(id.value)

object Provenance:
  def source(
      nodeId: ProvenanceId,
      source: SourceId,
      evidence: Vector[ProvenanceEvidence] = Vector(ProvenanceEvidence.NoneDeclared)
  ): Provenance =
    new Provenance(
      Vector(ProvenanceNode(nodeId, ProvenanceOperation.SourceRead(source), Vector.empty, evidence)),
      Vector(nodeId)
    )

  def make(
      nodes: Vector[ProvenanceNode],
      roots: Vector[ProvenanceId]
  ): Either[ProvenanceError, Provenance] =
    if nodes.isEmpty || roots.isEmpty then Left(ProvenanceError.Empty)
    else
      val available = scala.collection.mutable.HashSet.empty[String]
      var nodeIndex = 0
      while nodeIndex < nodes.length do
        val node = nodes(nodeIndex)
        if available.contains(node.id.value) then
          return Left(ProvenanceError.DuplicateNode(node.id))
        var parentIndex = 0
        while parentIndex < node.parents.length do
          val parent = node.parents(parentIndex)
          if !available.contains(parent.value) then
            return Left(ProvenanceError.MissingParent(node.id, parent))
          parentIndex += 1
        available += node.id.value
        nodeIndex += 1

      val seenRoots = scala.collection.mutable.HashSet.empty[String]
      var rootIndex = 0
      while rootIndex < roots.length do
        val root = roots(rootIndex)
        if !available.contains(root.value) then
          return Left(ProvenanceError.InvalidRoot(root))
        if seenRoots.contains(root.value) then
          return Left(ProvenanceError.DuplicateRoot(root))
        seenRoots += root.value
        rootIndex += 1
      Right(new Provenance(nodes, roots))

  def derive(
      parent: Provenance,
      nodeId: ProvenanceId,
      operation: ProvenanceOperation,
      evidence: Vector[ProvenanceEvidence] =
        Vector(ProvenanceEvidence.NoneDeclared)
  ): Either[ProvenanceError, Provenance] =
    make(
      parent.nodes :+ ProvenanceNode(
        nodeId,
        operation,
        parent.roots,
        evidence
      ),
      Vector(nodeId)
    )
