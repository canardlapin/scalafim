package scalafim.pipeline

final class ArtifactKind[A] private (val label: String):
  require(label.nonEmpty, "artifact kind label must be non-empty")

  override def equals(other: Any): Boolean =
    other match
      case that: ArtifactKind[?] => label == that.label
      case _ => false

  override def hashCode(): Int =
    label.hashCode

  override def toString: String =
    label

object ArtifactKind:
  def apply[A](label: String): Either[PipelineError, ArtifactKind[A]] =
    Identifier.validate("artifact kind", label).map(new ArtifactKind[A](_))

  def unsafe[A](label: String): ArtifactKind[A] =
    apply[A](label).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class ArtifactRef[A] private[pipeline] (
    nodeId: NodeId,
    kind: ArtifactKind[A],
    label: Option[String]
):
  def expr: PipelineExpr[A] =
    PipelineExpr.ref(this)

  def displayName: String =
    label.getOrElse(nodeId.value)

private[pipeline] final case class StoredArtifact(kind: ArtifactKind[?], value: Any)

final case class ArtifactTable private[pipeline] (
    private val values: Map[NodeId, StoredArtifact]
):
  def contains(ref: ArtifactRef[?]): Boolean =
    values.contains(ref.nodeId)

  def nodeIds: Set[NodeId] =
    values.keySet

  def get[A](ref: ArtifactRef[A]): Either[PipelineError, A] =
    values.get(ref.nodeId) match
      case None =>
        Left(PipelineError.MissingArtifact(ref.nodeId))
      case Some(stored) =>
        if stored.kind == ref.kind then Right(stored.value.asInstanceOf[A])
        else Left(PipelineError.ArtifactKindMismatch(ref.nodeId, ref.kind.label, stored.kind.label))

  private[pipeline] def put[A](ref: ArtifactRef[A], value: A): ArtifactTable =
    ArtifactTable(values.updated(ref.nodeId, StoredArtifact(ref.kind, value)))

  private[pipeline] def putStored(ref: ArtifactRef[?], stored: StoredArtifact): Either[PipelineError, ArtifactTable] =
    if stored.kind == ref.kind then Right(ArtifactTable(values.updated(ref.nodeId, stored)))
    else Left(PipelineError.ArtifactKindMismatch(ref.nodeId, ref.kind.label, stored.kind.label))

object ArtifactTable:
  val empty: ArtifactTable =
    ArtifactTable(Map.empty)

final case class RunContext private (
    private[pipeline] val inputs: Map[NodeId, StoredArtifact],
    metadata: Map[String, String]
):
  def withInput[A](ref: ArtifactRef[A], value: A): RunContext =
    copy(inputs = inputs.updated(ref.nodeId, StoredArtifact(ref.kind, value)))

  def withMetadata(key: String, value: String): RunContext =
    copy(metadata = metadata.updated(key, value))

  private[pipeline] def input[A](ref: ArtifactRef[A]): Either[PipelineError, A] =
    inputs.get(ref.nodeId) match
      case None =>
        Left(PipelineError.MissingPipelineInput(ref.nodeId))
      case Some(stored) =>
        if stored.kind == ref.kind then Right(stored.value.asInstanceOf[A])
        else Left(PipelineError.ArtifactKindMismatch(ref.nodeId, ref.kind.label, stored.kind.label))

object RunContext:
  val empty: RunContext =
    RunContext(Map.empty, Map.empty)
