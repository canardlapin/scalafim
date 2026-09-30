package scalafim.transform

/** A file or other external asset a transform was read from. `sha256` is recorded when the bytes were seen. */
final case class AssetRef(label: String, sha256: Option[String]) derives CanEqual

/** Where a world transform came from: one step per asset or operation, in application order. */
final case class TransformProvenance(steps: Vector[TransformProvenance.Step]) derives CanEqual:
  def andThen(next: TransformProvenance): TransformProvenance =
    TransformProvenance(steps ++ next.steps)

  def describe: String =
    if steps.isEmpty then "unrecorded" else steps.map(_.describe).mkString(" then ")

object TransformProvenance:
  enum Step derives CanEqual:
    /** Read from a toolkit file of the given format. */
    case Read(format: TransformFormat, asset: AssetRef)

    /** Built in code, e.g. a known template-to-template affine. */
    case Constructed(description: String)

    /** Derived from other steps: an inversion, a fused composition, a numerical estimate. */
    case Derived(operation: String)

    def describe: String =
      this match
        case Read(format, asset)       => s"$format ${asset.label}"
        case Constructed(description)  => description
        case Derived(operation)        => operation

  val empty: TransformProvenance = TransformProvenance(Vector.empty)

  def read(format: TransformFormat, asset: AssetRef): TransformProvenance =
    TransformProvenance(Vector(Step.Read(format, asset)))

  def constructed(description: String): TransformProvenance =
    TransformProvenance(Vector(Step.Constructed(description)))
