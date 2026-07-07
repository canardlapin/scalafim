package scalafim.surface

opaque type VertexId = Int

object VertexId:
  def apply(index: Int): VertexId =
    require(index >= 0, "vertex id must be non-negative")
    index

  private[surface] def unsafe(index: Int): VertexId =
    index

  extension (id: VertexId)
    def index: Int = id

opaque type FaceId = Int

object FaceId:
  def apply(index: Int): FaceId =
    require(index >= 0, "face id must be non-negative")
    index

  private[surface] def unsafe(index: Int): FaceId =
    index

  extension (id: FaceId)
    def index: Int = id

opaque type ParcelLabel = Int

object ParcelLabel:
  def apply(value: Int): ParcelLabel =
    require(value >= 0, "parcel label must be non-negative")
    value

  private[surface] def unsafe(value: Int): ParcelLabel =
    value

  extension (label: ParcelLabel)
    def value: Int = label

opaque type ParcelPart = Int

object ParcelPart:
  def apply(value: Int): ParcelPart =
    require(value > 0, "parcel part must be positive")
    value

  private[surface] def unsafe(value: Int): ParcelPart =
    value

  extension (part: ParcelPart)
    def value: Int = part
