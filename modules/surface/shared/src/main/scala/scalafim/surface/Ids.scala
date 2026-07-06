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
