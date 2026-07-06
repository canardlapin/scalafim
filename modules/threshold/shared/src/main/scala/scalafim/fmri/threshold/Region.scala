package scalafim.fmri.threshold

final case class BoundingBox(x0: Int, x1: Int, y0: Int, y1: Int, z0: Int, z1: Int):
  require(x0 <= x1 && y0 <= y1 && z0 <= z1, "bounding-box minima must be <= maxima")

  def isSingleton: Boolean =
    x0 == x1 && y0 == y1 && z0 == z1

  def midX: Int = (x0 + x1) / 2
  def midY: Int = (y0 + y1) / 2
  def midZ: Int = (z0 + z1) / 2

object BoundingBox:
  def fromIndices(indices: Array[Int], field: MaskedField): Either[ThresholdError, BoundingBox] =
    if indices.isEmpty then return Left(ThresholdError.EmptyRegion)
    var i = 0
    while i < indices.length do
      val idx = indices(i)
      if idx < 0 || idx >= field.size then return Left(ThresholdError.IndexOutOfBounds(idx, field.size))
      i += 1

    var xmin = Int.MaxValue
    var xmax = Int.MinValue
    var ymin = Int.MaxValue
    var ymax = Int.MinValue
    var zmin = Int.MaxValue
    var zmax = Int.MinValue
    i = 0
    while i < indices.length do
      val idx = indices(i)
      val (xx, yy, zz) = field.coord(idx)
      if xx < xmin then xmin = xx
      if xx > xmax then xmax = xx
      if yy < ymin then ymin = yy
      if yy > ymax then ymax = yy
      if zz < zmin then zmin = zz
      if zz > zmax then zmax = zz
      i += 1
    Right(BoundingBox(xmin, xmax, ymin, ymax, zmin, zmax))

final class Region private (
    val id: Int,
    private[threshold] val indexArray: Array[Int],
    val bbox: BoundingBox,
    val priorMass: Double
):
  require(indexArray.nonEmpty, "region must be non-empty")
  require(priorMass.isFinite && priorMass >= 0.0, "prior mass must be finite and non-negative")

  def size: Int = indexArray.length

  def indices: Array[Int] =
    indexArray.clone

  def indicesVector: Vector[Int] =
    indexArray.toVector

object Region:
  def fromIndices(
    id: Int,
    indices: Array[Int],
    field: MaskedField,
    priors: PriorWeights
  ): Either[ThresholdError, Region] =
    if field.size != priors.length then
      return Left(ThresholdError.ShapeMismatch("field/priors", field.size.toString, priors.length.toString))
    if indices.isEmpty then return Left(ThresholdError.EmptyRegion)

    var mass = 0.0
    var i = 0
    while i < indices.length do
      val idx = indices(i)
      if idx < 0 || idx >= field.size then return Left(ThresholdError.IndexOutOfBounds(idx, field.size))
      mass += priors(idx)
      i += 1

    BoundingBox.fromIndices(indices, field).map { bbox =>
      unsafe(id, indices, bbox, mass)
    }

  private[threshold] def unsafe(
    id: Int,
    indices: Array[Int],
    bbox: BoundingBox,
    priorMass: Double
  ): Region =
    new Region(id, indices.clone, bbox, priorMass)
