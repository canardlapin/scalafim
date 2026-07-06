package scalafim.dataset

enum IndexSelection:
  case All
  case Indices(values: Vector[Int])

  def resolve(size: Int, axis: String): Vector[Int] =
    require(size > 0, s"$axis size must be positive")
    this match
      case IndexSelection.All => (0 until size).toVector
      case IndexSelection.Indices(values) =>
        require(values.nonEmpty, s"$axis selection must be non-empty")
        require(values.distinct.length == values.length, s"$axis selection must not contain duplicate indices")
        values.foreach { i =>
          require(i >= 0 && i < size, s"$axis index $i out of bounds for size $size")
        }
        values

object IndexSelection:
  def indices(values: Int*): IndexSelection =
    IndexSelection.Indices(values.toVector)

final case class DataSelection(
    time: IndexSelection = IndexSelection.All,
    voxels: IndexSelection = IndexSelection.All
):
  def resolve(shape: DatasetShape): ResolvedDataSelection =
    ResolvedDataSelection(
      timepoints = time.resolve(shape.timepoints, "time"),
      voxels = voxels.resolve(shape.spatialSize, "voxel")
    )

object DataSelection:
  val All: DataSelection = DataSelection()

final case class ResolvedDataSelection(timepoints: Vector[Int], voxels: Vector[Int]):
  require(timepoints.nonEmpty, "time selection must be non-empty")
  require(voxels.nonEmpty, "voxel selection must be non-empty")
