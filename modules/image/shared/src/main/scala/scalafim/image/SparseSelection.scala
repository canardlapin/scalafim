package scalafim.image

enum MissingVoxelPolicy[+A]:
  case RequireCovered extends MissingVoxelPolicy[Nothing]
  case DropMissing extends MissingVoxelPolicy[Nothing]
  case Fill[A](value: A) extends MissingVoxelPolicy[A]

enum SparseSelectionError:
  case Grid(error: GridMismatch)
  case InvalidSelection(error: VoxelIndexSetError)
  case OutsideSupport(missing: VoxelRegion)

  def message: String =
    this match
      case Grid(error) =>
        error.message
      case InvalidSelection(error) =>
        error.message
      case OutsideSupport(missing) =>
        s"${missing.size} selected voxels are outside sparse support"
