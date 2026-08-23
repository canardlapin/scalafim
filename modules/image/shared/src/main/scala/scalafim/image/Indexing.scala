package scalafim.image

object Indexing:

  def gridToIndex(dims: SpatialDims, coord: VoxelCoord): Int =
    gridToIndexChecked(dims, coord).fold(err => throw new IllegalArgumentException(err.message), identity)

  def gridToIndexChecked(dims: SpatialDims, coord: VoxelCoord): Either[GeometryError, Int] =
    if coord.x < 0 || coord.x >= dims.x ||
       coord.y < 0 || coord.y >= dims.y ||
       coord.z < 0 || coord.z >= dims.z then
      Left(GeometryError.VoxelOutOfBounds(coord, dims))
    else
      Right((coord.x * dims.y + coord.y) * dims.z + coord.z)

  def gridToIndex(dims: Vector[Int], coords: Vector[Int]): Int =
    require(dims.nonEmpty, "dims must be non-empty")
    require(coords.length == dims.length, s"expected ${dims.length} coords")
    var lin = 0
    var d = 0
    while d < dims.length do
      val c = coords(d)
      val dim = dims(d)
      require(c >= 0 && c < dim, s"coord $c out of bounds for dim $d")
      lin = lin * dim + c
      d += 1
    lin

  def indexToGrid(dims: SpatialDims, idx: Int): VoxelCoord =
    indexToGridChecked(dims, idx).fold(err => throw new IllegalArgumentException(err.message), identity)

  def indexToGridChecked(dims: SpatialDims, idx: Int): Either[GeometryError, VoxelCoord] =
    val size = dims.product
    if idx < 0 || idx >= size then Left(GeometryError.LinearIndexOutOfBounds(idx, size))
    else
      val z = idx % dims.z
      val xy = idx / dims.z
      val y = xy % dims.y
      val x = xy / dims.y
      Right(VoxelCoord(x, y, z))

  def indexToGrid(dims: Vector[Int], idx: Int): Vector[Int] =
    require(dims.nonEmpty, "dims must be non-empty")
    require(idx >= 0 && idx < dims.product, "index out of bounds")
    var rem = idx
    val out = Array.ofDim[Int](dims.length)
    var d = dims.length - 1
    while d >= 0 do
      val dim = dims(d)
      out(d) = rem % dim
      rem = rem / dim
      d -= 1
    out.toVector

  inline def gridToIndex3D(dims: SpatialDims, coord: VoxelCoord): Int =
    gridToIndex(dims, coord)

  inline def gridToIndex3D(dims: SpatialDims, x: Int, y: Int, z: Int): Int =
    gridToIndex(dims, VoxelCoord(x, y, z))

  def gridToIndex3D(dims: Vector[Int], x: Int, y: Int, z: Int): Int =
    gridToIndex3D(SpatialDims.unsafeFromVector(dims.take(3)), x, y, z)

  inline def indexToGrid3D(dims: SpatialDims, idx: Int): VoxelCoord =
    indexToGrid(dims, idx)

  def indexToGrid3D(dims: Vector[Int], idx: Int): Vector[Int] =
    indexToGrid3D(SpatialDims.unsafeFromVector(dims.take(3)), idx).toVector
