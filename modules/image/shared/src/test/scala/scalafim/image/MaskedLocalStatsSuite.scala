package scalafim.image

class MaskedLocalStatsSuite extends munit.FunSuite:
  test("integral-window normalization matches an independent masked loop"):
    val grid = GridSpec.identity(Vector(7, 6, 5))
    val source = NArrayUtil.ofSize[Double](grid.nVoxels)
    val mask = NArrayUtil.ofSize[Boolean](grid.nVoxels)
    var z = 0
    while z < grid.shape.z do
      var y = 0
      while y < grid.shape.y do
        var x = 0
        while x < grid.shape.x do
          val index = x + grid.shape.x * y + grid.shape.x * grid.shape.y * z
          source(index) = 0.3 * x - 0.2 * y + 0.4 * z + math.sin(0.17 * index)
          mask(index) = !(x == 2 && y == 3 && z == 2)
          x += 1
        y += 1
      z += 1

    val values = NArrayUtil.ofSize[Double](grid.nVoxels)
    val valid = NArrayUtil.ofSize[Boolean](grid.nVoxels)
    val workspace = MaskedLocalStatsWorkspace(grid)
    val summary = MaskedLocalStats.normalizeChannelsInto(
      source,
      FieldValidity.Mask(mask),
      grid,
      Vector(VoxelWindowRadius(1, 1, 1)),
      Vector(0.2),
      minimumValidFraction = 0.8,
      values,
      valid,
      workspace
    )
    val index = 3 + grid.shape.x * 3 + grid.shape.x * grid.shape.y * 2
    val expected = naiveNormalized(source, mask, grid, 3, 3, 2, 1, 0.2)
    assert(valid(index))
    assertEqualsDouble(values(index), expected, 1e-12)
    assertEquals(summary.validPerChannel.head, countTrue(valid))
    assertEquals(workspace.ownedScalarBuffers, 3)

  test("minimum nominal window fraction rejects undersupported boundaries"):
    val grid = GridSpec.identity(Vector(5, 5, 5))
    val source = NArrayUtil.ofSize[Double](grid.nVoxels)
    var index = 0
    while index < source.length do
      source(index) = 7.0
      index += 1
    val values = NArrayUtil.ofSize[Double](grid.nVoxels)
    val valid = NArrayUtil.ofSize[Boolean](grid.nVoxels)
    MaskedLocalStats.normalizeChannelsInto(
      source,
      FieldValidity.All,
      grid,
      Vector(VoxelWindowRadius(1, 1, 1)),
      Vector(0.1),
      minimumValidFraction = 0.8,
      values,
      valid,
      MaskedLocalStatsWorkspace(grid)
    )
    assert(!valid(0))
    val center = 2 + 5 * 2 + 25 * 2
    assert(valid(center))
    assertEqualsDouble(values(center), 0.0, 1e-12)

  test("physical channel gradients recover an analytic world covector on an oblique grid"):
    val affine = DMat.fromRows(
      Vector(
        Vector(2.0, 0.3, 0.0, 10.0),
        Vector(0.0, 1.5, 0.2, -4.0),
        Vector(0.1, 0.0, 2.5, 3.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )
    val grid = GridSpec(Vector(6, 6, 6), affine)
    val values = NArrayUtil.ofSize[Double](grid.nVoxels)
    var z = 0
    while z < 6 do
      var y = 0
      while y < 6 do
        var x = 0
        while x < 6 do
          val index = x + 6 * y + 36 * z
          val world = grid.voxelToWorld(SpatialPoint(x.toDouble, y.toDouble, z.toDouble))
          values(index) = 2.0 * world.x - 3.0 * world.y + 0.5 * world.z + 1.0
          x += 1
        y += 1
      z += 1
    val sourceValid = NArrayUtil.ofSize[Boolean](grid.nVoxels)
    var index = 0
    while index < sourceValid.length do
      sourceValid(index) = true
      index += 1
    val gradients = NArrayUtil.ofSize[Double](grid.nVoxels * 3)
    val gradientValid = NArrayUtil.ofSize[Boolean](grid.nVoxels)
    MaskedLocalStats.physicalGradientChannelsInto(
      values,
      sourceValid,
      grid,
      1,
      gradients,
      gradientValid,
      MaskedLocalStatsWorkspace(grid)
    )
    val center = 3 + 6 * 3 + 36 * 3
    assert(gradientValid(center))
    assertEqualsDouble(gradients(center), 2.0, 1e-12)
    assertEqualsDouble(gradients(center + grid.nVoxels), -3.0, 1e-12)
    assertEqualsDouble(gradients(center + 2 * grid.nVoxels), 0.5, 1e-12)

  private def naiveNormalized(
      source: narr.NArray[Double],
      mask: narr.NArray[Boolean],
      grid: GridSpec,
      cx: Int,
      cy: Int,
      cz: Int,
      radius: Int,
      epsilon: Double
  ): Double =
    var sum = 0.0
    var sumSquares = 0.0
    var count = 0
    var z = cz - radius
    while z <= cz + radius do
      var y = cy - radius
      while y <= cy + radius do
        var x = cx - radius
        while x <= cx + radius do
          val index = x + grid.shape.x * y + grid.shape.x * grid.shape.y * z
          if mask(index) then
            sum += source(index)
            sumSquares += source(index) * source(index)
            count += 1
          x += 1
        y += 1
      z += 1
    val mean = sum / count.toDouble
    val variance = math.max(0.0, sumSquares / count.toDouble - mean * mean)
    val center = cx + grid.shape.x * cy + grid.shape.x * grid.shape.y * cz
    (source(center) - mean) / math.sqrt(variance + epsilon * epsilon)

  private def countTrue(values: narr.NArray[Boolean]): Int =
    var count = 0
    var index = 0
    while index < values.length do
      if values(index) then count += 1
      index += 1
    count
