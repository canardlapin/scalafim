package scalafim.image

import ravel.NDArray as RavelArray

import scala.reflect.ClassTag
import ravel.DType

class SliceSamplingSuite extends munit.FunSuite:

  private val Tol = 1e-9

  private def volume[A: ClassTag: DType: MigrationValueSemantics](
    dims: SpatialDims,
    affine: Option[DMat] = None,
    label: String = "test"
  )(f: (Int, Int, Int) => A): NeuroVol[A] =
    val values = PrimitiveBuffers.tabulate[A](dims.product) { index =>
      val x = index % dims.x
      val y = (index / dims.x) % dims.y
      val z = index / (dims.x * dims.y)
      f(x, y, z)
    }
    NeuroVol.fromLinear(values, NeuroSpace(dims.toVector, trans = affine), label)

  private def gridAt(
    space: VolumeSpace,
    anatomicalPlane: AnatomicalPlane,
    cursor: WorldPoint,
    convention: LeftRightConvention = LeftRightConvention.PatientLeftOnLeft
  ): SliceGrid =
    SliceGrid.covering(
      space,
      SlicePlane.canonical(anatomicalPlane, cursor, convention),
      PixelSpacing(1.0, 1.0)
    )

  test("linear slice sampling exactly reproduces an analytic affine field") {
    val dims = SpatialDims(10, 10, 10)
    val source = volume[Double](dims) { (x, y, z) =>
      2.0 * x + 3.0 * y - 4.0 * z + 7.0
    }
    val grid = gridAt(source.volumeSpace, AnatomicalPlane.Axial, WorldPoint(0.0, 0.0, 4.25))
    val sampled = SlicePlan.make(source.volumeSpace, grid).sample(source, SliceSampling.Linear()).toOption.get

    var row = 0
    while row < sampled.dimensions.height do
      var column = 0
      while column < sampled.dimensions.width do
        val voxel = source.volumeSpace.worldToVoxel(grid.unsafeWorldAt(column, row))
        val expected = 2.0 * voxel.x + 3.0 * voxel.y - 4.0 * voxel.z + 7.0
        assertEqualsDouble(sampled(column, row), expected, Tol)
        column += 1
      row += 1
  }

  test("nearest sampling preserves label values and the top-to-bottom row contract") {
    val dims = SpatialDims(10, 10, 10)
    val labels = volume[Int](dims) { (x, y, z) => x + 10 * y + 100 * z }
    val grid = gridAt(labels.volumeSpace, AnatomicalPlane.Axial, WorldPoint(0.0, 0.0, 4.4))
    val sampled = SlicePlan.make(labels.volumeSpace, grid)
      .sample(labels, SliceSampling.Nearest(-1))
      .toOption
      .get

    assertEquals(sampled(0, 0), 490)
    assertEquals(sampled(9, 0), 499)
    assertEquals(sampled(0, 9), 400)
  }

  test("cubic sampling shares the kernel contract and reproduces a linear field") {
    val dims = SpatialDims(10, 10, 10)
    val source = volume[Double](dims) { (_, _, z) => 5.0 + 2.5 * z }
    val grid = gridAt(source.volumeSpace, AnatomicalPlane.Axial, WorldPoint(0.0, 0.0, 4.25))
    val sampled = SlicePlan.make(source.volumeSpace, grid).sample(source, SliceSampling.Cubic()).toOption.get

    var i = 0
    while i < sampled.values.length do
      assertEqualsDouble(sampled.values(i), 15.625, Tol)
      i += 1
  }

  test("outside values participate explicitly in interpolation") {
    val dims = SpatialDims(4, 4, 4)
    val source = volume[Double](dims)((_, _, _) => 8.0)
    val nearestGrid = gridAt(source.volumeSpace, AnatomicalPlane.Axial, WorldPoint(0.0, 0.0, -2.0))
    val nearest = SlicePlan.make(source.volumeSpace, nearestGrid)
      .sample(source, SliceSampling.Nearest(-3.0))
      .toOption
      .get
    assert(Vector.tabulate(nearest.values.size)(nearest.values(_)).forall(_ == -3.0))

    val linearGrid = gridAt(source.volumeSpace, AnatomicalPlane.Axial, WorldPoint(0.0, 0.0, -0.25))
    val linear = SlicePlan.make(source.volumeSpace, linearGrid)
      .sample(source, SliceSampling.Linear(0.0))
      .toOption
      .get
    assertEqualsDouble(linear(1, 1), 6.0, Tol)
  }

  test("validated sampling kernels agree with checked voxel access at every boundary") {
    val dims = SpatialDims(4, 3, 5)
    val source = volume[Double](dims) { (x, y, z) =>
      x.toDouble + 10.0 * y.toDouble + 100.0 * z.toDouble
    }
    val outside = -1234.5

    var z = -1
    while z <= dims.z do
      var y = -1
      while y <= dims.y do
        var x = -1
        while x <= dims.x do
          val expected =
            if x >= 0 && x < dims.x && y >= 0 && y < dims.y && z >= 0 && z < dims.z then
              source(x, y, z)
            else outside
          assertEqualsDouble(
            VoxelSamplingKernel.valueOrOutside(source, dims, x, y, z, outside),
            expected,
            0.0
          )
          assertEqualsDouble(
            VoxelSamplingKernel.nearest(source, dims, x.toDouble, y.toDouble, z.toDouble, outside),
            expected,
            0.0
          )
          x += 1
        y += 1
      z += 1
  }

  test("display mirroring reverses pixels without changing sampled anatomy") {
    val dims = SpatialDims(6, 5, 4)
    val source = volume[Double](dims) { (x, y, z) => x + 10.0 * y + 100.0 * z }
    val cursor = WorldPoint(0.0, 0.0, 2.0)
    val leftGrid = gridAt(
      source.volumeSpace,
      AnatomicalPlane.Axial,
      cursor,
      LeftRightConvention.PatientLeftOnLeft
    )
    val rightGrid = gridAt(
      source.volumeSpace,
      AnatomicalPlane.Axial,
      cursor,
      LeftRightConvention.PatientRightOnLeft
    )
    val left = SlicePlan.make(source.volumeSpace, leftGrid)
      .sample(source, SliceSampling.Nearest(-1.0))
      .toOption
      .get
    val right = SlicePlan.make(source.volumeSpace, rightGrid)
      .sample(source, SliceSampling.Nearest(-1.0))
      .toOption
      .get

    var row = 0
    while row < left.dimensions.height do
      var column = 0
      while column < left.dimensions.width do
        assertEqualsDouble(
          left(column, row),
          right(right.dimensions.width - 1 - column, row),
          Tol
        )
        column += 1
      row += 1
  }

  test("affine stepping agrees with direct world-to-voxel conversion") {
    val affine = DMat.fromRows(
      Vector(
        Vector(0.0, -2.0, 0.25, 5.0),
        Vector(1.5, 0.0, 0.10, -4.0),
        Vector(0.0, 0.0, 3.0, 12.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )
    val source = volume[Double](SpatialDims(8, 7, 6), Some(affine)) { (x, y, z) =>
      x + 10.0 * y + 100.0 * z
    }
    val cursor = source.volumeSpace.voxelToWorld(VoxelPoint(3.5, 3.0, 2.5))
    val grid = SliceGrid.covering(
      source.volumeSpace,
      SlicePlane.canonical(AnatomicalPlane.Coronal, cursor),
      PixelSpacing(0.7, 0.9)
    )
    val plan = SlicePlan.make(source.volumeSpace, grid)
    val pixels = Vector(
      PixelCoord(0, 0),
      PixelCoord(grid.dimensions.width / 2, grid.dimensions.height / 2),
      PixelCoord(grid.dimensions.width - 1, grid.dimensions.height - 1)
    )

    pixels.foreach { pixel =>
      val direct = source.volumeSpace.worldToVoxel(grid.worldAt(pixel).toOption.get)
      val planned = plan.sourceVoxelAt(pixel).toOption.get
      assertEqualsDouble(planned.x, direct.x, Tol)
      assertEqualsDouble(planned.y, direct.y, Tol)
      assertEqualsDouble(planned.z, direct.z, Tol)
    }
  }

  test("plans reject a volume from a different source space") {
    val dims = SpatialDims(3, 3, 3)
    val source = volume[Double](dims)((x, y, z) => x + y + z)
    val shifted = volume[Double](
      dims,
      Some(
        DMat.fromRows(
          Vector(
            Vector(1.0, 0.0, 0.0, 10.0),
            Vector(0.0, 1.0, 0.0, 0.0),
            Vector(0.0, 0.0, 1.0, 0.0),
            Vector(0.0, 0.0, 0.0, 1.0)
          )
        )
      )
    )((x, y, z) => x + y + z)
    val grid = gridAt(source.volumeSpace, AnatomicalPlane.Axial, WorldPoint(0.0, 0.0, 1.0))
    val result = SlicePlan.make(source.volumeSpace, grid).sample(shifted, SliceSampling.Linear())

    assert(result.isLeft)
  }

  test("mapped slice plans materialize nonlinear pullback coordinates once") {
    val dims = SpatialDims(5, 3, 1)
    val source = volume[Double](dims) { (x, _, _) => x.toDouble }
    val grid = gridAt(source.volumeSpace, AnatomicalPlane.Axial, WorldPoint(0.0, 0.0, 0.0))
    val fieldGrid = GridSpec.fromVolumeSpace(source.volumeSpace)
    val field =
      RavelArray.tabulate[Double](
        fieldGrid.shape.x,
        fieldGrid.shape.y,
        fieldGrid.shape.z,
        3
      ) { (x, _, _, component) =>
        if component == 0 && x >= 2 then 1.0 else 0.0
      }
    val mapping = DenseFieldMorphism.displacement(
      SpatialDomainId("source"),
      SpatialDomainId("reference"),
      fieldGrid,
      field,
      Resample.Method.Nearest
    ).toOption.get
    val plan = MappedSlicePlan.make(source.volumeSpace, grid, mapping)
    val sampled = plan.sample(source, SliceSampling.Nearest(-1.0)).toOption.get

    assertEqualsDouble(sampled(0, 1), 0.0, Tol)
    assertEqualsDouble(sampled(1, 1), 1.0, Tol)
    assertEqualsDouble(sampled(2, 1), 3.0, Tol)
    assertEqualsDouble(sampled(3, 1), 4.0, Tol)
    assertEqualsDouble(sampled(4, 1), -1.0, Tol)
    assertEquals(plan.sourceVoxelAt(PixelCoord(2, 1)).toOption.get, VoxelPoint(3.0, 1.0, 0.0))
    assertEquals(plan.mappingBatchCount, grid.dimensions.height)
    assert(plan.mappingBatchCount < grid.dimensions.pixelCount)
  }

  test("primitive dense-field slice mapping agrees with the typed morphism oracle") {
    val dims = SpatialDims(6, 5, 4)
    val affine = DMat.fromRows(
      Vector(
        Vector(1.5, 0.10, 0.00, 4.0),
        Vector(0.0, 2.00, 0.15, -3.0),
        Vector(0.0, 0.00, 2.50, 7.0),
        Vector(0.0, 0.00, 0.00, 1.0)
      )
    )
    val source = volume[Double](dims, Some(affine)) { (x, y, z) =>
      x.toDouble + 10.0 * y.toDouble + 100.0 * z.toDouble
    }
    val cursor = source.volumeSpace.voxelToWorld(VoxelPoint(2.4, 2.1, 1.6))
    val grid = SliceGrid.covering(
      source.volumeSpace,
      SlicePlane.canonical(AnatomicalPlane.Coronal, cursor),
      PixelSpacing(0.8, 0.9)
    )
    val fieldGrid = GridSpec.fromVolumeSpace(source.volumeSpace)
    val displacement =
      RavelArray.tabulate[Double](
        fieldGrid.shape.x,
        fieldGrid.shape.y,
        fieldGrid.shape.z,
        3
      ) { (x, y, z, component) =>
        component match
          case 0 => 0.10 * y.toDouble
          case 1 => -0.05 * x.toDouble
          case _ => 0.08 * z.toDouble
      }
    val absolute =
      RavelArray.tabulate[Double](
        fieldGrid.shape.x,
        fieldGrid.shape.y,
        fieldGrid.shape.z,
        3
      ) { (x, y, z, component) =>
        val world =
          fieldGrid
            .voxelToWorld(VoxelPoint(x.toDouble, y.toDouble, z.toDouble))
            .toOption
            .get
        component match
          case 0 => world.x + 0.10 * y.toDouble
          case 1 => world.y - 0.05 * x.toDouble
          case _ => world.z + 0.08 * z.toDouble
      }
    val domain = SpatialDomainId("source")
    val reference = SpatialDomainId("reference")
    val mappings = Vector(
      DenseFieldMorphism.displacement(domain, reference, fieldGrid, displacement, Resample.Method.Nearest).toOption.get,
      DenseFieldMorphism.displacement(domain, reference, fieldGrid, displacement, Resample.Method.Linear).toOption.get,
      DenseFieldMorphism.displacement(domain, reference, fieldGrid, displacement, Resample.Method.Cubic).toOption.get,
      DenseFieldMorphism.coordinates(domain, reference, fieldGrid, absolute, Resample.Method.Nearest).toOption.get,
      DenseFieldMorphism.coordinates(domain, reference, fieldGrid, absolute, Resample.Method.Linear).toOption.get,
      DenseFieldMorphism.coordinates(domain, reference, fieldGrid, absolute, Resample.Method.Cubic).toOption.get
    )

    mappings.foreach { mapping =>
      val plan = MappedSlicePlan.make(source.volumeSpace, grid, mapping)
      var row = 0
      while row < grid.dimensions.height do
        var column = 0
        while column < grid.dimensions.width do
          val pixel = PixelCoord(column, row)
          val referenceWorld = grid.worldAt(pixel).toOption.get
          val expected = source.volumeSpace.worldToVoxel(mapping.transform(referenceWorld))
          val actual = plan.sourceVoxelAt(pixel).toOption.get
          assertEqualsDouble(actual.x, expected.x, Tol)
          assertEqualsDouble(actual.y, expected.y, Tol)
          assertEqualsDouble(actual.z, expected.z, Tol)
          column += 1
        row += 1
    }
  }
