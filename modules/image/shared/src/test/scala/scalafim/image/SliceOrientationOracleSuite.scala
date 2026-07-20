package scalafim.image

import scala.reflect.ClassTag
import scala.util.Random

class SliceOrientationOracleSuite extends munit.FunSuite:

  private val Tol = 1e-8
  private val Dims = SpatialDims(2, 3, 4)

  private final case class SignedPermutation(
    worldAxisForVoxel: Vector[Int],
    signs: Vector[Int]
  ):
    require(worldAxisForVoxel.sorted == Vector(0, 1, 2))
    require(signs.forall(sign => sign == -1 || sign == 1))

    def affine(dims: SpatialDims): DMat =
      val sizes = dims.toVector
      DMat.fromRows(
        (0 until 3).map { worldAxis =>
          val voxelAxis = worldAxisForVoxel.indexOf(worldAxis)
          val sign = signs(voxelAxis)
          val offset = if sign < 0 then sizes(voxelAxis).toDouble - 1.0 else 0.0
          Vector.tabulate(4) { column =>
            if column == voxelAxis then sign.toDouble
            else if column == 3 then offset
            else 0.0
          }
        }.toVector :+ Vector(0.0, 0.0, 0.0, 1.0)
      )

    def worldDimensions(dims: SpatialDims): Vector[Int] =
      val sizes = dims.toVector
      Vector.tabulate(3)(worldAxis => sizes(worldAxisForVoxel.indexOf(worldAxis)))

    def storageAt(world: Vector[Int], dims: SpatialDims): Vector[Int] =
      val sizes = dims.toVector
      Vector.tabulate(3) { voxelAxis =>
        val coordinate = world(worldAxisForVoxel(voxelAxis))
        if signs(voxelAxis) > 0 then coordinate else sizes(voxelAxis) - 1 - coordinate
      }

    def clue: String =
      s"permutation=$worldAxisForVoxel signs=$signs"

  private object SignedPermutation:
    val all: Vector[SignedPermutation] =
      val permutations = Vector(0, 1, 2).permutations.toVector
      val signs =
        for
          x <- Vector(-1, 1)
          y <- Vector(-1, 1)
          z <- Vector(-1, 1)
        yield Vector(x, y, z)
      for
        permutation <- permutations
        reflection <- signs
      yield SignedPermutation(permutation, reflection)

  private final case class AffineFixture(
    linear: Vector[Vector[Double]],
    offset: Vector[Double]
  ):
    require(linear.length == 3 && linear.forall(_.length == 3))
    require(offset.length == 3)

    def matrix: DMat =
      DMat.fromRows(
        Vector.tabulate(3) { row =>
          linear(row) :+ offset(row)
        } :+ Vector(0.0, 0.0, 0.0, 1.0)
      )

    def voxelToWorld(x: Double, y: Double, z: Double): WorldPoint =
      val voxel = Vector(x, y, z)
      WorldPoint(
        dot(linear(0), voxel) + offset(0),
        dot(linear(1), voxel) + offset(1),
        dot(linear(2), voxel) + offset(2)
      )

    /** Independent closed-form 3x3 inverse used only by the test oracle. */
    def worldToVoxel(point: WorldPoint): VoxelPoint =
      val a = linear(0)(0)
      val b = linear(0)(1)
      val c = linear(0)(2)
      val d = linear(1)(0)
      val e = linear(1)(1)
      val f = linear(1)(2)
      val g = linear(2)(0)
      val h = linear(2)(1)
      val i = linear(2)(2)
      val determinant =
        a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
      require(math.abs(determinant) > 1e-6)
      val x = point.x - offset(0)
      val y = point.y - offset(1)
      val z = point.z - offset(2)
      VoxelPoint(
        ((e * i - f * h) * x + (c * h - b * i) * y + (b * f - c * e) * z) / determinant,
        ((f * g - d * i) * x + (a * i - c * g) * y + (c * d - a * f) * z) / determinant,
        ((d * h - e * g) * x + (b * g - a * h) * y + (a * e - b * d) * z) / determinant
      )

  private def dot(left: Vector[Double], right: Vector[Double]): Double =
    left(0) * right(0) + left(1) * right(1) + left(2) * right(2)

  private def volume[A: ClassTag](
    dims: SpatialDims,
    affine: DMat
  )(value: (Int, Int, Int) => A): NeuroVol[A] =
    val values = NArrayUtil.tabulate[A](dims.product) { index =>
      val x = index % dims.x
      val y = (index / dims.x) % dims.y
      val z = index / (dims.x * dims.y)
      value(x, y, z)
    }
    NeuroVol.fromLinear(values, NeuroSpace(dims.toVector, trans = Some(affine)), "orientation-oracle")

  private def encoded(storage: Vector[Int]): Int =
    storage(0) + 10 * storage(1) + 100 * storage(2)

  private def expectedWorldAt(
    plane: AnatomicalPlane,
    convention: LeftRightConvention,
    fixed: Int,
    column: Int,
    row: Int,
    worldDimensions: Vector[Int]
  ): Vector[Int] =
    plane match
      case AnatomicalPlane.Axial =>
        val x =
          if convention == LeftRightConvention.PatientLeftOnLeft then column
          else worldDimensions(0) - 1 - column
        Vector(x, worldDimensions(1) - 1 - row, fixed)
      case AnatomicalPlane.Coronal =>
        val x =
          if convention == LeftRightConvention.PatientLeftOnLeft then column
          else worldDimensions(0) - 1 - column
        Vector(x, fixed, worldDimensions(2) - 1 - row)
      case AnatomicalPlane.Sagittal =>
        Vector(fixed, worldDimensions(1) - 1 - column, worldDimensions(2) - 1 - row)

  private def planeDimensions(plane: AnatomicalPlane, worldDimensions: Vector[Int]): SliceDimensions =
    plane match
      case AnatomicalPlane.Axial => SliceDimensions(worldDimensions(0), worldDimensions(1))
      case AnatomicalPlane.Coronal => SliceDimensions(worldDimensions(0), worldDimensions(2))
      case AnatomicalPlane.Sagittal => SliceDimensions(worldDimensions(1), worldDimensions(2))

  private def fixedAxis(plane: AnatomicalPlane): Int =
    plane match
      case AnatomicalPlane.Sagittal => 0
      case AnatomicalPlane.Coronal => 1
      case AnatomicalPlane.Axial => 2

  private def pointOnPlane(plane: AnatomicalPlane, fixed: Int): WorldPoint =
    plane match
      case AnatomicalPlane.Sagittal => WorldPoint(fixed.toDouble, 0.0, 0.0)
      case AnatomicalPlane.Coronal => WorldPoint(0.0, fixed.toDouble, 0.0)
      case AnatomicalPlane.Axial => WorldPoint(0.0, 0.0, fixed.toDouble)

  private def worldField(point: WorldPoint): Double =
    1.5 * point.x - 2.25 * point.y + 0.75 * point.z + 4.0

  private def generatedAffine(seed: Int): AffineFixture =
    val random = new Random(seed.toLong)
    val angle = -0.9 + 1.8 * random.nextDouble()
    val cosine = math.cos(angle)
    val sine = math.sin(angle)
    val reflection = if seed % 2 == 0 then -1.0 else 1.0
    val scaleX = reflection * (0.65 + 1.7 * random.nextDouble())
    val scaleY = 0.75 + 1.9 * random.nextDouble()
    val scaleZ = 0.85 + 2.1 * random.nextDouble()
    val shearXY = -0.35 + 0.7 * random.nextDouble()
    val shearXZ = -0.25 + 0.5 * random.nextDouble()
    val shearYZ = -0.30 + 0.6 * random.nextDouble()
    val base = Vector(
      Vector(scaleX, shearXY, shearXZ),
      Vector(0.0, scaleY, shearYZ),
      Vector(0.0, 0.0, scaleZ)
    )
    val rotation = Vector(
      Vector(cosine, -sine, 0.0),
      Vector(sine, cosine, 0.0),
      Vector(0.0, 0.0, 1.0)
    )
    val linear = Vector.tabulate(3, 3) { (row, column) =>
      var sum = 0.0
      var inner = 0
      while inner < 3 do
        sum += rotation(row)(inner) * base(inner)(column)
        inner += 1
      sum
    }
    AffineFixture(
      linear,
      Vector.tabulate(3)(_ => -8.0 + 16.0 * random.nextDouble())
    )

  test("all 48 signed axis permutations reslice every anatomical plane correctly") {
    assertEquals(SignedPermutation.all.length, 48)
    SignedPermutation.all.foreach { orientation =>
      val source = volume[Int](Dims, orientation.affine(Dims)) { (x, y, z) =>
        encoded(Vector(x, y, z))
      }
      val worldDimensions = orientation.worldDimensions(Dims)
      LeftRightConvention.values.foreach { convention =>
        AnatomicalPlane.values.foreach { plane =>
          val axis = fixedAxis(plane)
          var fixed = 0
          while fixed < worldDimensions(axis) do
            val grid = SliceGrid.covering(
              source.volumeSpace,
              SlicePlane.canonical(plane, pointOnPlane(plane, fixed), convention),
              PixelSpacing(1.0, 1.0)
            )
            assertEquals(
              grid.dimensions,
              planeDimensions(plane, worldDimensions),
              clue = s"${orientation.clue} plane=$plane convention=$convention fixed=$fixed"
            )
            val sampled = SlicePlan.make(source.volumeSpace, grid)
              .sample(source, SliceSampling.Nearest(-1))
              .toOption
              .get
            var row = 0
            while row < grid.dimensions.height do
              var column = 0
              while column < grid.dimensions.width do
                val world = expectedWorldAt(plane, convention, fixed, column, row, worldDimensions)
                val expected = encoded(orientation.storageAt(world, Dims))
                assertEquals(
                  sampled(column, row),
                  expected,
                  clue =
                    s"${orientation.clue} plane=$plane convention=$convention fixed=$fixed pixel=($column,$row) world=$world"
                )
                column += 1
              row += 1
            fixed += 1
        }
      }
    }
  }

  test("deterministic oblique affine reslices reproduce a world-linear field") {
    val dims = SpatialDims(6, 7, 8)
    var compared = 0
    (0 until 24).foreach { seed =>
      val fixture = generatedAffine(seed)
      val source = volume[Double](dims, fixture.matrix) { (x, y, z) =>
        worldField(fixture.voxelToWorld(x.toDouble, y.toDouble, z.toDouble))
      }
      val cursor = fixture.voxelToWorld(2.5, 3.0, 3.5)
      AnatomicalPlane.values.foreach { plane =>
        val grid = SliceGrid.covering(
          source.volumeSpace,
          SlicePlane.canonical(plane, cursor),
          PixelSpacing(0.8, 1.1)
        )
        val sampled = SlicePlan.make(source.volumeSpace, grid)
          .sample(source, SliceSampling.Linear())
          .toOption
          .get
        var row = 0
        while row < grid.dimensions.height do
          var column = 0
          while column < grid.dimensions.width do
            val world = grid.worldAt(PixelCoord(column, row)).toOption.get
            val voxel = fixture.worldToVoxel(world)
            val safelyInside =
              voxel.x >= 0.0 && voxel.x < dims.x - 1.0 &&
                voxel.y >= 0.0 && voxel.y < dims.y - 1.0 &&
                voxel.z >= 0.0 && voxel.z < dims.z - 1.0
            if safelyInside then
              assertEqualsDouble(
                sampled(column, row),
                worldField(world),
                Tol,
                clue = s"seed=$seed plane=$plane pixel=($column,$row) voxel=$voxel"
              )
              compared += 1
            column += 1
          row += 1
      }
    }
    assert(compared >= 1000, clue = s"expected broad deterministic coverage; compared $compared pixels")
  }
