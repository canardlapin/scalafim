package scalafim.image

import image4s.Continuous
import image4s.Axis as ImageAxis
import image4s.AxisKind
import image4s.ImageMetadata
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import ravel.NDArray as RavelArray
import ravel.Rank
import ravel.Shape
import image4s.geometry.D3

enum DenseVectorFieldKind:
  case SourceCoordinates, Displacement

/** A continuous D3 image with one three-valued Direction axis.
  *
  * `Role` is a phantom scientific invariant. The runtime value is exactly the
  * image4s `Sampled` object and retains its Ravel array without a wrapper.
  */
opaque type DenseVectorField[Role <: DenseVectorFieldKind] =
  Sampled[
    ? <: SampleSpace[?, D3],
    Double,
    Continuous,
    Rank[4]
  ]

type SourceCoordinateField =
  DenseVectorField[DenseVectorFieldKind.SourceCoordinates.type]

type DisplacementField =
  DenseVectorField[DenseVectorFieldKind.Displacement.type]

object DenseVectorField:
  extension [Role <: DenseVectorFieldKind](
      field: DenseVectorField[Role]
  )
    inline def sampled: Sampled[
      ? <: SampleSpace[?, D3],
      Double,
      Continuous,
      Rank[4]
    ] = field

    inline def values: RavelArray[Double, Rank[4]] =
      field.data

    inline def space: SomeSampleSpace =
      SampleSpaces.fromCanonical(field.sampleSpace)

    inline def kind(using role: ValueOf[Role]): Role =
      role.value

    inline def apply(
        x: Int,
        y: Int,
        z: Int,
        component: Int
    ): Double =
      field.data(x, y, z, component)

    inline def apply(voxel: VoxelCoord, component: Int): Double =
      apply(voxel.x, voxel.y, voxel.z, component)

    /** Explicitly copy values in canonical `(x,y,z,direction)` order. */
    def copyToCanonicalArray: Array[Double] =
      val shape = field.data.shape
      val out = Array.ofDim[Double](field.data.size)
      var x = 0
      while x < shape(0) do
        var y = 0
        while y < shape(1) do
          var z = 0
          while z < shape(2) do
            var component = 0
            while component < shape(3) do
              val ordinal = (((x * shape(1)) + y) * shape(2) + z) * shape(3) + component
              out(ordinal) = field.data(x, y, z, component)
              component += 1
            z += 1
          y += 1
        x += 1
      out

  def sourceCoordinates(
      grid: GridSpec,
      values: RavelArray[Double, Rank[4]]
  ): SourceCoordinateField =
    make[DenseVectorFieldKind.SourceCoordinates.type](grid, values)

  def displacement(
      grid: GridSpec,
      values: RavelArray[Double, Rank[4]]
  ): DisplacementField =
    make[DenseVectorFieldKind.Displacement.type](grid, values)

  private def make[Role <: DenseVectorFieldKind](
      grid: GridSpec,
      values: RavelArray[Double, Rank[4]]
  )(using role: ValueOf[Role]): DenseVectorField[Role] =
    require(
      values.shape == Shape(grid.dims(0), grid.dims(1), grid.dims(2), 3),
      "dense vector field must have grid dims plus three components"
    )
    val spatial =
      SampleSpaces
        .requireSpatialD3(grid.toSampleSpace)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val direction =
      ImageAxis
        .create("direction", 3, AxisKind.Direction)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val axes =
      NonSpatialAxes
        .from(Vector(direction))
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val componentSpace = SampleSpace.create(spatial.grid, axes)
    Sampled
      .continuous(
        componentSpace,
        values,
        ImageMetadata.named(s"dense-vector-field:${role.value}")
      )
      .fold(error => throw new IllegalArgumentException(error.message), identity)

object MorphismFields:

  def sourceCoordinates(
      morphism: SpatialMorphism,
      grid: GridSpec
  ): SourceCoordinateField =
    val targetPoints = grid.worldPoints
    val sourcePoints = morphism.transformPoints(targetPoints)
    DenseVectorField.sourceCoordinates(
      grid,
      vectorField(grid, sourcePoints.map(_.toVector))
    )

  def displacement(
      morphism: SpatialMorphism,
      grid: GridSpec
  ): DisplacementField =
    val targetPoints = grid.worldPoints
    val sourcePoints = morphism.transformPoints(targetPoints)
    val deltas =
      Vector.tabulate(sourcePoints.length) { i =>
        val source = sourcePoints(i)
        val target = targetPoints(i)
        Vector(source.x - target.x, source.y - target.y, source.z - target.z)
      }
    DenseVectorField.displacement(grid, vectorField(grid, deltas))

  def jacobianDeterminant(
      morphism: SpatialMorphism,
      grid: GridSpec,
      log: Boolean = false,
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, SomeScalarVolume[Double]] =
    val points = grid.worldPoints
    morphism.jacobianDetAt(points, log, mode).map { dets =>
      val nx = grid.shape.x
      val ny = grid.shape.y
      val values =
        RavelArray.tabulate[Double](nx, ny, grid.shape.z): (x, y, z) =>
          dets(Indexing.gridToIndex3D(grid.shape, x, y, z))
      SomeNeuroVolume.unsafeFromRavel(values, grid.toSampleSpace, "jacobian-det")
    }

  private def vectorField(grid: GridSpec, vectors: Vector[Vector[Double]]): RavelArray[Double, Rank[4]] =
    require(vectors.length == grid.nVoxels, "vector count must match grid voxels")
    val nx = grid.shape.x
    val ny = grid.shape.y
    RavelArray.tabulate[Double](nx, ny, grid.shape.z, 3) {
      (i, j, k, component) =>
        val vector =
          vectors(Indexing.gridToIndex3D(grid.shape, i, j, k))
        require(vector.length == 3, "field vectors must be 3D")
        vector(component)
    }
