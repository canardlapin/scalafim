package scalafim.image

import image4s.ComponentImage
import ravel.NDArray as RavelArray
import ravel.Rank
import ravel.Shape
import image4s.geometry.D3
import image4s.geometry.Frame

enum DenseVectorFieldKind:
  case SourceCoordinates, Displacement

/** Zero-copy compatibility view over image4s' canonical component image.
  *
  * The `ComponentImage` owns both Ravel storage and sampled-image geometry.
  * This class retains the historical ScalaFIM name and field-kind semantic
  * while delegating all value access to that one canonical value.
  */
final class DenseVectorField private (
    val grid: GridSpec,
    val sampled: ComponentImage[? <: Frame[D3], D3, Rank[4]],
    val kind: DenseVectorFieldKind
):
  val values: RavelArray[Double, Rank[4]] =
    sampled.data
  private[scalafim] val flatValues: RavelArray[Double, Rank[1]] =
    values.reshapeView(Shape(values.size))

  inline def apply(i: Int, j: Int, k: Int, component: Int): Double =
    values(i, j, k, component)

  inline def apply(voxel: VoxelCoord, component: Int): Double =
    apply(voxel.x, voxel.y, voxel.z, component)

  def linearComponent(linearVoxel: Int, component: Int): Double =
    require(linearVoxel >= 0 && linearVoxel < grid.nVoxels, "linear voxel index out of bounds")
    require(component >= 0 && component < 3, "vector component out of bounds")
    val nx = grid.shape.x
    val ny = grid.shape.y
    val x = linearVoxel % nx
    val yz = linearVoxel / nx
    val y = yz % ny
    val z = yz / ny
    flatValues(component + 3 * (z + grid.extentZ * (y + grid.extentY * x)))

  private[scalafim] inline def flatValue(storageIndex: Int): Double =
    flatValues(storageIndex)

  /** Explicit compatibility export in ScalaFIM's historical
    * component-planar, first-axis-fastest order. Always copies.
    */
  def copyLegacyPlanar: Array[Double] =
    val out = PrimitiveBuffers.ofSize[Double](grid.nVoxels * 3)
    var linear = 0
    while linear < grid.nVoxels do
      var component = 0
      while component < 3 do
        out(linear + component * grid.nVoxels) =
          linearComponent(linear, component)
        component += 1
      linear += 1
    out

object DenseVectorField:
  def apply(
      grid: GridSpec,
      values: RavelArray[Double, Rank[4]],
      kind: DenseVectorFieldKind
  ): DenseVectorField =
    require(
      values.shape == Shape(grid.dims(0), grid.dims(1), grid.dims(2), 3),
      "dense vector field must have grid dims plus three components"
    )
    require(
      values.isCanonicalLayout && values.isWholeBuffer,
      "dense vector field storage must be a whole canonical Ravel buffer"
    )
    val sampled =
      Image4sInterop
        .componentsFromRavel(values, grid, kind.toString)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    new DenseVectorField(grid, sampled, kind)

  /** Materializing ingress from component-planar, x-fastest values. */
  def fromLegacyPlanar(
      grid: GridSpec,
      values: Array[Double],
      kind: DenseVectorFieldKind
  ): DenseVectorField =
    require(values.length == grid.nVoxels * 3, "dense vector field data length mismatch")
    val nx = grid.shape.x
    val ny = grid.shape.y
    val packed =
      RavelArray.tabulate[Double](nx, ny, grid.shape.z, 3) {
        (i, j, k, component) =>
          values(i + nx * (j + ny * k) + component * grid.nVoxels)
      }
    apply(grid, packed, kind)

object MorphismFields:

  def sourceCoordinates(
      morphism: SpatialMorphism,
      grid: GridSpec
  ): DenseVectorField =
    val targetPoints = grid.worldPoints
    val sourcePoints = morphism.transformPoints(targetPoints)
    DenseVectorField(grid, vectorField(grid, sourcePoints.map(_.toVector)), DenseVectorFieldKind.SourceCoordinates)

  def displacement(
      morphism: SpatialMorphism,
      grid: GridSpec
  ): DenseVectorField =
    val targetPoints = grid.worldPoints
    val sourcePoints = morphism.transformPoints(targetPoints)
    val deltas =
      Vector.tabulate(sourcePoints.length) { i =>
        val source = sourcePoints(i)
        val target = targetPoints(i)
        Vector(source.x - target.x, source.y - target.y, source.z - target.z)
      }
    DenseVectorField(grid, vectorField(grid, deltas), DenseVectorFieldKind.Displacement)

  def jacobianDeterminant(
      morphism: SpatialMorphism,
      grid: GridSpec,
      log: Boolean = false,
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, NeuroVol[Double]] =
    val points = grid.worldPoints
    morphism.jacobianDetAt(points, log, mode).map { dets =>
      NeuroVol.fromLinear(PrimitiveBuffers.fromArray(dets.toArray), grid.toNeuroSpace, "jacobian-det")
    }

  private def vectorField(grid: GridSpec, vectors: Vector[Vector[Double]]): RavelArray[Double, Rank[4]] =
    require(vectors.length == grid.nVoxels, "vector count must match grid voxels")
    val nx = grid.shape.x
    val ny = grid.shape.y
    RavelArray.tabulate[Double](nx, ny, grid.shape.z, 3) {
      (i, j, k, component) =>
        val vector = vectors(i + nx * (j + ny * k))
        require(vector.length == 3, "field vectors must be 3D")
        vector(component)
    }
