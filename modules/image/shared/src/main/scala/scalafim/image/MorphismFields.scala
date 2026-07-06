package scalafim.image

enum DenseVectorFieldKind:
  case SourceCoordinates, Displacement

final case class DenseVectorField(
    grid: GridSpec,
    values: NDArray[Double],
    kind: DenseVectorFieldKind
):
  require(values.shape == (grid.dims :+ 3), "dense vector field must have grid dims plus three components")

object MorphismFields:

  def sourceCoordinates(
      morphism: SpatialMorphism,
      grid: GridSpec
  ): DenseVectorField =
    val targetCoords = grid.worldCoords
    val sourceCoords = morphism.transform(targetCoords)
    DenseVectorField(grid, vectorField(grid, sourceCoords), DenseVectorFieldKind.SourceCoordinates)

  def displacement(
      morphism: SpatialMorphism,
      grid: GridSpec
  ): DenseVectorField =
    val targetCoords = grid.worldCoords
    val sourceCoords = morphism.transform(targetCoords)
    val deltas =
      Vector.tabulate(sourceCoords.length) { i =>
        Vector.tabulate(3)(axis => sourceCoords(i)(axis) - targetCoords(i)(axis))
      }
    DenseVectorField(grid, vectorField(grid, deltas), DenseVectorFieldKind.Displacement)

  def jacobianDeterminant(
      morphism: SpatialMorphism,
      grid: GridSpec,
      log: Boolean = false,
      mode: JacobianMode = JacobianMode.Pullback
  ): Either[MorphismError, NeuroVol[Double]] =
    val coords = grid.worldCoords
    morphism.jacobianDet(coords, log, mode).map { dets =>
      NeuroVol.fromLinear(NArrayUtil.fromArray(dets.toArray), grid.toNeuroSpace, "jacobian-det")
    }

  private def vectorField(grid: GridSpec, vectors: Vector[Vector[Double]]): NDArray[Double] =
    require(vectors.length == grid.nVoxels, "vector count must match grid voxels")
    val data = NArrayUtil.ofSize[Double](grid.nVoxels * 3)
    var i = 0
    while i < grid.nVoxels do
      val vector = vectors(i)
      require(vector.length == 3, "field vectors must be 3D")
      var component = 0
      while component < 3 do
        data(component * grid.nVoxels + i) = vector(component)
        component += 1
      i += 1
    NDArray(data, grid.dims :+ 3)
