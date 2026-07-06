package scalafim.dataset

import scalafim.image.{DMat, Mask, NeuroSpace}
import scalafim.latent.{LatentResponse, LatentSelection}

final case class LatentResponseDatasetBackend(
    id: DatasetId,
    response: LatentResponse,
    space: NeuroSpace,
    mask: Mask.MaskVol,
    metadata: DatasetMetadata = DatasetMetadata.Empty
) extends DatasetBackend:

  override val shape: DatasetShape =
    DatasetShape(space.spatialSpace, response.shape.timepoints)

  require(mask.space.spatialDims == shape.spatialDims, "mask/space dimension mismatch")
  require(mask.space.spacing == shape.space.spacing && mask.space.origin == shape.space.origin, "mask/space mismatch")

  private val sampleVoxels: Vector[Int] =
    val indices = Mask.indices(mask)
    require(indices.length == response.shape.samples, "latent sample count must match mask cardinality")
    Vector.tabulate(indices.length)(indices(_))

  private val voxelToSample: Array[Int] =
    val lookup = Array.fill(shape.spatialSize)(-1)
    var sample = 0
    while sample < sampleVoxels.length do
      lookup(sampleVoxels(sample)) = sample
      sample += 1
    lookup

  override def read(selection: DataSelection = DataSelection.All): FmriSeries =
    val resolved = selection.resolve(shape)
    val samples = resolveSamples(resolved.voxels)
    val decoded =
      response
        .reconstruct(LatentSelection(timepoints = Some(resolved.timepoints), samples = Some(samples)))
        .fold(err => throw new IllegalArgumentException(err.message), identity)

    FmriSeries(
      data = DMat.fromRows(decoded.toRows),
      voxelIndices = resolved.voxels,
      timepoints = resolved.timepoints,
      shape = shape,
      metadata = metadata
    )

  private def resolveSamples(voxels: Vector[Int]): Vector[Int] =
    val out = Vector.newBuilder[Int]
    out.sizeHint(voxels.length)
    var i = 0
    while i < voxels.length do
      val voxel = voxels(i)
      val sample = voxelToSample(voxel)
      if sample < 0 then throw new IllegalArgumentException(s"voxel $voxel is outside the latent mask")
      out += sample
      i += 1
    out.result()

object LatentResponseDatasetBackend:
  def apply(
      id: DatasetId,
      response: LatentResponse,
      space: NeuroSpace,
      metadata: DatasetMetadata
  ): LatentResponseDatasetBackend =
    LatentResponseDatasetBackend(
      id = id,
      response = response,
      space = space.spatialSpace,
      mask = Mask.all(space.spatialSpace),
      metadata = metadata
    )

  def apply(
      id: DatasetId,
      response: LatentResponse,
      space: NeuroSpace
  ): LatentResponseDatasetBackend =
    LatentResponseDatasetBackend(id, response, space, DatasetMetadata.Empty)
