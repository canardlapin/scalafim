package scalafim.fmri.group

import scalafim.image.NeuroSpace

/** The sample axis of a group analysis. fmrigds's insight is that a "sample"
  * can be a voxel, a parcel, a surface vertex, or a latent component; the group
  * statistics are identical regardless. Only the geometry differs, and only
  * spatial correction (a later phase) needs it.
  */
sealed trait GroupSpace:
  def nSamples: Int

object GroupSpace:

  /** A generic, geometry-free axis of `nSamples` samples, optionally labelled.
    * The default axis for tabular or ROI data.
    */
  final case class SampleAxis(nSamples: Int, labels: Vector[SampleLabel] = Vector.empty) extends GroupSpace:
    require(nSamples > 0, "sample axis must have at least one sample")
    require(labels.isEmpty || labels.length == nSamples, "labels must match sample count when present")

  /** Voxel samples packed into a `NeuroSpace`. `sampleIndices` are linear voxel
    * indices into the spatial grid, so that later spatial correction can scatter
    * a per-sample vector back to a full volume.
    */
  final case class VoxelAxis(space: NeuroSpace, sampleIndices: Vector[Int]) extends GroupSpace:
    require(sampleIndices.nonEmpty, "voxel axis must reference at least one voxel")
    private val gridSize = space.spatialDims.product
    require(sampleIndices.forall(i => i >= 0 && i < gridSize), "voxel index out of spatial bounds")
    def nSamples: Int = sampleIndices.length

  /** Discrete parcels / regions of interest, one sample per parcel label. */
  final case class ParcelAxis(labels: Vector[SampleLabel]) extends GroupSpace:
    require(labels.nonEmpty, "parcel axis must have at least one parcel")
    def nSamples: Int = labels.length
