package scalafim.spatial.io

import neuroim4s.spatial.VolumeReadRequest
import neuroim4s.spatial.io.{NiftiVolumeSource, NiftiVolumeSourceStats}
import neuroim4s.spaces.SpaceEvidence
import scalafim.spatial.*
import java.nio.file.Path

/** Functional field identity and provenance around the anatomical scoped reader. */
final class NiftiFieldSource private (
    private val volume: NiftiVolumeSource,
    override val descriptor: FieldSourceDescriptor
) extends FieldSource:
  def path: Path = volume.path
  def evidence: SpaceEvidence = volume.evidence
  def stats: NiftiVolumeSourceStats = volume.stats

  override def validate(): Either[SpatialError, Unit] = volume.validate().left.map(VolumeReadErrors(descriptor.id))

  override def read(request: FieldSourceRequest): Either[SpatialError, FieldSourceBlock] =
    for
      checked <- VolumeReadRequest.make(descriptor.rows, descriptor.observations, request.sourceRows, request.observations)
        .left.map(VolumeReadErrors(descriptor.id))
      block <- volume.read(checked).left.map(VolumeReadErrors(descriptor.id))
      result <- FieldSourceBlock.make(descriptor.id, request, block.data)
    yield result

object NiftiFieldSource:
  /** No filesystem access until terminal validation, which pins the backing revision. */
  def prepare(path: Path, domain: Domain, evidence: SpaceEvidence, observations: Int, label: String = ""): Either[SpatialError, NiftiFieldSource] =
    val normalized = path.toAbsolutePath.normalize()
    for
      sourceId <- FieldSourceId(s"nifti:$normalized")
      revision <- FieldSourceRevision(s"first-read-snapshot:$normalized")
      descriptor <- FieldSourceDescriptor.make(sourceId, revision,
        if label.trim.isEmpty then normalized.getFileName.toString else label,
        domain.id, domain.geometry, domain.nElements, observations)
      grid <- domain.geometry match
        case SamplingGeometry.Volume(grid) => Right(grid)
        case _ => Left(SpatialError.FieldSourceGeometryMismatch(sourceId))
      volume <- NiftiVolumeSource.prepare(normalized, grid, evidence, observations).left.map(VolumeReadErrors(sourceId))
    yield new NiftiFieldSource(volume, descriptor)
