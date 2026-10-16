package scalafim.dataset

/** Defers storage access until a bounded read. The opened source must match the
  * declared acquisition; no response arrays or open handles are cached here.
  */
final class DeferredResponseBlockSource private (
    val shape: DatasetShape, val voxelDomain: VoxelDomain, val metadata: DatasetMetadata,
    open: () => Either[DatasetError,ResponseBlockSource]
) extends ResponseBlockSource:
  private var opened: Option[ResponseBlockSource] = None
  private def resolveSource(): Either[DatasetError,ResponseBlockSource] = synchronized {
    opened match
      case Some(source) => Right(source)
      case None => open().flatMap { source =>
        if source.shape != shape || source.voxelDomain != voxelDomain then
          Left(DatasetError.ShapeMismatch("deferred source does not match the reviewed shape and voxel domain"))
        else
          opened = Some(source)
          Right(source)
      }
  }
  protected[dataset] def readResolved(selection: ResolvedDataSelection): Either[DatasetError,FmriSeries] =
    resolveSource().flatMap(_.readResolved(selection))

object DeferredResponseBlockSource:
  def make(shape: DatasetShape, voxelDomain: VoxelDomain, metadata: DatasetMetadata = DatasetMetadata.Empty)(
      open: () => Either[DatasetError,ResponseBlockSource]
  ): Either[DatasetError,DeferredResponseBlockSource] =
    DatasetAcquisitionDomain.structuralCompatibility(shape,voxelDomain)
      .map(_ => new DeferredResponseBlockSource(shape,voxelDomain,metadata,open))
