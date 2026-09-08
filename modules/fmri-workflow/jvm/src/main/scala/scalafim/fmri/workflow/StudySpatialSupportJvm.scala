package scalafim.fmri.workflow

import java.net.URI
import java.nio.file.Path
import scalafim.dataset.*
import scalafim.dataset.io.{NiftiResponseBlockSource, NiftiStagingCache}
import scalafim.image.io.Nifti
import scala.util.control.NonFatal

object StudySpatialSupportJvm:
  /** Compressed masks require explicitly managed native staging. No BOLD data
    * are opened. Header checks precede full-domain allocation and staging.
    */
  def compile(catalog: StudyCatalog, limits: StudySpatialLimits,
      staging: Option[NiftiStagingCache] = None): Either[WorkflowError, StudySpatialSupport] =
    StudySpatialSupport.compile(catalog, limits) { artifact =>
      try
        if Thread.currentThread().isInterrupted then throw InterruptedException("Study spatial support cancelled")
        val location = URI.create(artifact.location.value)
        if location.getScheme != "file" then
          Left(DatasetError.StorageFailure(s"mask requires a local file URI: $location"))
        else
          val path = Path.of(location).toAbsolutePath.normalize()
          val header = Nifti.readHeader(path)
          val isVolume = header.dims.length == 3 || (header.dims.length == 4 && header.dims(3) == 1)
          if !isVolume || header.space.spatialSpace != catalog.units.head.shape.space then
            Left(DatasetError.ShapeMismatch("mask header differs from the expected single-volume spatial grid"))
          else NiftiResponseBlockSource.open(path, staging).map { opened =>
            new ResponseBlockSource:
              val shape = opened.shape
              val voxelDomain = opened.voxelDomain
              val metadata = opened.metadata
              override lazy val acquisitionDomain = opened.acquisitionDomain
              def readResolved(selection: ResolvedDataSelection): Either[DatasetError, FmriSeries] =
                if Thread.currentThread().isInterrupted then throw InterruptedException("Study spatial support cancelled")
                opened.readBlock(DataSelection(time = TimepointSelection.Indices(selection.timepointIndices),
                  voxels = VoxelSelection.Indices(selection.voxelIndexValues)))
          }
      catch case NonFatal(error) =>
        Left(DatasetError.StorageFailure(s"failed to open mask '${artifact.location.value}': ${error.getMessage}"))
    }
