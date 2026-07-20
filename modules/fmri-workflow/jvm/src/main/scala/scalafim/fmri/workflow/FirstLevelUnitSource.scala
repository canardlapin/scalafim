package scalafim.fmri.workflow

import scalafim.dataset.*
import scalafim.dataset.io.{NiftiResponseBlockSource, NiftiStagingCache}
import scalafim.image.{Mask, NArrayUtil, NeuroVol}
import scalafim.image.io.Nifti

import java.net.URI
import java.nio.file.Path
import scala.util.control.NonFatal

final case class OpenedFirstLevelUnit(
    unit: FirstLevelUnit,
    source: CompositeResponseBlockSource,
    mask: Mask.MaskVol
):
  def backend(
      datasetId: DatasetId,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): ResponseBlockDatasetBackend =
    ResponseBlockDatasetBackend(datasetId, source, mask, metadata)

object FirstLevelUnitSource:
  def open(
      unit: FirstLevelUnit,
      staging: Option[NiftiStagingCache] = None,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): Either[DatasetError, OpenedFirstLevelUnit] =
    for
      mask <- readMask(unit.mask)
      _ <-
        if mask.space == unit.shape.space then Right(())
        else Left(DatasetError.ShapeMismatch("unit mask does not match catalog geometry"))
      voxelDomain <- VoxelDomain.fromMask(mask, unit.shape)
      runSources <- traverse(unit.runs) { run =>
        for
          path <- filePath(run.bold.location)
          source <- NiftiResponseBlockSource.open(path, staging, Some(voxelDomain), metadata)
          _ <- validateRunSource(unit, run, source)
        yield RunResponseBlockSource(run.id, source)
      }
      composite <- CompositeResponseBlockSource.make(runSources, metadata)
      _ <-
        if composite.shape == unit.shape then Right(())
        else Left(DatasetError.ShapeMismatch("opened run sources do not match catalog unit shape"))
    yield OpenedFirstLevelUnit(unit, composite, mask)

  private def validateRunSource(
      unit: FirstLevelUnit,
      run: RunInput,
      source: NiftiResponseBlockSource
  ): Either[DatasetError, Unit] =
    if source.shape.space != unit.shape.space then
      Left(DatasetError.ShapeMismatch(s"run '${run.id.value}' geometry differs from its catalog unit"))
    else if source.shape.timepoints != run.timepoints then
      Left(
        DatasetError.ShapeMismatch(
          s"run '${run.id.value}' header has ${source.shape.timepoints} timepoints; catalog records ${run.timepoints}"
        )
      )
    else Right(())

  private def readMask(mask: UnitMask): Either[DatasetError, Mask.MaskVol] =
    mask match
      case UnitMask.Single(artifact) =>
        readMaskArtifact(artifact).flatMap(mask => intersectMasks(Vector(mask)))
      case intersection: UnitMask.Intersection =>
        traverse(intersection.runMasks.map(_._2))(readMaskArtifact).flatMap(intersectMasks)

  private def readMaskArtifact(
      artifact: WorkflowArtifactRef[MaskImageResource]
  ): Either[DatasetError, NeuroVol[Double]] =
    filePath(artifact.location).flatMap { path =>
      try Right(Nifti.readVol(path))
      catch
        case NonFatal(error) => Left(DatasetError.StorageFailure(s"failed to read mask NIfTI '$path': ${error.getMessage}"))
    }

  private def intersectMasks(
      masks: Vector[NeuroVol[Double]]
  ): Either[DatasetError, Mask.MaskVol] =
    if masks.isEmpty then Left(DatasetError.StorageFailure("mask intersection requires at least one mask"))
    else
      val first = masks.head
      val incompatible = masks.tail.exists(_.space != first.space)
      if incompatible then Left(DatasetError.ShapeMismatch("run masks have incompatible geometry"))
      else
        val indices = Array.newBuilder[Int]
        var voxel = 0
        while voxel < first.values.data.length do
          var keep = true
          var maskIndex = 0
          while maskIndex < masks.length && keep do
            val value = masks(maskIndex).values.data(voxel)
            keep = value.isFinite && value != 0.0
            maskIndex += 1
          if keep then indices += voxel
          voxel += 1
        val selected = indices.result()
        if selected.isEmpty then Left(DatasetError.ShapeMismatch("run-mask intersection is empty"))
        else Right(Mask.fromIndices(first.space, NArrayUtil.fromArray(selected), "run-mask-intersection"))

  private def filePath(location: ArtifactLocation): Either[DatasetError, Path] =
    try
      val uri = URI.create(location.value)
      if uri.getScheme != "file" then
        Left(DatasetError.StorageFailure(s"reference is not a local file URI: ${location.value}"))
      else Right(Path.of(uri).toAbsolutePath.normalize())
    catch
      case NonFatal(error) => Left(DatasetError.StorageFailure(s"invalid file artifact location '${location.value}': ${error.getMessage}"))

  private def traverse[A, B](
      values: Vector[A]
  )(f: A => Either[DatasetError, B]): Either[DatasetError, Vector[B]] =
    values.foldLeft[Either[DatasetError, Vector[B]]](Right(Vector.empty)) { (acc, value) =>
      for
        collected <- acc
        next <- f(value)
      yield collected :+ next
    }
