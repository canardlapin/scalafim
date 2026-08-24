package scalafim.fmri.workflow

import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid
import image4s.geometry.GridCongruence
import scalafim.dataset.*
import scalafim.dataset.io.{NiftiResponseBlockSource, NiftiStagingCache}
import scalafim.image.Mask
import scalafim.image.PrimitiveBuffers
import scalafim.image.SomeScalarVolume
import scalafim.image.{space, valueAtCanonicalOrdinal, values}
import scalafim.image.io.Nifti

import java.net.URI
import java.nio.file.Path
import scala.util.control.NonFatal

final case class OpenedFirstLevelUnit(
    unit: FirstLevelUnit,
    source: CompositeResponseBlockSource,
    mask: Mask.MaskVol,
    maskCongruence: GridCongruence[D3, ? <: Frame[D3], ? <: Frame[D3]]
):
  def backend(
      datasetId: DatasetId,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): Either[DatasetError, ResponseBlockDatasetBackend] =
    ResponseBlockDatasetBackend.makeCongruent(
      datasetId,
      source,
      mask,
      maskCongruence,
      metadata
    )

object FirstLevelUnitSource:
  def open(
      unit: FirstLevelUnit,
      staging: Option[NiftiStagingCache] = None,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): Either[DatasetError, OpenedFirstLevelUnit] =
    for
      mask <- readMask(unit.mask)
      maskCongruence <- Grid
        .approximateCongruence(unit.shape.grid, mask.grid, 1e-6)
        .left
        .map(DatasetError.Geometry.apply)
      voxelDomain <- VoxelDomain.fromMask(mask, unit.shape, maskCongruence)
      openedRuns <- traverse(unit.runs) { run =>
        for
          path <- filePath(run.bold.location)
          source <- NiftiResponseBlockSource.open(path, staging, Some(voxelDomain), metadata)
          congruence <- validateRunSource(unit, run, source)
        yield RunResponseBlockSource(run.id, source) -> congruence
      }
      composite <- CompositeResponseBlockSource.makeCongruent(
        openedRuns.map(_._1),
        unit.shape.space,
        openedRuns.map(_._2),
        metadata
      )
      _ <- validateSameShape(composite.shape, unit.shape)
    yield OpenedFirstLevelUnit(unit, composite, mask, maskCongruence)

  private def validateRunSource(
      unit: FirstLevelUnit,
      run: RunInput,
      source: NiftiResponseBlockSource
  ): Either[
    DatasetError,
    GridCongruence[D3, ? <: Frame[D3], ? <: Frame[D3]]
  ] =
    for
      congruence <- Grid
        .approximateCongruence(
          unit.shape.grid,
          source.shape.grid,
          1e-6
        )
        .left
        .map(DatasetError.Geometry.apply)
      _ <-
        if source.shape.timepoints != run.timepoints then
          Left(
            DatasetError.ShapeMismatch(
              s"run '${run.id.value}' header has ${source.shape.timepoints} timepoints; catalog records ${run.timepoints}"
            )
          )
        else Right(())
    yield congruence

  private def readMask(mask: UnitMask): Either[DatasetError, Mask.MaskVol] =
    mask match
      case UnitMask.Single(artifact) =>
        readMaskArtifact(artifact).flatMap(mask => intersectMasks(Vector(mask)))
      case intersection: UnitMask.Intersection =>
        traverse(intersection.runMasks.map(_._2))(readMaskArtifact).flatMap(intersectMasks)

  private def readMaskArtifact(
      artifact: WorkflowArtifactRef[MaskImageResource]
  ): Either[DatasetError, SomeScalarVolume[Double]] =
    filePath(artifact.location).flatMap { path =>
      Nifti
        .readVolume(path)
        .left
        .map(error => DatasetError.StorageFailure(s"failed to read mask NIfTI '$path': ${error.message}"))
        .map(_.image)
    }

  private def intersectMasks(
      masks: Vector[SomeScalarVolume[Double]]
  ): Either[DatasetError, Mask.MaskVol] =
    if masks.isEmpty then Left(DatasetError.StorageFailure("mask intersection requires at least one mask"))
    else
      val first = masks.head
      val alignments =
        masks.tail.foldLeft[Either[DatasetError, Unit]](Right(())):
          (acc, mask) =>
            for
              _ <- acc
              _ <- Grid
                .approximateCongruence(first.grid, mask.grid, 1e-6)
                .left
                .map(DatasetError.Geometry.apply)
            yield ()
      alignments.map: _ =>
        val indices = Array.newBuilder[Int]
        var voxel = 0
        while voxel < first.values.size do
          var keep = true
          var maskIndex = 0
          while maskIndex < masks.length && keep do
            val value = masks(maskIndex).valueAtCanonicalOrdinal(voxel)
            keep = value.isFinite && value != 0.0
            maskIndex += 1
          if keep then indices += voxel
          voxel += 1
        val selected = indices.result()
        selected
      .flatMap: selected =>
        if selected.isEmpty then
          Left(DatasetError.ShapeMismatch("run-mask intersection is empty"))
        else
          Right(
            Mask.fromIndices(
              first.space,
              PrimitiveBuffers.fromArray(selected),
              "run-mask-intersection"
            )
          )

  private def validateSameShape(
      expected: DatasetShape,
      actual: DatasetShape
  ): Either[DatasetError, Unit] =
    if expected.timepoints != actual.timepoints then
      Left(DatasetError.ShapeMismatch("opened run sources do not match catalog unit timepoints"))
    else
      Grid
        .exactCongruence(expected.grid, actual.grid)
        .left
        .map(DatasetError.Geometry.apply)
        .map(_ => ())

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
