package scalafim.fmri.fit

import image4s.Axis as ImageAxis
import image4s.AxisKind
import image4s.Continuous
import image4s.ImageMetadata
import image4s.locus.GridDomain
import locus4s.DomainRegistry
import locus4s.Selection
import scalafim.dataset.DatasetShape
import ravel.NDArray as RavelArray
import scalafim.image.{NeuroSpace, NeuroVec, SelectedSeries, SomeSelectedSeries}
import spire.implicits.DoubleAlgebra

final case class FitImageMaps(
    names: Vector[String],
    values: SomeSelectedSeries[Double, Continuous]
):
  require(names.nonEmpty, "image map names must be non-empty")
  require(values.value.nTime == names.length, "image map names must match map volumes")

  def nMaps: Int = names.length
  def dense: NeuroVec[Double] =
    val native =
      values.value
        .toDense(0.0)
        .fold(error => throw new IllegalStateException(error.message), identity)
    NeuroVec.fromNative(native)
  def mapIndex(name: String): Option[Int] = names.indexOf(name) match
    case -1 => None
    case i  => Some(i)

object FitImageMaps:

  def fromStatMap(map: StatMap): FitImageMaps =
    fromRows(
      names = Vector(map.label),
      rowsByMap = Vector(map.valueVector),
      shape = map.shape,
      selectedVoxels = map.selectedVoxels,
      kind = FitImageMapKind.Custom(map.kind.label)
    )

  def fromParameterMaps(maps: Vector[ParameterMap]): Either[FitError, FitImageMaps] =
    validateCompatibleParameterMaps(maps).map { _ =>
      val first = maps.head
      val kind =
        first.statistic match
          case ParameterMapKind.Coefficient   => FitImageMapKind.Coefficients
          case ParameterMapKind.StandardError => FitImageMapKind.StandardErrors
      fromRows(
        names = maps.map(_.parameterName.value),
        rowsByMap = maps.map(_.map.valueVector),
        shape = first.map.shape,
        selectedVoxels = first.map.selectedVoxels,
        kind = kind
      )
    }

  def fromContrastMaps(maps: Vector[ContrastMap]): Either[FitError, FitImageMaps] =
    validateCompatibleContrastMaps(maps).map { _ =>
      val first = maps.head
      fromRows(
        names = maps.map(_.map.label),
        rowsByMap = maps.map(_.map.valueVector),
        shape = first.map.shape,
        selectedVoxels = first.map.selectedVoxels,
        kind = FitImageMapKind.Custom(first.contrastId.value)
      )
    }

  def fromRows(
      names: Vector[String],
      rowsByMap: Vector[Vector[Double]],
      shape: DatasetShape,
      voxelIndices: Vector[Int],
      label: String
  ): FitImageMaps =
    fromRows(names, rowsByMap, shape, SelectedVoxelIndices.unsafe(voxelIndices), FitImageMapKind.Custom(label))

  def fromRows(
      names: Vector[String],
      rowsByMap: Vector[Vector[Double]],
      shape: DatasetShape,
      selectedVoxels: SelectedVoxelIndices,
      kind: FitImageMapKind
  ): FitImageMaps =
    val voxelIndices = selectedVoxels.toVector
    val label = kind.label
    require(names.nonEmpty, "image map names must be non-empty")
    require(rowsByMap.length == names.length, "rowsByMap length must match names")
    require(voxelIndices.forall(i => i >= 0 && i < shape.spatialSize), "voxel index out of bounds for dataset shape")
    rowsByMap.foreach { row =>
      require(row.length == voxelIndices.length, "map rows must match voxel index count")
      require(row.forall(!_.isNaN), "image map values must not be NaN")
    }

    val sorted = voxelIndices.zipWithIndex.sortBy(_._1)
    val sortedIndices = sorted.map(_._1).toArray
    val nMaps = names.length
    val nVoxels = sorted.length
    val data =
      RavelArray.tabulate[Double](nVoxels, nMaps) { (outPosition, map) =>
        rowsByMap(map)(sorted(outPosition)._2)
      }
    val spatial =
      NeuroSpace
        .requireSpatialD3(shape.space)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val resolution =
      GridDomain
        .register(spatial.grid, s"$label selected voxels", DomainRegistry.empty)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val selection =
      Selection
        .fromOrdinals(resolution.value.space, sortedIndices)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val mapAxis =
      ImageAxis
        .create("map", nMaps, AxisKind.Time)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val selected =
      SelectedSeries
        .create(
          resolution.value,
          selection,
          mapAxis,
          data,
          ImageMetadata(label)
        )
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    FitImageMaps(
      names = names,
      values = SomeSelectedSeries(selected)
    )

  private def validateCompatibleParameterMaps(maps: Vector[ParameterMap]): Either[FitError, Unit] =
    if maps.isEmpty then Left(FitError.InvalidFitAxis("parameter maps", "must be non-empty"))
    else
      val first = maps.head
      maps.find(_.statistic != first.statistic) match
        case Some(_) =>
          Left(FitError.InvalidFitAxis("parameter maps", "must share one statistic kind"))
        case None =>
          validateCompatibleStatMaps("parameter maps", maps.map(_.map))

  private def validateCompatibleContrastMaps(maps: Vector[ContrastMap]): Either[FitError, Unit] =
    if maps.isEmpty then Left(FitError.InvalidFitAxis("contrast maps", "must be non-empty"))
    else
      val first = maps.head
      maps.find(_.contrastId != first.contrastId) match
        case Some(_) =>
          Left(FitError.InvalidFitAxis("contrast maps", "must share one contrast id"))
        case None =>
          validateCompatibleStatMaps("contrast maps", maps.map(_.map))

  private def validateCompatibleStatMaps(label: String, maps: Vector[StatMap]): Either[FitError, Unit] =
    val first = maps.head
    maps.find(map => map.shape != first.shape || map.selectedVoxels != first.selectedVoxels) match
      case Some(_) =>
        Left(FitError.InvalidFitAxis(label, "must share dataset shape and selected voxels"))
      case None =>
        Right(())

extension (result: DenseFmriFitResult)
  def coefficientMaps(shape: DatasetShape): FitImageMaps =
    FitImageMaps.fromRows(
      names = result.columnNames,
      rowsByMap = matrixRows(result.coefficients.value),
      shape = shape,
      selectedVoxels = result.selectedVoxels,
      kind = FitImageMapKind.Coefficients
    )

  def standardErrorMaps(shape: DatasetShape): FitImageMaps =
    val allowed = result.inferenceScope.allowedIndices(result.predictors)
    FitImageMaps.fromRows(
      names = allowed.map(result.columnNames),
      rowsByMap = allowed.map(row => matrixRowValues(result.standardErrors.value, row)),
      shape = shape,
      selectedVoxels = result.selectedVoxels,
      kind = FitImageMapKind.StandardErrors
    )

extension (result: TContrastResult)
  def statisticMap(shape: DatasetShape): FitImageMaps =
    FitImageMaps.fromRows(
      names = Vector(result.name),
      rowsByMap = Vector(result.statistics.toSeq.toVector),
      shape = shape,
      selectedVoxels = result.selectedVoxels,
      kind = FitImageMapKind.TStatistic(result.name)
    )

  def maps(shape: DatasetShape): FitImageMaps =
    FitImageMaps.fromRows(
      names = Vector(s"${result.name}_estimate", s"${result.name}_standard_error", s"${result.name}_t"),
      rowsByMap = Vector(result.estimates.toSeq.toVector, result.standardErrors.toSeq.toVector, result.statistics.toSeq.toVector),
      shape = shape,
      selectedVoxels = result.selectedVoxels,
      kind = FitImageMapKind.TContrastBundle(result.name)
    )

extension (result: FContrastResult)
  def statisticMap(shape: DatasetShape): FitImageMaps =
    FitImageMaps.fromRows(
      names = Vector(result.name),
      rowsByMap = Vector(result.statistics.toSeq.toVector),
      shape = shape,
      selectedVoxels = result.selectedVoxels,
      kind = FitImageMapKind.FStatistic(result.name)
    )

  def maps(shape: DatasetShape): FitImageMaps =
    val estimateNames =
      Vector.tabulate(result.estimates.rows)(i => s"${result.name}_estimate_${i + 1}")
    FitImageMaps.fromRows(
      names = estimateNames :+ s"${result.name}_f",
      rowsByMap = matrixRows(result.estimates) :+ result.statistics.toSeq.toVector,
      shape = shape,
      selectedVoxels = result.selectedVoxels,
      kind = FitImageMapKind.FContrastBundle(result.name)
    )

private def matrixRows(matrix: gale.linalg.DMat): Vector[Vector[Double]] =
  Vector.tabulate(matrix.rows) { row =>
    Vector.tabulate(matrix.cols)(col => matrix(row, col))
  }

private def matrixRowValues(matrix: gale.linalg.DMat, row: Int): Vector[Double] =
  Vector.tabulate(matrix.cols)(col => matrix(row, col))
