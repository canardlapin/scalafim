package scalafim.fmri.workflow

import scalafim.dataset.*
import scalafim.image.{VoxelSelection as ImageVoxelSelection}

final class StudySpatialLimits private (
    val maximumSpatialVoxels: Int,
    val maximumVoxelsPerRead: Int
)

object StudySpatialLimits:
  def make(maximumSpatialVoxels: Int, maximumVoxelsPerRead: Int): Either[WorkflowError, StudySpatialLimits] =
    if maximumSpatialVoxels <= 0 || maximumVoxelsPerRead <= 0 then
      Left(WorkflowError.InvalidValue("spatial support limits", s"$maximumSpatialVoxels/$maximumVoxelsPerRead", "must be positive"))
    else Right(new StudySpatialLimits(maximumSpatialVoxels, maximumVoxelsPerRead))

/** One source mask and the acquired runs to which the catalog assigns it. */
final class StudyMaskCoverage private[workflow] (
    val artifact: WorkflowArtifactRef[MaskImageResource],
    val runs: Vector[RunId],
    val selectedVoxels: Int,
    val nonfiniteVoxels: Int
)

final class StudyUnitCoverage private[workflow] (
    val unit: FirstLevelUnit,
    val masks: Vector[StudyMaskCoverage],
    val selectedVoxels: Int
)

/** Read width is an enforced request bound, not an estimate of total JVM heap. */
final class StudySpatialReads private[workflow] (
    val limits: StudySpatialLimits,
    val maskSourcesOpened: Long,
    val requests: Long,
    val largestReadVoxels: Int
)

final class StudySpatialSupport private (
    val catalog: StudyCatalog,
    val selection: ImageVoxelSelection,
    val units: Vector[StudyUnitCoverage],
    val reads: StudySpatialReads
):
  /** Bind the common absolute voxel indices only to an unchanged captured unit. */
  def selectionFor(unit: FirstLevelUnit): Either[WorkflowError, VoxelSelection] =
    if !catalog.units.contains(unit) then
      Left(WorkflowError.InvalidUnit(unit.id.value, "does not match the spatial-support catalog"))
    else VoxelSelection.fromImage(selection, unit.shape)
      .left.map(error => WorkflowError.InvalidUnit(unit.id.value, error.message))

object StudySpatialSupport:
  /** Strict intersection; finite nonzero mask values denote available voxels.
    * Each source is opened and scanned before the next source is opened.
    * Storage held here is two grid-sized Boolean arrays, one bounded response
    * block, immutable output indices and scalar coverage receipts. Source
    * implementations own their own indexing/decoder/staging storage.
    */
  def compile(catalog: StudyCatalog, limits: StudySpatialLimits)(
      openMask: WorkflowArtifactRef[MaskImageResource] => Either[DatasetError, ResponseBlockSource]
  ): Either[WorkflowError, StudySpatialSupport] =
    if catalog.units.isEmpty then return Left(WorkflowError.InvalidCatalog("spatial support requires at least one analysis unit"))
    val first = catalog.units.head
    val size = first.shape.spatialSize
    if size > limits.maximumSpatialVoxels then
      return Left(WorkflowError.InvalidCatalog(s"spatial grid has $size voxels; limit is ${limits.maximumSpatialVoxels}"))
    // Complete catalog validation precedes any mask opener invocation.
    var unitIndex = 0
    while unitIndex < catalog.units.size do
      val unit = catalog.units(unitIndex)
      if unit.space != first.space || unit.shape.space != first.shape.space then
        return Left(WorkflowError.InvalidUnit(unit.id.value, "named space or spatial geometry differs from the common grid"))
      unit.mask match
        case _: UnitMask.Single => ()
        case masks: UnitMask.Intersection =>
          if masks.runMasks.map(_._1).toSet != unit.runIds.toSet then
            return Left(WorkflowError.InvalidUnit(unit.id.value, "run-mask references must cover exactly the unit's run ids"))
      unitIndex += 1
    val common = Array.fill(size)(true)
    val withinUnit = new Array[Boolean](size)
    val coverage = Vector.newBuilder[StudyUnitCoverage]
    var opened = 0L
    var requests = 0L
    var largest = 0
    unitIndex = 0
    while unitIndex < catalog.units.size do
      val unit = catalog.units(unitIndex)
      var voxel = 0
      while voxel < size do
        withinUnit(voxel) = true
        voxel += 1
      val sources = unit.mask match
        case UnitMask.Single(artifact) => Vector(unit.runIds -> artifact)
        case masks: UnitMask.Intersection =>
          val byRun = masks.runMasks.toMap
          unit.runIds.map(run => Vector(run) -> byRun(run))
      val maskCoverage = Vector.newBuilder[StudyMaskCoverage]
      var maskIndex = 0
      while maskIndex < sources.size do
        val (runs, artifact) = sources(maskIndex)
        def failure(reason: String): WorkflowError =
          WorkflowError.InvalidValue("mask", artifact.location.value, s"unit ${unit.id.value}: $reason")
        val source = openMask(artifact) match
          case Left(error) => return Left(failure(error.message))
          case Right(value) => value
        opened += 1
        if source.shape.space != first.shape.space || source.shape.timepoints != 1 then
          return Left(failure("expected one mask volume on the common spatial grid"))
        if !source.voxelDomain.isFullSpatial || source.voxelDomain.spatialSize != size then
          return Left(failure("mask reader must expose the full spatial domain"))
        var offset = 0
        var selected = 0
        var nonfinite = 0
        while offset < size do
          val count = math.min(limits.maximumVoxelsPerRead, size - offset)
          val indices = Vector.tabulate(count)(i => offset + i)
          val requested = DataSelection(voxels = VoxelSelection.indices(indices*))
          val block = source.readBlock(requested) match
            case Left(error) => return Left(failure(error.message))
            case Right(value) => value
          requests += 1
          largest = math.max(largest, count)
          if block.shape != source.shape || block.timepoints != Vector(0) || block.voxelIndices != indices then
            return Left(failure("mask block does not preserve the requested geometry, timepoint and voxel order"))
          var column = 0
          while column < count do
            val value = block.data(0, column)
            val included = value.isFinite && value != 0.0
            if included then selected += 1
            else withinUnit(offset + column) = false
            if !value.isFinite then nonfinite += 1
            column += 1
          offset += count
        maskCoverage += new StudyMaskCoverage(artifact, runs, selected, nonfinite)
        maskIndex += 1
      voxel = 0
      var unitCount = 0
      while voxel < size do
        if withinUnit(voxel) then unitCount += 1 else common(voxel) = false
        voxel += 1
      coverage += new StudyUnitCoverage(unit, maskCoverage.result(), unitCount)
      unitIndex += 1
    val indices = Array.newBuilder[Int]
    var voxel = 0
    while voxel < size do
      if common(voxel) then indices += voxel
      voxel += 1
    val selected = indices.result()
    if selected.isEmpty then Left(WorkflowError.InvalidCatalog("the study mask intersection is empty"))
    else ImageVoxelSelection.make(first.shape.space, selected)
      .left.map(error => WorkflowError.InvalidCatalog(error.message))
      .map(selection => new StudySpatialSupport(catalog, selection, coverage.result(),
        new StudySpatialReads(limits, opened, requests, largest)))
