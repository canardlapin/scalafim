package scalafim.spatial

import scalafim.image.{Affine, GridSpec, Indexing, NeuroSpace, SpatialDims}
import scalafim.linalg.{CsrMatrix, DoubleMatrix, LinearMap, LinearMapError, SparseTriplets}

import scala.collection.mutable.ArrayBuffer

enum SamplingPolicy:
  case Nearest, Trilinear

final case class CompileRequest(
  source: DomainId,
  target: DomainId,
  routing: RoutingPolicy = RoutingPolicy.Shortest,
  sampling: SamplingPolicy = SamplingPolicy.Trilinear,
  roi: Option[Vector[Int]] = None,
  allowInverses: Boolean = false
)

final case class CoverageReport private (
  targetRows: Vector[Int],
  rowCoverage: Vector[Double]
):
  require(targetRows.length == rowCoverage.length, "target rows and coverage must have equal length")
  require(rowCoverage.forall(value => value.isFinite && value >= 0.0 && value <= 1.0), "coverage must be in [0, 1]")

  def totalRows: Int =
    targetRows.length

  def coveredRows: Int =
    rowCoverage.count(_ > 0.0)

  def uncoveredTargetRows: Vector[Int] =
    targetRows.zip(rowCoverage).collect { case (row, coverage) if coverage == 0.0 => row }

  def fraction: Double =
    if totalRows == 0 then 0.0 else coveredRows.toDouble / totalRows.toDouble

object CoverageReport:
  def build(targetRows: Vector[Int], rowCoverage: Vector[Double]): Either[SpatialError, CoverageReport] =
    if targetRows.length != rowCoverage.length then
      Left(SpatialError.OperatorAssemblyFailed("coverage length does not match target row count"))
    else if rowCoverage.exists(value => !value.isFinite || value < 0.0 || value > 1.0) then
      Left(SpatialError.OperatorAssemblyFailed("coverage values must be finite and in [0, 1]"))
    else Right(new CoverageReport(targetRows, rowCoverage))

final case class OperatorQc(
  coverage: CoverageReport,
  pathQuality: Double
)

final case class OperatorProvenance(
  path: Vector[MorphismId],
  routing: RoutingPolicy,
  sampling: SamplingPolicy,
  roi: Option[Vector[Int]],
  allowInverses: Boolean,
  compiler: String
)

final case class SpatialOperator private (
  source: DomainId,
  target: DomainId,
  map: LinearMap,
  path: MorphismPath,
  qc: OperatorQc,
  provenance: OperatorProvenance
):
  require(map.rows == qc.coverage.totalRows, "operator row count must match coverage report")

  def rows: Int =
    map.rows

  def cols: Int =
    map.cols

  def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    map.forward(input)

object SpatialOperator:
  def build(
    source: DomainId,
    target: DomainId,
    map: LinearMap,
    path: MorphismPath,
    qc: OperatorQc,
    provenance: OperatorProvenance
  ): Either[SpatialError, SpatialOperator] =
    if map.rows != qc.coverage.totalRows then
      Left(SpatialError.OperatorAssemblyFailed("operator row count does not match coverage report"))
    else Right(new SpatialOperator(source, target, map, path, qc, provenance))

trait OperatorCompiler:
  def compile(graph: SpatialGraph, request: CompileRequest): Either[SpatialError, SpatialOperator]

object OperatorCompiler:
  val volumeAffine: OperatorCompiler =
    VolumeAffineOperatorCompiler

  def compile(graph: SpatialGraph, request: CompileRequest): Either[SpatialError, SpatialOperator] =
    volumeAffine.compile(graph, request)

object VolumeAffineOperatorCompiler extends OperatorCompiler:
  private val CompilerName = "volume-affine-v1"

  override def compile(graph: SpatialGraph, request: CompileRequest): Either[SpatialError, SpatialOperator] =
    for
      sourceDomain <- graph.domain(request.source)
      targetDomain <- graph.domain(request.target)
      sourceSpace <- volumeSpace(sourceDomain)
      targetSpace <- volumeSpace(targetDomain)
      path <- graph.path(request.source, request.target, request.routing, request.allowInverses)
      coordinateMap <- pathCoordinateMap(path)
      rows <- selectedTargetRows(request.roi, targetDomain.nElements)
      rowAssembly <- assembleRows(sourceSpace, targetSpace, rows, coordinateMap, request.sampling)
      triplets <- SparseTriplets(
        rows = rows.length,
        cols = sourceDomain.nElements,
        rowIndices = rowAssembly.rowIndices.toArray,
        colIndices = rowAssembly.colIndices.toArray,
        values = rowAssembly.values.toArray
      ).left.map(mapLinearError)
      csr <- CsrMatrix.fromTriplets(triplets).left.map(mapLinearError)
      coverage <- CoverageReport.build(rows, rowAssembly.coverage.toVector)
      operator <- SpatialOperator.build(
        source = request.source,
        target = request.target,
        map = csr,
        path = path,
        qc = OperatorQc(coverage, path.pathQuality),
        provenance = OperatorProvenance(
          path = path.ids,
          routing = request.routing,
          sampling = request.sampling,
          roi = request.roi,
          allowInverses = request.allowInverses,
          compiler = CompilerName
        )
      )
    yield operator

  private def volumeSpace(domain: Domain): Either[SpatialError, NeuroSpace] =
    domain.geometry match
      case SamplingGeometry.Volume(space, _) => Right(space.spatialSpace)
      case _ => Left(SpatialError.NonVolumeDomain(domain.id))

  private def selectedTargetRows(roi: Option[Vector[Int]], targetRows: Int): Either[SpatialError, Vector[Int]] =
    roi match
      case None =>
        Right(Vector.tabulate(targetRows)(identity))
      case Some(rows) =>
        if rows.isEmpty then Left(SpatialError.EmptyRoi)
        else
          val seen = scala.collection.mutable.HashSet.empty[Int]
          var i = 0
          var error = Option.empty[SpatialError]
          while i < rows.length && error.isEmpty do
            val row = rows(i)
            if row < 0 || row >= targetRows then error = Some(SpatialError.InvalidRoiRow(row, targetRows))
            else if seen.contains(row) then error = Some(SpatialError.DuplicateRoiRow(row))
            else seen += row
            i += 1
          error match
            case Some(err) => Left(err)
            case None => Right(rows)

  private def pathCoordinateMap(path: MorphismPath): Either[SpatialError, CoordinateMap] =
    var matrix = scalafim.image.DMat.eye(4)
    var sawAffine = false
    var i = path.morphisms.length - 1
    var error = Option.empty[SpatialError]
    while i >= 0 && error.isEmpty do
      val morphism = path.morphisms(i)
      morphism.coordinateMap match
        case CoordinateMap.Identity =>
          if morphism.kind != MorphismKind.Identity then
            error = Some(SpatialError.UnsupportedMorphismForCompilation(morphism.id, morphism.kind))
        case CoordinateMap.Affine3D(step) =>
          if morphism.kind != MorphismKind.Affine3D then
            error = Some(SpatialError.UnsupportedMorphismForCompilation(morphism.id, morphism.kind))
          else
            matrix = Affine.multiply(step, matrix)
            sawAffine = true
        case CoordinateMap.Unspecified =>
          error = Some(SpatialError.MissingCoordinateMap(morphism.id))
      i -= 1

    error match
      case Some(err) => Left(err)
      case None =>
        if sawAffine then Right(CoordinateMap.Affine3D(matrix)) else Right(CoordinateMap.Identity)

  private def assembleRows(
    sourceSpace: NeuroSpace,
    targetSpace: NeuroSpace,
    targetRows: Vector[Int],
    coordinateMap: CoordinateMap,
    sampling: SamplingPolicy
  ): Either[SpatialError, RowAssembly] =
    val sourceGrid = GridSpec.fromSpace(sourceSpace)
    val targetGrid = GridSpec.fromSpace(targetSpace)
    val sourceDims = sourceGrid.shape
    val targetDims = targetGrid.shape
    val assembly = RowAssembly()

    var outRow = 0
    var error = Option.empty[SpatialError]
    while outRow < targetRows.length && error.isEmpty do
      val targetIndex = targetRows(outRow)
      val targetVoxel = Indexing.indexToGrid3D(targetDims, targetIndex)
      val targetWorld =
        targetGrid.voxelToWorld(Vector(targetVoxel.x.toDouble, targetVoxel.y.toDouble, targetVoxel.z.toDouble))
      coordinateMap.transform(targetWorld) match
        case Left(err) =>
          error = Some(err)
        case Right(sourceWorld) =>
          sourceGrid.worldToVoxel(sourceWorld) match
            case Left(err) =>
              error = Some(SpatialError.CoordinateTransformFailed(err.message))
            case Right(sourceVoxel) =>
              val weights =
                sampling match
                  case SamplingPolicy.Nearest =>
                    nearestWeights(sourceDims, sourceVoxel)
                  case SamplingPolicy.Trilinear =>
                    trilinearWeights(sourceDims, sourceVoxel)
              assembly.coverage += weights.coverage
              var j = 0
              while j < weights.cols.length do
                assembly.rowIndices += outRow
                assembly.colIndices += weights.cols(j)
                assembly.values += weights.values(j)
                j += 1
      outRow += 1

    error match
      case Some(err) => Left(err)
      case None => Right(assembly)

  private def nearestWeights(sourceDims: SpatialDims, voxel: Vector[Double]): RowWeights =
    val x = math.round(voxel(0)).toInt
    val y = math.round(voxel(1)).toInt
    val z = math.round(voxel(2)).toInt
    if inBounds(sourceDims, x, y, z) then
      RowWeights(Vector(Indexing.gridToIndex3D(sourceDims, x, y, z)), Vector(1.0), 1.0)
    else RowWeights.empty

  private def trilinearWeights(sourceDims: SpatialDims, voxel: Vector[Double]): RowWeights =
    val x0 = math.floor(voxel(0)).toInt
    val y0 = math.floor(voxel(1)).toInt
    val z0 = math.floor(voxel(2)).toInt
    val xd = voxel(0) - x0.toDouble
    val yd = voxel(1) - y0.toDouble
    val zd = voxel(2) - z0.toDouble

    val cols = ArrayBuffer.empty[Int]
    val values = ArrayBuffer.empty[Double]
    var coverage = 0.0

    addTrilinearCorner(sourceDims, x0, y0, z0, (1.0 - xd) * (1.0 - yd) * (1.0 - zd), cols, values) match
      case Some(weight) => coverage += weight
      case None => ()
    addTrilinearCorner(sourceDims, x0 + 1, y0, z0, xd * (1.0 - yd) * (1.0 - zd), cols, values) match
      case Some(weight) => coverage += weight
      case None => ()
    addTrilinearCorner(sourceDims, x0, y0 + 1, z0, (1.0 - xd) * yd * (1.0 - zd), cols, values) match
      case Some(weight) => coverage += weight
      case None => ()
    addTrilinearCorner(sourceDims, x0 + 1, y0 + 1, z0, xd * yd * (1.0 - zd), cols, values) match
      case Some(weight) => coverage += weight
      case None => ()
    addTrilinearCorner(sourceDims, x0, y0, z0 + 1, (1.0 - xd) * (1.0 - yd) * zd, cols, values) match
      case Some(weight) => coverage += weight
      case None => ()
    addTrilinearCorner(sourceDims, x0 + 1, y0, z0 + 1, xd * (1.0 - yd) * zd, cols, values) match
      case Some(weight) => coverage += weight
      case None => ()
    addTrilinearCorner(sourceDims, x0, y0 + 1, z0 + 1, (1.0 - xd) * yd * zd, cols, values) match
      case Some(weight) => coverage += weight
      case None => ()
    addTrilinearCorner(sourceDims, x0 + 1, y0 + 1, z0 + 1, xd * yd * zd, cols, values) match
      case Some(weight) => coverage += weight
      case None => ()

    if coverage > 0.0 then
      var i = 0
      while i < values.length do
        values(i) = values(i) / coverage
        i += 1

    RowWeights(cols.toVector, values.toVector, coverage)

  private def addTrilinearCorner(
    dims: SpatialDims,
    x: Int,
    y: Int,
    z: Int,
    weight: Double,
    cols: ArrayBuffer[Int],
    values: ArrayBuffer[Double]
  ): Option[Double] =
    if weight == 0.0 || !inBounds(dims, x, y, z) then None
    else
      cols += Indexing.gridToIndex3D(dims, x, y, z)
      values += weight
      Some(weight)

  private inline def inBounds(dims: SpatialDims, x: Int, y: Int, z: Int): Boolean =
    x >= 0 && x < dims.x &&
      y >= 0 && y < dims.y &&
      z >= 0 && z < dims.z

  private def mapLinearError(error: LinearMapError): SpatialError =
    SpatialError.OperatorAssemblyFailed(error.message)

private final case class RowWeights(cols: Vector[Int], values: Vector[Double], coverage: Double)

private object RowWeights:
  val empty: RowWeights =
    RowWeights(Vector.empty, Vector.empty, 0.0)

private final case class RowAssembly(
  rowIndices: ArrayBuffer[Int] = ArrayBuffer.empty[Int],
  colIndices: ArrayBuffer[Int] = ArrayBuffer.empty[Int],
  values: ArrayBuffer[Double] = ArrayBuffer.empty[Double],
  coverage: ArrayBuffer[Double] = ArrayBuffer.empty[Double]
)
