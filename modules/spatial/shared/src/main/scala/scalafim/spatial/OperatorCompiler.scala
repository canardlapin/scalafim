package scalafim.spatial

import scalafim.image.{Affine, GridSpec, Indexing, NeuroSpace, SpatialDims, SpatialPoint}
import scalafim.linalg.{CsrMatrix, DoubleMatrix, LinearMap, LinearMapError, SparseTriplets}

import scala.collection.mutable.ArrayBuffer

enum SamplingPolicy:
  case Nearest, Trilinear

final class CompileRequest private (
  val source: DomainId,
  val target: DomainId,
  val routing: RoutingPolicy,
  val sampling: SamplingPolicy,
  val rowSelection: RowSelection,
  val allowInverses: Boolean
):
  def roi: Option[Vector[Int]] =
    rowSelection.roi

object CompileRequest:
  def apply(
    source: DomainId,
    target: DomainId,
    routing: RoutingPolicy = RoutingPolicy.Shortest,
    sampling: SamplingPolicy = SamplingPolicy.Trilinear,
    roi: Option[Vector[Int]] = None,
    allowInverses: Boolean = false
  ): CompileRequest =
    new CompileRequest(source, target, routing, sampling, RowSelection.unsafeFromRoi(roi), allowInverses)

  def forRows(
    source: DomainId,
    target: DomainId,
    rowSelection: RowSelection,
    routing: RoutingPolicy = RoutingPolicy.Shortest,
    sampling: SamplingPolicy = SamplingPolicy.Trilinear,
    allowInverses: Boolean = false
  ): CompileRequest =
    new CompileRequest(source, target, routing, sampling, rowSelection, allowInverses)

final case class CoverageReport private (
  rows: TargetRows,
  rowCoverage: Vector[Double]
):
  require(rows.length == rowCoverage.length, "target rows and coverage must have equal length")
  require(rowCoverage.forall(value => value.isFinite && value >= 0.0 && value <= 1.0), "coverage must be in [0, 1]")

  def targetRows: Vector[Int] =
    rows.indices

  def totalRows: Int =
    rows.length

  def coveredRows: Int =
    rowCoverage.count(_ > 0.0)

  def uncoveredTargetRows: Vector[Int] =
    targetRows.zip(rowCoverage).collect { case (row, coverage) if coverage == 0.0 => row }

  def fraction: Double =
    if totalRows == 0 then 0.0 else coveredRows.toDouble / totalRows.toDouble

object CoverageReport:
  def build(targetRows: TargetRows, rowCoverage: Vector[Double]): Either[SpatialError, CoverageReport] =
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
  recipe: OperatorRecipe
):
  def path: Vector[MorphismId] =
    recipe.path

  def routing: RoutingPolicy =
    recipe.routing

  def sampling: SamplingPolicy =
    recipe.sampling

  def rowSelection: RowSelection =
    recipe.rowSelection

  def roi: Option[Vector[Int]] =
    recipe.roi

  def allowInverses: Boolean =
    recipe.allowInverses

  def compiler: String =
    recipe.compiler

object OperatorProvenance:
  def apply(
    path: Vector[MorphismId],
    routing: RoutingPolicy,
    sampling: SamplingPolicy,
    roi: Option[Vector[Int]],
    allowInverses: Boolean,
    compiler: String
  ): OperatorProvenance =
    new OperatorProvenance(
      OperatorRecipe.unsafe(
        path,
        routing,
        sampling,
        RowSelection.unsafeFromRoi(roi),
        allowInverses,
        compiler
      )
    )

  def fromRecipe(recipe: OperatorRecipe): OperatorProvenance =
    new OperatorProvenance(recipe)

final case class SpatialOperator private (
  source: DomainId,
  target: DomainId,
  map: LinearMap,
  path: MorphismPath,
  qc: OperatorQc,
  signature: OperatorSignature,
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
    else if source != path.source || target != path.target then
      Left(SpatialError.OperatorAssemblyFailed("operator domains do not match morphism path"))
    else if provenance.path != path.ids then
      Left(SpatialError.OperatorAssemblyFailed("operator provenance path does not match morphism path"))
    else
      for
        shape <- OperatorShape.build(map.rows, map.cols)
        signature <- OperatorSignature.build(source, target, shape, provenance.recipe)
      yield new SpatialOperator(source, target, map, path, qc, signature, provenance)

trait OperatorCompiler:
  def compile(graph: SpatialGraph, request: CompileRequest): Either[SpatialError, SpatialOperator]

object OperatorCompiler:
  val volumeAffine: OperatorCompiler =
    VolumeAffineOperatorCompiler

  val volumePullback: OperatorCompiler =
    VolumePullbackOperatorCompiler

  def compile(graph: SpatialGraph, request: CompileRequest): Either[SpatialError, SpatialOperator] =
    volumePullback.compile(graph, request)

object VolumePullbackOperatorCompiler extends OperatorCompiler:
  private val AffineCompilerName = "affine-pullback-fused-v1"
  private val GeneralCompilerName = "volume-pullback-fused-v1"

  override def compile(graph: SpatialGraph, request: CompileRequest): Either[SpatialError, SpatialOperator] =
    PullbackProgram.compile(graph, request).flatMap(compile)

  def compile(program: PullbackProgram): Either[SpatialError, SpatialOperator] =
    if program.valueStages.nonEmpty then StagedOperatorCompiler.compile(program)
    else compileCoordinateOnly(program)

  private[spatial] def compileCoordinateOnly(program: PullbackProgram): Either[SpatialError, SpatialOperator] =
    if program.route.source.kind == DomainKind.Volume && program.route.target.kind == DomainKind.Volume then
      val compilerName =
        if program.steps.exists(_.morphism.kind == MorphismKind.Warp3D) then GeneralCompilerName
        else AffineCompilerName
      compileAs(program, compilerName)
    else MixedPullbackOperatorCompiler.compile(program)

  private[spatial] def compileAs(
    program: PullbackProgram,
    compilerName: String
  ): Either[SpatialError, SpatialOperator] =
    val route = program.route
    val sourceDomain = route.source
    val targetDomain = route.target
    val path = route.path
    val rows = route.targetRows
    for
      sourceSpace <- volumeSpace(sourceDomain)
      targetSpace <- volumeSpace(targetDomain)
      rowAssembly <- assembleRows(sourceSpace, targetSpace, rows, program, route.sampling)
      triplets <- SparseTriplets(
        rows = rows.length,
        cols = sourceDomain.nElements,
        rowIndices = rowAssembly.rowIndices.toArray,
        colIndices = rowAssembly.colIndices.toArray,
        values = rowAssembly.values.toArray
      ).left.map(mapLinearError)
      csr <- CsrMatrix.fromTriplets(triplets).left.map(mapLinearError)
      coverage <- CoverageReport.build(rows, rowAssembly.coverage.toVector)
      recipe <- OperatorRecipe.build(
        path = path.ids,
        routing = route.routing,
        sampling = route.sampling,
        rowSelection = rows.selection,
        allowInverses = route.allowInverses,
        compiler = compilerName
      )
      operator <- SpatialOperator.build(
        source = sourceDomain.id,
        target = targetDomain.id,
        map = csr,
        path = path,
        qc = OperatorQc(coverage, path.pathQuality),
        provenance = OperatorProvenance.fromRecipe(recipe)
      )
    yield operator

  private def volumeSpace(domain: Domain): Either[SpatialError, NeuroSpace] =
    domain.geometry match
      case SamplingGeometry.Volume(space, _) => Right(space.spatialSpace)
      case _ => Left(SpatialError.NonVolumeDomain(domain.id))

  private def assembleRows(
    sourceSpace: NeuroSpace,
    targetSpace: NeuroSpace,
    targetRows: TargetRows,
    program: PullbackProgram,
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
      val targetIndex = targetRows.indices(outRow)
      val targetVoxel = Indexing.indexToGrid3D(targetDims, targetIndex)
      val targetWorld =
        targetGrid.voxelToWorld(SpatialPoint(targetVoxel.x.toDouble, targetVoxel.y.toDouble, targetVoxel.z.toDouble))
      program.pullback(targetWorld) match
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

  private def nearestWeights(sourceDims: SpatialDims, voxel: SpatialPoint): RowWeights =
    val x = math.round(voxel.x).toInt
    val y = math.round(voxel.y).toInt
    val z = math.round(voxel.z).toInt
    if inBounds(sourceDims, x, y, z) then
      RowWeights(Vector(Indexing.gridToIndex3D(sourceDims, x, y, z)), Vector(1.0), 1.0)
    else RowWeights.empty

  private def trilinearWeights(sourceDims: SpatialDims, voxel: SpatialPoint): RowWeights =
    val x0 = math.floor(voxel.x).toInt
    val y0 = math.floor(voxel.y).toInt
    val z0 = math.floor(voxel.z).toInt
    val xd = voxel.x - x0.toDouble
    val yd = voxel.y - y0.toDouble
    val zd = voxel.z - z0.toDouble

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

object VolumeAffineOperatorCompiler extends OperatorCompiler:
  private val CompilerName = "affine-pullback-fused-v1"

  override def compile(graph: SpatialGraph, request: CompileRequest): Either[SpatialError, SpatialOperator] =
    PullbackProgram.compile(graph, request).flatMap(compile)

  def compile(program: PullbackProgram): Either[SpatialError, SpatialOperator] =
    program.route.path.morphisms.find { morphism =>
      morphism.kind != MorphismKind.Identity && morphism.kind != MorphismKind.Affine3D
    } match
      case Some(morphism) =>
        Left(SpatialError.UnsupportedMorphismForCompilation(morphism.id, morphism.kind))
      case None =>
        VolumePullbackOperatorCompiler.compileAs(program, CompilerName)

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
