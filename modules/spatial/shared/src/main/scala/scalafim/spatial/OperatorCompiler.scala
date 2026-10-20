package scalafim.spatial

import image4s.SomeSampleSpace
import neuroim4s.image.GridSpec

import neuroim4s.spatial.{SamplingPolicy, VolumeSampling, VolumeSamplingError}
import neuroim4s.image.SampleSpaces.*
import gale.linalg.{DMat, DoubleLinearOperator, LinAlgError}


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

/** How an operator was made. `usedInverses` records whether its route ran any morphism backwards through a geometric
  * inverse; [[SpatialOperator.build]] sets it from the operator's path, which is the authority.
  */
final case class OperatorProvenance(
  recipe: OperatorRecipe,
  usedInverses: Boolean = false
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
  map: DoubleLinearOperator,
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

  def forward(input: DMat): Either[LinAlgError, DMat] =
    map.applyTo(input)

object SpatialOperator:
  def build(
    source: DomainId,
    target: DomainId,
    map: DoubleLinearOperator,
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
      yield new SpatialOperator(source, target, map, path, qc, signature, provenance.copy(usedInverses = path.usedInverses))

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
      sampled <- VolumeSampling.assemble(GridSpec.fromSpace(sourceSpace), GridSpec.fromSpace(targetSpace), rows.indices, program.pullback, route.sampling)
        .left.map:
          case VolumeSamplingError.Pullback(error) => error
          case VolumeSamplingError.Geometry(error) => SpatialError.CoordinateTransformFailed(error.message)
          case VolumeSamplingError.Assembly(error) => SpatialError.OperatorAssemblyFailed(error.getMessage)
          case VolumeSamplingError.InvalidRows(reason) => SpatialError.OperatorAssemblyFailed(reason)
      coverage <- CoverageReport.build(rows, sampled.coverage)
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
        map = sampled.map,
        path = path,
        qc = OperatorQc(coverage, path.pathQuality),
        provenance = OperatorProvenance.fromRecipe(recipe)
      )
    yield operator

  private def volumeSpace(domain: Domain): Either[SpatialError, SomeSampleSpace] =
    domain.geometry match
      case SamplingGeometry.Volume(volumeGrid) =>
        val space = volumeGrid.space
        Right(space.typed.spatialOnly)
      case _ => Left(SpatialError.NonVolumeDomain(domain.id))

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
