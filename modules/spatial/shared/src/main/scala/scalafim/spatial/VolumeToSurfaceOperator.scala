package scalafim.spatial

import scalafim.image.{Affine, GridSpec, Indexing, NeuroSpace, NeuroVol, SpatialDims, SpatialPoint}
import gale.linalg.LinAlgError
import scalafim.surface.{SurfaceGeometry, SurfaceGeometryPair, SurfaceRoi, SurfaceSamplingPath, VertexId, VolumeSurfaceSamplingPlan}

import scala.collection.mutable.ArrayBuffer

final class VolumeToSurfaceRequest private (
  val source: DomainId,
  val target: DomainId,
  val surfaces: SurfaceGeometryPair,
  val path: SurfaceSamplingPath,
  val sampling: SamplingPolicy,
  val rowSelection: RowSelection,
  val routing: RoutingPolicy,
  val allowInverses: Boolean
):
  def roi: Option[Vector[Int]] =
    rowSelection.roi

object VolumeToSurfaceRequest:
  def apply(
    source: DomainId,
    target: DomainId,
    surfaces: SurfaceGeometryPair,
    path: SurfaceSamplingPath = SurfaceSamplingPath.Midpoint,
    sampling: SamplingPolicy = SamplingPolicy.Nearest,
    roi: Option[Vector[Int]] = None,
    routing: RoutingPolicy = RoutingPolicy.Shortest,
    allowInverses: Boolean = false
  ): VolumeToSurfaceRequest =
    new VolumeToSurfaceRequest(
      source,
      target,
      surfaces,
      path,
      sampling,
      RowSelection.unsafeFromRoi(roi),
      routing,
      allowInverses
    )

  def forRows(
    source: DomainId,
    target: DomainId,
    surfaces: SurfaceGeometryPair,
    rowSelection: RowSelection,
    path: SurfaceSamplingPath = SurfaceSamplingPath.Midpoint,
    sampling: SamplingPolicy = SamplingPolicy.Nearest,
    routing: RoutingPolicy = RoutingPolicy.Shortest,
    allowInverses: Boolean = false
  ): VolumeToSurfaceRequest =
    new VolumeToSurfaceRequest(source, target, surfaces, path, sampling, rowSelection, routing, allowInverses)

  def midpoint(
    source: DomainId,
    target: DomainId,
    surfaces: SurfaceGeometryPair,
    sampling: SamplingPolicy = SamplingPolicy.Nearest,
    roi: Option[Vector[Int]] = None
  ): VolumeToSurfaceRequest =
    VolumeToSurfaceRequest(source, target, surfaces, SurfaceSamplingPath.Midpoint, sampling, roi)

  def ribbon(
    source: DomainId,
    target: DomainId,
    surfaces: SurfaceGeometryPair,
    fractions: Vector[Double],
    sampling: SamplingPolicy = SamplingPolicy.Nearest,
    roi: Option[Vector[Int]] = None
  ): VolumeToSurfaceRequest =
    VolumeToSurfaceRequest(source, target, surfaces, SurfaceSamplingPath.FractionalThickness(fractions), sampling, roi)

object VolumeToSurfaceOperatorCompiler:
  private val CompilerName = "volume-to-surface-v1"

  def compile(graph: SpatialGraph, request: VolumeToSurfaceRequest): Either[SpatialError, SpatialOperator] =
    for
      targetDomain <- graph.domain(request.target)
      target <- surfaceTarget(targetDomain)
      _ <- validateSurfacePair(targetDomain.id, target.geometry, request.surfaces)
      path <- graph.path(request.source, request.target, request.routing, request.allowInverses)
      _ <- validateMixedRoute(path)
      plan = VolumeSurfaceSamplingPlan(request.surfaces, request.path)
      bridgeCompiler = RequestVolumeToSurfacePullbackCompiler(plan)
      registry <- MorphismCompilerRegistry.build(
        Vector(
          MorphismPullbackCompiler.identity,
          MorphismPullbackCompiler.affine3D,
          MorphismPullbackCompiler.warp3D,
          bridgeCompiler,
          MorphismPullbackCompiler.surfaceToSurface
        )
      )
      program <- PullbackProgram.compile(
        graph,
        CompileRequest.forRows(
          source = request.source,
          target = request.target,
          rowSelection = request.rowSelection,
          routing = request.routing,
          sampling = request.sampling,
          allowInverses = request.allowInverses
        ),
        registry
      )
      compilerName = s"$CompilerName:${CoordinateMap.volumeSamples(plan).fingerprint}"
      operator <- MixedPullbackOperatorCompiler.compile(program, compilerName)
    yield operator

  private def validateMixedRoute(path: MorphismPath): Either[SpatialError, Unit] =
    path.morphisms.find { morphism =>
      morphism.kind != MorphismKind.Identity &&
      morphism.kind != MorphismKind.Affine3D &&
      morphism.kind != MorphismKind.Warp3D &&
      morphism.kind != MorphismKind.VolumeToSurface &&
      morphism.kind != MorphismKind.SurfaceToSurface
    } match
      case Some(morphism) =>
        Left(SpatialError.UnsupportedMorphismForCompilation(morphism.id, morphism.kind))
      case None =>
        val bridges = path.morphisms.count(_.kind == MorphismKind.VolumeToSurface)
        if bridges == 1 then Right(())
        else Left(SpatialError.InvalidMixedPullback(s"expected one volume-to-surface bridge, got $bridges"))

  private def volumeSource(domain: Domain): Either[SpatialError, VolumeSource] =
    domain.geometry match
      case SamplingGeometry.Volume(space, mask) =>
        Right(VolumeSource(space.spatialSpace, mask))
      case _ =>
        Left(SpatialError.NonVolumeDomain(domain.id))

  private def surfaceTarget(domain: Domain): Either[SpatialError, SurfaceTarget] =
    domain.geometry match
      case SamplingGeometry.Surface(geometry, mask) =>
        Right(SurfaceTarget(geometry, mask))
      case _ =>
        Left(SpatialError.NonSurfaceDomain(domain.id))

  private def validateSurfacePair(
    target: DomainId,
    geometry: SurfaceGeometry,
    surfaces: SurfaceGeometryPair
  ): Either[SpatialError, Unit] =
    if geometry.vertexCount != surfaces.white.vertexCount || geometry.hemisphere != surfaces.white.hemisphere then
      Left(SpatialError.SurfacePairMismatch(target))
    else Right(())

  private def assembleRows(
    source: VolumeSource,
    target: SurfaceTarget,
    targetRows: TargetRows,
    request: VolumeToSurfaceRequest
  ): Either[SpatialError, SurfaceRowAssembly] =
    val sourceGrid = GridSpec.fromSpace(source.space)
    val assembly = SurfaceRowAssembly()

    var outRow = 0
    var error = Option.empty[SpatialError]
    while outRow < targetRows.length && error.isEmpty do
      val targetRow = targetRows.indices(outRow)
      val vertex = VertexId(targetRow)
      rowWeights(sourceGrid, source.mask, target.mask, request.surfaces, request.path, request.sampling, vertex) match
        case Left(err) =>
          error = Some(err)
        case Right(weights) =>
          assembly.coverage += weights.coverage
          var i = 0
          while i < weights.cols.length do
            assembly.rowIndices += outRow
            assembly.colIndices += weights.cols(i)
            assembly.values += weights.values(i)
            i += 1
      outRow += 1

    error match
      case Some(err) => Left(err)
      case None => Right(assembly)

  private def rowWeights(
    sourceGrid: GridSpec,
    sourceMask: Option[NeuroVol[Boolean]],
    targetMask: Option[SurfaceRoi[Boolean]],
    surfaces: SurfaceGeometryPair,
    path: SurfaceSamplingPath,
    sampling: SamplingPolicy,
    vertex: VertexId
  ): Either[SpatialError, SurfaceRowWeights] =
    if !surfaceMaskAllows(targetMask, vertex) then Right(SurfaceRowWeights.empty)
    else
      samplePoints(surfaces, path, vertex).map { points =>
        val pointWeights = Vector.newBuilder[SurfacePointWeights]
        var coverageSum = 0.0
        var i = 0
        while i < points.length do
          val weights = sourcePointWeights(sourceGrid, sourceMask, points(i), sampling)
          pointWeights += weights
          coverageSum += weights.coverage
          i += 1

        val weights = pointWeights.result()
        val valid = weights.filter(_.coverage > 0.0)
        if valid.isEmpty then SurfaceRowWeights.empty.copy(coverage = coverageSum / points.length.toDouble)
        else
          val cols = ArrayBuffer.empty[Int]
          val values = ArrayBuffer.empty[Double]
          val scale = 1.0 / valid.length.toDouble
          valid.foreach { point =>
            var j = 0
            while j < point.cols.length do
              cols += point.cols(j)
              values += point.values(j) * scale
              j += 1
          }
          SurfaceRowWeights(cols.toVector, values.toVector, coverageSum / points.length.toDouble)
      }

  private[spatial] def sourcePointWeights(
    sourceGrid: GridSpec,
    sourceMask: Option[NeuroVol[Boolean]],
    point: SpatialPoint,
    sampling: SamplingPolicy
  ): SurfacePointWeights =
    sourceGrid.worldToVoxel(point) match
      case Left(_) =>
        SurfacePointWeights.empty
      case Right(voxel) =>
        sampling match
          case SamplingPolicy.Nearest =>
            nearestWeights(sourceGrid.shape, sourceMask, voxel)
          case SamplingPolicy.Trilinear =>
            trilinearWeights(sourceGrid.shape, sourceMask, voxel)

  private def nearestWeights(
    sourceDims: SpatialDims,
    sourceMask: Option[NeuroVol[Boolean]],
    voxel: SpatialPoint
  ): SurfacePointWeights =
    val x = math.round(voxel.x).toInt
    val y = math.round(voxel.y).toInt
    val z = math.round(voxel.z).toInt
    if inBounds(sourceDims, x, y, z) then
      val col = Indexing.gridToIndex3D(sourceDims, x, y, z)
      if sourceMask.forall(_.linear(col)) then SurfacePointWeights(Vector(col), Vector(1.0), 1.0)
      else SurfacePointWeights.empty
    else SurfacePointWeights.empty

  private def trilinearWeights(
    sourceDims: SpatialDims,
    sourceMask: Option[NeuroVol[Boolean]],
    voxel: SpatialPoint
  ): SurfacePointWeights =
    val x0 = math.floor(voxel.x).toInt
    val y0 = math.floor(voxel.y).toInt
    val z0 = math.floor(voxel.z).toInt
    val xd = voxel.x - x0.toDouble
    val yd = voxel.y - y0.toDouble
    val zd = voxel.z - z0.toDouble

    val cols = ArrayBuffer.empty[Int]
    val values = ArrayBuffer.empty[Double]
    var coverage = 0.0

    coverage += addTrilinearCorner(sourceDims, sourceMask, x0, y0, z0, (1.0 - xd) * (1.0 - yd) * (1.0 - zd), cols, values)
    coverage += addTrilinearCorner(sourceDims, sourceMask, x0 + 1, y0, z0, xd * (1.0 - yd) * (1.0 - zd), cols, values)
    coverage += addTrilinearCorner(sourceDims, sourceMask, x0, y0 + 1, z0, (1.0 - xd) * yd * (1.0 - zd), cols, values)
    coverage += addTrilinearCorner(sourceDims, sourceMask, x0 + 1, y0 + 1, z0, xd * yd * (1.0 - zd), cols, values)
    coverage += addTrilinearCorner(sourceDims, sourceMask, x0, y0, z0 + 1, (1.0 - xd) * (1.0 - yd) * zd, cols, values)
    coverage += addTrilinearCorner(sourceDims, sourceMask, x0 + 1, y0, z0 + 1, xd * (1.0 - yd) * zd, cols, values)
    coverage += addTrilinearCorner(sourceDims, sourceMask, x0, y0 + 1, z0 + 1, (1.0 - xd) * yd * zd, cols, values)
    coverage += addTrilinearCorner(sourceDims, sourceMask, x0 + 1, y0 + 1, z0 + 1, xd * yd * zd, cols, values)

    if coverage > 0.0 then
      var i = 0
      while i < values.length do
        values(i) = values(i) / coverage
        i += 1

    SurfacePointWeights(cols.toVector, values.toVector, coverage)

  private def addTrilinearCorner(
    dims: SpatialDims,
    mask: Option[NeuroVol[Boolean]],
    x: Int,
    y: Int,
    z: Int,
    weight: Double,
    cols: ArrayBuffer[Int],
    values: ArrayBuffer[Double]
  ): Double =
    if weight == 0.0 || !inBounds(dims, x, y, z) then 0.0
    else
      val col = Indexing.gridToIndex3D(dims, x, y, z)
      if mask.exists(m => !m.linear(col)) then 0.0
      else
        cols += col
        values += weight
        weight

  private[spatial] def samplePoints(
    surfaces: SurfaceGeometryPair,
    path: SurfaceSamplingPath,
    vertex: VertexId
  ): Either[SpatialError, Vector[SpatialPoint]] =
    for
      white <- worldPoint(surfaces.white, vertex)
      pial <- worldPoint(surfaces.pial, vertex)
      delta = subtract(pial, white)
      points <- path match
        case SurfaceSamplingPath.White =>
          Right(Vector(white))
        case SurfaceSamplingPath.Pial =>
          Right(Vector(pial))
        case SurfaceSamplingPath.Midpoint =>
          Right(Vector(add(white, scale(delta, 0.5))))
        case SurfaceSamplingPath.FractionalThickness(fractions) =>
          if fractions.isEmpty || fractions.exists(f => !f.isFinite || f < 0.0 || f > 1.0) then
            Left(SpatialError.UnsupportedSurfaceSampling("fractional thickness"))
          else Right(fractions.map(f => add(white, scale(delta, f))))
        case SurfaceSamplingPath.NormalLine(offsets) =>
          if offsets.isEmpty || offsets.exists(offset => !offset.isFinite) then
            Left(SpatialError.UnsupportedSurfaceSampling("normal line"))
          else
            val midpoint = add(white, scale(delta, 0.5))
            val n = norm(delta)
            val unit = if n == 0.0 then SpatialPoint.Origin else scale(delta, 1.0 / n)
            Right(offsets.map(offset => add(midpoint, scale(unit, offset))))
    yield points

  private def worldPoint(surface: SurfaceGeometry, vertex: VertexId): Either[SpatialError, SpatialPoint] =
    val point = surface.mesh.vertex(vertex)
    SpatialPoint
      .fromVector(Affine.applyAffine(surface.surfaceToWorld, point.toVector), "surface sample point")
      .left.map(err => SpatialError.CoordinateTransformFailed(err.message))

  private[spatial] def surfaceMaskAllows(mask: Option[SurfaceRoi[Boolean]], vertex: VertexId): Boolean =
    mask match
      case None =>
        true
      case Some(roi) =>
        var i = 0
        var allowed = false
        while i < roi.indices.length do
          if roi.indices(i) == vertex.index then
            allowed = roi.data(i)
            i = roi.indices.length
          else i += 1
        allowed

  private inline def inBounds(dims: SpatialDims, x: Int, y: Int, z: Int): Boolean =
    x >= 0 && x < dims.x &&
      y >= 0 && y < dims.y &&
      z >= 0 && z < dims.z

  private def add(a: SpatialPoint, b: SpatialPoint): SpatialPoint =
    SpatialPoint(a.x + b.x, a.y + b.y, a.z + b.z)

  private def subtract(a: SpatialPoint, b: SpatialPoint): SpatialPoint =
    SpatialPoint(a.x - b.x, a.y - b.y, a.z - b.z)

  private def scale(a: SpatialPoint, value: Double): SpatialPoint =
    SpatialPoint(a.x * value, a.y * value, a.z * value)

  private def norm(a: SpatialPoint): Double =
    math.sqrt(a.x * a.x + a.y * a.y + a.z * a.z)

  private def linearError(error: LinAlgError): SpatialError =
    SpatialError.OperatorAssemblyFailed(error.getMessage)

private final case class VolumeSource(space: NeuroSpace, mask: Option[NeuroVol[Boolean]])

private final case class SurfaceTarget(geometry: SurfaceGeometry, mask: Option[SurfaceRoi[Boolean]])

private[spatial] final case class SurfacePointWeights(cols: Vector[Int], values: Vector[Double], coverage: Double)

private object SurfacePointWeights:
  val empty: SurfacePointWeights =
    SurfacePointWeights(Vector.empty, Vector.empty, 0.0)

private final case class SurfaceRowWeights(cols: Vector[Int], values: Vector[Double], coverage: Double)

private object SurfaceRowWeights:
  val empty: SurfaceRowWeights =
    SurfaceRowWeights(Vector.empty, Vector.empty, 0.0)

private final case class SurfaceRowAssembly(
  rowIndices: ArrayBuffer[Int] = ArrayBuffer.empty[Int],
  colIndices: ArrayBuffer[Int] = ArrayBuffer.empty[Int],
  values: ArrayBuffer[Double] = ArrayBuffer.empty[Double],
  coverage: ArrayBuffer[Double] = ArrayBuffer.empty[Double]
)

private final class RequestVolumeToSurfacePullbackCompiler(
  plan: scalafim.surface.VolumeSurfaceSamplingPlan
) extends MorphismPullbackCompiler:
  override val id: MorphismCompilerId =
    MorphismCompilerId.unsafe("volume-to-surface-request-pullback-v1")

  override val kind: MorphismKind =
    MorphismKind.VolumeToSurface

  override def lower(
    morphism: Morphism,
    source: Domain,
    target: Domain
  ): Either[SpatialError, Vector[PullbackStep]] =
    if morphism.kind != kind then
      Left(SpatialError.MorphismCompilerKindMismatch(id.value, kind, morphism.kind))
    else
      for
        _ <- Morphism.validateDomains(morphism, source, target)
        _ <- target.geometry match
          case SamplingGeometry.Surface(geometry, _) if geometry == plan.surfaces.white => Right(())
          case _ => Left(SpatialError.SurfaceSamplingGeometryMismatch(morphism.id))
        step <- PullbackStep.build(morphism, id, CoordinateMap.volumeSamples(plan))
      yield Vector(step)

object MixedPullbackOperatorCompiler:
  private val CompilerName = "mixed-pullback-fused-v1"

  def compile(program: PullbackProgram): Either[SpatialError, SpatialOperator] =
    compile(program, CompilerName)

  private[spatial] def compile(
    program: PullbackProgram,
    compilerName: String
  ): Either[SpatialError, SpatialOperator] =
    (program.route.source.kind, program.route.target.kind) match
      case (DomainKind.Volume, DomainKind.Surface) =>
        compileVolumeRoot(program, compilerName)
      case (DomainKind.Surface, DomainKind.Surface) =>
        compileSurfaceRoot(program, compilerName)
      case (source, target) =>
        Left(SpatialError.InvalidMixedPullback(s"unsupported root/target transition $source->$target"))

  private def compileVolumeRoot(
    program: PullbackProgram,
    compilerName: String
  ): Either[SpatialError, SpatialOperator] =
    val bridges = program.steps.zipWithIndex.collect {
      case (step, index) if step.morphism.kind == MorphismKind.VolumeToSurface => index
    }
    if bridges.length != 1 then
      Left(SpatialError.InvalidMixedPullback(s"expected one volume-to-surface bridge, got ${bridges.length}"))
    else
      val bridgeIndex = bridges.head
      for
        plan <- program.steps(bridgeIndex).coordinateMap match
          case CoordinateMap.VolumeSamples(value) => Right(value)
          case _ => Left(SpatialError.InvalidMixedPullback("volume-to-surface bridge has no executable sampling plan"))
        _ <- validateVolumePrefix(program.steps.take(bridgeIndex))
        _ <- validateSurfaceSuffix(program.steps.drop(bridgeIndex + 1))
        source <- volumeSource(program.route.source)
        target <- surfaceTarget(program.route.target)
        assembly <- assembleVolumeRoot(program, bridgeIndex, plan, source, target)
        operator <- buildOperator(program, assembly, compilerName)
      yield operator

  private def compileSurfaceRoot(
    program: PullbackProgram,
    compilerName: String
  ): Either[SpatialError, SpatialOperator] =
    for
      _ <- validateSurfaceSuffix(program.steps)
      source <- surfaceTarget(program.route.source)
      target <- surfaceTarget(program.route.target)
      assembly <- assembleSurfaceRoot(program, source, target)
      operator <- buildOperator(program, assembly, compilerName)
    yield operator

  private def assembleVolumeRoot(
    program: PullbackProgram,
    bridgeIndex: Int,
    plan: scalafim.surface.VolumeSurfaceSamplingPlan,
    source: VolumeSource,
    target: SurfaceTarget
  ): Either[SpatialError, MixedRowAssembly] =
    val sourceGrid = GridSpec.fromSpace(source.space)
    val assembly = MixedRowAssembly()
    val rows = program.route.targetRows
    var outRow = 0
    var error = Option.empty[SpatialError]

    while outRow < rows.length && error.isEmpty do
      val targetVertex = VertexId(rows.indices(outRow))
      if !VolumeToSurfaceOperatorCompiler.surfaceMaskAllows(target.mask, targetVertex) then
        assembly.coverage += 0.0
      else
        pullSurfaceVertex(program.steps, bridgeIndex + 1, targetVertex) match
          case Left(err) => error = Some(err)
          case Right(bridgeVertex) =>
            VolumeToSurfaceOperatorCompiler.samplePoints(plan.surfaces, plan.path, bridgeVertex) match
              case Left(err) => error = Some(err)
              case Right(points) =>
                val pointWeights = Vector.newBuilder[SurfacePointWeights]
                var coverageSum = 0.0
                var pointIndex = 0
                while pointIndex < points.length && error.isEmpty do
                  pullVolumePoint(program.steps, bridgeIndex, points(pointIndex)) match
                    case Left(err) => error = Some(err)
                    case Right(rootPoint) =>
                      val weights =
                        VolumeToSurfaceOperatorCompiler.sourcePointWeights(
                          sourceGrid,
                          source.mask,
                          rootPoint,
                          program.route.sampling
                        )
                      pointWeights += weights
                      coverageSum += weights.coverage
                  pointIndex += 1

                if error.isEmpty then
                  appendAveragedWeights(
                    assembly,
                    outRow,
                    pointWeights.result(),
                    coverageSum / points.length.toDouble
                  )
      outRow += 1

    error match
      case Some(err) => Left(err)
      case None => Right(assembly)

  private def assembleSurfaceRoot(
    program: PullbackProgram,
    source: SurfaceTarget,
    target: SurfaceTarget
  ): Either[SpatialError, MixedRowAssembly] =
    val assembly = MixedRowAssembly()
    val rows = program.route.targetRows
    var outRow = 0
    var error = Option.empty[SpatialError]
    while outRow < rows.length && error.isEmpty do
      val targetVertex = VertexId(rows.indices(outRow))
      if !VolumeToSurfaceOperatorCompiler.surfaceMaskAllows(target.mask, targetVertex) then
        assembly.coverage += 0.0
      else
        pullSurfaceVertex(program.steps, 0, targetVertex) match
          case Left(err) => error = Some(err)
          case Right(rootVertex) =>
            if VolumeToSurfaceOperatorCompiler.surfaceMaskAllows(source.mask, rootVertex) then
              assembly.rowIndices += outRow
              assembly.colIndices += rootVertex.index
              assembly.values += 1.0
              assembly.coverage += 1.0
            else assembly.coverage += 0.0
      outRow += 1
    error match
      case Some(err) => Left(err)
      case None => Right(assembly)

  private def appendAveragedWeights(
    assembly: MixedRowAssembly,
    outRow: Int,
    weights: Vector[SurfacePointWeights],
    coverage: Double
  ): Unit =
    val valid = weights.filter(_.coverage > 0.0)
    assembly.coverage += coverage
    if valid.nonEmpty then
      val scale = 1.0 / valid.length.toDouble
      var point = 0
      while point < valid.length do
        var index = 0
        while index < valid(point).cols.length do
          assembly.rowIndices += outRow
          assembly.colIndices += valid(point).cols(index)
          assembly.values += valid(point).values(index) * scale
          index += 1
        point += 1

  private def pullVolumePoint(
    steps: Vector[PullbackStep],
    bridgeIndex: Int,
    point: SpatialPoint
  ): Either[SpatialError, SpatialPoint] =
    var current = point
    var index = bridgeIndex - 1
    var error = Option.empty[SpatialError]
    while index >= 0 && error.isEmpty do
      steps(index).coordinateMap.transform(current) match
        case Left(err) => error = Some(err)
        case Right(next) => current = next
      index -= 1
    error.toLeft(current)

  private def pullSurfaceVertex(
    steps: Vector[PullbackStep],
    start: Int,
    target: VertexId
  ): Either[SpatialError, VertexId] =
    var current = target
    var index = steps.length - 1
    while index >= start do
      steps(index).coordinateMap match
        case CoordinateMap.SurfaceVertices(mapping) =>
          if current.index < 0 || current.index >= mapping.sourceForTarget.length then
            return Left(SpatialError.InvalidMixedPullback(s"surface target vertex ${current.index} is out of bounds"))
          current = mapping.sourceForTarget(current.index)
        case other =>
          return Left(SpatialError.InvalidMixedPullback(s"expected surface vertex mapping, got $other"))
      index -= 1
    Right(current)

  private def validateVolumePrefix(steps: Vector[PullbackStep]): Either[SpatialError, Unit] =
    steps.find { step =>
      step.morphism.kind != MorphismKind.Affine3D && step.morphism.kind != MorphismKind.Warp3D
    } match
      case Some(step) => Left(SpatialError.InvalidMixedPullback(s"${step.morphism.kind} appears before the surface bridge"))
      case None => Right(())

  private def validateSurfaceSuffix(steps: Vector[PullbackStep]): Either[SpatialError, Unit] =
    steps.find(_.morphism.kind != MorphismKind.SurfaceToSurface) match
      case Some(step) => Left(SpatialError.InvalidMixedPullback(s"${step.morphism.kind} appears after the surface bridge"))
      case None => Right(())

  private def buildOperator(
    program: PullbackProgram,
    assembly: MixedRowAssembly,
    compilerName: String
  ): Either[SpatialError, SpatialOperator] =
    val route = program.route
    val rows = route.targetRows
    for
      csr <- GaleSpatialSupport.sparseCsr(
        rows = rows.length,
        cols = route.source.nElements,
        rowIndices = assembly.rowIndices.toArray,
        colIndices = assembly.colIndices.toArray,
        values = assembly.values.toArray
      ).left.map(linearError)
      coverage <- CoverageReport.build(rows, assembly.coverage.toVector)
      recipe <- OperatorRecipe.build(
        path = route.path.ids,
        routing = route.routing,
        sampling = route.sampling,
        rowSelection = rows.selection,
        allowInverses = route.allowInverses,
        compiler = compilerName
      )
      operator <- SpatialOperator.build(
        source = route.source.id,
        target = route.target.id,
        map = csr,
        path = route.path,
        qc = OperatorQc(coverage, route.path.pathQuality),
        provenance = OperatorProvenance.fromRecipe(recipe)
      )
    yield operator

  private def volumeSource(domain: Domain): Either[SpatialError, VolumeSource] =
    domain.geometry match
      case SamplingGeometry.Volume(space, mask) => Right(VolumeSource(space.spatialSpace, mask))
      case _ => Left(SpatialError.NonVolumeDomain(domain.id))

  private def surfaceTarget(domain: Domain): Either[SpatialError, SurfaceTarget] =
    domain.geometry match
      case SamplingGeometry.Surface(geometry, mask) => Right(SurfaceTarget(geometry, mask))
      case _ => Left(SpatialError.NonSurfaceDomain(domain.id))

  private def linearError(error: LinAlgError): SpatialError =
    SpatialError.OperatorAssemblyFailed(error.getMessage)

private final case class MixedRowAssembly(
  rowIndices: ArrayBuffer[Int] = ArrayBuffer.empty[Int],
  colIndices: ArrayBuffer[Int] = ArrayBuffer.empty[Int],
  values: ArrayBuffer[Double] = ArrayBuffer.empty[Double],
  coverage: ArrayBuffer[Double] = ArrayBuffer.empty[Double]
)
