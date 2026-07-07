package scalafim.spatial

import scalafim.image.{Affine, GridSpec, Indexing, NeuroSpace, NeuroVol, SpatialDims, SpatialPoint}
import scalafim.linalg.{CsrMatrix, LinearMapError, SparseTriplets}
import scalafim.surface.{SurfaceGeometry, SurfaceGeometryPair, SurfaceRoi, SurfaceSamplingPath, VertexId}

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
      sourceDomain <- graph.domain(request.source)
      targetDomain <- graph.domain(request.target)
      source <- volumeSource(sourceDomain)
      target <- surfaceTarget(targetDomain)
      _ <- validateSurfacePair(targetDomain.id, target.geometry, request.surfaces)
      path <- graph.path(request.source, request.target, request.routing, request.allowInverses)
      volumeToSurfacePath <- VolumeToSurfacePath.from(path)
      rows <- TargetRows.fromSelection(request.rowSelection, targetDomain.nElements)
      rowAssembly <- assembleRows(source, target, rows, request)
      triplets <- SparseTriplets(
        rows = rows.length,
        cols = sourceDomain.nElements,
        rowIndices = rowAssembly.rowIndices.toArray,
        colIndices = rowAssembly.colIndices.toArray,
        values = rowAssembly.values.toArray
      ).left.map(linearError)
      csr <- CsrMatrix.fromTriplets(triplets).left.map(linearError)
      coverage <- CoverageReport.build(rows, rowAssembly.coverage.toVector)
      recipe <- OperatorRecipe.build(
        path = volumeToSurfacePath.ids,
        routing = request.routing,
        sampling = request.sampling,
        rowSelection = rows.selection,
        allowInverses = request.allowInverses,
        compiler = CompilerName
      )
      operator <- SpatialOperator.build(
        source = request.source,
        target = request.target,
        map = csr,
        path = path,
        qc = OperatorQc(coverage, path.pathQuality),
        provenance = OperatorProvenance.fromRecipe(recipe)
      )
    yield operator

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

  private def sourcePointWeights(
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

  private def samplePoints(
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

  private def surfaceMaskAllows(mask: Option[SurfaceRoi[Boolean]], vertex: VertexId): Boolean =
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

  private def linearError(error: LinearMapError): SpatialError =
    SpatialError.OperatorAssemblyFailed(error.message)

private final case class VolumeSource(space: NeuroSpace, mask: Option[NeuroVol[Boolean]])

private final case class SurfaceTarget(geometry: SurfaceGeometry, mask: Option[SurfaceRoi[Boolean]])

private final case class SurfacePointWeights(cols: Vector[Int], values: Vector[Double], coverage: Double)

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
