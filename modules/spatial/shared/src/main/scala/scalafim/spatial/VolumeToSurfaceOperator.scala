package scalafim.spatial

import scalafim.image.{Affine, GridSpec, Indexing, NeuroSpace, NeuroVol, SpatialDims, SpatialPoint}
import scalafim.linalg.{CsrMatrix, LinearMapError, SparseTriplets}
import scalafim.surface.{SurfaceGeometry, SurfaceGeometryPair, SurfaceRoi, SurfaceSamplingPath, VertexId}

import scala.collection.mutable.ArrayBuffer

final case class VolumeToSurfaceRequest(
  source: DomainId,
  target: DomainId,
  surfaces: SurfaceGeometryPair,
  path: SurfaceSamplingPath = SurfaceSamplingPath.Midpoint,
  sampling: SamplingPolicy = SamplingPolicy.Nearest,
  roi: Option[Vector[Int]] = None,
  routing: RoutingPolicy = RoutingPolicy.Shortest,
  allowInverses: Boolean = false
)

object VolumeToSurfaceRequest:
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
      _ <- requireVolumeToSurfacePath(path)
      rows <- selectedTargetRows(request.roi, targetDomain.nElements)
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

  private def requireVolumeToSurfacePath(path: MorphismPath): Either[SpatialError, Unit] =
    if path.morphisms.length == 1 && path.morphisms.head.kind == MorphismKind.VolumeToSurface then Right(())
    else
      val offending = path.morphisms.find(_.kind != MorphismKind.Identity).getOrElse(path.morphisms.head)
      Left(SpatialError.UnsupportedMorphismForCompilation(offending.id, offending.kind))

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

  private def assembleRows(
    source: VolumeSource,
    target: SurfaceTarget,
    targetRows: Vector[Int],
    request: VolumeToSurfaceRequest
  ): Either[SpatialError, SurfaceRowAssembly] =
    val sourceGrid = GridSpec.fromSpace(source.space)
    val assembly = SurfaceRowAssembly()

    var outRow = 0
    var error = Option.empty[SpatialError]
    while outRow < targetRows.length && error.isEmpty do
      val targetRow = targetRows(outRow)
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
    point: Vector[Double],
    sampling: SamplingPolicy
  ): SurfacePointWeights =
    sourceGrid.worldToVoxel(SpatialPoint.unsafeFromVector(point, "surface sample point")) match
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
  ): Either[SpatialError, Vector[Vector[Double]]] =
    val white = worldPoint(surfaces.white, vertex)
    val pial = worldPoint(surfaces.pial, vertex)
    val delta = subtract(pial, white)

    path match
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
          val unit = if n == 0.0 then Vector(0.0, 0.0, 0.0) else scale(delta, 1.0 / n)
          Right(offsets.map(offset => add(midpoint, scale(unit, offset))))

  private def worldPoint(surface: SurfaceGeometry, vertex: VertexId): Vector[Double] =
    val point = surface.mesh.vertex(vertex)
    Affine.applyAffine(surface.surfaceToWorld, Vector(point.x, point.y, point.z))

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

  private def add(a: Vector[Double], b: Vector[Double]): Vector[Double] =
    Vector.tabulate(3)(i => a(i) + b(i))

  private def subtract(a: Vector[Double], b: Vector[Double]): Vector[Double] =
    Vector.tabulate(3)(i => a(i) - b(i))

  private def scale(a: Vector[Double], value: Double): Vector[Double] =
    Vector.tabulate(3)(i => a(i) * value)

  private def norm(a: Vector[Double]): Double =
    math.sqrt(a.map(x => x * x).sum)

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
