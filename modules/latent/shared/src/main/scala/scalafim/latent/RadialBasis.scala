package scalafim.latent

import scalafim.archive.lna.{SharedBasisArtifact, SharedBasisId, SharedBasisLocator, SharedBasisMask}
import scalafim.image.{DMat as ArchiveDMat}
import scalafim.image.{Indexing, NArrayUtil, NeuroSpace, VoxelIndexSet}
import gale.linalg.{DMat, DVec}

final case class RadialActiveVoxels private (
    indexSet: VoxelIndexSet,
    coordinates: Vector[WorldCoordinate3D]
):
  require(!indexSet.isEmpty, "radial basis active index set must be non-empty")
  require(coordinates.length == indexSet.size, "active coordinate count must match index set size")

  def size: Int = indexSet.size
  def indices: Vector[Int] = indexSet.toVector

object RadialActiveVoxels:
  def fromIndices(
      space: NeuroSpace,
      activeIndices: IndexedSeq[Int]
  ): Either[RadialBasisError, RadialActiveVoxels] =
    VoxelIndexSet
      .makeUnique(space, NArrayUtil.fromArray(activeIndices.toArray))
      .left
      .map(error => RadialBasisError.InvalidActiveVoxelIndices(error.message))
      .flatMap(fromIndexSet)

  def fromIndexSet(indexSet: VoxelIndexSet): Either[RadialBasisError, RadialActiveVoxels] =
    if indexSet.isEmpty then Left(RadialBasisError.EmptyActiveCoordinates)
    else
      val space = indexSet.space.toNeuroSpace
      val out = Vector.newBuilder[WorldCoordinate3D]
      out.sizeHint(indexSet.size)
      var i = 0
      var error = Option.empty[RadialBasisError]

      while i < indexSet.size && error.isEmpty do
        val grid = space.indexToGrid3D(indexSet(i)).map(_.toDouble)
        val world = space.indexToCoord(grid)
        WorldCoordinate3D(world(0), world(1), world(2)) match
          case Left(err)      => error = Some(err)
          case Right(coord)   => out += coord
        i += 1

      error match
        case Some(err) => Left(err)
        case None      => Right(RadialActiveVoxels(indexSet, out.result()))

final case class RadialBasis(
    kernel: RadialKernel,
    threshold: RadialValueThreshold,
    atoms: Vector[RadialAtom],
    activeIndices: Option[Vector[Int]],
    activeCoordinates: Vector[WorldCoordinate3D],
    loadings: DMat
):
  require(atoms.nonEmpty, "radial basis atom set must be non-empty")
  require(activeCoordinates.nonEmpty, "radial basis active coordinates must be non-empty")
  require(activeIndices.forall(_.length == activeCoordinates.length), "active index count must match active coordinates")
  require(loadings.rows == activeCoordinates.length, "radial basis loading rows must match active coordinates")
  require(loadings.cols == atoms.length, "radial basis loading columns must match atoms")

  def nVoxels: Int = activeCoordinates.length
  def nAtoms: Int = atoms.length
  def atomLevels: Vector[Int] = atoms.map(_.level)

  def activeMaskOrder: Either[RadialBasisError, RadialMaskOrder] =
    activeIndices
      .toRight(RadialBasisError.MissingActiveIndices)
      .flatMap(RadialMaskOrder.fromActiveIndices)

  def metadata: Map[String, String] =
    Map(
      "family" -> "radial_basis",
      "kernel" -> kernel.metadataValue,
      "threshold" -> threshold.metadataValue,
      "n_atoms" -> nAtoms.toString,
      "n_voxels" -> nVoxels.toString
    )

  def sharedBasisParams(extra: Map[String, String] = Map.empty): Map[String, String] =
    extra ++ Map(
      "family" -> "hrbf",
      "radial.kernel" -> kernel.metadataValue,
      "radial.threshold" -> threshold.metadataValue,
      "radial.n_atoms" -> nAtoms.toString,
      "radial.n_voxels" -> nVoxels.toString,
      "radial.atom_levels" -> atomLevels.mkString(",")
    )

  def toSharedBasisArtifact(
      maskDims: Vector[Int],
      kind: String = "hrbf",
      params: Map[String, String] = Map.empty,
      created: Option[String] = None
  ): Either[RadialBasisError, SharedBasisArtifact] =
    for
      order <- activeMaskOrder
      maskSize <- checkedMaskSize(maskDims)
      maskValues <- order.maskValues(maskSize)
      mask <- SharedBasisMask
        .checked(maskDims, maskValues)
        .left
        .map(error => RadialBasisError.InvalidMaskDimensions(error.message))
      loadings = canonicalDMat(order)
      artifact <- SharedBasisArtifact
        .checked(
          loadings = loadings,
          mask = mask,
          kind = kind,
          params = sharedBasisParams(params),
          created = created
        )
        .left
        .map(error => RadialBasisError.InvalidSharedBasisArtifact(error.message))
    yield artifact

  def decode(
      coefficients: DMat,
      selection: RadialDecodeSelection = RadialDecodeSelection.All,
      offset: Option[DVec] = None
  ): Either[LatentError, DMat] =
    for
      _ <- validateDecodeInputs(coefficients, offset)
      timeIndices <- resolveTimepoints(selection.timepoints, coefficients.rows)
      voxelRows <- resolveVoxelRows(selection.voxels)
    yield decodeResolved(coefficients, timeIndices, voxelRows, offset)

  private[latent] def dataInMaskOrder(data: DMat): Either[LatentError, DMat] =
    if data.cols != nVoxels then Left(LatentError.DimensionMismatch("radial basis data columns", nVoxels, data.cols))
    else activeMaskOrderLatent.map(order => selectColumns(data, order.activeRowsInMaskOrder))

  private[latent] def vectorInActiveOrderFromMaskOrder(values: DVec): Either[LatentError, DVec] =
    activeMaskOrderLatent.flatMap(_.vectorInActiveOrderFromMaskOrder(values))

  private[latent] def activeRowsInMaskOrder: Either[LatentError, Vector[Int]] =
    activeMaskOrderLatent.map(_.activeRowsInMaskOrder)

  private def activeMaskOrderLatent: Either[LatentError, RadialMaskOrder] =
    activeIndices match
      case Some(indices) =>
        RadialMaskOrder
          .fromActiveIndices(indices)
          .left
          .map(error => LatentError.ProjectionFailed(error.message))
      case None =>
        Left(LatentError.MissingComponent("radial active-index map"))

  private def canonicalDMat(order: RadialMaskOrder): ArchiveDMat =
    ArchiveDMat.fromRows(
      loadings
        .selectRows(order.maskOrderSelection.ordinals.toVector)
        .toRows
    )

  private def selectColumns(matrix: DMat, columns: IndexedSeq[Int]): DMat =
    val out = new Array[Double](matrix.rows * columns.length)
    var row = 0
    while row < matrix.rows do
      var outCol = 0
      while outCol < columns.length do
        out(row * columns.length + outCol) = matrix(row, columns(outCol))
        outCol += 1
      row += 1
    LatentNumerics.matrixFromRowMajor(matrix.rows, columns.length, out)

  private def checkedMaskSize(maskDims: Vector[Int]): Either[RadialBasisError, Int] =
    if maskDims.isEmpty then Left(RadialBasisError.InvalidMaskDimensions("mask dimensions must be non-empty"))
    else if maskDims.exists(_ <= 0) then Left(RadialBasisError.InvalidMaskDimensions("mask dimensions must be positive"))
    else Right(maskDims.product)

  private def validateDecodeInputs(
      coefficients: DMat,
      offset: Option[DVec]
  ): Either[LatentError, Unit] =
    if coefficients.rows <= 0 then Left(LatentError.NonPositiveDimension("radial coefficient rows", coefficients.rows))
    else if coefficients.cols != nAtoms then Left(LatentError.DimensionMismatch("radial coefficient columns", nAtoms, coefficients.cols))
    else
      offset match
        case Some(value) if value.length != nVoxels =>
          Left(LatentError.DimensionMismatch("radial offset length", nVoxels, value.length))
        case _ =>
          firstNonFinite("radial coefficients", coefficients)
            .orElse(offset.flatMap(value => firstNonFinite("radial offset", value))) match
            case Some(error) => Left(error)
            case None        => Right(())

  private def resolveTimepoints(
      requested: Option[IndexedSeq[TimepointIndex]],
      timepointCount: Int
  ): Either[LatentError, IndexedSeq[Int]] =
    resolvePlainSelection("timepoint", timepointCount, requested.map(_.map(_.value)))

  private def resolveVoxelRows(
      selection: RadialVoxelSelection
  ): Either[LatentError, IndexedSeq[Int]] =
    selection match
      case RadialVoxelSelection.AllActive =>
        Right(0 until nVoxels)
      case RadialVoxelSelection.Active(indices) =>
        resolvePlainSelection("active voxel", nVoxels, Some(indices.map(_.value)))
      case RadialVoxelSelection.FullGrid(indices) =>
        activeMaskOrderLatent.flatMap(_.activeRowsForFullGrid(indices.map(_.value)))

  private def resolvePlainSelection(
      axis: String,
      count: Int,
      requested: Option[IndexedSeq[Int]]
  ): Either[LatentError, IndexedSeq[Int]] =
    if count <= 0 then Left(LatentError.NonPositiveDimension(axis, count))
    else
      requested match
        case None =>
          Right(0 until count)
        case Some(indices) =>
          if indices.isEmpty then Left(LatentError.EmptySelection(axis))
          else
            val seen = Array.fill(count)(false)
            var i = 0
            var error = Option.empty[LatentError]
            while i < indices.length && error.isEmpty do
              val index = indices(i)
              if index < 0 || index >= count then error = Some(LatentError.IndexOutOfBounds(axis, index, count))
              else if seen(index) then error = Some(LatentError.DuplicateSelection(axis, index))
              else seen(index) = true
              i += 1
            error match
              case Some(err) => Left(err)
              case None      => Right(indices)

  private def decodeResolved(
      coefficients: DMat,
      timeIndices: IndexedSeq[Int],
      voxelRows: IndexedSeq[Int],
      offset: Option[DVec]
  ): DMat =
    val out = new Array[Double](timeIndices.length * voxelRows.length)
    var outTime = 0
    while outTime < timeIndices.length do
      val time = timeIndices(outTime)
      var outVoxel = 0
      while outVoxel < voxelRows.length do
        val voxel = voxelRows(outVoxel)
        var sum = offset.fold(0.0)(_(voxel))
        var atom = 0
        while atom < nAtoms do
          sum += coefficients(time, atom) * loadings(voxel, atom)
          atom += 1
        out(outTime * voxelRows.length + outVoxel) = sum
        outVoxel += 1
      outTime += 1
    LatentNumerics.matrixFromRowMajor(timeIndices.length, voxelRows.length, out)

  private def firstNonFinite(label: String, matrix: DMat): Option[LatentError] =
    val data = matrix.copyData
    var i = 0
    var error = Option.empty[LatentError]
    while i < data.length && error.isEmpty do
      val value = data(i)
      if !value.isFinite then error = Some(LatentError.NonFiniteValue(label, i, value))
      i += 1
    error

  private def firstNonFinite(label: String, vector: DVec): Option[LatentError] =
    var i = 0
    var error = Option.empty[LatentError]
    while i < vector.length && error.isEmpty do
      val value = vector(i)
      if !value.isFinite then error = Some(LatentError.NonFiniteValue(label, i, value))
      i += 1
    error

final case class RadialBasisEncoding(
    encoding: SharedBasisEncoding,
    radialResponse: ExplicitLatentResponse,
    radialBasis: RadialBasis,
    artifact: SharedBasisArtifact,
    basisId: SharedBasisId,
    locator: Option[SharedBasisLocator]
):
  def response: ExplicitLatentResponse = radialResponse
  def coefficients: DMat = encoding.coefficients
  def offset: Option[DVec] = radialResponse.offset

  def decode(selection: RadialDecodeSelection = RadialDecodeSelection.All): Either[LatentError, DMat] =
    radialBasis.decode(coefficients, selection, offset)

object RadialBasisEncoder:
  def encode(
      data: DMat,
      radialBasis: RadialBasis,
      maskDims: Vector[Int],
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator] = None,
      center: Boolean = true,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("radial_basis.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("voxels"),
      label: String = "",
      metadata: Map[String, String] = Map.empty,
      artifactParams: Map[String, String] = Map.empty
  ): Either[LatentError, RadialBasisEncoding] =
    radialBasis
      .toSharedBasisArtifact(maskDims = maskDims, params = artifactParams)
      .left
      .map(radialError)
      .flatMap { artifact =>
        for
          canonicalData <- radialBasis.dataInMaskOrder(data)
          sharedEncoding <-
            SharedBasisEncoder.encode(
              data = canonicalData,
              basis = artifact,
              basisId = basisId,
              locator = locator,
              center = center,
              ridge = ridge,
              sourceDomain = sourceDomain,
              targetDomain = targetDomain,
              label = label,
              metadata = radialBasis.sharedBasisParams(metadata)
            )
          activeOffset <- sharedEncoding.offset match
            case Some(values) => radialBasis.vectorInActiveOrderFromMaskOrder(values).map(Some(_))
            case None         => Right(None)
          radialResponse <- ExplicitLatentResponse(
            basis = sharedEncoding.coefficients,
            loadings = radialBasis.loadings,
            offset = activeOffset,
            sourceDomain = sourceDomain,
            targetDomain = targetDomain,
            label = sharedEncoding.response.label,
            metadata = sharedEncoding.response.metadata
          )
        yield
          RadialBasisEncoding(
            encoding = sharedEncoding,
            radialResponse = radialResponse,
            radialBasis = radialBasis,
            artifact = artifact,
            basisId = basisId,
            locator = locator
          )
      }

  private def radialError(error: RadialBasisError): LatentError =
    LatentError.ProjectionFailed(error.message)

object RadialBasis:
  def fromSpec(
      space: NeuroSpace,
      activeIndices: IndexedSeq[Int],
      spec: RadialBasisSpec
  ): Either[RadialBasisError, RadialBasis] =
    RadialActiveVoxels
      .fromIndices(space, activeIndices)
      .flatMap(fromActiveVoxelsSpec(_, spec))

  def fromActiveVoxelsSpec(
      activeVoxels: RadialActiveVoxels,
      spec: RadialBasisSpec
  ): Either[RadialBasisError, RadialBasis] =
    for
      atoms <- RadialAtomGenerator.generate(activeVoxels, spec)
      basis <- fromActiveVoxels(
        atoms = atoms,
        activeVoxels = activeVoxels,
        kernel = spec.kernel,
        threshold = spec.threshold,
        normalizeAtoms = spec.normalizeAtoms
      )
    yield basis

  def fromSpaceIndices(
      atoms: IndexedSeq[RadialAtom],
      space: NeuroSpace,
      activeIndices: IndexedSeq[Int],
      kernel: RadialKernel,
      threshold: RadialValueThreshold = RadialValueThreshold.Zero,
      normalizeAtoms: Boolean = false
  ): Either[RadialBasisError, RadialBasis] =
    RadialActiveVoxels
      .fromIndices(space, activeIndices)
      .flatMap(fromActiveVoxels(atoms, _, kernel, threshold, normalizeAtoms))

  def fromActiveVoxels(
      atoms: IndexedSeq[RadialAtom],
      activeVoxels: RadialActiveVoxels,
      kernel: RadialKernel,
      threshold: RadialValueThreshold = RadialValueThreshold.Zero,
      normalizeAtoms: Boolean = false
  ): Either[RadialBasisError, RadialBasis] =
    fromAtoms(
      atoms = atoms,
      activeCoordinates = activeVoxels.coordinates,
      kernel = kernel,
      threshold = threshold,
      activeIndices = Some(activeVoxels.indices),
      normalizeAtoms = normalizeAtoms
    )

  def fromAtoms(
      atoms: IndexedSeq[RadialAtom],
      activeCoordinates: IndexedSeq[WorldCoordinate3D],
      kernel: RadialKernel,
      threshold: RadialValueThreshold = RadialValueThreshold.Zero,
      activeIndices: Option[Vector[Int]] = None,
      normalizeAtoms: Boolean = false
  ): Either[RadialBasisError, RadialBasis] =
    if atoms.isEmpty then Left(RadialBasisError.EmptyAtoms)
    else if activeCoordinates.isEmpty then Left(RadialBasisError.EmptyActiveCoordinates)
    else if activeIndices.exists(_.length != activeCoordinates.length) then
      Left(RadialBasisError.ActiveIndexCountMismatch(activeCoordinates.length, activeIndices.map(_.length).getOrElse(0)))
    else
      val atomVector = atoms.toVector
      val coordinateVector = activeCoordinates.toVector
      val values = new Array[Double](coordinateVector.length * atomVector.length)

      var voxel = 0
      while voxel < coordinateVector.length do
        val coordinate = coordinateVector(voxel)
        var atom = 0
        while atom < atomVector.length do
          val radialAtom = atomVector(atom)
          val value =
            kernel.valueUnchecked(coordinate.distanceTo(radialAtom.center), radialAtom.sigma)
          values(voxel * atomVector.length + atom) =
            if value >= threshold.value then value else 0.0
          atom += 1
        voxel += 1

      if normalizeAtoms then normalizeColumns(values, coordinateVector.length, atomVector.length)

      Right(
        RadialBasis(
          kernel = kernel,
          threshold = threshold,
          atoms = atomVector,
          activeIndices = activeIndices,
          activeCoordinates = coordinateVector,
          loadings = LatentNumerics.matrixFromRowMajor(coordinateVector.length, atomVector.length, values)
        )
      )

  private def normalizeColumns(values: Array[Double], rows: Int, cols: Int): Unit =
    var col = 0
    while col < cols do
      var norm2 = 0.0
      var row = 0
      while row < rows do
        val value = values(row * cols + col)
        norm2 += value * value
        row += 1

      if norm2 > 0.0 then
        val scale = 1.0 / math.sqrt(norm2)
        row = 0
        while row < rows do
          values(row * cols + col) *= scale
          row += 1
      col += 1

private object RadialAtomGenerator:
  private final case class Candidate(index: Int, coordinate: WorldCoordinate3D, grid: Vector[Int])

  private val NeighborOffsets6: Vector[(Int, Int, Int)] =
    Vector(
      (-1, 0, 0), (1, 0, 0),
      (0, -1, 0), (0, 1, 0),
      (0, 0, -1), (0, 0, 1)
    )

  def generate(
      activeVoxels: RadialActiveVoxels,
      spec: RadialBasisSpec
  ): Either[RadialBasisError, Vector[RadialAtom]] =
    val candidates = activeCandidates(activeVoxels)
    if candidates.isEmpty then Left(RadialBasisError.EmptyActiveCoordinates)
    else if spec.tinyMaskIdentity && candidates.length <= spec.tinyMaskLimit then
      Right(tinyIdentityAtoms(candidates, spec))
    else
      val components = connectedComponents(candidates, activeVoxels.indexSet.space.dims)
      val atoms = Vector.newBuilder[RadialAtom]

      var level = 0
      while level <= spec.maxLevel do
        val sigma = RadialSigmaMm.unsafe(spec.sigma0.value / math.pow(2.0, level.toDouble))
        val radius = spec.radiusFactor.value * sigma.value
        val minDistance2 = radius * radius

        var componentIndex = 0
        while componentIndex < components.length do
          val selected = selectComponentAtoms(
            components(componentIndex),
            spec.seed,
            level,
            componentIndex,
            minDistance2
          )
          selected.foreach(candidate => atoms += RadialAtom(candidate.coordinate, sigma, level))
          componentIndex += 1
        level += 1

      val result = atoms.result()
      if result.isEmpty then Left(RadialBasisError.EmptyAtoms) else Right(result)

  private def activeCandidates(activeVoxels: RadialActiveVoxels): Vector[Candidate] =
    Vector.tabulate(activeVoxels.size) { i =>
      val index = activeVoxels.indexSet(i)
      Candidate(
        index = index,
        coordinate = activeVoxels.coordinates(i),
        grid = activeVoxels.indexSet.space.toNeuroSpace.indexToGrid3D(index)
      )
    }

  private def tinyIdentityAtoms(
      candidates: Vector[Candidate],
      spec: RadialBasisSpec
  ): Vector[RadialAtom] =
    val sigma = RadialSigmaMm.unsafe(math.max(1e-3, spec.sigma0.value / spec.tinySigmaScale))
    candidates.map(candidate => RadialAtom(candidate.coordinate, sigma, level = 0))

  private def selectComponentAtoms(
      component: Vector[Candidate],
      seed: Long,
      level: Int,
      componentIndex: Int,
      minDistance2: Double
  ): Vector[Candidate] =
    val ordered = component.sortBy(candidate => deterministicKey(seed, level, componentIndex, candidate.index))
    val selected = Vector.newBuilder[Candidate]
    val accepted = scala.collection.mutable.ArrayBuffer.empty[Candidate]

    var i = 0
    while i < ordered.length do
      val candidate = ordered(i)
      var keep = true
      var j = 0
      while j < accepted.length && keep do
        if candidate.coordinate.squaredDistanceTo(accepted(j).coordinate) < minDistance2 then keep = false
        j += 1
      if keep then
        accepted += candidate
        selected += candidate
      i += 1

    selected.result()

  private def connectedComponents(
      candidates: Vector[Candidate],
      dims: Vector[Int]
  ): Vector[Vector[Candidate]] =
    val byIndex = candidates.map(candidate => candidate.index -> candidate).toMap
    val visited = scala.collection.mutable.HashSet.empty[Int]
    val components = Vector.newBuilder[Vector[Candidate]]
    val seeds = candidates.sortBy(_.index)

    var seedIndex = 0
    while seedIndex < seeds.length do
      val seed = seeds(seedIndex)
      if !visited.contains(seed.index) then
        val component = Vector.newBuilder[Candidate]
        val queue = scala.collection.mutable.Queue.empty[Candidate]
        queue.enqueue(seed)
        visited += seed.index

        while queue.nonEmpty do
          val current = queue.dequeue()
          component += current
          var offsetIndex = 0
          while offsetIndex < NeighborOffsets6.length do
            val (dx, dy, dz) = NeighborOffsets6(offsetIndex)
            val x = current.grid(0) + dx
            val y = current.grid(1) + dy
            val z = current.grid(2) + dz
            if x >= 0 && x < dims(0) && y >= 0 && y < dims(1) && z >= 0 && z < dims(2) then
              val neighborIndex = Indexing.gridToIndex3D(dims, x, y, z)
              byIndex.get(neighborIndex) match
                case Some(neighbor) if !visited.contains(neighborIndex) =>
                  visited += neighborIndex
                  queue.enqueue(neighbor)
                case _ =>
                  ()
            offsetIndex += 1
        components += component.result().sortBy(_.index)
      seedIndex += 1

    components.result().sortBy(component => component.headOption.map(_.index).getOrElse(Int.MaxValue))

  private def deterministicKey(
      seed: Long,
      level: Int,
      componentIndex: Int,
      index: Int
  ): Long =
    var z =
      seed ^
        (index.toLong * 1103515245L) ^
        (level.toLong * 1000003L) ^
        (componentIndex.toLong * 1099511628211L)
    z = (z ^ (z >>> 33)) * -49064778989728563L
    z = (z ^ (z >>> 33)) * -4265267296055464877L
    z ^ (z >>> 33)
