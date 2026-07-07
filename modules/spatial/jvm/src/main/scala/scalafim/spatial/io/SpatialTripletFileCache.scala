package scalafim.spatial.io

import scalafim.linalg.{CsrMatrix, SparseTriplets}
import scalafim.spatial.*

import java.io.{DataInputStream, DataOutputStream}
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

final case class CachedSpatialTriplets(
  key: OperatorCacheKey,
  provenance: OperatorProvenance,
  triplets: SparseTriplets
)

object SpatialTripletFileCache:
  private val Magic = "scalafim-spatial-triplets"
  private val Version = 1

  def write(path: Path, operator: SpatialOperator): Either[SpatialIoError, Unit] =
    operator.map match
      case csr: CsrMatrix =>
        writeTriplets(path, OperatorCacheKey.from(operator), operator.provenance, csr.toTriplets)
      case other =>
        Left(SpatialIoError.UnsupportedLinearMap(path, other.getClass.getName))

  def writeTriplets(
    path: Path,
    key: OperatorCacheKey,
    provenance: OperatorProvenance,
    triplets: SparseTriplets
  ): Either[SpatialIoError, Unit] =
    validateCached(path, key, provenance, triplets).flatMap { _ =>
      try
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        val out = new DataOutputStream(Files.newOutputStream(path))
        try
          out.writeUTF(Magic)
          out.writeInt(Version)
          writeKey(out, key)
          writeProvenance(out, provenance)
          writeTriplets(out, triplets)
          Right(())
        finally out.close()
      catch
        case NonFatal(error) =>
          Left(SpatialIoError.IoFailure(path, safeMessage(error)))
    }

  def read(path: Path): Either[SpatialIoError, CachedSpatialTriplets] =
    try
      val in = new DataInputStream(Files.newInputStream(path))
      try readFrom(in, path)
      finally in.close()
    catch
      case NonFatal(error) =>
        Left(SpatialIoError.IoFailure(path, safeMessage(error)))

  private def readFrom(in: DataInputStream, path: Path): Either[SpatialIoError, CachedSpatialTriplets] =
    val magic = in.readUTF()
    if magic != Magic then Left(SpatialIoError.InvalidCacheHeader(path, s"expected $Magic, got $magic"))
    else
      val version = in.readInt()
      if version != Version then Left(SpatialIoError.InvalidCacheHeader(path, s"unsupported version $version"))
      else
        for
          key <- readKey(in, path)
          provenance <- readProvenance(in, path)
          triplets <- readTriplets(in, path)
          _ <- validateCached(path, key, provenance, triplets)
        yield CachedSpatialTriplets(key, provenance, triplets)

  private def writeKey(out: DataOutputStream, key: OperatorCacheKey): Unit =
    out.writeUTF(key.source.value)
    out.writeUTF(key.target.value)
    writeMorphismIds(out, key.path)
    out.writeUTF(key.routing.toString)
    out.writeUTF(key.sampling.toString)
    writeOptionalIntVector(out, key.roi)
    out.writeBoolean(key.allowInverses)
    out.writeUTF(key.compiler)
    out.writeInt(key.rows)
    out.writeInt(key.cols)

  private def readKey(in: DataInputStream, path: Path): Either[SpatialIoError, OperatorCacheKey] =
    for
      source <- readDomainId(in, path, "operator key source")
      target <- readDomainId(in, path, "operator key target")
      pathIds <- readMorphismIds(in, path, "operator key path")
      routing <- readRouting(in, path)
      sampling <- readSampling(in, path)
      roi <- readOptionalIntVector(in, path, "operator key ROI")
      allowInverses = in.readBoolean()
      compiler <- readNonEmptyUtf(in, path, "operator key compiler")
      rows <- readNonNegativeInt(in, path, "operator key rows")
      cols <- readNonNegativeInt(in, path, "operator key cols")
      shape <- OperatorShape.build(rows, cols).left.map(error => SpatialIoError.InvalidCachePayload(path, error.message))
      rowSelection <- RowSelection.fromRoi(roi).left.map(error => SpatialIoError.InvalidCachePayload(path, error.message))
      recipe <- OperatorRecipe
        .build(pathIds, routing, sampling, rowSelection, allowInverses, compiler)
        .left.map(error => SpatialIoError.InvalidCachePayload(path, error.message))
      signature <- OperatorSignature
        .build(source, target, shape, recipe)
        .left.map(error => SpatialIoError.InvalidCachePayload(path, error.message))
    yield new OperatorCacheKey(signature)

  private def writeProvenance(out: DataOutputStream, provenance: OperatorProvenance): Unit =
    writeMorphismIds(out, provenance.path)
    out.writeUTF(provenance.routing.toString)
    out.writeUTF(provenance.sampling.toString)
    writeOptionalIntVector(out, provenance.roi)
    out.writeBoolean(provenance.allowInverses)
    out.writeUTF(provenance.compiler)

  private def readProvenance(in: DataInputStream, path: Path): Either[SpatialIoError, OperatorProvenance] =
    for
      pathIds <- readMorphismIds(in, path, "operator provenance path")
      routing <- readRouting(in, path)
      sampling <- readSampling(in, path)
      roi <- readOptionalIntVector(in, path, "operator provenance ROI")
      allowInverses = in.readBoolean()
      compiler <- readNonEmptyUtf(in, path, "operator provenance compiler")
      rowSelection <- RowSelection.fromRoi(roi).left.map(error => SpatialIoError.InvalidCachePayload(path, error.message))
      recipe <- OperatorRecipe
        .build(pathIds, routing, sampling, rowSelection, allowInverses, compiler)
        .left.map(error => SpatialIoError.InvalidCachePayload(path, error.message))
    yield OperatorProvenance.fromRecipe(recipe)

  private def writeTriplets(out: DataOutputStream, triplets: SparseTriplets): Unit =
    val rows = triplets.rowIndices
    val cols = triplets.colIndices
    val values = triplets.values
    out.writeInt(triplets.rows)
    out.writeInt(triplets.cols)
    out.writeInt(triplets.nnz)
    var i = 0
    while i < triplets.nnz do
      out.writeInt(rows(i))
      out.writeInt(cols(i))
      out.writeDouble(values(i))
      i += 1

  private def readTriplets(in: DataInputStream, path: Path): Either[SpatialIoError, SparseTriplets] =
    for
      rows <- readNonNegativeInt(in, path, "triplet rows")
      cols <- readNonNegativeInt(in, path, "triplet cols")
      nnz <- readNonNegativeInt(in, path, "triplet nnz")
      triplets <- readTripletArrays(in, path, rows, cols, nnz)
    yield triplets

  private def readTripletArrays(
    in: DataInputStream,
    path: Path,
    rows: Int,
    cols: Int,
    nnz: Int
  ): Either[SpatialIoError, SparseTriplets] =
    val rowIndices = Array.ofDim[Int](nnz)
    val colIndices = Array.ofDim[Int](nnz)
    val values = Array.ofDim[Double](nnz)
    var i = 0
    while i < nnz do
      rowIndices(i) = in.readInt()
      colIndices(i) = in.readInt()
      values(i) = in.readDouble()
      i += 1
    SparseTriplets(rows, cols, rowIndices, colIndices, values)
      .left.map(error => SpatialIoError.InvalidCachePayload(path, error.message))

  private def writeMorphismIds(out: DataOutputStream, ids: Vector[MorphismId]): Unit =
    out.writeInt(ids.length)
    ids.foreach(id => out.writeUTF(id.value))

  private def readMorphismIds(
    in: DataInputStream,
    path: Path,
    label: String
  ): Either[SpatialIoError, Vector[MorphismId]] =
    readNonNegativeInt(in, path, s"$label count").flatMap { count =>
      val builder = Vector.newBuilder[MorphismId]
      var i = 0
      var error = Option.empty[SpatialIoError]
      while i < count && error.isEmpty do
        MorphismId(in.readUTF()) match
          case Right(id) => builder += id
          case Left(err) => error = Some(SpatialIoError.InvalidCachePayload(path, err.message))
        i += 1
      error match
        case Some(err) => Left(err)
        case None => Right(builder.result())
    }

  private def writeOptionalIntVector(out: DataOutputStream, values: Option[Vector[Int]]): Unit =
    out.writeBoolean(values.isDefined)
    values.foreach { rows =>
      out.writeInt(rows.length)
      rows.foreach(out.writeInt)
    }

  private def readOptionalIntVector(
    in: DataInputStream,
    path: Path,
    label: String
  ): Either[SpatialIoError, Option[Vector[Int]]] =
    if in.readBoolean() then readIntVector(in, path, label).map(Some(_))
    else Right(None)

  private def readIntVector(in: DataInputStream, path: Path, label: String): Either[SpatialIoError, Vector[Int]] =
    readNonNegativeInt(in, path, s"$label count").map { count =>
      val builder = Vector.newBuilder[Int]
      var i = 0
      while i < count do
        builder += in.readInt()
        i += 1
      builder.result()
    }

  private def readDomainId(in: DataInputStream, path: Path, label: String): Either[SpatialIoError, DomainId] =
    DomainId(in.readUTF()).left.map(error => SpatialIoError.InvalidCachePayload(path, s"$label: ${error.message}"))

  private def readRouting(in: DataInputStream, path: Path): Either[SpatialIoError, RoutingPolicy] =
    in.readUTF() match
      case "Shortest" => Right(RoutingPolicy.Shortest)
      case "Anatomical" => Right(RoutingPolicy.Anatomical)
      case "Functional" => Right(RoutingPolicy.Functional)
      case other => Left(SpatialIoError.InvalidCachePayload(path, s"unknown routing policy $other"))

  private def readSampling(in: DataInputStream, path: Path): Either[SpatialIoError, SamplingPolicy] =
    in.readUTF() match
      case "Nearest" => Right(SamplingPolicy.Nearest)
      case "Trilinear" => Right(SamplingPolicy.Trilinear)
      case other => Left(SpatialIoError.InvalidCachePayload(path, s"unknown sampling policy $other"))

  private def readNonEmptyUtf(in: DataInputStream, path: Path, label: String): Either[SpatialIoError, String] =
    val value = in.readUTF().trim
    if value.isEmpty then Left(SpatialIoError.InvalidCachePayload(path, s"$label must be non-empty"))
    else Right(value)

  private def readNonNegativeInt(
    in: DataInputStream,
    path: Path,
    label: String
  ): Either[SpatialIoError, Int] =
    val value = in.readInt()
    if value < 0 then Left(SpatialIoError.InvalidCachePayload(path, s"$label must be non-negative, got $value"))
    else Right(value)

  private def validateCached(
    path: Path,
    key: OperatorCacheKey,
    provenance: OperatorProvenance,
    triplets: SparseTriplets
  ): Either[SpatialIoError, Unit] =
    if key.path != provenance.path then
      Left(SpatialIoError.InvalidCachePayload(path, "cache key path differs from provenance path"))
    else if key.routing != provenance.routing then
      Left(SpatialIoError.InvalidCachePayload(path, "cache key routing differs from provenance routing"))
    else if key.sampling != provenance.sampling then
      Left(SpatialIoError.InvalidCachePayload(path, "cache key sampling differs from provenance sampling"))
    else if key.roi != provenance.roi then
      Left(SpatialIoError.InvalidCachePayload(path, "cache key ROI differs from provenance ROI"))
    else if key.allowInverses != provenance.allowInverses then
      Left(SpatialIoError.InvalidCachePayload(path, "cache key inverse policy differs from provenance inverse policy"))
    else if key.compiler != provenance.compiler then
      Left(SpatialIoError.InvalidCachePayload(path, "cache key compiler differs from provenance compiler"))
    else if key.rows != triplets.rows || key.cols != triplets.cols then
      Left(
        SpatialIoError.InvalidCachePayload(
          path,
          s"cache key shape ${key.rows}x${key.cols} differs from triplet shape ${triplets.rows}x${triplets.cols}"
        )
      )
    else Right(())

  private def safeMessage(error: Throwable): String =
    Option(error.getMessage).getOrElse(error.getClass.getName)
