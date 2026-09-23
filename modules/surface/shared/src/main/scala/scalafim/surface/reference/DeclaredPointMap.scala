package scalafim.surface.reference

import image4s.geometry.{Affine, D3}

import java.nio.{ByteBuffer, ByteOrder}

/** The TemplateFlow file a point map was converted from. */
final case class PointMapSource(archivePath: String, sha256: String, bytes: Long, catalogRevision: Option[String])

/** A quarantine notice carried by a manifest; a quarantined map is never used. */
final case class PointMapQuarantine(archivePath: String, reason: String)

/** One stage as a `templateflow4s.point-map/1` manifest lists it. */
enum ManifestStage:
  case AffineEntry(matrixRowMajor: Vector[Double])
  case DisplacementEntry(file: String, sha256: String, bytes: Long, dims: Vector[Int], voxelToRasRowMajor: Vector[Double])

/** A parsed `templateflow4s.point-map/1` manifest, before any verification. */
final case class PointMapManifest(
  schema: String,
  source: PointMapSource,
  input: String,
  output: String,
  quarantine: Option[PointMapQuarantine],
  stages: Vector[ManifestStage]
)

object PointMapManifest:
  val Schema: String = "templateflow4s.point-map/1"

/** A composite point map bound to the exact TemplateFlow source it was
  * converted from (its SHA-256 must be the one the caller expects) and to the
  * exact bytes of every displacement stage file. It maps points of `input`
  * to points of `output`.
  */
final class DeclaredPointMap private (
  val source: DeclaredAsset,
  val input: TemplateId,
  val output: TemplateId,
  val catalogRevision: Option[TemplateRelease],
  val stageFiles: Vector[DataAsset],
  val map: PointMap
):
  def display: String = s"${input.value} -> ${output.value} point map from ${source.display}"

object DeclaredPointMap:
  private val HeaderBytes = 352
  private val stageFilePattern = "stage-[0-9]+-displacement\\.nii".r

  /** Verify a manifest and its stage bytes. `stageBytes` returns the exact
    * bytes of a named stage file, or None when it is missing.
    */
  def fromManifest(
    manifest: PointMapManifest,
    expectedSourceSha256: String,
    stageBytes: String => Option[Array[Byte]]
  ): Either[ReferenceError, DeclaredPointMap] =
    for
      _ <- Either.cond(manifest.schema == PointMapManifest.Schema, (),
        ReferenceError.InvalidPointMap(s"expected schema ${PointMapManifest.Schema}; got '${manifest.schema}'"))
      _ <- manifest.quarantine.fold(Right(()))(q => Left(ReferenceError.QuarantinedTransform(q.archivePath, q.reason)))
      _ <- Either.cond(manifest.source.sha256 == expectedSourceSha256, (),
        ReferenceError.DigestMismatch(manifest.source.archivePath, expectedSourceSha256, manifest.source.sha256))
      input <- TemplateId.make(manifest.input)
      output <- TemplateId.make(manifest.output)
      _ <- Either.cond(input != output, (), ReferenceError.InvalidPointMap("input and output frames are identical"))
      source <- sourceAsset(manifest.source)
      release <- manifest.source.catalogRevision.fold(Right(None))(r => TemplateRelease.make(r).map(Some(_)))
      decoded <- sequence(manifest.stages.map(decodeStage(_, stageBytes)))
      map <- PointMap.make(decoded.map(_._1)).left.map(e => ReferenceError.InvalidPointMap(e.message))
    yield DeclaredPointMap(source, input, output, release, decoded.flatMap(_._2), map)

  /** Test-only escape hatch: declares a point map without any source bytes. */
  private[reference] def unsafeAssumeVerified(input: TemplateId, output: TemplateId, release: Option[TemplateRelease],
      map: PointMap): DeclaredPointMap =
    val synthetic = DataAsset.make("synthetic-point-map", "0" * 64).fold(e => throw new IllegalStateException(e.message), identity)
    DeclaredPointMap(synthetic, input, output, release, Vector.empty, map)

  private def sourceAsset(source: PointMapSource): Either[ReferenceError, DeclaredAsset] =
    val template = source.archivePath.stripPrefix("tpl-").takeWhile(_ != '/')
    source.catalogRevision match
      case Some(revision) => TemplateId.make(template).flatMap(AssetProvenance.make(_, source.archivePath, revision, source.sha256))
      case None => DataAsset.make(source.archivePath, source.sha256)

  private def decodeStage(stage: ManifestStage, stageBytes: String => Option[Array[Byte]])
      : Either[ReferenceError, (PointMapStage, Option[DataAsset])] =
    stage match
      case ManifestStage.AffineEntry(matrix) =>
        affine(matrix, "affine stage").map(a => (PointMapStage.AffineStage(a), None))
      case ManifestStage.DisplacementEntry(file, sha256, bytes, dims, voxelToRas) =>
        for
          _ <- Either.cond(stageFilePattern.matches(file), (), ReferenceError.InvalidPointMap(s"unexpected stage file name '$file'"))
          asset <- DataAsset.make(file, sha256)
          raw <- stageBytes(file).toRight(ReferenceError.AssetReadFailure(s"missing stage file $file"))
          _ <- Either.cond(raw.length.toLong == bytes, (),
            ReferenceError.InvalidPointMap(s"$file has ${raw.length} bytes; manifest declares $bytes"))
          _ <- asset.checkDigest(raw)
          grid <- affine(voxelToRas, s"$file voxelToRas")
          values <- decodeNifti(file, raw, dims, voxelToRas)
          field <- DisplacementField.owned(dims, grid, values).left.map(e => ReferenceError.InvalidPointMap(s"$file: ${e.message}"))
        yield (PointMapStage.DisplacementStage(field), Some(asset))

  private def affine(rowMajor: Vector[Double], label: String): Either[ReferenceError, Affine[D3]] =
    if rowMajor.length != 16 then Left(ReferenceError.InvalidPointMap(s"$label must have 16 values"))
    else if rowMajor.drop(12) != Vector(0.0, 0.0, 0.0, 1.0) then
      Left(ReferenceError.InvalidPointMap(s"$label must have last row [0, 0, 0, 1]"))
    else Affine.fromRowMajor[D3](rowMajor).left.map(e => ReferenceError.InvalidPointMap(s"$label: ${e.message}"))

  /** Check the canonical NIfTI-1 header against the manifest and decode the
    * component-planar float64 payload.
    */
  private def decodeNifti(file: String, raw: Array[Byte], dims: Vector[Int],
      voxelToRas: Vector[Double]): Either[ReferenceError, Array[Double]] =
    def fail(reason: String) = Left(ReferenceError.InvalidPointMap(s"$file: $reason"))
    val count = if dims.length == 3 && dims.forall(_ > 0) then dims.map(_.toLong).product * 3L else -1L
    if count < 0L then fail(s"dims must be three positive extents; got $dims")
    else if count > Int.MaxValue || raw.length.toLong != HeaderBytes + count * 8L then
      fail(s"expected ${HeaderBytes + count * 8L} bytes for dims $dims; got ${raw.length}")
    else
      val buffer = ByteBuffer.wrap(raw).order(ByteOrder.LITTLE_ENDIAN)
      val dim = Vector.tabulate(8)(i => buffer.getShort(40 + 2 * i).toInt)
      val sform = Vector.tabulate(12)(i => buffer.getFloat(280 + 4 * i))
      if buffer.getInt(0) != 348 then fail("sizeof_hdr is not 348 (not little-endian NIfTI-1)")
      else if buffer.get(344) != 'n'.toByte || buffer.get(345) != '+'.toByte || buffer.get(346) != '1'.toByte then
        fail("magic is not n+1")
      else if dim != Vector(5, dims(0), dims(1), dims(2), 1, 3, 1, 1) then fail(s"dim $dim does not match manifest dims $dims")
      else if buffer.getShort(70) != 64 || buffer.getShort(72) != 64 then fail("datatype is not float64 (64)")
      else if buffer.getShort(68) != 1007 then fail("intent_code is not VECTOR (1007)")
      else if buffer.getFloat(108) != HeaderBytes.toFloat then fail("vox_offset is not 352")
      else if buffer.getShort(254) != 5 then fail("sform_code is not 5")
      else if !sform.indices.forall(i => math.abs(sform(i).toDouble - voxelToRas(i)) <= math.ulp(voxelToRas(i).toFloat).toDouble) then
        fail("float32 sform does not match the manifest voxelToRas within float32 rounding")
      else
        val out = new Array[Double](count.toInt)
        var i = 0
        while i < out.length do
          out(i) = buffer.getDouble(HeaderBytes + 8 * i)
          i += 1
        Right(out)

  private def sequence[A](values: Vector[Either[ReferenceError, A]]): Either[ReferenceError, Vector[A]] =
    values.foldLeft[Either[ReferenceError, Vector[A]]](Right(Vector.empty)): (acc, next) =>
      for all <- acc; one <- next yield all :+ one
