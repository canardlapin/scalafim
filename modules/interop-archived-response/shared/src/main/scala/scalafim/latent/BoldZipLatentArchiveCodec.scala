package scalafim.latent

import scalafim.archive.{ArchiveError, ArchivePath, RunLabel}
import scalafim.archive.lna.{
  DatasetRef,
  DatasetRole,
  LnaArchive,
  LnaDType,
  LnaManifest,
  LnaRun,
  LnaShape,
  Payload,
  TransformDescriptor,
  TransformKind,
  TransformParams
}
import scalafim.image.SomeSampleSpace
import scalafim.image.spatialDims
import gale.linalg.{DMat, DVec}
import scalafim.latent.LatentArchivePayloads.*

object BoldZipLatentArchiveCodec:
  private val KindKey = "lna.response.kind"
  private val KindValue = "boldzip_sr"
  private val Kind = TransformKind.Custom(KindValue)
  private val CarrierThetaRole = DatasetRole.Other("boldzip_carrier_theta")
  private val CarrierLoadingsRole = DatasetRole.Other("boldzip_carrier_loadings")
  private val CoarseBasisRole = DatasetRole.Other("boldzip_phi_coarse")
  private val DetailBasisRole = DatasetRole.Other("boldzip_phi_detail")
  private val TextureIndexRole = DatasetRole.Other("boldzip_texture_index")
  private val TextureAmplitudeRole = DatasetRole.Other("boldzip_texture_amplitude")
  private val EventIndexRole = DatasetRole.Other("boldzip_residual_event_index")
  private val EventAmplitudeRole = DatasetRole.Other("boldzip_residual_event_amplitude")
  private val SpatialBasisLabelKey = "spatial_basis.label"

  def toArchive(
      response: BoldZipPayload,
      space: SomeSampleSpace,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-latent"
  ): Either[ArchiveError, LnaArchive] =
    if response.shape.samples != space.spatialDims.product then
      Left(ArchiveError.ShapeMismatch(s"BOLDZip response has ${response.shape.samples} samples but space has ${space.spatialDims.product} voxels"))
    else
      val base = ArchivePath(s"/scans/${runLabel.value}/step_00_boldzip_sr")
      val temporalPath = base / "temporal_basis"
      val thetaPath = base / "carrier_theta"
      val loadingsPath = base / "carrier_loadings"
      val coarsePath = base / "phi_coarse"
      val detailPath = base / "phi_detail"
      val textureIndexPath = base / "texture_index"
      val textureAmplitudePath = base / "texture_amplitude"
      val eventIndexPath = base / "residual_event_index"
      val eventAmplitudePath = base / "residual_event_amplitude"
      val offsetPath = base / "offset"

      val temporalRef = DatasetRef(
        temporalPath,
        DatasetRole.TemporalBasis,
        Vector(response.temporalBasis.rows, response.temporalBasis.cols),
        Some(LnaDType.Float64)
      )
      val thetaRef = DatasetRef(
        thetaPath,
        CarrierThetaRole,
        Vector(response.carrierTheta.rows, response.carrierTheta.cols),
        Some(LnaDType.Float64)
      )
      val loadingsRef =
        Option.when(response.spatialBasis.coarseAtoms > 0)(
          DatasetRef(
            loadingsPath,
            CarrierLoadingsRole,
            Vector(response.carrierLoadings.rows, response.carrierLoadings.cols),
            Some(LnaDType.Float64)
          )
        )
      val coarseRef =
        response.spatialBasis.phiCoarse.map { matrix =>
          DatasetRef(coarsePath, CoarseBasisRole, Vector(matrix.rows, matrix.cols), Some(LnaDType.Float64))
        }
      val detailRef =
        response.spatialBasis.phiDetail.map { matrix =>
          DatasetRef(detailPath, DetailBasisRole, Vector(matrix.rows, matrix.cols), Some(LnaDType.Float64))
        }
      val textureRefs =
        Option.when(response.texture.nonEmpty)(
          Vector(
            DatasetRef(textureIndexPath, TextureIndexRole, Vector(response.texture.length, 3), Some(LnaDType.Int32)),
            DatasetRef(textureAmplitudePath, TextureAmplitudeRole, Vector(response.texture.length), Some(LnaDType.Float64))
          )
        ).getOrElse(Vector.empty)
      val eventRefs =
        Option.when(response.events.nonEmpty)(
          Vector(
            DatasetRef(eventIndexPath, EventIndexRole, Vector(response.events.length, 3), Some(LnaDType.Int32)),
            DatasetRef(eventAmplitudePath, EventAmplitudeRole, Vector(response.events.length), Some(LnaDType.Float64))
          )
        ).getOrElse(Vector.empty)
      val offsetRef =
        response.offset.map(values => DatasetRef(offsetPath, DatasetRole.SampleOffset, Vector(values.length), Some(LnaDType.Float64)))

      val refs =
        Vector(temporalRef, thetaRef) ++
          loadingsRef.toVector ++
          coarseRef.toVector ++
          detailRef.toVector ++
          textureRefs ++
          eventRefs ++
          offsetRef.toVector

      val descriptor = TransformDescriptor(
        name = "00_boldzip_sr.json",
        kind = Kind,
        params = TransformParams.Custom(
          name = KindValue,
          sourceDomain = Some(response.sourceDomain.value),
          targetDomain = Some(response.targetDomain.value),
          label = Option.when(response.label.nonEmpty)(response.label),
          metadata = response.metadata ++ Map(
            KindKey -> KindValue,
            SpatialBasisLabelKey -> response.spatialBasis.label
          )
        ),
        inputs = Vector("temporal_basis", "carrier_theta", "spatial_basis"),
        outputs = Vector("latent_response"),
        datasets = refs
      )

      val payloads =
        Map[ArchivePath, Payload](
          temporalPath -> Payload.DoubleMatrix(toDMat(response.temporalBasis)),
          thetaPath -> Payload.DoubleMatrix(toDMat(response.carrierTheta))
        ) ++
          loadingsRef.map(_ => loadingsPath -> Payload.DoubleMatrix(toDMat(response.carrierLoadings))).toMap ++
          response.spatialBasis.phiCoarse.map(matrix => coarsePath -> Payload.DoubleMatrix(toDMat(matrix))).toMap ++
          response.spatialBasis.phiDetail.map(matrix => detailPath -> Payload.DoubleMatrix(toDMat(matrix))).toMap ++
          texturePayloads(response.texture, textureIndexPath, textureAmplitudePath) ++
          eventPayloads(response.events, eventIndexPath, eventAmplitudePath) ++
          response.offset.map(values => offsetPath -> Payload.DoubleVector(values.toVector)).toMap

      Right(
        LnaArchive(
          manifest = LnaManifest(
            creator = creator,
            requiredTransforms = Vector(Kind),
            transforms = Vector(descriptor),
            runs = Vector(LnaRun(runLabel, LnaShape(space, response.shape.timepoints), thetaPath)),
            datasets = refs,
            header = Map(
              "response.kind" -> KindValue,
              "space.dims" -> space.spatialDims.mkString("x"),
              "timepoints" -> response.shape.timepoints.toString,
              "samples" -> response.shape.samples.toString,
              "carriers" -> response.shape.coefficients.toString,
              "temporal.components" -> response.temporalBasis.cols.toString,
              "coarse_basis" -> response.spatialBasis.coarse.metadataValue,
              "detail_basis" -> response.spatialBasis.detail.metadataValue,
              "source_domain" -> response.sourceDomain.value,
              "target_domain" -> response.targetDomain.value
            )
          ),
          payloads = payloads
        )
      )

  def fromArchive(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, BoldZipPayload] =
    archive.validate.flatMap { valid =>
      for
        run <- valid.run(runLabel).toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' not found"))
        desc <- descriptor(valid, runLabel)
        params <- desc.params match
          case p: TransformParams.Custom if p.name == KindValue => Right(p)
          case _                                                => Left(ArchiveError.InvalidArchive("BOLDZip descriptor missing typed custom params"))
        sourceDomain <- params.sourceDomain.toRight(ArchiveError.InvalidArchive("BOLDZip descriptor missing source domain"))
        targetDomain <- params.targetDomain.toRight(ArchiveError.InvalidArchive("BOLDZip descriptor missing target domain"))
        temporalPath <- byRole(desc, DatasetRole.TemporalBasis)
        temporalBasis <- doubleMatrixPayload(valid, temporalPath, "BOLDZip temporal basis")
        thetaPath <- byRole(desc, CarrierThetaRole)
        carrierTheta <- doubleMatrixPayload(valid, thetaPath, "BOLDZip carrier theta")
        coarseMatrix <- optionalDoubleMatrix(valid, desc, CoarseBasisRole, "BOLDZip coarse spatial basis")
        detailMatrix <- optionalDoubleMatrix(valid, desc, DetailBasisRole, "BOLDZip detail spatial basis")
        spatialBasis <- BoldZipSpatialBasis(
          sampleCount = run.shape.spatialSize,
          coarse = coarseMatrix.map(matrix => BoldZipCoarseBasis.MatrixBasis(toDoubleMatrix(matrix))).getOrElse(BoldZipCoarseBasis.Absent),
          detail = detailMatrix.map(matrix => BoldZipDetailBasis.MatrixBasis(toDoubleMatrix(matrix))).getOrElse(BoldZipDetailBasis.IdentitySamples),
          label = params.metadata.getOrElse(SpatialBasisLabelKey, "")
        ).left.map(error)
        carrierLoadings <- readCarrierLoadings(valid, desc, spatialBasis, carrierTheta.rows)
        texture <- textureEntries(valid, desc)
        events <- eventEntries(valid, desc)
        offset <- optionalOffset(valid, desc, DatasetRole.SampleOffset, "BOLDZip offset")
        source <- DomainId(sourceDomain).left.map(error)
        target <- DomainId(targetDomain).left.map(error)
        response <- BoldZipPayload(
          temporalBasis = toDoubleMatrix(temporalBasis),
          carrierTheta = toDoubleMatrix(carrierTheta),
          carrierLoadings = carrierLoadings,
          spatialBasis = spatialBasis,
          texture = texture,
          events = events,
          offset = offset.map(DVec.fromSeq),
          sourceDomain = source,
          targetDomain = target,
          label = params.label.getOrElse(""),
          metadata = params.metadata -- Set(KindKey, SpatialBasisLabelKey)
        ).left.map(error)
        _ <-
          if response.shape.timepoints == run.shape.timepoints then Right(())
          else Left(ArchiveError.ShapeMismatch(s"BOLDZip response has ${response.shape.timepoints} timepoints but run has ${run.shape.timepoints} timepoints"))
        _ <-
          if response.shape.samples == run.shape.spatialSize then Right(())
          else Left(ArchiveError.ShapeMismatch(s"BOLDZip response has ${response.shape.samples} samples but run has ${run.shape.spatialSize} voxels"))
      yield response
    }

  def descriptor(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Either[ArchiveError, TransformDescriptor] =
    descriptorOption(archive, runLabel)
      .toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' has no BOLDZip latent descriptor"))

  def descriptorOption(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Option[TransformDescriptor] =
    val prefix = s"/scans/${runLabel.value}/"
    archive.manifest.transforms
      .find(desc => isDescriptor(desc) && desc.datasets.exists(_.path.value.startsWith(prefix)))

  def isArchive(archive: LnaArchive): Boolean =
    archive.manifest.transforms.exists(isDescriptor)

  def isDescriptor(desc: TransformDescriptor): Boolean =
    desc.kind == Kind &&
      (desc.params match
        case p: TransformParams.Custom => p.name == KindValue
        case _                         => false)

  private def readCarrierLoadings(
      archive: LnaArchive,
      desc: TransformDescriptor,
      spatialBasis: BoldZipSpatialBasis,
      carriers: Int
  ): Either[ArchiveError, DMat] =
    optionalDoubleMatrix(archive, desc, CarrierLoadingsRole, "BOLDZip carrier loadings").flatMap {
      case Some(matrix) =>
        val values = toDoubleMatrix(matrix)
        if values.rows != spatialBasis.coarseAtoms then
          Left(ArchiveError.ShapeMismatch(s"BOLDZip carrier loadings have ${values.rows} rows but coarse basis has ${spatialBasis.coarseAtoms} atoms"))
        else if values.cols != carriers then
          Left(ArchiveError.ShapeMismatch(s"BOLDZip carrier loadings have ${values.cols} columns but carrier theta has $carriers carriers"))
        else Right(values)
      case None if spatialBasis.coarseAtoms == 0 =>
        Right(DMat.zeros(0, carriers))
      case None =>
        Left(ArchiveError.InvalidArchive("BOLDZip archive has a coarse basis but no carrier loadings"))
    }

  private def textureEntries(
      archive: LnaArchive,
      desc: TransformDescriptor
  ): Either[ArchiveError, Vector[BoldZipTextureEntry]] =
    optionalIndexAmplitudeTable(
      archive = archive,
      desc = desc,
      indexRole = TextureIndexRole,
      amplitudeRole = TextureAmplitudeRole,
      label = "BOLDZip texture"
    ).flatMap {
      case None => Right(Vector.empty)
      case Some((index, amplitudes)) =>
        traverse(Vector.tabulate(index.rows)(identity)) { row =>
          BoldZipTextureEntry
            .checked(
              atom = index(row, 0),
              carrier = index(row, 1),
              amplitude = amplitudes(row),
              lag = index(row, 2)
            )
            .left
            .map(error)
        }
    }

  private def eventEntries(
      archive: LnaArchive,
      desc: TransformDescriptor
  ): Either[ArchiveError, Vector[BoldZipResidualEvent]] =
    optionalIndexAmplitudeTable(
      archive = archive,
      desc = desc,
      indexRole = EventIndexRole,
      amplitudeRole = EventAmplitudeRole,
      label = "BOLDZip residual event"
    ).flatMap {
      case None => Right(Vector.empty)
      case Some((index, amplitudes)) =>
        traverse(Vector.tabulate(index.rows)(identity)) { row =>
          BoldZipResidualEvent
            .checked(
              atom = index(row, 0),
              frame = index(row, 1),
              amplitude = amplitudes(row),
              duration = index(row, 2)
            )
            .left
            .map(error)
        }
    }

  private def optionalIndexAmplitudeTable(
      archive: LnaArchive,
      desc: TransformDescriptor,
      indexRole: DatasetRole,
      amplitudeRole: DatasetRole,
      label: String
  ): Either[ArchiveError, Option[(Payload.IntMatrix, Vector[Double])]] =
    (desc.datasets.find(_.role == indexRole), desc.datasets.find(_.role == amplitudeRole)) match
      case (None, None) =>
        Right(None)
      case (Some(_), None) =>
        Left(ArchiveError.InvalidArchive(s"$label index table is present without amplitudes"))
      case (None, Some(_)) =>
        Left(ArchiveError.InvalidArchive(s"$label amplitudes are present without an index table"))
      case (Some(indexRef), Some(amplitudeRef)) =>
        for
          index <- intMatrixPayload(archive, indexRef.path, s"$label index")
          amplitudes <- doubleVectorPayload(archive, amplitudeRef.path, s"$label amplitude")
          _ <-
            if index.cols == 3 then Right(())
            else Left(ArchiveError.ShapeMismatch(s"$label index table must have 3 columns"))
          _ <-
            if index.rows == amplitudes.length then Right(())
            else Left(ArchiveError.ShapeMismatch(s"$label index rows ${index.rows} do not match amplitude length ${amplitudes.length}"))
        yield Some((index, amplitudes))

  private def texturePayloads(
      entries: Vector[BoldZipTextureEntry],
      indexPath: ArchivePath,
      amplitudePath: ArchivePath
  ): Map[ArchivePath, Payload] =
    if entries.isEmpty then Map.empty
    else
      Map(
        indexPath -> Payload.IntMatrix(
          rows = entries.length,
          cols = 3,
          values = entries.flatMap(entry => Vector(entry.atom.value, entry.carrier.value, entry.lag.value)),
          dtype = LnaDType.Int32
        ),
        amplitudePath -> Payload.DoubleVector(entries.map(_.amplitude.value))
      )

  private def eventPayloads(
      entries: Vector[BoldZipResidualEvent],
      indexPath: ArchivePath,
      amplitudePath: ArchivePath
  ): Map[ArchivePath, Payload] =
    if entries.isEmpty then Map.empty
    else
      Map(
        indexPath -> Payload.IntMatrix(
          rows = entries.length,
          cols = 3,
          values = entries.flatMap(entry => Vector(entry.atom.value, entry.frame.value, entry.duration.value)),
          dtype = LnaDType.Int32
        ),
        amplitudePath -> Payload.DoubleVector(entries.map(_.amplitude.value))
      )
