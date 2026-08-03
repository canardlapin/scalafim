package scalafim.interop.archive.lna

import scalafim.archive.{
  ArchiveError,
  ArchivePath,
  PayloadDescriptor,
  PayloadId,
  PayloadRoleId,
  RepresentationKey,
  RepresentationMetadata,
  RunLabel,
  ScalarTypeId
}
import scalafim.archive.lna.{
  LnaPayloadRole,
  LnaArchive,
  LnaDType,
  LnaExplicitLatent,
  LnaTemporalDct,
  Payload,
  TemporalDctNorm,
  TemporalDctParams
}
import scalafim.image.{DMat, NeuroSpace}
import scalafim.interop.archive.RepresentationEnvelope
import scalafim.latent.{
  DctNorm,
  MaterializedTemporalDct,
  SampleOffsetValues,
  SpatialLoadingValues,
  TemporalBasisValues,
  TemporalDctRepresentation
}
import scalafim.response.{
  DecodeConsistency,
  ReconstructionContract,
  SampleDomainKind,
  TimeDomain
}

object TemporalDctLnaProfile:
  val Representation: RepresentationKey =
    RepresentationKey.unsafe("org.scalafim/temporal-dct@1")

  val ModelHeaderKey: String =
    "rra.temporal-dct.model-v1"

  val DependencyHeaderKey: String =
    "rra.temporal-dct.dependencies"

  val EmbeddedDependencies: String =
    "embedded"

enum TemporalDctLnaWriteStep:
  case BeginStaging
  case WritePayload(path: ArchivePath)
  case WriteManifest
  case Publish

final class TemporalDctLnaWritePlan private (
    val archive: LnaArchive,
    val steps: Vector[TemporalDctLnaWriteStep]
)

object TemporalDctLnaWritePlan:
  def create(
      value: MaterializedTemporalDct,
      space: NeuroSpace,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-interop-archived-response"
  ): Either[ArchiveError, TemporalDctLnaWritePlan] =
    for
      archive <- TemporalDctLnaModel.archive(value, space, runLabel, creator)
      payloadSteps = archive.manifest.datasets
        .sortBy(_.path.value)
        .map(reference => TemporalDctLnaWriteStep.WritePayload(reference.path))
      steps =
        Vector(TemporalDctLnaWriteStep.BeginStaging) ++
          payloadSteps ++
          Vector(
            TemporalDctLnaWriteStep.WriteManifest,
            TemporalDctLnaWriteStep.Publish
          )
    yield new TemporalDctLnaWritePlan(archive, steps)

final case class TemporalDctLnaLayout(
    basis: ArchivePath,
    loadings: ArchivePath,
    offset: Option[ArchivePath]
):
  def paths: Vector[ArchivePath] =
    Vector(basis, loadings) ++ offset.toVector

object TemporalDctLnaLayout:
  def fromEnvelope(
      model: TemporalDctRepresentation,
      envelope: RepresentationEnvelope
  ): Either[ArchiveError, TemporalDctLnaLayout] =
    for
      _ <-
        if envelope.key == TemporalDctLnaProfile.Representation then Right(())
        else Left(ArchiveError.InvalidArchive(
          s"temporal DCT binding received '${envelope.key.value}'"
        ))
      basis <- uniqueRole(
        envelope.payloads.values,
        LnaPayloadRole.TemporalBasis
      )
      loadings <- uniqueRole(
        envelope.payloads.values,
        LnaPayloadRole.Loadings
      )
      offset <- optionalRole(
        envelope.payloads.values,
        LnaPayloadRole.SampleOffset
      )
      _ <- checkPayload(
        basis,
        Vector(model.schema.time.count.toLong, model.spec.components.toLong)
      )
      _ <- checkPayload(
        loadings,
        Vector(model.schema.samples.count.toLong, model.spec.components.toLong)
      )
      _ <- (model.offsetSlot, offset) match
        case (Some(_), Some(found)) =>
          checkPayload(found, Vector(model.schema.samples.count.toLong))
        case (Some(_), None) =>
          Left(ArchiveError.InvalidArchive(
            "centered temporal DCT archive is missing its sample offset"
          ))
        case (None, Some(_)) =>
          Left(ArchiveError.InvalidArchive(
            "uncentered temporal DCT archive contains an unexpected sample offset"
          ))
        case (None, None) =>
          Right(())
    yield TemporalDctLnaLayout(
      ArchivePath(basis.id.value),
      ArchivePath(loadings.id.value),
      offset.map(found => ArchivePath(found.id.value))
    )

  private def uniqueRole(
      payloads: Vector[PayloadDescriptor],
      role: PayloadRoleId
  ): Either[ArchiveError, PayloadDescriptor] =
    payloads.filter(_.role == role) match
      case Vector(found) =>
        Right(found)
      case Vector() =>
        Left(ArchiveError.InvalidArchive(
          s"temporal DCT archive is missing '${role.value}'"
        ))
      case _ =>
        Left(ArchiveError.InvalidArchive(
          s"temporal DCT archive contains duplicate '${role.value}' payloads"
        ))

  private def optionalRole(
      payloads: Vector[PayloadDescriptor],
      role: PayloadRoleId
  ): Either[ArchiveError, Option[PayloadDescriptor]] =
    payloads.filter(_.role == role) match
      case Vector() =>
        Right(None)
      case Vector(found) =>
        Right(Some(found))
      case _ =>
        Left(ArchiveError.InvalidArchive(
          s"temporal DCT archive contains duplicate '${role.value}' payloads"
        ))

  private def checkPayload(
      payload: PayloadDescriptor,
      expectedShape: Vector[Long]
  ): Either[ArchiveError, Unit] =
    if payload.scalarType != ScalarTypeId.unsafe("float64") then
      Left(ArchiveError.InvalidArchive(
        s"temporal DCT payload '${payload.id.value}' must be float64"
      ))
    else if payload.shape != expectedShape then
      Left(ArchiveError.ShapeMismatch(
        s"${payload.id.value} expected ${expectedShape.mkString("x")} but found " +
          payload.shape.mkString("x")
      ))
    else Right(())

private object TemporalDctLnaModel:
  def archive(
      value: MaterializedTemporalDct,
      space: NeuroSpace,
      runLabel: RunLabel,
      creator: String
  ): Either[ArchiveError, LnaArchive] =
    val model = value.model
    for
      basis <- matrix(value.basis)
      loadings <- matrix(value.loadings)
      response = LnaExplicitLatent.Response(
        basis,
        loadings,
        value.offset.map(_.valuesCopy.toVector),
        sourceDomain = model.basisSlot.id.value,
        targetDomain = model.schema.samples.id.value,
        metadata = Map("representation-instance" -> model.id.value)
      )
      params <- TemporalDctParams.checked(
        model.spec.components,
        temporalNorm(model.spec.norm),
        model.center,
        model.ridge.value
      )
      base <- LnaTemporalDct.archive(
        response,
        params,
        space,
        runLabel,
        creator
      )
      header = base.manifest.header ++ Map(
        TemporalDctLnaProfile.ModelHeaderKey -> fingerprint(model),
        TemporalDctLnaProfile.DependencyHeaderKey ->
          TemporalDctLnaProfile.EmbeddedDependencies,
        RepresentationMetadata.DescriptorHeader ->
          TemporalDctLnaDescriptor.encodeModel(model),
        RepresentationMetadata.OutputSchemaHeader ->
          TemporalDctLnaDescriptor.encodeSchema(model.schema)
      )
      enriched = base.copy(manifest = base.manifest.copy(header = header))
      valid <- enriched.validate
    yield valid

  def fingerprint(model: TemporalDctRepresentation): String =
    val fields = Vector.newBuilder[String]
    fields += model.id.value
    fields += model.schema.id.value
    fields += model.schema.time.id.value
    fields += model.schema.time.units.value
    fields += model.schema.time.count.toString
    model.schema.time match
      case regular: TimeDomain.Regular =>
        fields += "regular"
        fields += bits(regular.origin.value)
        fields += bits(regular.interval.value)
      case explicit: TimeDomain.Explicit =>
        fields += "explicit"
        explicit.rawCoordinateBits.foreach(value =>
          fields += java.lang.Long.toHexString(value)
        )
    fields += model.schema.samples.id.value
    fields += model.schema.samples.count.toString
    model.schema.samples.kind match
      case SampleDomainKind.Volume(space, mask, ordering) =>
        fields += "volume"
        addReference(fields, space)
        fields += mask.fold("none")(_ => "some")
        mask.foreach(addReference(fields, _))
        addReference(fields, ordering)
      case SampleDomainKind.Surface(surface, topology, ordering) =>
        fields += "surface"
        addReference(fields, surface)
        addReference(fields, topology)
        addReference(fields, ordering)
    fields += model.schema.signal.units.value
    fields += model.schema.signal.calibration.toString
    fields += model.schema.signal.nonFinite.toString
    fields += model.spec.timepoints.toString
    fields += model.spec.components.toString
    fields += model.spec.norm.toString
    fields += model.center.toString
    fields += bits(model.ridge.value)
    model.reconstructionContract match
      case ReconstructionContract.Exact =>
        fields += "reconstruction-exact"
      case ReconstructionContract.DeterministicBounded(bounds) =>
        fields += "reconstruction-bounded"
        fields += bits(bounds.absolute)
        fields += bits(bounds.relative)
      case ReconstructionContract.ValidatedScientific(profile, report) =>
        fields += "reconstruction-validated"
        fields += profile.value
        fields += report.value
    model.decodeConsistency match
      case DecodeConsistency.ExactBits =>
        fields += "decode-exact-bits"
      case value: DecodeConsistency.UlpBounded =>
        fields += "decode-ulp"
        fields += value.maxUlps.toString
      case value: DecodeConsistency.AbsoluteRelative =>
        fields += "decode-absolute-relative"
        fields += bits(value.absolute)
        fields += bits(value.relative)
    fields.result().map(value => s"${value.length}:$value").mkString

  private def matrix(
      values: TemporalBasisValues | SpatialLoadingValues
  ): Either[ArchiveError, DMat] =
    val (rows, columns, source) =
      values match
        case basis: TemporalBasisValues =>
          (basis.rows, basis.components, basis.rowMajorCopy)
        case loadings: SpatialLoadingValues =>
          (loadings.rows, loadings.components, loadings.rowMajorCopy)
    val owned = Array.ofDim[Double](source.length)
    var index = 0
    while index < source.length do
      owned(index) = source(index)
      index += 1
    Right(DMat.fromRowMajorOwned(rows, columns, owned))

  private def temporalNorm(value: DctNorm): TemporalDctNorm =
    value match
      case DctNorm.Ortho =>
        TemporalDctNorm.Ortho
      case DctNorm.None =>
        TemporalDctNorm.None

  private def bits(value: Double): String =
    java.lang.Long.toHexString(java.lang.Double.doubleToRawLongBits(value))

  private def addReference(
      fields: scala.collection.mutable.Builder[String, Vector[String]],
      reference: scalafim.response.DomainReference
  ): Unit =
    fields += reference.namespace.value
    fields += reference.value
