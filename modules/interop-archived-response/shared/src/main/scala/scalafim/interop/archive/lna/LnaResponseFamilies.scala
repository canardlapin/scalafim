package scalafim.interop.archive.lna

import cats.data.EitherT
import cats.effect.{Async, Resource}
import narr.NArray
import scalafim.archive.{
  ArchiveError,
  ArchiveLocation,
  ArchiveResource,
  ArchiveRevisionId,
  RepresentationKey
}
import scalafim.archive.lna.{LnaArchive, LnaPipeline}
import scalafim.interop.archive.{
  ArchiveResponseAccess,
  ArchivedResponseFamily,
  RepresentationEnvelope
}
import scalafim.response.{
  CalibrationState,
  DecodeConsistency,
  DomainId,
  DomainReference,
  InMemoryResponseSource,
  NonFinitePolicy,
  ReconstructionContract,
  ResponseSchema,
  ResponseSchemaId,
  ResponseSource,
  SampleAxis,
  SampleDomain,
  SampleDomainKind,
  SignalSchema,
  SourceId,
  TimeAxis,
  TimeDomain,
  UnitId
}

final class TemporalDctLnaRepresentationFamily[F[_]] private (
)(using F: Async[F]) extends ArchivedResponseFamily[F]:
  val key: RepresentationKey =
    TemporalDctLnaProfile.Representation

  def open(
      envelope: RepresentationEnvelope,
      archive: ArchiveResponseAccess[F]
  ): ArchiveResource[F, ResponseSource[F]] =
    Resource.eval(EitherT.fromEither[F]:
      for
        _ <-
          if envelope.key == key then Right(())
          else Left(ArchiveError.InvalidArchive(
            s"temporal DCT family received '${envelope.key.value}'"
          ))
        model <- TemporalDctLnaDescriptor.decode(envelope)
        layout <- TemporalDctLnaLayout.fromEnvelope(model, envelope)
        source <- ArchivedTemporalDctSource.make(
          SourceId.unsafe(s"archive:${archive.revisionId.value}"),
          model,
          archive,
          layout
        )
      yield source
    )

object TemporalDctLnaRepresentationFamily:
  def apply[F[_]: Async]: TemporalDctLnaRepresentationFamily[F] =
    new TemporalDctLnaRepresentationFamily[F]()

trait LegacyLnaResponseOpener[F[_]]:
  def open(
      location: ArchiveLocation,
      revisionId: ArchiveRevisionId
  ): EitherT[F, ArchiveError, ResponseSource[F]]

final class LegacyLnaRepresentationFamily[F[_]] private (
    opener: LegacyLnaResponseOpener[F]
)(using F: Async[F]) extends ArchivedResponseFamily[F]:
  val key: RepresentationKey =
    LegacyLnaRepresentationFamily.Key

  def open(
      envelope: RepresentationEnvelope,
      archive: ArchiveResponseAccess[F]
  ): ArchiveResource[F, ResponseSource[F]] =
    if envelope.key != key then
      Resource.eval(EitherT.leftT(ArchiveError.InvalidArchive(
        s"legacy LNA family received '${envelope.key.value}'"
      )))
    else
      Resource.eval(opener.open(archive.location, archive.revisionId))

object LegacyLnaRepresentationFamily:
  val Key: RepresentationKey =
    RepresentationKey.unsafe("org.scalafim/lna-pipeline@2")

  def using[F[_]: Async](
      opener: LegacyLnaResponseOpener[F]
  ): LegacyLnaRepresentationFamily[F] =
    new LegacyLnaRepresentationFamily[F](opener)

  private[lna] def source[F[_]: Async](
      archive: LnaArchive,
      revisionId: ArchiveRevisionId
  ): Either[ArchiveError, ResponseSource[F]] =
    for
      run <- archive.manifest.runs match
        case Vector(value) =>
          Right(value)
        case values =>
          Left(ArchiveError.InvalidArchive(
            s"legacy response opening requires exactly one LNA run, found ${values.length}"
          ))
      dense <- LnaPipeline.reconstruct(archive, run.label)
      schema <- legacySchema(revisionId, run.shape.timepoints, run.shape.spatialSize)
      values = NArray.ofSize[Double](dense.rows * dense.cols)
      _ =
        var row = 0
        while row < dense.rows do
          var column = 0
          while column < dense.cols do
            values(row * dense.cols + column) = dense(row, column)
            column += 1
          row += 1
      source <- InMemoryResponseSource
        .copyFromRowMajor[F](
          SourceId.unsafe(s"legacy-lna:${revisionId.value}"),
          schema,
          values,
          DecodeConsistency.ExactBits
        )
        .left
        .map(error => ArchiveError.InvalidArchive(error.message))
    yield source

  private def legacySchema(
      revisionId: ArchiveRevisionId,
      timepoints: Int,
      samples: Int
  ): Either[ArchiveError, ResponseSchema] =
    val identity = revisionId.value
    for
      time <- TimeDomain
        .regular(
          DomainId.unsafe[TimeAxis](s"$identity:legacy-time"),
          0.0,
          1.0,
          timepoints,
          UnitId.unsafe("legacy-sample")
        )
        .left
        .map(error => ArchiveError.InvalidArchive(error.message))
      sampleDomain <- SampleDomain
        .make(
          DomainId.unsafe[SampleAxis](s"$identity:legacy-samples"),
          samples,
          SampleDomainKind.Volume(
            DomainReference.unsafe("lna-space", identity),
            None,
            DomainReference.unsafe("lna-ordering", "linear-voxel")
          )
        )
        .left
        .map(error => ArchiveError.InvalidArchive(error.message))
      schema <- ResponseSchema
        .make(
          ResponseSchemaId.unsafe(s"$identity:legacy-response"),
          time,
          sampleDomain,
          SignalSchema(
            UnitId.unsafe("legacy-archive-value"),
            CalibrationState.Applied,
            NonFinitePolicy.Preserve
          )
        )
        .left
        .map(error => ArchiveError.InvalidArchive(error.message))
    yield schema
