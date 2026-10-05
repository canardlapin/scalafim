package scalafim.atlas.io

import java.net.URI
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal
import scalafim.atlas.*
import scalafim.image.world.{SpaceEvidence, WorldSpace, XformCode}

object PinnedVolumeLoader:
  def resolve(asset: PinnedAtlasAsset, store: AtlasStore, policy: AssetPolicy): Either[AtlasAcquisitionError, Path] =
    attempt(store.resolve(AtlasAsset(asset.key, asset.fileName, URI.create(asset.url), asset.minBytes, Some(asset.sha256)), policy))

  def verify(asset: PinnedAtlasAsset, path: Path): Either[AtlasAcquisitionError, Unit] =
    attempt((AtlasAsset.sha256(path), Files.size(path))).flatMap: (actual, size) =>
      if actual == asset.sha256 && size >= asset.minBytes then Right(())
      else Left(AtlasAcquisitionError.Integrity(asset.key, asset.sha256, actual))

  def load(
      ref: VolumeAtlasRef, declared: Vector[AtlasRegionMetadata], volume: Path,
      sources: Vector[(PinnedAtlasAsset, ArtifactRole, Path)],
      coordinates: AtlasCoordinateAdmission = AtlasCoordinateAdmission.RequireTemplateHeader
  ): Either[AtlasAcquisitionError, VolumeAtlas] =
    for
      _ <- Either.cond(sources.exists((_, role, path) => role == ArtifactRole.ParcellationVolume && path == volume),
        (), AtlasAcquisitionError.Io("volume must be covered by a pinned parcellation asset"))
      _ <- sources.foldLeft[Either[AtlasAcquisitionError, Unit]](Right(())):
        (result, source) => result.flatMap(_ => verify(source._1, source._3))
      regions <- attempt(RegionIndex(declared))
      volumeAsset = sources.find((_, role, path) => role == ArtifactRole.ParcellationVolume && path == volume).get._1
      admission <- coordinateEvidence(ref, volumeAsset, coordinates)
      (admittedRef, evidence, coordinateNote) = admission
      labels <- attempt(AtlasLabelMaps.readIntVolume(volume, evidence, ref.name))
      present = AtlasLabelMaps.presentRegionIds(labels)
      unknown = present.diff(regions.ids.toSet).toVector.map(_.value).sorted
      _ <- Either.cond(unknown.isEmpty, (), AtlasAcquisitionError.Coverage(unknown))
      _ <- Either.cond(present.nonEmpty, (), AtlasAcquisitionError.EmptyCoverage)
      retained = RegionIndex(regions.regions.filter(r => present.contains(r.id)))
      withAssets = admittedRef.withDetails(d => d.copy(artifacts = sources.map: (asset, role, _) =>
        AtlasArtifact(role, "Pinned atlas source", asset.fileName, sourceUrl = Some(asset.url),
          notes = Some(s"Immutable revision ${asset.revision}; expected SHA-256 ${asset.sha256}"))))
      sourceArtifacts = sources.map: (asset, role, path) =>
          SourceArtifact(s"${role.legacy}:${asset.key}", role, "Pinned atlas source", asset.fileName,
            sourceUri = Some(asset.url), localPath = Some(path.toString), digest = Some(Digest.sha256(asset.sha256)),
            licenseInfo = LicenseInfo.Unspecified("Consult the pinned upstream source terms"),
            notes = Some(s"Immutable revision ${asset.revision}"))
      originalProvenance = AtlasProvenance.loaded(withAssets, retained, regions.ids)
      baseProvenance = originalProvenance.copy(
        sources = NonEmptyVector.unsafe(sourceArtifacts),
        labels = LabelSchema.fromRegions(withAssets, retained, sourceArtifacts),
        derivation = sourceArtifacts.map(source => DerivationStep.Loaded(source.id)) ++
          originalProvenance.derivation.filter:
            case DerivationStep.Loaded(_) => false
            case _ => true
      )
      provenance = coordinateNote.fold(baseProvenance)(note =>
        baseProvenance.withDerivationStep(DerivationStep.DeclaredDescriptor(note)))
      atlas <- VolumeAtlas.fromLabelVolumeEither(withAssets, retained, labels, provenance)
        .left.map(AtlasAcquisitionError.Realization.apply)
    yield atlas

  private def coordinateEvidence(ref: VolumeAtlasRef, asset: PinnedAtlasAsset, policy: AtlasCoordinateAdmission):
      Either[AtlasAcquisitionError, (VolumeAtlasRef, SpaceEvidence, Option[String])] =
    policy match
      case AtlasCoordinateAdmission.RequireTemplateHeader =>
        Right((ref, SpaceEvidence(bidsSpace = Some(ref.templateSpace.value)), None))
      case AtlasCoordinateAdmission.DeclaredArtifactCoordinates(expected, reason) =>
        if !Set(XformCode.Unknown, XformCode.ScannerAnatomical, XformCode.AlignedAnatomical).contains(expected) then
          Left(AtlasAcquisitionError.Parse("artifact-coordinate admission cannot replace a specific template xform"))
        else if reason.trim.isEmpty then Left(AtlasAcquisitionError.Parse("declared artifact coordinates require a reason"))
        else
          val id = s"pinned-atlas-${asset.sha256}"
          WorldSpace.decode(s"scalafim-world:declared:$id", Some(s"${ref.name} artifact coordinates"))
            .left.map(e => AtlasAcquisitionError.Parse(e.message)).flatMap: world =>
              val note = s"Artifact-coordinate admission: selected header xform must be $expected. $reason Source declares ${ref.templateSpace.value}; coordinates retain artifact identity $id; standard-template routing is unqualified."
              AtlasRef.checked(ref.details.copy(confidence = Confidence.Uncertain,
                notes = Some(ref.notes.fold(note)(_ + " " + note))), AtlasRepresentation.Volume,
                ref.templateSpace, SpaceId.unknown(id)).left.map(e => AtlasAcquisitionError.Parse(e.message))
                .flatMap:
                  case admitted: AtlasRef.Volume =>
                    Right((admitted, SpaceEvidence(xform = Some(expected), assertion = Some(world)), Some(note)))
                  case _ => Left(AtlasAcquisitionError.Parse("expected volume reference"))

  def attempt[A](body: => A): Either[AtlasAcquisitionError, A] =
    try Right(body)
    catch case NonFatal(error) => Left(AtlasAcquisitionError.Io(Option(error.getMessage).getOrElse(error.getClass.getSimpleName)))
