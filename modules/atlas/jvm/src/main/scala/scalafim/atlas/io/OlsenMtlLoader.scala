package scalafim.atlas.io

import scalafim.atlas.*
import scalafim.image.world.XformCode

object OlsenMtlLoader:
  def load(request: OlsenMtlRequest = OlsenMtlRequest(), store: AtlasStore = FileAtlasStore.default,
      policy: AssetPolicy = AssetPolicy.CacheOrDownload): Either[AtlasAcquisitionError, VolumeAtlas] =
    PinnedVolumeLoader.resolve(request.volume, store, policy).flatMap(path => loadFromPath(request, path))

  def loadFromPath(request: OlsenMtlRequest, volume: java.nio.file.Path): Either[AtlasAcquisitionError, VolumeAtlas] =
    PinnedVolumeLoader.load(request.ref, OlsenMtlRequest.regions, volume,
      Vector((request.volume, ArtifactRole.ParcellationVolume, volume)),
      AtlasCoordinateAdmission.DeclaredArtifactCoordinates(
        XformCode.ScannerAnatomical,
        "Pinned Olsen header identifies scanner anatomical coordinates while source metadata declares only MNI152_custom."
      )).flatMap: atlas =>
      request.mode match
        case OlsenMtlMode.Mtl => Right(atlas)
        case OlsenMtlMode.Hippocampus => atlas.subsetEither(region => OlsenMtlRequest.hippocampalIds.contains(region.id.value))
          .left.map(AtlasAcquisitionError.Realization.apply)
