package scalafim.atlas.io

import java.nio.file.Path
import scalafim.atlas.*

object JulichVisualLoader:
  def load(store: AtlasStore = FileAtlasStore.default, policy: AssetPolicy = AssetPolicy.CacheOrDownload):
      Either[AtlasAcquisitionError, VolumeAtlas] =
    FslAtlasLoader.load(JulichVisualRequest.source, store, policy).flatMap(JulichVisualRequest.select)

  def loadFromPaths(xml: Path, volume: Path): Either[AtlasAcquisitionError, VolumeAtlas] =
    FslAtlasLoader.loadFromPaths(JulichVisualRequest.source, xml, volume).flatMap(JulichVisualRequest.select)
