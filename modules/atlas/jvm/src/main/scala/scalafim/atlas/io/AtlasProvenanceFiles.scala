package scalafim.atlas.io

import java.nio.file.Path
import scalafim.atlas.*

object AtlasProvenanceFiles:
  def withLocalFiles(provenance: AtlasProvenance, files: (ArtifactRole, Path)*): AtlasProvenance =
    val pathByRole = files.toMap
    provenance.mapSourceArtifacts { artifact =>
      pathByRole.get(artifact.role) match
        case Some(path) => artifact.withResolvedFile(path.toString, AtlasAsset.digest(path))
        case None => artifact
    }
