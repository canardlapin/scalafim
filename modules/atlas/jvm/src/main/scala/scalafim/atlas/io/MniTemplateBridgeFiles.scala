package scalafim.atlas.io

import java.nio.file.Path

import scalafim.atlas.{AtlasError, MniTemplateBridge, TemplateFlowXfm}
import scalafim.surface.io.TemplateFlowCache
import scalafim.transform.{AssetRef, NativeTransform, TransformError, TransformFiles}

/** Loads the TemplateFlow MNI152NLin6Asym -> MNI152NLin2009cAsym composite from disk (about 200 MB; jHDF). */
object MniTemplateBridgeFiles:
  /** Read, hash and admit one composite file; see [[MniTemplateBridge.fromItk]] for what is checked. */
  def load(path: Path): Either[AtlasError, MniTemplateBridge] =
    // Hash first: an uninspected or refused file is rejected without decoding 200 MB of HDF5. The decoded bytes are
    // hashed again by TransformFiles, and fromItk checks that digest, so a file replaced in between is still refused.
    val digest =
      try Right(AtlasAsset.sha256(path))
      catch case scala.util.control.NonFatal(error) => Left(AtlasError.TemplateAssetMissing(path.toString, Vector(Option(error.getMessage).getOrElse(error.toString))))
    digest
      .flatMap(sha => MniTemplateBridge.admitted(AssetRef(path.toString, Some(sha))))
      .flatMap(_ => TransformFiles.load(path).left.map(error => AtlasError.Transform(TransformError.Io(error))))
      .flatMap: loaded =>
        loaded.native match
          case NativeTransform.ItkHdf5(file) => MniTemplateBridge.fromItk(file, loaded.asset)
          case other =>
            Left(AtlasError.TemplateAssetRefused(path.toString, s"not an ITK HDF5 composite (${other.format})"))

  /** The composite from a local TemplateFlow cache ([[TemplateFlowCache.roots]] by default). */
  def loadCached(roots: Vector[Path] = TemplateFlowCache.roots): Either[AtlasError, MniTemplateBridge] =
    val relative = TemplateFlowXfm.Mni6ToMni2009c.relativePath
    TemplateFlowCache
      .locate(relative, roots)
      .toRight(AtlasError.TemplateAssetMissing(relative, roots.map(_.toString)))
      .flatMap(load)
