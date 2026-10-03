package scalafim.surface.reference

import scalafim.surface.*
import scalafim.surface.io.GiftiReader
import scalafim.surface.gifti.GiftiPayload
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

/** Digest-bound 0/1 cortex flags on an ordered hemisphere domain. */
object DeclaredMedialWallReader:
  def read(path: Path, domain: SurfaceMeshDomain, asset: DeclaredAsset): Either[ReferenceError, MedialWallMask] =
    try
      val bytes = Files.readAllBytes(path)
      for
        _ <- asset.checkDigest(bytes)
        document <- GiftiReader.read(bytes).left.map(e => ReferenceError.AssetReadFailure(e.message))
        // Locked Conte69 ROI files use INTENT_NONE and float32 despite their
        // .label.gii suffix. Admit that exact 1D boolean encoding explicitly.
        array <- document.dataArrays match
          case Vector(one) if one.rank == 1 && (one.isLabel || one.intent == scalafim.surface.gifti.GiftiIntent.NoneIntent) => Right(one)
          case _ => Left(ReferenceError.InvalidMedialWall("expected one 1D label/ROI array"))
        values <- GiftiReader.doublePayload(array).flatMap(GiftiPayload.requireVector)
          .left.map(e => ReferenceError.InvalidMedialWall(e.message))
        _ <- Either.cond(values.values.forall(v => v == 0 || v == 1), (),
          ReferenceError.InvalidMedialWall("cortex flags must be 0 or 1"))
        primary = array.metadata.get("AnatomicalStructurePrimary").orElse(document.metadata.get("AnatomicalStructurePrimary"))
        expected = if domain.hemisphere == CorticalHemisphere.Left then "CortexLeft" else "CortexRight"
        _ <- Either.cond(primary.contains(expected), (),
          ReferenceError.InvalidMedialWall(s"mask hemisphere $primary differs from $expected or is undeclared"))
        mask <- MedialWallMask.verified(domain, values.values.map(_ == 1), bytes, asset)
      yield mask
    catch case NonFatal(error) => Left(ReferenceError.AssetReadFailure(s"$path: ${SurfaceError.reason(error)}"))
