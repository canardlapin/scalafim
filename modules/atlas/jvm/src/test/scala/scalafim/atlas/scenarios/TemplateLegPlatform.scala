package scalafim.atlas.scenarios

import java.nio.file.Path

import scalafim.atlas.{AtlasError, TemplateFlowXfm, TemplateSurfaceSampling}
import scalafim.atlas.io.{AtlasAsset, MniBridgeFixtures}
import scalafim.surface.*
import scalafim.surface.io.{GiftiSurfaceReader, TemplateFlowCache, TemplateSphereFiles}

import scala.io.Source

/** The template leg's assets from a local TemplateFlow cache ([[TemplateFlowCache.roots]]); nothing is downloaded. */
object TemplateLegPlatform:
  /** fsLR 32k's left midthickness (the Conte69 average, Caret 5.65), whose GIFTI declares Talairach-space coordinates:
    * HCP's FSL MNI152, i.e. MNI152NLin6Asym. Pinned to the bytes this scenario's evidence was recorded against.
    */
  val Midthickness: String = "tpl-fsLR/tpl-fsLR_den-32k_hemi-L_midthickness.surf.gii"
  val MidthicknessSha256: String = "036a8b6c84fa4b581b7ad7b36d99190b57ad6755c9d7ef7adc3e9ffc6448f1af"

  private val Oracle = "/scalafim/atlas/oracle/templateflow_mni_bridge"

  private val spheres: Vector[TemplateSurface] =
    Vector(TemplateMesh.FsAverage7, TemplateMesh.FsLR32k).map(TemplateSurface(_, CorticalHemisphere.Left))

  lazy val assets: TemplateLegAssets =
    val sphereFiles = spheres.map(surface => surface -> TemplateSphereAssets.relativePath(surface, SphereRegistration.FsAverage).getOrElse(surface.display))
    val wanted = Vector(TemplateFlowXfm.Mni6ToMni2009c.relativePath, Midthickness) ++ sphereFiles.map(_._2)
    val found = wanted.map(relative => relative -> TemplateFlowCache.locate(relative)).toMap
    val missing = wanted.filter(found(_).isEmpty)
    if missing.nonEmpty then TemplateLegAssets.Absent(missing)
    else
      val loaded =
        for
          midthickness <- midthickness(found(Midthickness).get)
          bridge <- MniBridgeFixtures.inverted.getOrElse(Left(AtlasError.TemplateAssetMissing(TemplateFlowXfm.Mni6ToMni2009c.relativePath, Vector.empty)))
          loadedSpheres <- sphereFiles.foldLeft[Either[AtlasError, Vector[TemplateSphere[SphereRegistration.FsAverage.type]]]](Right(Vector.empty)):
            case (acc, (surface, relative)) =>
              acc.flatMap(out => TemplateSphereFiles.load(surface, SphereRegistration.FsAverage, found(relative).get).left.map(AtlasError.TemplateSurface.apply).map(out :+ _))
          sampling <- TemplateSurfaceSampling.on(SphereRegistration.FsAverage, CorticalHemisphere.Left, loadedSpheres)
        yield TemplateLegInputs(
          bridge,
          midthickness,
          s"templateflow:$Midthickness|sha256=$MidthicknessSha256",
          sampling,
          oracle("pull_points.tsv"),
          oracle("push_points.tsv")
        )
      loaded.fold(error => TemplateLegAssets.Unusable(error.message), TemplateLegAssets.Ready.apply)

  private def midthickness(path: Path): Either[AtlasError, SurfaceGeometry] =
    val digest = AtlasAsset.sha256(path)
    if !digest.equalsIgnoreCase(MidthicknessSha256) then
      Left(AtlasError.TemplateAssetRefused(Midthickness, s"SHA-256 $digest is not the pinned $MidthicknessSha256"))
    else GiftiSurfaceReader.readEither(path, Hemisphere.Left, SurfaceKind.Midthickness).left.map(AtlasError.TemplateSurface.apply)

  /** An oracle table's rows: key, input x y z, output x y z (the bridge suite checks the table against its manifest). */
  private def oracle(name: String): Vector[(Vector[Double], Vector[Double])] =
    val stream = getClass.getResourceAsStream(s"$Oracle/$name")
    require(stream != null, s"missing oracle file $name")
    val text = try Source.fromInputStream(stream, "UTF-8").mkString finally stream.close()
    text.linesIterator.drop(1).filter(_.nonEmpty).map(_.split('\t').toVector).toVector.map(cells => (cells.slice(1, 4).map(_.toDouble), cells.slice(4, 7).map(_.toDouble)))
