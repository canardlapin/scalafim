package scalafim.atlas.scenarios

import scalafim.atlas.{MniTemplateBridge, TemplateSurfaceSampling}
import scalafim.surface.SurfaceGeometry

/** What the template-volume leg of the surface chain scenario reads, all of it TemplateFlow assets that are never
  * committed: the MNI152NLin6Asym -> MNI152NLin2009cAsym composite (with its qualified numerical inverse), fsLR 32k's
  * left midthickness, and the fsaverage-sphere spheres of fsaverage and fsLR 32k.
  *
  * @param bridge            the composite, its forward map attached by `withNumericalInverse`
  * @param midthickness      the fsLR 32k left midthickness (Conte69), stored coordinates in MNI152NLin6Asym
  * @param midthicknessAsset what the midthickness coordinates are (TemplateFlow path and SHA-256)
  * @param sampling          the left fsaverage and fsLR 32k spheres on the fsaverage sphere
  * @param pullOracle        SimpleITK TransformPoint of the composite: MNI152NLin2009cAsym points and their 6Asym images
  * @param pushOracle        SimpleITK's fixed-point inverse: MNI152NLin6Asym points and their 2009c images
  */
final case class TemplateLegInputs(
    bridge: MniTemplateBridge,
    midthickness: SurfaceGeometry,
    midthicknessAsset: String,
    sampling: TemplateSurfaceSampling,
    pullOracle: Vector[(Vector[Double], Vector[Double])],
    pushOracle: Vector[(Vector[Double], Vector[Double])]
)

/** Whether this platform and cache can run the template leg. */
enum TemplateLegAssets:
  /** Nothing to run it with: the named assets are in no cache, or the platform reads no files (Scala.js). */
  case Absent(missing: Vector[String])

  /** An asset is present but was refused or failed to load: the scenario fails, it is never treated as absent. */
  case Unusable(reason: String)

  case Ready(inputs: TemplateLegInputs)
