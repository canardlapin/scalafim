package scalafim.surface.reference

import scalafim.surface.*
import scalafim.surface.io.GiftiReader

import java.nio.file.{Files, Path}

/** Locked TemplateFlow assets for the fsLR 32k ↔ MNI152NLin2009cAsym gates,
  * found under `$TEMPLATEFLOW_HOME` or `~/.cache/templateflow`. Each is
  * loaded once per JVM through the digest-verifying readers.
  */
object RealAssets:
  val catalogRevision = "templateflow@d79aacb1ad7d1c52e5d10ad88f48fd8af6e5ae56"
  val transformSha256 = "2e3869a07b96aec406e0419ca2e434afc54882d37cc212b933b139d1b63a4dfe"
  val manifestSha256 = "34bdcea2dab7c0fccde6e6607cd080fbcea087abaf19721925b5b8fb61d85318"
  val gmProbsegSha256 = "662b18e83dddc554b19c621d9750af3454b54d4e103df03633eacced3884805a"
  val midthicknessSha256 = Map(
    "L" -> "036a8b6c84fa4b581b7ad7b36d99190b57ad6755c9d7ef7adc3e9ffc6448f1af",
    "R" -> "9d2cef05096c433b134870456abebe7ff201cdda60ce5741d7b794018eeccda7")
  val nomedialwallSha256 = Map(
    "L" -> "4ac9199dab151ccdc2a35bdddb5bac4f4907da7dfebd90807c09b82e2eb9d512",
    "R" -> "698f46b399f5a89829f83cc697e32dc8841031fbf31ffef75aaaa0c4a16c3015")

  val release: TemplateRelease = TemplateRelease.unsafe(catalogRevision)
  val nlin6: TemplateFrame = TemplateFrame.make(TemplateId.unsafe("MNI152NLin6Asym"), release).toOption.get
  val nlin2009c: TemplateFrame = TemplateFrame.make(TemplateId.unsafe("MNI152NLin2009cAsym"), release).toOption.get

  val root: Path =
    sys.env.get("TEMPLATEFLOW_HOME").map(Path.of(_))
      .getOrElse(Path.of(sys.props("user.home"), ".cache", "templateflow"))
  val pointMapDirectory: Path = root.resolve(".templateflow4s/derived/point-map").resolve(transformSha256)
  private val gmPath = root.resolve("tpl-MNI152NLin2009cAsym/tpl-MNI152NLin2009cAsym_res-01_label-GM_probseg.nii.gz")
  private def midthicknessPath(h: String) = root.resolve(s"tpl-fsLR/tpl-fsLR_den-32k_hemi-${h}_midthickness.surf.gii")
  private def nomedialwallPath(h: String) = root.resolve(s"tpl-fsLR/tpl-fsLR_hemi-${h}_den-32k_desc-nomedialwall_dparc.label.gii")

  def pointMapPresent: Boolean = Files.isRegularFile(pointMapDirectory.resolve("manifest.json"))

  def evidencePresent: Boolean =
    pointMapPresent && Files.isRegularFile(gmPath) &&
      Vector("L", "R").forall(h => Files.isRegularFile(midthicknessPath(h)) && Files.isRegularFile(nomedialwallPath(h)))

  lazy val pointMap: DeclaredPointMap =
    DeclaredPointMapReader.read(pointMapDirectory, transformSha256, manifestSha256).fold(e => throw new IllegalStateException(e.message), identity)

  val gmAsset: AssetProvenance = AssetProvenance.make(TemplateId.unsafe("MNI152NLin2009cAsym"),
    "tpl-MNI152NLin2009cAsym/tpl-MNI152NLin2009cAsym_res-01_label-GM_probseg.nii.gz", catalogRevision, gmProbsegSha256).toOption.get

  lazy val gm: DeclaredVolume =
    val basis = FrameBasis.literature("10.1016/j.neuroimage.2010.07.033",
      "TemplateFlow tpl-MNI152NLin2009cAsym asset, defined in its own template frame").toOption.get
    DeclaredVolumeReader.readNifti(gmPath, FrameDeclaration.make(nlin2009c, basis, gmAsset).toOption.get)
      .fold(e => throw new IllegalStateException(e.message), identity)

  /** The WS2 basis for TemplateFlow tpl-fsLR surfaces. */
  val fslrBasis: FrameBasis = FrameBasis.literature("10.1093/cercor/bhr291",
    "TemplateFlow tpl-fsLR (HCP Pipelines templates; ReferencesAndLinks doi:10.1093/cercor/bhr291) — surfaces in " +
      "MNI152NLin6Asym per HCP convention; corroborated by FrameEvidence on the 2009c GM probseg").toOption.get

  final case class Hemisphere32k(label: String, surface: DeclaredSurface, cortex: Vector[Boolean], reference: CorticalMeshReference)

  lazy val hemispheres: Vector[Hemisphere32k] = Vector("L", "R").map: h =>
    val hemisphere = if h == "L" then Hemisphere.Left else Hemisphere.Right
    val archive = s"tpl-fsLR/tpl-fsLR_den-32k_hemi-${h}_midthickness.surf.gii"
    val declaration = FrameDeclaration.make(nlin6, fslrBasis,
      AssetProvenance.make(TemplateId.unsafe("fsLR"), archive, catalogRevision, midthicknessSha256(h)).toOption.get).toOption.get
    val surface = DeclaredSurfaceReader.read(midthicknessPath(h), declaration, hemisphere, SurfaceKind.Midthickness)
      .fold(e => throw new IllegalStateException(e.message), identity)
    val labelBytes = Files.readAllBytes(nomedialwallPath(h))
    require(AssetSha256.of(labelBytes).value == nomedialwallSha256(h), s"nomedialwall $h digest")
    val document = GiftiReader.read(labelBytes).fold(e => throw new IllegalStateException(e.message), identity)
    val cortex = GiftiReader.doubleData(document.dataArrays.head).fold(e => throw new IllegalStateException(e.message), identity)
      .toVector.map(_ > 0.5)
    val wall = MedialWallMask.fromCortexFlags(surface.geometry.meshDomainEither.toOption.get, cortex).toOption.get
    val reference = CorticalMeshReference.make(StandardCorticalMesh.FsLR32k, surface.geometry, wall).toOption.get
    Hemisphere32k(h, surface, cortex, reference)

/** Real-asset suites skip when their inputs are absent, unless
  * `SCALAFIM_REQUIRE_REAL_ASSETS=1`, which turns absence into a failure.
  */
trait RealAssetGate:
  self: munit.FunSuite =>

  def requireReal(present: Boolean, what: String): Unit =
    if !present && sys.env.get("SCALAFIM_REQUIRE_REAL_ASSETS").contains("1") then
      fail(s"$what not found and SCALAFIM_REQUIRE_REAL_ASSETS=1")
    assume(present, s"$what not found; skipped (set SCALAFIM_REQUIRE_REAL_ASSETS=1 to fail instead)")
