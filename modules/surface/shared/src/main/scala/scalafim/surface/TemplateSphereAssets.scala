package scalafim.surface

/** A TemplateFlow registration-sphere file: which template surface it places on which sphere, where it lives under a
  * TemplateFlow home, and the SHA-256 of the bytes ScalaFIM's parity evidence was recorded against.
  */
final case class TemplateSphereAsset(
    surface: TemplateSurface,
    registration: SphereRegistration,
    relativePath: String,
    sha256: String
) derives CanEqual:
  def identity: String = s"templateflow:$relativePath|sha256=$sha256"

/** The pinned TemplateFlow spheres (`tools/transform/generate_template_sphere_resampling_oracle.py` records them).
  *
  * fsaverage 5, 6 and 7 are on the fsaverage sphere (7 is the radius-normalised `desc-std` sphere, 5 and 6 its nested
  * prefixes); fsLR 32k/59k/164k are on the fsLR sphere, and again deformed onto the fsaverage sphere
  * (`space-fsaverage`), which is what carries data between fsaverage and fsLR.
  */
object TemplateSphereAssets:
  private val digests: Map[String, String] =
    Map(
      "tpl-fsaverage/tpl-fsaverage_hemi-L_den-10k_sphere.surf.gii" -> "421d73e12a9cdce3224327601b73b0bf475943665854b4dc61869a8d93f23a8d",
      "tpl-fsaverage/tpl-fsaverage_hemi-L_den-41k_sphere.surf.gii" -> "f3826fb34c273ad26844d922853e7cf0121d5ed1594f79c2a3cd525a88239d66",
      "tpl-fsaverage/tpl-fsaverage_hemi-L_den-164k_desc-std_sphere.surf.gii" -> "9bf5b673cc1c4738b1137ac53ff724ff0ef45e12f5550448efa2da50bb521c32",
      "tpl-fsLR/tpl-fsLR_hemi-L_den-32k_sphere.surf.gii" -> "1846b053f870405466776d004d714cc1da0cec7361761c65a864782dd09f30a8",
      "tpl-fsLR/tpl-fsLR_space-fsaverage_hemi-L_den-32k_sphere.surf.gii" -> "31987d139b6cdf1040188d1795bf23c213c57f8e84fb45f2c04823d996b0defc",
      "tpl-fsLR/tpl-fsLR_hemi-L_den-59k_sphere.surf.gii" -> "65d38a5f938c2667f94f8978af4750ed66f76ce9060884bafccc3e9cdc52d178",
      "tpl-fsLR/tpl-fsLR_space-fsaverage_hemi-L_den-59k_sphere.surf.gii" -> "6d25494ddb49de4b782fa3fb91b05075fd64325cb329020fc166adc330c23122",
      "tpl-fsLR/tpl-fsLR_hemi-L_den-164k_sphere.surf.gii" -> "6968ef7d9638de54ade78cdbb7f0c6f25025c60477f72456ab2fb77c53a85cfa",
      "tpl-fsLR/tpl-fsLR_space-fsaverage_hemi-L_den-164k_sphere.surf.gii" -> "1f3b09b813fe32c934a565264dccc357629834259fa34f39de5c8c00dc0fec92",
      "tpl-fsaverage/tpl-fsaverage_hemi-R_den-10k_sphere.surf.gii" -> "b92a57bafc8239f2fdbb64fbedc0e9d787e3860c08b5a4321ebbe3f9c2df480f",
      "tpl-fsaverage/tpl-fsaverage_hemi-R_den-41k_sphere.surf.gii" -> "80558deca8e7efe1f092345effb1f099c0e5adfb48ca5e064145debceec780c2",
      "tpl-fsaverage/tpl-fsaverage_hemi-R_den-164k_desc-std_sphere.surf.gii" -> "a2fada23394286212f38cd44303bce0b3252759e09fe693f92d463286ca28b45",
      "tpl-fsLR/tpl-fsLR_hemi-R_den-32k_sphere.surf.gii" -> "1a898433a9f1070e4e0435d4db966776ecc3291bfcd444efc7910aeffb91561c",
      "tpl-fsLR/tpl-fsLR_space-fsaverage_hemi-R_den-32k_sphere.surf.gii" -> "8c1f67fa8fb3024ad3b292be995d5c9bae07dc4dbdd53fbac48e511e1366553d",
      "tpl-fsLR/tpl-fsLR_hemi-R_den-59k_sphere.surf.gii" -> "6df50dfa10d872f0aca5b4e9c96b6dbf0546131fec469f918445433cab805d60",
      "tpl-fsLR/tpl-fsLR_space-fsaverage_hemi-R_den-59k_sphere.surf.gii" -> "41a471a8fc5bff1330289646c61d95b7fe3e6c57297092e6af513c8c822ad6b6",
      "tpl-fsLR/tpl-fsLR_hemi-R_den-164k_sphere.surf.gii" -> "85e718e424a1d87521fb0f3942538db16d8c745fe5b9ee20405baac2aff1dbb1",
      "tpl-fsLR/tpl-fsLR_space-fsaverage_hemi-R_den-164k_sphere.surf.gii" -> "f024337147073263dc11e56603afcb9dae2c48cf8869809388d5985eb95a585a"
    )

  /** The TemplateFlow path of `surface` on `registration`, if TemplateFlow publishes one. */
  def relativePath(surface: TemplateSurface, registration: SphereRegistration): Option[String] =
    val hemi = surface.hemisphere match
      case CorticalHemisphere.Left  => "L"
      case CorticalHemisphere.Right => "R"
    val density = surface.mesh.density
    (surface.mesh.family, registration) match
      case (TemplateFamily.FsAverage, SphereRegistration.FsAverage) =>
        val desc = if surface.mesh == TemplateMesh.FsAverage7 then "_desc-std" else ""
        Some(s"tpl-fsaverage/tpl-fsaverage_hemi-${hemi}_den-$density${desc}_sphere.surf.gii")
      case (TemplateFamily.FsLR, SphereRegistration.FsLR) =>
        Some(s"tpl-fsLR/tpl-fsLR_hemi-${hemi}_den-${density}_sphere.surf.gii")
      case (TemplateFamily.FsLR, SphereRegistration.FsAverage) =>
        Some(s"tpl-fsLR/tpl-fsLR_space-fsaverage_hemi-${hemi}_den-${density}_sphere.surf.gii")
      case (TemplateFamily.FsAverage, SphereRegistration.FsLR) => None

  /** The pinned asset for `surface` on `registration`. */
  def find(surface: TemplateSurface, registration: SphereRegistration): Option[TemplateSphereAsset] =
    relativePath(surface, registration).flatMap(path => digests.get(path).map(TemplateSphereAsset(surface, registration, path, _)))

  /** Every pinned asset. */
  val all: Vector[TemplateSphereAsset] =
    for
      mesh         <- TemplateMesh.values.toVector
      hemisphere   <- CorticalHemisphere.values.toVector
      registration <- SphereRegistration.values.toVector
      asset        <- find(TemplateSurface(mesh, hemisphere), registration)
    yield asset
