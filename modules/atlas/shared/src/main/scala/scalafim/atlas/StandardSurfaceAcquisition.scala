package scalafim.atlas

/** Explicit immutable annotation pair. Only callers with verified pins can
  * request a standard surface atlas; unsupported families/formats are not
  * silently downloaded or guessed.
  */
final case class StandardSurfaceAnnotationRequest(
    ref: SurfaceAtlasRef,
    left: PinnedAtlasAsset,
    right: PinnedAtlasAsset,
    geometry: Option[StandardSurfaceGeometryAssets] = None,
    sourceName: String = "Pinned surface annotation",
    license: LicenseInfo = LicenseInfo.Unspecified("Consult pinned upstream source terms")
)

/** Immutable geometry receipts paired with annotation labels. Geometry is
  * supplied separately to the loader so callers retain control of its surface
  * kind, but a standard request records the bytes it is compatible with.
  */
final case class StandardSurfaceGeometryAssets(left: PinnedAtlasAsset, right: PinnedAtlasAsset)

object StandardSurfaceAnnotationRequest:
  /** Pin-qualified CBIG Schaefer 100/7 fsaverage6 annotations. */
  val schaefer100x7FsAverage6: StandardSurfaceAnnotationRequest =
    val revision = "634f676630929a71297852d01dd92a287103e861"
    val base = s"https://raw.githubusercontent.com/ThomasYeoLab/CBIG/$revision/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/FreeSurfer5.3/fsaverage6/label/"
    StandardSurfaceAnnotationRequest(
      Schaefer2018Surface(SchaeferParcels.P100, YeoNetworks.Seven).atlasRef(),
      PinnedAtlasAsset("schaefer100x7-fsaverage6-lh", "lh.Schaefer2018_100Parcels_7Networks_order.annot", base + "lh.Schaefer2018_100Parcels_7Networks_order.annot", revision, "5b6cf5f1da9b9900ad8b2785e595d27c1d6665ea541be02cd900d8cb2d523ecc"),
      PinnedAtlasAsset("schaefer100x7-fsaverage6-rh", "rh.Schaefer2018_100Parcels_7Networks_order.annot", base + "rh.Schaefer2018_100Parcels_7Networks_order.annot", revision, "9728a5e58ecf7851e7ddd2739b90fc31c7bd234edc157fd6b9d58ecfef93b555"),
      Some(StandardSurfaceGeometryAssets(
        PinnedAtlasAsset("schaefer100x7-fsaverage6-lh-white", "lh.white", base.replace("label/", "surf/") + "lh.white", revision, "9e927bc7ed863e0e4d01035616456e06b170847dfe7b4fc1e46628ed030e598b"),
        PinnedAtlasAsset("schaefer100x7-fsaverage6-rh-white", "rh.white", base.replace("label/", "surf/") + "rh.white", revision, "8671586105b1cac6c1ca1b8db265e84a64d2a7dfd518a131a98885f6def57373")
      ))
    )

  /** Verified Kathryn Mills fsaverage projection of HCP-MMP1.0 (CC-BY 4.0).
    * This is a fsaverage derivative, not an assertion of native fsLR parity.
    * Geometry remains caller-supplied and must be admitted separately.
    */
  val glasserMillsFsAverage: StandardSurfaceAnnotationRequest =
    val revision = "figshare-3498446-v2"
    StandardSurfaceAnnotationRequest(
      GlasserHcpMmp1Surface(StandardSurface.FsAverage).atlasRef(Confidence.High),
      PinnedAtlasAsset("glasser-mills-fsaverage-lh", "lh.HCP-MMP1.annot", "https://ndownloader.figshare.com/files/5528816", revision, "d4da634644b4c595dbda23963e01752059f0e7714be70169eb84e25e09ba2b44"),
      PinnedAtlasAsset("glasser-mills-fsaverage-rh", "rh.HCP-MMP1.annot", "https://ndownloader.figshare.com/files/5528819", revision, "744eff4e57ce8121c43851eea425475baf92d9ba8686299a7435517ab972e9a2"),
      sourceName = "Kathryn Mills Figshare 3498446 v2 fsaverage projection",
      license = LicenseInfo.Known("CC-BY-4.0")
    )
