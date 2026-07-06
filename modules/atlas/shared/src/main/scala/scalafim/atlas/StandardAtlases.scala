package scalafim.atlas

import scalafim.surface.SurfaceKind

enum SchaeferParcels(val value: Int):
  case P100 extends SchaeferParcels(100)
  case P200 extends SchaeferParcels(200)
  case P300 extends SchaeferParcels(300)
  case P400 extends SchaeferParcels(400)
  case P500 extends SchaeferParcels(500)
  case P600 extends SchaeferParcels(600)
  case P700 extends SchaeferParcels(700)
  case P800 extends SchaeferParcels(800)
  case P900 extends SchaeferParcels(900)
  case P1000 extends SchaeferParcels(1000)

enum YeoNetworks(val value: Int):
  case Seven extends YeoNetworks(7)
  case Seventeen extends YeoNetworks(17)

enum VoxelResolution(val mm: Int):
  case OneMm extends VoxelResolution(1)
  case TwoMm extends VoxelResolution(2)

enum StandardSurface(
  val key: String,
  val spaceId: SpaceId,
  val density: String,
  val verticesPerHemisphere: Option[Int],
  val preferredKind: SurfaceKind
):
  case FsAverage extends StandardSurface("fsaverage", SpaceId.FsAverage, "164k", Some(163842), SurfaceKind.Pial)
  case FsAverage6 extends StandardSurface("fsaverage6", SpaceId.FsAverage6, "41k", Some(40962), SurfaceKind.Pial)
  case FsAverage5 extends StandardSurface("fsaverage5", SpaceId.FsAverage5, "10k", Some(10242), SurfaceKind.Pial)
  case FsLR32k extends StandardSurface("fsLR_32k", SpaceId.FsLR32k, "32k", Some(32492), SurfaceKind.Midthickness)

final case class Schaefer2018(
  parcels: SchaeferParcels,
  networks: YeoNetworks,
  resolution: VoxelResolution = VoxelResolution.TwoMm
):
  def id: String =
    s"schaefer-${parcels.value}-${networks.value}-${resolution.mm}mm"

  def model: String =
    "Schaefer2018"

  def atlasRef(confidence: Confidence = Confidence.High): AtlasRef =
    AtlasRef(
      family = "schaefer",
      model = model,
      representation = AtlasRepresentation.Volume,
      templateSpace = SpaceId.MNI152NLin6Asym,
      coordSpace = SpaceId.MNI152,
      resolution = Some(s"${resolution.mm}mm"),
      provenance = Some("https://github.com/ThomasYeoLab/CBIG"),
      source = Some("cbig_mni"),
      lineage = Some("Computed on fsaverage6 and sampled to MNI volume in CBIG release."),
      confidence = confidence,
      notes = Some(s"${parcels.value} parcels, ${networks.value} Yeo networks")
    )

object Schaefer2018:
  val default: Schaefer2018 =
    Schaefer2018(SchaeferParcels.P400, YeoNetworks.Seventeen)

final case class Schaefer2018Surface(
  parcels: SchaeferParcels,
  networks: YeoNetworks,
  surface: StandardSurface = StandardSurface.FsAverage6
):
  def id: String =
    s"schaefer-surface-${parcels.value}-${networks.value}-${surface.key}"

  def model: String =
    "Schaefer2018"

  def atlasRef(confidence: Confidence = Confidence.High): AtlasRef =
    AtlasRef(
      family = "schaefer",
      model = model,
      representation = AtlasRepresentation.Surface,
      templateSpace = surface.spaceId,
      coordSpace = surface.spaceId,
      density = Some(surface.density),
      provenance = Some("https://github.com/ThomasYeoLab/CBIG"),
      source = Some("cbig_surface"),
      lineage = Some("Computed on fsaverage6 in the CBIG Schaefer2018 release; other surface spaces require explicit resampling plans."),
      confidence = confidence,
      notes = Some(s"${parcels.value} parcels, ${networks.value} Yeo networks, ${surface.key} surface labels"),
      artifacts = Vector(
        AtlasArtifact(
          role = "surface-labels",
          sourceName = "CBIG Schaefer2018 surface annotations",
          sourceRef = s"${parcels.value}Parcels_${networks.value}Networks_${surface.key}",
          sourceUrl = Some(
            "https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/FreeSurfer5.3"
          ),
          license = Some("Unspecified: consult CBIG upstream repository terms"),
          notes = Some("Descriptor only; no shared-core asset download.")
        )
      )
    )

object Schaefer2018Surface:
  val default: Schaefer2018Surface =
    Schaefer2018Surface(SchaeferParcels.P400, YeoNetworks.Seventeen)

final case class GlasserHcpMmp1(source: GlasserSource = GlasserSource.XcpEngine):
  def id: String =
    s"glasser-${source.key}"

  def atlasRef(confidence: Confidence = source.defaultConfidence): AtlasRef =
    AtlasRef(
      family = "glasser",
      model = "HCP-MMP1.0",
      representation = AtlasRepresentation.Volume,
      templateSpace = source.defaultTemplateSpace,
      coordSpace = SpaceId.MNI152,
      resolution = source.resolution,
      provenance = Some(source.provenance),
      source = Some(source.key),
      lineage = Some(source.lineage),
      confidence = confidence,
      notes = source.notes
    )

enum GlasserSource(
  val key: String,
  val defaultTemplateSpace: SpaceId,
  val resolution: Option[String],
  val provenance: String,
  val lineage: String,
  val defaultConfidence: Confidence,
  val notes: Option[String]
):
  case XcpEngine extends GlasserSource(
    "xcpengine",
    SpaceId("MNI152_unspecified"),
    None,
    "xcpEngine glasser360MNI.nii.gz",
    "xcpEngine-distributed volumetric Glasser360 labelmap.",
    Confidence.Uncertain,
    Some("Stable runtime source but less explicit template provenance.")
  )
  case Mni2009c extends GlasserSource(
    "mni2009c",
    SpaceId.MNI152NLin2009cAsym,
    Some("1mm"),
    "MMP_in_MNI_corr.nii.gz",
    "MNI152NLin2009cAsym-provenance Glasser volume.",
    Confidence.High,
    None
  )

final case class GlasserHcpMmp1Surface(surface: StandardSurface = StandardSurface.FsLR32k):
  def id: String =
    s"glasser-surface-${surface.key}"

  def atlasRef(confidence: Confidence = Confidence.High): AtlasRef =
    AtlasRef(
      family = "glasser",
      model = "HCP-MMP1.0",
      representation = AtlasRepresentation.Surface,
      templateSpace = surface.spaceId,
      coordSpace = surface.spaceId,
      density = Some(surface.density),
      provenance = Some("HCP Workbench / HCP-MMP1.0 surface annotations"),
      source = Some("hcp_mmp_surface"),
      lineage = Some("HCP-MMP1.0 cortical areas distributed as surface annotations; volume representations are derived payloads."),
      confidence = confidence,
      notes = Some(s"360 cortical areas on ${surface.key}; use SurfaceAtlas for left/right labeled surfaces."),
      artifacts = Vector(
        AtlasArtifact(
          role = "surface-labels",
          sourceName = "HCP-MMP1.0 surface annotations",
          sourceRef = s"HCP-MMP1.0_${surface.key}",
          sourceUrl = Some("https://balsa.wustl.edu/study/show/RVVG"),
          citationDoi = Some("10.1038/nature18933"),
          license = Some("Unspecified: consult HCP-MMP1.0 and BALSA source terms"),
          notes = Some("Descriptor only; no shared-core asset download.")
        )
      )
    )

object GlasserHcpMmp1Surface:
  val default: GlasserHcpMmp1Surface =
    GlasserHcpMmp1Surface()

final case class Brainnetome246(source: BrainnetomeSource = BrainnetomeSource.CasCloud):
  def id: String =
    s"brainnetome-246-${source.key}"

  def model: String =
    "BrainnetomeAtlas246"

  def atlasRef(confidence: Confidence = Confidence.High): AtlasRef =
    AtlasRef(
      family = "brainnetome",
      model = model,
      representation = AtlasRepresentation.Volume,
      templateSpace = SpaceId.MNI152,
      coordSpace = SpaceId.MNI152,
      resolution = Some("1mm"),
      provenance = Some(source.provenance),
      source = Some(source.key),
      lineage = Some("Brainnetome Center MNI152 1mm maximum-probability map."),
      confidence = confidence,
      notes = Some("246 parcels; use is governed by the Brainnetome website legal agreement.")
    )

object Brainnetome246:
  val default: Brainnetome246 =
    Brainnetome246()

enum BrainnetomeSource(
  val key: String,
  val provenance: String,
  val pageUrl: String,
  val notes: Option[String]
):
  case CasCloud extends BrainnetomeSource(
    "brainnetome_download",
    "https://atlas.brainnetome.org/download.html",
    "https://atlas.brainnetome.org/download.html",
    Some("Runtime download from Brainnetome Center CAS Cloud share.")
  )

final case class FreeSurferAseg(source: AsegSource = AsegSource.NeuroatlasExtdata):
  def id: String =
    s"aseg-${source.key}"

  def model: String =
    "FreeSurferASEG"

  def atlasRef(confidence: Confidence = Confidence.High): AtlasRef =
    AtlasRef(
      family = "aseg",
      model = model,
      representation = AtlasRepresentation.Volume,
      templateSpace = SpaceId.MNI152NLin6Asym,
      coordSpace = SpaceId.MNI152,
      resolution = Some("1mm"),
      provenance = Some(source.provenance),
      source = Some(source.key),
      lineage = Some("Bundled neuroatlas volume derived from FreeSurfer ASEG labels."),
      confidence = confidence,
      notes = Some(
        "Header (193x229x193; 1mm; RAS) matches MNI152NLin6Asym; FreeSurfer aparc+aseg uses FSL MNI152 as standard space."
      )
    )

object FreeSurferAseg:
  val default: FreeSurferAseg =
    FreeSurferAseg()

enum AsegSource(
  val key: String,
  val provenance: String,
  val sourceUrl: String,
  val notes: Option[String]
):
  case NeuroatlasExtdata extends AsegSource(
    "bundled_extdata",
    "inst/extdata/atlas_aparc_aseg_prob33.nii.gz",
    "https://raw.githubusercontent.com/bbuchsbaum/neuroatlas/master/inst/extdata/atlas_aparc_aseg_prob33.nii.gz",
    Some("Packaged in neuroatlas inst/extdata.")
  )

object StandardAtlases:
  val schaeferSpec: AtlasSpec =
    AtlasSpec(
      id = "schaefer",
      label = "Schaefer2018 cortical parcellation",
      family = "schaefer",
      defaultSpace = SpaceId.MNI152NLin6Asym,
      representation = AtlasRepresentation.Volume,
      aliases = Vector("schaefer2018", "schaefer_volume")
    )

  val glasserSpec: AtlasSpec =
    AtlasSpec(
      id = "glasser",
      label = "Glasser HCP-MMP1.0 cortical parcellation",
      family = "glasser",
      defaultSpace = SpaceId.MNI152NLin2009cAsym,
      representation = AtlasRepresentation.Volume,
      aliases = Vector("hcp-mmp", "hcp_mmp", "mmp1", "glasser360")
    )

  val brainnetomeSpec: AtlasSpec =
    AtlasSpec(
      id = "brainnetome",
      label = "Brainnetome 246-region atlas",
      family = "brainnetome",
      defaultSpace = SpaceId.MNI152,
      representation = AtlasRepresentation.Volume,
      aliases = Vector("bna", "brainnetome246")
    )

  val asegSpec: AtlasSpec =
    AtlasSpec(
      id = "aseg",
      label = "FreeSurfer ASEG subcortical atlas",
      family = "aseg",
      defaultSpace = SpaceId.MNI152NLin6Asym,
      representation = AtlasRepresentation.Volume,
      aliases = Vector("freesurfer_aseg")
    )

  val schaeferSurfaceSpec: AtlasSpec =
    AtlasSpec(
      id = "schaefer-surface",
      label = "Schaefer2018 cortical surface parcellation",
      family = "schaefer",
      defaultSpace = SpaceId.FsAverage6,
      representation = AtlasRepresentation.Surface,
      aliases = Vector("schaefer2018_surface", "schaefer_fsaverage", "schaefer_surface")
    )

  val glasserSurfaceSpec: AtlasSpec =
    AtlasSpec(
      id = "glasser-surface",
      label = "Glasser HCP-MMP1.0 cortical surface parcellation",
      family = "glasser",
      defaultSpace = SpaceId.FsLR32k,
      representation = AtlasRepresentation.Surface,
      aliases = Vector("hcp-mmp-surface", "hcp_mmp_surface", "glasser360_surface")
    )

  val builtins: Vector[AtlasSpec] =
    Vector(schaeferSpec, glasserSpec, brainnetomeSpec, asegSpec, schaeferSurfaceSpec, glasserSurfaceSpec)

  def registerBuiltins(registry: AtlasRegistry): AtlasRegistry =
    builtins.foldLeft(registry)((acc, spec) => acc.register(spec))
