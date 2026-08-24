package scalafim.examples.atlas

import java.nio.file.Path
import scalafim.atlas.*
import scalafim.atlas.io.*
import scalafim.image.SampleSpaces.*

final case class LoadedAtlasSummary(
  name: String,
  nRegions: Int,
  spatialDims: Vector[Int],
  sourceArtifacts: Vector[String],
  strictIssues: Vector[ProvenanceIssue]
):
  def lines: Vector[String] =
    Vector(
      s"atlas: $name",
      s"regions: $nRegions",
      s"spatialDims: ${spatialDims.mkString("x")}",
      s"sources: ${sourceArtifacts.mkString(", ")}",
      s"strictIssues: ${strictIssues.map(_.toString).mkString(", ")}"
    )

object LoadAtlasFromPaths:
  def schaefer(
    volumePath: Path,
    labelPath: Path,
    spec: Schaefer2018 = Schaefer2018.default
  ): LoadedAtlasSummary =
    summarize(SchaeferLoader.loadFromPaths(spec, volumePath, labelPath))

  def glasser(
    volumePath: Path,
    labelPath: Path,
    spec: GlasserHcpMmp1 = GlasserHcpMmp1(GlasserSource.Mni2009c)
  ): LoadedAtlasSummary =
    summarize(GlasserLoader.loadFromPaths(spec, volumePath, labelPath))

  def brainnetome(
    volumePath: Path,
    lutPath: Path,
    networkPath: Option[Path] = None,
    spec: Brainnetome246 = Brainnetome246.default
  ): LoadedAtlasSummary =
    summarize(BrainnetomeLoader.loadFromPaths(spec, volumePath, lutPath, networkPath))

  def aseg(
    volumePath: Path,
    spec: FreeSurferAseg = FreeSurferAseg.default
  ): LoadedAtlasSummary =
    summarize(AsegLoader.loadFromPaths(spec, volumePath))

  def summarize(atlas: VolumeAtlas): LoadedAtlasSummary =
    LoadedAtlasSummary(
      name = atlas.name,
      nRegions = atlas.regions.size,
      spatialDims = atlas.space.spatialDims,
      sourceArtifacts = atlas.provenance.sourceArtifacts.map(source => source.localPath.getOrElse(source.sourceRef)),
      strictIssues = atlas.provenance.validate(strict = true)
    )

@main def loadSchaeferAtlas(volumePath: String, labelPath: String): Unit =
  LoadAtlasFromPaths
    .schaefer(Path.of(volumePath), Path.of(labelPath))
    .lines
    .foreach(println)
