package scalafim.examples.surface

import java.nio.file.Path
import scalafim.surface.*
import scalafim.surface.io.*

final case class SurfaceGeometrySummary(
  source: String,
  vertexCount: Int,
  faceCount: Int,
  hemisphere: Hemisphere,
  kind: SurfaceKind,
  edgeCount: Int,
  surfaceArea: Double,
  worldOffset: Vector[Double]
):
  def tabSeparated: String =
    Vector(
      source,
      vertexCount.toString,
      faceCount.toString,
      hemisphere.toString,
      kind.label,
      edgeCount.toString,
      surfaceArea.toString,
      worldOffset.mkString("[", ",", "]")
    ).mkString("\t")

object SurfaceIoExamples:
  def readFreeSurfer(path: Path): SurfaceGeometry =
    FreeSurferSurfaceReader.read(path)

  def readGifti(path: Path): SurfaceGeometry =
    GiftiSurfaceReader.read(path)

  def bundledSummaries(): Vector[SurfaceGeometrySummary] =
    Vector(
      summarize("FreeSurfer ASCII", readFreeSurfer(SurfaceExampleResources.freeSurferAsciiPath())),
      summarize("GIFTI", readGifti(SurfaceExampleResources.giftiPath()))
    )

  def summarize(source: String, geometry: SurfaceGeometry): SurfaceGeometrySummary =
    val topology = MeshTopology.from(geometry.mesh)
    SurfaceGeometrySummary(
      source = source,
      vertexCount = geometry.vertexCount,
      faceCount = geometry.faceCount,
      hemisphere = geometry.hemisphere,
      kind = geometry.kind,
      edgeCount = topology.edgeCount,
      surfaceArea = topology.surfaceArea,
      worldOffset = Vector(
        geometry.surfaceToWorld(0, 3),
        geometry.surfaceToWorld(1, 3),
        geometry.surfaceToWorld(2, 3)
      )
    )

@main def inspectExampleSurfaces(): Unit =
  println("source\tvertices\tfaces\themisphere\tkind\tedges\tsurfaceArea\tworldOffset")
  SurfaceIoExamples.bundledSummaries().foreach(row => println(row.tabSeparated))
