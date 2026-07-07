package scalafim.atlas.io

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scalafim.atlas.*
import scalafim.surface.{Hemisphere as SurfaceHemisphere, *}

class SurfaceGiftiAtlasLoaderSuite extends munit.FunSuite:

  test("loadFromPathsEither builds a bilateral SurfaceAtlas from GIFTI label payloads"):
    withGiftiPair(labelXml(Vector(1, 1, 0, 2)), labelXml(Vector(3, 3, 0, 0))) { paths =>
      val atlas =
        SurfaceGiftiAtlasLoader
          .loadFromPathsEither(ref, geometry, paths, SurfaceGiftiAtlasLoader.Options(label = "synthetic"))
          .toOption
          .get

      assertEquals(atlas.labelIdAt(SurfaceHemisphere.Left, VertexId(0)), Some(RegionId(1)))
      assertEquals(atlas.labelIdAt(SurfaceHemisphere.Left, VertexId(2)), None)
      assertEquals(atlas.labelIdAt(SurfaceHemisphere.Right, VertexId(0)), Some(RegionId(3)))
      assertEquals(atlas.region(RegionId(1)).flatMap(_.hemisphere), Some(Hemisphere.Left))
      assertEquals(atlas.region(RegionId(3)).flatMap(_.hemisphere), Some(Hemisphere.Right))
      assertEquals(atlas.region(RegionId(2)).flatMap(_.color), Some(Rgb(0, 255, 0)))
      assert(atlas.provenance.sourceArtifacts.exists(_.localPath.contains(paths.left.toString)))
      assert(atlas.provenance.sourceArtifacts.exists(_.localPath.contains(paths.right.toString)))
      assertEquals(atlas.provenance.labels.background, Some(0))
    }

  test("loadFromPathsEither supports sparse NODE_INDEX label payloads"):
    val left = labelXml(Vector(1, 2), nodeIndices = Some(Vector(1, 3)))
    val right = labelXml(Vector(3), nodeIndices = Some(Vector(0)))

    withGiftiPair(left, right) { paths =>
      val atlas = SurfaceGiftiAtlasLoader.loadFromPathsEither(ref, geometry, paths).toOption.get

      assertEquals(atlas.left.labelAt(VertexId(0)), None)
      assertEquals(atlas.left.labelAt(VertexId(1)), Some(1))
      assertEquals(atlas.left.labelAt(VertexId(3)), Some(2))
      assertEquals(atlas.right.labelAt(VertexId(0)), Some(3))
      assertEquals(atlas.region(RegionId(1)).flatMap(_.hemisphere), Some(Hemisphere.Left))
    }

  test("loadFromPathsEither rejects payload labels missing from the GIFTI LabelTable"):
    withGiftiPair(labelXml(Vector(1, 9, 0, 2)), labelXml(Vector(3, 3, 0, 0))) { paths =>
      val result = SurfaceGiftiAtlasLoader.loadFromPathsEither(ref, geometry, paths)

      assertEquals(
        result.left.map(_.message),
        Left("GIFTI label payload contains ids not present in LabelTable: 9")
      )
    }

  private val ref: SurfaceAtlasRef =
    AtlasRef.surface(
      family = "synthetic",
      model = "GIFTI",
      templateSpace = SpaceId.FsAverage6,
      coordSpace = SpaceId.FsAverage6,
      density = Some("4v"),
      provenance = Some("synthetic GIFTI labels"),
      source = Some("test"),
      confidence = Confidence.Exact
    )

  private val geometry: SurfaceGiftiAtlasLoader.Geometry =
    SurfaceGiftiAtlasLoader.Geometry(
      left = surface(SurfaceHemisphere.Left),
      right = surface(SurfaceHemisphere.Right)
    )

  private def surface(hemisphere: SurfaceHemisphere): SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        vertices = Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 1.0)
        ),
        faces = Vector((0, 1, 2), (0, 1, 3))
      ),
      hemisphere,
      SurfaceKind.Midthickness
    )

  private def withGiftiPair[A](leftXml: String, rightXml: String)(f: SurfaceGiftiAtlasLoader.Paths => A): A =
    val dir = Files.createTempDirectory("scalafim-surface-gifti-atlas-")
    val left = dir.resolve("left.label.gii")
    val right = dir.resolve("right.label.gii")
    Files.writeString(left, leftXml, StandardCharsets.UTF_8)
    Files.writeString(right, rightXml, StandardCharsets.UTF_8)
    try f(SurfaceGiftiAtlasLoader.Paths(left, right))
    finally
      Files.deleteIfExists(left)
      Files.deleteIfExists(right)
      Files.deleteIfExists(dir)

  private def labelXml(labels: Vector[Int], nodeIndices: Option[Vector[Int]] = None): String =
    val nodeIndexArray =
      nodeIndices match
        case None => ""
        case Some(indices) =>
          s"""|  <DataArray Intent="NIFTI_INTENT_NODE_INDEX" DataType="NIFTI_TYPE_INT32"
              |             ArrayIndexingOrder="RowMajorOrder" Dimensionality="1"
              |             Dim0="${indices.length}" Encoding="ASCII" Endian="LittleEndian">
              |    <Data>${indices.mkString(" ")}</Data>
              |  </DataArray>
              |""".stripMargin

    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<GIFTI Version="1.0" NumberOfDataArrays="${nodeIndices.fold(1)(_ => 2)}">
       |  <MetaData>
       |    <MD><Name>Name</Name><Value>SyntheticGIFTIAtlas</Value></MD>
       |  </MetaData>
       |  <LabelTable>
       |    <Label Key="0" Red="0" Green="0" Blue="0" Alpha="0">Background</Label>
       |    <Label Key="1" Red="1" Green="0" Blue="0" Alpha="1">Left_A</Label>
       |    <Label Key="2" Red="0" Green="1" Blue="0" Alpha="1">Left_B</Label>
       |    <Label Key="3" Red="0" Green="0" Blue="1" Alpha="1">Right_A</Label>
       |  </LabelTable>
       |$nodeIndexArray  <DataArray Intent="NIFTI_INTENT_LABEL" DataType="NIFTI_TYPE_INT32"
       |             ArrayIndexingOrder="RowMajorOrder" Dimensionality="1"
       |             Dim0="${labels.length}" Encoding="ASCII" Endian="LittleEndian">
       |    <Data>${labels.mkString(" ")}</Data>
       |  </DataArray>
       |</GIFTI>
       |""".stripMargin
