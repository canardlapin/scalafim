package scalafim.atlas.io

import java.io.{DataOutputStream, FileOutputStream}
import java.nio.file.Files
import scalafim.atlas.*
import scalafim.surface.{CorticalHemisphere, Hemisphere as SurfaceHemisphere, SurfaceGeometry, SurfaceKind, TriangleMesh, VertexId}
import scalafim.surface.io.FreeSurferAnnotationReader

class StandardSurfaceAtlasLoaderSuite extends munit.FunSuite:
  test("FreeSurfer annotation sides are remapped to distinct canonical parcel ids"):
    val directory = Files.createTempDirectory("scalafim-standard-surface-")
    val left = directory.resolve("lh.annot")
    val right = directory.resolve("rh.annot")
    writeOldAnnotation(left, "left-area", 1, 2, 3)
    writeOldAnnotation(right, "right-area", 1, 2, 3)
    try
      val request = StandardSurfaceAnnotationRequest(
        AtlasRef.surface("synthetic", "annot", SpaceId.FsAverage6, SpaceId.FsAverage6, confidence = Confidence.Exact),
        pinned("left", left),
        pinned("right", right)
      )
      val atlas = StandardSurfaceAtlasLoader.loadFromPaths(request, geometry, left, right).fold(error => fail(error.message), identity)
      assertEquals(atlas.labelIdAt(CorticalHemisphere.Left, VertexId(0)), Some(RegionId(1)))
      assertEquals(atlas.labelIdAt(CorticalHemisphere.Right, VertexId(0)), Some(RegionId(2)))
      assertEquals(atlas.region(RegionId(1)).flatMap(_.hemisphere), Some(Hemisphere.Left))
      assertEquals(atlas.region(RegionId(2)).flatMap(_.hemisphere), Some(Hemisphere.Right))
      assertEquals(atlas.labelInfo(CorticalHemisphere.Left, RegionId(1)).map(_.name), Some("left-area"))
      assertEquals(atlas.labelInfo(CorticalHemisphere.Right, RegionId(2)).map(_.name), Some("right-area"))
      assertEquals(atlas.provenance.sourceArtifacts.map(_.localPath), Vector(Some(left.toString), Some(right.toString)))
      assertEquals(atlas.provenance.sourceArtifacts.flatMap(_.digest.map(_.value)), Vector(AtlasAsset.sha256(left), AtlasAsset.sha256(right)))
      assertEquals(atlas.provenance.derivation.collect { case DerivationStep.Loaded(id) => id }, Vector("surface_annotation:left", "surface_annotation:right"))
    finally
      Files.deleteIfExists(left)
      Files.deleteIfExists(right)
      Files.deleteIfExists(directory)

  test("annotation geometry coverage is checked before realization"):
    val file = Files.createTempFile("scalafim-annotation-coverage-", ".annot")
    writeOldAnnotation(file, "area", 1, 2, 3, vertices = 2)
    try
      val request = StandardSurfaceAnnotationRequest(
        AtlasRef.surface("synthetic", "annot", SpaceId.FsAverage6, SpaceId.FsAverage6), pinned("left", file), pinned("right", file))
      val result = StandardSurfaceAtlasLoader.loadFromPaths(request, geometry, file, file)
      assertEquals(result.left.map(_.message), Left("atlas metadata parse failed: annotation vertex count does not match supplied left/right geometry"))
    finally Files.deleteIfExists(file)

  test("pinned bilateral Mills Glasser projection annotations verify and decode as fsaverage derivatives"):
    val request = StandardSurfaceAnnotationRequest.glasserMillsFsAverage
    val left = resourceAt("figshare-glasser", "lh.annot")
    val right = resourceAt("figshare-glasser", "rh.annot")
    assertEquals(PinnedVolumeLoader.verify(request.left, left), Right(()))
    assertEquals(PinnedVolumeLoader.verify(request.right, right), Right(()))
    val decodedLeft = FreeSurferAnnotationReader.read(left).fold(error => fail(error), identity)
    val decodedRight = FreeSurferAnnotationReader.read(right).fold(error => fail(error), identity)
    assertEquals(decodedLeft.vertexAnnotations.length, 163842)
    assertEquals(decodedRight.vertexAnnotations.length, 163842)
    assert(decodedLeft.table.exists(_.name == "L_V1_ROI"))
    assert(decodedRight.table.exists(_.name == "R_V1_ROI"))
    assert(decodedLeft.table.exists(_.name == "???"))
    assert(decodedRight.table.exists(_.name == "???"))
    assertEquals(request.ref.templateSpace, SpaceId.FsAverage)
    val geometry = syntheticGeometry(163842)
    val atlas = StandardSurfaceAtlasLoader.loadFromPaths(request, geometry, left, right).fold(error => fail(error.message), identity)
    assertEquals(atlas.regions.ids.length, 360)
    assertEquals(atlas.region(RegionId(1)).flatMap(_.labelFull.map(_.value)), Some("L_V1_ROI"))
    assertEquals(atlas.regions.regions.count(_.hemisphere.contains(Hemisphere.Left)), 180)
    assertEquals(atlas.regions.regions.count(_.hemisphere.contains(Hemisphere.Right)), 180)
    assertEquals(atlas.provenance.sourceArtifacts.map(_.licenseInfo), Vector(LicenseInfo.Known("CC-BY-4.0"), LicenseInfo.Known("CC-BY-4.0")))

  test("opt-in CBIG Schaefer pair verifies annotations and matching geometry receipts"):
    val keys = Vector("lh.annotation", "rh.annotation", "lh.white", "rh.white")
    val paths = keys.map(key => sys.props.get(s"scalafim.cbig.$key").map(java.nio.file.Path.of(_)))
    if paths.forall(_.nonEmpty) then
      val values = paths.map(_.get)
      val leftAnnotation = values(0)
      val rightAnnotation = values(1)
      val leftGeometry = values(2)
      val rightGeometry = values(3)
      val atlas = StandardSurfaceAtlasLoader.loadFromPathsWithVerifiedGeometry(
        StandardSurfaceAnnotationRequest.schaefer100x7FsAverage6,
        leftAnnotation, rightAnnotation, leftGeometry, rightGeometry
      ).fold(error => fail(error.message), identity)
      assertEquals(atlas.regions.ids.length, 100)
    else assume(false, "source-backed qualification inactive")

  private val geometry =
    val mesh = TriangleMesh.fromRows(
      Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0)),
      Vector((0, 1, 2))
    )
    SurfaceGiftiAtlasLoader.Geometry(
      SurfaceGeometry(mesh, SurfaceHemisphere.Left, SurfaceKind.Midthickness),
      SurfaceGeometry(mesh, SurfaceHemisphere.Right, SurfaceKind.Midthickness)
    )

  private def pinned(key: String, path: java.nio.file.Path): PinnedAtlasAsset =
    PinnedAtlasAsset(key, s"$key.annot", s"https://example.invalid/$key/immutable.annot", "fixture", AtlasAsset.sha256(path))

  private def resourceAt(directory: String, name: String): java.nio.file.Path =
    val url = Option(getClass.getResource(s"/scalafim/atlas/io/$directory/$name")).getOrElse(fail(s"missing source fixture $directory/$name"))
    java.nio.file.Path.of(url.toURI)

  private def syntheticGeometry(vertices: Int): SurfaceGiftiAtlasLoader.Geometry =
    val coordinates = Array.fill(vertices * 3)(0.0)
    val faces = Array(0, 1, 2)
    val mesh = TriangleMesh.fromArrays(coordinates, faces)
    SurfaceGiftiAtlasLoader.Geometry(
      SurfaceGeometry(mesh, SurfaceHemisphere.Left, SurfaceKind.White),
      SurfaceGeometry(mesh, SurfaceHemisphere.Right, SurfaceKind.White)
    )

  private def writeOldAnnotation(path: java.nio.file.Path, name: String, red: Int, green: Int, blue: Int, vertices: Int = 3): Unit =
    val out = DataOutputStream(FileOutputStream(path.toFile))
    try
      val packed = red | (green << 8) | (blue << 16)
      out.writeInt(vertices)
      var vertex = vertices - 1
      while vertex >= 0 do
        out.writeInt(vertex); out.writeInt(packed)
        vertex -= 1
      out.writeInt(1)
      out.writeInt(1)
      cstring(out, "fixture")
      cstring(out, name)
      out.writeInt(red); out.writeInt(green); out.writeInt(blue); out.writeInt(0)
    finally out.close()

  private def cstring(out: DataOutputStream, value: String): Unit =
    val bytes = (value + "\u0000").getBytes("UTF-8")
    out.writeInt(bytes.length)
    out.write(bytes)
