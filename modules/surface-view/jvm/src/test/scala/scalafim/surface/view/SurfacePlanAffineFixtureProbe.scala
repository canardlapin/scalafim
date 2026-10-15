package scalafim.surface.view

import intaglio.*
import scalafim.image.{x, y, z}
import scalafim.surface.*
import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream}
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

/** Bounded test fixture producer for the frozen, opaque, unlit v23 affine cases.
  * Reads original geometry and RGBA, compiles the typed model, and writes only
  * public mesh/layer buffers after proving original vertex and face ordering.
  * The external probe's fixed pixel-parallel projection is retained: this
  * does not serialize or qualify application camera fitting, normals or lighting.
  */
object SurfacePlanAffineFixtureProbe:
  private final case class Original(
    positions: Array[Float],
    rgba: Array[Int],
    triangles: Array[Int],
    boundary: Array[Int]
  )

  private final case class Lowered(
    name: String,
    source: Path,
    original: Original,
    plan: SurfaceRenderPlan
  )

  private val names =
    for
      family <- Vector("diagonal", "rgb", "ramp32", "ramp128")
      order <- Vector("original", "cyclic", "shuffled")
      view <- Vector("front", "oblique")
      aa <- Vector("DISABLED", "BALANCED")
    yield s"$family-$order-$view-$aa.bin"

  private def readOriginal(path: Path): Original =
    val in = new DataInputStream(new BufferedInputStream(Files.newInputStream(path)))
    try
      require(in.readInt() == 23, s"$path requires frozen unlit v23")
      val vertices = in.readInt()
      val faces = in.readInt()
      require(vertices > 0 && vertices <= 20000, s"$path exceeds bounded vertex count")
      require(faces > 0 && faces <= 40000, s"$path exceeds bounded face count")
      val boundary = Array.fill(4)(in.readInt())
      require(boundary.distinct.length == 4 && boundary.forall(index => index >= 0 && index < vertices),
        s"$path has invalid original boundary ids")
      val positions = Array.fill(vertices * 3)(in.readFloat())
      require(positions.forall(_.isFinite), s"$path has nonfinite original coordinates")
      val rgba = Array.fill(vertices)(in.readInt())
      require(rgba.forall(color => (color & 255) == 255), s"$path requires opaque original RGBA")
      val triangles = Array.fill(faces * 3)(in.readInt())
      require(triangles.forall(index => index >= 0 && index < vertices), s"$path has invalid vertex ids")
      require(in.read() == -1, s"$path has unsupported trailing fields")
      Original(positions, rgba, triangles, boundary)
    finally in.close()

  private def admit(name: String, source: Path): Lowered =
    val original = readOriginal(source)
    val surface = SurfaceId.unsafe("color-oracle")
    val layerId = SurfaceLayerId.unsafe("original-rgb")
    val geometry = SurfaceGeometry(
      TriangleMesh.fromArrays(original.positions.map(_.toDouble), original.triangles),
      Hemisphere.Left,
      SurfaceKind.Inflated
    )
    val layer = SurfaceLayer.packedRgba(
      layerId, surface, geometry, original.rgba.toVector.map(Rgba32.fromPackedInt)
    ).fold(error => throw new IllegalArgumentException(error.message), identity)
    val asset = SurfaceAsset.make(surface, geometry)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val model = SurfaceViewerModel.make(Vector(asset), Vector(layer))
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val state = SurfaceViewerState.initial(model).copy(lighting = SurfaceLighting.Unlit)
    val plan = SurfaceCompiler.compile(model, state)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    require(plan.lighting == SurfaceLighting.Unlit && plan.clipping == SurfaceClipping.Disabled,
      s"$name has unsupported lighting or clipping")
    require(plan.slots.length == 1 && plan.meshes.length == 1 && plan.layers.length == 1 && plan.drawPasses.length == 1,
      s"$name has unsupported geometry, layer or pass multiplicity")
    val slot = plan.slots.head
    val mesh = plan.meshes.head
    val colors = plan.layers.head
    val pass = plan.drawPasses.head
    require(slot.surface == surface && mesh.surface == surface && colors.surface == surface && colors.layer == layerId,
      s"$name changed surface or layer identity")
    require(Vector(slot.worldOffsetX, slot.worldOffsetY, slot.worldOffsetZ)
      .forall(value => java.lang.Double.doubleToRawLongBits(value) == 0L), s"$name applies an unsupported world offset")
    require(colors.opacity == DisplayOpacity.Opaque && colors.blendMode == DisplayBlendMode.Normal,
      s"$name has unsupported opacity or blending")
    require(pass.slot == 0 && pass.mesh == mesh.resourceKey && pass.layer == colors.resourceKey &&
      pass.blendMode == DisplayBlendMode.Normal, s"$name draw pass does not address the admitted buffers")
    require(plan.receipt.meshKeys == Vector(mesh.resourceKey) && plan.receipt.layerKeys == Vector(colors.resourceKey) &&
      plan.receipt.drawPassCount == 1 && plan.receipt.timepoint == 0 && plan.readouts.isEmpty,
      s"$name has unsupported receipt or readout scope")
    require(mesh.positions.length == original.positions.length && colors.colors.length == original.rgba.length &&
      mesh.indices.length == original.triangles.length, s"$name changed original vertex or face counts")
    require(plan.profile.meshesPacked == 1 && plan.profile.layersColored == 1 &&
      plan.profile.verticesPacked == original.rgba.length && plan.profile.facesPacked == original.triangles.length / 3,
      s"$name profile changed original mesh shape")
    var vertex = 0
    while vertex < original.rgba.length do
      val id = VertexId(vertex)
      val point = geometry.mesh.vertex(id)
      val coordinates = Vector(point.x.toFloat, point.y.toFloat, point.z.toFloat)
      var axis = 0
      while axis < 3 do
        val offset = id.index * 3 + axis
        val expected = java.lang.Float.floatToRawIntBits(original.positions(offset))
        require(java.lang.Float.floatToRawIntBits(coordinates(axis)) == expected &&
          java.lang.Float.floatToRawIntBits(mesh.positions(offset)) == expected,
          s"$name changed original vertex ${id.index} axis $axis")
        axis += 1
      require(colors.colors(id.index) == original.rgba(id.index), s"$name changed vertex ${id.index} RGBA")
      vertex += 1
    var face = 0
    while face < original.triangles.length / 3 do
      val id = FaceId(face)
      val triangle = geometry.mesh.face(id)
      val vertices = Vector(triangle.a.index, triangle.b.index, triangle.c.index)
      var corner = 0
      while corner < 3 do
        val offset = id.index * 3 + corner
        require(vertices(corner) == original.triangles(offset) && mesh.indices(offset) == original.triangles(offset),
          s"$name changed original face ${id.index} corner $corner")
        corner += 1
      face += 1
    Lowered(name, source, original, plan)

  private def writePlan(path: Path, lowered: Lowered): Unit =
    val mesh = lowered.plan.meshes.head
    val colors = lowered.plan.layers.head
    val out = new DataOutputStream(new BufferedOutputStream(Files.newOutputStream(path)))
    try
      out.writeInt(23)
      out.writeInt(colors.colors.length)
      out.writeInt(mesh.indices.length / 3)
      lowered.original.boundary.foreach(out.writeInt)
      var index = 0
      while index < mesh.positions.length do
        out.writeFloat(mesh.positions(index))
        index += 1
      index = 0
      while index < colors.colors.length do
        out.writeInt(colors.colors(index))
        index += 1
      index = 0
      while index < mesh.indices.length do
        out.writeInt(mesh.indices(index))
        index += 1
    finally out.close()

  private def digest(path: Path): String =
    MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(path)).map(byte => f"${byte & 255}%02x").mkString

  def main(args: Array[String]): Unit =
    require(args.length == 2, "usage: SurfacePlanAffineFixtureProbe original-v23-folder output-folder")
    val input = Path.of(args(0)).toRealPath()
    val output = Path.of(args(1)).toAbsolutePath.normalize()
    require(!output.startsWith(input) && !input.startsWith(output), "fixture input and output must be separate")
    val files = Files.list(input)
    val actual =
      try files.iterator().asScala.filter(_.getFileName.toString.endsWith(".bin"))
        .map(_.getFileName.toString).toSet
      finally files.close()
    require(actual == names.toSet, "requires exactly the frozen 48 unlit affine case names")
    if Files.exists(output) then
      val existing = Files.list(output)
      try require(existing.findAny().isEmpty, "output folder must be empty")
      finally existing.close()
    // Admit the whole batch before creating any output file. No remapping or
    // facetization fallback is permitted by this fixture schema.
    val lowered = names.map(name => admit(name, input.resolve(name)))
    Files.createDirectories(output)
    val records = lowered.map: value =>
      val destination = output.resolve(value.name)
      writePlan(destination, value)
      val originalSha = digest(value.source)
      val planSha = digest(destination)
      require(originalSha == planSha, s"${value.name} plan binary differs from frozen original")
      require(java.util.Arrays.equals(Files.readAllBytes(value.source), Files.readAllBytes(destination)),
        s"${value.name} plan bytes differ from frozen original")
      println(s"PASS ${value.name} original vertex/face ids retained; SHA-256 $planSha")
      s"""{"name":"${value.name}","vertices":${value.original.rgba.length},"faces":${value.original.triangles.length / 3},"source_sha256":"$originalSha","plan_sha256":"$planSha","original_ids_retained":true}"""
    Files.writeString(output.resolve("plan-input-manifest.json"),
      s"""{"schema":"frozen-unlit-v23","status":"pass","cases":48,"camera":"external fixed pixel-parallel; application camera excluded","lighting":"Unlit","records":[${records.mkString(",")}]}\n""")
    println("PASS 48 typed SurfaceRenderPlan inputs; v23 fields and original vertex/face ids unchanged")
