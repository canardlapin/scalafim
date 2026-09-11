package scalafim.surface.view.raster.spike

import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream}
import java.nio.file.{Files, Path}

import intaglio.*
import scalafim.surface.view.*

/** Fitted slots for one canvas size. The producing compiler resolves viewport
  * fitting (letterboxing) before serialisation so a consumer without that
  * compiler feature renders the same geometry.
  */
final case class SpikeFittedSlots(width: Int, height: Int, slots: Vector[SurfaceViewSlot])

/** A compiled `SurfaceRenderPlan` plus per-size fitted slots and free-form
  * JSON metadata. This is the scene description that crosses the boundary
  * between the producing application and a figure interpreter.
  */
final case class SpikeScene(plan: SurfaceRenderPlan, fitted: Vector[SpikeFittedSlots], metadata: String):
  def planFor(width: Int, height: Int): SurfaceRenderPlan =
    fitted.find(f => f.width == width && f.height == height) match
      case Some(f) => plan.copy(slots = f.slots)
      case None => throw new IllegalArgumentException(s"no fitted slots for ${width}x$height")

/** Spike-only binary codec for `SurfaceRenderPlan` (big-endian, versioned magic).
  * Chrome and readouts are not carried: figures render without the readout label.
  */
object SpikePlanCodec:
  private val Magic = "SFIM-SPIKE-PLAN-1"

  def write(path: Path, scene: SpikeScene): Unit =
    val out = new DataOutputStream(new BufferedOutputStream(Files.newOutputStream(path), 1 << 20))
    try
      out.writeUTF(Magic)
      out.writeUTF(scene.metadata)
      val plan = scene.plan
      writeSlots(out, plan.slots)
      out.writeInt(plan.meshes.length)
      plan.meshes.foreach: mesh =>
        out.writeUTF(mesh.surface.value)
        out.writeUTF(mesh.resourceKey.value)
        out.writeUTF(mesh.geometryKey.value)
        writeFloats(out, mesh.positions.unsafeArray)
        writeFloats(out, mesh.normals.unsafeArray)
        writeInts(out, mesh.indices.unsafeArray)
      out.writeInt(plan.layers.length)
      plan.layers.foreach: layer =>
        out.writeUTF(layer.layer.value)
        out.writeUTF(layer.surface.value)
        out.writeUTF(layer.resourceKey.value)
        writeInts(out, layer.colors.unsafeArray)
        out.writeDouble(layer.opacity.toDouble)
        out.writeUTF(layer.blendMode.toString)
      writeFloats(out, plan.camera.viewMatrix.unsafeArray)
      writeFloats(out, plan.camera.projectionMatrix.unsafeArray)
      out.writeDouble(plan.camera.directionX)
      out.writeDouble(plan.camera.directionY)
      out.writeDouble(plan.camera.directionZ)
      plan.lighting match
        case SurfaceLighting.Unlit =>
          out.writeUTF("unlit")
        case SurfaceLighting.Directional(ambient, diffuse, dx, dy, dz) =>
          out.writeUTF("directional")
          out.writeDouble(ambient.value); out.writeDouble(diffuse.value)
          out.writeDouble(dx); out.writeDouble(dy); out.writeDouble(dz)
      plan.clipping match
        case SurfaceClipping.Disabled => out.writeUTF("disabled")
        case SurfaceClipping.NearFar(near, far) =>
          out.writeUTF("near-far"); out.writeDouble(near); out.writeDouble(far)
        case SurfaceClipping.WorldPlanes(planes) =>
          out.writeUTF("world-planes")
          out.writeInt(planes.length)
          planes.foreach: plane =>
            out.writeDouble(plane.normalX); out.writeDouble(plane.normalY); out.writeDouble(plane.normalZ)
            out.writeDouble(plane.offset)
            out.writeUTF(plane.keep.toString)
      out.writeInt(plan.drawPasses.length)
      plan.drawPasses.foreach: pass =>
        out.writeInt(pass.slot)
        out.writeUTF(pass.mesh.value)
        out.writeUTF(pass.layer.value)
        out.writeUTF(pass.blendMode.toString)
      val profile = plan.profile
      out.writeInt(profile.meshesPacked); out.writeInt(profile.verticesPacked); out.writeInt(profile.facesPacked)
      out.writeInt(profile.layersColored); out.writeInt(profile.colorValuesWritten); out.writeLong(profile.primitiveBytes)
      val receipt = plan.receipt
      out.writeInt(receipt.meshKeys.length); receipt.meshKeys.foreach(key => out.writeUTF(key.value))
      out.writeInt(receipt.layerKeys.length); receipt.layerKeys.foreach(key => out.writeUTF(key.value))
      out.writeUTF(receipt.cameraKey)
      out.writeInt(receipt.drawPassCount)
      out.writeInt(receipt.timepoint)
      out.writeInt(scene.fitted.length)
      scene.fitted.foreach: fitted =>
        out.writeInt(fitted.width); out.writeInt(fitted.height)
        writeSlots(out, fitted.slots)
    finally out.close()

  def read(path: Path): SpikeScene =
    val in = new DataInputStream(new BufferedInputStream(Files.newInputStream(path), 1 << 20))
    try
      val magic = in.readUTF()
      require(magic == Magic, s"unexpected plan magic '$magic'")
      val metadata = in.readUTF()
      val slots = readSlots(in)
      val meshes = Vector.fill(in.readInt()):
        val surface = SurfaceId.unsafe(in.readUTF())
        val key = in.readUTF()
        val geometryKey = in.readUTF()
        val positions = readFloats(in)
        val normals = readFloats(in)
        val indices = readInts(in)
        SurfaceMeshPacket(
          surface,
          SurfaceResourceKey(key),
          new FloatBufferView(positions),
          new FloatBufferView(normals),
          new IntBufferView(indices),
          if geometryKey == key then None else Some(SurfaceResourceKey(geometryKey))
        )
      val layers = Vector.fill(in.readInt()):
        val layer = SurfaceLayerId.unsafe(in.readUTF())
        val surface = SurfaceId.unsafe(in.readUTF())
        val key = in.readUTF()
        val colors = readInts(in)
        val opacity = DisplayOpacity.unsafe(in.readDouble())
        val blend = DisplayBlendMode.valueOf(in.readUTF())
        SurfaceLayerPacket(layer, surface, SurfaceResourceKey(key), new IntBufferView(colors), opacity, blend)
      val view = readFloats(in)
      val projection = readFloats(in)
      val camera = SurfaceCameraPacket(
        new FloatBufferView(view), new FloatBufferView(projection),
        in.readDouble(), in.readDouble(), in.readDouble()
      )
      val lighting = in.readUTF() match
        case "unlit" => SurfaceLighting.Unlit
        case "directional" =>
          val ambient = in.readDouble(); val diffuse = in.readDouble()
          SurfaceLighting.Directional(
            LightFraction.unsafe(ambient), LightFraction.unsafe(diffuse),
            in.readDouble(), in.readDouble(), in.readDouble()
          )
        case other => throw new IllegalStateException(s"unknown lighting '$other'")
      val clipping = in.readUTF() match
        case "disabled" => SurfaceClipping.Disabled
        case "near-far" => SurfaceClipping.NearFar(in.readDouble(), in.readDouble())
        case "world-planes" =>
          SurfaceClipping.WorldPlanes(Vector.fill(in.readInt()):
            val nx = in.readDouble(); val ny = in.readDouble(); val nz = in.readDouble()
            val offset = in.readDouble()
            WorldClipPlane.unsafe(nx, ny, nz, offset, ClipKeepSide.valueOf(in.readUTF())))
        case other => throw new IllegalStateException(s"unknown clipping '$other'")
      val passes = Vector.fill(in.readInt()):
        val slot = in.readInt()
        val mesh = in.readUTF()
        val layer = in.readUTF()
        SurfaceDrawPass(slot, SurfaceResourceKey(mesh), SurfaceResourceKey(layer), DisplayBlendMode.valueOf(in.readUTF()))
      val profile = SurfaceProfile(in.readInt(), in.readInt(), in.readInt(), in.readInt(), in.readInt(), in.readLong())
      val meshKeys = Vector.fill(in.readInt())(SurfaceResourceKey(in.readUTF()))
      val layerKeys = Vector.fill(in.readInt())(SurfaceResourceKey(in.readUTF()))
      val receipt = SurfaceRenderReceipt(meshKeys, layerKeys, in.readUTF(), in.readInt(), in.readInt())
      val fitted = Vector.fill(in.readInt()):
        val width = in.readInt(); val height = in.readInt()
        SpikeFittedSlots(width, height, readSlots(in))
      val plan = SurfaceRenderPlan(
        slots, meshes, layers, camera, lighting, clipping, passes,
        Scene.empty, Vector.empty, profile, receipt
      )
      SpikeScene(plan, fitted, metadata)
    finally in.close()

  private def writeSlots(out: DataOutputStream, slots: Vector[SurfaceViewSlot]): Unit =
    out.writeInt(slots.length)
    slots.foreach: slot =>
      out.writeUTF(slot.surface.value)
      out.writeDouble(slot.viewport.x); out.writeDouble(slot.viewport.y)
      out.writeDouble(slot.viewport.width); out.writeDouble(slot.viewport.height)
      out.writeDouble(slot.worldOffsetX); out.writeDouble(slot.worldOffsetY); out.writeDouble(slot.worldOffsetZ)

  private def readSlots(in: DataInputStream): Vector[SurfaceViewSlot] =
    Vector.fill(in.readInt()):
      val surface = SurfaceId.unsafe(in.readUTF())
      val viewport = SurfaceViewport(in.readDouble(), in.readDouble(), in.readDouble(), in.readDouble())
      SurfaceViewSlot(surface, viewport, in.readDouble(), in.readDouble(), in.readDouble())

  private def writeFloats(out: DataOutputStream, values: Array[Float]): Unit =
    out.writeInt(values.length)
    var index = 0
    while index < values.length do
      out.writeFloat(values(index))
      index += 1

  private def readFloats(in: DataInputStream): Array[Float] =
    val values = new Array[Float](in.readInt())
    var index = 0
    while index < values.length do
      values(index) = in.readFloat()
      index += 1
    values

  private def writeInts(out: DataOutputStream, values: Array[Int]): Unit =
    out.writeInt(values.length)
    var index = 0
    while index < values.length do
      out.writeInt(values(index))
      index += 1

  private def readInts(in: DataInputStream): Array[Int] =
    val values = new Array[Int](in.readInt())
    var index = 0
    while index < values.length do
      values(index) = in.readInt()
      index += 1
    values
