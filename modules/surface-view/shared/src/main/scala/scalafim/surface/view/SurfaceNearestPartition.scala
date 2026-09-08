package scalafim.surface.view

/** Fixed display subdivision; the scientific mesh and its ordered faces stay intact.
  * Every original face produces six separate triangles (eighteen corners).
  */
final class SurfaceNearestPartition private[view] (val originalIndices: IntBufferView):
  def originalFaceCount: Int = originalIndices.length / 3
  def renderFaceCount: Int = originalFaceCount * 6

  def sourceFace(renderFace: Int): Int =
    if renderFace < renderFaceCount then renderFace / 6
    else originalFaceCount + renderFace - renderFaceCount

  def weights(renderCorner: Int): (Double, Double, Double) =
    SurfaceNearestPartition.Corners(renderCorner % 18)

  def sourceWeights(renderFace: Int, a: Double, b: Double, c: Double): (Double, Double, Double) =
    if renderFace >= renderFaceCount then (a, b, c)
    else
      val wa = weights(renderFace * 3)
      val wb = weights(renderFace * 3 + 1)
      val wc = weights(renderFace * 3 + 2)
      (a * wa._1 + b * wb._1 + c * wc._1,
       a * wa._2 + b * wb._2 + c * wc._2,
       a * wa._3 + b * wb._3 + c * wc._3)

object SurfaceNearestPartition:
  private val A = (1.0, 0.0, 0.0)
  private val B = (0.0, 1.0, 0.0)
  private val C = (0.0, 0.0, 1.0)
  private val AB = (0.5, 0.5, 0.0)
  private val BC = (0.0, 0.5, 0.5)
  private val CA = (0.5, 0.0, 0.5)
  private val Center = (1.0 / 3.0, 1.0 / 3.0, 1.0 / 3.0)
  private val Corners = Vector(A, AB, Center, A, Center, CA,
    B, BC, Center, B, Center, AB, C, CA, Center, C, Center, BC)

  /** Largest original barycentric weight; exact ties choose the smallest original id. */
  def nearestVertex(a: Int, b: Int, c: Int, wa: Double, wb: Double, wc: Double): Int =
    val maximum = math.max(wa, math.max(wb, wc))
    math.min(if wa == maximum then a else Int.MaxValue,
      math.min(if wb == maximum then b else Int.MaxValue, if wc == maximum then c else Int.MaxValue))

  private[view] def lower(mesh: SurfaceMeshPacket): SurfaceMeshPacket =
    val provenance = new SurfaceNearestPartition(mesh.indices)
    val count = Math.multiplyExact(mesh.indices.length, 6)
    val positions = new Array[Float](Math.multiplyExact(count, 3))
    val normals = new Array[Float](positions.length)
    val indices = new Array[Int](count)
    val source = new Array[Int](count)
    var corner = 0
    while corner < count do
      val base = (corner / 18) * 3
      val a = mesh.indices(base)
      val b = mesh.indices(base + 1)
      val c = mesh.indices(base + 2)
      val (wa, wb, wc) = provenance.weights(corner)
      indices(corner) = corner
      source(corner) = mesh.indices(base + (corner % 18) / 6)
      var axis = 0
      while axis < 3 do
        positions(corner * 3 + axis) =
          (wa * mesh.positions(a * 3 + axis) + wb * mesh.positions(b * 3 + axis) + wc * mesh.positions(c * 3 + axis)).toFloat
        normals(corner * 3 + axis) =
          (wa * mesh.normals(a * 3 + axis) + wb * mesh.normals(b * 3 + axis) + wc * mesh.normals(c * 3 + axis)).toFloat
        axis += 1
      corner += 1
    SurfaceMeshPacket(mesh.surface, SurfaceResourceKey(mesh.resourceKey.value + ":nearest-v1"),
      new FloatBufferView(positions), new FloatBufferView(normals), new IntBufferView(indices),
      Some(SurfaceResourceKey(mesh.geometryKey.value + ":nearest-v1")), Some(new IntBufferView(source)), Some(provenance))
