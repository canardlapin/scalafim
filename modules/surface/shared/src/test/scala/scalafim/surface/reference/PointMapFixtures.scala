package scalafim.surface.reference

import image4s.geometry.{Affine, D3}
import scalafim.image.WorldPoint

import java.nio.{ByteBuffer, ByteOrder}

/** Builders for analytic point maps and canonical `templateflow4s.point-map/1` stage files. */
object PointMapFixtures:
  def affine(rowMajor: Double*): Affine[D3] = Affine.fromRowMajor[D3](rowMajor.toVector).toOption.get

  def translation(x: Double, y: Double, z: Double): Affine[D3] =
    affine(1, 0, 0, x, 0, 1, 0, y, 0, 0, 1, z, 0, 0, 0, 1)

  /** Voxel centre of `(i, j, k)` under `voxelToRas`. */
  def centre(voxelToRas: Affine[D3], i: Int, j: Int, k: Int): Vector[Double] =
    voxelToRas(Vector(i.toDouble, j.toDouble, k.toDouble)).toOption.get

  /** Component-planar field sampled from `d` at every voxel centre. */
  def sampled(dims: Vector[Int], voxelToRas: Affine[D3])(d: Vector[Double] => Vector[Double]): Array[Double] =
    val (nx, ny, nz) = (dims(0), dims(1), dims(2))
    val out = new Array[Double](3 * nx * ny * nz)
    for k <- 0 until nz; j <- 0 until ny; i <- 0 until nx do
      val v = d(centre(voxelToRas, i, j, k))
      for c <- 0 until 3 do out(i + nx * (j + ny * (k + nz * c))) = v(c)
    out

  def field(dims: Vector[Int], voxelToRas: Affine[D3])(d: Vector[Double] => Vector[Double]): DisplacementField =
    DisplacementField.make(dims, voxelToRas, sampled(dims, voxelToRas)(d)).toOption.get

  def point(x: Double, y: Double, z: Double): WorldPoint = WorldPoint(x, y, z)

  /** A canonical stage file: NIfTI-1, little-endian, vox_offset 352, float64 VECTOR. */
  def niftiBytes(dims: Vector[Int], voxelToRas: Affine[D3], values: Array[Double]): Array[Byte] =
    val buffer = ByteBuffer.allocate(352 + 8 * values.length).order(ByteOrder.LITTLE_ENDIAN)
    buffer.putInt(0, 348)
    Vector(5, dims(0), dims(1), dims(2), 1, 3, 1, 1).zipWithIndex.foreach((v, i) => buffer.putShort(40 + 2 * i, v.toShort))
    buffer.putShort(68, 1007.toShort)
    buffer.putShort(70, 64.toShort)
    buffer.putShort(72, 64.toShort)
    buffer.putFloat(108, 352.0f)
    buffer.putShort(254, 5.toShort)
    voxelToRas.rowMajor.take(12).zipWithIndex.foreach((v, i) => buffer.putFloat(280 + 4 * i, v.toFloat))
    buffer.put(344, 'n'.toByte)
    buffer.put(345, '+'.toByte)
    buffer.put(346, '1'.toByte)
    values.indices.foreach(i => buffer.putDouble(352 + 8 * i, values(i)))
    buffer.array()

  /** A manifest for one displacement stage then one affine stage, with its stage bytes. */
  def manifest(
    dims: Vector[Int],
    voxelToRas: Affine[D3],
    values: Array[Double],
    after: Affine[D3],
    input: String = "SynthIn",
    output: String = "SynthOut",
    revision: Option[String] = Some("templateflow@synthetic")
  ): (PointMapManifest, Array[Byte]) =
    val bytes = niftiBytes(dims, voxelToRas, values)
    val source = PointMapSource(s"tpl-$input/tpl-${input}_from-${output}_mode-image_xfm.h5", "a" * 64, 1L, revision)
    val fields = ManifestFields(PointMapManifest.Schema, source, input, output, "synthetic fixture", None, Vector(
      ManifestStage.DisplacementEntry("stage-0-displacement.nii", AssetSha256.of(bytes).value, bytes.length.toLong, dims,
        voxelToRas.rowMajor),
      ManifestStage.AffineEntry(after.rowMajor)))
    val m = PointMapManifest(AssetSha256.of(fields.toString.getBytes("UTF-8")), fields)
    (m, bytes)
