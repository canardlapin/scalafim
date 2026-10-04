package scalafim.surface.reference

import scalafim.image.WorldPoint

import java.nio.{ByteBuffer, ByteOrder}
import java.util.zip.GZIPInputStream

/** The all-vertex SimpleITK inverse oracle (`tools/transform/generate_fslr_inverse_oracle.py`): for every fsLR
  * 32k midthickness vertex, its MNI152NLin2009cAsym position solved against the original TemplateFlow HDF5
  * composite, independently of ScalaFIM and reframe4s. Every file is checked against the digests in the
  * manifest, and the manifest against the locked transform and surfaces.
  */
object RealInverseOracle:
  final case class Solution(world: WorldPoint, residualMm: Double, iterations: Int, inSupport: Boolean)

  private def resource(name: String): Array[Byte] =
    val stream = getClass.getResourceAsStream(s"/pointmap-real/$name")
    require(stream != null, s"missing oracle resource $name")
    try stream.readAllBytes() finally stream.close()

  lazy val manifest: ujson.Value =
    val json = ujson.read(resource("fslr-inverse-oracle-all.json"))
    require(json("schema").str == "scalafim.fslr-inverse-oracle-all/1", "unknown oracle schema")
    require(json("transform")("sha256").str == RealAssets.transformSha256, "oracle solved for another transform")
    json

  /** Solutions by hemisphere label, in vertex order. */
  lazy val solutions: Map[String, Vector[Solution]] =
    Vector("L", "R").map: h =>
      val entry = manifest("hemispheres")(h)
      require(entry("surface")("sha256").str == RealAssets.midthicknessSha256(h), s"oracle $h solved for another surface")
      val compressed = resource(entry("file").str)
      require(AssetSha256.of(compressed).value == entry("sha256").str, s"oracle file ${entry("file").str} differs from its manifest")
      val stream = new GZIPInputStream(new java.io.ByteArrayInputStream(compressed))
      val raw = try stream.readAllBytes() finally stream.close()
      val n = entry("vertices").num.toInt
      require(raw.length == n * 6 * 8, s"oracle $h has ${raw.length} bytes; expected ${n * 48}")
      val buffer = ByteBuffer.wrap(raw).order(ByteOrder.LITTLE_ENDIAN).asDoubleBuffer()
      h -> Vector.tabulate(n): i =>
        def at(c: Int) = buffer.get(6 * i + c)
        Solution(WorldPoint(at(0), at(1), at(2)), at(3), at(4).toInt, at(5) == 1.0)
    .toMap
