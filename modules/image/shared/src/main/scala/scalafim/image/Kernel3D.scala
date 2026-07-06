package scalafim.image

final case class Kernel3D private (
  kerndim: Vector[Int],
  vdim: Vector[Double],
  dx: Array[Int],
  dy: Array[Int],
  dz: Array[Int],
  w: Array[Double]
):
  require(kerndim.length == 3 && kerndim.forall(_ >= 1), "kerndim must be length-3 with entries >= 1")
  require(vdim.length == 3 && vdim.forall(_ > 0.0), "vdim must be length-3 positive")
  require(dx.length == dy.length && dx.length == dz.length && dx.length == w.length, "kernel arrays must have equal length")
  require(dx.length == kerndim.product, "kernel arrays length must equal product(kerndim)")

  def size: Int = w.length

object Kernel3D:
  def apply(
    kerndim: Vector[Int],
    vdim: Vector[Double] = Vector(1.0, 1.0, 1.0)
  )(
    f: Double => Double
  ): Kernel3D =
    require(kerndim.length == 3 && kerndim.forall(_ >= 1), "kerndim must be length-3 with entries >= 1")
    require(vdim.length == 3 && vdim.forall(_ > 0.0), "vdim must be length-3 positive")

    val cx = kerndim(0) / 2
    val cy = kerndim(1) / 2
    val cz = kerndim(2) / 2

    val total = kerndim.product
    val dx = Array.ofDim[Int](total)
    val dy = Array.ofDim[Int](total)
    val dz = Array.ofDim[Int](total)
    val w = Array.ofDim[Double](total)

    var q = 0
    var z = 0
    while z < kerndim(2) do
      val oz = z - cz
      var y = 0
      while y < kerndim(1) do
        val oy = y - cy
        var x = 0
        while x < kerndim(0) do
          val ox = x - cx
          dx(q) = ox
          dy(q) = oy
          dz(q) = oz

          val rx = ox.toDouble * vdim(0)
          val ry = oy.toDouble * vdim(1)
          val rz = oz.toDouble * vdim(2)
          val dist = math.sqrt(rx * rx + ry * ry + rz * rz)
          w(q) = f(dist)

          q += 1
          x += 1
        y += 1
      z += 1

    Kernel3D(kerndim, vdim, dx, dy, dz, w)
