package scalafim.image

import narr.NArray
import scala.reflect.ClassTag

final case class NDArray[A](data: NArray[A], shape: Vector[Int]):
  val size: Int = shape.product
  require(data.length == size, s"data length ${data.length} != product(shape) $size")

  def ndim: Int = shape.length

  private def linearIndex(idxs: Seq[Int]): Int =
    require(idxs.length == ndim, s"expected $ndim indices")
    var stride = 1
    var lin = 0
    var d = 0
    while d < ndim do
      val i = idxs(d)
      val dim = shape(d)
      require(i >= 0 && i < dim, s"index $i out of bounds for dim $d (0..${dim - 1})")
      lin += i * stride
      stride *= dim
      d += 1
    lin

  def apply(idxs: Int*): A =
    data(linearIndex(idxs))

  def updated(idxs: Seq[Int], value: A)(using ClassTag[A]): NDArray[A] =
    val out = narr.NArray.copy(data)
    out(linearIndex(idxs)) = value
    NDArray(out, shape)

  def map[B](f: A => B)(using ClassTag[B]): NDArray[B] =
    val out = NArray.ofSize[B](data.length)
    var i = 0
    while i < data.length do
      out(i) = f(data(i))
      i += 1
    NDArray(out, shape)

  def zipMap[B, C](that: NDArray[B])(f: (A, B) => C)(using ClassTag[C]): NDArray[C] =
    require(this.shape == that.shape, "shape mismatch")
    val out = NArray.ofSize[C](data.length)
    var i = 0
    while i < data.length do
      out(i) = f(this.data(i), that.data(i))
      i += 1
    NDArray(out, shape)

  def reshape(newShape: Vector[Int]): NDArray[A] =
    require(newShape.product == size, "reshape must preserve size")
    NDArray(data, newShape)

object NDArray:
  def zeros(shape: Vector[Int]): NDArray[Double] =
    val size = shape.product
    NDArray(NArrayUtil.fillConst[Double](size, 0.0), shape)
