package scalafim.image

import narr.NArray
import scala.reflect.ClassTag

enum NDArrayError:
  case InvalidDimension(position: Int, value: Int)
  case SizeOverflow(shape: Vector[Int])
  case DataLengthMismatch(expected: Int, actual: Int)

  def message: String =
    this match
      case InvalidDimension(position, value) =>
        s"array dimension $position must be non-negative; got $value"
      case SizeOverflow(shape) =>
        s"array shape $shape exceeds the supported array size"
      case DataLengthMismatch(expected, actual) =>
        s"data length $actual does not match shape size $expected"

/** Dense n-dimensional values with package-owned primitive storage.
  *
  * Public callers construct instances through [[NDArray.make]] or
  * [[NDArray.copyOf]], both of which defensively copy the supplied buffer.
  * Code inside `scalafim` may use the package-private constructor when it owns
  * a freshly allocated kernel result.
  */
final class NDArray[A] private[scalafim] (
    private[scalafim] val data: NArray[A],
    val shape: Vector[Int]
):
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

  def toNArray(using ClassTag[A]): NArray[A] =
    NArray.copy(data)

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

  override def equals(other: Any): Boolean =
    other match
      case that: NDArray[?] =>
        if this eq that then true
        else if shape != that.shape || data.length != that.data.length then false
        else
          var index = 0
          while index < data.length do
            if data(index) != that.data(index) then return false
            index += 1
          true
      case _ => false

  override def hashCode(): Int =
    var hash = shape.hashCode()
    var index = 0
    while index < data.length do
      hash = 31 * hash + data(index).##
      index += 1
    hash

  override def toString: String =
    s"NDArray(shape=$shape)"

object NDArray:
  private[scalafim] def apply[A](
      data: NArray[A],
      shape: Vector[Int]
  ): NDArray[A] =
    new NDArray(data, shape)

  def make[A](
      data: NArray[A],
      shape: Vector[Int]
  )(using ClassTag[A]): Either[NDArrayError, NDArray[A]] =
    checkedSize(shape).flatMap { expected =>
      if data.length == expected then
        Right(new NDArray(NArray.copy(data), shape))
      else Left(NDArrayError.DataLengthMismatch(expected, data.length))
    }

  def copyOf[A](
      data: NArray[A],
      shape: Vector[Int]
  )(using ClassTag[A]): NDArray[A] =
    make(data, shape)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def zeros(shape: Vector[Int]): NDArray[Double] =
    val size = shape.product
    NDArray(NArrayUtil.fillConst[Double](size, 0.0), shape)

  private def checkedSize(shape: Vector[Int]): Either[NDArrayError, Int] =
    var size = 1L
    var position = 0
    while position < shape.length do
      val dimension = shape(position)
      if dimension < 0 then
        return Left(NDArrayError.InvalidDimension(position, dimension))
      size *= dimension.toLong
      if size > Int.MaxValue.toLong then
        return Left(NDArrayError.SizeOverflow(shape))
      position += 1
    Right(size.toInt)
