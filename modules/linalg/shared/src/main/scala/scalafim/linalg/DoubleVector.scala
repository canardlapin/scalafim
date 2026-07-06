package scalafim.linalg

final class DoubleVector private (private[scalafim] val dataArray: Array[Double]):
  def length: Int = dataArray.length

  def apply(index: Int): Double = dataArray(index)

  def copyData: Array[Double] = dataArray.clone

  def toVector: Vector[Double] =
    val out = Vector.newBuilder[Double]
    out.sizeHint(dataArray.length)
    var i = 0
    while i < dataArray.length do
      out += dataArray(i)
      i += 1
    out.result()

object DoubleVector:
  def zeros(length: Int): DoubleVector =
    require(length >= 0, "length must be non-negative")
    unsafe(new Array[Double](length))

  def fromSeq(values: Seq[Double]): DoubleVector =
    val out = new Array[Double](values.length)
    var i = 0
    values.foreach { value =>
      out(i) = value
      i += 1
    }
    unsafe(out)

  def unsafe(data: Array[Double]): DoubleVector =
    new DoubleVector(data)
