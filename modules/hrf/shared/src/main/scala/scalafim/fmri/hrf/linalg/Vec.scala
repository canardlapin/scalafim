package scalafim.fmri.hrf.linalg

import scala.annotation.targetName

final case class Vec private (data: Array[Double]):
  def length: Int = data.length
  def apply(i: Int): Double = data(i)
  def toArray: Array[Double] = data.clone

  def map(f: Double => Double): Vec =
    val out = new Array[Double](length)
    var i = 0
    while i < length do
      out(i) = f(data(i))
      i += 1
    Vec.unsafe(out)

  def zipMap(other: Vec)(f: (Double, Double) => Double): Vec =
    require(length == other.length, s"Vec length mismatch: $length vs ${other.length}")
    val out = new Array[Double](length)
    var i = 0
    while i < length do
      out(i) = f(data(i), other.data(i))
      i += 1
    Vec.unsafe(out)

  @targetName("plus")
  def +(other: Vec): Vec = zipMap(other)(_ + _)

  @targetName("minus")
  def -(other: Vec): Vec = zipMap(other)(_ - _)

  @targetName("scale")
  def *(k: Double): Vec = map(_ * k)

  def dot(other: Vec): Double =
    require(length == other.length, s"Vec length mismatch: $length vs ${other.length}")
    var acc = 0.0
    var i = 0
    while i < length do
      acc += data(i) * other.data(i)
      i += 1
    acc

  def maxAbs: Double =
    var m = 0.0
    var i = 0
    while i < length do
      val a = math.abs(data(i))
      if a > m then m = a
      i += 1
    m

object Vec:
  def apply(xs: Seq[Double]): Vec = unsafe(xs.toArray)
  def zeros(n: Int): Vec = unsafe(Array.fill(n)(0.0))

  def unsafe(a: Array[Double]): Vec = new Vec(a)
