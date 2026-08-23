package scalafim.image

import scala.reflect.ClassTag

final case class NeuroVecSeq[A](vecs: Vector[NeuroVec[A]]):
  require(vecs.nonEmpty, "NeuroVecSeq cannot be empty")

  private val baseSpace = vecs.head.space.spatialSpace
  private val spatialDims = baseSpace.spatialDims
  private val spatialNels = spatialDims.product

  vecs.foreach { v =>
    GridCompatibility.requireSpatial(baseSpace, v.space)
  }

  val lens: Vector[Int] = vecs.map(_.nVolumes)
  val length: Int = lens.sum
  val space: NeuroSpace = baseSpace.addDim(length, Some(Axis.Time))

  private val offsets: Vector[Int] =
    lens.scanLeft(0)(_ + _).dropRight(1)

  private def locate(t: Int): (NeuroVec[A], Int) =
    require(t >= 0 && t < length, "time index out of bounds")
    var b = 0
    while b < lens.length do
      val start = offsets(b)
      val end = start + lens(b)
      if t >= start && t < end then
        return (vecs(b), t - start)
      b += 1
    throw new IllegalStateException("unreachable")

  def apply(t: Int)(using ClassTag[A]): NeuroVol[A] =
    val (v, localT) = locate(t)
    v.volume(localT)

  def apply(ts: Seq[Int])(using
      ClassTag[A],
      MigrationValueSemantics[A]
  ): NeuroVecSeq[A] =
    subVector(ts)

  private[scalafim] def valueAtCanonicalOrdinal(i: Int): A =
    require(
      i >= 0 && i < spatialNels * length,
      "canonical ordinal out of bounds"
    )
    val linSpatial = i / length
    val t = i % length
    val (v, localT) = locate(t)
    v.valueAtVoxelOrdinal(linSpatial, localT)

  private[scalafim] def valuesAtCanonicalOrdinals(
      indices: Array[Int]
  )(using ClassTag[A]): Array[A] =
    val out = Array.ofDim[A](indices.length)
    var p = 0
    while p < indices.length do
      out(p) = valueAtCanonicalOrdinal(indices(p))
      p += 1
    out

  def subVector(ts: Seq[Int])(using
      ClassTag[A],
      MigrationValueSemantics[A]
  ): NeuroVecSeq[A] =
    require(ts.nonEmpty, "ts must be non-empty")
    require(ts.forall(t => t >= 0 && t < length), "time index out of bounds")
    val perBucket = Array.fill(lens.length)(Vector.empty[Int])
    ts.foreach { t =>
      var b = 0
      while b < lens.length do
        val start = offsets(b)
        val end = start + lens(b)
        if t >= start && t < end then
          perBucket(b) = perBucket(b) :+ (t - start)
          b = lens.length
        else b += 1
    }

    val subVecs =
      perBucket.zipWithIndex.collect {
        case (localTs, b) if localTs.nonEmpty =>
          vecs(b).subVector(localTs)
      }.toVector

    NeuroVecSeq(subVecs)

  def toNeuroVec(using
      ClassTag[A],
      MigrationValueSemantics[A]
  ): NeuroVec[A] =
    vecs.reduce(_.concat(_))
