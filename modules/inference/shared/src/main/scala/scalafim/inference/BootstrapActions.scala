package scalafim.inference

final case class BootstrapSample private[inference] (
    rows: Vector[RowIx],
    sampledClusters: Vector[Int]
)

sealed trait BootstrapAction:
  def rowCount: RowCount
  def draw(seed: RootSeed, replicate: ReplicateId): Either[InferenceError, BootstrapSample]

object BootstrapAction:
  final case class Rows private[inference] (rowCount: RowCount) extends BootstrapAction:
    override def draw(
        seed: RootSeed,
        replicate: ReplicateId
    ): Either[InferenceError, BootstrapSample] =
      val random = RandomSource.forReplicate(seed, replicate)
      val rows = Vector.newBuilder[RowIx]
      var cursor = random
      var i = 0
      while i < rowCount.value do
        val (uniform, next) = cursor.nextDouble
        rows += RowIx.unsafe(Math.min(rowCount.value - 1, (uniform * rowCount.value).toInt))
        cursor = next
        i += 1
      Right(BootstrapSample(rows.result(), Vector.empty))

  final case class Clusters private[inference] (
      partition: ClusterPartition
  ) extends BootstrapAction:
    override def rowCount: RowCount = partition.rowCount

    override def draw(
        seed: RootSeed,
        replicate: ReplicateId
    ): Either[InferenceError, BootstrapSample] =
      val random = RandomSource.forReplicate(seed, replicate)
      val clusters = partition.clusters
      val sampled = Vector.newBuilder[Int]
      val rows = Vector.newBuilder[RowIx]
      var cursor = random
      var i = 0
      while i < clusters.length do
        val (uniform, next) = cursor.nextDouble
        val index = Math.min(clusters.length - 1, (uniform * clusters.length).toInt)
        sampled += index
        rows ++= clusters(index)
        cursor = next
        i += 1
      Right(BootstrapSample(rows.result(), sampled.result()))

  def rows(count: RowCount): BootstrapAction =
    Rows(count)

  def clusters(partition: ClusterPartition): BootstrapAction =
    Clusters(partition)
