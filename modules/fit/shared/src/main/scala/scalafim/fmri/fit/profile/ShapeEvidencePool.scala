package scalafim.fmri.fit.profile

/** Deterministic pooling of shape evidence across voxels within regions.
  *
  * Pools node energies (the per-voxel scan; shape evidence is quadratic in the
  * response, so a sign reversal leaves it unchanged) and the ten jet
  * components at each region's declared reference node. Accumulation is
  * per-region and order-sensitive only at floating-point round-off; use
  * [[merge]] in a fixed block order to make the frozen prior independent of
  * worker count. Fold boundaries and spatial dependence are the caller's:
  * a pooled curvature assumes independent voxels and is a composite
  * quantity, not a calibrated regional confidence.
  */
final class ShapeEvidencePool(val grid: NodeGrid, val regions: Int, val referenceNode: Vector[Int]):
  require(referenceNode.length == regions, "one reference node per region")
  require(referenceNode.forall(n => n >= 0 && n < grid.count), "reference nodes must lie on the grid")
  private val d = grid.dimension
  private val nodeSums = new Array[Double](regions * grid.count)
  private val nodeCounts = new Array[Int](regions * grid.count)
  private val weights = new Array[Double](regions)
  private val voxels = new Array[Int](regions)
  private val energy = new Array[Double](regions)
  private val gradient = new Array[Double](regions * d)
  private val hessian = new Array[Double](regions * d * d)

  /** Add one voxel's scanned node energies (`NaN` for unscanned nodes) with weight `w`. */
  def accumulateNodes(region: Int, nodeEnergies: Array[Double], w: Double): Unit =
    var g = 0
    while g < grid.count do
      val e = nodeEnergies(g)
      if !e.isNaN then
        nodeSums(region * grid.count + g) += w * e
        nodeCounts(region * grid.count + g) += 1
      g += 1
    weights(region) += w
    voxels(region) += 1

  /** Add one voxel's jet evaluated at the region's reference node. */
  def accumulateReferenceJet(region: Int, jet: ProfileJetBuffer, w: Double): Unit =
    energy(region) += w * jet.energy
    var i = 0
    while i < d do
      gradient(region * d + i) += w * jet.gradient(i)
      i += 1
    i = 0
    while i < d * d do
      hessian(region * d * d + i) += w * jet.hessian(i)
      i += 1

  /** Merge another pool with the same geometry (fixed order gives identical results). */
  def merge(other: ShapeEvidencePool): Unit =
    require(other.grid == grid && other.regions == regions && other.referenceNode == referenceNode, "pools must share geometry")
    var i = 0
    while i < nodeSums.length do
      nodeSums(i) += other.nodeSums(i)
      nodeCounts(i) += other.nodeCounts(i)
      i += 1
    i = 0
    while i < regions do
      weights(i) += other.weights(i)
      voxels(i) += other.voxels(i)
      energy(i) += other.energy(i)
      i += 1
    i = 0
    while i < gradient.length do
      gradient(i) += other.gradient(i)
      i += 1
    i = 0
    while i < hessian.length do
      hessian(i) += other.hessian(i)
      i += 1

  def voxelCount(region: Int): Int = voxels(region)
  def pooledNodeEnergy(region: Int, node: Int): Double = nodeSums(region * grid.count + node)
  def pooledReferenceHessian(region: Int): Vector[Double] = hessian.slice(region * d * d, (region + 1) * d * d).toVector

  /** The node with the lowest pooled energy among nodes every voxel scored. */
  def pooledBestNode(region: Int): Option[Int] =
    var best = -1
    var bestE = Double.PositiveInfinity
    var g = 0
    while g < grid.count do
      val idx = region * grid.count + g
      if nodeCounts(idx) == voxels(region) && voxels(region) > 0 && nodeSums(idx) < bestE then
        bestE = nodeSums(idx)
        best = g
      g += 1
    if best < 0 then None else Some(best)

  /** A frozen regional prior in energy units: mean at the pooled best node,
    * precision from the pooled reference curvature scaled by `shrinkage`
    * (0 disables), floored so the spread never collapses below `minSpread`
    * per coordinate. Diagonal by construction.
    */
  def regionalPrior(region: Int, shrinkage: Double, minSpread: Vector[Double]): Option[ShapePrior] =
    require(minSpread.length == d && minSpread.forall(_ > 0.0), "one positive spread floor per coordinate")
    pooledBestNode(region).map { node =>
      val mean = new Array[Double](d)
      grid.coordinatesInto(node, mean)
      val precision = new Array[Double](d * d)
      var i = 0
      while i < d do
        val h = hessian(region * d * d + i * d + i) / math.max(1, voxels(region))
        val cap = 1.0 / (minSpread(i) * minSpread(i))
        precision(i * d + i) = math.min(cap, math.max(0.0, shrinkage * 0.5 * h))
        i += 1
      ShapePrior(mean.toVector, precision.toVector)
    }
