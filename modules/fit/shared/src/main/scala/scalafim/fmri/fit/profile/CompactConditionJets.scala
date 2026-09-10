package scalafim.fmri.fit.profile

import scalafim.fmri.hrf.family.JetLayout

/** Compact condition jets: from the retained factor `R` (`K x C m`, basis-major
  * columns `basisIx * C + condition`), a voxel's compact response `z` (`K`) and
  * residual energy `e`, plus the coefficient jet `c(theta)` (`comps x m`), form
  * `D = R (I_C kron c)` and the `(s, b, G)` jets that [[ProfileReduction]]
  * consumes. `s = e` is shape-free here: the whitening is shared and frozen.
  */
final class CompactConditionJets(val rHat: Array[Double], val rank: Int, val conditions: Int, val basisRank: Int, val dimension: Int):
  require(rHat.length == rank * conditions * basisRank, s"rHat has ${rHat.length} entries for $rank x ${conditions * basisRank}")
  require(dimension >= 1 && dimension <= 3)

  val components: Int = JetLayout.components(dimension)
  private val k = rank
  private val c = conditions
  private val m = basisRank
  private val cm = c * m
  private val design = new Array[Double](components * k * c)
  val s: Array[Double] = new Array[Double](components)
  val b: Array[Double] = new Array[Double](components * c)
  val g: Array[Double] = new Array[Double](components * c * c)
  private val block = new Array[Double](c * c)

  /** Form the `(s, b, G)` jets for `z`, `e` and the coefficient jet `coefficients`
    * (first `activeComponents` components; the rest are set to zero).
    */
  def assemble(z: Array[Double], e: Double, coefficients: Array[Double], activeComponents: Int): Unit =
    var comp = 0
    while comp < activeComponents do
      designFrom(coefficients, comp)
      comp += 1
    assembleFromDesign(z, e, activeComponents)

  /** Size of one full design jet, for precomputed node banks. */
  def designJetSize: Int = components * k * c

  /** Copy the current design jet (all components) into `target(offset ...)`. */
  def exportDesign(target: Array[Double], offset: Int): Unit =
    System.arraycopy(design, 0, target, offset, components * k * c)

  /** Assemble from a precomputed design jet stored at `source(offset ...)`. */
  def assembleLoaded(z: Array[Double], e: Double, source: Array[Double], offset: Int, activeComponents: Int): Unit =
    System.arraycopy(source, offset, design, 0, activeComponents * k * c)
    assembleFromDesign(z, e, activeComponents)

  private def assembleFromDesign(z: Array[Double], e: Double, activeComponents: Int): Unit =
    java.util.Arrays.fill(s, 0.0)
    java.util.Arrays.fill(b, 0.0)
    java.util.Arrays.fill(g, 0.0)
    s(JetLayout.Value) = e
    var comp = 0
    while comp < activeComponents do
      project(comp, z)
      comp += 1
    // G_0
    cross(0, 0, block)
    System.arraycopy(block, 0, g, 0, c * c)
    if activeComponents > 1 then
      val d = dimension
      var p = 0
      while p < d do
        val comp1 = JetLayout.first(p)
        if comp1 < activeComponents then
          cross(comp1, 0, block)
          addSymmetric(block, comp1)
        p += 1
      p = 0
      while p < d do
        var q = p
        while q < d do
          val comp2 = JetLayout.second(d, p, q)
          if comp2 < activeComponents then
            cross(comp2, 0, block)
            addSymmetric(block, comp2)
            cross(JetLayout.first(p), JetLayout.first(q), block)
            addSymmetric(block, comp2)
          q += 1
        p += 1

  /** The value design `D` (`K x C`, row-major) from the last [[assemble]]. */
  def valueDesign: Array[Double] = java.util.Arrays.copyOf(design, k * c)

  private def designFrom(coefficients: Array[Double], comp: Int): Unit =
    val base = comp * k * c
    val cb = comp * m
    var kk = 0
    while kk < k do
      val rb = kk * cm
      var cond = 0
      while cond < c do
        var acc = 0.0
        var j = 0
        while j < m do
          acc += rHat(rb + j * c + cond) * coefficients(cb + j)
          j += 1
        design(base + kk * c + cond) = acc
        cond += 1
      kk += 1

  private def project(comp: Int, z: Array[Double]): Unit =
    val base = comp * k * c
    var i = 0
    while i < c do
      var acc = 0.0
      var kk = 0
      while kk < k do
        acc += design(base + kk * c + i) * z(kk)
        kk += 1
      b(comp * c + i) = acc
      i += 1

  /** `out = D_a' D_b`. */
  private def cross(compA: Int, compB: Int, out: Array[Double]): Unit =
    val ba = compA * k * c
    val bb = compB * k * c
    var i = 0
    while i < c do
      var j = 0
      while j < c do
        var acc = 0.0
        var kk = 0
        while kk < k do
          acc += design(ba + kk * c + i) * design(bb + kk * c + j)
          kk += 1
        out(i * c + j) = acc
        j += 1
      i += 1

  /** `g(comp) += block + block'`. */
  private def addSymmetric(block: Array[Double], comp: Int): Unit =
    val base = comp * c * c
    var i = 0
    while i < c do
      var j = 0
      while j < c do
        g(base + i * c + j) += block(i * c + j) + block(j * c + i)
        j += 1
      i += 1
