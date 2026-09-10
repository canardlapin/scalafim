package scalafim.fmri.fit.profile

import scalafim.fmri.design.hrf.HrfKernelBasis
import scalafim.fmri.hrf.family.{JetLayout, ShapePoint}

/** Gram-form condition jets from the sufficient statistics an ordinary
  * basis-expanded fit already retains: the partialled task Gram `G`
  * (`Cm x Cm`, condition-major columns `condition * m + basis`), a voxel's
  * cross-products `x = X'y` (`Cm`) and its partialled response energy `s`.
  *
  * With `c = c(theta)`: `b_a = (I kron c_a)' x` and
  * `G^{ab} = (I kron c_a)' G (I kron c_b)`, so the `(s, b, G)` jets of the
  * profile reduction need `Y_b = G (I kron c_b)` for the value and the
  * first-order components only. `s` is shape-free; the response never
  * appears at time resolution.
  */
final class GramConditionJets(val gram: Array[Double], val conditions: Int, val basisRank: Int, val dimension: Int):
  private val c = conditions
  private val m = basisRank
  private val cm = c * m
  private val d = dimension
  require(gram.length == cm * cm, s"gram has ${gram.length} entries for $cm x $cm")
  val components: Int = JetLayout.components(d)
  val s: Array[Double] = new Array[Double](components)
  val b: Array[Double] = new Array[Double](components * c)
  val g: Array[Double] = new Array[Double](components * c * c)
  private val y = new Array[Double]((1 + d) * cm * c)
  private val block = new Array[Double](c * c)

  /** `out(i) = sum_j coefficients(offset + j) x(i * m + j)`. */
  private def contractResponse(x: Array[Double], coefficients: Array[Double], offset: Int, out: Array[Double], outOffset: Int): Unit =
    var i = 0
    while i < c do
      var acc = 0.0
      var j = 0
      while j < m do
        acc += coefficients(offset + j) * x(i * m + j)
        j += 1
      out(outOffset + i) = acc
      i += 1

  /** `Y = G (I kron c_b)` into `y(slot ...)`, `Cm x C` row-major. */
  private def gramTimes(coefficients: Array[Double], offset: Int, slot: Int): Unit =
    val base = slot * cm * c
    var row = 0
    while row < cm do
      var k = 0
      while k < c do
        var acc = 0.0
        var l = 0
        while l < m do
          acc += gram(row * cm + k * m + l) * coefficients(offset + l)
          l += 1
        y(base + row * c + k) = acc
        k += 1
      row += 1

  /** `block = (I kron c_a)' Y_slot`. */
  private def contractGram(coefficients: Array[Double], offset: Int, slot: Int): Unit =
    val base = slot * cm * c
    var i = 0
    while i < c do
      var k = 0
      while k < c do
        var acc = 0.0
        var j = 0
        while j < m do
          acc += coefficients(offset + j) * y(base + (i * m + j) * c + k)
          j += 1
        block(i * c + k) = acc
        k += 1
      i += 1

  private def addSymmetric(comp: Int): Unit =
    val base = comp * c * c
    var i = 0
    while i < c do
      var k = 0
      while k < c do
        g(base + i * c + k) += block(i * c + k) + block(k * c + i)
        k += 1
      i += 1

  /** Assemble for the coefficient jet `coefficients` (component-major, `m` per component). */
  def assemble(x: Array[Double], energy: Double, coefficients: Array[Double], activeComponents: Int): Unit =
    java.util.Arrays.fill(s, 0.0)
    java.util.Arrays.fill(b, 0.0)
    java.util.Arrays.fill(g, 0.0)
    s(JetLayout.Value) = energy
    var comp = 0
    while comp < activeComponents do
      contractResponse(x, coefficients, comp * m, b, comp * c)
      comp += 1
    gramTimes(coefficients, 0, 0)
    contractGram(coefficients, 0, 0)
    System.arraycopy(block, 0, g, 0, c * c)
    if activeComponents > 1 then
      var p = 0
      while p < d do
        val comp1 = JetLayout.first(p)
        if comp1 < activeComponents then
          gramTimes(coefficients, comp1 * m, 1 + p)
          contractGram(coefficients, comp1 * m, 0)
          addSymmetric(comp1)
        p += 1
      p = 0
      while p < d do
        var q = p
        while q < d do
          val comp2 = JetLayout.second(d, p, q)
          if comp2 < activeComponents then
            contractGram(coefficients, comp2 * m, 0)
            addSymmetric(comp2)
            contractGram(coefficients, JetLayout.first(p) * m, 1 + q)
            addSymmetric(comp2)
          q += 1
        p += 1

  /** Copy the assembled Gram jet blocks (`components x C x C`) to a bank. */
  def exportGram(target: Array[Double], offset: Int): Unit =
    System.arraycopy(g, 0, target, offset, components * c * c)

/** The Gram-form condition backend as a [[ShapeObjective]]. The node bank
  * holds, per node, the coefficient vector, the Cholesky factor of the node
  * Gram and the assembled Gram jet blocks, so node scores cost `C m` products
  * and node jets cost `comps C m`.
  */
final class GramConditionObjective(val jets: GramConditionJets, val basis: HrfKernelBasis, val grid: NodeGrid) extends ShapeObjective:
  private val c = jets.conditions
  private val m = jets.basisRank
  private val d = jets.dimension
  private val comps = jets.components
  private val reduction = new ProfileReduction(d, c)
  private val kernelScratch = new Array[Double](comps * basis.fineCount)
  private val coeff = new Array[Double](comps * m)
  private val coords = new Array[Double](d)
  private val nodeCoefficients = new Array[Double](grid.count * comps * m)
  private val nodeFactor = new Array[Double](grid.count * c * c)
  private val nodeGram = new Array[Double](grid.count * comps * c * c)
  private val bv = new Array[Double](c)
  private var x: Array[Double] = new Array[Double](c * m)
  private var energy: Double = 0.0

  locally {
    var node = 0
    while node < grid.count do
      grid.coordinatesInto(node, coords)
      basis.coefficientJetInto(ShapePoint.unsafe(coords.toVector), kernelScratch, coeff, comps)
      System.arraycopy(coeff, 0, nodeCoefficients, node * comps * m, comps * m)
      jets.assemble(new Array[Double](c * m), 0.0, coeff, comps)
      jets.exportGram(nodeGram, node * comps * c * c)
      System.arraycopy(jets.g, 0, nodeFactor, node * c * c, c * c)
      val ok = SmallCholesky.factorInPlace(c, java.util.Arrays.copyOfRange(nodeFactor, node * c * c, (node + 1) * c * c))
      require(ok, s"node $node has a singular Gram")
      val factor = java.util.Arrays.copyOfRange(nodeFactor, node * c * c, (node + 1) * c * c)
      SmallCholesky.factorInPlace(c, factor)
      System.arraycopy(factor, 0, nodeFactor, node * c * c, c * c)
      node += 1
  }

  def amplitudeCount: Int = c

  def pointAt(crossProducts: Array[Double], responseEnergy: Double): Unit =
    x = crossProducts
    energy = responseEnergy

  def scoreNode(node: Int): Double =
    var i = 0
    while i < c do
      var acc = 0.0
      var j = 0
      while j < m do
        acc += nodeCoefficients(node * comps * m + j) * x(i * m + j)
        j += 1
      bv(i) = acc
      i += 1
    val w = bv.clone()
    SmallCholesky.solveInPlace(c, java.util.Arrays.copyOfRange(nodeFactor, node * c * c, (node + 1) * c * c), w)
    var fit = 0.0
    i = 0
    while i < c do
      fit += bv(i) * w(i)
      i += 1
    energy - fit

  def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
    // b from the banked coefficients, g from the banked blocks, s = energy
    java.util.Arrays.fill(jets.s, 0.0)
    jets.s(JetLayout.Value) = energy
    var comp = 0
    while comp < comps do
      var i = 0
      while i < c do
        var acc = 0.0
        var j = 0
        while j < m do
          acc += nodeCoefficients(node * comps * m + comp * m + j) * x(i * m + j)
          j += 1
        jets.b(comp * c + i) = acc
        i += 1
      comp += 1
    System.arraycopy(nodeGram, node * comps * c * c, jets.g, 0, comps * c * c)
    reduction.reduce(jets.s, jets.b, jets.g, out)

  def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
    basis.coefficientJetInto(ShapePoint.unsafe(coordinates.toVector), kernelScratch, coeff, comps)
    jets.assemble(x, energy, coeff, comps)
    reduction.reduce(jets.s, jets.b, jets.g, out)

  def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
    basis.coefficientJetInto(ShapePoint.unsafe(coordinates.toVector), kernelScratch, coeff, 1)
    jets.assemble(x, energy, coeff, 1)
    reduction.reduce(jets.s, jets.b, jets.g, out)
    out.energy
