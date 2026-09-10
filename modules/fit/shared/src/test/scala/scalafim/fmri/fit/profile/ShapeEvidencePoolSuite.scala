package scalafim.fmri.fit.profile

import scalafim.fmri.hrf.family.ShapeChart

class ShapeEvidencePoolSuite extends munit.FunSuite:

  private val grid = NodeGrid(ShapeChart(("a", 0.0, 1.0), ("b", 0.0, 1.0)), Vector(5, 5))

  private def energies(shift: Double): Array[Double] =
    Array.tabulate(grid.count) { node =>
      val x = new Array[Double](2)
      grid.coordinatesInto(node, x)
      (x(0) - 0.5 - shift) * (x(0) - 0.5 - shift) + (x(1) - 0.5) * (x(1) - 0.5)
    }

  test("pooled node energies sum, ignore unscanned nodes and pick a fully scanned best node"):
    val pool = new ShapeEvidencePool(grid, 1, Vector(12))
    pool.accumulateNodes(0, energies(0.1), 1.0)
    val partial = energies(-0.1)
    partial(3) = Double.NaN
    pool.accumulateNodes(0, partial, 1.0)
    assertEquals(pool.voxelCount(0), 2)
    assertEqualsDouble(pool.pooledNodeEnergy(0, 12), energies(0.1)(12) + energies(-0.1)(12), 1e-15)
    assertEquals(pool.pooledBestNode(0), Some(12))

  test("merging in a fixed order reproduces a single accumulation exactly"):
    val a = new ShapeEvidencePool(grid, 2, Vector(12, 6))
    val b = new ShapeEvidencePool(grid, 2, Vector(12, 6))
    val whole = new ShapeEvidencePool(grid, 2, Vector(12, 6))
    val jet = new ProfileJetBuffer(2, 1)
    jet.energy = 3.0
    jet.gradient(0) = 0.5
    jet.hessian(0) = 4.0
    jet.hessian(3) = 9.0
    a.accumulateNodes(0, energies(0.0), 1.0); a.accumulateReferenceJet(0, jet, 1.0)
    b.accumulateNodes(1, energies(0.2), 2.0); b.accumulateReferenceJet(1, jet, 2.0)
    whole.accumulateNodes(0, energies(0.0), 1.0); whole.accumulateReferenceJet(0, jet, 1.0)
    whole.accumulateNodes(1, energies(0.2), 2.0); whole.accumulateReferenceJet(1, jet, 2.0)
    a.merge(b)
    assertEquals(a.pooledReferenceHessian(1), whole.pooledReferenceHessian(1))
    assertEquals((0 until grid.count).map(g => a.pooledNodeEnergy(1, g)), (0 until grid.count).map(g => whole.pooledNodeEnergy(1, g)))

  test("the regional prior sits at the pooled best node with a floored spread"):
    val pool = new ShapeEvidencePool(grid, 1, Vector(12))
    val jet = new ProfileJetBuffer(2, 1)
    jet.hessian(0) = 200.0
    jet.hessian(3) = 0.02
    var i = 0
    while i < 4 do
      pool.accumulateNodes(0, energies(0.0), 1.0)
      pool.accumulateReferenceJet(0, jet, 1.0)
      i += 1
    val prior = pool.regionalPrior(0, shrinkage = 1.0, minSpread = Vector(0.2, 0.2)).getOrElse(fail("no prior"))
    assertEquals(prior.mean, Vector(0.5, 0.5))
    assertEqualsDouble(prior.precision(0), 1.0 / (0.2 * 0.2), 1e-12, "capped by the spread floor")
    assertEqualsDouble(prior.precision(3), 0.01, 1e-12, "half the mean curvature")
    assertEquals(pool.regionalPrior(0, 0.0, Vector(0.2, 0.2)).map(_.precision.sum), Some(0.0))
    assertEquals(new ShapeEvidencePool(grid, 1, Vector(0)).regionalPrior(0, 1.0, Vector(0.1, 0.1)), None)
