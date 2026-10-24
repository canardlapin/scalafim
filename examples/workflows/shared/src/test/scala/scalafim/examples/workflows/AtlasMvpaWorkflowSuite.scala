package scalafim.examples.workflows

class AtlasMvpaWorkflowSuite extends munit.FunSuite:
  test("atlas workflow exposes typed regional measurements from atlas realization") {
    val rows = AtlasMvpaWorkflows.runClassification()
    assertEquals(rows.map(_.label), Vector("Visual", "Somatomotor", "Default"))
    assertEquals(rows.map(_.nFeatures), Vector(4, 4, 4))
    // Canonical row-major ordinal is (x * 4 + y) * 2 + z.
    assertEquals(rows.map(_.featureOrdinals), Vector(Vector(0, 2, 8, 10), Vector(16, 18, 24, 26), Vector(13, 15, 21, 23)))
  }

  test("atlas workflow runs cross-validated classification per region") {
    val rows = AtlasMvpaWorkflows.runClassification()
    assertEquals(rows.map(_.regionId), Vector(1, 2, 3))
    assertEquals(rows.map(_.label), Vector("Visual", "Somatomotor", "Default"))
    assertEquals(rows.map(_.nFeatures), Vector(4, 4, 4))
    assertEquals(rows.map(_.testedSamples), Vector(8, 8, 8))
    rows.foreach(row => assertEqualsDouble(row.accuracy, 1.0, 1e-12))
  }
