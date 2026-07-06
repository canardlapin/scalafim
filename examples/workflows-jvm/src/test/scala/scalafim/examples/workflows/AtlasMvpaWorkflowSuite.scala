package scalafim.examples.workflows

class AtlasMvpaWorkflowSuite extends munit.FunSuite:
  test("atlas workflow builds regional MVPA feature plan from atlas labels") {
    val plan = AtlasMvpaWorkflows.featurePlan()
    assertEquals(plan.name, "workflow-atlas-regions")
    assertEquals(plan.size, 3)
    assertEquals(plan.featureSets.map(_.size), Vector(4, 4, 4))
    assertEquals(plan.featureSets.flatMap(_.label), Vector("Visual", "Somatomotor", "Default"))
  }

  test("atlas workflow runs cross-validated classification per region") {
    val rows = AtlasMvpaWorkflows.runClassification()
    assertEquals(rows.map(_.regionId), Vector(1, 2, 3))
    assertEquals(rows.map(_.label), Vector("Visual", "Somatomotor", "Default"))
    assertEquals(rows.map(_.nFeatures), Vector(4, 4, 4))
    assertEquals(rows.map(_.testedSamples), Vector(8, 8, 8))
    rows.foreach(row => assertEqualsDouble(row.accuracy, 1.0, 1e-12))
  }
