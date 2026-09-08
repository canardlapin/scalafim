package scalafim.examples.workflows

class AtlasMvpaWorkflowSuite extends munit.FunSuite:
  test("atlas workflow builds a measurement frame from atlas labels") {
    val frame = AtlasMvpaWorkflows.frame
    assertEquals(frame.size, 3)
    assertEquals(frame.entries.map(_.measurement.local.size), Vector(4, 4, 4))
    assertEquals(
      frame.entries.map(_.rendition.region.label),
      Vector("Visual", "Somatomotor", "Default")
    )
  }

  test("atlas workflow runs cross-validated classification per region") {
    val rows = AtlasMvpaWorkflows.runClassification()
    assertEquals(rows.map(_.regionId), Vector(1, 2, 3))
    assertEquals(rows.map(_.label), Vector("Visual", "Somatomotor", "Default"))
    assertEquals(rows.map(_.nFeatures), Vector(4, 4, 4))
    assertEquals(rows.map(_.testedSamples), Vector(8, 8, 8))
    rows.foreach(row => assertEqualsDouble(row.accuracy, 1.0, 1e-12))
  }

  test("atlas workflow runs an identified relational RDM through the same frame") {
    val rows = AtlasMvpaWorkflows.runRelationalRdm()
    assertEquals(rows.map(_.regionId), Vector(1, 2, 3))
    assertEquals(rows.map(_.label), Vector("Visual", "Somatomotor", "Default"))
    assertEquals(rows.map(_.nFeatures), Vector(4, 4, 4))
    assertEqualsDouble(rows(0).faceSceneDistance, 16.56423333333333, 1e-12)
    assertEqualsDouble(rows(1).faceSceneDistance, 9.424233333333335, 1e-12)
    assertEqualsDouble(rows(2).faceSceneDistance, 4.284233333333334, 1e-12)
    assertEquals(
      rows.map(_.measurement),
      AtlasMvpaWorkflows.frame.entries.map(_.measurement.identity)
    )
    assertEquals(rows.map(_.fit).distinct.size, 3)
  }
