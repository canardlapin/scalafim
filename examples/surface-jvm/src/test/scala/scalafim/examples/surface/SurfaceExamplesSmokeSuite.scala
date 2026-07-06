package scalafim.examples.surface

import scalafim.surface.*

class SurfaceExamplesSmokeSuite extends munit.FunSuite:
  test("surface IO examples read bundled FreeSurfer and GIFTI geometry") {
    val summaries = SurfaceIoExamples.bundledSummaries()

    assertEquals(summaries.map(_.source), Vector("FreeSurfer ASCII", "GIFTI"))
    assertEquals(summaries.map(_.vertexCount), Vector(4, 4))
    assertEquals(summaries.map(_.faceCount), Vector(2, 2))
    assertEquals(summaries.map(_.hemisphere), Vector(Hemisphere.Left, Hemisphere.Left))
    assertEquals(summaries.map(_.kind), Vector(SurfaceKind.SmoothWm, SurfaceKind.Midthickness))
    assertEquals(summaries.map(_.edgeCount), Vector(5, 5))
    assertEquals(summaries.last.worldOffset, Vector(10.0, 20.0, 30.0))
  }

  test("labeled surface parcel workflow produces parcel units and boundary contacts") {
    val rows = LabeledSurfaceParcelExamples.parcelRows()

    assertEquals(rows.map(_.label), Vector(1, 2))
    assertEquals(rows.map(_.name), Vector("TriangleParcel", "ApexParcel"))
    assertEquals(rows.map(_.nVertices), Vector(3, 1))
    assertEquals(rows.map(_.representativeVertex), Vector(0, 3))
    assertEquals(rows.map(_.boundaryContacts), Vector(2, 2))
  }

  test("labeled surface keeps vertex labels addressable") {
    val labeled = LabeledSurfaceParcelExamples.bundledLabeledSurface()

    assertEquals(labeled.labelAt(VertexId(0)), Some(1))
    assertEquals(labeled.labelAt(VertexId(3)), Some(2))
    assertEquals(labeled.info(1).map(_.name), Some("TriangleParcel"))
  }
