package scalafim.fmri.fit

import scalafim.dataset.{DataSelection, DatasetId, FmriDataset, IndexSelection, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitPlan, FmriModel}
import scalafim.image.{DMat, NeuroSpace}

class ImageMapsSuite extends munit.FunSuite:

  private def assertVectorClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).zipWithIndex.foreach { case ((a, e), i) =>
      assert(math.abs(a - e) <= tol, clues(i, a, e))
    }

  private def samplingFrame: SamplingFrame =
    SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))

  private def dataset: FmriDataset =
    val data = DMat.fromRows(
      Vector(
        Vector(1.0, 2.0, 10.0, -1.0),
        Vector(3.0, 1.0, 9.0, -2.0),
        Vector(5.0, 0.0, 8.0, -3.0),
        Vector(7.0, -1.0, 7.0, -4.0)
      )
    )
    FmriDataset(
      backend = InMemoryDatasetBackend(DatasetId("image-map-demo"), data, NeuroSpace(Vector(2, 2, 1))),
      samplingFrame = samplingFrame
    )

  private def model: FmriModel =
    val eventModel =
      EventModel(
        terms = Vector.empty,
        samplingFrame = samplingFrame,
        designMatrix = Mat.fromRows(Vector(Vector(0.0), Vector(1.0), Vector(2.0), Vector(3.0))),
        columnNames = Vector("task"),
        termSpans = Vector(0 -> 1),
        colIndices = Map("task" -> Vector(0))
      )
    val baseline =
      BaselineModel.build(
        samplingFrame = samplingFrame,
        basis = BaselineBasis.Constant,
        intercept = Intercept.Global
      )
    FmriModel(eventModel, baseline, dataset)

  test("Dense fit coefficient and SE maps preserve full voxel placement") {
    val result = FitPlanExecutor.unsafeFit(FitPlan(model)).asInstanceOf[DenseFmriFitResult]
    val coef = result.coefficientMaps(dataset.shape)
    val se = result.standardErrorMaps(dataset.shape)

    assertEquals(coef.names, Vector("task", "base_constant"))
    assertEquals(coef.values.map.cardinality, 4)
    assertVectorClose(coef.dense.series(0).toVector, Vector(2.0, 1.0), 1e-10)
    assertVectorClose(coef.dense.series(1).toVector, Vector(-1.0, 2.0), 1e-10)
    assertVectorClose(coef.dense.series(2).toVector, Vector(-1.0, 10.0), 1e-10)
    assertVectorClose(coef.dense.series(3).toVector, Vector(-1.0, -1.0), 1e-10)
    assertEquals(se.names, coef.names)
    assert(se.dense.series(0).toVector.forall(v => math.abs(v) < 1e-10))
  }

  test("image maps place selected voxels into their original image locations") {
    val result = FitPlanExecutor.unsafeFit(
      FitPlan(model),
      DataSelection(voxels = IndexSelection.indices(3, 1))
    ).asInstanceOf[DenseFmriFitResult]

    assertEquals(result.voxelIndices, Vector(3, 1))
    assertEquals(result.selectedVoxels.toVector, Vector(3, 1))
    val coef = result.coefficientMaps(dataset.shape)
    val dense = coef.dense

    assertEquals(coef.values.map.cardinality, 2)
    assertVectorClose(dense.series(0).toVector, Vector(0.0, 0.0), 1e-10)
    assertVectorClose(dense.series(1).toVector, Vector(-1.0, 2.0), 1e-10)
    assertVectorClose(dense.series(2).toVector, Vector(0.0, 0.0), 1e-10)
    assertVectorClose(dense.series(3).toVector, Vector(-1.0, -1.0), 1e-10)
  }

  test("t and F contrast statistics map into image space") {
    val result = FitPlanExecutor.unsafeFit(
      FitPlan(model),
      DataSelection(voxels = IndexSelection.indices(1))
    ).asInstanceOf[DenseFmriFitResult]

    val t = TContrast("task", Map("task" -> 1.0)).evaluate(result).toOption.get
    val tMap = t.statisticMap(dataset.shape).dense
    assertEqualsDouble(tMap.series(0)(0), 0.0, 1e-10)
    assertEqualsDouble(tMap.series(1)(0), t.statistics(0), 1e-10)

    val f = FContrast("task", Vector(Map("task" -> 1.0))).evaluate(result).toOption.get
    val fMap = f.statisticMap(dataset.shape).dense
    assertEqualsDouble(fMap.series(0)(0), 0.0, 1e-10)
    assertEqualsDouble(fMap.series(1)(0), f.statistics(0), 1e-10)
  }
