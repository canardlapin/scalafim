package scalafim.fmri.fit

import scalafim.dataset.*
import scalafim.fmri.design.baseline.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{FitPlan, FmriModelBuilder, ModelBuildSpec}
import scalafim.image.{DMat as ImageDMat, NeuroSpace}

class DctDriftFitSuite extends munit.FunSuite:
  private def right[A](value: Either[FitError, A]): A = value.fold(error => fail(error.message), identity)

  private def dot(a: Vector[Double], b: Vector[Double]): Double = a.indices.map(i => a(i) * b(i)).sum

  // Independent modified Gram-Schmidt projection with a second orthogonalization pass.
  // The production estimator uses Gale QR and a selected adjoint operator.
  private def residualize(columns: Vector[Vector[Double]], values: Vector[Double]): Vector[Double] =
    val q = columns.foldLeft(Vector.empty[Vector[Double]]) { (basis, column) =>
      val residual = (0 until 2).foldLeft(column) { (pass, _) =>
        basis.foldLeft(pass) { (current, direction) =>
          val coefficient = dot(current, direction)
          current.indices.map(i => current(i) - coefficient * direction(i)).toVector
        }
      }
      val norm = math.sqrt(dot(residual, residual))
      assert(norm > 1e-8, "oracle nuisance columns must be independent")
      basis :+ residual.map(_ / norm)
    }
    q.foldLeft(values) { (current, direction) =>
      val coefficient = dot(current, direction)
      current.indices.map(i => current(i) - coefficient * direction(i)).toVector
    }

  test("native selected betas and contrasts equal independent FWL on complete and censored original rows") {
    val frame = SamplingFrame(Seq(48), Seq(1.0))
    val events = DatasetEvents(Vector(0, 5, 11, 16, 24, 31).zipWithIndex.map { case (onset, i) =>
      Map("onset" -> onset.toString, "cond" -> (if i % 2 == 0 then "A" else "B"))
    })
    val values = Vector.tabulate(48)(row => Vector.tabulate(2)(voxel =>
      math.sin(0.31 * row) + (voxel + 1) * math.cos(0.67 * row) + 0.1 * row))
    val dataset = FmriDataset.unsafe(
      InMemoryDatasetBackend(DatasetId("dct-selected-fwl"), ImageDMat.fromRows(values), NeuroSpace(Vector(2, 1, 1))),
      frame, events)
    val built = FmriModelBuilder.buildModel(dataset, ModelBuildSpec(
      "onset ~ hrf(cond)", precision = 0.25.s,
      baselineBasis = BaselineBasis.Dct(DctCutoffPeriod.unsafeSeconds(24.0)),
      baselineIntercept = Intercept.Runwise
    ))
    assertEquals(built.eventModel.designMatrix.cols, 2)
    assertEquals(built.baselineModel.designMatrix.cols, 5)
    val plan = FitPlan(built)
    val ids = plan.coefficientAxis.get.columnIds
    val request = right(FirstLevelEstimateRequest.make(Vector(
      EstimateOutput.Coefficient(ids(0)), EstimateOutput.Coefficient(ids(1)),
      EstimateOutput.Contrast(StructuralTContrast.fromIds(ContrastId.unsafe("a-minus-b"), "A minus B",
        Map(ids(0) -> 1.0, ids(1) -> -1.0)))
    )))
    val rowSets = Vector((0 until 48).toVector, (0 until 48).filterNot(i => Set(0, 2, 7, 12, 23, 36, 47)(i)).toVector.reverse)
    for rows <- rowSets do
      val selection = DataSelection(time = IndexSelection.Indices(rows), voxels = IndexSelection.Indices(Vector(1, 0)))
      val prepared = right(FirstLevelEstimates.prepare(plan, request, ChunkSize.unsafe(1), selection))
      assertEquals(prepared.timepoints, rows)
      assertEquals(prepared.outputs.size, 3)
      assert(!prepared.computesResidualVariance)
      var reads = 0
      val reader = new DatasetSeriesReader:
        val dataset: FmriDataset = built.dataset
        def seriesEither(selected: DataSelection): Either[DatasetError, FmriSeries] =
          reads += 1
          dataset.seriesEither(selected)
      val nuisance = (2 until built.nPredictors).map(col => rows.map(row => built.designMatrix(row, col))).toVector
      val x0 = residualize(nuisance, rows.map(row => built.designMatrix(row, 0)))
      val x1 = residualize(nuisance, rows.map(row => built.designMatrix(row, 1)))
      val aa = dot(x0, x0)
      val ab = dot(x0, x1)
      val bb = dot(x1, x1)
      val determinant = aa * bb - ab * ab
      assert(determinant > 1e-8)
      var delivered = Vector.empty[Int]
      val outcome = right(prepared.foreachBlock(reader, block => {
        delivered ++= block.voxelIndices
        assertEquals(block.result.uncertainty, OlsEstimateUncertainty.NotRequested)
        block.voxelIndices.zipWithIndex.foreach { case (voxel, local) =>
          val y = residualize(nuisance, rows.map(row => values(row)(voxel)))
          val ay = dot(x0, y)
          val by = dot(x1, y)
          val beta0 = (bb * ay - ab * by) / determinant
          val beta1 = (aa * by - ab * ay) / determinant
          assertEqualsDouble(block.result.estimates(0, local), beta0, 1e-10)
          assertEqualsDouble(block.result.estimates(1, local), beta1, 1e-10)
          assertEqualsDouble(block.result.estimates(2, local), beta0 - beta1, 1e-10)
        }
        Right(())
      }))
      assertEquals(delivered, Vector(1, 0))
      assertEquals(reads, 2)
      assertEquals(outcome, EstimateExecutionOutcome.Completed(2, 2))
  }
