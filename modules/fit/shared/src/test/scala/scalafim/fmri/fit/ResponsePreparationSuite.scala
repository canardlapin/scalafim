package scalafim.fmri.fit

import scalafim.fmri.fit.GaleTestSyntax.*
import scalafim.fmri.fit.fixtures.WlsRFixture

import gale.linalg.Matrix
import scalafim.fmri.model.{
  ArOptions,
  ArStructure,
  DvarsWeightEstimator,
  DvarsWeightFunction,
  DvarsWeightScope,
  FitConfig,
  FixedWeightAlignment,
  MissingDataPolicy,
  NuisanceProjection,
  RobustOptions,
  RobustPsi,
  VolumeWeighting
}

class ResponsePreparationSuite extends munit.FunSuite:

  test("default plan is inspectable and preserves block input identity") {
    val block = blockInput()
    val plan = ResponsePreparationPlan.fromConfig(FitConfig())
    val prepared = plan.prepare(block).toOption.get

    assertEquals(prepared.input.design.value.toRows, block.design.value.toRows)
    assertEquals(prepared.input.response.value.toRows, block.response.value.toRows)
    assertEquals(prepared.input.voxelIndices, block.voxelIndices)
    assertEquals(prepared.input.timepoints, block.timepoints)
    assertEquals(prepared.input.preparationProvenance, Some(prepared.provenance))
    assertEquals(prepared.provenance.records.length, 6)
    assertEquals(prepared.provenance.deferred, Vector.empty)
    assert(prepared.provenance.applied.exists(_.step == ResponsePreparationStep.MissingData(MissingDataPolicy.Error)))
  }

  test("non-default plan distinguishes applied, rejected, and deferred surfaces") {
    val config = FitConfig(
      robust = RobustOptions(psi = RobustPsi.Huber()),
      autocorrelation = ArOptions(
        structure = ArStructure.Ar(1),
        censoredTimepoints = Vector(1),
        rho = Some(0.2)
      ),
      volumeWeighting = VolumeWeighting.Fixed(Vector(1.0, 0.9, 1.1)),
      nuisanceProjection = NuisanceProjection.MatrixProjection(
        Matrix.dense(3, 1)(1.0, 0.0, 1.0)
      ),
      missingData = MissingDataPolicy.Propagate
    )

    val provenance = ResponsePreparationPlan.fromConfig(config).provenance

    assertEquals(provenance.records.length, 6)
    assert(provenance.applied.exists(_.step match
      case ResponsePreparationStep.VolumeWeights(VolumeWeighting.Fixed(_, _)) => true
      case _ => false
    ) == false)
    assert(provenance.planned.exists(_.step match
      case ResponsePreparationStep.VolumeWeights(VolumeWeighting.Fixed(_, _)) => true
      case _ => false
    ))
    assert(provenance.rejected.isEmpty)
    assert(provenance.deferred.exists(_.step == ResponsePreparationStep.MissingData(MissingDataPolicy.Propagate)))
  }

  test("fixed volume weights apply by full-model timepoint index") {
    val plan = ResponsePreparationPlan.fromConfig(
      FitConfig(volumeWeighting = VolumeWeighting.Fixed(Vector(4.0, 9.0, 16.0, 25.0)))
    )

    val result = plan.prepare(selectedBlockInput()).toOption.get

    assertEquals(result.input.design.value.toRows, Vector(Vector(3.0, 0.0), Vector(5.0, 10.0)))
    assertEquals(result.input.response.value.toRows, Vector(Vector(9.0, 12.0), Vector(25.0, 30.0)))
    assert(result.provenance.applied.exists(_.step match
      case ResponsePreparationStep.VolumeWeights(VolumeWeighting.Fixed(_, _)) => true
      case _ => false
    ))
    assertEquals(result.provenance.volumeWeighting.map(_.source), Some(VolumeWeightingSource.Fixed(FixedWeightAlignment.FullSeries)))
  }

  test("fixed weights equal OLS on square-root weighted design and response") {
    val weights = Vector(1.0, 4.0, 9.0)
    val block = blockInput()
    val prepared = ResponsePreparationPlan.fromConfig(
      FitConfig(volumeWeighting = VolumeWeighting.Fixed(weights))
    ).prepare(block).toOption.get
    val directDesign = DesignMatrix.unsafe(
      GaleTestMatrix.fromRows(
        block.design.value.toRows.zip(weights).map { case (row, weight) =>
          val scale = math.sqrt(weight)
          row.map(_ * scale)
        }
      )
    )
    val directResponse = ResponseBlock.unsafe(
      GaleTestMatrix.fromRows(
        block.response.value.toRows.zip(weights).map { case (row, weight) =>
          val scale = math.sqrt(weight)
          row.map(_ * scale)
        }
      )
    )
    val weighted = Ols.fit(prepared.input.design, prepared.input.response).toOption.get
    val direct = Ols.fit(directDesign, directResponse).toOption.get

    assertEquals(prepared.input.design.value.toRows, directDesign.value.toRows)
    assertEquals(prepared.input.response.value.toRows, directResponse.value.toRows)
    assertEquals(weighted.coefficients.value.toRows, direct.coefficients.value.toRows)
    assertEquals(weighted.normalizedCovariance.toRows, direct.normalizedCovariance.toRows)
    assertEquals(weighted.residualVariance.toVector, direct.residualVariance.toVector)
    assertEquals(weighted.standardErrors.value.toRows, direct.standardErrors.value.toRows)
    assertEquals(weighted.residualDegreesOfFreedom, direct.residualDegreesOfFreedom)
  }

  test("multiresponse WLS agrees with independently fitted response columns") {
    val prepared = ResponsePreparationPlan.fromConfig(
      FitConfig(volumeWeighting = VolumeWeighting.Fixed(Vector(1.0, 4.0, 9.0)))
    ).prepare(blockInput()).fold(error => fail(error.message), identity)
    val multi = Ols.fit(prepared.input.design, prepared.input.response).fold(error => fail(error.message), identity)
    val responseRows = prepared.input.response.value.toRows

    var responseIndex = 0
    while responseIndex < prepared.input.response.voxels do
      val singleResponse = ResponseBlock.unsafe(
        GaleTestMatrix.fromRows(responseRows.map(row => Vector(row(responseIndex))))
      )
      val single = Ols.fit(prepared.input.design, singleResponse).fold(error => fail(error.message), identity)

      var coefficientIndex = 0
      while coefficientIndex < prepared.input.design.predictors do
        assertEqualsDouble(
          multi.coefficients.value(coefficientIndex, responseIndex),
          single.coefficients.value(coefficientIndex, 0),
          1e-12
        )
        assertEqualsDouble(
          multi.standardErrors.value(coefficientIndex, responseIndex),
          single.standardErrors.value(coefficientIndex, 0),
          1e-12
        )
        coefficientIndex += 1
      assertEqualsDouble(
        multi.residualVariance(responseIndex),
        single.residualVariance(0),
        1e-12
      )
      responseIndex += 1
  }

  test("estimated DVARS weighting is executable and records its source and normalization") {
    val plan = ResponsePreparationPlan.fromConfig(
      FitConfig(volumeWeighting = VolumeWeighting.Estimated(DvarsWeightEstimator()))
    )
    val block = dvarsBlockInput()
    val prepared = plan.prepare(block).fold(error => fail(error.message), identity)
    val dvars = Vector(1.0, 1.0, 1.0, 8.0, 1.0)
    val raw = dvars.map(value => 1.0 / (1.0 + value * value))
    val expected = raw.map(_ / (raw.sum / raw.length.toDouble))

    assertEquals(prepared.provenance.volumeWeighting.map(_.qualityMetric), Some(Some(dvars)))
    assertEquals(prepared.provenance.volumeWeighting.map(_.weights), Some(expected))
    assertEquals(
      prepared.provenance.volumeWeighting.map(_.source),
      Some(VolumeWeightingSource.ResponseDvars(DvarsWeightEstimator()))
    )
    assertEquals(
      prepared.provenance.volumeWeighting.map(_.normalization),
      Some(VolumeWeightNormalization.MeanOne(DvarsWeightScope.WithinRun))
    )
    assertEquals(prepared.input.design.value.toRows, block.design.value.toRows.zip(expected).map { case (row, weight) => row.map(_ * math.sqrt(weight)) })
    assert(plan.provenance.planned.exists(_.step match
      case ResponsePreparationStep.VolumeWeights(VolumeWeighting.Estimated(_)) => true
      case _ => false
    ))
  }

  test("zero fixed weights exclude rows before rank and residual-df calculation") {
    val block = dvarsBlockInput()
    val prepared = ResponsePreparationPlan.fromConfig(
      FitConfig(volumeWeighting = VolumeWeighting.Fixed(Vector(1.0, 1.0, 1.0, 0.0, 1.0)))
    ).prepare(block).fold(error => fail(error.message), identity)
    val fit = Ols.fit(prepared.input.design, prepared.input.response).fold(error => fail(error.message), identity)

    assertEquals(prepared.input.timepoints, Vector(0, 1, 2, 4))
    assertEquals(prepared.input.partitions.map(_.rowIndices), Vector(Vector(0, 1, 2, 3)))
    assertEquals(prepared.input.partitions.map(_.timepoints), Vector(Vector(0, 1, 2, 4)))
    assertEquals(fit.residualDegreesOfFreedom.value, 2)
    assertEquals(prepared.provenance.volumeWeighting.map(_.zeroWeightTimepoints), Some(Vector(3)))
    assertEquals(prepared.provenance.volumeWeighting.map(_.excludedTimepoints), Some(Vector(3)))
  }

  test("Tukey DVARS estimation gives exact-zero artifacts lawful exclusion semantics") {
    val estimator = DvarsWeightEstimator(DvarsWeightFunction.TukeyBisquare())
    val prepared = ResponsePreparationPlan.fromConfig(
      FitConfig(volumeWeighting = VolumeWeighting.Estimated(estimator))
    ).prepare(dvarsBlockInput()).fold(error => fail(error.message), identity)
    val fit = Ols.fit(prepared.input.design, prepared.input.response).fold(error => fail(error.message), identity)

    assertEquals(prepared.input.timepoints, Vector(0, 1, 2, 4))
    assertEquals(prepared.provenance.volumeWeighting.map(_.zeroWeightTimepoints), Some(Vector(3)))
    assertEquals(fit.residualDegreesOfFreedom.value, 2)
  }

  test("negative and non-finite fixed weights are explicit preparation errors") {
    val inputs = Vector(
      (Vector(1.0, -1.0, 1.0), "timepoint 1 has negative weight -1.0"),
      (Vector(1.0, Double.NaN, 1.0), "timepoint 1 has non-finite weight NaN")
    )
    inputs.foreach { case (weights, detail) =>
      val plan = ResponsePreparationPlan(
        missingData = MissingDataPolicy.Error,
        censoredTimepoints = Vector.empty,
        volumeWeighting = VolumeWeighting.Fixed(weights),
        nuisanceProjection = NuisanceProjection.Disabled,
        autocorrelation = ArOptions(),
        robust = RobustOptions()
      )
      plan.prepare(blockInput()).left.toOption match
        case Some(FitError.InvalidVolumeWeights(actual)) =>
          assert(actual == detail || actual.replace("-1", "-1.0") == detail)
        case other => fail(s"expected invalid volume weights, got $other")
    }
  }

  test("selected-row fixed alignment is exact rather than length-heuristic") {
    val plan = ResponsePreparationPlan.fromConfig(
      FitConfig(volumeWeighting = VolumeWeighting.Fixed(
        Vector(4.0, 25.0),
        FixedWeightAlignment.SelectedRows
      ))
    )
    val prepared = plan.prepare(selectedBlockInput()).fold(error => fail(error.message), identity)

    assertEquals(prepared.input.design.value.toRows, Vector(Vector(2.0, 0.0), Vector(5.0, 10.0)))
    assertEquals(prepared.input.response.value.toRows, Vector(Vector(6.0, 8.0), Vector(25.0, 30.0)))
    assertEquals(
      prepared.provenance.volumeWeighting.map(_.source),
      Some(VolumeWeightingSource.Fixed(FixedWeightAlignment.SelectedRows))
    )
  }

  test("DVARS resets at selection gaps instead of differencing across censored timepoints") {
    val block = dvarsBlockInput(
      responseRows = Vector(Vector(0.0), Vector(1.0), Vector(100.0), Vector(101.0)),
      timepoints = Vector(0, 1, 3, 4)
    )
    val prepared = ResponsePreparationPlan.fromConfig(
      FitConfig(volumeWeighting = VolumeWeighting.Estimated(DvarsWeightEstimator()))
    ).prepare(block).fold(error => fail(error.message), identity)

    assertEquals(prepared.provenance.volumeWeighting.flatMap(_.qualityMetric), Some(Vector(1.0, 1.0, 1.0, 1.0)))
    assertEquals(prepared.provenance.volumeWeighting.map(_.partitions.map(_.timepoints)), Some(Vector(Vector(0, 1, 3, 4))))
  }

  test("within-run DVARS normalization does not cross acquisition boundaries") {
    val input = dvarsBlockInput(
      responseRows = Vector(
        Vector(0.0), Vector(1.0), Vector(2.0),
        Vector(100.0), Vector(110.0), Vector(120.0)
      ),
      timepoints = Vector(0, 1, 2, 3, 4, 5)
    ).copy(
      partitions = Vector(
        RunPartition(0, Vector(0, 1, 2), Vector(0, 1, 2)),
        RunPartition(1, Vector(3, 4, 5), Vector(3, 4, 5))
      )
    )
    val prepared = ResponsePreparationPlan.fromConfig(
      FitConfig(volumeWeighting = VolumeWeighting.Estimated(DvarsWeightEstimator()))
    ).prepare(input).fold(error => fail(error.message), identity)

    assertEquals(prepared.provenance.volumeWeighting.flatMap(_.qualityMetric), Some(Vector.fill(6)(1.0)))
    assertEquals(prepared.provenance.volumeWeighting.map(_.weights), Some(Vector.fill(6)(1.0)))
    assertEquals(
      prepared.provenance.volumeWeighting.map(_.partitions.map(_.runIndex.value)),
      Some(Vector(0, 1))
    )
  }

  test("all DVARS transformations match the independent base-R receipt") {
    val cases = Vector(
      DvarsWeightFunction.InverseSquared -> WlsRFixture.inverseSquaredWeights,
      DvarsWeightFunction.SoftThreshold() -> WlsRFixture.softThresholdWeights,
      DvarsWeightFunction.TukeyBisquare() -> WlsRFixture.tukeyWeights
    )
    val input = dvarsBlockInput(
      responseRows = WlsRFixture.dvarsResponse,
      timepoints = WlsRFixture.dvarsResponse.indices.toVector
    )

    cases.foreach { case (function, expected) =>
      val prepared = ResponsePreparationPlan.fromConfig(
        FitConfig(volumeWeighting = VolumeWeighting.Estimated(DvarsWeightEstimator(function)))
      ).prepare(input).fold(error => fail(error.message), identity)
      val receipt = prepared.provenance.volumeWeighting.getOrElse(fail("DVARS receipt is required"))
      assertEquals(receipt.qualityMetric, Some(WlsRFixture.dvars))
      receipt.weights.zip(expected).foreach { case (actual, reference) =>
        assertEqualsDouble(actual, reference, 1e-12)
      }
    }
  }

  test("nuisance projection rows must align with selected timepoints") {
    val plan = ResponsePreparationPlan.fromConfig(
      FitConfig(
        nuisanceProjection = NuisanceProjection.MatrixProjection(
          Matrix.dense(2, 1)(1.0, 0.0)
        )
      )
    )

    val result = plan.prepare(blockInput())

    assertEquals(
      result.left.toOption,
      Some(FitError.InvalidFitAxis("nuisance projection", "matrix rows 2 do not match selected timepoints 3"))
    )
  }

  private def blockInput(): FitBlockInput =
    FitBlockInput(
      design = DesignMatrix.unsafe(
        scalafim.fmri.fit.GaleTestMatrix.fromRows(
          Vector(
            Vector(1.0, 0.0),
            Vector(1.0, 1.0),
            Vector(1.0, 2.0)
          )
        )
      ),
      response = ResponseBlock.unsafe(
        scalafim.fmri.fit.GaleTestMatrix.fromRows(
          Vector(
            Vector(1.0, 2.0),
            Vector(3.0, 4.0),
            Vector(5.0, 6.0)
          )
        )
      ),
      voxelIndices = Vector(10, 11),
      timepoints = Vector(0, 1, 2)
    )

  private def selectedBlockInput(): FitBlockInput =
    FitBlockInput(
      design = DesignMatrix.unsafe(
        GaleTestMatrix.fromRows(
          Vector(
            Vector(1.0, 0.0),
            Vector(1.0, 2.0)
          )
        )
      ),
      response = ResponseBlock.unsafe(
        GaleTestMatrix.fromRows(
          Vector(
            Vector(3.0, 4.0),
            Vector(5.0, 6.0)
          )
        )
      ),
      voxelIndices = Vector(10, 11),
      timepoints = Vector(1, 3)
    )

  private def dvarsBlockInput(
      responseRows: Vector[Vector[Double]] = Vector(
        Vector(0.0, 0.0),
        Vector(1.0, 1.0),
        Vector(2.0, 2.0),
        Vector(10.0, 10.0),
        Vector(11.0, 11.0)
      ),
      timepoints: Vector[Int] = Vector(0, 1, 2, 3, 4)
  ): FitBlockInput =
    val rows = responseRows.length
    FitBlockInput(
      design = DesignMatrix.unsafe(
        GaleTestMatrix.fromRows(
          Vector.tabulate(rows)(row => Vector(1.0, row.toDouble))
        )
      ),
      response = ResponseBlock.unsafe(GaleTestMatrix.fromRows(responseRows)),
      voxelIndices = responseRows.head.indices.toVector,
      timepoints = timepoints,
      partitions = Vector(RunPartition(0, (0 until rows).toVector, timepoints))
    )
