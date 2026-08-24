package scalafim.fmri.fit

import scalafim.image.SampleSpaces

import scalafim.fmri.fit.GaleTestSyntax.*

import scalafim.dataset.DatasetShape
import scalafim.fmri.model.{FitEngine, FitSummary}
import scalafim.image.SomeSampleSpace
import gale.linalg.{DMat, DVec}

class ResultArtifactsSuite extends munit.FunSuite:

  test("ResultManifest from dense fit preserves parameter metadata and provenance") {
    val result = denseResult()
    val manifest =
      ResultManifest
        .fromDenseFit(
          result,
          shape,
          exportIntent = ResultExportIntent.NamedStem("sub-01_task-demo")
        )
        .toOption
        .get

    val coefficientMaps = manifest.parameterMaps(ParameterMapKind.Coefficient)
    val standardErrorMaps = manifest.parameterMaps(ParameterMapKind.StandardError)
    val covariance = manifest.coefficientCovariance.get

    assertEquals(manifest.provenance.engine, FitEngine.OrdinaryLeastSquares)
    assertEquals(manifest.provenance.columnNames, Vector("task", "base_constant"))
    assertEquals(manifest.provenance.timepoints.toVector, Vector(0, 1, 2, 3))
    assertEquals(coefficientMaps.map(_.parameter), Vector("task", "base_constant"))
    assertEquals(standardErrorMaps.map(_.parameter), Vector("task", "base_constant"))
    assertEquals(coefficientMaps.map(_.map.exportIntent), Vector.fill(2)(ResultExportIntent.NamedStem("sub-01_task-demo")))
    assertEquals(coefficientMaps.head.map.voxelIndices, Vector(3, 1))
    assertEquals(coefficientMaps.head.map.valueVector, Vector(2.0, -1.0))
    assertEquals(standardErrorMaps.last.map.valueVector, Vector(0.2, 0.4))
    assertEquals(covariance.scope, CoefficientCovarianceScope.Shared)
    assertEquals(covariance.parameterNames.map(_.value), Vector("task", "base_constant"))
    assertEquals(covariance.voxelIndices, Vector(3, 1))
    assertEquals(covariance.exportIntent, ResultExportIntent.NamedStem("sub-01_task-demo"))
    assertEqualsDouble(covariance.matrices.head(0, 0), 0.25, 1e-12)
  }

  test("ContrastMap preserves statistic kind and degrees of freedom") {
    val result = denseResult()
    val provenance = AnalysisProvenance.fromResult(result)
    val t = TContrast("task", Map("task" -> 1.0)).evaluate(result).toOption.get
    val f = FContrast("task", Vector(Map("task" -> 1.0))).evaluate(result).toOption.get

    val tMaps = ContrastMap.fromTContrast(t, shape, provenance).toOption.get
    val fMaps = ContrastMap.fromFContrast(f, shape, provenance).toOption.get

    assertEquals(tMaps.map(_.statistic), Vector(ContrastMapKind.Estimate(1), ContrastMapKind.StandardError, ContrastMapKind.TStatistic))
    assertEquals(tMaps.map(_.contrast), Vector("task", "task", "task"))
    assertEquals(tMaps.map(_.map.label), Vector("task_estimate", "task_standard_error", "task_t"))
    assertEquals(tMaps.flatMap(_.degreesOfFreedom).map(_.residual.value).distinct, Vector(2))
    assertEquals(fMaps.map(_.statistic), Vector(ContrastMapKind.Estimate(1), ContrastMapKind.FStatistic))
    assertEquals(fMaps.last.degreesOfFreedom.flatMap(_.numerator), Some(1))
  }

  test("StatMap rejects value and voxel-space mismatches") {
    val result = denseResult()
    val provenance = AnalysisProvenance.fromResult(result)
    val selected = SelectedVoxelIndices.unsafe(Vector(0, 1))

    val shortValues =
      StatMap.make(
        name = "bad",
        kind = StatisticKind.Custom("bad"),
        values = DVec.fromSeq(Vector(1.0)),
        shape = shape,
        selectedVoxels = selected,
        provenance = provenance
      )
    assert(shortValues.left.toOption.exists {
      case FitError.InvalidFitAxis(axis, detail) =>
        axis == "result map values" && detail.contains("selected voxels 2")
      case _ =>
        false
    })

    val outOfBounds =
      StatMap.make(
        name = "bad",
        kind = StatisticKind.Custom("bad"),
        values = DVec.fromSeq(Vector(1.0)),
        shape = shape,
        selectedVoxels = SelectedVoxelIndices.unsafe(Vector(shape.spatialSize)),
        provenance = provenance
      )
    assert(outOfBounds.left.toOption.exists {
      case FitError.InvalidFitAxis(axis, detail) =>
        axis == "result map voxel indices" && detail.contains("out of bounds")
      case _ =>
        false
    })
  }

  test("CoefficientCovarianceArtifact preserves voxelwise covariance and rejects shape mismatches") {
    val base = denseResult()
    val covariance = CoefficientCovariance.unsafeVoxelwise(
        Vector(
          scalafim.fmri.fit.GaleTestMatrix.fromRows(
            Vector(
              Vector(0.25, 0.0),
              Vector(0.0, 0.5)
            )
          ),
          scalafim.fmri.fit.GaleTestMatrix.fromRows(
            Vector(
              Vector(0.75, 0.0),
              Vector(0.0, 1.5)
            )
          )
        )
      )
    val result = base.copy(
      inference = CoefficientInference.unsafeFromExisting(
        CoefficientInferenceScope.All,
        base.standardErrors,
        covariance,
        base.inference.varianceScale,
        base.inference.residualDegreesOfFreedom
      )
    )
    val provenance = AnalysisProvenance.fromResult(result)
    val artifact =
      CoefficientCovarianceArtifact
        .fromDenseFit(result, shape, provenance, ResultExportIntent.BidsDerivative("stat"))
        .toOption
        .get

    assertEquals(artifact.scope, CoefficientCovarianceScope.Voxelwise)
    assertEquals(artifact.matrices.length, 2)
    assertEquals(artifact.parameterNames.map(_.value), Vector("task", "base_constant"))
    assertEquals(artifact.exportIntent, ResultExportIntent.BidsDerivative("stat"))
    assertEqualsDouble(artifact.matrices(1)(0, 0), 0.75, 1e-12)

    val mismatched =
      CoefficientCovarianceArtifact.make(
        parameterNames = Vector("task", "base_constant"),
        covariance = CoefficientCovariance.unsafeVoxelwise(Vector(result.normalizedCovariance)),
        shape = shape,
        selectedVoxels = result.selectedVoxels,
        provenance = provenance
      )

    assert(mismatched.left.toOption.exists {
      case FitError.InvalidFitAxis(axis, detail) =>
        axis == "voxelwise coefficient covariance" && detail.contains("expected 2")
      case _ =>
        false
    })
  }

  test("restricted inference exports only inferable standard errors and covariance") {
    val result = restrictedDenseResult()
    val manifest = ResultManifest.fromDenseFit(result, shape).toOption.get

    assertEquals(
      manifest.parameterMaps(ParameterMapKind.Coefficient).map(_.parameter),
      Vector("task", "base_constant")
    )
    assertEquals(
      manifest.parameterMaps(ParameterMapKind.StandardError).map(_.parameter),
      Vector("task")
    )
    assertEquals(
      manifest.provenance.coefficientInference.map(_.method),
      Some(CoefficientInferenceMethod.ReducedRankConditional)
    )
    assertEquals(
      manifest.provenance.coefficientInference.map(_.inferableColumns),
      Some(Vector("task"))
    )

    val covariance = manifest.coefficientCovariance.get
    assertEquals(covariance.parameterNames.map(_.value), Vector("task"))
    assertEquals(covariance.matrices.map(matrix => matrix.rows -> matrix.cols), Vector(1 -> 1))
    assertEqualsDouble(covariance.matrices.head(0, 0), 0.25, 1e-12)
  }

  private def shape: DatasetShape =
    DatasetShape.unsafe(SampleSpaces(Vector(2, 2, 1)), timepoints = 4)

  private def denseResult(): DenseFmriFitResult =
    val covariance =
      CoefficientCovariance.unsafeShared(
        scalafim.fmri.fit.GaleTestMatrix.fromRows(
          Vector(
            Vector(0.25, 0.0),
            Vector(0.0, 0.5)
          )
        )
      )
    val residualVariance = DVec.fromSeq(Vector(1.0, 2.0))
    val residualDegreesOfFreedom = ResidualDegreesOfFreedom.unsafe(2)
    DenseFmriFitResult(
      coefficients = CoefficientBlock(
        scalafim.fmri.fit.GaleTestMatrix.fromRows(
          Vector(
            Vector(2.0, -1.0),
            Vector(3.0, 4.0)
          )
        )
      ),
      inference = CoefficientInference.unsafeFromExisting(
        CoefficientInferenceScope.All,
        StandardErrorBlock(
          scalafim.fmri.fit.GaleTestMatrix.fromRows(
            Vector(
              Vector(0.1, 0.3),
              Vector(0.2, 0.4)
            )
          )
        ),
        covariance,
        residualVariance,
        residualDegreesOfFreedom
      ),
      residualVariance = residualVariance,
      residualDegreesOfFreedom = residualDegreesOfFreedom,
      columnNames = Vector("task", "base_constant"),
      voxelIndices = Vector(3, 1),
      timepoints = Vector(0, 1, 2, 3),
      engine = FitEngine.OrdinaryLeastSquares,
      summary = FitSummary(
        engine = FitEngine.OrdinaryLeastSquares,
        timepoints = 4,
        predictors = 2,
        voxels = 2,
        robust = false,
        autocorrelated = false
      )
    )

  private def restrictedDenseResult(): DenseFmriFitResult =
    val result = denseResult()
    val covariance =
      CoefficientCovariance.unsafeShared(
        scalafim.fmri.fit.GaleTestMatrix.fromRows(
          Vector(
            Vector(0.25, 0.0),
            Vector(0.0, 0.0)
          )
        )
      )
    result.copy(
      inference = CoefficientInference
        .fromCovariance(
          scope = CoefficientInferenceScope.unsafeOnly(Vector(0), "task coefficient"),
          covariance = covariance,
          varianceScale = DVec.fromSeq(Vector(1.0, 2.0)),
          residualDegreesOfFreedom = result.residualDegreesOfFreedom,
          method = CoefficientInferenceMethod.ReducedRankConditional
        )
        .toOption
        .get
    )
