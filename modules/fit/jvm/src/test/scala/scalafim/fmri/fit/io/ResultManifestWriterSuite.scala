package scalafim.fmri.fit.io

import scalafim.dataset.DatasetShape
import scalafim.fmri.fit.*
import scalafim.fmri.design.{DesignSchema, ModelSource}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitConfig, FitEngine, FitSummary}
import scalafim.image.NeuroSpace
import scalafim.image.io.Nifti
import gale.linalg.DVec

import java.nio.charset.StandardCharsets
import java.nio.file.Files

class ResultManifestWriterSuite extends munit.FunSuite:

  test("ResultManifestWriter writes BIDS-style NIfTI, covariance TSV, and sidecar artifacts") {
    val result = denseResult().copy(
      olsDiagnostics = Some(fullRankDiagnostics),
      preparationProvenance = Some(ResponsePreparationPlan.fromConfig(FitConfig()).provenance)
    )
    val provenance = AnalysisProvenance.fromResult(result, source = "writer-suite")
    val t = TContrast("task", Map("task" -> 1.0)).evaluate(result).toOption.get
    val tMaps = ContrastMap.fromTContrast(t, shape, provenance).toOption.get
    val manifest =
      ResultManifest
        .fromDenseFit(result, shape, exportIntent = ResultExportIntent.BidsDerivative("stat"), source = "writer-suite")
        .toOption
        .get
        .withContrasts(tMaps)

    val root = Files.createTempDirectory("scalafim-result-writer")
    val written =
      ResultManifestWriter
        .writeBidsDirectory(manifest, root, "sub-01_task-demo")
        .toOption
        .get

    val coefficientPath = root.resolve("sub-01_task-demo_desc-parametercoefficient_statmap.nii")
    val contrastPath = root.resolve("sub-01_task-demo_contrast-task_statmap.nii")
    val covariancePath = root.resolve("sub-01_task-demo_desc-coefficientCovariance_covariance.tsv")
    val sidecarPath = root.resolve("sub-01_task-demo_resultmanifest.json")

    assert(written.paths.contains(coefficientPath))
    assert(written.paths.contains(contrastPath))
    assert(written.paths.contains(covariancePath))
    assert(written.paths.contains(sidecarPath))

    val coefficientImage = Nifti.readVec(coefficientPath)
    assertEquals(coefficientImage.space.dims.take(4), Vector(2, 1, 1, 2))
    assertEqualsDouble(coefficientImage.linear(0), 2.0, 1e-12)
    assertEqualsDouble(coefficientImage.linear(1), -1.0, 1e-12)
    assertEqualsDouble(coefficientImage.linear(2), 3.0, 1e-12)
    assertEqualsDouble(coefficientImage.linear(3), 4.0, 1e-12)

    val contrastImage = Nifti.readVec(contrastPath)
    assertEquals(contrastImage.space.dims.take(4), Vector(2, 1, 1, 3))
    assertEqualsDouble(contrastImage.linear(0), t.estimates(0), 1e-12)
    assertEqualsDouble(contrastImage.linear(1), t.estimates(1), 1e-12)

    val covariance = Files.readString(covariancePath, StandardCharsets.UTF_8)
    assert(covariance.contains("scope\tparameter_i\tparameter_j\tvalue"))
    assert(covariance.contains("shared\ttask\ttask\t0.25"))
    assert(covariance.contains("shared\tbase_constant\tbase_constant\t0.5"))

    val sidecar = Files.readString(sidecarPath, StandardCharsets.UTF_8)
    assert(sidecar.contains("writer-suite"))
    assert(sidecar.contains("\"nifti_map_layout\": \"bundled\""))
    assert(sidecar.contains("parameter-coefficient"))
    assert(sidecar.contains("contrast-task"))
    assert(sidecar.contains("\"design_fingerprint\": \"design-schema/v1:"))
    assert(sidecar.contains("\"structural_columns\": [{\"id\":"))
    assert(sidecar.contains("\"inferable_column_ids\": [\"legacy|Event|0|task\", \"legacy|Event|1|base_constant\"]"))
    assert(sidecar.contains("\"response_preparation\": ["))
    assert(sidecar.contains("\"step\": \"missing_data\""))
    assert(sidecar.contains("\"rank_reports\": ["))
    assert(sidecar.contains("\"rank\": 2"))
    assert(sidecar.contains("\"tolerance_convention\": \"ScaleAware\""))
    assert(sidecar.contains("\"aliased_column_ids\": []"))
  }

  test("ResultManifestWriter can write one named NIfTI per parameter and contrast map") {
    val result = denseResult()
    val provenance = AnalysisProvenance.fromResult(result, source = "individual-writer-suite")
    val t = TContrast("task", Map("task" -> 1.0)).evaluate(result).toOption.get
    val manifest =
      ResultManifest
        .fromDenseFit(result, shape, exportIntent = ResultExportIntent.BidsDerivative("stat"), source = "individual-writer-suite")
        .toOption
        .get
        .withContrasts(ContrastMap.fromTContrast(t, shape, provenance).toOption.get)
    val root = Files.createTempDirectory("scalafim-individual-result-writer")

    val written =
      ResultManifestWriter
        .writeBidsDirectory(manifest, root, "sub-01_task-demo", BidsNiftiMapLayout.Individual)
        .toOption
        .get

    val expectedNiftis =
      Vector(
        "sub-01_task-demo_desc-task_stat-coefficient_statmap.nii",
        "sub-01_task-demo_desc-baseconstant_stat-coefficient_statmap.nii",
        "sub-01_task-demo_desc-task_stat-standarderror_statmap.nii",
        "sub-01_task-demo_desc-baseconstant_stat-standarderror_statmap.nii",
        "sub-01_task-demo_contrast-task_stat-estimate1_statmap.nii",
        "sub-01_task-demo_contrast-task_stat-standarderror_statmap.nii",
        "sub-01_task-demo_contrast-task_stat-t_statmap.nii"
      ).map(root.resolve)

    assertEquals(written.paths.filter(_.toString.endsWith(".nii")), expectedNiftis)
    assert(written.artifacts.filter(_.path.toString.endsWith(".nii")).forall(_.labels.length == 1))

    val taskCoefficient = Nifti.readVol(expectedNiftis.head)
    assertEquals(taskCoefficient.space.dims, Vector(2, 1, 1))
    assertEqualsDouble(taskCoefficient.linear(0), 2.0, 1e-12)
    assertEqualsDouble(taskCoefficient.linear(1), -1.0, 1e-12)

    val sidecar = Files.readString(root.resolve("sub-01_task-demo_resultmanifest.json"), StandardCharsets.UTF_8)
    assert(sidecar.contains("\"nifti_map_layout\": \"individual\""))
    expectedNiftis.foreach(path => assert(sidecar.contains(path.getFileName.toString)))
    assert(sidecar.contains("\"labels\": [\"base_constant\"]"))
    assert(sidecar.contains("\"labels\": [\"task_t\"]"))
  }

  test("ResultManifestWriter persists voxel status and contrast exclusions") {
    val result = denseResult().copy(
      voxelStatuses = Some(Vector(VoxelFitStatus.Constant, VoxelFitStatus.Estimable))
    )
    val provenance = AnalysisProvenance.fromResult(result, source = "voxel-status-writer-suite")
    val contrast = TContrast("task", Map("task" -> 1.0)).evaluate(result).toOption.get
    val manifest =
      ResultManifest
        .fromDenseFit(result, shape, source = "voxel-status-writer-suite")
        .toOption
        .get
        .withContrasts(ContrastMap.fromTContrast(contrast, shape, provenance).toOption.get)
    val root = Files.createTempDirectory("scalafim-voxel-status-writer")

    ResultManifestWriter.writeBidsDirectory(manifest, root, "sub-01_task-status").toOption.get

    val sidecar = Files.readString(root.resolve("sub-01_task-status_resultmanifest.json"), StandardCharsets.UTF_8)
    assert(sidecar.contains("\"voxel_statuses\": [{\"voxel\": 0, \"status\": \"constant\"}, {\"voxel\": 1, \"status\": \"ok\"}]"))
    assert(sidecar.contains("\"contrast_exclusions\": [{\"contrast_id\": \"task\", \"voxels\": [{\"voxel\": 0, \"status\": \"constant\"}]}]"))
  }

  test("ResultManifestWriter rejects unsafe stems and colliding named-map paths before writing") {
    val root = Files.createTempDirectory("scalafim-result-writer-preflight")
    assert(ResultManifestExportTarget.bidsDirectory(root, "../outside").left.toOption.exists {
      case ResultManifestIoError.InvalidTarget(detail) => detail.contains("one BIDS-safe path segment")
      case _ => false
    })

    val result = denseResult()
    val provenance = AnalysisProvenance.fromResult(result, source = "collision-writer-suite")
    val first = TContrast("task-a", Map("task" -> 1.0)).evaluate(result).toOption.get
    val second = TContrast("task_a", Map("task" -> 1.0)).evaluate(result).toOption.get
    val contrasts =
      ContrastMap.fromTContrast(first, shape, provenance).toOption.get ++
        ContrastMap.fromTContrast(second, shape, provenance).toOption.get
    val manifest = ResultManifest.fromDenseFit(result, shape, source = "collision-writer-suite").toOption.get.withContrasts(contrasts)

    val collision =
      ResultManifestWriter.writeBidsDirectory(
        manifest,
        root,
        "sub-01_task-demo",
        BidsNiftiMapLayout.Individual
      )
    assert(collision.left.toOption.exists {
      case ResultManifestIoError.InvalidTarget(detail) => detail.contains("multiple result artifacts resolve")
      case _ => false
    })
    val files = Files.list(root)
    try assertEquals(files.count(), 0L)
    finally files.close()
  }

  test("ResultManifestWriter reports unsupported HDF5 and GDS formats explicitly") {
    val manifest =
      ResultManifest
        .fromDenseFit(denseResult(), shape, exportIntent = ResultExportIntent.NamedStem("demo"))
        .toOption
        .get
    val root = Files.createTempDirectory("scalafim-result-writer")
    val hdf5 = ResultManifestExportTarget.hdf5(root.resolve("demo.h5"), "demo").toOption.get
    val gds = ResultManifestExportTarget.gds(root.resolve("demo.gds"), "demo").toOption.get

    assert(ResultManifestWriter.write(manifest, hdf5).left.toOption.exists {
      case ResultManifestIoError.UnsupportedFormat(ResultManifestExportFormat.Hdf5, detail) =>
        detail.contains("not wired")
      case _ =>
        false
    })
    assert(ResultManifestWriter.write(manifest, gds).left.toOption.exists {
      case ResultManifestIoError.UnsupportedFormat(ResultManifestExportFormat.Gds, detail) =>
        detail.contains("not wired")
      case _ =>
        false
    })
  }

  test("ResultManifestWriter persists restricted inference provenance") {
    val base = denseResult()
    val result = base.copy(
      inference = CoefficientInference
        .fromCovariance(
          scope = CoefficientInferenceScope.unsafeOnly(Vector(0), "task coefficient"),
          covariance = CoefficientCovariance.unsafeShared(
            scalafim.fmri.fit.GaleTestMatrix.fromRows(
              Vector(
                Vector(0.25, 0.0),
                Vector(0.0, 0.0)
              )
            )
          ),
          varianceScale = base.inference.varianceScale,
          residualDegreesOfFreedom = base.residualDegreesOfFreedom,
          method = CoefficientInferenceMethod.ReducedRankConditional
        )
        .toOption
        .get
    )
    val manifest = ResultManifest.fromDenseFit(result, shape, source = "restricted-writer-suite").toOption.get
    val root = Files.createTempDirectory("scalafim-restricted-result-writer")

    val written = ResultManifestWriter.writeBidsDirectory(manifest, root, "sub-01_task-restricted").toOption.get
    val standardErrors = written.artifacts.find(_.kind == ResultWrittenArtifactKind.ParameterMaps(ParameterMapKind.StandardError)).get
    assertEquals(standardErrors.labels, Vector("task"))

    val sidecar = Files.readString(root.resolve("sub-01_task-restricted_resultmanifest.json"), StandardCharsets.UTF_8)
    assert(sidecar.contains("reduced_rank_conditional"))
    assert(sidecar.contains("\"inferable_columns\": [\"task\"]"))
    assert(sidecar.contains("\"inferable_column_ids\": [\"legacy|Event|0|task\"]"))
  }

  private def shape: DatasetShape =
    DatasetShape.unsafe(NeuroSpace(Vector(2, 1, 1)), timepoints = 4)

  private def denseResult(): DenseFmriFitResult =
    val covariance =
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(0.25, 0.0),
          Vector(0.0, 0.5)
        )
      )
    val coefficientCovariance = CoefficientCovariance.unsafeShared(covariance)
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
              Vector(0.5, math.sqrt(0.5)),
              Vector(math.sqrt(0.5), 1.0)
            )
          )
        ),
        coefficientCovariance,
        residualVariance,
        residualDegreesOfFreedom
      ),
      residualVariance = residualVariance,
      residualDegreesOfFreedom = residualDegreesOfFreedom,
      columnNames = Vector("task", "base_constant"),
      voxelIndices = Vector(0, 1),
      timepoints = Vector(0, 1, 2, 3),
      engine = FitEngine.OrdinaryLeastSquares,
      summary = FitSummary(
        engine = FitEngine.OrdinaryLeastSquares,
        timepoints = 4,
        predictors = 2,
        voxels = 2,
        robust = false,
        autocorrelated = false
      ),
      coefficientAxis = Some(coefficientAxis)
    )

  private def coefficientAxis: scalafim.fmri.design.CoefficientAxis =
    DesignSchema
      .legacy(
        matrix = Mat.unsafe(4, 2, Array(1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 2.0, 1.0)),
        samplingFrame = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0)),
        columnNames = Vector("task", "base_constant"),
        source = ModelSource.Event
      )
      .coefficientAxis

  private def fullRankDiagnostics: OlsDiagnostics =
    val report = RankDiagnostics.fromPivotedQr(
      predictorCount = 2,
      rank = 2,
      tolerance = 1e-7,
      pivotOrder = Vector(0, 1),
      diagonalR = Vector(2.0, 1.0),
      toleranceConvention = scalafim.fmri.design.RankToleranceConvention.ScaleAware
    )
    OlsDiagnostics(
      solveMethod = OlsSolveMethod.QrRankRevealing,
      predictors = 2,
      rank = 2,
      policy = OlsSolvePolicy.Default,
      rankReport = report
    )
