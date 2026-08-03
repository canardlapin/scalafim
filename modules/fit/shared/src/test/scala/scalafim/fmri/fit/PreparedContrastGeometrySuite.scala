package scalafim.fmri.fit

import gale.backend.Backend.given
import gale.linalg.{DMat, Matrix}
import scalafim.fmri.ar.{ArmaCoefficients, WhiteningMethod, WhiteningPlan, WhiteningTransform}
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig}

class PreparedContrastGeometrySuite extends munit.FunSuite:

  private val designRows = Vector(
    Vector(1.0, -1.0, -1.0),
    Vector(1.0, -1.0, -0.7),
    Vector(1.0, 1.0, -0.4),
    Vector(1.0, 1.0, -0.1),
    Vector(1.0, -1.0, 0.1),
    Vector(1.0, -1.0, 0.4),
    Vector(1.0, 1.0, 0.7),
    Vector(1.0, 1.0, 1.0)
  )

  private val responseRows = Vector(
    Vector(-0.9, 0.1, -0.15),
    Vector(-1.14, 0.5, -0.09),
    Vector(0.91, -0.85, 0.21),
    Vector(0.67, -0.55, 0.47),
    Vector(-1.02, 0.75, -0.47),
    Vector(-0.56, 0.65, -0.56),
    Vector(0.99, -0.2, 0.49),
    Vector(1.05, -0.4, 0.1)
  )

  private val columnNames = Vector("intercept", "task", "drift")
  private val contrast = TContrast("task", Map("task" -> 1.0))

  test("moment geometry equals explicit dense effect and residual projectors"):
    val geometry = iidGeometry(designRows, columnNames, contrast, nuisanceRank = 2)
    val response = responseBlock(responseRows)
    val preparedResponse = geometry.prepareResponse(response).toOption.get.value
    val (effect, residual) = moments(geometry, preparedResponse)

    val x = geometry.preparedDesign.value
    val effectProjector = x * geometry.effectBasis * geometry.effectBasis.t * x.t
    val residualProjector = subtract(Matrix.eye(x.rows), x * geometry.inverseXtX * x.t)
    val denseEffect = preparedResponse.t * effectProjector * preparedResponse
    val denseResidual = preparedResponse.t * residualProjector * preparedResponse

    assertMatrixClose(effect, denseEffect, 1e-11)
    assertMatrixClose(residual, denseResidual, 1e-11)
    assertEquals(geometry.receipt.residualDegreesOfFreedom.value, 5)
    assertEquals(geometry.receipt.nuisanceRank.value, 2)

  test("rescaling a one-row contrast preserves the effect matrix"):
    val positive = iidGeometry(designRows, columnNames, contrast, nuisanceRank = 2)
    val negative = iidGeometry(
      designRows,
      columnNames,
      TContrast("negative task basis", Map("task" -> -3.0)),
      nuisanceRank = 2
    )
    val response = responseBlock(responseRows).value
    val positiveEffect = moments(positive, response)._1
    val negativeEffect = moments(negative, response)._1

    assertMatrixClose(positiveEffect, negativeEffect, 1e-11)
    assertEqualsDouble(positive.contrastVariance * 9.0, negative.contrastVariance, 1e-12)

  test("invertible nuisance basis changes preserve effect and residual geometry"):
    val changedRows = designRows.map: row =>
      val intercept = row(0)
      val task = row(1)
      val drift = row(2)
      Vector(intercept + 2.0 * drift, task, -intercept + drift)
    val original = iidGeometry(designRows, columnNames, contrast, nuisanceRank = 2)
    val changed = iidGeometry(changedRows, columnNames, contrast, nuisanceRank = 2)
    val response = responseBlock(responseRows).value
    val (originalEffect, originalResidual) = moments(original, response)
    val (changedEffect, changedResidual) = moments(changed, response)

    assertMatrixClose(changedEffect, originalEffect, 1e-10)
    assertMatrixClose(changedResidual, originalResidual, 1e-10)

  test("fixed AR whitening and censor resets match the first-level transform"):
    val design = designMatrix(designRows)
    val response = responseBlock(responseRows)
    val partitions = runPartitions(design.timepoints)
    val options = ArOptions(
      structure = ArStructure.Ar(1),
      censoredTimepoints = Vector(3),
      rho = Some(0.3)
    )
    val segments = Gls.timeSegments(partitions, options.censoredTimepoints).toOption.get
    val whiteningPlan = WhiteningPlan.global(
      ArmaCoefficients(Vector(0.3)),
      segments,
      exactFirstAr1 = true,
      method = WhiteningMethod.Fixed
    )
    val preparation = ResponsePreparationPlan.fromConfig(FitConfig(autocorrelation = options))
    val geometry = preparation
      .prepareContrast(
        design = design,
        columnNames = columnNames,
        contrast = contrast,
        selectedTimepoints = selectedTimepoints(design.timepoints),
        partitions = partitions,
        nuisanceRank = TemporalNuisanceRank.unsafe(2),
        whitening = CanonicalTemporalWhitening.Shared(whiteningPlan)
      )
      .toOption
      .get
    val expected = WhiteningTransform(whiteningPlan, design.value, response.value).toOption.get
    val actualResponse = geometry.prepareResponse(response).toOption.get

    assertMatrixClose(geometry.preparedDesign.value, expected.design, 0.0)
    assertMatrixClose(actualResponse.value, expected.response, 0.0)
    assertEquals(
      geometry.receipt.whitening,
      TemporalWhiteningReceipt.Shared(WhiteningMethod.Fixed, scalafim.fmri.ar.NoisePooling.Global, 1, 2, true)
    )
    assertEquals(geometry.receipt.provenance.deferred, Vector.empty)

  test("rank-deficient prepared designs are rejected"):
    val singularRows = designRows.map(row => Vector(row(0), row(1), row(1)))
    val result = ResponsePreparationPlan
      .fromConfig(FitConfig())
      .prepareContrast(
        design = designMatrix(singularRows),
        columnNames = columnNames,
        contrast = contrast,
        selectedTimepoints = selectedTimepoints(singularRows.length),
        partitions = runPartitions(singularRows.length),
        nuisanceRank = TemporalNuisanceRank.unsafe(2)
      )

    assert(result.left.toOption.exists(_.isInstanceOf[FitError.SingularDesign]))

  test("held-out responses cannot alter a training-scoped whitening geometry"):
    val design = designMatrix(designRows)
    val partitions = runPartitions(design.timepoints)
    val options = ArOptions(structure = ArStructure.Ar(1), global = true)
    val segments = Gls.timeSegments(partitions, Vector.empty).toOption.get
    val learnedPlan = WhiteningPlan.global(
      ArmaCoefficients(Vector(0.25)),
      segments,
      exactFirstAr1 = true,
      method = WhiteningMethod.Estimated
    )
    val training = TrainingRunScope.fromInts(Vector(0, 1)).toOption.get
    val geometry = ResponsePreparationPlan
      .fromConfig(FitConfig(autocorrelation = options))
      .prepareContrast(
        design = design,
        columnNames = columnNames,
        contrast = contrast,
        selectedTimepoints = selectedTimepoints(design.timepoints),
        partitions = partitions,
        nuisanceRank = TemporalNuisanceRank.unsafe(2),
        scope = TemporalPreparationScope.TrainingFold(training),
        whitening = CanonicalTemporalWhitening.Shared(learnedPlan)
      )
      .toOption
      .get

    val beforeXtx = matrixData(geometry.designCrossproduct)
    val beforeBasis = matrixData(geometry.effectBasis)
    val ordinary = geometry.prepareResponse(responseBlock(responseRows)).toOption.get
    val perturbed = geometry.prepareResponse(responseBlock(responseRows.map(_.map(_ * 1000.0)))).toOption.get

    assertEquals(matrixData(geometry.designCrossproduct), beforeXtx)
    assertEquals(matrixData(geometry.effectBasis), beforeBasis)
    assertEquals(geometry.receipt.scope, TemporalPreparationScope.TrainingFold(training))
    assertNotEquals(matrixData(ordinary.value), matrixData(perturbed.value))

  private def iidGeometry(
      rows: Vector[Vector[Double]],
      names: Vector[String],
      target: TContrast,
      nuisanceRank: Int
  ): PreparedContrastGeometry =
    val design = designMatrix(rows)
    ResponsePreparationPlan
      .fromConfig(FitConfig())
      .prepareContrast(
        design = design,
        columnNames = names,
        contrast = target,
        selectedTimepoints = selectedTimepoints(design.timepoints),
        partitions = runPartitions(design.timepoints),
        nuisanceRank = TemporalNuisanceRank.unsafe(nuisanceRank)
      )
      .toOption
      .get

  private def moments(geometry: PreparedContrastGeometry, response: DMat): (DMat, DMat) =
    val cross = response.t * geometry.preparedDesign.value
    val score = cross * geometry.effectBasis
    val effect = score * score.t
    val total = response.t * response
    val residual = subtract(total, cross * geometry.inverseXtX * cross.t)
    effect -> residual

  private def designMatrix(rows: Vector[Vector[Double]]): DesignMatrix =
    DesignMatrix.unsafe(GaleTestMatrix.fromRows(rows))

  private def responseBlock(rows: Vector[Vector[Double]]): ResponseBlock =
    ResponseBlock.unsafe(GaleTestMatrix.fromRows(rows))

  private def selectedTimepoints(size: Int): SelectedTimepointIndices =
    SelectedTimepointIndices.unsafe((0 until size).toVector)

  private def runPartitions(size: Int): Vector[RunPartition] =
    Vector(RunPartition(0, (0 until size).toVector, (0 until size).toVector))

  private def subtract(left: DMat, right: DMat): DMat =
    val out = Matrix.newBuilder(left.rows, left.cols)
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        out(row, col) = left(row, col) - right(row, col)
        col += 1
      row += 1
    out.result()

  private def matrixData(matrix: DMat): Vector[Double] =
    matrix.valuesRowMajor.toVector

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1
