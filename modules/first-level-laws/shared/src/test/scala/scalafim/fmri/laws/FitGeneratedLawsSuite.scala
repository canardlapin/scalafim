package scalafim.fmri.laws

import gale.linalg.{DMat, Matrix}
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, TimeSegments, WhiteningPlan, WhiteningTransform}
import scalafim.fmri.design.{CoefficientAxis, DesignSchema, ModelSource}
import scalafim.fmri.fit.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig, FixedWeightAlignment, VolumeWeighting}

class FitGeneratedLawsSuite extends GeneratedLawSuite:

  property("generated full-rank multiresponse systems recover their planted coefficients"):
    forAll(FirstLevelGenerators.linearCase) { generated =>
      fit(generated.designRows, generated.responseRows) match
        case Left(error)   => Prop.falsified :| error.message
        case Right(result) =>
          val expected = coefficientMatrix(generated.coefficients)
          val evidence = factorizationEvidence(generated)
          val gap = maxAbsDiff(result.coefficients.value, expected)
          Prop(
            result.diagnostics.fullRank &&
              result.voxels == generated.responses &&
              gap <= evidence.absolute
          ) :| s"rows=${generated.rows} predictors=${generated.predictors} responses=${generated.responses} " +
            s"rank=${result.diagnostics.rank} gap=$gap ${describe(evidence)}"
    }

  property("column scaling transports coefficients and preserves fitted geometry"):
    forAll(FirstLevelGenerators.linearCase) { generated =>
      val factors = Vector.tabulate(generated.predictors)(index => math.pow(2.0, (index % 5) - 2.0))
      val transformedRows = generated.designRows.map(row => row.zip(factors).map(_ / _))
      val result =
        for
          original <- fit(generated.designRows, generated.responseRows)
          transformed <- fit(transformedRows, generated.responseRows)
        yield (original, transformed)
      result match
        case Left(error)                    => Prop.falsified :| error.message
        case Right((original, transformed)) =>
          val expectedMoved = coefficientMatrix(
            generated.coefficients.zip(factors).map { case (row, factor) => row.map(_ * factor) }
          )
          val originalFitted = multiply(dmat(generated.designRows), original.coefficients.value)
          val movedFitted = multiply(dmat(transformedRows), transformed.coefficients.value)
          val evidence = NumericalEvidence(
            NumericalOperation.QrFactorization,
            scale = generated.scaleEvidence * factors.max,
            conditionEstimate = generated.conditionEvidence * factors.max / factors.min,
            primitiveOperations = generated.rows * generated.predictors * generated.responses
          )
          val coefficientGap = maxAbsDiff(transformed.coefficients.value, expectedMoved)
          val fittedGap = maxAbsDiff(originalFitted, movedFitted)
          Prop(coefficientGap <= evidence.absolute && fittedGap <= evidence.absolute) :|
            s"factors=$factors coefficientGap=$coefficientGap fittedGap=$fittedGap ${describe(evidence)}"
    }

  property("public fixed WLS preparation is the declared square-root-weight transformation"):
    forAll(FirstLevelGenerators.weightedLinearCase) { generated =>
      val system = generated.system
      val plan = ResponsePreparationPlan.fromConfig(
        FitConfig(
          volumeWeighting = VolumeWeighting.Fixed(
            generated.weights,
            FixedWeightAlignment.SelectedRows
          )
        )
      )
      val preparedInput =
        for
          design <- DesignMatrix.fromMatrix(dmat(system.designRows))
          response <- ResponseBlock.fromMatrix(dmat(system.responseRows))
          input = FitBlockInput(
            design = design,
            response = response,
            voxelIndices = (0 until system.responses).toVector,
            timepoints = (0 until system.rows).toVector,
            partitions = Vector(RunPartition(0, (0 until system.rows).toVector, (0 until system.rows).toVector))
          )
          prepared <- plan.prepare(input)
        yield prepared
      preparedInput match
        case Left(error)     => Prop.falsified :| error.message
        case Right(prepared) =>
          val manualDesign = scaleRows(dmat(system.designRows), generated.squareRootWeights)
          val manualResponse = scaleRows(dmat(system.responseRows), generated.squareRootWeights)
          val fitted = Ols.fit(prepared.input.design, prepared.input.response)
          val evidence = NumericalEvidence(
            NumericalOperation.WeightedTransformation,
            scale = system.scaleEvidence * generated.squareRootWeights.max,
            conditionEstimate = system.conditionEvidence,
            primitiveOperations = system.rows * (system.predictors + system.responses)
          )
          val designGap = maxAbsDiff(prepared.input.design.value, manualDesign)
          val responseGap = maxAbsDiff(prepared.input.response.value, manualResponse)
          val receipt = prepared.provenance.volumeWeighting
          fitted match
            case Left(error)  => Prop.falsified :| error.message
            case Right(value) =>
              val coefficientGap = maxAbsDiff(value.coefficients.value, coefficientMatrix(system.coefficients))
              Prop(
                designGap <= evidence.absolute &&
                  responseGap <= evidence.absolute &&
                  coefficientGap <= evidence.absolute &&
                  receipt.exists(_.weights == generated.weights)
              ) :| s"designGap=$designGap responseGap=$responseGap coefficientGap=$coefficientGap " +
                s"receipt=${receipt.map(_.weights)} ${describe(evidence)}"
    }

  property("multiresponse fitting is invariant to response-column chunking"):
    forAll(FirstLevelGenerators.linearCase) { generated =>
      val compared =
        for
          design <- DesignMatrix.fromMatrix(dmat(generated.designRows))
          prepared <- Ols.prepare(design)
          response <- ResponseBlock.fromMatrix(dmat(generated.responseRows))
          whole <- prepared.fit(response)
          width = math.max(1, generated.responses / 2)
          chunks = (0 until generated.responses).grouped(width).map(_.toVector).toVector
          parts <- chunks.foldLeft[Either[FitError, Vector[OlsFit]]](Right(Vector.empty)) {
            case (Left(error), _)         => Left(error)
            case (Right(values), columns) =>
              val rows = generated.responseRows.map(row => columns.map(row))
              ResponseBlock
                .fromMatrix(dmat(rows))
                .flatMap(prepared.fit)
                .map(values :+ _)
          }
        yield (whole, parts, chunks)
      compared match
        case Left(error)                   => Prop.falsified :| error.message
        case Right((whole, parts, chunks)) =>
          val joined = joinColumns(parts.map(_.coefficients.value))
          val evidence = factorizationEvidence(generated)
          val gap = maxAbsDiff(whole.coefficients.value, joined)
          val variances = parts.flatMap(part => vectorValues(part.residualVariance))
          val varianceGap =
            vectorValues(whole.residualVariance).zip(variances).map((a, b) => math.abs(a - b)).maxOption.getOrElse(0.0)
          Prop(gap <= evidence.absolute && varianceGap <= evidence.absolute) :|
            s"chunks=${chunks.map(_.size)} coefficientGap=$gap varianceGap=$varianceGap ${describe(evidence)}"
    }

  property("known AR(1) GLS equals independently whitened OLS and resets at censor gaps"):
    forAll(FirstLevelGenerators.censorCase) { generated =>
      val timepoints = generated.selectedTimepoints
      val mean = timepoints.sum.toDouble / timepoints.length
      val scale = math.max(1.0, timepoints.map(value => math.abs(value - mean)).max)
      val designRows = timepoints.map(value => Vector(1.0, (value - mean) / scale))
      val responseRows = designRows.zipWithIndex.map { case (row, index) =>
        Vector(1.25 * row(0) - 0.75 * row(1) + 0.05 * math.sin(index * 1.7))
      }
      val rho = 0.35
      val segments = expectedSegments(generated.partitions)
      val plan = WhiteningPlan.global(ArmaCoefficients.ar(rho), segments, exactFirstAr1 = true)
      val compared =
        for
          design <- DesignMatrix.fromMatrix(dmat(designRows))
          response <- ResponseBlock.fromMatrix(dmat(responseRows))
          whitened <- WhiteningTransform(plan, design.value, response.value).left.map(error =>
            FitError.UnsupportedAutocorrelation(error.message)
          )
          whitenedDesign <- DesignMatrix.fromMatrix(whitened.design)
          whitenedResponse <- ResponseBlock.fromMatrix(whitened.response)
          reference <- Ols.fit(whitenedDesign, whitenedResponse)
          gls <- Gls.fit(
            design,
            response,
            generated.partitions,
            ArOptions(structure = ArStructure.Ar(1), rho = Some(rho))
          )
        yield (gls, reference)
      compared match
        case Left(error)             => Prop.falsified :| error.message
        case Right((gls, reference)) =>
          val evidence = NumericalEvidence(
            NumericalOperation.WeightedTransformation,
            scale = responseRows.iterator.flatten.map(math.abs).maxOption.getOrElse(1.0),
            conditionEstimate = (1.0 + math.abs(rho)) / (1.0 - math.abs(rho)),
            primitiveOperations = timepoints.length * 4
          )
          val coefficientGap = maxAbsDiff(gls.coefficients.value, reference.coefficients.value)
          val covarianceGap = maxAbsDiff(gls.normalizedCovariance, reference.normalizedCovariance)
          val actualSegments = gls.diagnostics.whitening.segments.map(segment =>
            (segment.runIndex, segment.startRow, segment.endRowExclusive)
          )
          val expectedSegmentCoordinates =
            segments.map(segment => (segment.runIndex, segment.start, segment.endExclusive))
          val reportedCensors = gls.diagnostics.whitening.censorGaps.flatMap(_.timepoints).toSet
          Prop(
            coefficientGap <= evidence.absolute &&
              covarianceGap <= evidence.absolute &&
              actualSegments == expectedSegmentCoordinates &&
              reportedCensors == generated.censoredTimepoints.toSet
          ) :| s"coefficientGap=$coefficientGap covarianceGap=$covarianceGap " +
            s"segments=$actualSegments expected=$expectedSegmentCoordinates " +
            s"censors=$reportedCensors expectedCensors=${generated.censoredTimepoints.toSet} ${describe(evidence)}"
    }

  property("fixed-effects sufficient-statistics combination is associative"):
    forAll(FirstLevelGenerators.linearCase) { generated =>
      val compared =
        for
          axis <- coefficientAxis(generated)
          residualDf <- ResidualDegreesOfFreedom(generated.rows - generated.predictors).left.map(_.message)
          contributions = Vector.tabulate(3)(run =>
            contribution(generated, run, precisionScale = run + 1.0, residualDf = residualDf)
          )
          parts = contributions.map { value =>
            FixedEffectsSufficientStatistics(
              coefficientAxis = axis,
              voxelIndices = (0 until generated.responses).toVector,
              contributions = Vector(value)
            )
          }
          left <- parts(0).combine(parts(1)).flatMap(_.combine(parts(2))).left.map(_.message)
          right <- parts(1).combine(parts(2)).flatMap(parts(0).combine).left.map(_.message)
        yield (left, right)
      compared match
        case Left(error)   => Prop.falsified :| error
        case Right((a, b)) =>
          val sameRuns = a.runIndices == b.runIndices
          val sameDf = a.effectiveResidualDegreesOfFreedom == b.effectiveResidualDegreesOfFreedom
          val sameStatistics = a.contributions.zip(b.contributions).forall { case (x, y) =>
            x.runIndex == y.runIndex &&
            maxAbsDiff(x.precisionWeightedCoefficients, y.precisionWeightedCoefficients) == 0.0 &&
            x.precisionByVoxel.zip(y.precisionByVoxel).forall((p, q) => maxAbsDiff(p, q) == 0.0)
          }
          Prop(sameRuns && sameDf && sameStatistics) :|
            s"left=${a.runIndices}/${a.effectiveResidualDegreesOfFreedom} " +
            s"right=${b.runIndices}/${b.effectiveResidualDegreesOfFreedom} statistics=$sameStatistics"
    }

  property("rank-deficient systems compare fitted geometry and estimable functions, never arbitrary betas"):
    forAll(FirstLevelGenerators.rankDeficientCase) { generated =>
      val rows = generated.designRows
      val betaA = Vector(0.75, -1.25, 2.0)
      val betaB = Vector(betaA(0), betaA(1) + betaA(2), 0.0)
      val fittedA = rows.map(row => row.zip(betaA).map(_ * _).sum)
      val fittedB = rows.map(row => row.zip(betaB).map(_ * _).sum)
      val evidence = NumericalEvidence(
        NumericalOperation.LinearCombination,
        scale = (fittedA ++ fittedB).map(math.abs).maxOption.getOrElse(1.0),
        conditionEstimate = 1.0,
        primitiveOperations = generated.rows * 3
      )
      val fittedGap = fittedA.zip(fittedB).map((a, b) => math.abs(a - b)).max
      val estimableA = betaA(1) + betaA(2)
      val estimableB = betaB(1) + betaB(2)
      val rejection = DesignMatrix.fromMatrix(dmat(rows)).flatMap(design => Ols.prepare(design))
      val typedRankRejection = rejection match
        case Left(FitError.RankDeficientDesign(report)) => report.deficient && report.numericalRank < 3
        case _                                          => false
      Prop(
        typedRankRejection &&
          fittedGap <= evidence.absolute &&
          evidence.close(estimableA, estimableB) &&
          betaA != betaB
      ) :| s"typedRejection=$typedRankRejection fittedGap=$fittedGap " +
        s"estimable=$estimableA/$estimableB betaA=$betaA betaB=$betaB ${describe(evidence)}"
    }

  property("scale-aware rank decisions are invariant to global matrix scale"):
    forAll(FirstLevelGenerators.rankStressCase) { generated =>
      val scaled = rankState(generated.designRows)
      val normalized = rankState(generated.designRows.map(_.map(_ / generated.scale)))
      Prop(scaled == normalized) :|
        s"rows=${generated.rows} scale=${generated.scale} perturbation=${generated.perturbation} " +
        s"scaled=$scaled normalized=$normalized"
    }

  private enum RankState:
    case Full(rank: Int)
    case Deficient(rank: Int)

  private def rankState(rows: Vector[Vector[Double]]): Either[String, RankState] =
    DesignMatrix.fromMatrix(dmat(rows)).flatMap(design => Ols.prepare(design)) match
      case Right(prepared)                            => Right(RankState.Full(prepared.diagnostics.rank))
      case Left(FitError.RankDeficientDesign(report)) => Right(RankState.Deficient(report.numericalRank))
      case Left(error)                                => Left(error.message)

  private def fit(
      designRows: Vector[Vector[Double]],
      responseRows: Vector[Vector[Double]]
  ): Either[FitError, OlsFit] =
    for
      design <- DesignMatrix.fromMatrix(dmat(designRows))
      response <- ResponseBlock.fromMatrix(dmat(responseRows))
      fitted <- Ols.fit(design, response)
    yield fitted

  private def dmat(rows: Vector[Vector[Double]]): DMat =
    require(rows.nonEmpty && rows.head.nonEmpty, "law matrices must be non-empty")
    require(rows.forall(_.length == rows.head.length), "law matrix rows must have equal width")
    val out = Matrix.newBuilder(rows.length, rows.head.length)
    var row = 0
    while row < rows.length do
      var column = 0
      while column < rows.head.length do
        out(row, column) = rows(row)(column)
        column += 1
      row += 1
    out.result()

  private def coefficientMatrix(coefficients: Vector[Vector[Double]]): DMat =
    dmat(coefficients)

  private def multiply(left: DMat, right: DMat): DMat =
    left * right

  private def scaleRows(matrix: DMat, scales: Vector[Double]): DMat =
    require(matrix.rows == scales.length, "row scales must align with matrix")
    val out = Matrix.newBuilder(matrix.rows, matrix.cols)
    var row = 0
    while row < matrix.rows do
      var column = 0
      while column < matrix.cols do
        out(row, column) = matrix(row, column) * scales(row)
        column += 1
      row += 1
    out.result()

  private def joinColumns(parts: Vector[DMat]): DMat =
    require(parts.nonEmpty && parts.map(_.rows).distinct.length == 1, "chunks must share coefficient rows")
    val rows = parts.head.rows
    val columns = parts.map(_.cols).sum
    val out = Matrix.newBuilder(rows, columns)
    var row = 0
    while row < rows do
      var offset = 0
      parts.foreach { part =>
        var column = 0
        while column < part.cols do
          out(row, offset + column) = part(row, column)
          column += 1
        offset += part.cols
      }
      row += 1
    out.result()

  private def expectedSegments(partitions: Vector[RunPartition]): Vector[TimeSegment] =
    val base = partitions.map(partition =>
      TimeSegment(partition.rowIndices.head, partition.rowIndices.last + 1, partition.runIndex)
    )
    val resets = partitions.iterator.flatMap { partition =>
      partition.timepoints.zip(partition.rowIndices).sliding(2).collect {
        case Vector((leftTimepoint, leftRow), (rightTimepoint, _)) if rightTimepoint > leftTimepoint + 1 => leftRow
      }
    }.toSet
    TimeSegments.withCensorResets(base, resets)

  private def coefficientAxis(generated: LinearCase): Either[String, CoefficientAxis] =
    SamplingFrame
      .regular(1.0, generated.rows)
      .left
      .map(_.message)
      .map { sampling =>
        DesignSchema
          .legacy(
            Mat.fromRows(generated.designRows),
            sampling,
            Vector.tabulate(generated.predictors)(index => s"x${index + 1}"),
            ModelSource.Event
          )
          .coefficientAxis
      }

  private def contribution(
      generated: LinearCase,
      run: Int,
      precisionScale: Double,
      residualDf: ResidualDegreesOfFreedom
  ): FixedEffectsRunContribution =
    val predictors = generated.predictors
    val responses = generated.responses
    val precision = Vector.fill(responses)(diagonal(predictors, precisionScale))
    val weighted = Matrix.newBuilder(predictors, responses)
    var predictor = 0
    while predictor < predictors do
      var response = 0
      while response < responses do
        val shifted = generated.coefficients(predictor)(response) + run * 0.1
        weighted(predictor, response) = precisionScale * shifted
        response += 1
      predictor += 1
    FixedEffectsRunContribution(
      runIndex = run,
      rowCount = generated.rows,
      timepoints = (0 until generated.rows).toVector,
      residualDegreesOfFreedom = residualDf,
      precisionByVoxel = precision,
      precisionWeightedCoefficients = weighted.result()
    )

  private def diagonal(size: Int, value: Double): DMat =
    val out = Matrix.newBuilder(size, size)
    var row = 0
    while row < size do
      var column = 0
      while column < size do
        out(row, column) = if row == column then value else 0.0
        column += 1
      row += 1
    out.result()

  private def factorizationEvidence(generated: LinearCase): NumericalEvidence =
    NumericalEvidence(
      NumericalOperation.QrFactorization,
      scale = generated.scaleEvidence,
      conditionEstimate = generated.conditionEvidence,
      primitiveOperations = generated.rows * generated.predictors * generated.responses
    )

  private def vectorValues(vector: gale.linalg.DVec): Vector[Double] =
    Vector.tabulate(vector.length)(vector(_))

  private def maxAbsDiff(left: DMat, right: DMat): Double =
    if left.rows != right.rows || left.cols != right.cols then Double.PositiveInfinity
    else
      var maximum = 0.0
      var row = 0
      while row < left.rows do
        var column = 0
        while column < left.cols do
          maximum = math.max(maximum, math.abs(left(row, column) - right(row, column)))
          column += 1
        row += 1
      maximum

  private def describe(evidence: NumericalEvidence): String =
    s"bound=${evidence.absolute} scale=${evidence.scale} condition=${evidence.conditionEstimate} " +
      s"operations=${evidence.primitiveOperations}"
