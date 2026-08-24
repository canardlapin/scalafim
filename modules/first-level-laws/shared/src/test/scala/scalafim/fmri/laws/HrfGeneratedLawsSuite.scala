package scalafim.fmri.laws

import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.HrfCombinators.*
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.hrf.laws.HrfLaws
import scalafim.fmri.hrf.regressor.{Regressor, evaluate}

class HrfGeneratedLawsSuite extends GeneratedLawSuite:

  property("generated kernels enforce causality and declared compact support"):
    forAll(FirstLevelGenerators.kernelCase) { generated =>
      val failures = HrfLaws.causality(generated.hrf) ++ HrfLaws.support(generated.hrf)
      Prop(failures.isEmpty) :| failures.map(_.message).mkString("\n")
    }

  property("generated impulse and epoch responses are additive, homogeneous, and translation equivariant"):
    forAll(FirstLevelGenerators.kernelCase, FirstLevelGenerators.stimulusCase) { (generated, stimulus) =>
      val hrf = generated.hrf
      val grid = evaluationGrid(stimulus, hrf)
      val complete = render(hrf, stimulus.onsets, stimulus.durations, stimulus.amplitudes, grid)
      val leftIndices = stimulus.onsets.indices.filter(_ % 2 == 0)
      val rightIndices = stimulus.onsets.indices.filter(_ % 2 == 1)
      val left = render(
        hrf,
        leftIndices.map(stimulus.onsets).toVector,
        leftIndices.map(stimulus.durations).toVector,
        leftIndices.map(stimulus.amplitudes).toVector,
        grid
      )
      val right = render(
        hrf,
        rightIndices.map(stimulus.onsets).toVector,
        rightIndices.map(stimulus.durations).toVector,
        rightIndices.map(stimulus.amplitudes).toVector,
        grid
      )
      val sum = combine(left, right)(_ + _)
      val scaled = render(
        hrf,
        stimulus.onsets,
        stimulus.durations,
        stimulus.amplitudes.map(_ * stimulus.scale),
        grid
      )
      val expectedScaled = map(complete)(_ * stimulus.scale)
      val shifted = render(
        hrf,
        stimulus.onsets.map(_ + stimulus.shift),
        stimulus.durations,
        stimulus.amplitudes,
        grid.map(_ + stimulus.shift)
      )
      val evidence = NumericalEvidence(
        NumericalOperation.LinearCombination,
        scale = Vector(peak(complete), peak(scaled), peak(shifted)).max,
        conditionEstimate = 1.0,
        primitiveOperations = math.max(1, grid.length * hrf.nbasis * stimulus.onsets.length)
      )
      val additiveGap = maxAbsDiff(complete, sum)
      val homogeneousGap = maxAbsDiff(scaled, expectedScaled)
      val translationGap = maxAbsDiff(complete, shifted)
      Prop(
        additiveGap <= evidence.absolute &&
          homogeneousGap <= evidence.absolute &&
          translationGap <= evidence.absolute
      ) :| s"kernel=${hrf.name} impulse=${stimulus.isImpulse} epoch=${stimulus.isEpoch} " +
        s"additive=$additiveGap homogeneous=$homogeneousGap translation=$translationGap " +
        s"bound=${evidence.absolute}"
    }

  property("generated pulse partitions agree and numerical quadrature converges under refinement"):
    forAll(
      FirstLevelGenerators.kernelCase,
      FirstLevelGenerators.epochDuration
    ) { (generated, generatedDuration) =>
      val hrf = generated.hrf
      val duration = generatedDuration.value
      val grid = Vector.tabulate(80)(index => index * 0.5)
      val steps = Vector(0.2, 0.1, 0.05, 0.025)
      val whole = Evaluate.doubles(hrf, grid, duration = duration, integration = Integration.Exact)
      val firstHalf = Evaluate.doubles(hrf, grid, duration = duration / 2.0, integration = Integration.Exact)
      val secondHalf = Evaluate.doubles(
        hrf,
        grid.map(_ - duration / 2.0),
        duration = duration / 2.0,
        integration = Integration.Exact
      )
      val partitioned = combine(firstHalf, secondHalf)(_ + _)
      val partitionGap = maxAbsDiff(whole, partitioned)
      val evidence = NumericalEvidence(
        NumericalOperation.Quadrature,
        scale = math.max(peak(whole), peak(partitioned)),
        conditionEstimate = 1.0,
        primitiveOperations = grid.length * hrf.nbasis * math.max(1, math.ceil(duration / 0.025).toInt)
      )
      val convergence = HrfLaws.quadratureConvergence(
        hrf,
        grid,
        duration = duration,
        steps = steps
      )
      Prop(partitionGap <= evidence.absolute && convergence.isEmpty) :|
        s"kernel=${hrf.name} duration=$duration partitionGap=$partitionGap " +
        s"bound=${evidence.absolute} " +
        convergence.map(_.message).mkString("; ")
    }

  property("generated basis reconstruction commutes with event rendering"):
    forAll(FirstLevelGenerators.kernelCase, FirstLevelGenerators.stimulusCase) { (generated, stimulus) =>
      val basis = generated.hrf
      val coefficients =
        Vector.tabulate(basis.nbasis)(index => stimulus.amplitudes(index % stimulus.amplitudes.length) / (index + 1.0))
      val grid = evaluationGrid(stimulus, basis)
      val evidence = NumericalEvidence(
        NumericalOperation.LinearCombination,
        scale = coefficients.map(math.abs).maxOption.getOrElse(1.0),
        conditionEstimate = basis.nbasis.toDouble,
        primitiveOperations = grid.length * basis.nbasis * stimulus.onsets.length
      )
      val failures = HrfLaws.reconstructionCommutes(
        basis,
        coefficients,
        stimulus.onsets,
        grid,
        tol = evidence.absolute
      )
      Prop(failures.isEmpty) :| failures.map(_.message).mkString("\n")
    }

  property("basis permutations transport coefficients without changing the reconstructed response"):
    forAll(FirstLevelGenerators.kernelCase, FirstLevelGenerators.stimulusCase) { (generated, stimulus) =>
      val source = generated.hrf
      val sourceBasis = ResponseBasis.of(source)
      val raw = Vector.tabulate(source.nbasis)(index => stimulus.amplitudes(index % stimulus.amplitudes.length))
      val sourceCoefficients = sourceBasis.coefficients(raw)
      val order = (0 until source.nbasis).reverse.toVector
      val transform = BasisTransform.Permutation(order)
      val permuted = Hrf.multi(
        name = s"${source.name}-generated-permutation",
        nbasis = source.nbasis,
        span = source.span,
        support = source.support
      ) { lag =>
        val values = source(lag).data
        order.map(values).toArray
      }
      val moved =
        for
          coefficients <- sourceCoefficients
          transported <- transform.transportCoefficients(coefficients)
          retagged <- ResponseBasis.of(permuted).coefficients(transported.values)
        yield (sourceBasis.reconstruct(coefficients), ResponseBasis.of(permuted).reconstruct(retagged))
      moved match
        case Left(error)                  => Prop.falsified :| error.message
        case Right((original, reordered)) =>
          val lags = Vector.tabulate(math.ceil(source.span.value * 2.0).toInt + 1)(index => index * 0.5)
          val actual = lags.map(lag => reordered(Lag(lag)).data(0))
          val expected = lags.map(lag => original(Lag(lag)).data(0))
          val evidence = NumericalEvidence(
            NumericalOperation.BasisTransport,
            scale = (actual ++ expected).map(math.abs).maxOption.getOrElse(1.0),
            conditionEstimate = source.nbasis.toDouble,
            primitiveOperations = lags.length * source.nbasis
          )
          val gap = actual.zip(expected).map((left, right) => math.abs(left - right)).maxOption.getOrElse(0.0)
          Prop(gap <= evidence.absolute) :|
            s"kernel=${source.name} order=$order gap=$gap bound=${evidence.absolute}"
    }

  property("peak normalization is numerically idempotent and reports a transport each time"):
    forAll(FirstLevelGenerators.kernelCase) { generated =>
      val source = generated.hrf
      val first = source.normalizeWithTransform(Seconds(0.05))
      val second = first.basis.normalizeWithTransform(Seconds(0.05))
      val lags = Vector.tabulate(math.ceil(source.span.value * 4.0).toInt + 1)(index => index * 0.25)
      val firstValues = first.basis.evalDoubles(lags)
      val secondValues = second.basis.evalDoubles(lags)
      val evidence = NumericalEvidence(
        NumericalOperation.BasisTransport,
        scale = math.max(peak(firstValues), peak(secondValues)),
        conditionEstimate = source.nbasis.toDouble,
        primitiveOperations = lags.length * source.nbasis * 2
      )
      val gap = maxAbsDiff(firstValues, secondValues)
      val dimensionsAgree = first.transform.dimension == source.nbasis && second.transform.dimension == source.nbasis
      Prop(dimensionsAgree && gap <= evidence.absolute) :|
        s"kernel=${source.name} dimensions=${first.transform.dimension}/${second.transform.dimension} " +
        s"gap=$gap bound=${evidence.absolute}"
    }

  private def evaluationGrid(stimulus: StimulusCase, hrf: Hrf): Vector[Double] =
    val end = stimulus.onsets.last + stimulus.durations.max + hrf.span.value + 2.0
    Vector.tabulate(math.ceil(end).toInt + 1)(_.toDouble)

  private def render(
      hrf: Hrf,
      onsets: Vector[Double],
      durations: Vector[Double],
      amplitudes: Vector[Double],
      grid: Vector[Double],
      precision: Double = 0.05
  ): Mat =
    Regressor(onsets, hrf, duration = durations, amplitude = amplitudes)
      .evaluate(grid, precision = precision, method = Regressor.EvalMethod.Loop)

  private def peak(matrix: Mat): Double =
    matrix.data.iterator.map(math.abs).maxOption.getOrElse(0.0)

  private def maxAbsDiff(left: Mat, right: Mat): Double =
    require(left.rows == right.rows && left.cols == right.cols, "law matrices must have matching shapes")
    left.data.iterator.zip(right.data.iterator).map((a, b) => math.abs(a - b)).maxOption.getOrElse(0.0)

  private def map(matrix: Mat)(f: Double => Double): Mat =
    Mat.unsafe(matrix.rows, matrix.cols, matrix.data.map(f))

  private def combine(left: Mat, right: Mat)(f: (Double, Double) => Double): Mat =
    require(left.rows == right.rows && left.cols == right.cols, "law matrices must have matching shapes")
    Mat.unsafe(left.rows, left.cols, left.data.zip(right.data).map { case (a, b) => f(a, b) })
