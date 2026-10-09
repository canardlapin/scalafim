package scalafim.fmri.laws.profile

import scalafim.fmri.design.hrf.{KernelBasisCompilation, TrialDesignLowering}
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.family.{NormalizationRule, ShapeChart, ShapePoint}
import scalafim.fmri.model.ProfileHrfSource
import scalafim.scenarios.*

/** Frozen-shape feasibility experiment. The all-eight comparison is an offline oracle for reference availability, not a
  * legal runtime router. Shape truth never enters a decoder; this experiment does not execute a decoder.
  */
object TrialNeighborhoodAudit:
  val amplitudeTolerance = 1e-3
  // Separate exploratory absolute query target, not the Float32 conversion
  // tolerance and not a newly adopted scientific acceptance criterion.
  val queryTolerance = 1e-6
  val radii: Vector[Double] = Vector(0.0, 0.001, 0.003, 0.01, 0.03, 0.1, 0.25)
  final case class Point(label: String, unit: Vector[Double], anchor: Option[Int], radius: Double)
  final case class Record(
      layout: String,
      point: Point,
      reference: Int,
      nearestTwo: Boolean,
      preparedRelativeError: Double,
      originalRelativeError: Double,
      exactOriginalRelativeError: Double,
      preparedQueryError: Double,
      originalQueryError: Double,
      exactOriginalQueryError: Double,
      relativeTailDesignError: Double,
      relativeBasisDesignError: Double,
      preparedResidual: Double,
      inverseApplications: Long,
      corrections: Long,
      exactFactors: Long,
      retainedQueryTrials: Int
  )
  final case class Geometry(
      trials: Int,
      horizon: Double,
      rank: Int,
      bandwidth: Int,
      sharedBytes: Long,
      workerBytes: Long,
      references: Int,
      preparationSeconds: Double
  )
  final case class Coverage(
      layout: String,
      attempted: Int,
      nearestTwoAmplitudePasses: Int,
      anyReferenceAmplitudePasses: Int,
      anyReferenceOriginalAmplitudePasses: Int,
      anyReferencePreparedQueryPasses: Int,
      anyReferenceOriginalQueryPasses: Int,
      exactOriginalAmplitudePasses: Int,
      bestPreparedP95: Double,
      bestOriginalP95: Double
  )
  final case class Result(
      geometry: Geometry,
      records: Vector[Record],
      coverage: Vector[Coverage],
      scenarios: Vector[ScenarioResult]
  )

  def fixture(trials: Int, horizon: Double): DecodedTrialCheckpoint.Fixture =
    DecodedTrialCheckpoint.fixture(
      DecodedTrialCheckpoint.Config(
        DecodedTrialCheckpoint.Geometry.B0Dense,
        voxels = 1,
        trials = trials,
        blockSize = 1,
        compilation = KernelBasisCompilation.BlockedPartial(96),
        trialPreparation = TrialPreparationPolicy(TrialDesignLowering.Blocked(32)),
        horizonSeconds = Some(horizon),
        basisMaxRank = if horizon == 48.0 then 32 else 24
      )
    )

  def coordinates(f: DecodedTrialCheckpoint.Fixture, unit: Vector[Double]): Vector[Double] =
    val chart = f.plan.basis.family.chart
    unit.indices.map(i => chart.clamp(i, chart.lower(i) + unit(i) * chart.width(i))).toVector

  def onsets(f: DecodedTrialCheckpoint.Fixture): Vector[Double] = f.plan.source match
    case ProfileHrfSource.TrialEvents(_, drive, _, _) =>
      require(drive.schedule.durations.forall(_.value == 0.0), "impulse observation court only")
      drive.schedule.onsets.map(_.value)
    case _ => throw new IllegalArgumentException("trial fixture required")

  /** Analytic impulse response at the declared physical sample times. Full acquisition support; no sampled convolution,
    * tail cutoff or quadrature. Arithmetic certification is supplied separately by the interval court.
    */
  def originalDesign(f: DecodedTrialCheckpoint.Fixture, actual: Vector[Double]): Array[Double] =
    val events = onsets(f)
    val times = f.dataset.samplingFrame.samples().map(_.value)
    val lags = Array.tabulate(f.rows * f.trials)(i => times(i / f.trials) - events(i % f.trials))
    val out = new Array[Double](lags.length)
    f.plan.basis.family.evalInto(lags, ShapePoint.unsafe(actual), out)
    out

  def response(
      f: DecodedTrialCheckpoint.Fixture,
      design: Array[Double],
      noise: Double = 0.1,
      seed: Long = 2026100802L
  ): Array[Double] =
    val amplitudes = Vector.tabulate(f.trials)(j => Vector(1.2, -0.8, 0.4)(j % 3) + 0.25 * math.sin(0.71 * j + 0.31))
    val signal = Array.tabulate(f.rows)(t => (0 until f.trials).map(j => design(t * f.trials + j) * amplitudes(j)).sum)
    val rms = math.sqrt(signal.iterator.map(x => x * x).sum / signal.length)
    val rng = new scala.util.Random(seed)
    var ar = rng.nextGaussian()
    Array.tabulate(f.rows): t =>
      if t > 0 then ar = 0.3 * ar + math.sqrt(0.91) * rng.nextGaussian()
      signal(t) + 0.2 + 0.03 * f.baseline.designMatrix(t, 1) + noise * rms * ar

  def fresh(count: Int): Vector[Point] =
    val rng = new scala.util.Random(2026100801L)
    Vector.tabulate(count)(i => Point(s"fresh-$i", Vector.fill(3)(0.02 + 0.96 * rng.nextDouble()), None, -1.0))

  def rays(
      layout: String,
      selectedRadii: Vector[Double] = radii,
      nodes: Vector[Int] = (0 until 8).toVector
  ): Vector[Point] =
    nodes.flatMap: node =>
      val origin = Vector.tabulate(3)(i =>
        if (node & (1 << i)) == 0 then (if layout == "corners" then 0.0 else 0.25)
        else (if layout == "corners" then 1.0
              else 0.75)
      )
      selectedRadii.flatMap: radius =>
        (if radius == 0.0 then Vector(3) else Vector(0, 1, 2, 3)).map: direction =>
          val unit = origin.indices.map: i =>
            val delta =
              if direction == 3 || direction == i then radius * (if origin(i) < 0.5 then 1.0 else -1.0) else 0.0
            origin(i) + delta
          Point(s"ray-$node-$direction-$radius", unit.toVector, Some(node), radius)

  def grid(f: DecodedTrialCheckpoint.Fixture, layout: String): NodeGrid =
    require(layout == "corners" || layout == "quarters")
    val chart = f.plan.basis.family.chart
    val inset = if layout == "corners" then 0.0 else 0.25
    NodeGrid(
      ShapeChart(
        chart.names.indices.map(i =>
          (chart.names(i), chart.lower(i) + inset * chart.width(i), chart.upper(i) - inset * chart.width(i))
        )*
      ),
      Vector(2, 2, 2)
    )

  def run(
      trials: Int = 30,
      horizon: Double = 48.0,
      freshCount: Int = 64,
      selectedRadii: Vector[Double] = radii,
      nodes: Vector[Int] = (0 until 8).toVector,
      completed: String => Unit = _ => ()
  ): Result =
    val start = System.nanoTime()
    val f = fixture(trials, horizon)
    val outputs = f.prepare.flatMap(_.trialOutputs).fold(e => throw new IllegalArgumentException(e.message), identity)
    val axis = outputs.axis
    val preparation = axis.preparation
    val preparationSeconds = (System.nanoTime() - start) / 1e9
    val records = Vector.newBuilder[Record]
    var shared = 0L
    var worker = 0L
    Vector("corners", "quarters").foreach: layout =>
      val referenceGrid = grid(f, layout)
      val bank = preparation.objective(referenceGrid).fold(e => throw new IllegalArgumentException(e.message), identity)
      shared = math.max(shared, bank.estimatedSharedBytes)
      worker = math.max(worker, bank.estimatedWorkerBytes)
      val weights = Vector.tabulate(trials)(i => if i % 2 == 0 then 1.0 else -1.0)
      val query = ProfileTrialSignedQuery
        .make("alternating", axis, weights, 1e-6)
        .fold(e => throw new IllegalArgumentException(e.message), identity)
      (rays(layout, selectedRadii, nodes) ++ fresh(freshCount)).foreach: point =>
        val actual = coordinates(f, point.unit)
        val design = originalDesign(f, actual)
        val times = f.dataset.samplingFrame.samples().map(_.value)
        val events = onsets(f)
        val truncated = Array.tabulate(design.length)(i =>
          if times(i / trials) - events(i % trials) > horizon then 0.0 else design(i)
        )
        val basisDesign = DecodedTrialCheckpoint.designAt(f.expanded, actual)
        def designError(a: Array[Double], b: Array[Double]): Double =
          math.sqrt(a.indices.map(i => math.pow(a(i) - b(i), 2)).sum / b.iterator.map(x => x * x).sum)
        val tailDesignError = designError(truncated, design)
        val basisDesignError = designError(basisDesign, truncated)
        val raw = response(f, design)
        val current = f.copy(rawBlock = raw)
        val original = current.oracleDesign(0, design).take(trials)
        val input = ProfileTrialResponse
          .make(axis, axis.selectedResponseRows, ProfileTrialResponseDomain.Original, raw)
          .fold(e => throw new IllegalArgumentException(e.message), identity)
        def evaluate(node: Int, mode: ProfileTrialReadoutMode, request: OutputRequest) =
          ProfileTrialReadout
            .freeze(bank, axis, actual, node, NormalizationRule.Unnormalised, mode)
            .flatMap(_.newWorker().evaluate(input, request))
            .fold(e => throw new IllegalArgumentException(e.message), identity)
        val exact = evaluate(0, ProfileTrialReadoutMode.ExactShape, DecodedTrialCheckpoint.request).trialAmplitudes.get
        def relative(a: Vector[Double], b: Vector[Double]): Double =
          math.sqrt(a.zip(b).map((x, y) => (x - y) * (x - y)).sum / b.map(x => x * x).sum)
        def signed(a: Vector[Double]): Double = a.zip(weights).map(_ * _).sum
        val nearest = (0 until 8).toVector.sortBy: node =>
          val ref = referenceGrid.point(node)
          actual.indices.map(i => math.pow((actual(i) - ref(i)) / f.plan.basis.family.chart.width(i), 2)).sum
        val selected = point.anchor.fold((0 until 8).toVector)(Vector(_))
        selected.foreach: node =>
          val corrected = evaluate(node, ProfileTrialReadoutMode.CorrectedReference, DecodedTrialCheckpoint.request)
          val queried = evaluate(
            node,
            ProfileTrialReadoutMode.CorrectedReference,
            OutputRequest.TrialQueries(Vector(query), NormalizationRule.Unnormalised)
          )
          val candidate = corrected.trialAmplitudes.get
          records += Record(
            layout,
            point,
            node,
            nearest.take(2).contains(node),
            relative(candidate, exact),
            relative(candidate, original),
            relative(exact, original),
            math.abs(queried.queries.head.value - signed(exact)),
            math.abs(queried.queries.head.value - signed(original)),
            math.abs(signed(exact) - signed(original)),
            tailDesignError,
            basisDesignError,
            corrected.evidence.preparedBasisNormalResidualNorm,
            corrected.work.referenceInverseAttempts,
            corrected.work.residualCorrections,
            corrected.work.exactReadoutFactorAttempts,
            queried.work.retainedTrialAmplitudeValues
          )
      completed(s"N=$trials horizon=$horizon layout=$layout complete")
    val values = records.result()
    val coverage = Vector("corners", "quarters").map: layout =>
      val groups =
        values.filter(r => r.layout == layout && r.point.anchor.isEmpty).groupBy(_.point.label).values.toVector
      def count(predicate: Record => Boolean) = groups.count(_.exists(predicate))
      def p95(f: Record => Double): Double =
        val sorted = groups.map(_.map(f).min).sorted
        if sorted.isEmpty then 0.0 else sorted(math.ceil(0.95 * sorted.length).toInt - 1)
      Coverage(
        layout,
        groups.length,
        count(r => r.nearestTwo && r.preparedRelativeError <= amplitudeTolerance),
        count(_.preparedRelativeError <= amplitudeTolerance),
        count(_.originalRelativeError <= amplitudeTolerance),
        count(_.preparedQueryError <= queryTolerance),
        count(_.originalQueryError <= queryTolerance),
        groups.count(_.head.exactOriginalRelativeError <= amplitudeTolerance),
        p95(_.preparedRelativeError),
        p95(_.originalRelativeError)
      )
    val caveats = Vector(
      ScenarioCaveat(
        "fixed-shape-exploration",
        CaveatKind.DiagnosticsGap,
        CaveatSeverity.Blocking,
        "first-level-laws",
        Some("PHRF-14"),
        "fixed shapes and exploratory responses are not adaptive scientific qualification"
      ),
      ScenarioCaveat(
        "runtime-routing-certificate",
        CaveatKind.PublicApiGap,
        CaveatSeverity.Blocking,
        "fit/profile",
        Some("PHRF-11"),
        "all-eight availability is an offline diagnostic, with no bounded certified runtime router"
      )
    )
    val scenarios = coverage.map: c =>
      ScenarioResult(
        s"phrf-neighborhood-$trials-$horizon-${c.layout}",
        Vector(
          ScenarioHarness.fact(
            "original amplitude coverage at least 95 percent",
            c.attempted > 0 &&
              c.anyReferenceOriginalAmplitudePasses.toDouble / c.attempted >= 0.95,
            s"${c.anyReferenceOriginalAmplitudePasses}/${c.attempted}"
          )
        ),
        caveats
      )
    Result(
      Geometry(trials, horizon, preparation.basisRank, preparation.bandwidth, shared, worker, 8, preparationSeconds),
      values,
      coverage,
      scenarios
    )
