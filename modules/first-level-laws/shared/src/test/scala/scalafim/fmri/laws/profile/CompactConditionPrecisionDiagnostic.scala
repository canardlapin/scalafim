package scalafim.fmri.laws.profile

import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.family.ShapePoint

/** Observation-only precision snapshots. Every replay and snapshot evaluation
  * occurs after the original voxel fit and uses separate counters/workspaces.
  * This helper does not change the cohort, objective, decoder or admission.
  * Chunked JSON records avoid platform log line truncation.
  */
private[profile] object CompactConditionPrecisionDiagnostic:
  final case class Work(nodes: Long, jets: Long, exact: Long, attempts: Long, steps: Long)

  def work(counters: DecoderCounters): Work =
    Work(counters.nodeScores, counters.jets, counters.exactEvaluations, counters.candidateAttempts, counters.newtonSteps)

  private def number(value: Double): String =
    if value.isFinite then value.toString else "null"

  private def vector(values: Iterable[Double]): String = values.iterator.map(number).mkString("[", ",", "]")

  private def array(scope: String, field: String, values: Array[Double]): Unit =
    var offset = 0
    while offset < values.length do
      val end = math.min(offset + 128, values.length)
      println(s"""{"kind":"compact-precision-array","scope":"$scope","field":"$field","offset":$offset,"total":${values.length},"values":${vector(values.slice(offset, end).toIndexedSeq)}}""")
      offset = end

  def global(prep: CompactConditionPreparation, budget: DecodeBudget): Unit =
    val basis = prep.basis
    println(s"""{"kind":"compact-precision-header","schema":1,"family":"gaussian","rank":${prep.rank},"conditions":${prep.conditions},"basisRank":${basis.rank},"fineCount":${basis.fineCount},"rows":${prep.rows},"maxNewtonSteps":${budget.maxNewtonSteps},"maxJets":${budget.maxJets},"maxExactEvaluations":${budget.maxExactEvaluations},"maxCandidateAttempts":${budget.maxCandidateAttempts},"stationarityStepTolerance":${budget.stationarityStepTolerance},"diagnosticEvaluationsExcludedFromAlgorithmCounters":true}""")
    array("global", "lags", basis.lags)
    array("global", "phi", Array.tabulate(basis.rank * basis.fineCount)(i => basis.value(i / basis.fineCount, i % basis.fineCount)))
    array("global", "rHat", prep.rHat)

  private def jetRecord(scope: String, coordinates: Array[Double], ok: Boolean, jet: ProfileJetBuffer): Unit =
    println(s"""{"kind":"compact-precision-point","scope":"$scope","coordinates":${vector(coordinates.toIndexedSeq)},"ok":$ok,"energy":${number(jet.energy)},"gradient":${vector(jet.gradient.toIndexedSeq)},"hessian":${vector(jet.hessian.toIndexedSeq)},"amplitudes":${vector(jet.amplitudes.toIndexedSeq)},"curvature":"${jet.curvature}"}""")

  private def snapshot(scope: String, prep: CompactConditionPreparation, coordinates: Array[Double], response: Array[Double], energy: Double): Unit =
    val basis = prep.basis
    val kernel = new Array[Double](prep.family.jetComponents * basis.fineCount)
    val coefficients = new Array[Double](prep.family.jetComponents * basis.rank)
    basis.coefficientJetInto(ShapePoint.unsafe(coordinates.toVector), kernel, coefficients, prep.family.jetComponents)
    val assembler = new CompactConditionJets(prep.rHat, prep.rank, prep.conditions, basis.rank, prep.family.dimension)
    assembler.assemble(response, energy, coefficients, prep.family.jetComponents)
    array(scope, "kernel", kernel.take(basis.fineCount))
    array(scope, "coefficients", coefficients.take(basis.rank))
    array(scope, "design", assembler.valueDesign)

  def voxel(voxel: Int, runtime: CompactConditionRuntime, grid: NodeGrid, budget: DecodeBudget,
      column: Array[Double], fit: CompactConditionFit, before: Work, after: Work): Unit =
    val result = fit.decode
    println(s"""{"kind":"compact-precision-voxel","voxel":$voxel,"status":"${result.status}","budgetExit":"${result.budgetExit}","coordinates":${vector(result.coordinates)},"energy":${number(result.energy)},"amplitudes":${vector(result.amplitudes)},"node":${result.node},"steps":${result.newtonSteps},"nodes":${after.nodes - before.nodes},"jets":${after.jets - before.jets},"exact":${after.exact - before.exact},"attempts":${after.attempts - before.attempts}}""")
    if result.status != DecodeStatus.Accepted then
      val prep = runtime.prep
      val response = new Array[Double](prep.rank)
      val nuisance = new Array[Double](prep.nuisanceRank)
      val energy = prep.project(column, 0, response, nuisance)
      array(s"voxel-$voxel", "response", response)
      println(s"""{"kind":"compact-precision-response","voxel":$voxel,"energy":$energy}""")
      var callback = 0
      val observed = new ShapeObjective:
        def grid: NodeGrid = runtime.objective.grid
        def amplitudeCount: Int = runtime.objective.amplitudeCount
        def scoreNode(node: Int): Double = runtime.objective.scoreNode(node)
        def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
          val ok = runtime.objective.jetAtNode(node, out)
          jetRecord(s"voxel-$voxel-replay-$callback-node", grid.point(node).coordinates.toArray, ok, out)
          callback += 1
          ok
        def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
          val ok = runtime.objective.jetAt(coordinates, out)
          jetRecord(s"voxel-$voxel-replay-$callback-jet", coordinates, ok, out)
          callback += 1
          ok
        def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
          val value = runtime.objective.energyAt(coordinates, out)
          jetRecord(s"voxel-$voxel-replay-$callback-energy", coordinates, value.isFinite, out)
          callback += 1
          value
      val replayCounters = new DecoderCounters
      val replay = new ShapeDecoder(observed, budget, None, 1.0).decode(replayCounters)
      require(replay == result, s"observation replay changed voxel $voxel result")
      println(s"""{"kind":"compact-precision-replay","voxel":$voxel,"sameResult":true,"jets":${replayCounters.jets},"exact":${replayCounters.exactEvaluations},"attempts":${replayCounters.candidateAttempts}}""")
      val coordinates = result.coordinates.toArray
      val terminal = new ProfileJetBuffer(2, prep.conditions)
      val terminalOk = runtime.objective.jetAt(coordinates, terminal)
      jetRecord(s"voxel-$voxel-terminal", coordinates, terminalOk, terminal)
      snapshot(s"voxel-$voxel-terminal", prep, coordinates, response, energy)
      val h = terminal.hessian
      val g = terminal.gradient
      // Explicit scalar reproduction of the existing two-dimensional Cholesky
      // operation order, solely for this diagnostic's proposed full Newton pair.
      val l00 = math.sqrt(h(0))
      val l10 = h(2) / l00
      val l11 = math.sqrt(h(3) - l10 * l10)
      var step0 = -g(0) / l00
      var step1 = (-g(1) - l10 * step0) / l11
      step1 /= l11
      step0 = (step0 - l10 * step1) / l00
      val candidate = Array(grid.chart.clamp(0, coordinates(0) + step0), grid.chart.clamp(1, coordinates(1) + step1))
      val next = new ProfileJetBuffer(2, prep.conditions)
      val candidateOk = runtime.objective.jetAt(candidate, next)
      jetRecord(s"voxel-$voxel-candidate", candidate, candidateOk, next)
      snapshot(s"voxel-$voxel-candidate", prep, candidate, response, energy)
      println(s"""{"kind":"compact-precision-pair","voxel":$voxel,"rawCorrection":${vector(Vector(step0, step1))},"predictedDrop":${number(-0.5 * (g(0) * step0 + g(1) * step1))},"halfUlp":${number(0.5 * math.ulp(terminal.energy))},"reportedDifference":${number(next.energy - terminal.energy)},"reportedUlpDifference":${number((next.energy - terminal.energy) / math.ulp(terminal.energy))},"candidateIsDiagnosticFullNewton":true}""")
