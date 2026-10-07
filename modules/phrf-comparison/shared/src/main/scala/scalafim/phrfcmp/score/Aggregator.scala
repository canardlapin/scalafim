package scalafim.phrfcmp.score

import scala.util.control.NonFatal

/** Sealed-side by-products of aggregation: complete-case sigma, imputation, floor and zero-variance counts, and the
  * raw refusal numerators. Only `blob` leaves, and only into the sealed store (no codec, no `toString`).
  */
final class SealedDiagnostics private[score] (private val bytes: Array[Byte]):
  private[phrfcmp] def blob: Array[Byte] = bytes.clone()
  override def toString: String = "SealedDiagnostics(<redacted>)"

/** What the aggregator returns. `runComplete` is the only free-form line, built from the total CPU hours alone. */
final class AggregationOutput private[score] (
    val whitelist: PilotWhitelist,
    val whitelistJson: String,
    val whitelistSha256: String,
    private[phrfcmp] val sealedDiagnostics: SealedDiagnostics,
    val runComplete: String
):
  override def toString: String = "AggregationOutput(<redacted>)"

object RunComplete:
  /** The run-complete message and total-CPU line: independent of any score. */
  def line(totalCpuSeconds: Double): String =
    "run complete; total CPU hours: " + Fmt.fixed2(totalCpuSeconds / 3600.0)

object PilotAggregator:
  /** Required df for a reported sigma (design 3.3: a pair with df < 14 is refused, not extrapolated). */
  val MinDf: Int = 14

  /** Coverage ICC is emitted only while the pooled coverage proportion lies in this closed band. */
  val CoverageBand: (Double, Double) = (0.05, 0.95)

  /** Exceptions never cross this boundary: anything unexpected becomes `Internal(className)`, class name only. The
    * exception message, cause and stack trace are dropped here.
    */
  def aggregateGuarded(corpus: PilotCorpus): Either[ScoreError, AggregationOutput] = guard(aggregate(corpus))

  private[phrfcmp] def guard[A](body: => Either[ScoreError, A]): Either[ScoreError, A] =
    try body
    catch case NonFatal(e) => Left(ScoreError.Internal(e.getClass.getName))

  /** A trial cell with constant true within-condition amplitudes has no Fisher-z endpoint; its pairs render
    * `degenerate` without aborting the rest of the pilot output.
    */
  private enum PairOutcome:
    case Estimated(acc: PairAcc)
    case Degenerate

  private final class PairAcc(
      val sigma: Centred,
      val complete: Option[Centred],
      val completeN: Int,
      val imputed: Long,
      val excluded: Long,
      val floors: Long,
      val zeroVar: Long
  )

  private def sequence[A](xs: Vector[Either[ScoreError, A]]): Either[ScoreError, Vector[A]] =
    xs.foldLeft[Either[ScoreError, Vector[A]]](Right(Vector.empty))((acc, x) => acc.flatMap(v => x.map(v :+ _)))

  private def pairAcc(corpus: PilotCorpus, pair: GatingPair): Either[ScoreError, PairOutcome] =
    val results: Either[ScoreError, Vector[PairDatasetResult]] =
      if pair.isCondition then Right(corpus.condition(pair.cell).map(Endpoints.conditionPair(_, pair)))
      else sequence(corpus.trial(pair.cell).map(Endpoints.trialPair(_, pair)))
    results match
      case Left(ScoreError.TruthDegenerate(_)) => Right(PairOutcome.Degenerate)
      case Left(e)                              => Left(e)
      case Right(rs)                            => estimated(pair, rs)

  private def estimated(pair: GatingPair, rs: Vector[PairDatasetResult]): Either[ScoreError, PairOutcome] =
    Centred.of(rs.flatMap(_.endpoint)).flatMap { c =>
      if c.df < MinDf then Left(ScoreError.DfTooSmall(pair, c.df, MinDf))
      else
        val cc = rs.flatMap(_.completeCaseEndpoint)
        val ccC = if cc.length >= 2 then Centred.of(cc).toOption else None
        Right(
          PairOutcome.Estimated(
            new PairAcc(
              c, ccC, cc.length,
              rs.map(_.imputedVoxels.toLong).sum, rs.map(_.excludedVoxels.toLong).sum,
              rs.map(_.floorUses.toLong).sum, rs.map(_.zeroVarianceEstimates.toLong).sum
            )
          )
        )
    }

  private def refusalCounts(corpus: PilotCorpus): Map[Method, (Long, Long)] =
    val b = scala.collection.mutable.Map.empty[Method, (Long, Long)]
    def add(m: Method, missing: Int, total: Int): Unit =
      val (a, t) = b.getOrElse(m, (0L, 0L))
      b(m) = (a + missing, t + total)
    corpus.condition.valuesIterator.foreach(_.foreach(ds => ds.arms.foreach((m, vs) => add(m, vs.count(_.isMissing), vs.length))))
    corpus.trial.valuesIterator.foreach(_.foreach(ds => ds.arms.foreach((m, vs) => add(m, vs.count(_.isMissing), vs.length))))
    b.toMap

  private def median(xs: Vector[Double]): Double =
    val s = xs.sorted
    val n = s.length
    if n % 2 == 1 then s(n / 2) else 0.5 * (s(n / 2 - 1) + s(n / 2))

  private def summary(xs: Vector[Double]): TimingSummary = TimingSummary(median(xs), xs.min, xs.max)

  private def iccValue(clusters: Vector[Vector[Double]]): Either[ScoreError, IccValue] =
    Icc.of(clusters).map {
      case Some(e) => IccValue.Estimate(e.icc, e.upper80)
      case None    => IccValue.Degenerate
    }

  private def iccBlock(corpus: PilotCorpus): Either[ScoreError, IccBlock] =
    val obs: Vector[Vector[CoverageObs]] = corpus.coverage.map(_.voxels.flatMap(_.toOption))
    val all = obs.flatten
    val prop = if all.isEmpty then 0.0 else all.count(_.covered).toDouble / all.length
    for
      e <- iccValue(obs.map(_.map(_.signedRelativePeakError)))
      t <- iccValue(obs.map(_.map(_.tauError)))
      cv <- if all.isEmpty || prop < CoverageBand._1 || prop > CoverageBand._2 then Right(IccValue.Degenerate)
            else iccValue(obs.map(_.map(o => if o.covered then 1.0 else 0.0)))
    yield IccBlock(e, t, cv)

  private def timingBlock(t: TimingSamples): TimingBlock =
    val ratios = t.alphaPreparationProfiles.map(p => p.max / p.min)
    TimingBlock(
      summary(t.trialMlCoreSecondsPerVoxel),
      summary(t.trialPreparationCoreSecondsPerAlpha),
      AlphaCacheReport.NotPossibleViaPublicApi(median(ratios)),
      summary(t.glmsingleCoreSecondsPerDataset),
      summary(t.coldConditionPreparationCoreSeconds)
    )

  def aggregate(corpus: PilotCorpus): Either[ScoreError, AggregationOutput] =
    for
      accs <- sequence(GatingPair.all.map(pairAcc(corpus, _)))
      icc <- iccBlock(corpus)
    yield
      val counts = refusalCounts(corpus)
      val rates = Method.all.map { m =>
        val (miss, tot) = counts.getOrElse(m, (0L, 0L))
        PooledRate(m, if tot == 0L then 0.0 else miss.toDouble / tot, tot)
      }
      val table = SigmaTable(GatingPair.all.zip(accs).map { (p, o) =>
        SigmaEntry(p, o match
          case PairOutcome.Estimated(a) => SigmaValue.Estimate(a.sigma.sd, a.sigma.df)
          case PairOutcome.Degenerate   => SigmaValue.Degenerate
        )
      })
      val wl = PilotWhitelist(table, icc, PooledRefusals(rates), timingBlock(corpus.timing))
      val json = WhitelistWriter.json(wl)
      new AggregationOutput(wl, json, WhitelistWriter.sha256Hex(wl), diagnostics(accs, counts), RunComplete.line(corpus.totalCpuSeconds))

  private final class Sink:
    val buf = scala.collection.mutable.ArrayBuffer.empty[Byte]
    def long(x: Long): Unit =
      var i = 7
      while i >= 0 do
        buf += ((x >>> (8 * i)) & 0xffL).toByte
        i -= 1
    def int(x: Int): Unit = long(x.toLong & 0xffffffffL, 4)
    private def long(x: Long, bytes: Int): Unit =
      var i = bytes - 1
      while i >= 0 do
        buf += ((x >>> (8 * i)) & 0xffL).toByte
        i -= 1
    def bool(b: Boolean): Unit = buf += (if b then 1.toByte else 0.toByte)
    def double(x: Double): Unit = long(java.lang.Double.doubleToLongBits(x))
    def ascii(s: String): Unit = s.foreach(c => buf += c.toByte)

  private def diagnostics(accs: Vector[PairOutcome], counts: Map[Method, (Long, Long)]): SealedDiagnostics =
    val d = new Sink
    d.ascii("PHRFCMP-S8-SEALED-1")
    accs.foreach {
      case PairOutcome.Degenerate => d.bool(false)
      case PairOutcome.Estimated(a) =>
        d.bool(true)
        d.int(a.sigma.n)
        d.int(a.completeN)
        d.bool(a.complete.isDefined)
        d.double(a.complete.map(_.sd).getOrElse(0.0))
        d.long(a.imputed); d.long(a.excluded); d.long(a.floors); d.long(a.zeroVar)
    }
    Method.all.foreach { m =>
      val (x, t) = counts.getOrElse(m, (0L, 0L))
      d.long(x); d.long(t)
    }
    new SealedDiagnostics(d.buf.toArray)
