package scalafim.phrfcmp.exec

import java.io.{ByteArrayOutputStream, DataOutputStream}
import java.nio.charset.StandardCharsets.UTF_8

import scalafim.phrfcmp.score.{AggregationOutput, Method, PilotAggregator, PilotCorpus, ScoreError, VoxelOutcome}
import scalafim.phrfcmp.score.PilotCell as ScoreCell

/** Why aggregation or sealing its by-products failed. Messages are the scorer's redacted `SafeMessage` text. */
enum AggregationRefusal(val message: String):
  case Score(error: ScoreError) extends AggregationRefusal(error.message.toString)
  case Seal(error: SealError) extends AggregationRefusal(error.message)
  case AlreadyAggregated(runId: String)
      extends AggregationRefusal(s"run $runId has already been aggregated into this store; one aggregate per run id")

/** Runs the S8 aggregator inside the runner's process and appends its sealed-side by-products (complete-case sigma,
  * imputation, floor and refusal numerators) to the sealed store. Only the whitelist, its hash and the run-complete
  * line are returned in the clear; the diagnostics exist only as a sealed blob.
  *
  * The by-products depend on the corpus (on `D`, for a start), so they are sealed under names that carry the
  * aggregating invocation's run id (finding M1): aggregating a partial pilot and later the full one leaves two
  * aggregates side by side, never a differing duplicate. Next to the diagnostics, `aggregate/<runId>/record` binds the
  * aggregate to the whitelist hash, to digests of the scorer's input corpus (decision D1) and, unit by unit, to the
  * commit whose payload the scorer consumed (finding F4). At unseal the owner compares that unit list with the first
  * completed attempt of every kept unit and recomputes [[CorpusDigest.outcomes]]; any mismatch invalidates the
  * whitelist released from that aggregate and names the units for the deviation report (format spec section 10).
  */
object PilotAggregation:
  def diagnosticsName(runId: String): String = SealedNames.aggregateDiagnostics(runId)
  def recordName(runId: String): String = SealedNames.aggregateRecord(runId)

  /** (store directory, run id) pairs aggregated in this process. Run ids are drawn per invocation inside
    * `PilotRunner.run`, so a later process cannot hold the same run id; within one process a second call is refused.
    */
  private val aggregated = java.util.concurrent.ConcurrentHashMap.newKeySet[(String, String)]()

  /** Once per run id and store: a second call with the same run id is refused (`AlreadyAggregated`) before anything
    * is sealed, so `aggregate/<runId>/...` can never be sealed twice with different content. A scorer refusal seals
    * nothing and releases the claim.
    *
    * @param runId        the aggregating invocation's run id (`PilotReport.runId`); there is no default
    * @param scorerInputs the commits the scorer consumed, one per kept unit (`PilotReport.scorerInputs`)
    */
  def aggregateAndSeal(corpus: PilotCorpus, store: SealedStore, runId: String, scorerInputs: Vector[ScorerInput]): Either[AggregationRefusal, AggregationOutput] =
    require(SafeName.valid(runId), "run id must be a safe name")
    require(scorerInputs.forall(_.runId == runId), "the scorer consumed commits of the aggregating invocation only")
    require(scorerInputs.map(_.unit).distinct.length == scorerInputs.length, "one scorer input per unit")
    val key = (store.dir.toAbsolutePath.normalize.toString, runId)
    if !aggregated.add(key) then Left(AggregationRefusal.AlreadyAggregated(runId))
    else
      PilotAggregator.aggregateGuarded(corpus) match
        case Left(e) =>
          aggregated.remove(key): Unit // nothing was sealed
          Left(AggregationRefusal.Score(e))
        case Right(out) =>
          for
            _ <- store.append(diagnosticsName(runId), out.sealedDiagnostics.blob).left.map(AggregationRefusal.Seal(_))
            _ <- store.append(recordName(runId), record(corpus, out, runId, scorerInputs)).left.map(AggregationRefusal.Seal(_))
          yield out

  private def record(corpus: PilotCorpus, out: AggregationOutput, runId: String, inputs: Vector[ScorerInput]): Array[Byte] =
    val units = inputs.sortBy(i => (i.unit.cell.value, i.unit.dataset, i.unit.arm.value)).map { i =>
      ujson.Obj(
        "cell" -> ujson.Str(i.unit.cell.value),
        "dataset" -> ujson.Num(i.unit.dataset.toDouble),
        "arm" -> ujson.Str(i.unit.arm.value),
        "run_id" -> ujson.Str(i.runId),
        "phase" -> ujson.Str(i.phase.code),
        "payload_sha256" -> ujson.Str(i.payloadSha256)
      )
    }
    (ujson.write(
      ujson.Obj(
        "run_id" -> ujson.Str(runId),
        "datasets" -> ujson.Num(corpus.datasets.toDouble),
        "whitelist_sha256" -> ujson.Str(out.whitelistSha256),
        "corpus_outcomes_sha256" -> ujson.Str(CorpusDigest.outcomes(corpus)),
        "corpus_timing_sha256" -> ujson.Str(CorpusDigest.timing(corpus)),
        "units" -> ujson.Arr.from(units)
      )
    ) + "\n").getBytes(UTF_8)

/** Canonical digests of the scorer's input corpus (big-endian, IEEE-754 bit patterns, cells in `PilotCell` order,
  * datasets in index order, methods in `Method.all` order).
  *
  *   - [[outcomes]] covers everything result-bearing and reproducible from the sealed first attempts plus the
  *     generator truth: per cell and dataset the truth (noise variance, horizon, trial layout and amplitudes), every
  *     arm's per-voxel outcome, and the PHRF coverage observations.
  *   - [[timing]] covers the timing samples, which are measurements and differ between attempts by nature; it lets
  *     the owner match them against the sealed timing records, but a difference does not invalidate the whitelist.
  */
object CorpusDigest:
  private val OutcomesTag = "phrf-cmp-corpus-outcomes-v1"
  private val TimingTag = "phrf-cmp-corpus-timing-v1"

  private def digest(write: DataOutputStream => Unit): String =
    val bytes = new ByteArrayOutputStream()
    val out = new DataOutputStream(bytes)
    write(out)
    out.flush()
    Fs.sha256(bytes.toByteArray)

  private def outcome[A](o: DataOutputStream, v: VoxelOutcome[A])(value: A => Unit): Unit = v match
    case VoxelOutcome.Estimated(a) => o.writeByte(0); value(a)
    case VoxelOutcome.Refused => o.writeByte(1)
    case VoxelOutcome.Failed => o.writeByte(2)

  private def arms[A](o: DataOutputStream, arms: Map[Method, Vector[VoxelOutcome[A]]])(value: A => Unit): Unit =
    val present = Method.all.filter(arms.contains)
    o.writeInt(present.length)
    present.foreach { m =>
      o.writeUTF(m.code)
      val voxels = arms(m)
      o.writeInt(voxels.length)
      voxels.foreach(outcome(o, _)(value))
    }

  private def doubles(o: DataOutputStream, xs: Vector[Double]): Unit =
    o.writeInt(xs.length)
    xs.foreach(o.writeDouble)

  def outcomes(c: PilotCorpus): String = digest { o =>
    o.writeUTF(OutcomesTag)
    o.writeInt(c.datasets)
    ScoreCell.conditionCells.foreach { cell =>
      o.writeUTF(cell.id)
      val xs = c.condition.getOrElse(cell, Vector.empty)
      o.writeInt(xs.length)
      xs.foreach { x =>
        o.writeInt(x.dataset)
        o.writeDouble(x.noiseVariance)
        o.writeDouble(x.horizonSeconds)
        arms(o, x.arms)(o.writeDouble)
      }
    }
    ScoreCell.trialCells.foreach { cell =>
      o.writeUTF(cell.id)
      val xs = c.trial.getOrElse(cell, Vector.empty)
      o.writeInt(xs.length)
      xs.foreach { x =>
        o.writeInt(x.dataset)
        o.writeInt(x.truth.conditionOfTrial.length)
        x.truth.conditionOfTrial.foreach(o.writeInt)
        o.writeInt(x.truth.amplitudes.length)
        x.truth.amplitudes.foreach(doubles(o, _))
        arms(o, x.arms)(e => doubles(o, e.values))
      }
    }
    o.writeInt(c.coverage.length)
    c.coverage.foreach { x =>
      o.writeInt(x.dataset)
      o.writeInt(x.voxels.length)
      x.voxels.foreach(outcome(o, _) { obs =>
        o.writeDouble(obs.signedRelativePeakError)
        o.writeDouble(obs.tauError)
        o.writeBoolean(obs.covered)
      })
    }
  }

  def timing(c: PilotCorpus): String = digest { o =>
    val t = c.timing
    o.writeUTF(TimingTag)
    doubles(o, t.trialMlCoreSecondsPerVoxel)
    doubles(o, t.trialPreparationCoreSecondsPerAlpha)
    o.writeInt(t.alphaPreparationProfiles.length)
    t.alphaPreparationProfiles.foreach(doubles(o, _))
    doubles(o, t.glmsingleCoreSecondsPerDataset)
    doubles(o, t.coldConditionPreparationCoreSeconds)
    o.writeDouble(c.totalCpuSeconds)
  }
