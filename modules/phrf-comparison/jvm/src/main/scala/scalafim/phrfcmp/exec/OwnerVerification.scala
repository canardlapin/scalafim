package scalafim.phrfcmp.exec

import java.nio.charset.StandardCharsets.UTF_8
import scala.util.control.NonFatal

/** Why the owner cannot verify an aggregate although the store is well formed. Such a store is never `Valid`: the
  * whitelist released from it stands unverified, and the owner treats it as the deviation report requires.
  */
enum UnverifiableReason(val message: String):
  /** The aggregate record names no contribution schema: it was sealed by [[PilotAggregation.aggregateAndSeal]] from a
    * corpus the runner assembled out of sight, so nothing sealed binds the corpus to the units' payloads.
    */
  case LegacyAggregate
      extends UnverifiableReason("the aggregate record carries no contribution schema; its corpus is not bound to sealed contributions")
  case UnknownSchema(schema: String) extends UnverifiableReason(s"no corpus assembler for contribution schema '$schema'")
  case RebuildFailed(code: String) extends UnverifiableReason(s"the corpus could not be rebuilt from the first completed attempts ($code)")
  /** A first completed attempt sealed no scorer contribution: nothing binds its share of the corpus. */
  case MissingContribution(unit: WorkUnit)
      extends UnverifiableReason(s"the first completed attempt of ${OwnerVerification.path(unit)} sealed no scorer contribution")

/** Why the owner refuses the store or the aggregate outright: it is malformed, ambiguous or self-inconsistent. */
enum VerificationRefusal(val message: String):
  case NoAggregate extends VerificationRefusal("no aggregate record carries the manifest's whitelist hash")
  case AmbiguousAggregate(runIds: Vector[String])
      extends VerificationRefusal(s"several aggregate records carry the manifest's whitelist hash (${runIds.mkString(", ")})")
  case MalformedRecord(name: String, detail: String) extends VerificationRefusal(s"malformed record $name: $detail")
  case PayloadMismatch(name: String) extends VerificationRefusal(s"payload $name is missing or does not match its ledger hash")
  case PayloadLedgerMismatch(ledger: String)
      extends VerificationRefusal(s"ledger record $ledger disagrees with its payload's status or entry inventory")
  case DatasetsOutOfPlan(datasets: Int) extends VerificationRefusal(s"aggregate D = $datasets is outside the plan's dataset bounds")
  case ImpossibleLedger(name: String, detail: String) extends VerificationRefusal(s"ledger record $name cannot come from this plan's runner: $detail")
  case InconsistentInvocations(detail: String) extends VerificationRefusal(s"ledger invocation history is inconsistent: $detail")
  case UnitListMismatch(detail: String) extends VerificationRefusal(s"the aggregate's unit list is not the kept set: $detail")
  case ConsumedCommitMissing(unit: WorkUnit)
      extends VerificationRefusal(s"the commit the scorer consumed for ${OwnerVerification.path(unit)} is not in the store")
  case NoFirstAttempt(unit: WorkUnit)
      extends VerificationRefusal(s"kept unit ${OwnerVerification.path(unit)} has no scheduled commit")

/** The secondary check (format spec section 10): the outcome digest rebuilt from the first completed attempts. */
enum DigestCheck:
  case Matched
  case Mismatched
  case NotChecked(reason: UnverifiableReason)

/** The owner's verdict on one aggregate. `nondeterministic` lists the units whose scheduled commits differ in
  * `payload_sha256`; it goes to the deviation report and never invalidates on its own.
  */
enum OwnerVerdict:
  case Valid(runId: String, datasets: Int, units: Int, nondeterministic: Vector[WorkUnit])
  case Invalidated(runId: String, mismatched: Vector[WorkUnit], digest: DigestCheck, nondeterministic: Vector[WorkUnit])
  case Unverifiable(runId: String, reason: UnverifiableReason, nondeterministic: Vector[WorkUnit])
  case Refused(reason: VerificationRefusal)

/** The owner-side verification of format spec section 10, over a store the owner has already decrypted and read
  * (`name -> plaintext` after the section 6 reader checks). It holds no key material and the runner never calls it.
  *
  *   1. The authoritative aggregate is the one whose `whitelist_sha256` the manifest records; none or several refuse.
  *      Its D must be one the runner can produce for the plan (the full plan, or at least `minDatasets`).
  *   2. Every ledger record must name its own payload, match its SHA-256, and agree with the payload's status and
  *      entry inventory; a payload that does not decode refuses (never an exception).
  *   3. The scored commit of a unit is its first completed attempt: the `scheduled` record with the smallest
  *      invocation (then the smallest run id), whatever its status, failures and refusals included.
  *   4. The aggregate's `units` must be exactly the plan's cells and arms over datasets `0 until D`, each naming a
  *      commit of the aggregating invocation that exists in the store.
  *   5. Primary check: each consumed `payload_sha256` equals the first attempt's; every difference is named.
  *   6. Secondary check: the corpus is rebuilt by the aggregate's [[CorpusAssembler]] from the scorer contributions
  *      sealed in the first attempts' payloads, and its [[CorpusDigest.outcomes]] must equal `corpus_outcomes_sha256`.
  *
  * Any difference in 5 or 6 invalidates the whitelist. A store whose aggregate binds no contributions (a legacy,
  * pre-S10 aggregate), whose schema has no assembler, or whose first attempts lack a contribution is
  * [[OwnerVerdict.Unverifiable]], never `Valid`, whatever defaults an assembler would supply.
  */
object OwnerVerification:
  private[exec] def path(u: WorkUnit): String = s"${u.cell.value}/${SealedNames.dataset(u.dataset)}/${u.arm.value}"

  private val RecordName = """aggregate/([^/]+)/record""".r

  private final case class Consumed(unit: WorkUnit, runId: String, phase: CommitPhase, payloadSha256: String)
  private final case class Aggregate(runId: String, datasets: Int, outcomes: String, schema: Option[String], units: Vector[Consumed])

  def verify(
      items: Map[String, Array[Byte]],
      plan: PilotPlan,
      manifestWhitelistSha256: String,
      assemblers: Vector[CorpusAssembler]
  ): OwnerVerdict =
    val result = for
      agg <- authoritative(items, manifestWhitelistSha256)
      _ <- datasetBounds(agg, plan)
      ledgers <- ledgerRecords(items, plan)
      _ <- unitList(agg, plan)
      firsts <- firstAttempts(agg, ledgers)
    yield
      val nondeterministic = ledgers.filter(_.phase == CommitPhase.Scheduled).groupBy(_.unit).toVector
        .collect { case (u, rs) if rs.map(_.payloadSha256).distinct.length > 1 => u }.sortBy(path)
      val mismatched = agg.units.collect { case c if firsts(c.unit).payloadSha256 != c.payloadSha256 => c.unit }
      val digest = rebuild(items, agg, firsts, assemblers)
      digest match
        case Left(r: VerificationRefusal) => OwnerVerdict.Refused(r)
        case Right(d) =>
          if mismatched.nonEmpty || d == DigestCheck.Mismatched then OwnerVerdict.Invalidated(agg.runId, mismatched, d, nondeterministic)
          else
            d match
              case DigestCheck.NotChecked(reason) => OwnerVerdict.Unverifiable(agg.runId, reason, nondeterministic)
              case _ => OwnerVerdict.Valid(agg.runId, agg.datasets, agg.units.length, nondeterministic)
    result.fold(OwnerVerdict.Refused(_), identity)

  private def text(items: Map[String, Array[Byte]], name: String): String = new String(items(name), UTF_8)

  private def authoritative(items: Map[String, Array[Byte]], whitelist: String): Either[VerificationRefusal, Aggregate] =
    val parsed = items.keys.toVector.sorted.collect { case n @ RecordName(rid) => n -> rid }.map((n, rid) => parseAggregate(n, rid, text(items, n)))
    parsed.collectFirst { case Left(e) => e } match
      case Some(e) => Left(e)
      case None =>
        parsed.collect { case Right((wl, a)) if wl == whitelist => a } match
          case Vector() => Left(VerificationRefusal.NoAggregate)
          case Vector(one) => Right(one)
          case many => Left(VerificationRefusal.AmbiguousAggregate(many.map(_.runId)))

  private def parseAggregate(name: String, runId: String, json: String): Either[VerificationRefusal, (String, Aggregate)] =
    def bad(detail: String) = Left(VerificationRefusal.MalformedRecord(name, detail))
    try
      val o = ujson.read(json).obj
      val keys = o.keySet.toSet
      val base = Set("run_id", "datasets", "whitelist_sha256", "corpus_outcomes_sha256", "corpus_timing_sha256", "units")
      if keys != base && keys != base + "contribution_schema" then bad("unexpected keys")
      else if o("run_id").str != runId then bad("run id differs from the record name")
      else
        val units = o("units").arr.toVector.map { u =>
          for
            cell <- CellId.parse(u("cell").str)
            arm <- ArmId.parse(u("arm").str)
            phase <- CommitPhase.fromCode(u("phase").str).toRight("unknown phase")
            d = u("dataset").num
            _ <- Either.cond(d.isWhole && d >= 0 && d <= Int.MaxValue, (), "dataset index")
            sha = u("payload_sha256").str
            _ <- Either.cond(LedgerRecord.isSha256(sha), (), "payload hash")
          yield Consumed(WorkUnit(cell, d.toInt, arm), u("run_id").str, phase, sha)
        }
        val datasets = o("datasets").num
        units.collectFirst { case Left(e) => e } match
          case Some(e) => bad(e)
          case None =>
            // bounded before narrowing: a non-integral, non-positive or non-Int D is malformed
            if datasets.isNaN || !datasets.isWhole || datasets < 1 || datasets > Int.MaxValue then bad("datasets")
            else
              val schema = o.get("contribution_schema").map(_.str)
              if schema.exists(s => !SafeName.valid(s)) then bad("contribution schema")
              else
                val agg = Aggregate(runId, datasets.toInt, o("corpus_outcomes_sha256").str, schema, units.collect { case Right(c) => c })
                Right(o("whitelist_sha256").str -> agg)
    catch case e: Exception => bad(e.getClass.getSimpleName)

  /** Every ledger record, scheduled and rerun, after checking that it sits under its own name and that the payload
    * it names exists and matches its hash.
    */
  private def ledgerRecords(items: Map[String, Array[Byte]], plan: PilotPlan): Either[VerificationRefusal, Vector[LedgerRecord]] =
    val arms = plan.cells.map(c => c.id -> c.arms.toSet).toMap
    val names = items.keys.toVector.sorted.filter(n => n.startsWith("ledger/") || n.startsWith("rerun/") && n.split('/').lift(2).contains("ledger"))
    val parsed = names.map { n =>
      LedgerRecord.parse(text(items, n)).left.map(VerificationRefusal.MalformedRecord(n, _)).flatMap { r =>
        val (own, payload) = r.phase match
          case CommitPhase.Scheduled => (SealedNames.ledger(r.unit, r.runId), SealedNames.data(r.unit, r.runId))
          case CommitPhase.Rerun => (SealedNames.rerunLedger(r.unit, r.runId), SealedNames.rerunData(r.unit, r.runId))
        if own != n || r.payload != payload then Left(VerificationRefusal.MalformedRecord(n, "record does not match its name"))
        else if !arms.get(r.unit.cell).exists(_.contains(r.unit.arm)) || r.unit.dataset >= plan.datasets then
          Left(VerificationRefusal.ImpossibleLedger(n, "unit outside the plan"))
        else if r.invocation > CostState.MaxInvocations then
          Left(VerificationRefusal.ImpossibleLedger(n, s"invocation ${r.invocation} exceeds the runner's largest ordinal ${CostState.MaxInvocations}"))
        else if r.attempts.toLong > plan.maxRetries.toLong + 1L then Left(VerificationRefusal.ImpossibleLedger(n, s"${r.attempts} attempts exceed the retry cap"))
        else if !items.get(payload).exists(b => Fs.sha256(b) == r.payloadSha256) then Left(VerificationRefusal.PayloadMismatch(payload))
        else
          decode(items, payload).flatMap { (status, entries) =>
            if status != r.status || entries.map(_._1) != r.entries then Left(VerificationRefusal.PayloadLedgerMismatch(n))
            // RetryPolicy: a failure is retried while retries remain, so a committed Failed has exhausted them all
            else if r.status == UnitStatus.Failed && r.attempts.toLong != plan.maxRetries.toLong + 1L then
              Left(VerificationRefusal.ImpossibleLedger(n, s"Failed after ${r.attempts} attempts, before the retries were exhausted"))
            else Right(r)
          }
      }
    }
    parsed.collectFirst { case Left(e) => e }.toLeft(parsed.collect { case Right(r) => r }).flatMap(invocationHistory)

  /** One invocation is one run id (format spec section 10): every record of a run id carries the same ordinal, and
    * no two run ids share one.
    */
  private def invocationHistory(ledgers: Vector[LedgerRecord]): Either[VerificationRefusal, Vector[LedgerRecord]] =
    val byRun = ledgers.groupBy(_.runId).view.mapValues(_.map(_.invocation).distinct).toMap
    val byInvocation = ledgers.groupBy(_.invocation).view.mapValues(_.map(_.runId).distinct).toMap
    byRun.collectFirst { case (rid, invs) if invs.length > 1 => s"run $rid carries invocations ${invs.sorted.mkString(",")}" }
      .orElse(byInvocation.collectFirst { case (inv, rids) if rids.length > 1 => s"invocation $inv carries ${rids.length} run ids" })
      .map(VerificationRefusal.InconsistentInvocations(_))
      .toLeft(ledgers)

  /** A unit payload, decoded with every failure contained in a typed refusal. */
  private def decode(items: Map[String, Array[Byte]], payload: String): Either[VerificationRefusal, (UnitStatus, Vector[(String, Array[Byte])])] =
    try UnitPayload.decode(items(payload)).left.map(VerificationRefusal.MalformedRecord(payload, _))
    catch case NonFatal(e) => Left(VerificationRefusal.MalformedRecord(payload, e.getClass.getSimpleName))

  /** D as the runner can produce it for this plan: the full plan, or a partial pilot of at least `minDatasets`. */
  private def datasetBounds(agg: Aggregate, plan: PilotPlan): Either[VerificationRefusal, Unit] =
    val d = agg.datasets
    val ok = d >= 2 && d <= plan.datasets && (d == plan.datasets || d >= plan.minDatasets)
    Either.cond(ok, (), VerificationRefusal.DatasetsOutOfPlan(d))

  private def unitList(agg: Aggregate, plan: PilotPlan): Either[VerificationRefusal, Unit] =
    // counted in Long before anything of size D is built: a huge D refuses without allocating
    val expectedCount = plan.cells.map(_.arms.length.toLong).sum * agg.datasets.toLong
    if agg.units.length.toLong != expectedCount then
      Left(VerificationRefusal.UnitListMismatch(s"${agg.units.length} units listed, $expectedCount expected"))
    else unitSet(agg, plan)

  private def unitSet(agg: Aggregate, plan: PilotPlan): Either[VerificationRefusal, Unit] =
    val expected = for
      c <- plan.cells
      d <- 0 until agg.datasets
      a <- c.arms
    yield WorkUnit(c.id, d, a)
    val listed = agg.units.map(_.unit)
    if listed.distinct.length != listed.length then Left(VerificationRefusal.UnitListMismatch("a unit is listed twice"))
    else if listed.toSet != expected.toSet then
      val missing = expected.filterNot(listed.toSet).map(path)
      val extra = listed.filterNot(expected.toSet).map(path)
      Left(VerificationRefusal.UnitListMismatch(s"missing ${missing.take(3).mkString(",")} extra ${extra.take(3).mkString(",")}"))
    else if agg.units.exists(_.runId != agg.runId) then Left(VerificationRefusal.UnitListMismatch("a consumed commit is not of the aggregating invocation"))
    else Right(())

  /** The first completed attempt of every kept unit, after checking that each consumed commit exists. */
  private def firstAttempts(agg: Aggregate, ledgers: Vector[LedgerRecord]): Either[VerificationRefusal, Map[WorkUnit, LedgerRecord]] =
    val byCommit = ledgers.map(r => (r.unit, r.runId, r.phase) -> r).toMap
    val first = ledgers.filter(_.phase == CommitPhase.Scheduled).groupBy(_.unit).view.mapValues(_.minBy(r => (r.invocation, r.runId))).toMap
    agg.units.collectFirst {
      case c if !byCommit.get((c.unit, c.runId, c.phase)).exists(_.payloadSha256 == c.payloadSha256) => VerificationRefusal.ConsumedCommitMissing(c.unit)
      case c if !first.contains(c.unit) => VerificationRefusal.NoFirstAttempt(c.unit)
    }.toLeft(first.view.filterKeys(agg.units.map(_.unit).toSet).toMap)

  /** The secondary check over the first attempts' sealed contributions. */
  private def rebuild(
      items: Map[String, Array[Byte]],
      agg: Aggregate,
      firsts: Map[WorkUnit, LedgerRecord],
      assemblers: Vector[CorpusAssembler]
  ): Either[VerificationRefusal, DigestCheck] =
    agg.schema match
      case None => Right(DigestCheck.NotChecked(UnverifiableReason.LegacyAggregate))
      case Some(schema) =>
        assemblers.find(_.schema == schema) match
          case None => Right(DigestCheck.NotChecked(UnverifiableReason.UnknownSchema(schema)))
          case Some(assembler) =>
            val decoded = agg.units.map { c =>
              val rec = firsts(c.unit)
              decode(items, rec.payload).map((_, entries) =>
                new UnitContribution(c.unit, entries.collectFirst { case (n, b) if n == ScorerContribution.EntryName => b })
              )
            }
            decoded.collectFirst { case Left(e) => e } match
              case Some(e) => Left(e)
              case None =>
                val contributions = UnitContribution.canonical(decoded.collect { case Right(u) => u })
                // absent evidence is never made acceptable by an assembler's defaults (re-review failure 1)
                contributions.find(_.payload.isEmpty) match
                  case Some(missing) => Right(DigestCheck.NotChecked(UnverifiableReason.MissingContribution(missing.unit)))
                  case None =>
                    val rebuilt =
                      try assembler.outcomes(agg.datasets, contributions).map(CorpusDigest.outcomes(agg.datasets, _))
                      catch case NonFatal(e) => Left("assembler_" + e.getClass.getSimpleName)
                    Right(rebuilt match
                      case Left(code) => DigestCheck.NotChecked(UnverifiableReason.RebuildFailed(code.take(64)))
                      case Right(digest) => if digest == agg.outcomes then DigestCheck.Matched else DigestCheck.Mismatched)
