package scalafim.phrfcmp.score

import ScoreSynth.{ok, Rng}

/** S8 review follow-ups: trial imputation direction, Failed in pooled rates, pool membership, trial planted scan,
  * count-free messages, degenerate trial cells, the df = 14 boundary.
  */
class FollowUpSuite extends munit.FunSuite:
  private def agg(c: PilotCorpus): AggregationOutput = ok(PilotAggregator.aggregate(c))
  private def est(e: SigmaEntry): (Double, Int) = e.value match
    case SigmaValue.Estimate(s, d) => (s, d)
    case SigmaValue.Degenerate     => fail("unexpected degenerate sigma")
  private def entry(o: AggregationOutput, p: GatingPair): SigmaEntry = o.whitelist.sigmas.entries.find(_.pair == p).get

  private val Z: Map[Method, Double] = Map(Method.Phrf -> 0.8, Method.Lsa -> 0.5, Method.Lss -> 0.6, Method.Rlss -> 0.7, Method.GlmsD -> 0.3)

  private def refusedCorpus(glmsZ: Double = 0.3, glmsRefused: Boolean = false, d: Int = 20): PilotCorpus =
    ScoreSynth.corpus(
      d = d,
      trialFn = Some((c, dd) =>
        ScoreSynth.exactTrialDataset(
          c, dd,
          (m, _) => if m == Method.GlmsD then glmsZ else Z(m),
          (m, v) => (m == Method.Phrf && v == 0 && dd % 2 == 0) || (m == Method.GlmsD && glmsRefused)
        )
      )
    )

  // ---- item 2: trial imputation direction -----------------------------------------------------------------

  test("trial worst-in-family imputation uses the voxel-level minimum: independent sigma and delta-z sign"):
    val corp = refusedCorpus()
    val out = agg(corp)
    val k = math.sqrt(20.0 / 19.0)
    // Even datasets: PHRF refused at voxel 0, imputed with the lowest z of the pool arms (LSA 0.5 for T-TX/T-TS).
    // PHRF mean z = (0.5 + 3 * 0.8) / 4 = 0.725. Odd datasets: 0.8.
    Seq(PilotCell.TTXFast, PilotCell.TTXJit, PilotCell.TTSFast).foreach { cell =>
      Seq(Method.Lsa -> 0.5, Method.Lss -> 0.6, Method.Rlss -> 0.7).foreach { (m, zc) =>
        val pair = GatingPair.all.find(p => p.cell == cell && p.comparator == m).get
        val even = 0.725 - zc
        val odd = 0.8 - zc
        val (sigma, df) = est(entry(out, pair))
        assertEqualsDouble(sigma, k * 0.5 * math.abs(odd - even), 1e-9) // 0.5 * |gap| * sqrt(D/(D-1))
        assertEquals(df, 19)
        val ds = corp.trial(cell)
        val ev = Endpoints.trialPair(ds(0), pair).toOption.get.endpoint.get
        val od = Endpoints.trialPair(ds(1), pair).toOption.get.endpoint.get
        assertEqualsDouble(ev, even, 1e-9)
        assertEqualsDouble(od, odd, 1e-9)
        assert(ev < od, "imputing the worst z lowers PHRF's score where it refused")
      }
    }
    // T-G: pool adds GLMs-D (z 0.3): even PHRF mean = (0.3 + 2.4) / 4 = 0.675
    val g = GatingPair.TTXFastGlmsD
    assertEqualsDouble(est(entry(out, g))._1, k * 0.5 * math.abs((0.8 - 0.3) - (0.675 - 0.3)), 1e-9)

  // ---- item 3: Failed counts in pooled rates ---------------------------------------------------------------

  test("pooled rates count Failed as well as Refused"):
    val corp = ScoreSynth.corpus(withFailures = true)
    val rates = agg(corp).whitelist.pooledRefusals.rates.map(r => r.method -> r).toMap
    val firFailed = corp.condition.valuesIterator.flatMap(_.flatMap(_.arms(Method.Fir))).count { case VoxelOutcome.Failed => true; case _ => false }
    val firRefused = corp.condition.valuesIterator.flatMap(_.flatMap(_.arms(Method.Fir))).count { case VoxelOutcome.Refused => true; case _ => false }
    assert(firFailed > 0 && firRefused == 0)
    assertEqualsDouble(rates(Method.Fir).rate, firFailed.toDouble / rates(Method.Fir).attempted, 1e-15)
    val glmsFailed = corp.trial.valuesIterator.flatMap(_.flatMap(_.arms.get(Method.GlmsD).toVector.flatten)).count(_ == VoxelOutcome.Failed)
    assert(glmsFailed > 0)
    assertEqualsDouble(rates(Method.GlmsD).rate, glmsFailed.toDouble / rates(Method.GlmsD).attempted, 1e-15)

  // ---- item 4: pool membership -----------------------------------------------------------------------------

  test("a GLMs-D arm cannot change any T-TX pair's sigma; it does change the T-G pair"):
    val a = agg(refusedCorpus(glmsZ = 0.3))
    val b = agg(refusedCorpus(glmsZ = -2.0, glmsRefused = true))
    val notGlms = GatingPair.all.filter(p => !p.isCondition && p.comparator != Method.GlmsD)
    notGlms.foreach(p => assertEquals(entry(a, p), entry(b, p)))
    Seq(GatingPair.TTXFastGlmsD, GatingPair.TTXJitGlmsD).foreach(p => assertNotEquals(entry(a, p), entry(b, p)))

  test("conditionPair and trialPair ignore extra arms in ds.arms"):
    val base = ScoreSynth.conditionDataset(PilotCell.CTX5, 3, 0.1, refuse = (m, _, v) => m == Method.Phrf && v == 1)
    val extra = ScoreSynth.ok(
      ConditionDataset.of(3, 1.0, 32.0, base.arms + (Method.Lsa -> Vector.fill(base.voxels)(VoxelOutcome.Estimated(1e9))))
    )
    val r0 = Endpoints.conditionPair(base, GatingPair.CTX5Can)
    val r1 = Endpoints.conditionPair(extra, GatingPair.CTX5Can)
    assertEquals(r1.endpoint, r0.endpoint)
    assertEquals((r1.imputedVoxels, r1.excludedVoxels, r1.completeCaseEndpoint), (r0.imputedVoxels, r0.excludedVoxels, r0.completeCaseEndpoint))
    val ttsBase = ScoreSynth.exactTrialDataset(PilotCell.TTSFast, 0, (m, _) => Z(m), (m, v) => m == Method.Phrf && v == 0)
    val ttsExtra = ScoreSynth.exactTrialDataset(
      PilotCell.TTSFast, 0, (m, _) => if m == Method.GlmsD then -5.0 else Z(m), (m, v) => m == Method.Phrf && v == 0,
      methods = Some(Family.TG.gatingArms)
    )
    assertEquals(
      Endpoints.trialPair(ttsExtra, GatingPair.TTSFastLsa).toOption.get.endpoint,
      Endpoints.trialPair(ttsBase, GatingPair.TTSFastLsa).toOption.get.endpoint
    )

  // ---- item 5: trial planted scan and guarded exceptions ---------------------------------------------------

  private val PlantedT = 0.123456

  private def plantedTrialCorpus(planted: Double, d: Int = 20): (PilotCorpus, Vector[Double]) =
    val e = Vector.tabulate(d)(i => (new Rng(4242L + i).uniform() - 0.5) * 0.2)
    val zc = Vector.tabulate(d)(i => 0.4 + 0.05 * new Rng(777L + i).uniform())
    val c = ScoreSynth.corpus(
      d = d,
      trialFn = Some((cell, dd) =>
        ScoreSynth.exactTrialDataset(cell, dd, (m, _) => if m == Method.Phrf then zc(dd) + planted + e(dd) else zc(dd))
      )
    )
    (c, e)

  test("trial location invariance end to end: a planted delta z leaves the whitelist unchanged (to 1e-9)"):
    val (ca, e) = plantedTrialCorpus(PlantedT)
    val (cb, _) = plantedTrialCorpus(0.654321)
    val a = agg(ca)
    val b = agg(cb)
    val mu = e.sum / e.length
    val sd = math.sqrt(e.map(x => (x - mu) * (x - mu)).sum / (e.length - 1))
    GatingPair.all.filter(!_.isCondition).foreach { p =>
      assertEqualsDouble(est(entry(a, p))._1, sd, 1e-9)
      assertEqualsDouble(est(entry(b, p))._1, est(entry(a, p))._1, 1e-9)
    }
    // condition side and everything else is byte-identical
    val ja = ujson.read(a.whitelistJson)
    val jb = ujson.read(b.whitelistJson)
    assertEquals(ja("icc").toString, jb("icc").toString)
    assertEquals(ja("pooledRefusals").toString, jb("pooledRefusals").toString)
    assertEquals(ja("timing").toString, jb("timing").toString)
    assertEquals(a.runComplete, b.runComplete)

  test("planted trial difference and its mean appear in no text channel, error message or sealed byte"):
    val (corp, _) = plantedTrialCorpus(PlantedT)
    val out = agg(corp)
    val mean = (0 until 20).map(i => Endpoints.trialPair(corp.trial(PilotCell.TTXFast)(i), GatingPair.TTXFastLsa).toOption.get.endpoint.get).sum / 20
    val guarded = PilotAggregator.guard[Int](throw new IllegalStateException("planted 0.123456 and 1.23456e-1"))
    val guardedNested = PilotAggregator.guard[Int](throw new RuntimeException("outer 0.123456", new IllegalArgumentException("inner 0.123456")))
    guarded match
      case Left(ScoreError.Internal(c)) => assertEquals(c, "java.lang.IllegalStateException")
      case other                        => fail(s"expected Internal, got ${other.isRight}")
    val texts = Vector(out.whitelistJson, out.runComplete, out.toString, corp.toString, out.sealedDiagnostics.toString) ++
      Vector(guarded, guardedNested).map(_.left.toOption.get).flatMap(e => Vector(e.message.text, e.toString))
    val digits = Vector(PlantedT, mean).map(_.toString.filter(_.isDigit).dropWhile(_ == '0').take(6))
    texts.foreach(t => digits.foreach(g => assert(!ScoreSynth.textHits(t, g), "planted trial value in a text channel")))
    val raw = new java.io.ByteArrayOutputStream
    corp.trial.valuesIterator.foreach(_.foreach(ds => ds.arms.valuesIterator.foreach(_.foreach(v => v.toOption.foreach(_.values.foreach(x => ScoreSynth.bits(x).head.foreach(b => raw.write(b.toInt))))))))
    val tree = out.sealedDiagnostics.blob ++ raw.toByteArray
    Vector(PlantedT, mean).foreach(p => ScoreSynth.bits(p).foreach(n => assert(!ScoreSynth.containsBytes(tree, n), "planted IEEE bytes in the sealed tree")))

  // ---- item 6: count-free messages -------------------------------------------------------------------------

  test("error messages for the count-carrying constructors are the constructor token only (no digits)"):
    val errs = Vector[ScoreError](
      ScoreError.UnequalDatasetCounts(PilotCell.CTG5, 16, 15), ScoreError.TooFewDatasets(14, 15),
      ScoreError.DfTooSmall(GatingPair.CTX5Can, 13, 14), ScoreError.TooFewValues(ScoreError.Site.Centring, 1, 2)
    )
    assertEquals(errs.map(_.message.text), Vector("UnequalDatasetCounts", "TooFewDatasets", "DfTooSmall", "TooFewValues"))
    errs.foreach { e =>
      assert(!e.message.text.exists(_.isDigit))
      assertEquals(e.toString, e.message.text)
    }
    // the counts stay in the fields
    assertEquals(errs(1), ScoreError.TooFewDatasets(14, 15))
    // and real refusals produced by the aggregator and the corpus constructor carry no digits either
    val real = PilotAggregator.aggregate(ScoreSynth.corpus(d = 20, refusals = false)).isRight
    assert(real)
    val tooFew = PilotCorpus.of(
      PilotCell.conditionCells.map(c => c -> Vector.tabulate(3)(ScoreSynth.conditionDataset(c, _, 0.1))).toMap,
      PilotCell.trialCells.map(c => c -> Vector.tabulate(3)(ScoreSynth.trialDataset(c, _))).toMap,
      Vector.tabulate(3)(ScoreSynth.coverageDataset(_, 50)), ScoreSynth.timing, 1.0
    )
    assert(!tooFew.left.toOption.get.message.text.exists(_.isDigit))

  // ---- item 7: degenerate trial cell -----------------------------------------------------------------------

  test("constant true amplitudes make only that cell's trial sigmas 'degenerate'; the pilot output survives"):
    val base = agg(ScoreSynth.corpus())
    val out = agg(ScoreSynth.corpus(constantTruthCell = Some((PilotCell.TTSFast, 3))))
    val (deg, ok2) = out.whitelist.sigmas.entries.partition(_.value == SigmaValue.Degenerate)
    assertEquals(deg.map(_.pair).toSet, Set(GatingPair.TTSFastLsa, GatingPair.TTSFastLss, GatingPair.TTSFastRlss))
    assertEquals(ok2.length, 20)
    ok2.foreach(e => assertEquals(e, entry(base, e.pair)))
    val j = ujson.read(out.whitelistJson)("sigmas").arr.filter(_("cell").str == "T-TS-fast")
    assert(j.forall(x => x("sigma").str == "degenerate" && x("df").str == "degenerate"))
    // T-TX cell with constant truth also degenerates its GLMs-D pair
    val tx = agg(ScoreSynth.corpus(constantTruthCell = Some((PilotCell.TTXJit, 0))))
    assertEquals(tx.whitelist.sigmas.entries.count(_.value == SigmaValue.Degenerate), 4)
    assert(tx.whitelist.sigmas.entries.filter(_.pair.cell == PilotCell.TTXJit).forall(_.value == SigmaValue.Degenerate))
    assertEquals(tx.whitelist.icc, base.whitelist.icc)

  // ---- item 8: df = 14 boundary ----------------------------------------------------------------------------

  test("df = 14 is accepted, df = 13 is refused (D = 20 with 5 or 6 undefined endpoints)"):
    def corpusWithUndefined(k: Int): PilotCorpus =
      val allRefused: (Method, Int, Int) => Boolean = (_, d, _) => d < k
      ok(
        PilotCorpus.of(
          PilotCell.conditionCells.map { c =>
            c -> Vector.tabulate(20)(d => ScoreSynth.conditionDataset(c, d, 0.1, refuse = if c == PilotCell.CTX5 then allRefused else (_, _, _) => false))
          }.toMap,
          PilotCell.trialCells.map(t => t -> Vector.tabulate(20)(ScoreSynth.trialDataset(t, _))).toMap,
          Vector.tabulate(20)(ScoreSynth.coverageDataset(_, 50)), ScoreSynth.timing, 1.0
        )
      )
    val ok14 = agg(corpusWithUndefined(5))
    assertEquals(entry(ok14, GatingPair.CTX5Can).value match { case SigmaValue.Estimate(_, d) => d; case _ => -1 }, 14)
    assertEquals(PilotAggregator.aggregate(corpusWithUndefined(6)).left.toOption, Some(ScoreError.DfTooSmall(GatingPair.CTX5Can, 13, 14)))
