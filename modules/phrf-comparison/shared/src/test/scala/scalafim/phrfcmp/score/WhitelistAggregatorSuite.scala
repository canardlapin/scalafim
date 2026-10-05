package scalafim.phrfcmp.score

import ScoreSynth.{ok, Rng}

class WhitelistAggregatorSuite extends munit.FunSuite:
  private val Planted = 0.123456

  private def agg(c: PilotCorpus): AggregationOutput = ok(PilotAggregator.aggregate(c))

  private def sigmaOf(e: SigmaEntry): Double = e.value match
    case SigmaValue.Estimate(s, _) => s
    case SigmaValue.Degenerate     => Double.NaN
  private def dfOf(e: SigmaEntry): Int = e.value match
    case SigmaValue.Estimate(_, d) => d
    case SigmaValue.Degenerate     => -1

  // ---- schema closure --------------------------------------------------------------------------------------

  private def jsonPaths(v: ujson.Value, prefix: String): Set[String] = v match
    case o: ujson.Obj => o.value.toSet.flatMap((k, x) => jsonPaths(x, s"$prefix.$k"))
    case a: ujson.Arr => a.value.toSet.flatMap(x => jsonPaths(x, s"$prefix[]"))
    case _            => Set(prefix)

  private val GoldenPaths: Set[String] =
    val timing = Set("median", "min", "max")
    Set("$.schema") ++
      Set("cell", "comparator", "sigma", "df").map(k => s"$$.sigmas[].$k") ++
      Set("cell", "signedRelativeEPeakError", "tauError", "coverageIndicator").map(k => s"$$.icc.$k") ++
      Set("method", "rate", "attempted").map(k => s"$$.pooledRefusals[].$k") ++
      Set("trialMlPerVoxel", "trialPreparationPerAlpha", "glmsingleDataset", "coldConditionPreparation").flatMap(q => timing.map(t => s"$$.timing.$q.$t")) ++
      Set("$.timing.alphaCache.status", "$.timing.alphaCache.flatnessRatio")

  test("schema closure: the JSON key paths are exactly the pre-registered set (ICC fields may be 'degenerate')"):
    val out = agg(ScoreSynth.corpus(coveragePct = 50))
    def collapse(j: String) =
      jsonPaths(ujson.read(j), "$").map(p => p.replaceAll("""\.(signedRelativeEPeakError|tauError|coverageIndicator)\.(icc|upper80)$""", ".$1"))
    assertEquals(collapse(out.whitelistJson), GoldenPaths)
    // every ICC field renders as an object here; the degenerate form is a bare string
    val degenerate = agg(ScoreSynth.corpus(coveragePct = 99))
    assertEquals(collapse(degenerate.whitelistJson), GoldenPaths)

  private def walk(x: Any, nodes: scala.collection.mutable.Set[String]): Unit = x match
    case _: Double | _: Int | _: Long => ()
    case v: Vector[?]                => v.foreach(walk(_, nodes))
    case p: Product                  =>
      nodes += p.productPrefix
      p.productIterator.foreach(walk(_, nodes))
    case other => fail(s"free-form value in the whitelist ADT: ${other.getClass.getName}")

  test("schema closure: the whitelist ADT holds only numbers, vectors and closed case classes or enums"):
    val nodes = scala.collection.mutable.Set.empty[String]
    walk(agg(ScoreSynth.corpus()).whitelist, nodes)
    val expected = Set(
      "PilotWhitelist", "SigmaTable", "SigmaEntry", "IccBlock", "Estimate", "PooledRefusals", "PooledRate", "TimingBlock", "TimingSummary",
      "NotPossibleViaPublicApi"
    ) ++ GatingPair.values.map(_.productPrefix) ++ Method.values.map(_.productPrefix) ++ PilotCell.values.map(_.productPrefix)
    assertEquals(nodes.toSet -- expected, Set.empty[String])
    assert(GatingPair.all.forall(p => nodes.contains(p.productPrefix)))

  test("the sigma table refuses anything but the 23 gating pairs"):
    intercept[IllegalArgumentException](SigmaTable(Vector(SigmaEntry(GatingPair.CTX5Can, SigmaValue.Estimate(1.0, 19)))))

  // ---- values ----------------------------------------------------------------------------------------------

  test("sigma of centred lambda_d equals the independent computation; df = D - 1"):
    val out = agg(ScoreSynth.corpus(d = 20))
    val es = (0 until 20).map(d => (new Rng(1000L * PilotCell.CTX5.ordinal + d).uniform() - 0.5) * 0.2)
    val mu = es.sum / es.length
    val sd = math.sqrt(es.map(e => (e - mu) * (e - mu)).sum / 19)
    val entry = out.whitelist.sigmas.entries.find(_.pair == GatingPair.CTX5Can).get
    assertEqualsDouble(sigmaOf(entry), sd, 1e-12)
    assertEquals(dfOf(entry), 19)
    assertEquals(out.whitelist.sigmas.entries.length, 23)
    assert(out.whitelist.sigmas.entries.filter(_.pair.isCondition).forall(e => dfOf(e) == 19))

  test("UCL factor from the realized df (about 1.18 at 19)"):
    val df = dfOf(agg(ScoreSynth.corpus(d = 20)).whitelist.sigmas.entries.head)
    assertEqualsDouble(Dist.sigmaUclFactor(df), 1.1769727057611257, 1e-10)

  test("refusal below D = 15, and a pair with fewer than 14 df is refused, not extrapolated"):
    val tooFew = PilotCorpus.of(
      PilotCell.conditionCells.map(c => c -> Vector.tabulate(14)(ScoreSynth.conditionDataset(c, _, Planted))).toMap,
      PilotCell.trialCells.map(c => c -> Vector.tabulate(14)(ScoreSynth.trialDataset(c, _))).toMap,
      Vector.tabulate(14)(ScoreSynth.coverageDataset(_, 50)),
      ScoreSynth.timing,
      1.0
    )
    assertEquals(tooFew.left.toOption, Some(ScoreError.TooFewDatasets(14, 15)))
    // C-TX-.5: six datasets where every arm refuses every voxel -> only 14 defined endpoints -> df 13
    val allRefused: (Method, Int, Int) => Boolean = (_, d, _) => d < 6
    val cond = PilotCell.conditionCells.map { c =>
      c -> Vector.tabulate(20)(d => ScoreSynth.conditionDataset(c, d, Planted, refuse = if c == PilotCell.CTX5 then allRefused else (_, _, _) => false))
    }.toMap
    val c = ok(
      PilotCorpus.of(
        cond,
        PilotCell.trialCells.map(t => t -> Vector.tabulate(20)(ScoreSynth.trialDataset(t, _))).toMap,
        Vector.tabulate(20)(ScoreSynth.coverageDataset(_, 50)),
        ScoreSynth.timing,
        1.0
      )
    )
    assertEquals(PilotAggregator.aggregate(c).left.toOption, Some(ScoreError.DfTooSmall(GatingPair.CTX5Can, 13, 14)))

  test("corpus shape errors are typed"):
    val c = ScoreSynth.corpus(d = 15)
    assertEquals(c.datasets, 15)
    val cond = PilotCell.conditionCells.map(x => x -> Vector.tabulate(15)(ScoreSynth.conditionDataset(x, _, Planted))).toMap
    val trial = PilotCell.trialCells.map(x => x -> Vector.tabulate(15)(ScoreSynth.trialDataset(x, _))).toMap
    assertEquals(
      PilotCorpus.of(cond - PilotCell.CTS1, trial, Vector.tabulate(15)(ScoreSynth.coverageDataset(_, 50)), ScoreSynth.timing, 1.0).left.toOption,
      Some(ScoreError.CellMissing(PilotCell.CTS1))
    )
    assertEquals(
      PilotCorpus.of(cond, trial, Vector.tabulate(16)(ScoreSynth.coverageDataset(_, 50)), ScoreSynth.timing, 1.0).left.toOption,
      Some(ScoreError.UnequalDatasetCounts(PilotCell.CTG5, 16, 15))
    )

  // ---- coverage ICC degenerate rule ------------------------------------------------------------------------

  private def iccOf(pct: Int): IccBlock = agg(ScoreSynth.corpus(coveragePct = pct)).whitelist.icc

  test("coverage ICC: emitted only when the pooled proportion lies in [0.05, 0.95], else 'degenerate'"):
    assert(iccOf(50).coverageIndicator.isInstanceOf[IccValue.Estimate])
    assertEquals(iccOf(99).coverageIndicator, IccValue.Degenerate)
    assertEquals(iccOf(100).coverageIndicator, IccValue.Degenerate)
    assertEquals(iccOf(1).coverageIndicator, IccValue.Degenerate)
    assertEquals(iccOf(0).coverageIndicator, IccValue.Degenerate)
    // the error ICCs do not depend on the coverage proportion rule
    assert(iccOf(99).tauError.isInstanceOf[IccValue.Estimate])

  test("coverage ICC: the degenerate field is byte-identical on both sides of the band"):
    def field(pct: Int): String = ujson.read(agg(ScoreSynth.corpus(coveragePct = pct)).whitelistJson)("icc")("coverageIndicator").toString
    assertEquals(field(99), field(1))
    assertEquals(field(100), "\"degenerate\"")

  test("coverage band edges: exactly 0.05 and 0.95 are emitted, just outside is degenerate"):
    // build clusters of 20 voxels with an exact pooled proportion
    def corpusWithCovered(k: Int): PilotCorpus =
      val base = ScoreSynth.corpus(d = 20)
      val cov = Vector.tabulate(20) { d =>
        val rng = new Rng(9000L + d)
        ok(CoverageDataset.of(d, Vector.tabulate(20)(v => VoxelOutcome.Estimated(ok(CoverageObs.of(rng.normal(), rng.normal(), d * 20 + v < k))))))
      }
      ok(PilotCorpus.of(base.condition, base.trial, cov, base.timing, base.totalCpuSeconds))
    val n = 400
    def cov(k: Int) = agg(corpusWithCovered(k)).whitelist.icc.coverageIndicator
    assert(cov((0.05 * n).toInt).isInstanceOf[IccValue.Estimate]) // 20/400 = 0.05
    assert(cov((0.95 * n).toInt).isInstanceOf[IccValue.Estimate]) // 380/400 = 0.95
    assertEquals(cov(19), IccValue.Degenerate)
    assertEquals(cov(381), IccValue.Degenerate)

  // ---- location invariance ---------------------------------------------------------------------------------

  test("location invariance: Centred ignores a constant added to every value (exact on dyadic data)"):
    val xs = Vector(0.5, 1.25, -0.75, 2.0, 0.125, -1.5, 3.0)
    val a = ok(Centred.of(xs)).sd
    assertEquals(ok(Centred.of(xs.map(_ + 0.25))).sd, a)
    assertEquals(ok(Centred.of(xs.map(_ - 3.0))).sd, a)
    assertEquals(ok(Centred.of(xs)).df, 6)

  test("location invariance: a constant offset in a comparator's log-MISE leaves the whitelist byte-identical"):
    val base = agg(ScoreSynth.corpus())
    // multiplying CAN's errors by 2 shifts every CAN lambda_d by -ln 2; no refusals, so imputation is not coupled
    val shifted = agg(ScoreSynth.corpus(compScale = Map(Method.Can -> 2.0)))
    assertEquals(shifted.whitelistJson, base.whitelistJson)
    assertEquals(shifted.whitelistSha256, base.whitelistSha256)

  test("location invariance: the planted difference does not change the whitelist"):
    val a = agg(ScoreSynth.corpus(planted = 0.123456))
    val b = agg(ScoreSynth.corpus(planted = 0.654321))
    assertEquals(a.whitelistJson, b.whitelistJson)

  // ---- planted-mean scan -----------------------------------------------------------------------------------

  private def bits(d: Double): Vector[Vector[Byte]] =
    val l = java.lang.Double.doubleToLongBits(d)
    val be = Vector.tabulate(8)(i => ((l >>> (8 * (7 - i))) & 0xffL).toByte)
    Vector(be, be.reverse)

  private def containsBytes(hay: Array[Byte], needle: Vector[Byte]): Boolean =
    hay.indices.exists(i => i + needle.length <= hay.length && needle.indices.forall(j => hay(i + j) == needle(j)))

  private def textHits(text: String, planted: String): Boolean =
    val tokens = """-?[0-9]+(\.[0-9]+)?([eE][-+]?[0-9]+)?""".r.findAllIn(text).toVector
    text.contains("0." + planted) || tokens.exists(t => t.filter(_.isDigit).dropWhile(_ == '0').startsWith(planted))

  test("planted-mean scan: whitelist text, messages, toStrings, run-complete line and sealed bytes carry no planted value"):
    val d = 20
    val corp = ScoreSynth.corpus(d = d, planted = Planted, refusals = true)
    val out = agg(corp)
    val trialMean = (0 until d).map(i => ok(Endpoints.trialPair(corp.trial(PilotCell.TTXFast)(i), GatingPair.TTXFastLsa)).endpoint.get).sum / d
    val lambdaMean = (0 until d).map(i => Endpoints.conditionPair(corp.condition(PilotCell.CTX5)(i), GatingPair.CTX5Can).endpoint.get).sum / d
    val planteds = Vector(Planted, lambdaMean, trialMean)
    // text channels
    val forcedErrors = Vector[ScoreError](
      ScoreError.TooFewDatasets(14, 15), ScoreError.DfTooSmall(GatingPair.CTX5Can, 13, 14), ScoreError.NonFiniteValue(ScoreError.Site.Centring),
      ScoreError.Internal(new RuntimeException("0.123456 secret").getClass.getName), ScoreError.TruthDegenerate(PilotCell.TTXJit)
    ).map(_.message.text)
    val texts = Vector(out.whitelistJson, out.runComplete, out.toString, corp.toString, out.sealedDiagnostics.toString, corp.condition(PilotCell.CTX5).head.toString,
      corp.trial(PilotCell.TTXFast).head.toString, corp.coverage.head.toString, corp.timing.toString) ++ forcedErrors
    val digits = Vector(Planted, lambdaMean, trialMean).map(_.toString.filter(_.isDigit).dropWhile(_ == '0').take(6))
    texts.foreach(t => digits.foreach(g => assert(!textHits(t, g), "planted or mean decimal in a text channel")))
    // binary channel: sealed diagnostics plus a test-side dump of the raw corpus doubles (the "sealed tree")
    val raw = new java.io.ByteArrayOutputStream
    def dump(x: Double): Unit = bits(x).head.foreach(b => raw.write(b.toInt))
    corp.condition.valuesIterator.foreach(_.foreach(ds => ds.arms.valuesIterator.foreach(_.foreach(v => v.toOption.foreach(dump)))))
    corp.trial.valuesIterator.foreach(_.foreach(ds => ds.arms.valuesIterator.foreach(_.foreach(v => v.toOption.foreach(_.values.foreach(dump))))))
    val tree = out.sealedDiagnostics.blob ++ raw.toByteArray
    planteds.foreach(p => bits(p).foreach(n => assert(!containsBytes(tree, n), "planted IEEE bytes in the sealed tree")))

  test("redaction: error messages carry constructor names and integers only; exception class tokens drop messages"):
    val m = ScoreError.Internal("java.lang.IllegalStateException").message.text
    assertEquals(m, "Internal.java.lang.IllegalStateException")
    assertEquals(SafeMessage.classToken("a b/c$1"), "a_b_c_1")
    intercept[IllegalArgumentException](SafeMessage.of("bad name with spaces"))
    assertEquals(ScoreError.TooFewDatasets(3, 15).message.text, "TooFewDatasets")

  test("run-complete line is byte-identical across corpora with different planted differences"):
    val a = agg(ScoreSynth.corpus(planted = 0.123456, totalCpuSeconds = 5400.0))
    val b = agg(ScoreSynth.corpus(planted = 0.654321, totalCpuSeconds = 5400.0))
    assertEquals(a.runComplete, b.runComplete)
    assertEquals(a.runComplete, "run complete; total CPU hours: 1.50")

  test("guarded aggregation never throws"):
    assert(PilotAggregator.aggregateGuarded(ScoreSynth.corpus(d = 15)).isRight)

  // ---- pooled rates, timing --------------------------------------------------------------------------------

  test("pooled refusal rate is per method over all its cells, with the denominator"):
    val corp = ScoreSynth.corpus(d = 20, refusals = true)
    val out = agg(corp)
    val rates = out.whitelist.pooledRefusals.rates.map(r => r.method -> r).toMap
    // PHRF ran in 4 condition (8 voxels) + 3 trial cells (6 voxels) x 20 datasets
    assertEquals(rates(Method.Phrf).attempted, (4L * 8 + 3L * 6) * 20)
    val manual = corp.condition.valuesIterator.flatMap(_.flatMap(_.arms(Method.Phrf))).count(_.isMissing) +
      corp.trial.valuesIterator.flatMap(_.flatMap(_.arms(Method.Phrf))).count(_.isMissing)
    assertEqualsDouble(rates(Method.Phrf).rate, manual.toDouble / rates(Method.Phrf).attempted, 1e-15)
    assertEquals(rates(Method.GlmsD).attempted, 2L * 6 * 20) // only the two T-TX cells
    assertEquals(rates(Method.GlmsD).rate, 0.0)

  test("timing summaries are medians and ranges; the cache report carries the flatness ratio"):
    val t = agg(ScoreSynth.corpus()).whitelist.timing
    assertEquals(t.trialMlPerVoxel, TimingSummary(0.023, 0.021, 0.025))
    assertEquals(t.glmsingleDataset, TimingSummary(10.0, 9.0, 12.0))
    t.alphaCache match
      case AlphaCacheReport.NotPossibleViaPublicApi(r) => assertEqualsDouble(r, 0.5 * (0.55 / 0.5 + 0.62 / 0.6), 1e-12)

  test("complete-case and imputed sigma differ when refusals are present (complete-case is sealed only)"):
    val refused = agg(ScoreSynth.corpus(refusals = true))
    val clean = agg(ScoreSynth.corpus())
    assert(!java.util.Arrays.equals(refused.sealedDiagnostics.blob, clean.sealedDiagnostics.blob))
    assert(!refused.whitelistJson.contains("completeCase"))

  test("whitelist writer is deterministic: golden prefix and stable hash"):
    val out = agg(ScoreSynth.corpus())
    assert(out.whitelistJson.startsWith("""{"schema":"phrf-cmp-pilot-whitelist-1","sigmas":[{"cell":"C-TX-.5","comparator":"CAN","sigma":"""))
    assertEquals(out.whitelistSha256, agg(ScoreSynth.corpus()).whitelistSha256)
    assertEquals(out.whitelistSha256.length, 64)

  test("number rendering is locale-free and platform-identical"):
    assertEquals(Fmt.sci12(0.123456), "1.234560000000e-01")
    assertEquals(Fmt.sci12(-1234.5), "-1.234500000000e+03")
    assertEquals(Fmt.sci12(0.0), "0.000000000000e+00")
    assertEquals(Fmt.sci12(1.0), "1.000000000000e+00")
    assertEquals(Fmt.sci12(9.9999999999999e-5), "1.000000000000e-04") // rounds up across a decade
    assertEquals(Fmt.fixed2(1.5), "1.50")
    assertEquals(Fmt.fixed2(0.004), "0.00")
