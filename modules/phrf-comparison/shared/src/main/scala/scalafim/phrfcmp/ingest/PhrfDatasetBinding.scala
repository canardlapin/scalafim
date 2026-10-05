package scalafim.phrfcmp.ingest

import scala.util.Try

/** Binds a generator `.npz` to its manifest and the v0-frozen expectation; refuses on any mismatch. */
object PhrfDatasetBinding:

  val SchemaId: String = "phrf-gen-npz-1"

  private val CommonArrays: Set[String] = Set(
    "y", "signal", "nuisance", "nuisance_coef", "sample_time", "run_id", "ev_onset", "ev_onset_true", "ev_cond",
    "ev_stim", "ev_run", "ev_duration", "truth_t", "truth_kernel", "truth_params", "cond_coef", "cond_peak_amp",
    "signal_scale"
  )
  private val TrialArrays: Set[String] = Set("trial_beta", "signal_condition_mean", "y_pool", "nuisance_coef_pool")

  def parseManifest(json: String): Either[IngestRefusal, Manifest] =
    Seeds.scanRaw(json).left.map(IngestRefusal.ManifestParse(_)).flatMap(parseWith(json, _))

  private def parseWith(json: String, raw: Seeds.RawSeeds): Either[IngestRefusal, Manifest] =
    Try {
      val o = ujson.read(json).obj
      val cj = o("cell").obj
      val kind = cj("kind").str match
        case "condition" => CellKind.Condition
        case "trial"     => CellKind.Trial
        case other       => throw new IllegalArgumentException(s"unknown cell.kind '$other'")
      val rootKind = RootKind
        .parse(o("root_kind").str)
        .getOrElse(throw new IllegalArgumentException(s"unknown root_kind '${o("root_kind").str}'"))
      Manifest(
        schema = o("schema").str,
        generatorCodeSha256 = o("generator_code_sha256").str,
        rootKind = rootKind,
        rootHex = o("root_hex").str,
        rootDenylisted = o("root_denylisted").bool,
        denylist = o("denylist").arr.map(_.num.toLong).toSet,
        dataset = o("dataset").num.toInt,
        streams = raw.streams,
        streamCheckSeeds = raw.checkSeeds,
        streamHitFlags = raw.checkHits,
        file = o("file").str,
        npzSha256 = o("npz_sha256").str,
        arrays = o("arrays").obj.iterator.map { (name, a) =>
          name -> ArrayEntry(a.obj("dtype").str, a.obj("shape").arr.map(_.num.toInt).toVector, a.obj("npy_sha256").str)
        }.toMap,
        cell = CellSpec(
          cj("cell_id").str, kind, cj("n_voxels").num.toInt, cj("n_pool").num.toInt, cj("tr").num,
          cj("tr_aligned_onsets").bool
        ),
        nSamples = o("n_samples").num.toInt,
        nEvents = o("n_events").num.toInt
      )
    }.toEither.left.map(e => IngestRefusal.ManifestParse(String.valueOf(e.toString)))

  /** Metadata-only checks of the manifest against the frozen expectation (no array bytes needed). */
  def checkManifest(m: Manifest, e: IngestExpectation): Either[IngestRefusal, Unit] =
    def mism(field: String, exp: Any, got: Any) = Left(IngestRefusal.CellMismatch(field, exp.toString, got.toString))
    if m.schema != SchemaId then Left(IngestRefusal.SchemaMismatch(m.schema))
    else if m.rootKind != e.rootKind then Left(IngestRefusal.RootKindMismatch(e.rootKind, m.rootKind.toString.toLowerCase))
    else if m.rootHex != e.rootHex then Left(IngestRefusal.RootMismatch(e.rootHex, m.rootHex))
    else if m.denylist != e.denylist then Left(IngestRefusal.DenylistMismatch(e.denylist, m.denylist))
    else if m.rootDenylisted || rootHit(e) then Left(IngestRefusal.RootDenylisted)
    else if m.generatorCodeSha256 != e.generatorCodeSha256 then
      Left(IngestRefusal.GeneratorCodeMismatch(e.generatorCodeSha256, m.generatorCodeSha256))
    else
      val c = m.cell
      val x = e.cell
      if c.cellId != x.cellId then mism("cell_id", x.cellId, c.cellId)
      else if c.kind != x.kind then mism("kind", x.kind, c.kind)
      else if c.nVoxels != x.nVoxels then mism("n_voxels", x.nVoxels, c.nVoxels)
      else if c.nPool != x.nPool then mism("n_pool", x.nPool, c.nPool)
      else if c.tr != x.tr then mism("tr", fmt(x.tr), fmt(c.tr))
      else if c.trAlignedOnsets != x.trAlignedOnsets then mism("tr_aligned_onsets", x.trAlignedOnsets, c.trAlignedOnsets)
      else if m.dataset != e.dataset then mism("dataset", e.dataset, m.dataset)
      else checkSeeds(m, e)

  /** Seeds re-derived from the frozen root, cell id and dataset; compared as exact digit strings; hits recomputed. */
  private def checkSeeds(m: Manifest, e: IngestExpectation): Either[IngestRefusal, Unit] =
    parseRoot(e.rootHex) match
      case None => Left(IngestRefusal.ManifestParse(s"expected root '${e.rootHex}' is not 64-bit hex"))
      case Some(root) =>
        val derived = Seeds.ManifestPurposes.map((p, i) => (p, Seeds.streamSeed(root, e.cell.cellId, e.dataset, i)))
        val bad = derived.iterator.flatMap { (p, seed) =>
          val want = Seeds.unsigned(seed)
          val hit = Seeds.denylistHit(seed, e.denylist)
          if m.streams.get(p) != Some(want) then Some(IngestRefusal.SeedMismatch(p, want, m.streams.getOrElse(p, "")))
          else if m.streamCheckSeeds.get(p) != Some(want) then
            Some(IngestRefusal.SeedMismatch(p, want, m.streamCheckSeeds.getOrElse(p, "")))
          else if !m.streamHitFlags.get(p).contains(hit) then
            Some(IngestRefusal.HitFlagMismatch(p, m.streamHitFlags.getOrElse(p, false), hit))
          else if hit then Some(IngestRefusal.StreamDenylisted(p))
          else None
        }
        bad.nextOption().toLeft(())

  private def parseRoot(hex: String): Option[Long] =
    if hex.length == 16 then Try(java.lang.Long.parseUnsignedLong(hex, 16)).toOption else None

  // Double.toString differs between the JVM and Scala.js for whole numbers; refusals must read identically.
  private def fmt(d: Double): String = if d == math.rint(d) && math.abs(d) < 1e15 then s"${d.toLong}.0" else d.toString

  /** The frozen root (and its low 32 bits) against the frozen denylist, recomputed rather than read from the manifest. */
  private def rootHit(e: IngestExpectation): Boolean =
    parseRoot(e.rootHex).forall(r => Seeds.denylistHit(r, e.denylist))

  def bind(npz: Array[Byte], manifestJson: String, expect: IngestExpectation): Either[IngestRefusal, BoundDataset] =
    for
      m <- parseManifest(manifestJson)
      _ <- checkManifest(m, expect)
      actual = Digests.sha256Hex(npz)
      _ <- Either.cond(actual == m.npzSha256, (), IngestRefusal.NpzHashMismatch(m.npzSha256, actual))
      parsed <- Npz.parse(npz).left.map(IngestRefusal.Npz(_))
      hashes <- verifyArrays(parsed, m)
      views <- views(parsed, m)
    yield BoundDataset(m, actual, hashes, views._1, views._2)

  private def verifyArrays(npz: Npz, m: Manifest): Either[IngestRefusal, Map[String, String]] =
    val allowed = CommonArrays ++ (if m.cell.kind == CellKind.Trial then TrialArrays else Set.empty[String])
    val out = Map.newBuilder[String, String]
    val firstBad = npz.members.iterator.map { mem =>
      m.arrays.get(mem.name) match
        case None => Some(IngestRefusal.ArrayNotInManifest(mem.name))
        case Some(_) if !allowed.contains(mem.name) => Some(IngestRefusal.ArrayNotInManifest(mem.name))
        case Some(ent) =>
          val h = Digests.sha256Hex(mem.rawNpy)
          if h != ent.npySha256 then Some(IngestRefusal.ArrayHashMismatch(mem.name, ent.npySha256, h))
          else if ent.dtype != mem.array.dtypeName then
            Some(IngestRefusal.DtypeMismatch(mem.name, ent.dtype, mem.array.dtypeName))
          else if ent.shape != mem.array.shape then
            Some(IngestRefusal.ShapeMismatch(mem.name, ent.shape, mem.array.shape))
          else
            out += mem.name -> h
            None
    }.collectFirst { case Some(r) => r }
    firstBad match
      case Some(r) => Left(r)
      case None =>
        allowed.toVector.sorted.find(n => npz.get(n).isEmpty) match
          case Some(n) => Left(IngestRefusal.ArrayMissing(n))
          case None =>
            m.arrays.keys.toVector.sorted.find(n => npz.get(n).isEmpty) match
              case Some(n) => Left(IngestRefusal.ArrayMissing(n))
              case None    => Right(out.result())

  private def f64(npz: Npz, n: String): Either[IngestRefusal, (Vector[Int], Array[Double])] =
    npz.get(n).map(_.array) match
      case Some(NpyArray.F64(s, d)) => Right((s, d))
      case Some(other)              => Left(IngestRefusal.DtypeMismatch(n, "float64", other.dtypeName))
      case None                     => Left(IngestRefusal.ArrayMissing(n))

  private def i32(npz: Npz, n: String): Either[IngestRefusal, Array[Int]] =
    npz.get(n).map(_.array) match
      case Some(NpyArray.I32(_, d)) => Right(d)
      case Some(other)              => Left(IngestRefusal.DtypeMismatch(n, "int32", other.dtypeName))
      case None                     => Left(IngestRefusal.ArrayMissing(n))

  private def vec(npz: Npz, n: String): Either[IngestRefusal, Array[Double]] = f64(npz, n).map(_._2)

  private def mat(npz: Npz, n: String): Either[IngestRefusal, Matrix] =
    f64(npz, n).flatMap { (s, d) =>
      if s.length == 2 then Matrix.of(s(0), s(1), d)
      else Left(IngestRefusal.ShapeMismatch(n, Vector(0, 0), s))
    }

  private def optMat(npz: Npz, n: String, present: Boolean): Either[IngestRefusal, Option[Matrix]] =
    if present then mat(npz, n).map(Some(_)) else Right(None)

  private def views(npz: Npz, m: Manifest): Either[IngestRefusal, (FitInputs, ScoreTruth)] =
    val trial = m.cell.kind == CellKind.Trial
    for
      y <- mat(npz, "y")
      yPool <- optMat(npz, "y_pool", trial)
      nuis <- mat(npz, "nuisance")
      st <- vec(npz, "sample_time")
      rid <- i32(npz, "run_id")
      eo <- vec(npz, "ev_onset")
      ec <- i32(npz, "ev_cond")
      es <- i32(npz, "ev_stim")
      er <- i32(npz, "ev_run")
      ed <- vec(npz, "ev_duration")
      signal <- mat(npz, "signal")
      eot <- vec(npz, "ev_onset_true")
      tt <- vec(npz, "truth_t")
      tk <- mat(npz, "truth_kernel")
      tp <- mat(npz, "truth_params")
      cc <- mat(npz, "cond_coef")
      cpa <- mat(npz, "cond_peak_amp")
      ss <- vec(npz, "signal_scale")
      nc <- mat(npz, "nuisance_coef")
      ncp <- optMat(npz, "nuisance_coef_pool", trial)
      tb <- optMat(npz, "trial_beta", trial)
      scm <- optMat(npz, "signal_condition_mean", trial)
      _ <- dims(m, y, yPool, nuis, st, rid, eo, ec, es, er, ed, signal, eot, tb, scm, ncp, nc)
    yield (
      FitInputs(y, yPool, nuis, st, rid, eo, ec, es, er, ed),
      ScoreTruth(signal, eot, tt, tk, tp, cc, cpa, ss, nc, ncp, tb, scm)
    )

  // Cross-array consistency: V, T, N and the pool size must agree with the manifest and with each other.
  private def dims(
      m: Manifest, y: Matrix, yPool: Option[Matrix], nuis: Matrix, st: Array[Double], rid: Array[Int],
      eo: Array[Double], ec: Array[Int], es: Array[Int], er: Array[Int], ed: Array[Double], signal: Matrix,
      eot: Array[Double], tb: Option[Matrix], scm: Option[Matrix], ncp: Option[Matrix], nc: Matrix
  ): Either[IngestRefusal, Unit] =
    val V = m.cell.nVoxels
    val T = m.nSamples
    val N = m.nEvents
    val checks: Vector[(String, Boolean)] = Vector(
      "y is V x T" -> (y.rows == V && y.cols == T),
      "signal is V x T" -> (signal.rows == V && signal.cols == T),
      "nuisance has T rows" -> (nuis.rows == T),
      "nuisance_coef is V x P" -> (nc.rows == V && nc.cols == nuis.cols),
      "sample_time and run_id have T entries" -> (st.length == T && rid.length == T),
      "ev_* arrays have N entries" -> Seq(eo.length, ec.length, es.length, er.length, ed.length, eot.length).forall(_ == N),
      "y_pool is n_pool x T" -> yPool.forall(p => p.rows == m.cell.nPool && p.cols == T),
      "nuisance_coef_pool is n_pool x P" -> ncp.forall(p => p.rows == m.cell.nPool && p.cols == nuis.cols),
      "trial_beta is V x N" -> tb.forall(b => b.rows == V && b.cols == N),
      "signal_condition_mean is V x T" -> scm.forall(s => s.rows == V && s.cols == T)
    )
    checks.collectFirst { case (d, false) => IngestRefusal.Inconsistent(d) }.toLeft(())
