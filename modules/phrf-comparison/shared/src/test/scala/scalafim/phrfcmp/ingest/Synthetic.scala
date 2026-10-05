package scalafim.phrfcmp.ingest

import scalafim.phrfcmp.ingest.TestSupport.*

/** A tiny generator-shaped dataset (V=2, T=4, N=3, P=2, G=3, pool 1) with a matching manifest, built in-test. */
object Synthetic:
  val CodeSha: String = "c0de" * 16
  val RootHex: String = "00000000000000ab"
  val V = 2
  val T = 4
  val N = 3
  val P = 2
  val G = 3
  val Pool = 1

  def cell(kind: CellKind): CellSpec =
    CellSpec(if kind == CellKind.Trial then "T-SYN" else "C-SYN", kind, V, if kind == CellKind.Trial then Pool else 0, 1.0, kind == CellKind.Trial)

  def expectation(kind: CellKind): IngestExpectation =
    IngestExpectation(RootKind.Pilot, RootHex, CodeSha, cell(kind), 0, Seeds.ProtocolDenylist)

  val Root: Long = java.lang.Long.parseUnsignedLong(RootHex, 16)

  def seed(kind: CellKind, purpose: String): Long =
    Seeds.streamSeed(Root, cell(kind).cellId, 0, Seeds.ManifestPurposes.toMap.apply(purpose))

  /** Compact JSON with the (string-placeholder) seeds written as bare integers, as the generator writes them. */
  def render(m: ujson.Value): String =
    ujson.write(m)
      .replaceAll("\"seed\":\"(\\d+)\"", "\"seed\":$1")
      .replaceAll("\"(design|truth|noise|nullcal)\":\"(\\d+)\"", "\"$1\":$2")

  private def ramp(n: Int, s: Double): Seq[Double] = (0 until n).map(i => s + 0.25 * i)

  def arrays(kind: CellKind): Vector[(String, Array[Byte], String, Seq[Int])] =
    def f(name: String, shape: Seq[Int], s: Double) = (name, npyF64(shape, ramp(shape.product, s)), "float64", shape)
    def i(name: String, shape: Seq[Int], xs: Seq[Int]) = (name, npyI32(shape, xs), "int32", shape)
    val base = Vector(
      f("y", Seq(V, T), 1.0), f("signal", Seq(V, T), 2.0), f("nuisance", Seq(T, P), 3.0), f("nuisance_coef", Seq(V, P), 4.0),
      f("sample_time", Seq(T), 0.0), i("run_id", Seq(T), Seq(0, 0, 1, 1)),
      f("ev_onset", Seq(N), 1.0), f("ev_onset_true", Seq(N), 1.5), i("ev_cond", Seq(N), Seq(0, 1, 2)),
      i("ev_stim", Seq(N), Seq(0, 1, 2)), i("ev_run", Seq(N), Seq(0, 0, 1)), f("ev_duration", Seq(N), 0.0),
      f("truth_t", Seq(G), 0.0), f("truth_kernel", Seq(V, G), 5.0), f("truth_params", Seq(V, 2), 6.0),
      f("cond_coef", Seq(V, 3), 7.0), f("cond_peak_amp", Seq(V, 3), 8.0), f("signal_scale", Seq(V), 9.0)
    )
    val trial = Vector(
      f("trial_beta", Seq(V, N), 10.0), f("signal_condition_mean", Seq(V, T), 11.0),
      f("y_pool", Seq(Pool, T), 12.0), f("nuisance_coef_pool", Seq(Pool, P), 13.0)
    )
    if kind == CellKind.Trial then base ++ trial else base

  /** Returns the npz bytes and the JSON manifest (as a mutable ujson tree) that matches them. */
  def build(kind: CellKind = CellKind.Trial): (Array[Byte], ujson.Obj) =
    val as = arrays(kind)
    val npz = zip(as.sortBy(_._1).map((n, b, _, _) => s"$n.npy" -> b))
    val c = cell(kind)
    val m = ujson.Obj(
      "schema" -> "phrf-gen-npz-1",
      "generator_code_sha256" -> CodeSha,
      "root_kind" -> "pilot",
      "root_hex" -> RootHex,
      "root_denylisted" -> false,
      "denylist" -> ujson.Arr.from(Seeds.ProtocolDenylist.toVector.sorted.map(x => ujson.Num(x.toDouble))),
      "dataset" -> 0,
      "streams" -> ujson.Obj.from(Seeds.ManifestPurposes.map((p, _) => p -> ujson.Str(Seeds.unsigned(seed(kind, p))))),
      "stream_denylist_check" -> ujson.Obj.from(Seeds.ManifestPurposes.map { (p, _) =>
        p -> ujson.Obj("hit" -> false, "seed" -> ujson.Str(Seeds.unsigned(seed(kind, p))))
      }),
      "file" -> s"${c.cellId}__d0000.npz",
      "npz_sha256" -> Digests.sha256Hex(npz),
      "arrays" -> ujson.Obj.from(as.map { (n, b, dt, sh) =>
        n -> ujson.Obj("dtype" -> dt, "shape" -> ujson.Arr.from(sh), "npy_sha256" -> Digests.sha256Hex(b))
      }),
      "cell" -> ujson.Obj(
        "cell_id" -> c.cellId, "kind" -> (if kind == CellKind.Trial then "trial" else "condition"),
        "n_voxels" -> c.nVoxels, "n_pool" -> c.nPool, "tr" -> c.tr, "tr_aligned_onsets" -> c.trAlignedOnsets
      ),
      "n_samples" -> T,
      "n_events" -> N
    )
    (npz, m)

  /** Re-derive the npz hash in a manifest after the npz bytes were deliberately changed. */
  def rehash(npz: Array[Byte], m: ujson.Obj): ujson.Obj =
    m("npz_sha256") = Digests.sha256Hex(npz)
    m
