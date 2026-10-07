package scalafim.phrfcmp.ingest

import scalafim.phrfcmp.ingest.IngestRefusal.*
import scalafim.phrfcmp.ingest.TestSupport.*

class BindingSuite extends munit.FunSuite:

  private val trialExp = Synthetic.expectation(CellKind.Trial)

  private def refusal(npz: Array[Byte], m: ujson.Value, exp: IngestExpectation = trialExp): IngestRefusal =
    PhrfDatasetBinding.bind(npz, Synthetic.render(m), exp) match
      case Left(r)  => r
      case Right(_) => fail("expected a refusal")

  private def ref(t: (Array[Byte], ujson.Obj)): IngestRefusal = refusal(t._1, t._2)

  private def mutate(f: ujson.Obj => Unit): (Array[Byte], ujson.Obj) =
    val (npz, m) = Synthetic.build()
    f(m)
    (npz, m)

  test("trial dataset binds; views carry the arrays; hashes recorded"):
    val (npz, m) = Synthetic.build()
    val b = PhrfDatasetBinding.bind(npz, Synthetic.render(m), trialExp).fold(r => fail(r.message), identity)
    assertEquals(b.inputSha256, Digests.sha256Hex(npz))
    assertEquals(b.arrayHashes.size, 22)
    assertEquals((b.fit.y.rows, b.fit.y.cols), (2, 4))
    assertEqualsDouble(b.fit.y(1, 2), 1.0 + 0.25 * 6, 0.0)
    assertEquals(b.fit.yPool.map(_.rows), Some(1))
    assertEquals(b.fit.runId.toSeq, Seq(0, 0, 1, 1))
    assertEquals(b.truth.trialBeta.map(_.cols), Some(3))
    assertEqualsDouble(b.truth.evOnsetTrue(0), 1.5, 0.0)

  test("condition dataset binds without trial-only arrays"):
    val (npz, m) = Synthetic.build(CellKind.Condition)
    val b = PhrfDatasetBinding.bind(npz, Synthetic.render(m), Synthetic.expectation(CellKind.Condition)).fold(r => fail(r.message), identity)
    assertEquals(b.fit.yPool, None)
    assertEquals(b.truth.trialBeta, None)

  test("npz byte flip: whole-file hash mismatch"):
    val (npz, m) = Synthetic.build()
    val bad = npz.clone()
    bad(60) = (bad(60) ^ 1).toByte
    assert(refusal(bad, m).isInstanceOf[NpzHashMismatch])

  test("npz byte flip with a re-signed whole-file hash is caught by the CRC"):
    val (npz, m) = Synthetic.build()
    val bad = npz.clone()
    bad(60 + 128) = (bad(60 + 128) ^ 1).toByte
    assert(refusal(bad, Synthetic.rehash(bad, m)).isInstanceOf[Npz])

  test("per-array hash mismatch is reported by array name"):
    val (npz, m) = mutate(_("arrays")("y")("npy_sha256") = "0" * 64)
    assertEquals(refusal(npz, m), ArrayHashMismatch("y", "0" * 64, Digests.sha256Hex(Synthetic.arrays(CellKind.Trial).find(_._1 == "y").get._2)))

  test("manifest tamper: npz_sha256"):
    val (npz, m) = mutate(_("npz_sha256") = "f" * 64)
    assert(refusal(npz, m).isInstanceOf[NpzHashMismatch])

  test("manifest tamper: schema, root_kind, root_hex, code hash"):
    assertEquals(ref(mutate(_("schema") = "phrf-gen-npz-2")), SchemaMismatch("phrf-gen-npz-2"))
    assertEquals(ref(mutate(_("root_kind") = "harness")), RootKindMismatch(RootKind.Pilot, "harness"))
    assertEquals(ref(mutate(_("root_kind") = "confirmatory")), RootKindMismatch(RootKind.Pilot, "confirmatory"))
    assertEquals(ref(mutate(_("root_hex") = "00000000000000ac")), RootMismatch(Synthetic.RootHex, "00000000000000ac"))
    assertEquals(ref(mutate(_("generator_code_sha256") = "a" * 64)), GeneratorCodeMismatch(Synthetic.CodeSha, "a" * 64))

  test("unknown root_kind is a typed manifest refusal"):
    assert(ref(mutate(_("root_kind") = "weird")).isInstanceOf[ManifestParse])

  test("denylist: flagged root, recomputed root hit, flipped stream flag, weakened or extra manifest denylist"):
    assertEquals(ref(mutate(_("root_denylisted") = true)), RootDenylisted)
    // Root whose low 32 bits are in the frozen denylist, even with the manifest flag falsified.
    val hitRoot = "ffffffff0000000b"
    val (npz, m) = mutate(_("root_hex") = hitRoot)
    assertEquals(refusal(npz, m, trialExp.copy(rootHex = hitRoot)), RootDenylisted)
    assertEquals(ref(mutate(_("stream_denylist_check")("noise")("hit") = true)), HitFlagMismatch("noise", true, false))
    val weak = Seeds.ProtocolDenylist - 11L
    assertEquals(
      ref(mutate(_("denylist") = ujson.Arr.from(weak.toVector.sorted.map(x => ujson.Num(x.toDouble))))),
      DenylistMismatch(Seeds.ProtocolDenylist, weak)
    )
    assertEquals(
      refusal(npz, Synthetic.build()._2, trialExp.copy(denylist = Seeds.ProtocolDenylist + 12L)),
      DenylistMismatch(Seeds.ProtocolDenylist + 12L, Seeds.ProtocolDenylist)
    )

  test("a genuinely denylisted derived stream seed (low 32 bits) is refused by name"):
    val low = Synthetic.seed(CellKind.Trial, "noise") & 0xffffffffL
    val deny = Seeds.ProtocolDenylist + low
    val (npz, m) = mutate { m =>
      m("denylist") = ujson.Arr.from(deny.toVector.sorted.map(x => ujson.Num(x.toDouble)))
      m("stream_denylist_check")("noise")("hit") = true
    }
    assertEquals(refusal(npz, m, trialExp.copy(denylist = deny)), StreamDenylisted("noise"))

  private def bigSeedPurpose: String =
    Seeds.ManifestPurposes.map(_._1).find(p => BigInt(Seeds.unsigned(Synthetic.seed(CellKind.Trial, p))) > BigInt(2).pow(53)).get

  test("seeds above 2^53 are compared as exact digits; the Double-rounded digits are refused"):
    val p = bigSeedPurpose
    val exact = Seeds.unsigned(Synthetic.seed(CellKind.Trial, p))
    val rounded = BigDecimal(exact.toDouble).toBigInt.toString
    assertNotEquals(rounded, exact) // low digits change under Double rounding
    assertEquals(ref(mutate(_("streams")(p) = rounded)), SeedMismatch(p, exact, rounded))
    assertEquals(ref(mutate(_("stream_denylist_check")(p)("seed") = rounded)), SeedMismatch(p, exact, rounded))
    // A one-unit tamper that is invisible to Double arithmetic is still caught.
    val plusOne = (BigInt(exact) + 1).toString
    val minusOne = (BigInt(exact) - 1).toString
    val invisible = Seq(plusOne, minusOne).find(x => BigInt(x).toDouble == BigInt(exact).toDouble).get
    assertEquals(ref(mutate(_("streams")(p) = invisible)), SeedMismatch(p, exact, invisible))

  test("tampered seed digit, wrong dataset, missing purpose, duplicate streams member"):
    val exact = Seeds.unsigned(Synthetic.seed(CellKind.Trial, "design"))
    val bad = exact.dropRight(1) + (if exact.last == '0' then '1' else '0')
    assertEquals(ref(mutate(_("streams")("design") = bad)), SeedMismatch("design", exact, bad))
    assertEquals(ref(mutate(_("dataset") = 1)), CellMismatch("dataset", "0", "1"))
    assert(ref(mutate(m => { m("streams").obj.remove("truth"); () })).isInstanceOf[ManifestParse])
    val (npz, m2) = Synthetic.build()
    val dup = Synthetic.render(m2).replace("\"streams\"", "\"streams\":{},\"streams\"")
    assert(PhrfDatasetBinding.bind(npz, dup, trialExp).left.exists(_.isInstanceOf[ManifestParse]))

  test("cell spec: id, kind, V, pool, tr, tr_aligned_onsets"):
    assertEquals(ref(mutate(_("cell")("tr_aligned_onsets") = false)), CellMismatch("tr_aligned_onsets", "true", "false"))
    assertEquals(ref(mutate(_("cell")("tr") = 2.0)), CellMismatch("tr", "1.0", "2.0"))
    assertEquals(ref(mutate(_("cell")("n_voxels") = 3)), CellMismatch("n_voxels", "2", "3"))
    assertEquals(ref(mutate(_("cell")("n_pool") = 0)), CellMismatch("n_pool", "1", "0"))
    assertEquals(ref(mutate(_("cell")("cell_id") = "T-OTHER")), CellMismatch("cell_id", "T-SYN", "T-OTHER"))
    assertEquals(ref(mutate(_("cell")("kind") = "condition")), CellMismatch("kind", "Trial", "Condition"))

  test("array table: dtype, shape, undeclared and missing arrays"):
    assertEquals(ref(mutate(_("arrays")("y")("dtype") = "int32")), DtypeMismatch("y", "int32", "float64"))
    assertEquals(
      ref(mutate(_("arrays")("y")("shape") = ujson.Arr(4, 2))),
      ShapeMismatch("y", Vector(4, 2), Vector(2, 4))
    )
    assertEquals(ref(mutate(m => { m("arrays").obj.remove("signal"); () })), ArrayNotInManifest("signal"))
    // Manifest declares an array the npz does not carry.
    val (npz, m) = Synthetic.build()
    val trimmed = zip(Synthetic.arrays(CellKind.Trial).filterNot(_._1 == "signal").sortBy(_._1).map((n, b, _, _) => s"$n.npy" -> b))
    assertEquals(refusal(trimmed, Synthetic.rehash(trimmed, m)), ArrayMissing("signal"))
    assert(npz.nonEmpty)

  test("an npz member outside the schema is refused even when declared and hashed"):
    val extra = npyF64(Seq(1), Seq(1.0))
    val (_, m) = Synthetic.build()
    val withExtra = zip((Synthetic.arrays(CellKind.Trial).map((n, b, _, _) => n -> b) :+ ("zzz" -> extra)).sortBy(_._1).map((n, b) => s"$n.npy" -> b))
    m("arrays")("zzz") = ujson.Obj("dtype" -> "float64", "shape" -> ujson.Arr(1), "npy_sha256" -> Digests.sha256Hex(extra))
    assertEquals(refusal(withExtra, Synthetic.rehash(withExtra, m)), ArrayNotInManifest("zzz"))

  test("inconsistent dimensions across arrays are refused"):
    val (_, m) = Synthetic.build()
    val as = Synthetic.arrays(CellKind.Trial).map {
      case ("ev_cond", _, dt, _) => ("ev_cond", npyI32(Seq(2), Seq(0, 1)), dt, Seq(2))
      case other                 => other
    }
    val npz = zip(as.sortBy(_._1).map((n, b, _, _) => s"$n.npy" -> b))
    m("arrays")("ev_cond") = ujson.Obj("dtype" -> "int32", "shape" -> ujson.Arr(2), "npy_sha256" -> Digests.sha256Hex(as.find(_._1 == "ev_cond").get._2))
    assert(refusal(npz, Synthetic.rehash(npz, m)).isInstanceOf[Inconsistent])

  test("a compressed-member npz that the manifest hashes is still refused"):
    val (_, m) = Synthetic.build()
    val z = zip(Synthetic.arrays(CellKind.Trial).sortBy(_._1).map((n, b, _, _) => s"$n.npy" -> b), ZipOpts(method = 8))
    assertEquals(refusal(z, Synthetic.rehash(z, m)), Npz(NpzError.CompressedMember("cond_coef.npy", 8)))

  test("garbage manifest JSON is a typed refusal"):
    val (npz, _) = Synthetic.build()
    assert(PhrfDatasetBinding.bind(npz, "{not json", trialExp).left.exists(_.isInstanceOf[ManifestParse]))
    assert(PhrfDatasetBinding.bind(npz, "{}", trialExp).left.exists(_.isInstanceOf[ManifestParse]))
