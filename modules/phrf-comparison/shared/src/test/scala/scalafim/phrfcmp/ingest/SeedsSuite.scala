package scalafim.phrfcmp.ingest

class SeedsSuite extends munit.FunSuite:

  test("derivation matches phrf_gen/seeds.py (HARNESS root, T-TX-fast, dataset 0)"):
    val root = java.lang.Long.parseUnsignedLong("7a3c91d50b44e2f1", 16)
    val got = Seeds.ManifestPurposes.map((p, i) => p -> Seeds.unsigned(Seeds.streamSeed(root, "T-TX-fast", 0, i))).toMap
    assertEquals(
      got,
      Map(
        "design" -> "7445587948497063369",
        "truth" -> "3236142379493101838",
        "noise" -> "6355680464947817139",
        "nullcal" -> "3246703383566900510"
      )
    )

  test("denylist hit includes the low 32 bits"):
    assert(Seeds.denylistHit(11L, Seeds.ProtocolDenylist))
    assert(Seeds.denylistHit(0x1234567800000000L | 101L, Seeds.ProtocolDenylist))
    assert(!Seeds.denylistHit(0x1234567800000000L | 12L, Seeds.ProtocolDenylist))

  test("raw scan keeps digits above 2^53 and refuses duplicates and malformed blocks"):
    val j = """{"streams": {"design": 7445587948497063369, "truth": 1, "noise": 2, "nullcal": 3},
              | "stream_denylist_check": {"design": {"hit": false, "seed": 7445587948497063369},
              | "truth": {"hit": true, "seed": 1}, "noise": {"hit": false, "seed": 2}, "nullcal": {"hit": false, "seed": 3}}}""".stripMargin
    val r = Seeds.scanRaw(j).fold(e => fail(e), identity)
    assertEquals(r.streams("design"), "7445587948497063369")
    assertEquals(r.checkSeeds("design"), "7445587948497063369")
    assertEquals(r.checkHits("truth"), true)
    assert(Seeds.scanRaw("{}").isLeft)
    assert(Seeds.scanRaw(j.replace("\"truth\": 1,", "\"truth\": 1.5,")).isLeft)
