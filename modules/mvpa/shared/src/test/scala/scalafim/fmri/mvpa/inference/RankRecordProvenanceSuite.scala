package scalafim.fmri.mvpa.inference

import multivar.inference.*
import resample4s.kernel.Seed
import resample4s.spi.AlgorithmId

class RankRecordProvenanceSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(e => fail(e.toString), identity)
  private val gaussian = CanonicalRankMethod.GaussianInterlacingWilksV1
  private val permutation = CanonicalRankMethod.ScoreOrthogonalPermutationV2
  private def receipt(seed: Long, algorithm: String): MonteCarloReceipt =
    val draws = Vector.tabulate(3)(i => right(ReplicateStatistic.from(right(ReplicateId(i)), i.toDouble)))
    right(MonteCarlo.fixed(1.5, Alternative.Greater, draws,
      ReplicateProvenance.Deterministic(Seed.fromLong(seed), right(AlgorithmId.of(algorithm)), Seed.derivationAlgorithm)))

  test("each Gaussian hypothesis checks its own independently derived seed; legacy permutation uses its root"):
    // Independent Python seed-path reference, root=42, method tag 103, hypothesis tag 1011.
    val seeds = Vector(1093620216397299100L, -8361707105971832312L)
    val receipts = seeds.map(receipt(_, GaussianCanonicalRank.referenceAlgorithm.value))
    val checked = right(RankRecordProvenance.verifyReceipts(42L, 3, gaussian, receipts))
    assertEquals(checked.seeds, seeds)
    assert(RankRecordProvenance.verifyReceipts(43L, 3, gaussian, receipts).isLeft)
    assert(RankRecordProvenance.verifyReceipts(42L, 4, gaussian, receipts).isLeft)
    assert(RankRecordProvenance.verifyReceipts(42L, 3, gaussian, receipts.reverse).isLeft)
    assert(RankRecordProvenance.verifyReceipts(42L, 3, permutation, receipts).isLeft)
    assert(RankRecordProvenance.verifyReceipts(42L, 3, gaussian, receipts.map(_.copy(provenance = ReplicateProvenance.External))).isLeft)
    assert(RankRecordProvenance.verifyReceipts(42L, 3, gaussian, receipts.map(_.copy(stopReason = MonteCarloStopReason.NonRejectionEarly))).isLeft)
    val old = Vector.fill(2)(receipt(5765800120822435668L, "canonical-rank-distinct-nonidentity/v1"))
    assertEquals(right(RankRecordProvenance.verifyReceipts(42L, 3, permutation, old)).seeds, Vector.fill(2)(5765800120822435668L))
    val metrics = right(CalibrationProtocolSupport.rankMetrics(Vector(.5, .5), Vector(.5, .5), Vector(1, 1), 3))
    val json = ujson.read("{" + metrics.jsonFields(checked).drop(1) + "}")
    assertEquals(json("record_schema").num, 2.0)
    assertEquals(json("provenance_version").num, 1.0)
    assertEquals(json("raw_exceedances").arr.map(_.num).toVector, Vector(1.0, 1.0))
    assertEquals(json("receipt_seed64").arr.map(_.str).toVector, seeds.map(_.toString))

  test("source lock binds exact bytes and rejects absent, invalid, or changed inputs"):
    val bytes = "source κ\n"
    val lock = ujson.write(ujson.Obj("source.scala" -> CalibrationPlatform.sha256Utf8(bytes)))
    val source = right(RankRecordProvenance.SourceSnapshot.from("a" * 40, lock))
    assertEquals(source.lockSha256, CalibrationPlatform.sha256Utf8(lock))
    assert(source.verify(_ => bytes).isRight)
    assert(source.verify(_ => bytes + "\n").isLeft)
    assert(RankRecordProvenance.SourceSnapshot.from("a" * 7, lock).isLeft)
    assert(RankRecordProvenance.SourceSnapshot.from("a" * 40, "{}").isLeft)
    assert(RankRecordProvenance.SourceSnapshot.from("a" * 40, "{\"../source\":\"" + "b" * 64 + "\"}").isLeft)

  test("case identity hashes the exact input text, including an otherwise ignored blank line"):
    val root = right(CalibrationProtocolSupport.seed("fixture", "provenance-fixture", 0))
    val text = s"case\tfixture\tprovenance-fixture\t0\t$root\t3\t3\nX\t3\t1\t1,2,3\nY\t3\t1\t3,2,1\nZ\t3\t1\t1,1,1\nrho\t0\n"
    val input = right(CalibrationProtocolSupport.readCase(text))
    assertEquals(input.inputSha256, CalibrationPlatform.sha256Utf8(text))
    assertNotEquals(input.inputSha256, right(CalibrationProtocolSupport.readCase(text + "\n")).inputSha256)

  test("production rank adapters bind the input identity and their actual method receipts"):
    val scenario = "provenance-adapter-fixture"
    val root = right(CalibrationProtocolSupport.seed("fixture", scenario, 0))
    val x = Vector.tabulate(48)(i => math.sin((i / 2 + 1) * (if i % 2 == 0 then .7 else .3))).mkString(",")
    val y = Vector.tabulate(48)(i => math.cos((i / 2 + 1) * (if i % 2 == 0 then .5 else .11))).mkString(",")
    val z = Vector.fill(24)(1.0).mkString(",")
    val text = s"case\tfixture\t$scenario\t0\t$root\t3\t24\nX\t24\t2\t$x\nY\t24\t2\t$y\nZ\t24\t1\t$z\nrho\t0,0\n"
    val input = right(CalibrationProtocolSupport.readCase(text))
    Vector(gaussian, permutation).foreach: method =>
      val fitted = right(scalafim.fmri.mvpa.analysis.CalibrationBindings.rank(
        input.x, input.y, input.nuisance, input.rootSeed, input.draws, input.scenario, input.ordinal, method))
      val verified = right(RankRecordProvenance.verify(input, fitted))
      assertEquals(verified.observed, fitted.candidateArithmetic.receipts.map(_.observed))
      assert(RankRecordProvenance.verify(input.copy(ordinal = 1), fitted).isLeft)
      assert(RankRecordProvenance.verify(input.copy(rootSeed = root + 1), fitted).isLeft)
      assert(RankRecordProvenance.verify(input.copy(draws = 4), fitted).isLeft)
