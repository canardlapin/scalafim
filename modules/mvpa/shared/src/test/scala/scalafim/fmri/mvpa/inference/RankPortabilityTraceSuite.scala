package scalafim.fmri.mvpa.inference

import gale.backend.Backend.given
import gale.linalg.{DMat, QROptions, QRPivoting}
import multivar.inference.{CanonicalRankMethod, FixedCanonicalRankResult, StepwiseCanonicalRank}
import resample4s.kernel.Seed
import scalafim.fmri.mvpa.analysis.CalibrationBindings
import scala.concurrent.duration.*

/** Trace already exposed resource inputs. This never allocates pilot streams. */
class RankPortabilityTraceSuite extends munit.FunSuite:
  override val munitTimeout: Duration = 15.minutes
  // Immutable exposed resource corpus: docs/verification/umvpa-rank-v2-20261008/probe/inputs.tar.gz
  private val retainedHashes = Set(
    "a2739b0e361a7fc6723f305fd935b50c41b359ca47e215511d754ca3098bf454",
    "720e89e0f0fa1ce9f8f254fcdee91c61edcde8055dd3100c33ec015b5e460dbf",
    "97b695dcd7b76228b19b43dbb1fde97c60f73cc9cda86746d791690c777aaae5",
    "25244769f543bec0fbf02f9186d59449f3a6775f167ff9c371462cdeba53396c",
    "516be5a8024fd9b7920666c8be25eb35fc228a6d150687f31fe013dd2eee8b42",
    "db3a6bf93164889909295ff9e8ce6832481c2388149d53f14cf0de9f53e46e8f",
    "a37455359764b16ab36378d4d701ba2be703ed0c4bc284d200d0d4aef47abbd5",
    "a28514877d64dcdcd1d80baf912af1e9dbd121a7476b76b888563acd6c35727a"
  )
  private def right[A](value: Either[?, A]): A = value.fold(e => fail(e.toString), identity)
  private def matrix(value: DMat): ujson.Value =
    ujson.Arr.from(Vector.tabulate(value.rows * value.cols)(i => value(i / value.cols, i % value.cols)))

  test("opt-in retained fixture traces nuisance QR, residual coordinates and every accepted draw"):
    val list = CalibrationPlatform.environment("SCALAFIM_RANK_TRACE_LIST")
    assume(list.isDefined, "retained trace not requested")
    val prefix = CalibrationPlatform.environment("SCALAFIM_RANK_TRACE_OUTPUT").getOrElse(fail("trace output required"))
    val paths = CalibrationPlatform.readText(list.get).linesIterator.filter(_.nonEmpty).toVector
    assertEquals(paths.size, 8)
    val texts = paths.map(CalibrationPlatform.readText)
    assertEquals(texts.map(CalibrationPlatform.sha256Utf8).toSet, retainedHashes)
    texts.foreach: text =>
      val input = right(CalibrationProtocolSupport.readCase(text,
        "scalafim/umvpa/rank-method-comparison/v2"))
      assertEquals(input.phase, "fixture")
      assertEquals(input.draws, 199)
      val result = right(CalibrationBindings.rank(input.x, input.y, input.nuisance, input.rootSeed,
        input.draws, input.scenario, input.ordinal, CanonicalRankMethod.ScoreOrthogonalPermutationV2))
      val verified = right(RankRecordProvenance.verify(input, result))
      val arithmetic = result.candidateArithmetic match
        case value: FixedCanonicalRankResult => value
        case _ => fail("expected permutation result")
      val basis = result.residualBasis
      val x = right(basis.project(input.x))
      val y = right(basis.project(input.y))
      val problem = right(StepwiseCanonicalRank.from(x, y, 10000000L))
      val qr = result.nuisanceWorkingDesign.qr(QROptions(pivoting = QRPivoting.Column, rankTolerance = Some(1e-12)))
      val draws = arithmetic.sampledCandidateIds.map: id =>
        val action = right(arithmetic.action.draw(Seed.fromLong(verified.seeds.head), id))
        val statistics = right(problem.nullStatistics(action))
        ujson.Obj("candidate" -> id.value, "permutation" -> ujson.Arr.from(action.toIArray.toVector),
          "statistics" -> ujson.Arr.from(statistics))
      val record = ujson.Obj("platform" -> CalibrationPlatform.platform,
        "scenario" -> input.scenario, "input_sha256" -> input.inputSha256,
        "new_datasets" -> 0, "seed" -> verified.seeds.head.toString,
        "rows" -> input.x.rows, "nuisance_columns" -> result.nuisanceWorkingDesign.cols,
        "nuisance" -> matrix(result.nuisanceWorkingDesign), "qr_r" -> matrix(qr.r),
        "qr_reflectors" -> matrix(qr.reflectors),
        "basis" -> matrix(basis.matrix), "projected_x" -> matrix(x), "projected_y" -> matrix(y),
        "observed" -> ujson.Arr.from(problem.observedWilks), "counts" -> ujson.Arr.from(arithmetic.receipts.map(_.exceedances)),
        "draws" -> ujson.Arr.from(draws))
      CalibrationPlatform.appendText(prefix + "-" + CalibrationPlatform.platform + ".jsonl", ujson.write(record) + "\n")
