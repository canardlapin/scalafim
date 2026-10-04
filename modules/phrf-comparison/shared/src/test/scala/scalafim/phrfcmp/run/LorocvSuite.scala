package scalafim.phrfcmp.run

import scalafim.phrfcmp.ingest.Matrix

class LorocvSuite extends munit.FunSuite:

  private def mat(rows: Int, cols: Int)(f: (Int, Int) => Double): Matrix =
    Matrix.of(rows, cols, Array.tabulate(rows * cols)(k => f(k / cols, k % cols))).fold(e => fail(e.message), identity)

  private val runId = Array.tabulate(40)(_ / 10)

  test("folds leave exactly one run out, every run once, with training and test rows partitioning the time axis"):
    val folds = Lorocv.folds(runId).fold(e => fail(e.message), identity)
    assertEquals(folds.map(_.heldOut), Vector(0, 1, 2, 3))
    folds.foreach { f =>
      assertEquals(f.testRows.toVector, runId.indices.filter(runId(_) == f.heldOut).toVector)
      assert(f.trainRows.forall(runId(_) != f.heldOut))
      assertEquals(f.trainRuns, Vector(0, 1, 2, 3).filterNot(_ == f.heldOut))
      assertEquals((f.trainRows ++ f.testRows).sorted.toVector, runId.indices.toVector)
      assertEquals(f.trainRows.map(runId(_)).distinct.sorted.toVector, f.trainRuns)
    }

  test("fewer than two runs refuses"):
    assertEquals(Lorocv.folds(Array.fill(10)(0)), Left(LorocvRefusal.TooFewRuns(1)))
    assertEquals(Lorocv.folds(Array.empty[Int]), Left(LorocvRefusal.TooFewRuns(0)))

  test("selection takes the largest mean fold score; an exact tie goes to the smaller alpha"):
    val g = AlphaGrid.Pilot
    val peak = Vector.tabulate(g.size)(k => -math.abs(k - 5.0))
    val sel = Lorocv.select(g, Vector(peak, peak)).fold(e => fail(e.message), identity)
    assertEquals(sel.selected, 5)
    assertEquals(sel.selectedAlpha, g.alpha(5))
    assert(!sel.atLowerEnd && !sel.atUpperEnd)
    val flat = Vector.fill(g.size)(0.25)
    assertEquals(Lorocv.select(g, Vector(flat)).map(_.selected), Right(0))
    val twoWay = Vector.tabulate(g.size)(k => if k == 3 || k == 7 then 1.0 else 0.0)
    assertEquals(Lorocv.select(g, Vector(twoWay)).map(_.selected), Right(3), "tie between 3 and 7 keeps the smaller alpha")
    val top = Vector.tabulate(g.size)(_.toDouble)
    assert(Lorocv.select(g, Vector(top)).exists(_.atUpperEnd))

  test("mean fold scores are the per-grid-point mean over folds; refusals are typed"):
    val g = AlphaGrid.Pilot
    val a = Vector.tabulate(g.size)(_.toDouble)
    val b = Vector.tabulate(g.size)(k => 2.0 * k)
    assertEquals(Lorocv.select(g, Vector(a, b)).map(_.meanScores), Right(Vector.tabulate(g.size)(k => 1.5 * k)))
    assertEquals(Lorocv.select(g, Vector.empty), Left(LorocvRefusal.Inconsistent("no folds")))
    assert(Lorocv.select(g, Vector(a.take(3))).isLeft)
    assertEquals(Lorocv.select(g, Vector(a.updated(4, Double.NaN))), Left(LorocvRefusal.NonFiniteScore(0, 4)))

  test("run applies the callback to every fold in order and stops at the first refusal"):
    val folds = Lorocv.folds(runId).fold(e => fail(e.message), identity)
    val seen = scala.collection.mutable.ArrayBuffer.empty[Int]
    val ok = Lorocv.run(folds, AlphaGrid.Pilot) { f =>
      seen += f.heldOut
      Right(Vector.tabulate(9)(k => -math.abs(k - 2.0) - 0.01 * f.heldOut))
    }
    assertEquals(seen.toVector, Vector(0, 1, 2, 3))
    assertEquals(ok.map(_.selected), Right(2))
    seen.clear()
    val stopped = Lorocv.run(folds, AlphaGrid.Pilot) { f =>
      seen += f.heldOut
      if f.heldOut == 1 then Left(LorocvRefusal.Evaluation(1, "boom", "x")) else Right(Vector.fill(9)(0.0))
    }
    assertEquals(seen.toVector, Vector(0, 1))
    assertEquals(stopped.left.map(_.code), Left("boom"))

  test("fold score: a perfect prediction scores 1, no prediction scores 0, and the held-out nuisance is removed first"):
    val rows = 30
    val nuisance = mat(rows, 2)((r, c) => if c == 0 then 1.0 else (r - 15.0) / 30.0)
    val signal = mat(rows, 3)((r, c) => math.sin(0.4 * r + c) )
    val y = mat(rows, 3)((r, c) => signal(r, c) + 5.0 + 2.0 * nuisance(r, 1) * (c + 1))
    val perfect = FoldScore.predictedR2(y, signal, nuisance).fold(e => fail(e.message), identity)
    assertEqualsDouble(perfect.r2, 1.0, 1e-12)
    // predicting the signal plus anything in the nuisance span is just as good: the baseline is not scored
    val shifted = mat(rows, 3)((r, c) => signal(r, c) + 100.0 - 7.0 * nuisance(r, 1))
    assertEqualsDouble(FoldScore.predictedR2(y, shifted, nuisance).fold(e => fail(e.message), _.r2), 1.0, 1e-12)
    val none = FoldScore.predictedR2(y, mat(rows, 3)((_, _) => 0.0), nuisance).fold(e => fail(e.message), identity)
    assertEqualsDouble(none.r2, 0.0, 1e-12)
    val half = FoldScore.predictedR2(y, mat(rows, 3)((r, c) => 0.5 * signal(r, c)), nuisance).fold(e => fail(e.message), identity)
    assert(half.r2 > 0.0 && half.r2 < 1.0)
    assertEqualsDouble(half.r2, 0.75, 1e-10, "predicting half the signal leaves a quarter of its variance: R2 = 1 - 1/4")
    // a negative score is possible (worse than the baseline)
    assert(FoldScore.predictedR2(y, mat(rows, 3)((r, c) => -signal(r, c)), nuisance).exists(_.r2 < 0.0))

  test("fold score is pooled over voxels (sums of RSS and TSS, not a mean of per-voxel R2)"):
    val rows = 20
    val nuisance = mat(rows, 1)((_, _) => 1.0)
    val y = mat(rows, 2)((r, c) => (if c == 0 then 10.0 else 1.0) * math.sin(0.7 * r))
    val pred = mat(rows, 2)((r, c) => if c == 0 then math.sin(0.7 * r) * 10.0 else 0.0)
    val s = FoldScore.predictedR2(y, pred, nuisance).fold(e => fail(e.message), identity)
    // voxel 0 is predicted perfectly and carries ~99% of the variance, voxel 1 not at all: pooled R2 is near 1, not 0.5
    assert(s.r2 > 0.95)

  test("fold score refuses on mismatched shapes and on no held-out variance"):
    val y = mat(10, 2)((r, c) => r + c)
    assert(FoldScore.predictedR2(y, mat(9, 2)((_, _) => 0.0), mat(10, 1)((_, _) => 1.0)).isLeft)
    assert(FoldScore.predictedR2(y, y, mat(8, 1)((_, _) => 1.0)).isLeft)
    val constant = mat(10, 2)((_, _) => 3.0)
    assert(FoldScore.predictedR2(constant, y, mat(10, 1)((_, _) => 1.0)).isLeft, "constant held-out data has no variance beyond the nuisance")
    assertEquals(FoldScore.predictedR2(y, y, mat(10, 0)((_, _) => 0.0)).map(_.r2), Right(1.0))

  test("amplitude prediction: same-stimulus training mean, condition mean for an unseen stimulus"):
    val trainStim = Array(0, 1, 0, 1, 2)
    val trainCond = Array(0, 0, 0, 0, 1)
    val amp = mat(5, 2)((r, c) => Vector(10.0, 20.0, 14.0, 22.0, 7.0)(r) + c)
    val got = AmplitudePrediction.byStimulus(trainStim, trainCond, amp, Array(0, 1, 2, 9), Array(0, 0, 1, 0)).fold(e => fail(e.message), identity)
    assertEquals((got.rows, got.cols), (4, 2))
    assertEquals(Vector(got(0, 0), got(0, 1)), Vector(12.0, 13.0), "stimulus 0: mean of 10 and 14")
    assertEquals(Vector(got(1, 0), got(1, 1)), Vector(21.0, 22.0), "stimulus 1: mean of 20 and 22")
    assertEquals(Vector(got(2, 0), got(2, 1)), Vector(7.0, 8.0), "stimulus 2: single training trial")
    assertEquals(Vector(got(3, 0), got(3, 1)), Vector(16.5, 17.5), "unseen stimulus 9 in condition 0: condition mean of 10, 20, 14, 22")

  test("amplitude prediction refuses a condition with no training trial and inconsistent stimulus/condition pairs"):
    val amp = mat(2, 1)((r, _) => r.toDouble)
    assertEquals(
      AmplitudePrediction.byStimulus(Array(0, 1), Array(0, 0), amp, Array(5), Array(3)),
      Left(LorocvRefusal.NoTrainingCondition(3))
    )
    assert(AmplitudePrediction.byStimulus(Array(0, 1), Array(0, 0), amp, Array(0), Array(1)).isLeft)
    assert(AmplitudePrediction.byStimulus(Array(0), Array(0, 0), amp, Array(0), Array(0)).isLeft)
