package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix}

/** `E[gamma_raw] = A gamma_true`, checked against an independent analytic expectation.
  *
  * The expectation of the raw lag-`h` estimate is built from the residual operator and a known AR(1) covariance,
  * without going through the lag decomposition `Sigma = sum_k gamma_k S_k` that the production code uses:
  *
  * {{{
  * E[gamma_raw_h] = (1 / pairs_h) * trace(R' T_h R Sigma),    R = C (I - P)[keep, :]
  * }}}
  *
  * with `P` the hat matrix (from a normal-equations inverse local to this test), `C` the per-run centring over
  * the surviving rows and `T_h` the indicator of the lag-`h` pairs. Two runs of six rows make every within-run lag
  * (0..5) part of the budget, so the AR(1) covariance has no tail beyond what `A` covers and the identity is
  * exact rather than approximate. No Monte Carlo.
  */
class AcvfBiasTheorySuite extends munit.FunSuite:

  private val Rho = 0.6
  private val Sigma2 = 1.7
  private val Lag = 5
  private val Ar1 = Vector.tabulate(Lag + 1)(k => Sigma2 * math.pow(Rho, k))
  // Autocovariance that is exactly zero beyond lag 2, so the budget of 5 lags leaves no tail.
  private val Banded = Vector(1.0, 0.5, 0.2, 0.0, 0.0, 0.0)

  private def design(rows: Int): Vector[Vector[Double]] =
    Vector.tabulate(rows)(i => Vector(1.0, i.toDouble / rows, math.sin(1.3 * i + 0.4), if i % 3 == 0 then 1.0 else 0.0))

  private def solveSpd(a: Array[Array[Double]], b: Array[Array[Double]]): Array[Array[Double]] =
    // Gauss-Jordan on [a | b]; test-local and only used on a 4 x 4 normal-equations system.
    val n = a.length
    val m = Array.tabulate(n)(i => a(i) ++ b(i))
    var col = 0
    while col < n do
      val pivot = (col until n).maxBy(r => math.abs(m(r)(col)))
      val tmp = m(col); m(col) = m(pivot); m(pivot) = tmp
      val scale = m(col)(col)
      m(col) = m(col).map(_ / scale)
      (0 until n).filter(_ != col).foreach { r =>
        val factor = m(r)(col)
        m(r) = Array.tabulate(m(r).length)(c => m(r)(c) - factor * m(col)(c))
      }
      col += 1
    m.map(_.drop(n))

  private def hat(x: Vector[Vector[Double]]): Array[Array[Double]] =
    val n = x.length
    val p = x.head.length
    val xtx = Array.tabulate(p, p)((i, j) => (0 until n).map(r => x(r)(i) * x(r)(j)).sum)
    val xt = Array.tabulate(p, n)((i, r) => x(r)(i))
    val inv = solveSpd(xtx, xt) // (X'X)^-1 X'
    Array.tabulate(n, n)((r, c) => (0 until p).map(k => x(r)(k) * inv(k)(c)).sum)

  private def check(runLengths: Vector[Int], truth: Vector[Double], censor: Set[Int], recover: Boolean): Unit =
    val rows = runLengths.sum
    val x = design(rows)
    val layout =
      NoiseEstimationLayout.excludingRows(TimeSegments.fromRunLengths(runLengths), rows, censor).fold(e => fail(e.message), identity)
    val a = AcvfBias.matrices(Matrix.tabulate(rows, 4)((r, c) => x(r)(c)), layout, Lag).fold(e => fail(e.message), identity)
    assertEquals(a.lag, Lag)

    val runOf = Vector.tabulate(rows)(i => if i < runLengths.head then 0 else 1)
    val projector = hat(x)
    // Sigma: AR(1) within a run, zero across runs.
    val sigma = Array.tabulate(rows, rows)((i, j) =>
      if runOf(i) == runOf(j) && math.abs(i - j) <= Lag then truth(math.abs(i - j)) else 0.0
    )

    runLengths.indices.foreach { run =>
      val keep = (0 until rows).filter(i => runOf(i) == run && !censor.contains(i)).toVector
      val nv = keep.length
      // R = C (I - P)[keep, :]
      val m = Array.tabulate(nv, rows)((a, j) => (if keep(a) == j then 1.0 else 0.0) - projector(keep(a))(j))
      val colMean = Array.tabulate(rows)(j => (0 until nv).map(a => m(a)(j)).sum / nv)
      val r = Array.tabulate(nv, rows)((a, j) => m(a)(j) - colMean(j))
      // Cov(e_a, e_b) = (R Sigma R')[a, b]
      val rs = Array.tabulate(nv, rows)((a, j) => (0 until rows).map(k => r(a)(k) * sigma(k)(j)).sum)
      val cov = Array.tabulate(nv, nv)((a, b) => (0 until rows).map(j => rs(a)(j) * r(b)(j)).sum)
      // Segments are the contiguous surviving blocks of the run.
      val segment = keep.indices.map(i => keep.take(i + 1).sliding(2).count(w => w.length == 2 && w(1) - w(0) > 1)).toVector

      val expected = (0 to Lag).map { h =>
        val pairs = (h until nv).filter(i => segment(i) == segment(i - h)).map(i => (i, i - h))
        if h == 0 then Some((0 until nv).map(i => cov(i)(i)).sum / nv)
        else if pairs.isEmpty then None
        else Some(pairs.map { case (i, j) => cov(i)(j) }.sum / pairs.length)
      }

      val matrix = a.byRun(run)
      (0 to Lag).foreach { h =>
        expected(h).foreach { e =>
          val viaA = (0 to Lag).map(k => matrix(h, k) * truth(k)).sum
          assert(math.abs(viaA - e) <= 1e-12, clues(run, h, viaA, e))
        }
      }

      // Solving A gamma = E[gamma_raw] recovers the truth wherever every lag has pairs.
      if recover && expected.forall(_.isDefined) then
        val (recovered, applied) = AcvfBias.correct(expected.map(_.get).toVector, matrix)
        assert(applied, clues(AcvfBias.reciprocalCondition(matrix), recovered, expected))
        recovered.zip(truth).zipWithIndex.foreach { case ((got, want), k) =>
          assert(math.abs(got - want) <= 1e-9, clues(run, k, got, want))
        }
    }

  test("E[gamma_raw] = A gamma_true holds exactly for AR(1) noise, two runs") {
    // Two runs of six rows: every within-run lag (0..5) is inside the budget, so AR(1) has no tail.
    check(Vector(6, 6), Ar1, Set.empty, recover = false)
  }

  test("E[gamma_raw] = A gamma_true holds exactly for AR(1) noise with censoring that splits a run") {
    check(Vector(6, 6), Ar1, Set(9), recover = false)
  }

  test("E[gamma_raw] = A gamma_true and the solve recovers gamma exactly for noise that dies within the budget") {
    check(Vector(14, 12), Banded, Set.empty, recover = true)
    check(Vector(14, 12), Banded, Set(5, 19), recover = true)
  }

  test("the raw estimate is biased: A is not the identity, and its lag-zero row sums below one") {
    val runLengths = Vector(14, 12)
    val rows = runLengths.sum
    val x = design(rows)
    val layout = NoiseEstimationLayout.allRows(TimeSegments.fromRunLengths(runLengths), rows).fold(e => fail(e.message), identity)
    val a = AcvfBias.matrices(Matrix.tabulate(rows, 4)((r, c) => x(r)(c)), layout, Lag).fold(e => fail(e.message), identity)
    val m: DMat = a.byRun.head
    assert(m(0, 0) < 1.0, clues(m(0, 0)))
    assert((1 to Lag).exists(k => math.abs(m(0, k)) > 1e-3))
  }
