package scalafim.fmri.hrf.family

import scalafim.fmri.hrf.{NonNegativeSeconds, Seconds}

class Cascade34RealizationSuite extends munit.FunSuite:
  import Cascade34RealizationFixtures.*

  private val wideChart = ShapeChart(
    ("logKappaP", -5.0, 5.0), ("logitRateRatio", -30.0, 30.0), ("rho", 0.0, 2.0)
  )
  private val family = Cascade34Family.make(wideChart).toOption.get
  private val pairs = Vector((0, 0), (0, 1), (0, 2), (1, 1), (1, 2), (2, 2))
  private def duration(t: Double): NonNegativeSeconds = NonNegativeSeconds(t).toOption.get
  private def model(a: Double, b: Double, rho: Double): Cascade34Realization =
    Cascade34Realization.make(family, wideChart.point(a, b, rho).toOption.get).toOption.get
  private def close(actual: Double, expected: Double, clue: String): Unit =
    assert(actual.isFinite, clue)
    assertEqualsDouble(actual, expected, 3e-14 + 3e-11 * math.abs(expected), clue)

  // Test-only dense product-rule oracle; production has no generic algebra.
  private def jetProduct(a: Array[Double], rows: Int, inner: Int, b: Array[Double], cols: Int): Array[Double] =
    val out = new Array[Double](10 * rows * cols)
    def add(component: Int, ca: Int, cb: Int): Unit =
      for r <- 0 until rows; c <- 0 until cols; k <- 0 until inner do
        out(component * rows * cols + r * cols + c) +=
          a(ca * rows * inner + r * inner + k) * b(cb * inner * cols + k * cols + c)
    add(0, 0, 0)
    for p <- 0 until 3 do
      add(1 + p, 1 + p, 0)
      add(1 + p, 0, 1 + p)
    for ((p, q), index) <- pairs.zipWithIndex do
      add(4 + index, 4 + index, 0)
      add(4 + index, 1 + p, 1 + q)
      add(4 + index, 1 + q, 1 + p)
      add(4 + index, 0, 4 + index)
    out

  test("all operators and chart derivatives agree with independent 80-digit dense expm fixtures"):
    for (sample, index) <- samples.zipWithIndex do
      val realization = model(sample.a, sample.b, sample.rho)
      val transition = Array.fill(490)(Double.NaN)
      val injection = Array.fill(70)(Double.NaN)
      val observation = Array.fill(70)(Double.NaN)
      realization.transitionJetInto(duration(sample.delta), transition)
      realization.injectionJetInto(injection)
      realization.observationJetInto(observation)
      for component <- 0 until 10; row <- 0 until 7; col <- 0 until 7 do
        val diagonal = row - col
        val expected =
          if col <= row && row < 3 then sample.transition(component * 7 + diagonal)
          else if col >= 3 && col <= row then sample.transition(component * 7 + 3 + diagonal)
          else 0.0
        close(transition(component * 49 + row * 7 + col), expected, s"sample=$index transition=$component,$row,$col")
      for i <- 0 until 70 do
        close(injection(i), sample.injection(i), s"sample=$index injection=$i")
        close(observation(i), sample.observation(i), s"sample=$index observation=$i")
      val response = jetProduct(observation, 1, 7, jetProduct(transition, 7, 7, injection, 1), 1)
      for c <- 0 until 10 do close(response(c), sample.kernel(c), s"sample=$index kernel component=$c")

  test("value and differentiated semigroup identities hold on unequal intervals"):
    for point <- Vector((math.log(.4), 0.0, .35), (0.0, math.log(4.0), 0.0));
        (s, t) <- Vector((0.0, 0.7), (1e-12, .31), (.37, 1.13), (41.0, 63.0)) do
      val realization = model(point._1, point._2, point._3)
      val left = new Array[Double](490)
      val right = new Array[Double](490)
      val sum = new Array[Double](490)
      realization.transitionJetInto(duration(s), left)
      realization.transitionJetInto(duration(t), right)
      realization.transitionJetInto(duration(s + t), sum)
      val composed = jetProduct(left, 7, 7, right, 7)
      for i <- sum.indices do close(composed(i), sum(i), s"semigroup $s+$t component=$i")

  test("observable jets match the existing family throughout its default chart"):
    val default = Cascade34Family.Default
    for rho <- Vector(0.0, .35, .8) do
      val point = default.chart.point(math.log(.4), 0.0, rho).toOption.get
      val realization = Cascade34Realization.make(default, point).toOption.get
      val times = Array(0.0, .001, 1.3, 12.0, 85.0)
      val expected = new Array[Double](10 * times.length)
      default.jetInto(times, point, expected)
      val b = new Array[Double](70)
      val c = new Array[Double](70)
      realization.injectionJetInto(b)
      realization.observationJetInto(c)
      for i <- times.indices do
        val a = new Array[Double](490)
        realization.transitionJetInto(duration(times(i)), a)
        val result = jetProduct(c, 1, 7, jetProduct(a, 7, 7, b, 1), 1)
        for component <- 0 until 10 do close(result(component), expected(component * times.length + i), s"family $rho,$i,$component")

  test("subnormal derivatives survive value underflow and tiny intervals retain derivatives"):
    val realization = model(0.0, 0.0, .3)
    val out = new Array[Double](490)
    realization.transitionJetInto(duration(750.0), out)
    val expected = samples.find(_.delta == 750.0).get.transition
    assertEqualsDouble(out(0), 0.0, 0.0)
    assert(out(4 * 49) > 0.0, "second derivative must survive underflow of A00")
    for component <- Vector(1, 4); diagonal <- 0 until 3 do
      val value = expected(component * 7 + diagonal)
      assertEqualsDouble(out(component * 49 + diagonal * 7), value,
        32.0 * Double.MinPositiveValue + 1e-10 * math.abs(value))
    realization.transitionJetInto(duration(1500.0), out)
    val tail = samples.find(_.delta == 1500.0).get.transition
    assertEqualsDouble(out(3 * 7 + 3), 0.0, 0.0)
    assert(out(4 * 49 + 3 * 7 + 3) > 0.0)
    for component <- Vector(1, 2, 4, 5, 7); diagonal <- 0 until 4 do
      val value = tail(component * 7 + 3 + diagonal)
      assertEqualsDouble(out(component * 49 + (3 + diagonal) * 7 + 3), value,
        32.0 * Double.MinPositiveValue + 1e-10 * math.abs(value))
    realization.transitionJetInto(duration(1e-18), out)
    assertEqualsDouble(out(49), -1e-18, 1e-32)
    assertEqualsDouble(out(4 * 49), -1e-18, 1e-32)
    val nearEqual = samples.find(_.b > 20.0).get
    val injection = new Array[Double](70)
    model(nearEqual.a, nearEqual.b, nearEqual.rho).injectionJetInto(injection)
    assertEqualsDouble(injection(17), nearEqual.injection(17), math.abs(nearEqual.injection(17)) * 1e-12)

  test("overflowed rate times duration gives the finite zero-transition limit"):
    val realization = model(math.log(100.0), 0.0, .3)
    val values = Array.fill(49)(Double.NaN)
    val jets = Array.fill(490)(Double.NaN)
    realization.transitionInto(duration(Double.MaxValue), values)
    realization.transitionJetInto(duration(Double.MaxValue), jets)
    for value <- values ++ jets do assertEqualsDouble(value, 0.0, 0.0)

  private def transition(realization: Cascade34Realization, t: Double): Array[Double] =
    val out = new Array[Double](49)
    realization.transitionInto(duration(t), out)
    out
  private def multiply(a: Array[Double], b: Array[Double], rows: Int, inner: Int, cols: Int): Array[Double] =
    Array.tabulate(rows * cols): index =>
      (0 until inner).map(k => a((index / cols) * inner + k) * b(k * cols + index % cols)).sum
  private def transpose(a: Array[Double]): Array[Double] = Array.tabulate(49)(i => a((i % 7) * 7 + i / 7))

  test("off-grid interval covariance preserves shared branches, start events and independent coincident trials"):
    val realization = model(math.log(.4), 0.0, .35)
    val b = new Array[Double](7)
    val c = new Array[Double](7)
    realization.injectionInto(b)
    realization.observationInto(c)
    var stateCovariance = new Array[Double](49)
    var previous = 0.0
    val snapshots = scala.collection.mutable.ArrayBuffer.empty[Array[Double]]
    var admitted = 0
    for (time, index) <- observationTimes.zipWithIndex do
      val a = transition(realization, time - previous)
      stateCovariance = multiply(multiply(a, stateCovariance, 7, 7, 7), transpose(a), 7, 7, 7)
      for event <- eventTimes.indices do
        val onset = eventTimes(event)
        // Zero state immediately before the run-start impulses; thereafter (previous,time].
        if (onset > previous || (index == 0 && onset == previous)) && onset <= time then
          admitted += 1
          val v = multiply(transition(realization, time - onset), b, 7, 7, 1)
          val variance = eventWeights(event) * eventWeights(event) / 2.1
          for i <- 0 until 7; j <- 0 until 7 do stateCovariance(i * 7 + j) += variance * v(i) * v(j)
      snapshots += stateCovariance.clone()
      assert(stateCovariance(3) > 0.0, "one impulse must correlate positive and undershoot branches")
      for j <- 0 to index do
        val crossState = multiply(transition(realization, time - observationTimes(j)), snapshots(j), 7, 7, 7)
        val observed = multiply(multiply(c, crossState, 1, 7, 7), c, 1, 7, 1)(0)
        val actual = observed + (if index == j then 1.0 else 0.0)
        close(actual, covariance(index * observationTimes.length + j), s"covariance $index,$j")
      previous = time
    assertEquals(admitted, 6, "start impulse once, future impulse excluded, coincident trials separate")
    // At the run start only the first impulse has occurred: it must survive to later times.
    assertEqualsDouble(snapshots.head(0), .7 * .7 * .4 * .4 / 2.1, 1e-15)

  test("construction rejects unsafe or foreign-chart shapes"):
    for coordinates <- Vector(Vector(0.0), Vector(Double.NaN, 0.0, .3), Vector(0.0, 0.0, -1.0), Vector(99.0, 0.0, .3)) do
      assert(Cascade34Realization.make(family, ShapePoint.unsafe(coordinates)).isLeft)
    val validElsewhere = wideChart.point(0.0, 0.0, 1.0).toOption.get
    assert(Cascade34Realization.make(Cascade34Family.Default, validElsewhere).isLeft)

  test("buffers have exact write prefixes and invalid input leaves them untouched"):
    val realization = model(math.log(.4), 0.0, .35)
    val writers = Vector[(Int, Array[Double] => Unit)](
      (49, out => realization.transitionInto(duration(.37), out)),
      (490, out => realization.transitionJetInto(duration(.37), out)),
      (7, realization.injectionInto), (70, realization.injectionJetInto),
      (7, realization.observationInto), (70, realization.observationJetInto)
    )
    for (size, write) <- writers do
      val out = Array.fill(size + 2)(12345.0)
      write(out)
      assert(out.take(size).forall(x => x.isFinite && x != 12345.0))
      assertEquals(out.drop(size).toVector, Vector(12345.0, 12345.0))
      val short = Array.fill(size - 1)(12345.0)
      intercept[IllegalArgumentException](write(short))
      assert(short.forall(_ == 12345.0))
    for t <- Vector(-1.0, Double.NaN, Double.PositiveInfinity) do
      val out = Array.fill(490)(12345.0)
      val invalid = NonNegativeSeconds.unsafe(Seconds.unsafe(t))
      intercept[IllegalArgumentException](realization.transitionInto(invalid, out))
      intercept[IllegalArgumentException](realization.transitionJetInto(invalid, out))
      assert(out.forall(_ == 12345.0))

  test("value writers agree with jet values and zero duration is the identity"):
    val realization = model(math.log(.4), 0.0, .35)
    for t <- Vector(0.0, .37, 85.0, 750.0) do
      val values = new Array[Double](49)
      val jets = new Array[Double](490)
      realization.transitionInto(duration(t), values)
      realization.transitionJetInto(duration(t), jets)
      for i <- values.indices do assertEqualsDouble(values(i), jets(i), 0.0)
      if t == 0.0 then
        for i <- values.indices do assertEqualsDouble(values(i), if i / 7 == i % 7 then 1.0 else 0.0, 0.0)
        assert(jets.drop(49).forall(_ == 0.0))
    for (write, writeJet) <- Vector[(Array[Double] => Unit, Array[Double] => Unit)](
        (realization.injectionInto, realization.injectionJetInto),
        (realization.observationInto, realization.observationJetInto)) do
      val value = new Array[Double](7)
      val jet = new Array[Double](70)
      write(value)
      writeJet(jet)
      for i <- value.indices do assertEqualsDouble(value(i), jet(i), 0.0)
