package scalafim.group.research.bootstrap

class StreamsSuite extends munit.FunSuite:
  test("SplitMix64 reproduces the reference sequence for seed 0"):
    val g = SplitMix64.fromSeed(0L)
    // Reference outputs of Vigna's splitmix64.c for x = 0.
    assertEquals(g.nextLong(), 0xe220a8397b1dcdafL)
    assertEquals(g.nextLong(), 0x6e789e6aa1b965f4L)
    assertEquals(g.nextLong(), 0x06c45d188009454fL)

  test("stream keys are deterministic and every key component changes the lane seed"):
    val id = CellId.unsafe("C-n8-DI-Vflat-T0-Ninf")
    val other = CellId.unsafe("C-n8-DI-Vflat-T0-N8")
    val base = StreamKey(2026100101L, id, 3, StreamKind.Outcome)
    assertEquals(base.seed(Lane.SubjectU), StreamKey(2026100101L, id, 3, StreamKind.Outcome).seed(Lane.SubjectU))
    val variants = Vector(
      base.copy(root = 2026100201L).seed(Lane.SubjectU),
      base.copy(cell = other).seed(Lane.SubjectU),
      base.copy(study = 4).seed(Lane.SubjectU),
      base.copy(stream = StreamKind.FirstLevel).seed(Lane.SubjectU),
      base.seed(Lane.SubjectE)
    )
    assert(variants.forall(_ != base.seed(Lane.SubjectU)), "each component must reseed")
    assertEquals(variants.distinct.length, variants.length)

  test("declared roots: pilot 2026100101-05, confirmation 2026100201-05; power studies use the power root"):
    assertEquals(StreamKind.values.toVector.map(Phase.Pilot.root), Vector(2026100101L, 2026100102L, 2026100103L, 2026100104L, 2026100105L))
    assertEquals(StreamKind.values.toVector.map(Phase.Confirmation.root), Vector(2026100201L, 2026100202L, 2026100203L, 2026100204L, 2026100205L))
    val id = CellId.unsafe("C-n8-DI-Vflat-T0-Ninf")
    assertEquals(StreamKey.of(Phase.Pilot, StudyPurpose.Power, id, 0, StreamKind.Outcome).root, 2026100105L)
    assertEquals(StreamKey.of(Phase.Pilot, StudyPurpose.Null, id, 0, StreamKind.Outcome).root, 2026100101L)

  test("normal, chi-square and gamma draws have the right first two moments"):
    val g = SplitMix64.fromSeed(2026093011L)
    val m = 20000
    val z = Array.fill(m)(g.nextGaussian())
    val zMean = z.sum / m
    val zVar = z.map(x => (x - zMean) * (x - zMean)).sum / (m - 1)
    assertEqualsDouble(zMean, 0.0, 4.0 / math.sqrt(m))
    assertEqualsDouble(zVar, 1.0, 4.0 * math.sqrt(2.0 / m))
    Vector(0.5, 3.0, 8.0, 40.0, 11.3).foreach { df =>
      val x = Array.fill(m)(g.nextChiSquare(df))
      val mean = x.sum / m
      val variance = x.map(a => (a - mean) * (a - mean)).sum / (m - 1)
      assertEqualsDouble(mean, df, 4.0 * math.sqrt(2.0 * df / m), s"chi2 mean at df $df")
      assertEqualsDouble(variance / (2.0 * df), 1.0, 0.12, s"chi2 variance at df $df")
    }

  test("uniforms stay inside their declared intervals and signs are balanced"):
    val g = SplitMix64.fromSeed(7L)
    val u = Array.fill(10000)(g.nextOpenDouble())
    assert(u.forall(x => x > 0.0 && x < 1.0))
    val plus = (0 until 10000).count(_ => g.nextSign() > 0.0)
    assertEqualsDouble(plus / 10000.0, 0.5, 0.02)
