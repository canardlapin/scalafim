package scalafim.fmri.workflow

import scalafim.dataset.RunId

class SamplingReferenceSuite extends munit.FunSuite:
  private def run(id: String, tr: Double = 2.0, count: Int = 300): RunInput =
    RunInput.unsafe(RunId(id), RepetitionTime.unsafe(tr), count,
      WorkflowArtifactRef.unsafe[BoldImageResource](s"file:///not-opened/$id-bold.nii"),
      WorkflowArtifactRef.unsafe[EventsTableResource](s"file:///not-opened/$id-events.tsv"))

  test("reference fractions accept endpoints and refuse nonfinite or out-of-range values") {
    Vector(0.0, 0.125, 0.5, 1.0).foreach(value =>
      assertEqualsDouble(SamplingFraction(value).toOption.get.value, value, 0.0))
    Vector(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity, -0.001, 1.001)
      .foreach(value => assert(SamplingFraction(value).isLeft))
  }

  test("TR2 boundary grids have explicit first and last samples without changing run duration") {
    val runs = Vector(run("01"))
    Vector((SamplingReference.VolumeOnset, 0.0, 598.0),
      (SamplingReference.VolumeMidpoint, 1.0, 599.0),
      (SamplingReference.VolumeEnd, 2.0, 600.0)).foreach { (reference, first, last) =>
      val resolved = reference.resolve(runs).fold(e => fail(e.message), identity)
      assertEquals(resolved.declaration, reference)
      assertEquals(resolved.frame.samples().length, 300)
      assertEqualsDouble(resolved.frame.samples().head.value, first, 0.0)
      assertEqualsDouble(resolved.frame.samples().last.value, last, 0.0)
      assertEqualsDouble(resolved.runs.head.firstSample.value, first, 0.0)
      assertEqualsDouble(resolved.runs.head.lastSample.value, last, 0.0)
      assertEqualsDouble(resolved.runs.head.repetitionTime.value, 2.0, 0.0)
    }
  }

  test("per-run fractions preserve exact labels and selection order with varying TR") {
    val reference = SamplingReference.PerRun(Map(
      RunId("02") -> SamplingFraction.unsafe(0.25), RunId("01") -> SamplingFraction.VolumeEnd))
    val resolved = reference.resolve(Vector(run("02", 4.0, 3), run("01", 2.0, 2)))
      .fold(e => fail(e.message), identity)
    assertEquals(resolved.runs.map(_.run.value), Vector("02", "01"))
    assertEquals(resolved.frame.samples().map(_.value), Vector(1.0, 5.0, 9.0, 2.0, 4.0))
    assertEquals(resolved.frame.samples(global = true).map(_.value), Vector(1.0, 5.0, 9.0, 14.0, 16.0))
    val reversed = reference.resolve(Vector(run("01", 2.0, 2), run("02", 4.0, 3))).toOption.get
    assertEquals(reversed.frame.samples().map(_.value), Vector(2.0, 4.0, 1.0, 5.0, 9.0))
  }

  test("empty, duplicate, missing and foreign run declarations are rejected") {
    assert(SamplingReference.VolumeOnset.resolve(Vector.empty).isLeft)
    assert(SamplingReference.VolumeOnset.resolve(Vector(run("01"), run("01"))).isLeft)
    val runs = Vector(run("01"), run("02"))
    Vector(Map.empty[RunId, SamplingFraction], Map(RunId("01") -> SamplingFraction.VolumeOnset),
      Map(RunId("1") -> SamplingFraction.VolumeOnset, RunId("02") -> SamplingFraction.VolumeMidpoint),
      Map(RunId("01") -> SamplingFraction.VolumeOnset, RunId("02") -> SamplingFraction.VolumeMidpoint,
        RunId("03") -> SamplingFraction.VolumeEnd)).foreach { fractions =>
      assert(SamplingReference.PerRun(fractions).resolve(runs).isLeft)
    }
  }

  test("invalid precision and unrepresentable sampling domains return errors") {
    Vector(0.0, -1.0, 2.0, Double.NaN, Double.PositiveInfinity).foreach { precision =>
      assert(SamplingReference.VolumeOnset.resolve(Vector(run("01")), precision).isLeft)
    }
    assert(SamplingReference.VolumeEnd.resolve(Vector(run("01", Double.MaxValue, 2))).isLeft)
    assert(SamplingReference.VolumeOnset.resolve(Vector(run("01", 1.0, Int.MaxValue), run("02", 1.0, 1))).isLeft)
  }
