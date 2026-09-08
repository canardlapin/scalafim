package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.Matrix
import multivar.core.ValueId
import multivar.core.ValueIdentity
import scalafim.fmri.fit.LeastSquaresSeparate
import scalafim.fmri.fit.LssTrialDesign
import scalafim.fmri.fit.ResponseBlock

class RunwiseObservationsSuite extends munit.FunSuite:
  test("runwise trial readouts produce one matrix-free observation source with exact identities"):
    val fixture = observationFixture()
    val result = right(
      RunwiseObservations(
        fixture.partitions,
        fixture.samples,
        fixture.neural,
        fixture.runs
      )
    )

    assertEquals(
      result.observations.samples.identity,
      fixture.samples.identity
    )
    assertEquals(
      result.observations.neuralAxis.identity,
      fixture.neural.identity
    )
    assertEquals(
      result.observations.evidence.representation,
      multivar.core.OperatorRepresentation.MatrixFree
    )
    assertEquals(result.estimability.toVector, Vector.fill(4)(true))
    assertEquals(
      fixture.samples.keys.map(result.partitionOf),
      Vector(
        Some(PartitionId.unsafe("run-1")),
        Some(PartitionId.unsafe("run-1")),
        Some(PartitionId.unsafe("run-2")),
        Some(PartitionId.unsafe("run-2"))
      )
    )
    assertEquals(
      result.receipt.runs.map(_.responseRevision.stableKey),
      Vector("observation-response-1", "observation-response-2")
    )

    val budget = MaterializationBudget.unsafe(12)
    val actual = right(
      result.observations.evidence.materialize(
        MaterializationPolicy.Allow(budget)
      )
    ).value
    val expected = stack(
      fixture.runs.map(run => right(run.readout.forward(run.response)).value)
    )
    assertMatrix(actual, expected)

  test("runwise observation construction rejects duplicate semantic samples before stacking"):
    val fixture = observationFixture()
    val duplicate = right(
      RunwiseObservationInput(
        PartitionId.unsafe("run-2"),
        fixture.runs(1).response,
        fixture.runs(1).readout,
        fixture.runs(0).samples,
        fixture.neural,
        fixture.runs(1).responseRevision,
        ValueId.unsafe("duplicate-sample-observations")
      )
    )

    RunwiseObservations(
      fixture.partitions,
      fixture.samples,
      fixture.neural,
      Vector(fixture.runs(0), duplicate)
    ) match
      case Left(FmriEvidenceError.DuplicateSample(sample)) =>
        assertEquals(sample, SampleId.unsafe("run-1-trial-a"))
      case other => fail(s"expected duplicate-sample rejection, obtained $other")

  private final case class Fixture[
      P <: multivar.core.SemanticSpace,
      S <: multivar.core.SemanticSpace,
      N <: multivar.core.SemanticSpace
  ](
      partitions: PartitionAxis[P],
      samples: AxisRef.Aux[SampleId, S],
      neural: AxisRef.Aux[FeatureId, N],
      runs: Vector[RunwiseObservationInput[N, FeatureId]]
  )

  private def observationFixture(): Fixture[?, ?, ?] =
    val partitionRef = right(
      AxisRef.create(
        AxisId.unsafe("observation-runs"),
        AxisPurpose.Partitions,
        Vector(PartitionId.unsafe("run-1"), PartitionId.unsafe("run-2")),
        CoordinateBasis.unsafe("run-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("runwise-observations-suite", "v1")
      )
    )
    val partitions = right(
      PartitionAxis(ScientificAxisName.unsafe("runs"), partitionRef)
    )
    val neural = right(
      AxisRef.create(
        AxisId.unsafe("observation-neural"),
        AxisPurpose.NeuralFeatures,
        Vector(FeatureId.unsafe("v1"), FeatureId.unsafe("v2"), FeatureId.unsafe("v3")),
        CoordinateBasis.unsafe("voxel-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("runwise-observations-suite", "v1")
      )
    )
    val runSamples = Vector.tabulate(2): run =>
      right(
        AxisRef.create(
          AxisId.unsafe(s"observation-run-${run + 1}-trials"),
          AxisPurpose.Samples,
          Vector("a", "b").map(label => SampleId.unsafe(s"run-${run + 1}-trial-$label")),
          CoordinateBasis.unsafe("trial-order"),
          None,
          AxisScale.nominal,
          CoordinateProvenance.unsafe("runwise-observations-suite", "v1")
        )
      )
    val samples = right(
      AxisRef.create(
        AxisId.unsafe("observation-trials"),
        AxisPurpose.Samples,
        runSamples.flatMap(_.keys),
        CoordinateBasis.unsafe("run-major-trial-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("runwise-observations-suite", "v1")
      )
    )
    val design = fromRows(
      Seq(
        Seq(1.0, 0.0),
        Seq(1.0, 0.0),
        Seq(0.0, 1.0),
        Seq(0.0, 1.0)
      )
    )
    val readout = right(
      LeastSquaresSeparate
        .unsafePrepare(LssTrialDesign.unsafe(design, Vector("trial-a", "trial-b")))
        .trialReadout
    )
    val responses = Vector(
      fromRows(Seq(Seq(1.0, 2.0, 3.0), Seq(3.0, 4.0, 5.0), Seq(7.0, 8.0, 9.0), Seq(9.0, 10.0, 11.0))),
      fromRows(Seq(Seq(2.0, 1.0, 0.0), Seq(4.0, 3.0, 2.0), Seq(8.0, 7.0, 6.0), Seq(10.0, 9.0, 8.0)))
    )
    val runs = Vector.tabulate(2): run =>
      right(
        RunwiseObservationInput(
          partitionRef.keys(run),
          ResponseBlock.unsafe(responses(run)),
          readout,
          runSamples(run),
          neural,
          ValueIdentity.source(ValueId.unsafe(s"observation-response-${run + 1}")),
          ValueId.unsafe(s"observation-estimate-${run + 1}")
        )
      )
    Fixture(partitions, samples, neural, runs)

  private def stack(values: Vector[DMat]): DMat =
    Matrix.tabulate(values.map(_.rows).sum, values.head.cols): (row, column) =>
      var offset = 0
      var part = 0
      while row >= offset + values(part).rows do
        offset += values(part).rows
        part += 1
      values(part)(row - offset, column)

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, column) => rows(row)(column))

  private def assertMatrix(actual: DMat, expected: DMat): Unit =
    assertEquals((actual.rows, actual.cols), (expected.rows, expected.cols))
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertEqualsDouble(actual(row, column), expected(row, column), 1e-12)
        column += 1
      row += 1

  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")
