package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

class PairedDualityDiagramSuite extends munit.FunSuite:

  private val x = MatrixView.dense(
    DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(1.0, 1.0)
      )
    )
  )

  private val y = MatrixView.dense(
    DoubleMatrix.fromRows(
      Vector(
        Vector(2.0),
        Vector(3.0),
        Vector(5.0)
      )
    )
  )

  test("paired diagram builds two observed diagrams over one sample space") {
    val sampleSpace = MvSpace.of("samples", SpaceRole.Samples, 3).toOption.get
    val xSpace = MvSpace.of("x", SpaceRole.Observed, 2).toOption.get
    val ySpace = MvSpace.of("y", SpaceRole.Observed, 1).toOption.get
    val rowMetric = MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.0, 0.5, 2.0)), Some(sampleSpace)).toOption.get

    val paired = Unsafe
      .pairedDiagramFromArrays(
        x,
        y,
        "fixture rows are constructed in the same order",
        rowMetric = Some(rowMetric),
        sampleSpace = Some(sampleSpace),
        xSpace = Some(xSpace),
        ySpace = Some(ySpace)
      )
      .toOption
      .get

    assertEquals(paired.rows, 3)
    assertEquals(paired.xCols, 2)
    assertEquals(paired.yCols, 1)
    assertEquals(paired.sampleSpace, sampleSpace)
    assertEquals(paired.x.rowSpace, sampleSpace)
    assertEquals(paired.y.rowSpace, sampleSpace)
    assertEquals(paired.x.columnSpace, xSpace)
    assertEquals(paired.y.columnSpace, ySpace)
    assertEquals(paired.x.rowMetric.space, Some(sampleSpace))
    assertEquals(paired.y.rowMetric.space, Some(sampleSpace))
  }

  test("paired diagram rejects row mismatch at the shared boundary") {
    val shortY = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0))))

    Unsafe.pairedDiagramFromArrays(x, shortY, "exercise positional shape validation") match
      case Left(MultivarError.MatrixShapeMismatch(detail)) =>
        assert(detail.contains("equal rows"), detail)
      case other =>
        fail(s"expected paired row mismatch, got $other")
  }

  test("paired diagram rejects metric space mismatch") {
    val sampleSpace = MvSpace.of("samples", SpaceRole.Samples, 3).toOption.get
    val otherSpace = MvSpace.of("other-samples", SpaceRole.Samples, 3).toOption.get
    val rowMetric = MvMetric.identity(3, Some(otherSpace)).toOption.get

    Unsafe.pairedDiagramFromArrays(
      x,
      y,
      "exercise metric-space validation",
      rowMetric = Some(rowMetric),
      sampleSpace = Some(sampleSpace)
    ) match
      case Left(MultivarError.MatrixShapeMismatch(detail)) =>
        assert(detail.contains("row metric space"), detail)
      case other =>
        fail(s"expected row metric space mismatch, got $other")
  }

  test("paired diagram rejects non-sample shared space and non-observed endpoints") {
    val badSample = MvSpace.of("not-samples", SpaceRole.Observed, 3).toOption.get
    val badEndpoint = MvSpace.of("latent-x", SpaceRole.Latent, 2).toOption.get

    Unsafe.pairedDiagramFromArrays(x, y, "exercise sample-space validation", sampleSpace = Some(badSample)) match
      case Left(MultivarError.MatrixShapeMismatch(detail)) =>
        assert(detail.contains("sample space"), detail)
      case other =>
        fail(s"expected sample role mismatch, got $other")

    Unsafe.pairedDiagramFromArrays(x, y, "exercise endpoint validation", xSpace = Some(badEndpoint)) match
      case Left(MultivarError.MatrixShapeMismatch(detail)) =>
        assert(detail.contains("role observed"), detail)
      case other =>
        fail(s"expected observed role mismatch, got $other")
  }

  test("fromDiagrams accepts separately built, numerically identical row metrics") {
    val xObserved = MvSpace.of("x-observed", SpaceRole.Observed, 2).toOption.get
    val yObserved = MvSpace.of("y-observed", SpaceRole.Observed, 1).toOption.get
    val first = MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.0, 0.5, 2.0))).toOption.get
    val second = MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.0, 0.5, 2.0))).toOption.get
    val xDiagram = DualityDiagram.from(x, rowMetric = Some(first), columnSpace = Some(xObserved)).toOption.get
    val yDiagram = DualityDiagram.from(y, rowMetric = Some(second), columnSpace = Some(yObserved)).toOption.get

    val paired = PairedDualityDiagram.fromDiagrams(xDiagram, yDiagram)
    assert(paired.isRight, s"expected identical-valued row metrics to be accepted, got $paired")
  }

  test("fromDiagrams rejects genuinely different row metrics with a typed metric mismatch") {
    val xObserved = MvSpace.of("x-observed", SpaceRole.Observed, 2).toOption.get
    val yObserved = MvSpace.of("y-observed", SpaceRole.Observed, 1).toOption.get
    val first = MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.0, 0.5, 2.0))).toOption.get
    val second = MvMetric.diagonal(DoubleVector.fromSeq(Vector(2.0, 2.0, 2.0))).toOption.get
    val xDiagram = DualityDiagram.from(x, rowMetric = Some(first), columnSpace = Some(xObserved)).toOption.get
    val yDiagram = DualityDiagram.from(y, rowMetric = Some(second), columnSpace = Some(yObserved)).toOption.get

    PairedDualityDiagram.fromDiagrams(xDiagram, yDiagram) match
      case Left(MultivarError.MetricMismatch(detail)) =>
        assert(detail.contains("share one row metric"), detail)
      case other =>
        fail(s"expected a typed metric mismatch for different row metrics, got $other")
  }

  test("paired diagrams require a common sample space and distinct observed spaces") {
    val sampleSpace = MvSpace.of("samples", SpaceRole.Samples, 3).toOption.get
    val otherSampleSpace = MvSpace.of("other-samples", SpaceRole.Samples, 3).toOption.get
    val observed = MvSpace.of("observed", SpaceRole.Observed, 2).toOption.get
    val xDiagram = DualityDiagram.from(x, rowSpace = Some(sampleSpace), columnSpace = Some(observed)).toOption.get
    val ySameEndpoint = DualityDiagram.from(x, rowSpace = Some(sampleSpace), columnSpace = Some(observed)).toOption.get
    val yOtherRows = DualityDiagram
      .from(y, rowSpace = Some(otherSampleSpace), columnSpace = Some(MvSpace.of("y", SpaceRole.Observed, 1).toOption.get))
      .toOption
      .get

    PairedDualityDiagram.fromDiagrams(xDiagram, ySameEndpoint) match
      case Left(MultivarError.MatrixShapeMismatch(detail)) =>
        assert(detail.contains("distinct observed"), detail)
      case other =>
        fail(s"expected observed-space collision, got $other")

    PairedDualityDiagram.fromDiagrams(xDiagram, yOtherRows) match
      case Left(MultivarError.MatrixShapeMismatch(detail)) =>
        assert(detail.contains("shared sample space"), detail)
      case other =>
        fail(s"expected sample-space mismatch, got $other")
  }
