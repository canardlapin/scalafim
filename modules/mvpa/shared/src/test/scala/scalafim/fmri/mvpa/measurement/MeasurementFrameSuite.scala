package scalafim.fmri.mvpa.measurement

import gale.linalg.DMat
import multivar.core.{SemanticSpace, SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{IndexSpace, Injection}
import scalafim.fmri.mvpa.{AxisRef, EvidenceSource, Observations}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class MeasurementFrameSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def axis(name: String, keys: Vector[String]): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, keys, "voxel-order", "psc", "raw", Vector("fixture:v1")))

  private def observed(samples: AxisRef[String], neural: AxisRef[String]): Observations[samples.Id, neural.Id] =
    val id = SourceId.unsafe("frame-source")
    val source = right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe("frame-root"), id)))
    right(Observations.fromDense(samples, neural, DMat.dense(2, 3, Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)), ValueIdentity.source(ValueId.unsafe("frame-values")), source))

  test("lazy frames open entries only during traversal and always release the owned observation"):
    val samples = axis("samples", Vector("s1", "s2"))
    val neural = axis("neural", Vector("a", "b", "c"))
    val first = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("a")))
    val population = right(IndexSpace.of(neural.size))
    val second = right(MeasurementLeg.hardSelection(neural, MeasurementId.unsafe("b"), right(Injection.from(IArray(2), population))))
    var openedEntries = 0
    val frame = MeasurementFrame.lazyFrame(
      neural,
      MeasurementFrameDeclaration("fixture-frame", "v1", Vector("radius" -> "3"))
    ):
      openedEntries += 1
      Iterator(PackedMeasurementEntry(first, "whole"), PackedMeasurementEntry(second, "tail"))
    assertEquals(openedEntries, 0)

    var closed = 0
    val visitor = new MeasurementVisitor[samples.Id, neural.Id, String, String, Int]:
      def visit[L <: SemanticSpace](entry: PackedMeasurementEntry[neural.Id, String, String] { type Local = L }, measured: MeasuredObservations[samples.Id, L]): Either[MeasurementFailure, Int] =
        Right(measured.patterns.cols)

    val result = frame.fold(1)(Right(MeasurementResource(observed(samples, neural))(closed += 1)))(visitor)(0)((total, outcome) => total + outcome.value.toOption.getOrElse(0))
    assertEquals(result.value, 4)
    assertEquals(result.error, None)
    assertEquals(openedEntries, 1)
    assertEquals(closed, 1)

  test("frame construction canonicalizes eager ordering and refuses duplicate ids"):
    val neural = axis("neural", Vector("a", "b", "c"))
    val one = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("z")))
    val two = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("a")))
    val frame = right(MeasurementFrame(neural, Vector(PackedMeasurementEntry(one, 1), PackedMeasurementEntry(two, 2))))
    assertEquals(frame.identity.source, neural.descriptor)
    assert(MeasurementFrame(neural, Vector(PackedMeasurementEntry(one, 1), PackedMeasurementEntry(one, 2))).isLeft)

  private def widthVisitor[S <: SemanticSpace, N <: SemanticSpace]: MeasurementVisitor[S, N, String, String, Int] =
    new MeasurementVisitor[S, N, String, String, Int]:
      def visit[L <: SemanticSpace](entry: PackedMeasurementEntry[N, String, String] { type Local = L }, measured: MeasuredObservations[S, L]): Either[MeasurementFailure, Int] =
        Right(measured.patterns.cols)

  test("a local task failure preserves later successful outputs and closes exactly once"):
    val samples = axis("samples", Vector("s1", "s2"))
    val neural = axis("neural", Vector("a", "b", "c"))
    val first = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("a")))
    val second = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("b")))
    val frame = right(MeasurementFrame(neural, Vector(PackedMeasurementEntry(first, "fail"), PackedMeasurementEntry(second, "ok"))))
    var closes = 0
    val visitor = new MeasurementVisitor[samples.Id, neural.Id, String, String, Int]:
      def visit[L <: SemanticSpace](entry: PackedMeasurementEntry[neural.Id, String, String] { type Local = L }, measured: MeasuredObservations[samples.Id, L]): Either[MeasurementFailure, Int] =
        if entry.rendition == "fail" then throw new IllegalStateException("local failure")
        else Right(measured.patterns.cols)
    val result = frame.traverse(1)(Right(MeasurementResource(observed(samples, neural))(closes += 1)))(visitor)
    assertEquals(result.error, None)
    assertEquals(result.value.map(_.value), Vector(Left(MeasurementFailure.Task("local failure")), Right(3)))
    assertEquals(closes, 1)

  test("iterator failure preserves completed outputs and releases the source"):
    val samples = axis("samples", Vector("s1", "s2"))
    val neural = axis("neural", Vector("a", "b", "c"))
    val leg = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("a")))
    var closes = 0
    val frame = MeasurementFrame.lazyFrame(neural, MeasurementFrameDeclaration("broken-iterator", "v1", Vector.empty)):
      new Iterator[PackedMeasurementEntry[neural.Id, String, String]]:
        var read = false
        def hasNext: Boolean =
          if read then throw new IllegalStateException("iterator failed") else true
        def next(): PackedMeasurementEntry[neural.Id, String, String] =
          read = true
          PackedMeasurementEntry(leg, "first")
    val result = frame.traverse(1)(Right(MeasurementResource(observed(samples, neural))(closes += 1)))(widthVisitor[samples.Id, neural.Id])
    assertEquals(result.value.map(_.value), Vector(Right(3)))
    assertEquals(result.error, Some(FrameTraversalError.Iterator("iterator failed")))
    assertEquals(closes, 1)

  test("invalid budgets and failed opens perform no entry or source work"):
    val samples = axis("samples", Vector("s1", "s2"))
    val neural = axis("neural", Vector("a", "b", "c"))
    var entries = 0
    var opens = 0
    val frame = MeasurementFrame.lazyFrame(neural, MeasurementFrameDeclaration("open-failures", "v1", Vector.empty)):
      entries += 1
      Iterator.empty[PackedMeasurementEntry[neural.Id, String, String]]
    val visitor = widthVisitor[samples.Id, neural.Id]
    def openObserved: Either[MeasurementError, MeasurementResource[Observations[samples.Id, neural.Id]]] =
      opens += 1
      Right(MeasurementResource(observed(samples, neural))(()))
    val budget = frame.traverse(0)(openObserved)(visitor)
    assertEquals(budget.error, Some(FrameTraversalError.InvalidBudget(0)))
    assertEquals(opens, 0)
    val failed = frame.traverse(1)(Left(MeasurementError.EmptySupport))(visitor)
    assertEquals(failed.error, Some(FrameTraversalError.Open(MeasurementError.EmptySupport)))
    val thrown = frame.traverse(1)(throw new IllegalStateException("open failed"))(visitor)
    assertEquals(thrown.error, Some(FrameTraversalError.OpenException("open failed")))
    assertEquals(entries, 0)

  test("close failure is reported after retaining successful outputs"):
    val samples = axis("samples", Vector("s1", "s2"))
    val neural = axis("neural", Vector("a", "b", "c"))
    val leg = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("a")))
    val frame = right(MeasurementFrame(neural, Vector(PackedMeasurementEntry(leg, "entry"))))
    val result = frame.traverse(1)(Right(MeasurementResource(observed(samples, neural))(throw new IllegalStateException("close failed"))))(widthVisitor[samples.Id, neural.Id])
    assertEquals(result.value.map(_.value), Vector(Right(3)))
    assertEquals(result.error, Some(FrameTraversalError.Close("close failed")))

  test("streaming fold does not retain generated entries and owns at most one resource"):
    val samples = axis("samples", Vector("s1", "s2"))
    val neural = axis("neural", Vector("a", "b", "c"))
    var made = 0
    var consumed = 0
    var active = 0
    val frame = MeasurementFrame.lazyFrame(neural, MeasurementFrameDeclaration("generated", "v1", Vector("count" -> "1000"))):
      Iterator.range(0, 1000).map: index =>
        assertEquals(made, consumed)
        made += 1
        val leg = right(MeasurementLeg.identity(neural, MeasurementId.unsafe(f"$index%04d")))
        PackedMeasurementEntry(leg, "entry")
    def openObserved: Either[MeasurementError, MeasurementResource[Observations[samples.Id, neural.Id]]] =
      active += 1
      assertEquals(active, 1)
      Right(MeasurementResource(observed(samples, neural))(active -= 1))
    val result = frame.fold(1)(openObserved)(widthVisitor[samples.Id, neural.Id])(0): (total, outcome) =>
      assertEquals(outcome.value, Right(3))
      consumed += 1
      total + 1
    assertEquals(result.value, 1000)
    assertEquals(result.error, None)
    assertEquals(active, 0)
    assertEquals(made, consumed)

  test("eager identity binds actual maps while rendition stays outside scientific identity"):
    val neural = axis("neural", Vector("a", "b", "c"))
    val population = right(IndexSpace.of(3))
    val first = right(MeasurementLeg.hardSelection(neural, MeasurementId.unsafe("selection"), right(Injection.from(IArray(0), population))))
    val second = right(MeasurementLeg.hardSelection(neural, MeasurementId.unsafe("selection"), right(Injection.from(IArray(1), population))))
    val one = right(MeasurementFrame(neural, Vector(PackedMeasurementEntry(first, "red"))))
    val rerendered = right(MeasurementFrame(neural, Vector(PackedMeasurementEntry(first, "blue"))))
    val changed = right(MeasurementFrame(neural, Vector(PackedMeasurementEntry(second, "red"))))
    assertEquals(one.identity, rerendered.identity)
    assertNotEquals(one.identity, changed.identity)

  test("invalid lazy ordering cannot lower the high-water mark and execute a duplicate"):
    val samples = axis("samples", Vector("s1", "s2"))
    val neural = axis("neural", Vector("a", "b", "c"))
    val a = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("a")))
    val b = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("b")))
    val c = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("c")))
    val frame = MeasurementFrame.lazyFrame(neural, MeasurementFrameDeclaration("unordered", "v1", Vector.empty)):
      Iterator(PackedMeasurementEntry(b, "b"), PackedMeasurementEntry(a, "a"), PackedMeasurementEntry(b, "b"), PackedMeasurementEntry(c, "c"))
    var visits = 0
    val visitor = new MeasurementVisitor[samples.Id, neural.Id, String, String, Int]:
      def visit[L <: SemanticSpace](entry: PackedMeasurementEntry[neural.Id, String, String] { type Local = L }, measured: MeasuredObservations[samples.Id, L]): Either[MeasurementFailure, Int] =
        visits += 1
        Right(measured.patterns.cols)
    val result = frame.traverse(1)(Right(MeasurementResource(observed(samples, neural))(())))(visitor)
    assertEquals(visits, 2)
    assertEquals(result.value.map(_.value), Vector(Right(3), Left(MeasurementFailure.Ordering("b", "a")), Left(MeasurementFailure.Ordering("b", "b")), Right(3)))
