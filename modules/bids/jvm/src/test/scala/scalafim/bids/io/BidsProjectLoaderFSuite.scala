package scalafim.bids.io

import scalafim.bids.*

import cats.effect.IO
import cats.effect.Ref
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

class BidsProjectLoaderFSuite extends munit.FunSuite:
  private val loader = BidsProjectLoaderF[IO]
  private val runtime = BidsPathRuntime[IO]

  private def withBidsFixture[A](body: Path => A): A =
    val root = Files.createTempDirectory("scalafim-bids-effect-")
    try body(root)
    finally deleteRecursive(root)

  private def write(path: Path, text: String): Unit =
    Files.createDirectories(path.getParent)
    Files.writeString(path, text, StandardCharsets.UTF_8)

  private def touch(path: Path): Unit =
    Files.createDirectories(path.getParent)
    Files.write(path, Array.emptyByteArray)

  private def deleteRecursive(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try stream.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists)
      finally stream.close()

  private def writeCoreFixture(root: Path): Unit =
    write(
      root.resolve("dataset_description.json"),
      """{"Name":"Effect Fixture","BIDSVersion":"1.10.0","DatasetType":"raw"}"""
    )
    write(root.resolve("participants.tsv"), "participant_id\tage\nsub-02\t30\nsub-01\t25\n")
    touch(root.resolve("sub-01/anat/sub-01_T1w.nii.gz"))
    touch(root.resolve("sub-02/func/sub-02_task-rest_bold.nii.gz"))
    write(root.resolve("task-rest_bold.json"), """{"TaskName":"Rest","RepetitionTime":2.0}""")
    write(
      root.resolve("derivatives/fmriprep/dataset_description.json"),
      """{"Name":"fMRIPrep","BIDSVersion":"1.10.0","DatasetType":"derivative"}"""
    )
    touch(root.resolve("derivatives/fmriprep/sub-01/func/sub-01_task-rest_desc-preproc_bold.nii.gz"))

  test("effect loader matches synchronous strict and permissive output"):
    withBidsFixture { root =>
      writeCoreFixture(root)

      val synchronous = BidsProjectLoader.load(root)
      val effectful = loader.load(root).unsafeRunSync()
      assertEquals(effectful, synchronous)
      assertEquals(loader.loadChecked(root).unsafeRunSync(), BidsProjectLoader.loadChecked(root))
      assertEquals(loader.loadStrict(root).unsafeRunSync(), BidsProjectLoader.loadStrict(root))

      Files.delete(root.resolve("participants.tsv"))
      assert(BidsProjectLoader.load(root).isLeft)
      assert(loader.load(root).unsafeRunSync().isLeft)

      val lax = BidsLoadConfig(strictParticipants = false)
      assertEquals(loader.load(root, lax).unsafeRunSync(), BidsProjectLoader.load(root, lax))
    }

  test("effect loader preserves checked diagnostics and strict rejection"):
    withBidsFixture { root =>
      writeCoreFixture(root)
      touch(root.resolve("notes.txt"))
      touch(root.resolve("sub-02/func/sub-02_bold.nii.gz"))

      val checked = loader.loadChecked(root).unsafeRunSync().fold(error => fail(error.message), identity)
      assertEquals(
        checked.issues.map(issue => issue.path.map(_.value) -> issue.code),
        Vector(
          Some("notes.txt") -> BidsIssueCode.UnrecognizedFile,
          Some("sub-02/func/sub-02_bold.nii.gz") -> BidsIssueCode.MissingRequiredField
        )
      )
      loader.loadStrict(root).unsafeRunSync() match
        case Left(BidsProjectLoadError.Validation(report)) => assertEquals(report.issues, checked.issues)
        case other => fail(s"expected strict validation failure, got $other")
    }

  test("checked loaders accumulate filename table sidecar and metadata defects deterministically"):
    withBidsFixture { root =>
      write(
        root.resolve("dataset_description.json"),
        """{"Name":1,"DatasetType":"unsupported"}"""
      )
      write(
        root.resolve("participants.tsv"),
        "participant_id\tparticipant_id\t\nsub-01\nsub-01\tvalue\ttoo\twide\n"
      )
      touch(root.resolve("sub-01/func/sub-01_task-rest_bold.nii.gz"))
      touch(root.resolve("sub-02/func/sub-02_bold.nii.gz"))
      touch(root.resolve("sub-03/func/sub-03_task-_bold.nii.gz"))
      write(
        root.resolve("sub-01/func/sub-01_task-rest_bold.json"),
        """{"TaskName":"","RepetitionTime":-1,"VolumeTiming":[0,0],"SliceTiming":[0,"bad"]}"""
      )
      write(root.resolve("task-a_bold.json"), "not-json")
      write(root.resolve("task-b_bold.json"), "also-not-json")

      val synchronous = BidsProjectLoader.loadChecked(root).fold(error => fail(error.message), identity)
      val effectful = loader.loadChecked(root).unsafeRunSync().fold(error => fail(error.message), identity)

      assertEquals(effectful, synchronous)
      assertEquals(effectful.errors.length, 16, clues(effectful.issues))
      assertEquals(effectful.warnings, Vector.empty)
      assertEquals(
        effectful.issues.map(_.code).distinct,
        Vector(
          BidsIssueCode.InvalidSidecar,
          BidsIssueCode.MissingRequiredField,
          BidsIssueCode.InconsistentMetadata,
          BidsIssueCode.InvalidTable,
          BidsIssueCode.InvalidName
        )
      )
      assertEquals(
        effectful.issues,
        effectful.issues.sortBy(issue => (
          issue.path.map(_.value).getOrElse(""),
          issue.code.ordinal,
          issue.field.getOrElse(""),
          issue.message
        ))
      )
      assert(BidsProjectLoader.load(root).isLeft)
      assert(loader.load(root).unsafeRunSync().isLeft)
      BidsProjectLoader.loadStrict(root) match
        case Left(BidsProjectLoadError.Validation(report)) => assertEquals(report.issues, effectful.issues)
        case other => fail(s"expected synchronous strict validation failure, got $other")
      loader.loadStrict(root).unsafeRunSync() match
        case Left(BidsProjectLoadError.Validation(report)) => assertEquals(report.issues, effectful.issues)
        case other => fail(s"expected effect strict validation failure, got $other")
    }

  test("in-memory store drives the same pure project assembly"):
    val store =
      InMemoryBidsStore
        .fromText[IO](
          Vector(
            "dataset_description.json" -> """{"Name":"Memory Fixture","BIDSVersion":"1.10.0"}""",
            "participants.tsv" -> "participant_id\nsub-01\n",
            "task-rest_bold.json" -> """{"TaskName":"Rest"}""",
            "sub-01/func/sub-01_task-rest_bold.nii.gz" -> ""
          )
        )
        .fold(error => fail(error.message), identity)

    val project =
      loader
        .loadStore(store, BidsPath("memory-fixture"))
        .unsafeRunSync()
        .fold(error => fail(error.message), identity)

    assertEquals(project.participants, Vector("01"))
    assertEquals(project.description.flatMap(_.name), Some("Memory Fixture"))
    assertEquals(project.manifest.paths().map(_.value), Vector(
      "dataset_description.json",
      "participants.tsv",
      "sub-01/func/sub-01_task-rest_bold.nii.gz",
      "task-rest_bold.json"
    ))
    assertEquals(project.sidecars.keySet, Set(BidsPath("task-rest_bold.json")))

  test("path store exposes stable relative entries and UTF-8 reads"):
    withBidsFixture { root =>
      writeCoreFixture(root)
      val store = BidsPathStore[IO](root)
      val entries = store.entries.unsafeRunSync().fold(error => fail(error.message), identity)
      val description =
        entries.find(_.path == BidsPath("dataset_description.json")).getOrElse(fail("missing dataset description entry"))

      assertEquals(entries.map(_.path.value), entries.map(_.path.value).sorted)
      assertEquals(description.size, Some(Files.size(root.resolve("dataset_description.json"))))
      assert(
        store
          .readUtf8(description.path)
          .unsafeRunSync()
          .fold(error => fail(error.message), identity)
          .contains("Effect Fixture")
      )
      assert(store.readUtf8(BidsPath("../escape.json")).unsafeRunSync().isLeft)
    }

  test("directory stream resources close on success, failure, and cancellation"):
    final class Probe extends AutoCloseable:
      val closed = new AtomicBoolean(false)
      def close(): Unit = closed.set(true)

    val success = new Probe
    val failure = new Probe
    val canceled = new Probe

    runtime.useAutoCloseable(IO.pure(success))(_ => IO.unit).unsafeRunSync()
    intercept[RuntimeException](
      runtime.useAutoCloseable(IO.pure(failure))(_ => IO.raiseError(new RuntimeException("boom"))).unsafeRunSync()
    )
    val cancellation =
      for
        fiber <- runtime.useAutoCloseable(IO.pure(canceled))(_ => IO.never).start
        _ <- IO.sleep(20.millis)
        _ <- fiber.cancel
      yield ()
    cancellation.unsafeRunSync()

    assert(success.closed.get())
    assert(failure.closed.get())
    assert(canceled.closed.get())

  test("blocking operations leave the compute pool"):
    val (computeThread, blockingThread) =
      (IO(Thread.currentThread().getName), runtime.blocking(Thread.currentThread().getName)).tupled.unsafeRunSync()

    assertNotEquals(blockingThread, computeThread)
    assert(blockingThread.contains("blocker"), clues(blockingThread, computeThread))

  test("sidecar concurrency is bounded and result ordering is stable"):
    withBidsFixture { root =>
      writeCoreFixture(root)
      (0 until 8).foreach { index =>
        write(root.resolve(f"sub-01/func/task-extra$index%02d_bold.json"), s"""{"Index":$index}""")
      }

      val program =
        for
          active <- Ref.of[IO, Int](0)
          peak <- Ref.of[IO, Int](0)
          observer = new BidsReadObserver[IO]:
            def beforeRead(path: Path): IO[Unit] =
              active.updateAndGet(_ + 1).flatMap(current => peak.update(math.max(_, current))) *> IO.sleep(25.millis)
            def afterRead(path: Path): IO[Unit] =
              active.update(_ - 1)
          observedLoader = BidsProjectLoaderF.observed[IO](observer)
          bounded <- observedLoader.load(root, maxConcurrentReads = PositiveInt.unsafe(2))
          serial <- loader.load(root, maxConcurrentReads = PositiveInt.unsafe(1))
          maximum <- peak.get
        yield (bounded, serial, maximum)

      val (bounded, serial, maximum) = program.unsafeRunSync()
      assertEquals(maximum, 2)
      assertEquals(bounded, serial)
      assertEquals(
        bounded.toOption.toVector.flatMap(_.manifest.files.map(_.path.value)),
        bounded.toOption.toVector.flatMap(_.manifest.files.map(_.path.value)).sorted
      )
    }

  test("parallel sidecar failures choose the first deterministic path"):
    withBidsFixture { root =>
      writeCoreFixture(root)
      val first = root.resolve("sub-01/func/task-a_bold.json")
      val second = root.resolve("sub-01/func/task-b_bold.json")
      write(first, "not-json")
      write(second, "also-not-json")

      val error =
        loader
          .load(root, maxConcurrentReads = PositiveInt.unsafe(2))
          .unsafeRunSync()
          .left
          .toOption
          .getOrElse(fail("expected sidecar decoding failure"))

      val firstRelative = root.relativize(first).toString.replace('\\', '/')
      val secondRelative = root.relativize(second).toString.replace('\\', '/')
      assert(error.message.contains(firstRelative), clues(error.message))
      assert(!error.message.contains(secondRelative), clues(error.message))
    }
