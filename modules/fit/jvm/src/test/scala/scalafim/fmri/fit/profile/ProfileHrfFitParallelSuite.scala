package scalafim.fmri.fit.profile

import scalafim.dataset.{DataSelection, DatasetError, DatasetSeriesReader, FmriDataset, FmriSeries, VoxelSelection}
import scalafim.fmri.fit.profile.ProfileHrfFitParallel.*

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.collection.mutable.ArrayBuffer

class ProfileHrfFitParallelSuite extends ProfileHrfFitSuite:
  private val ids = Vector(15, 0, 11, 2, 8, 4, 1, 13, 3, 14, 5, 12)
  private val chosen = DataSelection(voxels = VoxelSelection.indices(ids*))

  private def prepared(blockSize: Int, workers: Int): (FmriDataset, PreparedProfileHrf) =
    val (dataset, plan) = parallelFixture(16)
    (dataset, parallelChecked(ProfileHrfFit.prepare(plan, chosen, parallelWhitening,
      parallelPolicy(blockSize, workers))))

  private def readers(dataset: FmriDataset, count: Int): Vector[DatasetSeriesReader] =
    Vector.fill(count) {
      val source = parallelReader(dataset)
      new DatasetSeriesReader:
        val dataset: FmriDataset = source.dataset
        def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] = source.seriesEither(selection)
    }

  private def run(blockSize: Int, workers: Int): (ProfileRunSummary, Vector[ProfileFitBlock]) =
    val (dataset, fit) = prepared(blockSize, workers)
    val payloads = ArrayBuffer.empty[ProfileFitBlock]
    val count = math.min(workers, (ids.length + blockSize - 1) / blockSize)
    val result = parallelChecked(fit.runParallel(readers(dataset, count), parallelSink(payloads)))
    (result, payloads.toVector)

  private def sameResults(expected: Vector[ProfileFitBlock], actual: Vector[ProfileFitBlock]): Unit =
    val left = expected.flatMap(_.results)
    val right = actual.flatMap(_.results)
    assertEquals(left.map(_.voxelId), ids)
    assertEquals(right.map(_.voxelId), ids)
    left.zip(right).foreach { (a, b) =>
      assertEquals(a.status, b.status)
      assertEquals(a.coordinates.length, b.coordinates.length)
      a.coordinates.zip(b.coordinates).foreach((x, y) => assertEqualsDouble(x, y, 1e-10))
      assertEqualsDouble(a.penalizedEnergy, b.penalizedEnergy, 1e-8)
      a.conditionMeans.zip(b.conditionMeans).foreach((x, y) => assertEqualsDouble(x, y, 1e-8))
      (a.readout, b.readout) match
        case (ProfileAmplitudeReadout.AdaptiveTrial(x), ProfileAmplitudeReadout.AdaptiveTrial(y)) =>
          x.trialAmplitudes.zip(y.trialAmplitudes).foreach((u, v) => assertEqualsDouble(u, v, 1e-8))
        case other => fail(s"expected trial readouts, got $other")
    }

  test("off-node trial output, ordered receipts and attempted work are invariant across windows"):
    val (reference, referencePayloads) = run(1, 1)
    assertEquals(reference.progress.attemptedVoxels, ids.length)
    assertEquals(reference.progress.decodeStatuses.values.sum, ids.length.toLong)
    assert(reference.setup.bankSetup.exists(_.nodeReferenceAttempts > 0L))
    for blockSize <- Vector(1, 2, 256); workers <- Vector(1, 2, 8) do
      val (summary, payloads) = run(blockSize, workers)
      val expectedWorkers = math.min(workers, (ids.length + blockSize - 1) / blockSize)
      assertEquals(summary.progress.workersUsed, expectedWorkers)
      assertEquals(summary.progress.deliveredVoxels, ids.length)
      assertEquals(summary.progress.attemptedVoxels, ids.length)
      assertEquals(summary.receipts.map(_.ordinal), (0 until summary.receipts.length).toVector)
      assertEquals(summary.receipts.flatMap(_.voxelIds), ids)
      assertEquals(summary.setup.bankSetup, reference.setup.bankSetup)
      assertEquals(summary.progress.trial, reference.progress.trial)
      assertEquals(summary.progress.decoder, reference.progress.decoder)
      assertEquals(summary.progress.decodeStatuses, reference.progress.decodeStatuses)
      sameResults(referencePayloads, payloads)

  test("shared sequential run treats worker budget as an upper bound and starts fresh state each time"):
    val (dataset, fit) = prepared(1, 8)
    val first = ArrayBuffer.empty[ProfileFitBlock]
    val a = parallelChecked(fit.run(parallelReader(dataset), parallelSink(first)))
    val second = ArrayBuffer.empty[ProfileFitBlock]
    val b = parallelChecked(fit.run(parallelReader(dataset), parallelSink(second)))
    assertEquals(a.progress.workersUsed, 1)
    assertEquals(a.progress.trial, b.progress.trial)
    sameResults(first.toVector, second.toVector)

  test("parallel admission checks reader identity and budget before payload reads"):
    val (dataset, fit) = prepared(1, 2)
    val reads = new AtomicInteger(0)
    val source = parallelReader(dataset)
    val counting = new DatasetSeriesReader:
      val dataset: FmriDataset = source.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        reads.incrementAndGet()
        source.seriesEither(selection)
    val payloads = ArrayBuffer.empty[ProfileFitBlock]
    assert(fit.runParallel(Vector(counting, counting), parallelSink(payloads)).left.toOption
      .exists(_.isInstanceOf[ProfileFitError.Dataset]))
    assertEquals(reads.get(), 0)
    val (foreign, _) = parallelFixture(17)
    val wrong = new DatasetSeriesReader:
      val dataset: FmriDataset = foreign
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        fail("descriptor mismatch must be detected before read")
    assert(fit.runParallel(Vector(counting, wrong), parallelSink(payloads)).left.toOption
      .exists(_.isInstanceOf[ProfileFitError.Dataset]))
    assertEquals(reads.get(), 0)
    for bad <- Vector(ExecutionBudget(1, 9), ExecutionBudget(257, 1)) do
      val (_, plan) = parallelFixture(16)
      assert(ProfileHrfFit.prepare(plan, chosen, parallelWhitening,
        parallelPolicy(1, 1).copy(execution = bad)).left.toOption
        .exists(_.isInstanceOf[ProfileFitError.Unsupported]))
    assertEquals(reads.get(), 0)

  test("JVM parallel entry refuses the fixed condition route before a read"):
    val fit = fixedPrepared
    val reader = new DatasetSeriesReader:
      val dataset: FmriDataset = fit.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        fail("fixed route must be refused before read")
    assert(fit.runParallel(Vector(reader), parallelSink(ArrayBuffer.empty[ProfileFitBlock])).left.toOption
      .exists(_.isInstanceOf[ProfileFitError.Unsupported]))

  test("a later read can finish first while sink delivery remains ordered"):
    val (dataset, fit) = prepared(1, 2)
    val laterRead = new CountDownLatch(1)
    val firstWaited = new AtomicBoolean(false)
    val source = parallelReader(dataset)
    def delayed(): DatasetSeriesReader = new DatasetSeriesReader:
      val dataset: FmriDataset = source.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        source.seriesEither(selection).map { series =>
          if series.voxelIndices.head == ids.head then
            firstWaited.set(true)
            if !laterRead.await(5, TimeUnit.SECONDS) then throw new IllegalStateException("later read did not arrive")
          else laterRead.countDown()
          series
        }
    val payloads = ArrayBuffer.empty[ProfileFitBlock]
    val summary = parallelChecked(fit.runParallel(Vector(delayed(), delayed()), parallelSink(payloads)))
    assert(firstWaited.get())
    assertEquals(summary.receipts.flatMap(_.voxelIds), ids)

  test("reader, sink and cancellation failures report actual first-window work and no later delivery"):
    val (dataset, fit) = prepared(1, 2)
    val source = parallelReader(dataset)
    val firstDelivered = new CountDownLatch(1)
    val reads = new AtomicInteger(0)
    def failingReader(): DatasetSeriesReader = new DatasetSeriesReader:
      val dataset: FmriDataset = source.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        val series = source.seriesEither(selection)
        reads.incrementAndGet()
        if series.toOption.exists(_.voxelIndices.head == ids(1)) then
          if !firstDelivered.await(5, TimeUnit.SECONDS) then throw new IllegalStateException("first receipt missing")
          throw new IllegalStateException("reader failed")
        series
    val received = ArrayBuffer.empty[ProfileFitBlock]
    val firstSink = new BlockSink[ProfileFitBlock, ProfileFitReceipt]:
      def accept(block: VoxelBlock, payload: ProfileFitBlock): Either[String, ProfileFitReceipt] =
        received += payload
        firstDelivered.countDown()
        Right(ProfileFitReceipt(payload.ordinal, payload.voxelIds))
    fit.runParallel(Vector(failingReader(), failingReader()), firstSink) match
      case Left(ProfileFitError.Dataset(detail, progress)) =>
        assert(detail.contains("reader failed"))
        assertEquals(progress.deliveredBlocks, 1)
        assertEquals(progress.attemptedVoxels, 1)
        assertEquals(progress.workersUsed, 2)
      case other => fail(s"expected typed reader failure, got $other")
    assertEquals(received.length, 1)
    assertEquals(reads.get(), 2)

    val thrown = ArrayBuffer.empty[ProfileFitBlock]
    val throwing = new BlockSink[ProfileFitBlock, ProfileFitReceipt]:
      def accept(block: VoxelBlock, payload: ProfileFitBlock): Either[String, ProfileFitReceipt] =
        thrown += payload
        throw new IllegalStateException("sink failed")
    fit.runParallel(readers(dataset, 2), throwing) match
      case Left(ProfileFitError.SinkThrew(detail, progress)) =>
        assert(detail.contains("sink failed"))
        assertEquals(progress.deliveredBlocks, 0)
        assert(progress.attemptedVoxels >= 1 && progress.attemptedVoxels <= 2)
      case other => fail(s"expected typed sink failure, got $other")
    assertEquals(thrown.length, 1)

    val stopped = new AtomicBoolean(false)
    val cancelledPayloads = ArrayBuffer.empty[ProfileFitBlock]
    val stopping = new BlockSink[ProfileFitBlock, ProfileFitReceipt]:
      def accept(block: VoxelBlock, payload: ProfileFitBlock): Either[String, ProfileFitReceipt] =
        cancelledPayloads += payload
        stopped.set(true)
        Right(ProfileFitReceipt(payload.ordinal, payload.voxelIds))
    fit.runParallel(readers(dataset, 2), stopping, () => stopped.get()) match
      case Left(ProfileFitError.Cancelled(progress)) =>
        assertEquals(progress.deliveredBlocks, 1)
        assert(progress.attemptedVoxels >= 1 && progress.attemptedVoxels <= 2)
      case other => fail(s"expected typed cancellation, got $other")
    assertEquals(cancelledPayloads.length, 1)

    fit.runParallel(readers(dataset, 2), parallelSink(ArrayBuffer.empty[ProfileFitBlock]),
      () => throw new IllegalStateException("callback failed")) match
      case Left(ProfileFitError.Backend(detail, progress)) =>
        assert(detail.contains("callback failed"))
        assertEquals(progress.deliveredBlocks, 0)
        assertEquals(progress.attemptedVoxels, 0)
      case other => fail(s"expected typed callback failure, got $other")

  test("rejecting sink interruption retains caller readers until final trial progress is safe"):
    for expires <- Vector(false, true) do
      val (dataset, fit) = prepared(1, 2)
      val heldStarted = new CountDownLatch(1)
      val readerInterrupted = new CountDownLatch(1)
      val releaseReader = new CountDownLatch(1)
      val returned = new CountDownLatch(1)
      val activeReaders = new AtomicInteger(0)
      val sinkCalls = new AtomicInteger(0)
      val restored = new AtomicBoolean(false)
      val result = new AtomicReference[Either[ProfileFitError, ProfileRunSummary]]()
      val firstSource = parallelReader(dataset)
      val heldSource = parallelReader(dataset)
      def delayed(source: DatasetSeriesReader): DatasetSeriesReader = new DatasetSeriesReader:
        val dataset: FmriDataset = source.dataset
        def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
          val series = source.seriesEither(selection)
          if series.toOption.exists(_.voxelIndices.head == ids.head) then
            if !heldStarted.await(5, TimeUnit.SECONDS) then throw new IllegalStateException("held reader did not start")
          else
            activeReaders.incrementAndGet()
            heldStarted.countDown()
            try
              var done = false
              while !done do
                try done = releaseReader.await(10, TimeUnit.SECONDS)
                catch case _: InterruptedException => readerInterrupted.countDown()
              if !done then throw new IllegalStateException("reader was not released")
            finally
              activeReaders.decrementAndGet()
              ()
          series
      val sink = new BlockSink[ProfileFitBlock, ProfileFitReceipt]:
        def accept(block: VoxelBlock, payload: ProfileFitBlock): Either[String, ProfileFitReceipt] =
          sinkCalls.incrementAndGet()
          Thread.currentThread().interrupt()
          Left("reject trial block")
      val runner = new Thread(() =>
        result.set(fit.runParallel(Vector(delayed(firstSource), delayed(heldSource)), sink,
          cleanupTimeoutMillis = if expires then 40L else 60000L))
        restored.set(Thread.currentThread().isInterrupted)
        returned.countDown()
      )
      runner.start()
      try
        assert(readerInterrupted.await(5, TimeUnit.SECONDS))
        assertEquals(activeReaders.get(), 1)
        if expires then
          assert(returned.await(5, TimeUnit.SECONDS))
          assertEquals(activeReaders.get(), 1)
          result.get() match
            case Left(ProfileFitError.WorkersStillRunning(
                ExecutionError.SinkFailed(block, detail), receipts, setup, provenance, termination)) =>
              assertEquals(block.index, 0)
              assertEquals(detail, "reject trial block")
              assertEquals(receipts, Vector.empty[ProfileFitReceipt])
              assertEquals(setup, fit.setup)
              assertEquals(provenance, fit.provenance)
              assert(!termination.isTerminated)
              releaseReader.countDown()
              val finalFailure = termination.awaitFinal()
              assertEquals(finalFailure, termination.awaitFinal())
              assert(termination.isTerminated)
              finalFailure match
                case ProfileFitError.SinkRefused(reason, progress) =>
                  assertEquals(reason, "reject trial block")
                  assertEquals(progress.deliveredBlocks, 0)
                  assert(progress.attemptedVoxels >= 1 && progress.attemptedVoxels <= 2)
                  assertEquals(progress.workersUsed, 2)
                  assert(progress.trial.nonEmpty)
                case other => fail(s"expected final trial sink failure, got $other")
            case other => fail(s"expected nonfinal trial failure, got $other")
        else
          assert(!returned.await(50, TimeUnit.MILLISECONDS))
          releaseReader.countDown()
          assert(returned.await(5, TimeUnit.SECONDS))
          result.get() match
            case Left(ProfileFitError.SinkRefused(reason, progress)) =>
              assertEquals(reason, "reject trial block")
              assertEquals(progress.deliveredBlocks, 0)
              assert(progress.attemptedVoxels >= 1 && progress.attemptedVoxels <= 2)
              assertEquals(progress.workersUsed, 2)
            case other => fail(s"expected final trial sink failure, got $other")
        assertEquals(activeReaders.get(), 0)
        assertEquals(sinkCalls.get(), 1)
        assert(restored.get())
      finally
        releaseReader.countDown()
        runner.join(5000)
      assert(!runner.isAlive)
