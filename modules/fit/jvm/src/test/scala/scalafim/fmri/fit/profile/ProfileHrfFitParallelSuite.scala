package scalafim.fmri.fit.profile

import scalafim.dataset.{DataSelection, DatasetError, DatasetSeriesReader, FmriDataset, FmriSeries, VoxelSelection}
import scalafim.fmri.fit.profile.ProfileHrfFitParallel.*
import scalafim.fmri.fit.profile.ProfileHrfTrialOutputsParallel.*
import scalafim.fmri.hrf.family.NormalizationRule

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.collection.mutable.ArrayBuffer

class ProfileHrfFitParallelSuite extends ProfileHrfFitSuite:
  // The two 2048-voxel matrices deliberately exercise eight actual workers even
  // at chunk size 256. Hosted Java 17 coverage takes over three minutes per
  // matrix; these are semantic parity checks, with explicit short lifecycle
  // deadlines below, rather than 30-second performance gates.
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

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

  private def publicPrepared(blockSize: Int, workers: Int, count: Int = 16): (FmriDataset, PreparedProfileTrialOutputs) =
    val (dataset, plan) = parallelFixture(count)
    val selected = if count == 16 then chosen else DataSelection(voxels = VoxelSelection.indices(
      (0 until count).map(i => (i * 257 + 15) % count)*))
    val policy = parallelPolicy(blockSize, workers).copy(budget = DecodeBudget(maxNewtonSteps = 16,
      maxJets = 20, maxExactEvaluations = 40, maxCandidateAttempts = 12, stationarityStepTolerance = 1e-8))
    val fit = parallelChecked(ProfileHrfFit.prepare(plan, selected, parallelWhitening, policy))
    (dataset, parallelChecked(fit.trialOutputs))

  private def publicSink(values: ArrayBuffer[ProfileTrialOutputBlock]) =
    new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
      def accept(block: VoxelBlock, value: ProfileTrialOutputBlock): Either[String, ProfileFitReceipt] =
        values += value
        Right(ProfileFitReceipt(value.ordinal, value.voxelIds))

  test("public conditional route preserves ordered physical axes and all numerical totals across 1/2/8 workers and 1/2/256 chunks"):
    val count = 2048 // Eight actual blocks even at chunk size 256.
    val physicalIds = (0 until count).map(i => (i * 257 + 15) % count).toVector
    def runPublic(chunk: Int, workers: Int): (ProfileRunSummary, Vector[ProfileTrialOutputVoxel]) =
      val (dataset, view) = publicPrepared(chunk, workers, count)
      val q = ProfileTrialSignedQuery.make("signed", view.axis, Vector(0.5, -0.3, 0, 0.2, -0.1, 0.7), 1e-6).toOption.get
      val values = ArrayBuffer.empty[ProfileTrialOutputBlock]
      val actualWorkers = workers
      val summary = parallelChecked(view.runParallel(readers(dataset, actualWorkers),
        OutputRequest.TrialQueries(Vector(q), NormalizationRule.Density),
        ProfileTrialReadoutMode.CorrectedReference, publicSink(values)))
      val voxels = values.toVector.flatMap(_.results)
      voxels.foreach(_.output match
        case ProfileTrialOutputOutcome.Emitted(token, value) =>
          assert(value.axis eq view.axis)
          assert(token.bank eq view.bank)
          assert(token.axis eq view.axis)
          assert(value.axis.preparation eq view.bank.preparation)
        case other => fail(s"expected accepted public output, got $other"))
      (summary, voxels)
    val (reference, expected) = runPublic(1, 1)
    for chunk <- Vector(1, 2, 256); workers <- Vector(1, 2, 8) do
      val (summary, actual) = runPublic(chunk, workers)
      assertEquals(summary.receipts.flatMap(_.voxelIds), physicalIds)
      assertEquals(summary.receipts.map(_.ordinal), summary.receipts.indices.toVector)
      assertEquals(summary.progress.workersUsed, workers)
      assertEquals(summary.progress.decoder, reference.progress.decoder)
      assertEquals(summary.progress.trial, reference.progress.trial)
      assertEquals(summary.progress.decodeStatuses, reference.progress.decodeStatuses)
      assertEquals(summary.progress.publicReadout.get.copy(storage = reference.progress.publicReadout.get.storage), reference.progress.publicReadout.get)
      assertEquals(summary.setup.bankSetup, reference.setup.bankSetup)
      actual.zip(expected).foreach { (a, b) =>
        assertEquals(a.voxelId, b.voxelId)
        assertEquals(a.selection, b.selection)
        (a.output, b.output) match
          case (ProfileTrialOutputOutcome.Emitted(ar, av), ProfileTrialOutputOutcome.Emitted(br, bv)) =>
            assertEquals(ar.index, br.index)
            assertEquals(av.axis.trialIds, bv.axis.trialIds)
            assertEquals(av.axis.conditionIds, bv.axis.conditionIds)
            assertEquals(av.axis.conditionForTrial, bv.axis.conditionForTrial)
            assertEquals(av.axis.nuisanceColumnIds, bv.axis.nuisanceColumnIds)
            assertEquals(av.axis.selectedResponseRows, bv.axis.selectedResponseRows)
            assertEquals(av.actualCoordinates, bv.actualCoordinates)
            assertEquals(av.queries, bv.queries)
            assertEquals(av.conditionMeans, bv.conditionMeans)
            assertEquals(av.nuisanceCoefficients, bv.nuisanceCoefficients)
          case other => fail(s"expected emitted pair, got $other")
      }

  test("public parallel invalid query and certificate touch no reader descriptors or cancellation callbacks"):
    val (_, view) = publicPrepared(1, 2)
    val (_, foreign) = publicPrepared(1, 2)
    val query = ProfileTrialSignedQuery.make("foreign", foreign.axis, Vector(1.0, 0, 0, 0, 0, 0), 1e-6).toOption.get
    val touched = new AtomicInteger(0)
    val poison = new DatasetSeriesReader:
      def dataset: FmriDataset = { touched.incrementAndGet(); fail("descriptor touched") }
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] = fail("read touched")
    assert(view.runParallel(Vector(poison), OutputRequest.TrialQueries(Vector(query), NormalizationRule.Density),
      ProfileTrialReadoutMode.ExactShape, publicSink(ArrayBuffer.empty), () => { touched.incrementAndGet(); true }).isLeft)
    assert(view.runParallel(Vector(poison), OutputRequest.TrialAmplitudes(NormalizationRule.Density),
      ProfileTrialReadoutMode.ExactShape, publicSink(ArrayBuffer.empty),
      evidence = ProfileTrialEvidenceRequest.CertifiedOriginalEquations).isLeft)
    assertEquals(touched.get(), 0)

  test("public sequential and parallel executions retain the same checked declaration including runtime admission failures"):
    val (dataset, view) = publicPrepared(1, 2)
    val query = ProfileTrialSignedQuery.make("same scientific query", view.axis,
      Vector(0.5, -0.3, 0, 0.2, -0.1, 0.7), 1e-6).toOption.get
    val request = OutputRequest.TrialQueries(Vector(query), NormalizationRule.Density)
    val mode = ProfileTrialReadoutMode.CorrectedReference
    val sequential = parallelChecked(view.run(parallelReader(dataset), request, mode, publicSink(ArrayBuffer.empty)))
    val parallel = parallelChecked(view.runParallel(readers(dataset, 2), request, mode, publicSink(ArrayBuffer.empty)))
    assertEquals(sequential.publicExecution, parallel.publicExecution)
    assertEquals(sequential.provenance, parallel.provenance)
    assert(parallel.publicExecution.get.axis eq view.axis)
    assertEquals(parallel.publicExecution.get.queries.head.weights.map(java.lang.Double.toHexString),
      query.weights.map(java.lang.Double.toHexString))
    assertEqualsDouble(parallel.publicExecution.get.queries.head.absoluteTolerance, query.absoluteTolerance, 0.0)
    val noReaders = view.runParallel(Vector.empty, request, mode, publicSink(ArrayBuffer.empty)).left.toOption.get
    assertEquals(noReaders.publicExecution, parallel.publicExecution)
    val invalidCleanup = view.runParallel(readers(dataset, 2), request, mode,
      publicSink(ArrayBuffer.empty), cleanupTimeoutMillis = 0L).left.toOption.get
    assert(invalidCleanup.isInstanceOf[ProfileFitError.TrialExecutionAdmission])
    assertEquals(invalidCleanup.publicExecution, parallel.publicExecution)

  test("public readout interruption/deadline finalizes once after the held real reader stops"):
    for expires <- Vector(false, true) do
      val (dataset, view) = publicPrepared(1, 2)
      val request = OutputRequest.TrialAmplitudes(NormalizationRule.Density)
      val declaration = parallelChecked(view.executionDeclaration(request, ProfileTrialReadoutMode.CorrectedReference))
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
      val sink = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
        def accept(block: VoxelBlock, payload: ProfileTrialOutputBlock): Either[String, ProfileFitReceipt] =
          sinkCalls.incrementAndGet()
          Thread.currentThread().interrupt()
          Left("reject trial block")
      val runner = new Thread(() =>
        result.set(view.runParallel(Vector(delayed(firstSource), delayed(heldSource)),
          request, ProfileTrialReadoutMode.CorrectedReference, sink,
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
              assertEquals(setup, view.prepared.setup)
              assertEquals(provenance, declaration.provenance)
              assert(!provenance.contains("|exact-readout=true"))
              val retained = result.get().left.toOption.get.publicExecution.get
              assertEquals(retained, declaration)
              assert(retained eq termination.publicExecution.get)
              assert(!termination.isTerminated)
              releaseReader.countDown()
              val finalFailure = termination.awaitFinal()
              assertEquals(finalFailure, termination.awaitFinal())
              assert(finalFailure.publicExecution.get eq retained)
              assert(termination.isTerminated)
              finalFailure match
                case ProfileFitError.SinkRefused(reason, progress) =>
                  assert(progress.publicExecution.get eq retained)
                  assertEquals(reason, "reject trial block")
                  assertEquals(progress.deliveredBlocks, 0)
                  assert(progress.attemptedVoxels >= 1 && progress.attemptedVoxels <= 2)
                  assertEquals(progress.workersUsed, 2)
                  assert(progress.trial.nonEmpty)
                  assertEquals(progress.publicReadout.get.attempts, progress.attemptedVoxels.toLong)
                  assertEquals(progress.publicReadout.get.successes, progress.attemptedVoxels.toLong)
                  assertEquals(progress.publicReadout.get.numerical.attempted.conditionalReadoutAttempts, progress.attemptedVoxels.toLong)
                  assertEquals(progress.publicReadout.get.storage.emittedTrialAmplitudeValues, 0L)
                case other => fail(s"expected final trial sink failure, got $other")
            case other => fail(s"expected nonfinal trial failure, got $other")
        else
          assert(!returned.await(50, TimeUnit.MILLISECONDS))
          releaseReader.countDown()
          assert(returned.await(5, TimeUnit.SECONDS))
          result.get() match
            case Left(ProfileFitError.SinkRefused(reason, progress)) =>
              assertEquals(progress.publicExecution, Some(declaration))
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

  private def mlPrepared(blockSize: Int, workers: Int, count: Int = 16): (FmriDataset, PreparedProfileHrf) =
    val (dataset, rawPlan) = parallelFixture(count)
    val selected = if count == 16 then chosen else DataSelection(voxels = VoxelSelection.indices(
      (0 until count).map(i => (i * 257 + 15) % count)*))
    (dataset, parallelChecked(ProfileHrfFit.prepare(withMl(rawPlan), selected, parallelWhitening,
      parallelPolicy(blockSize, workers))))

  test("ML shares one setup and preserves both work ledgers across actual 1/2/8 workers and 1/2/256 chunks"):
    val count = 2048
    val physicalIds = (0 until count).map(i => (i * 257 + 15) % count).toVector
    def runMl(chunk: Int, workers: Int): (ProfileRunSummary, Vector[ProfileVoxelResult]) =
      val (dataset, fit) = mlPrepared(chunk, workers, count)
      val values = ArrayBuffer.empty[ProfileFitBlock]
      val summary = parallelChecked(fit.runParallel(readers(dataset, workers), parallelSink(values)))
      assertEquals(summary.progress.workersUsed, workers)
      assertEquals(summary.receipts.flatMap(_.voxelIds), physicalIds)
      assertEquals(summary.receipts.map(_.ordinal), summary.receipts.indices.toVector)
      assertEquals(summary.progress.trial.get.voxels, count.toLong)
      assertEquals(summary.progress.trial.get.attempted.readoutAttempts, count.toLong)
      assertEquals(summary.progress.trial.get.attempted.exactReadoutFactorAttempts, count.toLong)
      assertEquals(summary.progress.trial.get.attempted.referenceAttempts,
        summary.progress.trialMl.get.referenceAttempts + count)
      (summary, values.toVector.flatMap(_.results))
    val (reference, expected) = runMl(1, 1)
    for chunk <- Vector(1, 2, 256); workers <- Vector(1, 2, 8) do
      val (summary, actual) = runMl(chunk, workers)
      assertEquals(summary.setup, reference.setup)
      assertEquals(summary.progress.trial, reference.progress.trial)
      assertEquals(summary.progress.trialMl, reference.progress.trialMl)
      assertEquals(summary.progress.decoder, reference.progress.decoder)
      assertEquals(summary.progress.decodeStatuses, reference.progress.decodeStatuses)
      assertEquals(actual, expected)
      assert(actual.forall(_.criterionEvidence.nonEmpty))

  test("ML interruption/deadline captures both final ledgers once after its held real reader stops"):
    for expires <- Vector(false, true) do
      val (dataset, fit) = mlPrepared(1, 2)
      val heldStarted = new CountDownLatch(1)
      val readerInterrupted = new CountDownLatch(1)
      val releaseReader = new CountDownLatch(1)
      val returned = new CountDownLatch(1)
      val active = new AtomicInteger(0)
      val deliveries = new AtomicInteger(0)
      val restored = new AtomicBoolean(false)
      val result = new AtomicReference[Either[ProfileFitError, ProfileRunSummary]]()
      def delayed(): DatasetSeriesReader =
        val source = parallelReader(dataset)
        new DatasetSeriesReader:
          val dataset: FmriDataset = source.dataset
          def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
            val series = source.seriesEither(selection)
            if series.toOption.exists(_.voxelIndices.head == ids.head) then
              if !heldStarted.await(5, TimeUnit.SECONDS) then throw new IllegalStateException("held ML reader did not start")
            else
              active.incrementAndGet()
              heldStarted.countDown()
              try
                var stopped = false
                while !stopped do
                  try stopped = releaseReader.await(10, TimeUnit.SECONDS)
                  catch case _: InterruptedException => readerInterrupted.countDown()
              finally
                active.decrementAndGet()
                ()
            series
      val reject = new BlockSink[ProfileFitBlock, ProfileFitReceipt]:
        def accept(block: VoxelBlock, payload: ProfileFitBlock): Either[String, ProfileFitReceipt] =
          assert(payload.results.forall(_.criterionEvidence.nonEmpty))
          deliveries.incrementAndGet()
          Thread.currentThread().interrupt()
          Left("reject ML block")
      val runner = new Thread(() =>
        result.set(fit.runParallel(Vector(delayed(), delayed()), reject,
          cleanupTimeoutMillis = if expires then 40L else 60000L))
        restored.set(Thread.currentThread().isInterrupted)
        returned.countDown()
      )
      def checkFinal(error: ProfileFitError): Unit = error match
        case ProfileFitError.SinkRefused(reason, progress) =>
          assertEquals(reason, "reject ML block")
          assertEquals(progress.deliveredBlocks, 0)
          assert(progress.attemptedVoxels >= 1 && progress.attemptedVoxels <= 2)
          assertEquals(progress.trial.get.voxels, progress.attemptedVoxels.toLong)
          assertEquals(progress.trial.get.attempted.readoutAttempts, progress.attemptedVoxels.toLong)
          assertEquals(progress.trial.get.attempted.exactReadoutFactorAttempts, progress.attemptedVoxels.toLong)
          assert(progress.trialMl.get.solveAttempts > 0L)
          assertEquals(progress.trial.get.attempted.referenceAttempts,
            progress.trialMl.get.referenceAttempts + progress.attemptedVoxels)
        case other => fail(s"expected final ML sink refusal, got $other")
      runner.start()
      try
        val interrupted = readerInterrupted.await(5, TimeUnit.SECONDS)
        val observed = Option(result.get()).map(_.fold(_.message, _ => "completed"))
        assert(interrupted, clues(expires, observed, runner.isAlive, heldStarted.getCount,
          returned.getCount, active.get(), deliveries.get()))
        assertEquals(active.get(), 1)
        if expires then
          assert(returned.await(5, TimeUnit.SECONDS))
          result.get() match
            case Left(ProfileFitError.WorkersStillRunning(_, receipts, setup, provenance, termination)) =>
              assertEquals(receipts, Vector.empty[ProfileFitReceipt])
              assertEquals(setup, fit.setup)
              assert(setup.mlSetup.nonEmpty)
              assertEquals(provenance, fit.provenance)
              assert(provenance.contains("terminal-evidence=required-at-returned-shape"))
              assert(!termination.isTerminated)
              releaseReader.countDown()
              val finalError = termination.awaitFinal()
              assertEquals(finalError, termination.awaitFinal())
              assert(termination.isTerminated)
              checkFinal(finalError)
            case other => fail(s"expected unfinished ML outcome, got $other")
        else
          assert(!returned.await(50, TimeUnit.MILLISECONDS))
          releaseReader.countDown()
          assert(returned.await(5, TimeUnit.SECONDS))
          checkFinal(result.get().left.toOption.get)
        assertEquals(active.get(), 0)
        assertEquals(deliveries.get(), 1)
        assert(restored.get())
      finally
        releaseReader.countDown()
        runner.join(5000)
      assert(!runner.isAlive)
      assertEquals(deliveries.get(), 1)
