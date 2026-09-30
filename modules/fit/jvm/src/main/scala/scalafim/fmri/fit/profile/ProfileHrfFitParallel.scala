package scalafim.fmri.fit.profile

import scalafim.dataset.DatasetSeriesReader

/** JVM trial execution over caller-owned, distinct readers. The caller retains
  * every reader's lifetime; this adapter only owns executor threads.
  */
object ProfileHrfFitParallel:
  extension (prepared: PreparedProfileHrf)
    def runParallel(
        readers: Vector[DatasetSeriesReader],
        sink: BlockSink[ProfileFitBlock, ProfileFitReceipt],
        cancelled: () => Boolean = () => false,
        cleanupTimeoutMillis: Long = 60000L
    ): Either[ProfileFitError, ProfileRunSummary] =
      prepared.runWithExecutor(readers, sink, cancelled, parallel = true,
        (n, budget, factory, target, stop) =>
          ParallelBlockExecutor.run(n, budget, factory, target, stop, cleanupTimeoutMillis))
