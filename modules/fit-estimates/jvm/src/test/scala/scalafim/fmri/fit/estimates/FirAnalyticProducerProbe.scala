package scalafim.fmri.fit.estimates

import java.nio.file.{Files, Path}
import scalafim.estimates.*
import scalafim.estimates.io.LocalEstimateStore

/** Publishes the literal nine-scan FIR case, then exits before relocation. */
object FirAnalyticProducerProbe:
  private def checked[A](value: Either[EstimateError, A]): A =
    value.fold(error => throw new IllegalStateException(error.message), identity)

  def main(args: Array[String]): Unit =
    require(args.length == 1, "dataset-root")
    val root = Path.of(args(0))
    val store = checked(LocalEstimateStore.open(root))
    val producer = FirEstimateProducerFixture.producer
    val reference = checked(producer.write(FirEstimateProducerFixture.reader, checked(store.newSink(producer.unit, 1))))
    Files.writeString(root.resolve("fir-reference.json"), ujson.write(ujson.Obj(
      "Unit" -> reference.unit.value, "Revision" -> reference.revision.value,
      "Path" -> reference.manifest.path, "SHA256" -> reference.manifest.digest.value,
      "Bytes" -> reference.manifest.bytes.toDouble), indent = 2))
    println("FIR_PRODUCER_PASS scans=9 samples=2 bins=2 products=3 blockCells=1")
