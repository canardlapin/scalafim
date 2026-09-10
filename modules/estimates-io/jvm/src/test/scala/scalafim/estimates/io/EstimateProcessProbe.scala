package scalafim.estimates.io

import java.nio.file.Path
import scalafim.estimates.*
import scalafim.image.SampleSpaces

/** Separate-JVM heap and relocation probe. The writer emits a 128 MiB Float64
  * product under -Xmx64m; the reader deliberately has no fitter on its classpath.
  */
object EstimateProcessProbe:
  private def right[A](value: Either[EstimateError, A]): A = value.fold(e => throw new IllegalStateException(e.message), identity)
  def main(args: Array[String]): Unit =
    require(args.length == 2, "write|read dataset-root")
    require(Runtime.getRuntime.maxMemory() <= 64L * 1024L * 1024L, "run with -Xmx64m")
    val store = right(LocalEstimateStore.open(Path.of(args(1))))
    if args(0) == "write" then
      val dataset = DatasetId("00000000-0000-4000-8000-000000000011")
      val model = ModelRevisionId("00000000-0000-4000-8000-000000000012")
      val id = UnitId("00000000-0000-4000-8000-000000000013")
      val revision = UnitRevisionId("00000000-0000-4000-8000-000000000014")
      val estimands = Vector.tabulate(256)(i => EstimandId(s"effect-$i"))
      val catalog = EstimandCatalog(model, estimands.map(e => EstimandDefinition(e, e.value, EstimandKind.Coefficient, "signal", "unit", e.value)))
      val observation = Observation(ObservationId("subject-01"), ParticipantId(dataset, "01"), Vector(AcquisitionId("run-1")))
      val product = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
        Vector(observation.id), ProductTargets.Scalar(estimands), PoolingScope.Run, "signal")
      val domain = right(EstimateDomain.make(SampleSpaces(Vector(64, 64, 16)), Vector.range(0, 65536), "scanner"))
      val unknown = ScientificFact.Unknown("independent synthetic producer")
      val unit = EstimateUnit(dataset, id, revision, catalog, domain, Vector(observation), Vector.empty, Vector(product),
        Map(product.id -> ProductOutcome.Available(product.id)), EstimabilityEvidence.Unknown("synthetic"),
        EstimateProvenance("independent-probe", "1", "heap-proof", unknown, unknown, unknown, unknown, Vector.empty, Vector.empty))
      val sink = right(store.newSink(unit, 1024))
      val start = System.nanoTime()
      var map = 0
      while map < estimands.size do
        var first = 0
        while first < domain.sampleCount do
          val samples = Vector.range(first, first + 1024)
          right(sink.write(product.id, EstimateSelection(Vector(observation.id), Vector(estimands(map)), samples),
            Array.tabulate(1024)(i => map * 100000.0 + first + i), Array.fill[Byte](1024)(0)))
          first += 1024
        map += 1
      val ref = right(sink.seal())
      val collection = EstimateCollection(dataset, CollectionRevisionId("00000000-0000-4000-8000-000000000015"), model, Map(id -> UnitOutcome.Published(ref)))
      val pinned = right(store.publishCollection(collection))
      right(store.discover(pinned, None))
      println(s"WRITE_PASS heap=${Runtime.getRuntime.maxMemory()} payloadBytes=134217728 cells=16777216 seconds=${(System.nanoTime() - start) / 1e9}")
    else
      require(args(0) == "read")
      val withoutFitter = try
        Class.forName("scalafim.fmri.fit.SelectedEstimates$")
        false
      catch case _: ClassNotFoundException => true
      require(withoutFitter, "the independent reader classpath must not contain fit")
      val pinned = right(store.current()).get._1
      val collection = right(store.openCollection(pinned))
      val reference = collection.units.values.collectFirst { case UnitOutcome.Published(ref) => ref }.get
      val source = right(store.open(reference, ReadLimits(4)))
      try
        val product = source.unit.products.head
        val targets = product.targets.estimands
        val values = new Array[Double](4)
        val validity = new Array[Byte](4)
        right(source.read(product.id, EstimateSelection(product.observations, Vector(targets.last, targets.head), Vector(65535, 0)), values, validity))
        require(values.toVector == Vector(25565535.0, 25500000.0, 65535.0, 0.0))
        require(validity.forall(_ == 0))
        println("READ_PASS relocated=true fitterPresent=false cells=4")
      finally right(source.close())
