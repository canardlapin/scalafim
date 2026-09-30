package scalafim.fmri.fit.estimates

import java.nio.file.{Files, Path}
import scalafim.archive.ContentDigest
import scalafim.estimates.*
import scalafim.estimates.io.{CovarianceLayout, LocalEstimateStore}
import scalafim.fmri.group.*

object PooledGroupProducerProbe:
  def main(args: Array[String]): Unit =
    import PooledProducerFixture.*
    val store = checked(LocalEstimateStore.open(Path.of(args(0))))
    def entry(pinned: PinnedUnit, unit: EstimateUnit) =
      ujson.Obj("Unit" -> pinned.unit.value, "Revision" -> pinned.revision.value,
        "Path" -> pinned.manifest.path, "SHA256" -> pinned.manifest.digest.value, "Bytes" -> pinned.manifest.bytes.toDouble,
        "Observation" -> unit.observations.head.id.value, "Effect" -> unit.products.find(_.kind == ProductKind.Effect).get.id.value,
        "SE" -> unit.products.find(_.kind == ProductKind.StandardError).get.id.value)
    val identities = Vector(publication, publication.copy(unit = UnitId("00000000-0000-4000-8000-000000000112"),
      revision = UnitRevisionId("00000000-0000-4000-8000-000000000113"), participantLabel = "02", observation = ObservationId("pooled-02")))
    val inputs = identities.zipWithIndex.map: (identity, i) =>
      val f = new NativePooledFixture(i * 0.75)
      val p = f.producer(blockSize = args(1).toInt, identity = identity)
      val reader = f.reader
      val pinned = checked(p.write(reader, checked(store.newSink(p.unit, args(1).toInt, CovarianceLayout.PairNifti))))
      require(!reader.closed)
      entry(pinned, p.unit)
    val f = new NativePooledFixture()
    val runIdentity = publication.copy(unit = UnitId("00000000-0000-4000-8000-000000000122"),
      revision = UnitRevisionId("00000000-0000-4000-8000-000000000123"), observation = ObservationId("constituent-01"))
    val runPlan = checked(scalafim.fmri.fit.FirstLevelEstimates.prepare(scalafim.fmri.model.FitPlan(f.model),
      f.request(scalafim.fmri.fit.EstimateUncertaintyRequest.Marginal), scalafim.fmri.fit.ChunkSize.unsafe(2),
      scalafim.dataset.DataSelection(time = scalafim.dataset.IndexSelection.Indices(Vector(4,5,6,7)),
        voxels = scalafim.dataset.IndexSelection.Indices(Vector(0,1,2)))))
    val runProducer = checked(FitEstimateProducer.shared(runPlan, runIdentity, f.catalog(f.prepared()), ids, "scanner"))
    val runRef = checked(runProducer.write(f.reader, checked(store.newSink(runProducer.unit, 2))))
    Files.writeString(store.root.resolve("group-inputs.json"), ujson.write(ujson.Obj("Inputs" -> ujson.Arr.from(inputs),
      "Constituent" -> entry(runRef, runProducer.unit)), indent = 2))
    val runtime = Runtime.getRuntime
    println(s"POOLED_PRODUCER_PASS participants=2 blockSize=${args(1)} sampledHeapBytes=${runtime.totalMemory-runtime.freeMemory} maxHeapBytes=${runtime.maxMemory}")

/** Copy only this entry point to a classpath containing IO/estimates/group and
  * their dependencies. Neither producer, native fit, nor test fixtures is used.
  */
object PooledGroupReadbackProbe:
  private def checked[A](result: Either[EstimateError,A]): A = result.fold(e => throw new IllegalArgumentException(e.message), identity)
  private val ids = Vector(EstimandId("z-signed"), EstimandId("α-first"), EstimandId("a-mixed"))
  private val weights = Vector(Vector(1.0,-2.0), Vector(1.0,0.0), Vector(-0.5,1.5))
  private val reason = "variance estimated from run residual variances; effective degrees of freedom for estimated inverse-covariance pooling are unavailable"
  private def expected(sample: Int, shift: Double): (Vector[Double],Vector[Vector[Double]]) =
    // Literal inverses from independent rational 2x2 algebra, not native solvers.
    val sigma = sample match
      case 0 => Vector(Vector(1.0/5,0.0),Vector(0.0,1.0/3))
      case 1 => Vector(Vector(2.0/5,-2.0/15),Vector(-2.0/15,22.0/45))
      case 2 => Vector(Vector(5.0/17,-1.0/17),Vector(-1.0/17,7.0/17))
      case _ => throw new IllegalArgumentException("oracle sample")
    val s2 = if sample == 0 then 1.0 else if sample == 1 then 4.0 else 2.0
    val b1 = Vector(1.0+sample+shift,-2.0+sample-shift)
    val b2 = Vector(5.0-sample+shift,3.0+2*sample-shift)
    val rhs = Vector(2*b1(0)+b1(1)+(3*b2(0)-b2(1))/s2, b1(0)+2*b1(1)+(-b2(0)+b2(1))/s2)
    val beta = sigma.map(row => row.zip(rhs).map(_ * _).sum)
    (weights.map(row => row.zip(beta).map(_ * _).sum),
      weights.map(a => weights.map(b => (for i <- 0 until 2; j <- 0 until 2 yield a(i)*sigma(i)(j)*b(j)).sum)))
  private def close(actual: Double, expected: Double): Unit = require(math.abs(actual-expected) <= 1e-10, s"$actual != $expected")

  def main(args: Array[String]): Unit =
    val absent = Vector("scalafim.fmri.fit.SelectedEstimates$", "scalafim.fmri.fit.FirstLevelFixedEffectsEstimates$",
      "scalafim.fmri.fit.estimates.PooledFitEstimateProducer$", "scalafim.fmri.fit.estimates.FitEstimateProducer$",
      "scalafim.fmri.fit.estimates.PooledProducerFixture$", "scalafim.fmri.fit.estimates.PooledGroupProducerProbe$")
    absent.foreach: name =>
      val missing = try
        Class.forName(name)
        false
      catch case _: ClassNotFoundException => true
      require(missing, s"producer/fitter fixture must be absent: $name")
    val store = checked(LocalEstimateStore.open(Path.of(args(0))))
    val declarations = ujson.read(Files.readString(store.root.resolve("group-inputs.json")))
    def input(value: ujson.Value): GroupEstimateInput =
      val pinned = PinnedUnit(UnitId(value("Unit").str), UnitRevisionId(value("Revision").str),
        FileReference(value("Path").str, ContentDigest.unsafeSha256(value("SHA256").str), value("Bytes").num.toLong))
      GroupEstimateInput(pinned, ObservationId(value("Observation").str), ProductId(value("Effect").str),
        Some(GroupMarginalUncertainty.StandardError(ProductId(value("SE").str))))
    val inputs = declarations("Inputs").arr.toVector.map(input)
    val admission = new GroupEstimateAdmission:
      def verify(units: Vector[EstimateUnit]) =
        if units.forall(u => u.domain.dimensions == Vector(5,1,1) && u.domain.worldFrame == "scanner") then
          Right(GroupGeometryEvidence.Verified("scanner", "explicit identical synthetic scanner grid and native pooled method admission", Vector.empty))
        else Left(EstimateError.Invalid("unexpected synthetic grid"))
    val budgeted = checked(EstimateGroup.prepare(store, inputs, ids.reverse, admission, 32))
    require(budgeted.readBlock(Vector(2,0,1)).left.toOption.exists(_.message == "group block exceeds the total effect/variance cell budget"))
    val group = checked(EstimateGroup.prepare(store, inputs, ids.reverse, admission, 36))
    val samples = Vector(2,0,1)
    val data = checked(group.readBlock(samples)).data
    require(data.nSubjects == 2)
    for subject <- 0 until 2; position <- samples.indices; row <- ids.indices do
      val response = data.response(ids(row).value).get
      val (effect,covariance) = expected(samples(position), subject*0.75)
      close(response.effects(subject,position), effect(row))
      close(response.variances.get(subject,position), covariance(row)(row))
    require(data.uncertainty.get.sources.forall(_.origin == GroupVarianceOrigin.Unknown(reason)))
    var scalarCells = 0
    var pairCells = 0
    inputs.zipWithIndex.foreach: (in,subject) =>
      val source = checked(store.open(in.reference, ReadLimits(1)))
      try
        val unit = source.unit
        require(unit.products.forall(_.pooling == PoolingScope.PooledRuns))
        require(!unit.products.exists(_.kind == ProductKind.ResidualVariance))
        require(unit.degreesOfFreedom.head.value == DfValue.Scalar(4.0) && unit.degreesOfFreedom.head.method.contains("descriptive only"))
        require(unit.provenance.scans.map(_.zeroBasedRows) == Vector(Vector(0,1,2,3),Vector(0,1,2,3)))
        require(unit.observations.head.acquisitions == Vector(AcquisitionId("acq-A"),AcquisitionId("acq-B")))
        require(unit.bindings.map(_.weights) == weights)
        require(unit.bindings.forall(_.columnIds == unit.bindings.head.columnIds) && unit.bindings.head.columnIds.size == 2)
        require(unit.domain.support == Vector(2,4,0,3,1))
        require(unit.marginalUncertainty.head.origin == MarginalVarianceOrigin.Unknown(reason))
        val cov = unit.covariance.head
        require(cov.equation == CovarianceEquation.Absolute && !cov.invariantSamples)
        val value = new Array[Double](1)
        val validity = new Array[Byte](1)
        for product <- unit.products if product.kind != ProductKind.Covariance; row <- ids.indices; sample <- 0 until 5 do
          checked(source.read(product.id, EstimateSelection(product.observations, Vector(ids(row)), Vector(sample)), value, validity))
          val wanted = if sample >= 3 then 0.0 else if product.kind == ProductKind.Effect then expected(sample,subject*0.75)._1(row)
            else math.sqrt(expected(sample,subject*0.75)._2(row)(row))
          close(value(0),wanted)
          require(validity(0) == (if sample >= 3 then Validity.NonEstimable.code else Validity.Valid.code))
          scalarCells += 1
        for i <- ids.indices; j <- i until ids.size; sample <- 0 until 5 do
          checked(source.readCovariance(cov.product, CovarianceSelection(Vector(in.observation),Vector(EstimandPair(ids(i),ids(j))),Vector(sample)),value,validity))
          close(value(0),if sample >= 3 then 0.0 else expected(sample,subject*0.75)._2(i)(j))
          require(validity(0) == (if sample >= 3 then Validity.NonEstimable.code else Validity.Valid.code))
          pairCells += 1
        for sample <- samples do
          val matrix = checked(CovarianceAccess.matrix(source,cov.product,in.observation,sample,ids.reverse,CovariancePolicy(3,1e-12,1e-12)))
          close(matrix.varianceScale,1.0)
          for i <- ids.indices; j <- ids.indices do close(matrix.values(i,j),expected(sample,subject*0.75)._2(2-i)(2-j))
      finally checked(source.close())
    val refused = EstimateGroup.prepare(store,Vector(inputs.head,input(declarations("Constituent"))),ids,admission,32)
    require(refused.left.toOption.exists(_.message == "group units require one dataset, shared immutable catalog, effect units and pooling scope"))
    val duplicate = EstimateGroup.prepare(store,Vector(inputs.head,inputs.head),ids,admission,32)
    require(duplicate.left.toOption.exists(_.message == "runs, trials and pooled rows from one participant cannot become independent group subjects"))
    require(group.readBlock(Vector(3)).isLeft)
    val runtime = Runtime.getRuntime
    println(s"POOLED_READER_PASS relocated=true fitterPresent=false producerPresent=false participants=2 scalarCells=$scalarCells pairCells=$pairCells pooledPlusConstituentScopeRefused=true homogeneousDuplicateParticipantRefused=true budget32Refused=true sampledHeapBytes=${runtime.totalMemory-runtime.freeMemory} maxHeapBytes=${runtime.maxMemory}")
