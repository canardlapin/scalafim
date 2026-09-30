package scalafim.fmri.fit.estimates

import java.nio.file.{Files, Path}
import scalafim.archive.ContentDigest
import scalafim.estimates.*
import scalafim.estimates.io.{CovarianceLayout, LocalEstimateStore}
import scalafim.fmri.group.*

object CompactGroupProducerProbe:
  def main(args: Array[String]): Unit =
    val store = LocalEstimateStore.open(Path.of(args(0))).toOption.get
    val fixture = ProducerFixture
    val identities = Vector(fixture.identity, fixture.identity.copy(
      unit = UnitId("00000000-0000-4000-8000-000000000012"),
      revision = UnitRevisionId("00000000-0000-4000-8000-000000000013"), participantLabel = "02"))
    val inputs = identities.map: identity =>
      val producer = FitEstimateProducer.shared(fixture.prepared(scalafim.fmri.fit.EstimateUncertaintyRequest.Joint, 2),
        identity, fixture.catalog, fixture.ids, "scanner").toOption.get
      val sink = store.newSink(producer.unit, 2, CovarianceLayout.SharedNormalizedTable()).toOption.get
      val pinned = producer.write(fixture.reader, sink).toOption.get
      ujson.Obj("Unit" -> pinned.unit.value, "Revision" -> pinned.revision.value,
        "Path" -> pinned.manifest.path, "SHA256" -> pinned.manifest.digest.value, "Bytes" -> pinned.manifest.bytes.toDouble,
        "Observation" -> identity.observation.value, "Effect" -> producer.unit.products.find(_.kind == ProductKind.Effect).get.id.value,
        "SE" -> producer.unit.products.find(_.kind == ProductKind.StandardError).get.id.value)
    Files.writeString(store.root.resolve("group-inputs.json"), ujson.write(ujson.Arr.from(inputs), indent = 2))
    println("GROUP_PRODUCER_PASS compact=true participants=2 blockSize=2")

/** This entry point imports only IO, estimates and group. The controller removes
  * fitter and producer classes from its independently hashed runtime classpath.
  */
object CompactGroupReadbackProbe:
  def main(args: Array[String]): Unit =
    val absent = try
      Class.forName("scalafim.fmri.fit.SelectedEstimates$")
      false
    catch case _: ClassNotFoundException => true
    require(absent, "fitter must be absent from fresh consumer process")
    val store = LocalEstimateStore.open(Path.of(args(0))).toOption.get
    val inputs = ujson.read(Files.readString(store.root.resolve("group-inputs.json"))).arr.toVector.map: value =>
      val pinned = PinnedUnit(UnitId(value("Unit").str), UnitRevisionId(value("Revision").str),
        FileReference(value("Path").str, ContentDigest.unsafeSha256(value("SHA256").str), value("Bytes").num.toLong))
      GroupEstimateInput(pinned, ObservationId(value("Observation").str), ProductId(value("Effect").str),
        Some(GroupMarginalUncertainty.StandardError(ProductId(value("SE").str))))
    val ids = Vector(EstimandId("intercept"), EstimandId("task"))
    val admission = new GroupEstimateAdmission:
      def verify(units: Vector[EstimateUnit]) =
        if units.forall(u => u.domain.dimensions == Vector(2, 1, 1) && u.domain.worldFrame == "scanner") then
          Right(GroupGeometryEvidence.Verified("scanner", "owned identical synthetic scanner grid", Vector.empty))
        else Left(EstimateError.Invalid("unexpected geometry"))
    val group = EstimateGroup.prepare(store, inputs, ids, admission, 16).toOption.get
    val block = group.readBlock(Vector(1, 0)).toOption.get
    require(math.abs(block.data.response("task").get.effects(0, 1) - 2.0) <= 1e-12)
    require(math.abs(block.data.response("intercept").get.effects(1, 0) - 5.0) <= 1e-12)
    require(math.abs(block.data.response("task").get.variances.get(0, 0) - 1.6) <= 1e-12)
    require(block.data.uncertainty.get.sources.forall(_.origin == GroupVarianceOrigin.Estimated(
      GroupDegreesOfFreedom(DfRole.Residual, GroupDfValues.Scalar(2.0), "OLS n - numerical rank", false))))
    val source = store.open(inputs.head.reference, ReadLimits(1)).toOption.get
    try
      val covariance = source.unit.covariance.head
      val matrix = CovarianceAccess.matrix(source, covariance.product, inputs.head.observation, 1, ids,
        CovariancePolicy(2, 1e-12, 1e-12)).toOption.get
      require(math.abs(matrix.values(0, 0) - 5.6) <= 1e-12)
      require(math.abs(matrix.values(0, 1) + 2.4) <= 1e-12)
      require(math.abs(matrix.values(1, 1) - 1.6) <= 1e-12)
    finally source.close().toOption.get
    println("GROUP_READER_PASS relocated=true fitterPresent=false participants=2 joint=true marginal=true")
