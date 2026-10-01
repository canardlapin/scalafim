package scalafim.estimates.io

import java.nio.file.{Files, Path}
import scalafim.archive.ContentDigest
import scalafim.estimates.*

/** IO-only consumer with literal expectations; no fit or producer fixture. */
object FirAnalyticReadbackProbe:
  private def checked[A](value: Either[EstimateError, A]): A =
    value.fold(error => throw new IllegalStateException(error.message), identity)

  private def close(actual: Double, expected: Double): Unit =
    require(math.abs(actual - expected) <= 1e-10, s"$actual != $expected")

  def main(args: Array[String]): Unit =
    require(args.length == 1, "relocated-dataset-root")
    Vector("scalafim.fmri.fit.FirstLevelEstimates$", "scalafim.fmri.fit.SelectedEstimates$",
      "scalafim.fmri.fit.estimates.FitEstimateProducer$", "scalafim.fmri.fit.estimates.FirEstimateProducerFixture$",
      "scalafim.fmri.fit.estimates.FirAnalyticProducerProbe$").foreach: name =>
      val missing = try
        Class.forName(name)
        false
      catch case _: ClassNotFoundException => true
      require(missing, s"fitter or producer must be absent: $name")

    val root = Path.of(args(0))
    val declaration = ujson.read(Files.readString(root.resolve("fir-reference.json")))
    val reference = PinnedUnit(UnitId(declaration("Unit").str), UnitRevisionId(declaration("Revision").str),
      FileReference(declaration("Path").str, ContentDigest.unsafeSha256(declaration("SHA256").str), declaration("Bytes").num.toLong))
    val source = checked(LocalEstimateStore.open(root).flatMap(_.open(reference, ReadLimits(4))))
    try
      val unit = source.unit
      val ids = Vector(EstimandId("fir-bin-1"), EstimandId("fir-bin-2"))
      val observation = ObservationId("subject-01")
      val residualDf = DegreesOfFreedom(DfRole.Residual, DfValue.Scalar(6.0), "OLS n - numerical rank", false)
      require(unit.dataset == DatasetId("00000000-0000-4000-8000-000000000701"))
      require(unit.unit == UnitId("00000000-0000-4000-8000-000000000702"))
      require(unit.revision == UnitRevisionId("00000000-0000-4000-8000-000000000703"))
      require(unit.catalog.model == ModelRevisionId("00000000-0000-4000-8000-000000000704"))
      require(unit.catalog.entries.map(_.id) == ids)
      require(unit.catalog.entries.map(_.response) == Vector(
        ResponseCoordinate.FirInterval("event onset", 0.0, 2.0), ResponseCoordinate.FirInterval("event onset", 2.0, 4.0)))
      require(unit.catalog.entries.forall(entry => entry.kind == EstimandKind.Coefficient && entry.units == "signal" && entry.normalization == "unit height"))
      require(unit.domain.dimensions == Vector(2, 1, 1) && unit.domain.support == Vector(0, 1) && unit.domain.worldFrame == "scanner")
      require(unit.observations == Vector(Observation(observation,
        ParticipantId(unit.dataset, "01"), Vector(AcquisitionId("run-1")))))
      require(unit.bindings.map(_.estimand) == ids)
      require(unit.bindings.map(_.weights) == Vector(Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0)))
      require(unit.bindings.forall(binding => binding.rows == 1 && binding.columnIds.size == 3 && binding.columnIds == unit.bindings.head.columnIds))
      require(unit.degreesOfFreedom == Vector(residualDf))
      require(unit.provenance.scans == Vector(AcquisitionScans(AcquisitionId("run-1"), Vector.range(0, 9))))
      require(unit.provenance.estimator == ScientificFact.Known("shared ordinary least squares; compiled rank-revealing QR selected readout"))
      require(unit.provenance.noise == ScientificFact.Known("independent homoscedastic errors under the fitted model"))
      require(unit.provenance.runCombination == ScientificFact.Known("one shared fit over the declared selected rows"))
      require(unit.covariance.isEmpty)
      require(unit.products.map(_.kind) == Vector(ProductKind.Effect, ProductKind.StandardError, ProductKind.ResidualVariance))
      require(unit.products.forall(product => product.precision == NumericPrecision.Float64 && product.pooling == PoolingScope.Run &&
        product.observations == Vector(observation) && product.targets == ProductTargets.Scalar(ids)))
      val effect = unit.products.find(_.kind == ProductKind.Effect).get
      val standardError = unit.products.find(_.kind == ProductKind.StandardError).get
      require(unit.marginalUncertainty == Vector(MarginalUncertaintyDescriptor(standardError.id, effect.id, MarginalVarianceOrigin.Estimated(residualDf))))

      val values = new Array[Double](4)
      val validity = new Array[Byte](4)
      val expected = Map(
        ProductKind.Effect -> Vector(0.0, 5.0, -3.0, 2.0),
        ProductKind.StandardError -> Vector(14.0 / 3.0, 7.0 / 6.0, 8.0 / 3.0, 2.0 / 3.0).map(math.sqrt),
        ProductKind.ResidualVariance -> Vector(4.0, 1.0, 4.0, 1.0))
      unit.products.foreach: product =>
        checked(source.read(product.id, EstimateSelection(Vector(observation), ids.reverse, Vector(1, 0)), values, validity))
        values.zip(expected(product.kind)).foreach((actual, wanted) => close(actual, wanted))
        require(validity.toVector == Vector.fill(4)(Validity.Valid.code))
      println("FIR_READER_PASS fitterPresent=false producerPresent=false binsReversed=true samplesReversed=true scalarCells=12 residualDf=6 measuredZeroValid=true")
    finally checked(source.close())
