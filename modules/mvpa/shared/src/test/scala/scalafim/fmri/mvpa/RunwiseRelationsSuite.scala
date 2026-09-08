package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.Matrix
import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.IndexSpace
import resample4s.core.Injection
import scalafim.fmri.fit.LeastSquaresSeparate
import scalafim.fmri.fit.LssTrialDesign
import scalafim.fmri.fit.ResponseBlock

class RunwiseRelationsSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val effects = right(
    AxisRef.create(
      AxisId.unsafe("readout-effects"),
      AxisPurpose.Effects,
      Vector(AxisKey.unsafe("trial-a"), AxisKey.unsafe("trial-b")),
      CoordinateBasis.unsafe("trial-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("runwise-relations-suite", "v1")
    )
  )

  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("readout-neural"),
      AxisPurpose.NeuralFeatures,
      Vector(FeatureId.unsafe("v1"), FeatureId.unsafe("v2"), FeatureId.unsafe("v3")),
      CoordinateBasis.unsafe("voxel-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("runwise-relations-suite", "v1")
    )
  )

  private val partitions = right(
    AxisRef.create(
      AxisId.unsafe("readout-runs"),
      AxisPurpose.Partitions,
      Vector(PartitionId.unsafe("run-1"), PartitionId.unsafe("run-2")),
      CoordinateBasis.unsafe("run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("runwise-relations-suite", "v1")
    )
  )

  private val namedPartitions = right(
    PartitionAxis(ScientificAxisName.unsafe("runs"), partitions)
  )

  private val fitDesign = right(
    DesignIdentity(
      DesignKind.unsafe("trial-readout"),
      Vector("method" -> "lss")
    )
  )

  private def training(run: Int): AxisRef[SampleId] =
    right(
      AxisRef.create(
        AxisId.unsafe(s"run-$run-timepoints"),
        AxisPurpose.Samples,
        Vector.tabulate(4)(position => SampleId.unsafe(s"run-$run-t${position + 1}")),
        CoordinateBasis.unsafe("time-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("runwise-relations-suite", "v1")
      )
    )

  private def run(
      position: Int,
      zeroSecondTrial: Boolean = false
  ): RunwiseRelationInput =
    val second = if zeroSecondTrial then 0.0 else 1.0
    val trialDesign = fromRows(
      Seq(
        Seq(1.0, 0.0),
        Seq(1.0, 0.0),
        Seq(0.0, second),
        Seq(0.0, second)
      )
    )
    val response =
      if position == 0 then
        fromRows(
          Seq(
            Seq(1.0, 10.0, 2.0),
            Seq(8.0, 80.0, 7.0),
            Seq(3.0, 30.0, 4.0),
            Seq(6.0, 60.0, 5.0)
          )
        )
      else
        fromRows(
          Seq(
            Seq(9.0, 90.0, 8.0),
            Seq(1.5, 15.0, 2.5),
            Seq(7.0, 70.0, 6.0),
            Seq(2.5, 25.0, 3.5)
          )
        )
    val readout = right(
      LeastSquaresSeparate
        .unsafePrepare(
          LssTrialDesign.unsafe(
            trialDesign,
            Vector("trial-a", "trial-b")
          )
        )
        .trialReadout
    )
    right(
      RunwiseRelationInput(
        partitions.keys(position),
        ResponseBlock.unsafe(response),
        readout,
        training(position + 1),
        ValueIdentity.source(ValueId.unsafe(s"response-run-${position + 1}")),
        ValueId.unsafe(s"relation-run-${position + 1}")
      )
    )

  test("runwise readouts bind as relations without applying or materializing trial-pattern volumes"):
    val runs = Vector(run(0), run(1))
    val source = right(
      RunwiseRelations.estimateOnly(
        namedPartitions,
        effects,
        neural,
        runs,
        fitDesign
      )
    )

    assertEquals(source.partitionKeys, partitions.keys)
    assert(source.partitionKeys.forall: partition =>
      right(source.relation(partition)).estimate.representation ==
        multivar.core.OperatorRepresentation.MatrixFree)
    assertEquals(
      right(source.relation(partitions.keys.head)).receipt.sourceRevision.stableKey,
      "response-run-1"
    )

    val injection = right(
      Injection.from(
        IArray.unsafeFromArray(Array(0, 2)),
        right(IndexSpace.of(neural.size))
      )
    )
    val measurement = right(
      Measurement.hardSelection(
        neural,
        MeasurementId.unsafe("readout-local"),
        injection
      )
    )
    val pairing = right(
      PairingDesign.allOrdered(
        namedPartitions,
        PairingReducer.WeightedMean,
        GeneralizationAxis(ScientificAxisName.unsafe("runs"), partitions.identity),
        right(
          PartitionIndependenceTestSupport.declareAllPairs(
            source.identity,
            namedPartitions,
            "runwise-readout-independent-runs"
          )
        )
      )
    )
    val form = right(
      RelationalCompiler.effectForm(
        source,
        pairing,
        measurement,
        right(NeuralQuery.identity(measurement.local))
      )
    )

    val expectedRun1 = selectColumns(
      right(runs(0).readout.forward(runs(0).response)).value,
      Vector(0, 2)
    )
    val expectedRun2 = selectColumns(
      right(runs(1).readout.forward(runs(1).response)).value,
      Vector(0, 2)
    )
    val expected = crossAverage(expectedRun1, expectedRun2)
    assertMatrix(form.value, expected)
    assertEquals(form.receipt.materializedCells, 0L)
    assertEquals(form.receipt.avoidedFullRelationCells, 4L)
    assert(form.receipt.measurementComposedBeforeProjection)

  test("runwise readouts flow through one typed relation fit into an exact RDM and RSA"):
    val fitEffects = right(
      AxisRef.create(
        AxisId.unsafe("runwise-fit-effects"),
        AxisPurpose.Effects,
        Vector("a", "b", "c").map(AxisKey.unsafe),
        CoordinateBasis.unsafe("condition-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("runwise-relations-suite", "v1")
      )
    )
    val fitNeural = right(
      AxisRef.create(
        AxisId.unsafe("runwise-fit-neural"),
        AxisPurpose.NeuralFeatures,
        Vector.tabulate(4)(position => FeatureId.unsafe(s"voxel-${position + 1}")),
        CoordinateBasis.unsafe("voxel-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("runwise-relations-suite", "v1")
      )
    )
    val fitPartitions = right(
      AxisRef.create(
        AxisId.unsafe("runwise-fit-runs"),
        AxisPurpose.Partitions,
        Vector.tabulate(3)(position => PartitionId.unsafe(s"run-${position + 1}")),
        CoordinateBasis.unsafe("run-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("runwise-relations-suite", "v1")
      )
    )
    val fitPartitionAxis = right(
      PartitionAxis(ScientificAxisName.unsafe("runs"), fitPartitions)
    )
    val trialDesign = fromRows(
      Seq(
        Seq(1.0, 0.0, 0.0),
        Seq(1.0, 0.0, 0.0),
        Seq(0.0, 1.0, 0.0),
        Seq(0.0, 1.0, 0.0),
        Seq(0.0, 0.0, 1.0),
        Seq(0.0, 0.0, 1.0)
      )
    )
    val responseRows = Vector(
      Seq(
        Seq(0.0, 0.2, 0.0, 0.1),
        Seq(0.2, 0.0, 0.1, -0.1),
        Seq(1.0, 0.0, 0.5, 0.2),
        Seq(1.2, 0.2, 0.3, 0.0),
        Seq(0.0, 2.0, 0.2, -0.2),
        Seq(-0.2, 1.8, 0.4, 0.0)
      ),
      Seq(
        Seq(0.1, 0.0, 0.0, 0.2),
        Seq(-0.1, 0.2, 0.2, 0.0),
        Seq(1.1, 0.1, 0.4, 0.1),
        Seq(1.3, -0.1, 0.6, -0.1),
        Seq(0.2, 2.1, 0.1, 0.0),
        Seq(0.0, 1.9, 0.3, -0.2)
      ),
      Seq(
        Seq(-0.1, 0.1, 0.1, 0.0),
        Seq(0.1, -0.1, -0.1, 0.2),
        Seq(0.9, 0.2, 0.5, 0.0),
        Seq(1.1, 0.0, 0.5, 0.2),
        Seq(-0.1, 1.9, 0.3, -0.1),
        Seq(0.1, 2.1, 0.1, 0.1)
      )
    )
    val fitRuns = responseRows.zipWithIndex.map: (rows, position) =>
      val readout = right(
        LeastSquaresSeparate
          .unsafePrepare(
            LssTrialDesign.unsafe(trialDesign, Vector("a", "b", "c"))
          )
          .trialReadout
      )
      val training = right(
        AxisRef.create(
          AxisId.unsafe(s"runwise-fit-run-${position + 1}-timepoints"),
          AxisPurpose.Samples,
          Vector.tabulate(6)(timepoint => SampleId.unsafe(s"run-${position + 1}-t${timepoint + 1}")),
          CoordinateBasis.unsafe("time-order"),
          None,
          AxisScale.nominal,
          CoordinateProvenance.unsafe("runwise-relations-suite", "v1")
        )
      )
      right(
        RunwiseRelationInput(
          fitPartitions.keys(position),
          ResponseBlock.unsafe(fromRows(rows)),
          readout,
          training,
          ValueIdentity.source(ValueId.unsafe(s"runwise-fit-response-${position + 1}")),
          ValueId.unsafe(s"runwise-fit-relation-${position + 1}")
        )
      )

    val source = right(
      RunwiseRelations.estimateOnly(
        fitPartitionAxis,
        fitEffects,
        fitNeural,
        fitRuns,
        fitDesign
      )
    )
    val pairing = right(
      PairingDesign.allOrdered(
        fitPartitionAxis,
        PairingReducer.WeightedMean,
        GeneralizationAxis(
          ScientificAxisName.unsafe("runs"),
          fitPartitions.identity
        ),
        right(
          PartitionIndependenceTestSupport.declareAllPairs(
            source.identity,
            fitPartitionAxis,
            "runwise-fit-independent-runs"
          )
        )
      )
    )
    val measurement = right(
      Measurement.identity(
        fitNeural,
        MeasurementId.unsafe("runwise-fit-all-neural-features")
      )
    )
    val domain = right(WithinPairDomain(source.effects))
    val query = right(
      RelationalFitQuery.identityPrecision(
        source,
        domain,
        RdmNormalization.DivideByNeuralDimension
      )
    )
    val relationFit = right(RelationalFit.query(query, pairing, measurement))
    val observed = right(relationFit.rdm)

    // The reference side applies each readout explicitly, then performs the
    // ordered cross-run contrast products without using the relational compiler.
    val explicitRelations = fitRuns.map(run => right(run.readout.forward(run.response)).value)
    val expected = crossvalidatedRdm(explicitRelations, divideByNeuralDimension = true)
    assertEquals(
      observed.definition.domain.pairs.map(pair => pair.first -> pair.second),
      Vector(
        fitEffects.keys(0) -> fitEffects.keys(1),
        fitEffects.keys(0) -> fitEffects.keys(2),
        fitEffects.keys(1) -> fitEffects.keys(2)
      )
    )
    observed.distances.toVector
      .zip(expected)
      .foreach: (actual, reference) =>
        assertEqualsDouble(actual, reference, 1e-12)
    Vector(0.32458333333333333, 0.9120833333333334, 1.22)
      .zip(expected)
      .foreach: (reference, actual) =>
        assertEqualsDouble(actual, reference, 1e-12)

    val modelValues = Vector(2.0, 3.0, 1.0)
    val model = right(
      SecondOrderModel.signal(
        relationFit.definition.domain,
        SecondOrderModelName.unsafe("hypothesis"),
        modelValues
      )
    )
    val rsa = right(RelationalRsa.pearson(relationFit, model))
    assertEqualsDouble(rsa.correlation, pearson(expected, modelValues), 1e-12)
    assertEquals(rsa.fit, relationFit.identity)
    assertEquals(rsa.model, model.identity)
    assertEquals(relationFit.computation.materializedCells, 0L)
    assertEquals(
      relationFit.computation.projectionMode,
      RelationalProjectionMode.OperatorProjection
    )
    assert(
      relationFit.computation.projections.forall(
        _.sourceRepresentation == multivar.core.OperatorRepresentation.MatrixFree
      )
    )
    assert(relationFit.computation.measurementComposedBeforeProjection)

  test("trial estimability is preserved in each fit receipt"):
    val source = right(
      RunwiseRelations.estimateOnly(
        namedPartitions,
        effects,
        neural,
        Vector(
          run(0, zeroSecondTrial = true),
          run(1, zeroSecondTrial = true)
        ),
        fitDesign
      )
    )
    val receipt = right(source.relation(partitions.keys.head)).receipt
    assertEquals(receipt.estimability.flags.toVector, Vector(true, false))
    assertEquals(receipt.estimability.rank, 1)

  test("effect labels and keyed partition coverage fail closed"):
    val foreignEffects = right(
      AxisRef.create(
        AxisId.unsafe("foreign-readout-effects"),
        AxisPurpose.Effects,
        Vector(AxisKey.unsafe("wrong-a"), AxisKey.unsafe("trial-b")),
        CoordinateBasis.unsafe("trial-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("runwise-relations-suite", "v1")
      )
    )
    val mismatch = RunwiseRelations.estimateOnly(
      namedPartitions,
      foreignEffects,
      neural,
      Vector(run(0), run(1)),
      fitDesign
    )
    assert(mismatch.left.exists:
      case FmriEvidenceError.EffectKeyMismatch(partition, 0, "wrong-a", "trial-a") =>
        partition == partitions.keys.head
      case _ => false)

    val missing = RunwiseRelations.estimateOnly(
      namedPartitions,
      effects,
      neural,
      Vector(run(0)),
      fitDesign
    )
    assert(missing.left.exists:
      case FmriEvidenceError.Relation(RelationError.MissingPartition(partition)) =>
        partition == partitions.keys(1)
      case _ => false)

  private def crossAverage(left: DMat, right: DMat): DMat =
    val output = gale.linalg.Matrix.newBuilder(left.rows, left.rows)
    var row = 0
    while row < left.rows do
      var column = 0
      while column < left.rows do
        var forward = 0.0
        var reverse = 0.0
        var feature = 0
        while feature < left.cols do
          forward += left(row, feature) * right(column, feature)
          reverse += right(row, feature) * left(column, feature)
          feature += 1
        output(row, column) = (forward + reverse) / 2.0
        column += 1
      row += 1
    output.result()

  private def crossvalidatedRdm(
      relations: Vector[DMat],
      divideByNeuralDimension: Boolean
  ): Vector[Double] =
    val values = Vector.newBuilder[Double]
    var firstEffect = 0
    while firstEffect < relations.head.rows do
      var secondEffect = firstEffect + 1
      while secondEffect < relations.head.rows do
        var total = 0.0
        var pairCount = 0
        var leftRun = 0
        while leftRun < relations.length do
          var rightRun = 0
          while rightRun < relations.length do
            if leftRun != rightRun then
              var feature = 0
              var product = 0.0
              while feature < relations.head.cols do
                val leftDifference =
                  relations(leftRun)(firstEffect, feature) -
                    relations(leftRun)(secondEffect, feature)
                val rightDifference =
                  relations(rightRun)(firstEffect, feature) -
                    relations(rightRun)(secondEffect, feature)
                product += leftDifference * rightDifference
                feature += 1
              total += product
              pairCount += 1
            rightRun += 1
          leftRun += 1
        val mean = total / pairCount.toDouble
        values +=
          (if divideByNeuralDimension then mean / relations.head.cols.toDouble
           else mean)
        secondEffect += 1
      firstEffect += 1
    values.result()

  private def pearson(left: Vector[Double], right: Vector[Double]): Double =
    require(left.length == right.length && left.nonEmpty)
    val leftMean = left.sum / left.length.toDouble
    val rightMean = right.sum / right.length.toDouble
    var numerator = 0.0
    var leftSquares = 0.0
    var rightSquares = 0.0
    var position = 0
    while position < left.length do
      val leftCentered = left(position) - leftMean
      val rightCentered = right(position) - rightMean
      numerator += leftCentered * rightCentered
      leftSquares += leftCentered * leftCentered
      rightSquares += rightCentered * rightCentered
      position += 1
    numerator / math.sqrt(leftSquares * rightSquares)

  private def selectColumns(value: DMat, columns: Vector[Int]): DMat =
    val output = Matrix.newBuilder(value.rows, columns.length)
    var row = 0
    while row < value.rows do
      var column = 0
      while column < columns.length do
        output(row, column) = value(row, columns(column))
        column += 1
      row += 1
    output.result()

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    val output = Matrix.newBuilder(rows.length, rows.head.length)
    var row = 0
    while row < rows.length do
      var column = 0
      while column < rows.head.length do
        output(row, column) = rows(row)(column)
        column += 1
      row += 1
    output.result()

  private def assertMatrix(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertEqualsDouble(actual(row, column), expected(row, column), 1e-12)
        column += 1
      row += 1
