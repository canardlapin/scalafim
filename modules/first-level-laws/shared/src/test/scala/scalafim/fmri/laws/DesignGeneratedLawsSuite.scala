package scalafim.fmri.laws

import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import scalafim.fmri.design.*
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.linalg.Mat

class DesignGeneratedLawsSuite extends GeneratedLawSuite:

  property("generated explicit and mixed acquisition grids preserve run-local and global axes"):
    forAll(FirstLevelGenerators.acquisitionCase) { generated =>
      val frame = generated.samplingFrame
      val global = frame.acquisitionOnsets()
      val expectedBlocks = generated.blockLengths.zipWithIndex.flatMap { case (length, block) =>
        Vector.fill(length)(block)
      }
      val runCountsAgree = frame.blockIdsPerSample == expectedBlocks
      val sizeAgrees = global.length == generated.timepoints
      val strictlyIncreasing = global.zip(global.tail).forall { case (left, right) => right.value > left.value }
      val localStartsAgree = generated.blockLengths.indices.forall { block =>
        frame.samples(Seq(block), global = false).head.value == generated.startTimes(block)
      }
      Prop(runCountsAgree && sizeAgrees && strictlyIncreasing && localStartsAgree) :|
        s"blocks=${generated.blockLengths} tr=${generated.repetitionTimes} mixed=${generated.mixed} " +
        s"global=${global.map(_.value)}"
    }

  property("within-run event-row permutation preserves structural origins and the compiled numerical design"):
    forAll(FirstLevelGenerators.eventTableCase) { generated =>
      val order = generated.blockIds.distinct.flatMap { block =>
        generated.blockIds.indices.filter(generated.blockIds(_) == block).reverse
      }.toVector
      val result =
        for
          reference <- build(generated, ForwardFormula)
          permuted <- build(generated.permuted(order), ForwardFormula)
        yield (reference, permuted)
      result match
        case Left(error)                  => Prop.falsified :| error.message
        case Right((reference, permuted)) =>
          val structural = reference.designSchema.columns.map(_.origin) == permuted.designSchema.columns.map(_.origin)
          val evidence = matrixEvidence(reference.designMatrix, NumericalOperation.LinearCombination)
          val gap = maxAbsDiff(reference.designMatrix, permuted.designMatrix)
          val referenceParents = canonicalParents(reference)
          val permutedParents = canonicalParents(permuted)
          Prop(structural && referenceParents == permutedParents && gap <= evidence.absolute) :|
            s"structural=$structural parents=${referenceParents == permutedParents} gap=$gap " +
            s"bound=${evidence.absolute} order=$order"
    }

  property("term permutation is column-equivariant when aligned by structural origin"):
    forAll(FirstLevelGenerators.eventTableCase) { generated =>
      val result =
        for
          forward <- build(generated, ForwardFormula)
          reversed <- build(generated, ReversedFormula)
        yield (forward, reversed)
      result match
        case Left(error)                => Prop.falsified :| error.message
        case Right((forward, reversed)) =>
          val aligned = alignByOrigin(forward, reversed)
          aligned match
            case Left(detail)         => Prop.falsified :| detail
            case Right((left, right)) =>
              val evidence = matrixEvidence(left, NumericalOperation.LinearCombination)
              val gap = maxAbsDiff(left, right)
              val orderActuallyChanged =
                forward.designSchema.columns.map(_.origin) != reversed.designSchema.columns.map(_.origin)
              Prop(gap <= evidence.absolute) :|
                s"orderChanged=$orderActuallyChanged gap=$gap bound=${evidence.absolute}"
    }

  property("factor relabeling transports cell identities while leaving corresponding columns unchanged"):
    forAll(FirstLevelGenerators.eventTableCase) { generated =>
      val renamedConditions = generated.conditions.map {
        case "A"   => "alpha"
        case "B"   => "beta"
        case other => other
      }
      val renamedTable = tableWith(
        generated,
        onsets = generated.onsets,
        conditions = renamedConditions,
        probeOnsets = generated.probeOnsets
      )
      val renamedLevels = FactorLevelRegistry.of("condition" -> Vector("alpha", "beta"))
      val result =
        for
          levels <- renamedLevels
          original <- build(generated, ForwardFormula)
          renamed <- buildTable(generated, renamedTable, levels, ForwardFormula)
        yield (original, renamed)
      result match
        case Left(error)                => Prop.falsified :| error.message
        case Right((original, renamed)) =>
          val shapeAgrees =
            original.designMatrix.rows == renamed.designMatrix.rows &&
              original.designMatrix.cols == renamed.designMatrix.cols
          val evidence = matrixEvidence(original.designMatrix, NumericalOperation.LinearCombination)
          val gap = maxAbsDiff(original.designMatrix, renamed.designMatrix)
          val identitiesMoved =
            original.designSchema.columns.map(_.origin) != renamed.designSchema.columns.map(_.origin)
          Prop(shapeAgrees && identitiesMoved && gap <= evidence.absolute) :|
            s"shape=$shapeAgrees identitiesMoved=$identitiesMoved gap=$gap bound=${evidence.absolute}"
    }

  property("scientifically relevant timing changes alter the design fingerprint"):
    forAll(FirstLevelGenerators.eventTableCase) { generated =>
      val perturbedOnsets = generated.onsets.updated(0, generated.onsets.head + 0.25)
      val perturbedTable = tableWith(
        generated,
        onsets = perturbedOnsets,
        conditions = generated.conditions,
        probeOnsets = generated.probeOnsets
      )
      val result =
        for
          reference <- build(generated, ForwardFormula)
          perturbed <- buildTable(generated, perturbedTable, generated.factorLevels, ForwardFormula)
        yield (reference, perturbed)
      result match
        case Left(error)                   => Prop.falsified :| error.message
        case Right((reference, perturbed)) =>
          val fingerprintsDiffer = reference.designSchema.fingerprint != perturbed.designSchema.fingerprint
          val numericDifference = maxAbsDiff(reference.designMatrix, perturbed.designMatrix)
          Prop(fingerprintsDiffer && numericDifference > 0.0) :|
            s"reference=${reference.designSchema.fingerprint.value} " +
            s"perturbed=${perturbed.designSchema.fingerprint.value} gap=$numericDifference"
    }

  property("generated factor, missingness, provenance, and policy audits remain internally consistent"):
    forAll(FirstLevelGenerators.eventTableCase) { generated =>
      build(generated, ForwardFormula) match
        case Left(error)  => Prop.falsified :| error.message
        case Right(model) =>
          val schema = model.designSchema
          val expectedMissing = generated.modulators.count(_.isNaN)
          val conditionAudit = schema.audit.factorLevels.find(_.factor.value == "condition")
          val declaredLevels = conditionAudit.flatMap(_.declared).map(_.map(_.value))
          val observedLevels = conditionAudit.map(_.observed.map(_.value).toSet)
          val provenance = schema.audit.eventProvenance
          val provenanceConsistent =
            provenance.length == generated.onsets.length * 2 &&
              provenance.forall(value => generated.parentIds.contains(value.parent.value)) &&
              provenance.forall(value => value.sourceRow >= 0 && value.sourceRow < generated.onsets.length)
          val missingConsistent =
            schema.audit.missingValues.length == expectedMissing &&
              schema.audit.missingValues.forall(value =>
                value.source.exists(source => generated.parentIds.contains(source.parent.value))
              )
          val receiptsConsistent =
            schema.audit.centeringReceipts.forall(_.modulator.value == "rt") &&
              model.policyReceipts.forall(schema.audit.policyReceipts.contains) &&
              model.centeringReceipts.forall(schema.audit.centeringReceipts.contains)
          val valid = model.designSchemaValidation.isRight && schema.validate.isRight
          Prop(
            valid &&
              declaredLevels.contains(Vector("A", "B")) &&
              observedLevels.contains(Set("A", "B")) &&
              provenanceConsistent &&
              missingConsistent &&
              receiptsConsistent
          ) :| s"valid=$valid declared=$declaredLevels observed=$observedLevels " +
            s"provenance=${provenance.length}/${generated.onsets.length * 2} " +
            s"missing=${schema.audit.missingValues.length}/$expectedMissing receipts=$receiptsConsistent"
    }

  private val ForwardFormula =
    """onset ~
      |  hrf(condition, onsets = onset, durations = duration, basis = spmg1, phase = sample, parent = trial_id, id = sample) +
      |  hrf(condition, center_within(rt, condition), onsets = probe_onset, durations = duration, basis = spmg2, phase = probe, parent = trial_id, id = probe)""".stripMargin

  private val ReversedFormula =
    """onset ~
      |  hrf(condition, center_within(rt, condition), onsets = probe_onset, durations = duration, basis = spmg2, phase = probe, parent = trial_id, id = probe) +
      |  hrf(condition, onsets = onset, durations = duration, basis = spmg1, phase = sample, parent = trial_id, id = sample)""".stripMargin

  private def build(generated: EventTableCase, formula: String): Either[DesignError, EventModel] =
    buildTable(generated, generated.table, generated.factorLevels, formula)

  private def buildTable(
      generated: EventTableCase,
      table: DataTable,
      factorLevels: FactorLevelRegistry,
      formula: String
  ): Either[DesignError, EventModel] =
    EventModelBuilder.buildEither(
      formula = formula,
      data = table,
      samplingFrame = generated.acquisition.samplingFrame,
      blockIds = generated.blockIds,
      precision = Seconds(generated.acquisition.precision),
      factorLevels = factorLevels,
      missingValuePolicy = generated.missingPolicy
    )

  private def tableWith(
      generated: EventTableCase,
      onsets: Vector[Double],
      conditions: Vector[String],
      probeOnsets: Vector[Double]
  ): DataTable =
    DataTable.fromColumns(
      "onset" -> Column.Doubles(onsets),
      "probe_onset" -> Column.Doubles(probeOnsets),
      "duration" -> Column.Doubles(generated.durations),
      "condition" -> Column.Strings(conditions),
      "rt" -> Column.Doubles(generated.modulators),
      "trial_id" -> Column.Strings(generated.parentIds)
    )

  private def canonicalParents(model: EventModel): Vector[(String, String, Double, Double)] =
    model.designSchema.audit.eventProvenance
      .map(value =>
        (
          value.phase.fold("")(_.value),
          value.parent.value,
          value.onset.value,
          value.duration.value
        )
      )
      .sortBy(value => (value._1, value._2))

  private def alignByOrigin(left: EventModel, right: EventModel): Either[String, (Mat, Mat)] =
    val leftOrigins = left.designSchema.columns.map(_.origin.canonical)
    val rightOrigins = right.designSchema.columns.map(_.origin.canonical)
    if leftOrigins.toSet != rightOrigins.toSet || leftOrigins.distinct.length != leftOrigins.length then
      Left(s"structural origins do not form the same unique set: left=$leftOrigins right=$rightOrigins")
    else
      val rightIndices = rightOrigins.zipWithIndex.toMap
      val alignedData = new Array[Double](right.designMatrix.rows * leftOrigins.length)
      var row = 0
      while row < right.designMatrix.rows do
        var column = 0
        while column < leftOrigins.length do
          alignedData(row * leftOrigins.length + column) = right.designMatrix(row, rightIndices(leftOrigins(column)))
          column += 1
        row += 1
      Right((left.designMatrix, Mat.unsafe(right.designMatrix.rows, leftOrigins.length, alignedData)))

  private def matrixEvidence(matrix: Mat, operation: NumericalOperation): NumericalEvidence =
    NumericalEvidence(
      operation,
      scale = matrix.data.iterator.map(math.abs).maxOption.getOrElse(1.0),
      conditionEstimate = math.max(1.0, matrix.cols.toDouble),
      primitiveOperations = math.max(1, matrix.rows * matrix.cols)
    )

  private def maxAbsDiff(left: Mat, right: Mat): Double =
    if left.rows != right.rows || left.cols != right.cols then Double.PositiveInfinity
    else left.data.iterator.zip(right.data.iterator).map((a, b) => math.abs(a - b)).maxOption.getOrElse(0.0)
