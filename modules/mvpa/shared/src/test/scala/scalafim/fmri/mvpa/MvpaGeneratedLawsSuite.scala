package scalafim.fmri.mvpa

import alder.tune.PositiveInt
import downstream.predictive.ExternalMean
import downstream.predictive.ExternalMeanError
import downstream.predictive.ExternalMeanFit
import downstream.predictive.ExternalMeanPrediction
import gale.linalg.DMat
import multivar.core.OperatorRepresentation
import multivar.core.ValueId
import multivar.core.ValueIdentity
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalacheck.Shrink
import org.scalacheck.rng.Seed
import resample4s.core.Draw
import resample4s.core.IndexSpace
import resample4s.core.Injection
import resample4s.core.Selection
import scalafim.fmri.mvpa.predictive.CategoricalLearnerCompiler
import scalafim.fmri.mvpa.predictive.CategoricalLearnerDefinition
import scalafim.fmri.mvpa.predictive.ClassAxis
import scalafim.fmri.mvpa.predictive.ClassId
import scalafim.fmri.mvpa.predictive.DecisionScore

class MvpaGeneratedLawsSuite extends MvpaGeneratedLawSuite:
  import MvpaLawGenerators.given

  property("complete axis identity and permutation retain semantic keys without admitting foreign owners"):
    forAll(MvpaLawGenerators.reindexCase): generated =>
      val axis = MvpaLawFixtures.sampleAxis(generated.size, generated.salt)
      val foreign = MvpaLawFixtures.sampleAxis(
        generated.size,
        generated.salt,
        basisKind = "foreign-trial-table"
      )
      val relation = axis
        .reorder(generated.permutation)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val sourceValues = Vector.tabulate(generated.size)(position => 101 + position * 17)
      val column = Column(axis, sourceValues)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val reindexed = column
        .reindex(relation)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val expectedKeys = generated.permutation.map(axis.keys)
      val expectedValues = generated.permutation.map(sourceValues)
      val numerical = relation.leg
        .apply(MvpaLawFixtures.column(sourceValues))
        .fold(error => throw new IllegalStateException(error.message), identity)
      val decodedForeign = Column.decode(
        axis,
        foreign.identity.toRecord,
        MvpaLawFixtures.indices(sourceValues)
      )

      all(
        check(relation.parentIdentity == axis.identity, "parent identity changed"),
        check(relation.parentEvidence eq axis.evidence, "parent nominal witness changed"),
        check(relation.reindexing.toVector == generated.permutation, "permutation mapping changed"),
        check(relation.child.keys == expectedKeys, "semantic key order changed"),
        check(
          relation.child.identity.orderedKeys ==
            expectedKeys.map(summon[AxisKeyCodec[SampleId]].encode),
          "identity key order changed"
        ),
        check(
          relation.child.identity.fingerprint != axis.identity.fingerprint,
          "derived permutation reused its parent identity"
        ),
        check(reindexed.toVector == expectedValues, "column values did not follow semantic keys"),
        check(
          MvpaLawFixtures.sameMatrix(numerical, MvpaLawFixtures.column(expectedValues)),
          "typed linear leg disagreed with the ordinal oracle"
        ),
        check(
          axis.bind(axis.identity.toRecord).exists(_ eq axis.evidence),
          "canonical record did not bind to the existing witness"
        ),
        check(axis.bind(foreign.identity).isLeft, "equal-size foreign identity was accepted"),
        check(decodedForeign.isLeft, "column decoder accepted a foreign owner")
      )

  property("selection, injection, draw, and nested reorder preserve kind, order, and occurrence identity"):
    forAll(MvpaLawGenerators.reindexCase): generated =>
      val axis = MvpaLawFixtures.sampleAxis(generated.size, generated.salt)
      val space = IndexSpace
        .of(axis.size)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val selection = Selection
        .from(MvpaLawFixtures.indices(generated.selection), space)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val injection = Injection
        .from(MvpaLawFixtures.indices(generated.injection), space)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val draw = Draw
        .from(MvpaLawFixtures.indices(generated.draw), space)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val selected = ReindexingLeg
        .selection(axis, selection)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val selectedAsInjection = ReindexingLeg
        .injection(axis, selection.widen)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val injected = ReindexingLeg
        .injection(axis, injection)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val drawn = ReindexingLeg
        .draw(axis, draw)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val values = Vector.tabulate(axis.size)(position => generated.salt + position * 13)
      val column = Column(axis, values)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val selectedValues = column
        .reindex(selected)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val injectedValues = column
        .reindex(injected)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val drawnValues = column
        .reindex(drawn)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val secondMapping = injected.child.keys.indices.reverse.toVector
      val second = injected.child
        .reorder(secondMapping)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val nested = injectedValues
        .reindex(second)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val nestedPositions = secondMapping.map(generated.injection)
      val expectedOccurrences = drawOccurrences(generated.draw)
      val actualOccurrences = drawn.child.keys.map: occurrence =>
        (
          occurrence.sourcePosition,
          occurrence.occurrence,
          occurrence.drawPosition
        )
      val emptyDraw = Draw
        .from(MvpaLawFixtures.indices(Vector.empty), space)
        .fold(error => throw new IllegalStateException(error.message), identity)

      all(
        check(
          selected.child.keys == generated.selection.map(axis.keys),
          "selection changed key order"
        ),
        check(
          selectedValues.toVector == generated.selection.map(values),
          "selection changed values"
        ),
        check(
          injected.child.keys == generated.injection.map(axis.keys),
          "injection changed key order"
        ),
        check(
          injectedValues.toVector == generated.injection.map(values),
          "injection changed values"
        ),
        check(drawnValues.toVector == generated.draw.map(values), "draw changed values"),
        check(actualOccurrences == expectedOccurrences, "draw occurrence identity changed"),
        check(
          drawn.child.identity.orderedKeys.distinct.length == generated.draw.length,
          "draw occurrences did not remain unique"
        ),
        check(
          selected.child.keys == selectedAsInjection.child.keys,
          "same extensional mapping changed its keys"
        ),
        check(
          selected.child.identity.fingerprint != selectedAsInjection.child.identity.fingerprint,
          "selection and injection kinds collapsed to one identity"
        ),
        check(
          nested.toVector == nestedPositions.map(values),
          "nested reindexing changed composition order"
        ),
        check(
          ReindexingLeg
            .selection(axis, Selection.empty(space))
            .left
            .exists:
              case AxisRefError.InvalidIdentity(AxisIdentityError.EmptyAxis) => true
              case _                                                         => false
          ,
          "empty selection did not fail at the non-empty scientific-axis boundary"
        ),
        check(
          ReindexingLeg
            .draw(axis, emptyDraw)
            .left
            .exists:
              case AxisRefError.InvalidIdentity(AxisIdentityError.EmptyAxis) => true
              case _                                                         => false
          ,
          "empty draw did not fail at the non-empty scientific-axis boundary"
        )
      )

  property("dense and matrix-free evidence obey the same row, adjoint, measurement, and materialization laws"):
    forAll(MvpaLawGenerators.evidenceCase): generated =>
      val rows = MvpaLawFixtures.sampleAxis(generated.rowCount, generated.salt)
      val columns = MvpaLawFixtures.featureAxis(generated.columnCount, generated.salt)
      val matrix = MvpaLawFixtures.matrix(generated)
      val probe = MvpaLawFixtures.MatrixFree(matrix)
      val dense = EvidenceTable
        .dense(rows, columns, matrix, admitted(ValueId("generated-dense-evidence")))
        .fold(error => throw new IllegalStateException(error.message), identity)
      val matrixFree = EvidenceTable
        .operator(rows, columns, probe, admitted(ValueId("generated-operator-evidence")))
        .fold(error => throw new IllegalStateException(error.message), identity)
      val rightWeights = MvpaLawFixtures.column(generated.rightWeights)
      val rowScores = MvpaLawFixtures.column(generated.rowScores)
      val expectedRight = MvpaLawFixtures.rightMultiply(matrix, rightWeights)
      val expectedTranspose = MvpaLawFixtures.transposeMultiply(matrix, rowScores)
      val denseRight = dense
        .rightMultiply(rightWeights)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val operatorRight = matrixFree
        .rightMultiply(rightWeights)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val denseTranspose = dense
        .transposeMultiply(rowScores)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val operatorTranspose = matrixFree
        .transposeMultiply(rowScores)
        .fold(error => throw new IllegalStateException(error.message), identity)

      val rowSpace = IndexSpace
        .of(rows.size)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val selection = Selection
        .from(MvpaLawFixtures.indices(generated.rowSelection), rowSpace)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val selectionLeg = ReindexingLeg
        .selection(rows, selection)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val draw = Draw
        .from(MvpaLawFixtures.indices(generated.rowDraw), rowSpace)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val drawLeg = ReindexingLeg
        .draw(rows, draw)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val selectedExpected = MvpaLawFixtures.rightMultiply(
        MvpaLawFixtures.selectRows(matrix, generated.rowSelection),
        rightWeights
      )
      val drawnExpected = MvpaLawFixtures.rightMultiply(
        MvpaLawFixtures.selectRows(matrix, generated.rowDraw),
        rightWeights
      )
      val denseSelected = dense
        .restrictRows(selectionLeg)
        .flatMap(_.rightMultiply(rightWeights))
        .fold(error => throw new IllegalStateException(error.message), identity)
      val operatorSelected = matrixFree
        .restrictRows(selectionLeg)
        .flatMap(_.rightMultiply(rightWeights))
        .fold(error => throw new IllegalStateException(error.message), identity)
      val denseDrawn = dense
        .restrictRows(drawLeg)
        .flatMap(_.rightMultiply(rightWeights))
        .fold(error => throw new IllegalStateException(error.message), identity)
      val operatorDrawn = matrixFree
        .restrictRows(drawLeg)
        .flatMap(_.rightMultiply(rightWeights))
        .fold(error => throw new IllegalStateException(error.message), identity)

      val support = generated.supportPositions
        .zip(generated.supportWeights)
        .map:
          case (position, weight) => columns.keys(position) -> weight.toDouble
      val weighted = Measurement
        .weightedRegion(
          columns,
          admitted(MeasurementId("generated-weighted-region")),
          support.reverse
        )
        .fold(error => throw new IllegalStateException(error.message), identity)
      val weightedCanonical = Measurement
        .weightedRegion(
          columns,
          admitted(MeasurementId("generated-weighted-region")),
          support
        )
        .fold(error => throw new IllegalStateException(error.message), identity)
      val weightedProjection = Vector.tabulate(columns.size): position =>
        support
          .collectFirst:
            case (key, weight) if key == columns.keys(position) => weight
          .getOrElse(0.0)
      val weightedExpected = MvpaLawFixtures.measure(
        matrix,
        GaleTestMatrix.fromRows(Vector(weightedProjection))
      )
      val denseWeighted = dense
        .measureColumns(weighted)
        .flatMap(_.rightMultiply(DMat.eye(1)))
        .fold(error => throw new IllegalStateException(error.message), identity)
      val operatorWeighted = matrixFree
        .measureColumns(weighted)
        .flatMap(_.rightMultiply(DMat.eye(1)))
        .fold(error => throw new IllegalStateException(error.message), identity)

      val local = MvpaLawFixtures.featureAxis(
        generated.localCount,
        generated.salt + 37,
        AxisPurpose.Components
      )
      val projection = MvpaLawFixtures.projection(generated)
      val fixed = Measurement
        .fixedProjection(
          columns,
          local,
          admitted(MeasurementId("generated-fixed-projection")),
          projection
        )
        .fold(error => throw new IllegalStateException(error.message), identity)
      val projectionExpected = MvpaLawFixtures.measure(matrix, projection)
      val denseProjected = dense
        .measureColumns(fixed)
        .flatMap(_.rightMultiply(DMat.eye(local.size)))
        .fold(error => throw new IllegalStateException(error.message), identity)
      val operatorProjected = matrixFree
        .measureColumns(fixed)
        .flatMap(_.rightMultiply(DMat.eye(local.size)))
        .fold(error => throw new IllegalStateException(error.message), identity)
      val budget = admitted(
        MaterializationBudget(
          generated.rowCount.toLong * generated.columnCount.toLong
        )
      )
      val denseMaterialized = dense
        .materialize(MaterializationPolicy.Allow(budget))
        .fold(error => throw new IllegalStateException(error.message), identity)
      val operatorMaterialized = matrixFree
        .materialize(MaterializationPolicy.Allow(budget))
        .fold(error => throw new IllegalStateException(error.message), identity)
      val nonFiniteWeights = MvpaLawFixtures.fromFlat(
        generated.columnCount,
        1,
        Vector.tabulate(generated.columnCount): position =>
          if position == 0 then Double.NaN else 1.0
      )
      val denseNonFiniteFailure = dense.rightMultiply(nonFiniteWeights)
      val operatorNonFiniteFailure = matrixFree.rightMultiply(nonFiniteWeights)
      val denseRejected = dense.materialize(MaterializationPolicy.Reject)
      val operatorRejected = matrixFree.materialize(MaterializationPolicy.Reject)
      val requiredElements = generated.rowCount.toLong * generated.columnCount.toLong
      val undersizedFailure =
        if requiredElements == 1L then true
        else
          val undersized = admitted(MaterializationBudget(requiredElements - 1L))
          matrixFree
            .materialize(MaterializationPolicy.Allow(undersized))
            .left
            .exists:
              case EvidenceTableError.MaterializationBudgetExceeded(required, budget) =>
                required == requiredElements && budget.maxElements == requiredElements - 1L
              case _ => false

      all(
        check(
          dense.representation != OperatorRepresentation.MatrixFree,
          "dense backend misreported representation"
        ),
        check(
          matrixFree.representation == OperatorRepresentation.MatrixFree,
          "operator backend misreported representation"
        ),
        check(MvpaLawFixtures.sameMatrix(denseRight, expectedRight), "dense right action failed oracle"),
        check(
          MvpaLawFixtures.sameMatrix(operatorRight, expectedRight),
          "operator right action failed oracle"
        ),
        check(
          MvpaLawFixtures.sameMatrix(denseTranspose, expectedTranspose),
          "dense adjoint failed oracle"
        ),
        check(
          MvpaLawFixtures.sameMatrix(operatorTranspose, expectedTranspose),
          "operator adjoint failed oracle"
        ),
        check(
          MvpaLawFixtures.sameMatrix(denseSelected, selectedExpected),
          "dense selection failed oracle"
        ),
        check(
          MvpaLawFixtures.sameMatrix(operatorSelected, selectedExpected),
          "operator selection failed oracle"
        ),
        check(MvpaLawFixtures.sameMatrix(denseDrawn, drawnExpected), "dense draw failed oracle"),
        check(
          MvpaLawFixtures.sameMatrix(operatorDrawn, drawnExpected),
          "operator draw failed oracle"
        ),
        check(
          weighted.identity.fingerprint == weightedCanonical.identity.fingerprint,
          "weighted support input order changed scientific identity"
        ),
        check(
          MvpaLawFixtures.sameMatrix(denseWeighted, weightedExpected),
          "dense weighted leg failed oracle"
        ),
        check(
          MvpaLawFixtures.sameMatrix(operatorWeighted, weightedExpected),
          "operator weighted leg failed oracle"
        ),
        check(
          MvpaLawFixtures.sameMatrix(denseProjected, projectionExpected),
          "dense fixed projection failed oracle"
        ),
        check(
          MvpaLawFixtures.sameMatrix(operatorProjected, projectionExpected),
          "operator fixed projection failed oracle"
        ),
        check(
          MvpaLawFixtures.sameMatrix(denseMaterialized.value, matrix),
          "dense materialization changed values"
        ),
        check(
          MvpaLawFixtures.sameMatrix(operatorMaterialized.value, matrix),
          "operator materialization changed values"
        ),
        check(
          denseMaterialized.receipt.rowIdentity == rows.identity.fingerprint,
          "dense receipt lost row identity"
        ),
        check(
          operatorMaterialized.receipt.columnIdentity == columns.identity.fingerprint,
          "operator receipt lost column identity"
        ),
        check(
          denseNonFiniteFailure.left.exists:
            case EvidenceTableError.NonFiniteValue("evidence weights", 0, value) => value.isNaN
            case _                                                               => false
          ,
          "dense evidence admitted non-finite right weights"
        ),
        check(
          operatorNonFiniteFailure.left.exists:
            case EvidenceTableError.NonFiniteValue("evidence weights", 0, value) => value.isNaN
            case _                                                               => false
          ,
          "operator evidence admitted non-finite right weights"
        ),
        check(
          denseRejected.left.exists(_ == EvidenceTableError.MaterializationRejected),
          "dense evidence bypassed explicit materialization rejection"
        ),
        check(
          operatorRejected.left.exists(_ == EvidenceTableError.MaterializationRejected),
          "operator evidence bypassed explicit materialization rejection"
        ),
        check(undersizedFailure, "operator evidence bypassed its materialization budget"),
        check(probe.forwardCalls > 0, "matrix-free forward capability was never exercised"),
        check(probe.transposeCalls > 0, "matrix-free adjoint capability was never exercised")
      )

  property("relation capabilities certify degenerate, rank-deficient, ill-conditioned, and hostile residual domains"):
    forAll(MvpaLawGenerators.residualCapabilityCase): generated =>
      val neural = MvpaLawFixtures.featureAxis(generated.size, generated.salt)
      val foreign = MvpaLawFixtures.featureAxis(
        generated.size,
        generated.salt,
        basisKind = "foreign-neural-coordinates"
      )
      val rankDeficientDiagonal = Vector.tabulate(generated.size): position =>
        if position < generated.deficientRank then position.toDouble + 1.0
        else 0.0
      val rankDeficient = MvpaLawFixtures.diagonal(rankDeficientDiagonal)
      val illConditionedDiagonal = Vector.tabulate(generated.size): position =>
        val fraction = position.toDouble / (generated.size - 1).toDouble
        math.pow(10.0, -generated.conditionExponent.toDouble * fraction)
      val illConditioned = MvpaLawFixtures.diagonal(illConditionedDiagonal)
      val degenerate = MvpaLawFixtures.diagonal(Vector.fill(generated.size)(0.0))
      val asymmetric = GaleTestMatrix.fromRows(
        Vector.tabulate(generated.size): row =>
          Vector.tabulate(generated.size): column =>
            if row == 0 && column == 1 then 0.5
            else if row == 1 && column == 0 then 0.0
            else if row == column then 1.0
            else 0.0
      )
      val indefinite = MvpaLawFixtures.diagonal(
        Vector.tabulate(generated.size): position =>
          if position == 0 then -1.0 else 1.0
      )
      val nonFinite = MvpaLawFixtures.diagonal(
        Vector.tabulate(generated.size): position =>
          if position == 0 then Double.NaN else 1.0
      )

      val denseRank = admitted(
        EvidenceTable.dense(
          neural,
          neural,
          rankDeficient,
          admitted(ValueId(s"generated-rank-deficient-${generated.salt}"))
        )
      )
      val rankProbe = MvpaLawFixtures.MatrixFree(rankDeficient)
      val operatorRank = admitted(
        EvidenceTable.operator(
          neural,
          neural,
          rankProbe,
          admitted(ValueId(s"generated-rank-deficient-operator-${generated.salt}"))
        )
      )
      val denseCertified = admitted(CertifiedResidualMoments(denseRank))
      val operatorCertified = admitted(CertifiedResidualMoments(operatorRank))
      val illConditionedCertified = CertifiedResidualMoments(
        admitted(
          EvidenceTable.dense(
            neural,
            neural,
            illConditioned,
            admitted(ValueId(s"generated-ill-conditioned-${generated.salt}"))
          )
        )
      )
      val degenerateCertified = CertifiedResidualMoments(
        admitted(
          EvidenceTable.dense(
            neural,
            neural,
            degenerate,
            admitted(ValueId(s"generated-degenerate-${generated.salt}"))
          )
        )
      )
      val asymmetricFailure = CertifiedResidualMoments(
        admitted(
          EvidenceTable.dense(
            neural,
            neural,
            asymmetric,
            admitted(ValueId(s"generated-asymmetric-${generated.salt}"))
          )
        )
      )
      val indefiniteFailure = CertifiedResidualMoments(
        admitted(
          EvidenceTable.dense(
            neural,
            neural,
            indefinite,
            admitted(ValueId(s"generated-indefinite-${generated.salt}"))
          )
        )
      )
      val nonFiniteDense = EvidenceTable.dense(
        neural,
        neural,
        nonFinite,
        admitted(ValueId(s"generated-non-finite-dense-${generated.salt}"))
      )
      val nonFiniteOperator = admitted(
        EvidenceTable.operator(
          neural,
          neural,
          MvpaLawFixtures.MatrixFree(nonFinite),
          admitted(ValueId(s"generated-non-finite-operator-${generated.salt}"))
        )
      )
      val nonFiniteOperatorFailure = CertifiedResidualMoments(nonFiniteOperator)
      val capability = admitted(
        ResidualFitCapabilities(
          denseCertified,
          admitted(ResidualDegreesOfFreedom(generated.size.toDouble + 7.0))
        )
      )

      val effectAxis = MvpaLawFixtures.effectAxis(2, generated.salt + 17)
      val training = MvpaLawFixtures.sampleAxis(generated.size + 2, generated.salt + 29)
      val estimate = admitted(
        EvidenceTable.dense(
          effectAxis,
          neural,
          GaleTestMatrix.fromRows(
            Vector.tabulate(effectAxis.size): effect =>
              Vector.tabulate(neural.size): coordinate =>
                (effect + 1).toDouble * (coordinate + 2).toDouble
          ),
          admitted(ValueId(s"generated-relation-estimate-${generated.salt}"))
        )
      )
      val estimability = admitted(
        Estimability(effectAxis, Vector.fill(effectAxis.size)(true))
      )
      val receipt = admitted(
        RelationFitReceipt(
          ValueIdentity.source(
            admitted(ValueId(s"generated-relation-source-${generated.salt}"))
          ),
          admitted(
            DesignIdentity(
              admitted(DesignKind("generated-relation-fit")),
              Vector("case" -> generated.salt.toString)
            )
          ),
          estimability,
          NormalizationIdentity.none,
          training
        )
      )
      val relation = admitted(Relation(estimate, receipt, capability))
      val expectedSmallest = math.pow(10.0, -generated.conditionExponent.toDouble)
      val illConditionedSmallest = illConditionedCertified.toOption
        .map(_.certificate.smallestEigenvalue)
      val illConditionedTolerance = math.max(1e-14, expectedSmallest * 1e-5)

      all(
        check(
          denseCertified.certificate.contentDigest ==
            operatorCertified.certificate.contentDigest,
          "dense and operator residual certificates disagreed on contents"
        ),
        check(
          denseCertified.certificate.smallestEigenvalue >= -1e-10,
          "rank-deficient PSD residual was not certified"
        ),
        check(
          rankDeficientDiagonal.count(_ > 0.0) == generated.deficientRank,
          "generated residual lost its declared deficient rank"
        ),
        check(
          illConditionedSmallest.exists(value => math.abs(value - expectedSmallest) <= illConditionedTolerance),
          "ill-conditioned PSD residual changed its smallest eigenvalue"
        ),
        check(
          degenerateCertified.exists(certificate => math.abs(certificate.certificate.smallestEigenvalue) <= 1e-12),
          "zero residual moments were not represented as a certified degeneracy"
        ),
        check(
          asymmetricFailure.left.exists:
            case RelationError.AsymmetricResidualMoments(0, 1, _, _) => true
            case _                                                   => false
          ,
          "asymmetric residual moments did not fail closed"
        ),
        check(
          indefiniteFailure.left.exists:
            case RelationError.NonPositiveSemidefiniteResidualMoments(value, _) =>
              value < 0.0
            case _ => false
          ,
          "indefinite residual moments did not fail closed"
        ),
        check(
          nonFiniteDense.left.exists:
            case EvidenceTableError.NonFiniteValue("dense evidence", 0, value) =>
              value.isNaN
            case _ => false
          ,
          "dense residual evidence admitted a non-finite coordinate"
        ),
        check(
          nonFiniteOperatorFailure.left.exists:
            case RelationError.Evidence(
                  EvidenceTableError.NonFiniteValue("evidence output", 0, value)
                ) =>
              value.isNaN
            case _ => false
          ,
          "matrix-free residual evidence admitted a non-finite output"
        ),
        check(
          ResidualMomentTolerance(Double.NaN).left.exists:
            case RelationError.InvalidResidualMomentTolerance(value) => value.isNaN
            case _                                                   => false
          ,
          "non-finite residual tolerance was admitted"
        ),
        check(
          relation.capabilities eq capability,
          "relation did not retain the exact certified capability value"
        ),
        check(
          relation.capabilities.neuralAxis.evidence eq neural.evidence,
          "relation capability lost its exact neural witness"
        ),
        check(
          neural.identity.id == foreign.identity.id &&
            neural.identity.orderedKeys == foreign.identity.orderedKeys &&
            neural.identity.fingerprint != foreign.identity.fingerprint,
          "hostile foreign axis did not retain equal shape and apparent id"
        ),
        check(
          neural.bind(foreign.identity).isLeft,
          "equal-shape foreign neural owner was admitted"
        ),
        check(rankProbe.forwardCalls > 0, "operator residual certificate skipped execution")
      )

  property("measurement-frame identity is canonical while rendition order remains descriptive"):
    forAll(MvpaLawGenerators.evidenceCase): generated =>
      val source = MvpaLawFixtures.featureAxis(generated.columnCount, generated.salt)
      val space = IndexSpace
        .of(source.size)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val injection = Injection
        .from(MvpaLawFixtures.indices(generated.supportPositions), space)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val hard = Measurement
        .hardSelection(source, admitted(MeasurementId("zeta-hard-selection")), injection)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val support = generated.supportPositions
        .zip(generated.supportWeights)
        .map:
          case (position, weight) => source.keys(position) -> weight.toDouble
      val weighted = Measurement
        .weightedRegion(
          source,
          admitted(MeasurementId("alpha-weighted-region")),
          support
        )
        .fold(error => throw new IllegalStateException(error.message), identity)
      val forward = MeasurementFrame(source)(
        Vector(
          MeasurementEntry(hard, s"hard-${generated.salt}"),
          MeasurementEntry(weighted, s"weighted-${generated.salt}")
        )
      ).fold(error => throw new IllegalStateException(error.message), identity)
      val reverse = MeasurementFrame(source)(
        Vector(
          MeasurementEntry(weighted, s"other-weighted-${generated.salt}"),
          MeasurementEntry(hard, s"other-hard-${generated.salt}")
        )
      ).fold(error => throw new IllegalStateException(error.message), identity)

      all(
        check(
          forward.entries.map(_.measurement.identity.id.value) ==
            Vector("alpha-weighted-region", "zeta-hard-selection"),
          "frame order was not canonical"
        ),
        check(
          forward.identity == reverse.identity,
          "rendition or insertion order changed frame identity"
        ),
        check(
          forward.identity.source == source.identity.fingerprint,
          "frame lost source identity"
        ),
        check(
          forward.identity.measurements == forward.entries.map(_.measurement.identity.fingerprint),
          "frame identity disagreed with canonical measurement order"
        )
      )

  property("an open downstream learner obeys public prediction and typed-failure laws"):
    forAll(MvpaLawGenerators.learnerCase): generated =>
      val fixture = learnerFixture(generated)
      val configuration = admitted(ExternalMean(PositiveInt.one))
      val lawfulCompiler = ExternalMean.compiler
      val fitted = admitted(
        lawfulCompiler.fit(
          configuration,
          fixture.classes,
          fixture.data,
          fixture.labels
        )
      )
      val lawfulViolations = admitted(
        learnerViolations(lawfulCompiler, configuration, fixture)
      )
      val mutantViolations = admitted(
        learnerViolations(wrongWinnerCompiler(lawfulCompiler), configuration, fixture)
      )
      val demandingConfiguration = admitted(
        PositiveInt
          .create(generated.featureCount + 1)
          .flatMap(ExternalMean.apply)
      )
      val demandingFailure = lawfulCompiler.fit(
        demandingConfiguration,
        fixture.classes,
        fixture.data,
        fixture.labels
      )
      val missingClassFailure = lawfulCompiler.fit(
        configuration,
        fixture.classes,
        fixture.data,
        Vector.fill(fixture.labels.length)(fixture.firstClass)
      )
      val nonFiniteRows = Vector.tabulate(fixture.data.rows): row =>
        Vector.tabulate(fixture.data.cols): column =>
          if row == 0 && column == 0 then Double.PositiveInfinity
          else fixture.data(row, column)
      val nonFiniteFitFailure = lawfulCompiler.fit(
        configuration,
        fixture.classes,
        GaleTestMatrix.fromRows(nonFiniteRows),
        fixture.labels
      )
      val wrongDimensionFailure = lawfulCompiler.predict(
        fitted,
        IArray.unsafeFromArray(Array.fill(generated.featureCount + 1)(0.0))
      )
      val nonFiniteInput = IArray.unsafeFromArray(
        Array.tabulate(generated.featureCount): column =>
          if column == 0 then Double.NaN else 0.0
      )
      val nonFinitePredictionFailure = lawfulCompiler.predict(fitted, nonFiniteInput)

      all(
        check(lawfulViolations.isEmpty, s"lawful downstream learner violated ${lawfulViolations.mkString(",")}"),
        check(
          mutantViolations.contains("winner-score-consistency"),
          "wrong-winner learner implementation survived the public law court"
        ),
        check(
          demandingFailure.left.exists:
            case ExternalMeanError.DimensionMismatch(expected, actual) =>
              expected == generated.featureCount + 1 && actual == generated.featureCount
            case _ => false
          ,
          "learner minimum-feature failure was not preserved"
        ),
        check(
          missingClassFailure.left.exists:
            case ExternalMeanError.MissingClass(label) =>
              label == fixture.secondClass
            case _ => false
          ,
          "learner admitted a training fold missing one class"
        ),
        check(
          nonFiniteFitFailure.left.exists:
            case ExternalMeanError.NonFiniteCoordinate(0, value) =>
              value == Double.PositiveInfinity
            case _ => false
          ,
          "learner admitted non-finite training evidence"
        ),
        check(
          wrongDimensionFailure.left.exists:
            case ExternalMeanError.DimensionMismatch(expected, actual) =>
              expected == generated.featureCount && actual == generated.featureCount + 1
            case _ => false
          ,
          "learner admitted a foreign prediction dimension"
        ),
        check(
          nonFinitePredictionFailure.left.exists:
            case ExternalMeanError.NonFiniteCoordinate(0, value) => value.isNaN
            case _                                               => false
          ,
          "learner admitted a non-finite prediction coordinate"
        ),
        check(
          lawfulCompiler.definition(configuration).minimumFeatures == PositiveInt.one,
          "open learner definition changed its declared feature requirement"
        )
      )

  property("the conformance court kills identity-loss and hidden-reordering mutants"):
    forAll(MvpaLawGenerators.mutationCase): generated =>
      val lawful = ReindexLawCourt.violations(
        generated,
        PublicReindexSubject.observe(generated)
      )
      val identityLoss = ReindexLawCourt.violations(
        generated,
        IdentityDroppingSubject.observe(generated)
      )
      val hiddenReordering = ReindexLawCourt.violations(
        generated,
        HiddenReorderingSubject.observe(generated)
      )

      all(
        check(lawful.isEmpty, s"public implementation violated ${lawful.mkString(",")}"),
        check(identityLoss.contains("identity-loss"), "identity-loss mutant survived"),
        check(hiddenReordering.contains("source-order"), "hidden-reordering mutant survived"),
        check(
          hiddenReordering.exists(_ != "identity-loss"),
          "hidden-reordering mutant was detected only as an unrelated identity fault"
        )
      )

  test("hostile-domain shrinkers retain multiplicity, rank deficiency, conditioning, and class support"):
    val reindex = ReindexLawCase(
      6,
      911,
      Vector(5, 4, 3, 2, 1, 0),
      Vector(0, 2, 5),
      Vector(5, 2, 0),
      Vector(3, 3, 1, 3)
    )
    val evidence = EvidenceLawCase(
      4,
      3,
      919,
      Vector.tabulate(12)(position => position - 6),
      Vector(1, -2, 3),
      Vector(2, -1, 0, 4),
      Vector(0, 2),
      Vector(1, 1, 3),
      Vector(0, 2),
      Vector(1, -2),
      2,
      Vector(1, 0, -1, 2, 0, -2)
    )
    val residual = ResidualCapabilityLawCase(6, 929, 3, 12)
    val learner = LearnerLawCase(5, 6, 8, 937)

    val reindexShrinks = Shrink.shrink(reindex).take(32).toVector
    val evidenceShrinks = Shrink.shrink(evidence).take(32).toVector
    val residualShrinks = Shrink.shrink(residual).take(32).toVector
    val learnerShrinks = Shrink.shrink(learner).take(32).toVector

    assert(reindexShrinks.nonEmpty)
    assert(reindexShrinks.forall(value => value.draw.distinct.length < value.draw.length))
    assert(evidenceShrinks.nonEmpty)
    assert(
      evidenceShrinks.exists(value => value.rowCount < evidence.rowCount || value.columnCount < evidence.columnCount)
    )
    assert(
      evidenceShrinks.forall(value =>
        value.supportPositions.nonEmpty &&
          value.supportWeights.forall(_ != 0) &&
          value.projection.length == value.localCount * value.columnCount
      )
    )
    assert(residualShrinks.nonEmpty)
    assert(
      residualShrinks.forall(value =>
        value.deficientRank > 0 &&
          value.deficientRank < value.size &&
          value.conditionExponent >= 6
      )
    )
    assert(learnerShrinks.nonEmpty)
    assert(
      learnerShrinks.forall(value =>
        value.repetitionsPerClass > 0 &&
          value.featureCount > 0 &&
          value.separation > 0
      )
    )

  test("law profile exposes one reproducible seed and bounded single-worker case budget"):
    assert(Seed.fromBase64(MvpaLawProfile.initialSeed).isSuccess)
    assert(MvpaLawProfile.current.successfulTests > 0)
    assert(MvpaLawProfile.current.maximumSize > 0)
    assertEquals(scalaCheckInitialSeed, MvpaLawProfile.initialSeed)
    assertEquals(scalaCheckTestParameters.workers, 1)
    assertEquals(
      scalaCheckTestParameters.minSuccessfulTests,
      MvpaLawProfile.current.successfulTests
    )
    assertEquals(
      scalaCheckTestParameters.maxSize,
      MvpaLawProfile.current.maximumSize
    )

  private final case class LearnerFixture(
      classes: AxisRef[ClassId],
      firstClass: ClassId,
      secondClass: ClassId,
      data: DMat,
      labels: Vector[ClassId],
      inputs: Vector[IArray[Double]]
  )

  private def learnerFixture(value: LearnerLawCase): LearnerFixture =
    val first = admitted(ClassId(s"generated-class-a-${value.salt}"))
    val second = admitted(ClassId(s"generated-class-b-${value.salt}"))
    val classes = admitted(
      ClassAxis.create(
        admitted(AxisId(s"generated-learner-classes-${value.salt}")),
        Vector(first, second),
        admitted(
          CoordinateProvenance("mvpa-law-generator", s"learner-${value.salt}")
        )
      )
    )
    val labels = Vector.tabulate(value.repetitionsPerClass * 2): row =>
      if row % 2 == 0 then first else second
    val rows = Vector.tabulate(labels.length): row =>
      val center =
        if labels(row) == first then -value.separation.toDouble
        else value.separation.toDouble
      val replicate = row / 2
      Vector.tabulate(value.featureCount): column =>
        val token = Math.floorMod(value.salt + replicate * 5 + column * 3, 7)
        center + (token - 3).toDouble * 0.01
    LearnerFixture(
      classes,
      first,
      second,
      GaleTestMatrix.fromRows(rows),
      labels,
      rows.map(row => IArray.unsafeFromArray(row.toArray))
    )

  private def learnerViolations(
      compiler: CategoricalLearnerCompiler[
        ExternalMean,
        ExternalMeanFit,
        ExternalMeanError,
        ExternalMeanPrediction
      ],
      configuration: ExternalMean,
      fixture: LearnerFixture
  ): Either[ExternalMeanError, Vector[String]] =
    compiler
      .fit(configuration, fixture.classes, fixture.data, fixture.labels)
      .flatMap: fitted =>
        val violations = Vector.newBuilder[String]
        def inspect(row: Int): Either[ExternalMeanError, Vector[String]] =
          if row == fixture.inputs.length then Right(violations.result().distinct)
          else
            compiler
              .predict(fitted, fixture.inputs(row))
              .flatMap: prediction =>
                val scoreLabels = prediction.scores.map(_._1)
                if scoreLabels != fixture.classes.keys then violations += "score-axis-order"
                if scoreLabels.distinct.length != fixture.classes.size then violations += "score-axis-multiplicity"
                prediction.scores.maxByOption(_._2.value) match
                  case None              => violations += "missing-scores"
                  case Some((winner, _)) =>
                    if prediction.predicted != winner then violations += "winner-score-consistency"
                if prediction.predicted != fixture.labels(row) then violations += "target-reconstruction"
                inspect(row + 1)
        inspect(0)

  private def wrongWinnerCompiler(
      delegate: CategoricalLearnerCompiler[
        ExternalMean,
        ExternalMeanFit,
        ExternalMeanError,
        ExternalMeanPrediction
      ]
  ): CategoricalLearnerCompiler[
    ExternalMean,
    ExternalMeanFit,
    ExternalMeanError,
    ExternalMeanPrediction
  ] =
    new CategoricalLearnerCompiler[
      ExternalMean,
      ExternalMeanFit,
      ExternalMeanError,
      ExternalMeanPrediction
    ]:
      override def definition(
          configuration: ExternalMean
      ): CategoricalLearnerDefinition =
        delegate.definition(configuration)

      override def fit(
          configuration: ExternalMean,
          classes: AxisRef[ClassId],
          data: DMat,
          labels: Vector[ClassId]
      ): Either[ExternalMeanError, ExternalMeanFit] =
        delegate.fit(configuration, classes, data, labels)

      override def predict(
          fitted: ExternalMeanFit,
          input: IArray[Double]
      ): Either[ExternalMeanError, ExternalMeanPrediction] =
        delegate
          .predict(fitted, input)
          .map: prediction =>
            val wrong = prediction.scores.iterator
              .map(_._1)
              .find(_ != prediction.predicted)
              .getOrElse(prediction.predicted)
            prediction.copy(predicted = wrong)

      override def failureMessage(error: ExternalMeanError): String =
        delegate.failureMessage(error)

  private def drawOccurrences(mapping: Vector[Int]): Vector[(Int, Int, Int)] =
    val counts = scala.collection.mutable.Map.empty[Int, Int].withDefaultValue(0)
    mapping.zipWithIndex.map: (source, drawPosition) =>
      val occurrence = counts(source)
      counts.update(source, occurrence + 1)
      (source, occurrence, drawPosition)

  private def admitted[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"generated fixture admission failed: $error")

  private def all(checks: (Boolean, String)*): Prop =
    checks.foldLeft(Prop(true)):
      case (combined, (holds, label)) => combined && (Prop(holds) :| label)

  private def check(holds: Boolean, label: String): (Boolean, String) =
    holds -> label
