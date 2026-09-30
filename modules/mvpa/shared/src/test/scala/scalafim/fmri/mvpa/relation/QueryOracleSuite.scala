package scalafim.fmri.mvpa.relation

import gale.linalg.{DMat, DVec, DoubleLinearOperator, LinAlgError, MutableDVec}
import multivar.core.{CoordinateEvidence, Lin, SemanticProvenance, SemanticSpace, SpaceRole, Table, ValueId, ValueIdentity}
import scala.compiletime.testing.typeCheckErrors
import scalafim.fmri.mvpa.{AxisRef, EvidenceError}

/** Independent dense and access-policy oracles for the M2.03 relation queries.
  * The numerical oracle below is deliberately a four-index scalar contraction,
  * rather than another arrangement of the implementation's matrix products.
  */
class QueryOracleSuite extends munit.FunSuite:
  private def right[A](value: Either[EvidenceError, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def axis(name: String, role: SpaceRole, size: Int): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, role, Vector.tabulate(size)(index => s"$name-$index"), "fixture", "unit", "raw", Vector("query-oracle")))

  private def valueIdentity(name: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(name))

  private def table[RK, CK](rows: AxisRef[RK], columns: AxisRef[CK], values: DMat, name: String): Table[rows.Id, columns.Id] =
    Lin
      .fromDenseMatrix(values, CoordinateEvidence.dual(columns.evidence), CoordinateEvidence.primal(rows.evidence), valueIdentity(name), SemanticProvenance.source("query-oracle"))
      .fold(error => fail(error.toString), result => result)

  private def operatorTable[RK, CK](rows: AxisRef[RK], columns: AxisRef[CK], operator: DoubleLinearOperator, name: String): Table[rows.Id, columns.Id] =
    Lin
      .fromLinearMap(operator, CoordinateEvidence.dual(columns.evidence), CoordinateEvidence.primal(rows.evidence), valueIdentity(name), SemanticProvenance.source("query-oracle"))
      .fold(error => fail(error.toString), result => result)

  private def dense[R <: SemanticSpace, C <: SemanticSpace](value: Table[R, C]): DMat =
    right(value(DMat.eye(value.cols)).left.map(EvidenceError.SemanticFailure.apply))

  private val origins = RelationOrigins(RelationSource("acquisition", "response", "readout", "preparation", "noise"), RelationAccess.OwnedReplay("query-oracle-fixture"))
  private val oneShotOrigins = RelationOrigins(RelationSource("acquisition", "response", "readout", "preparation", "noise"), RelationAccess.OneShot)

  private def matrices: (DMat, DMat, DMat, DMat) =
    (
      DMat.dense(2, 3, Vector(1.0, -2.0, 3.0, 4.0, 0.0, -1.0)),
      DMat.dense(3, 2, Vector(2.0, 1.0, -1.0, 3.0, 0.5, -2.0)),
      DMat.dense(2, 3, Vector(1.0, -1.0, 2.0, 0.5, 3.0, -2.0)),
      DMat.dense(3, 2, Vector(2.0, -1.0, 4.0, 0.5, -3.0, 1.0))
    )

  private def relation[EK, NK](effects: AxisRef[EK], neural: AxisRef[NK], values: DMat, name: String): Relation[effects.Id, neural.Id] =
    right(Relation(effects, neural, table(effects, neural, values, name), origins, Vector.fill(effects.size)(EffectEstimability.Estimable)))

  private def assertMatrixClose(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertEqualsDouble(actual(row, column), expected(row, column), 1e-12)
        column += 1
      row += 1

  private def scalarOracle(bLeft: DMat, bRight: DMat, h: DMat, k: DMat): Double =
    var total = 0.0
    var leftEffect = 0
    while leftEffect < bLeft.rows do
      var rightEffect = 0
      while rightEffect < bRight.rows do
        var leftNeural = 0
        while leftNeural < bLeft.cols do
          var rightNeural = 0
          while rightNeural < bRight.cols do
            total += h(leftEffect, rightEffect) * bLeft(leftEffect, leftNeural) * k(leftNeural, rightNeural) * bRight(rightEffect, rightNeural)
            rightNeural += 1
          leftNeural += 1
        rightEffect += 1
      leftEffect += 1
    total

  private def effectOracle(bLeft: DMat, bRight: DMat, k: DMat): DMat =
    val out = DMat.newBuilder(bLeft.rows, bRight.rows)
    var leftEffect = 0
    while leftEffect < bLeft.rows do
      var rightEffect = 0
      while rightEffect < bRight.rows do
        var total = 0.0
        var leftNeural = 0
        while leftNeural < bLeft.cols do
          var rightNeural = 0
          while rightNeural < bRight.cols do
            total += bLeft(leftEffect, leftNeural) * k(leftNeural, rightNeural) * bRight(rightEffect, rightNeural)
            rightNeural += 1
          leftNeural += 1
        out.update(leftEffect, rightEffect, total)
        rightEffect += 1
      leftEffect += 1
    out.result()

  private def neuralOracle(bLeft: DMat, bRight: DMat, h: DMat): DMat =
    val out = DMat.newBuilder(bLeft.cols, bRight.cols)
    var leftNeural = 0
    while leftNeural < bLeft.cols do
      var rightNeural = 0
      while rightNeural < bRight.cols do
        var total = 0.0
        var leftEffect = 0
        while leftEffect < bLeft.rows do
          var rightEffect = 0
          while rightEffect < bRight.rows do
            total += bLeft(leftEffect, leftNeural) * h(leftEffect, rightEffect) * bRight(rightEffect, rightNeural)
            rightEffect += 1
          leftEffect += 1
        out.update(leftNeural, rightNeural, total)
        rightNeural += 1
      leftNeural += 1
    out.result()

  private def transpose(value: DMat): DMat =
    val out = DMat.newBuilder(value.cols, value.rows)
    var row = 0
    while row < value.rows do
      var column = 0
      while column < value.cols do
        out.update(column, row, value(row, column))
        column += 1
      row += 1
    out.result()

  private def frobenius(left: DMat, right: DMat): Double =
    assertEquals(left.rows, right.rows)
    assertEquals(left.cols, right.cols)
    var total = 0.0
    var row = 0
    while row < left.rows do
      var column = 0
      while column < left.cols do
        total += left(row, column) * right(row, column)
        column += 1
      row += 1
    total

  test("rectangular signed closures agree with the independent four-index contraction oracle"):
    val leftEffects = axis("left-effects", SpaceRole.Latent, 2)
    val leftNeural = axis("left-neural", SpaceRole.Observed, 3)
    val rightEffects = axis("right-effects", SpaceRole.Latent, 3)
    val rightNeural = axis("right-neural", SpaceRole.Observed, 2)
    val (bLeft, bRight, h, k) = matrices
    val pair = RelationPair(relation(leftEffects, leftNeural, bLeft, "b-left"), relation(rightEffects, rightNeural, bRight, "b-right"))
    val query = SecondOrderQuery(pair, Some(table(leftEffects, rightEffects, h, "h")), Some(table(leftNeural, rightNeural, k, "k")))

    val expectedScalar = scalarOracle(bLeft, bRight, h, k)
    val expectedEffect = effectOracle(bLeft, bRight, k)
    val expectedNeural = neuralOracle(bLeft, bRight, h)
    assertEqualsDouble(right(query.scalar).value, expectedScalar, 1e-12)
    assertEqualsDouble(frobenius(h, expectedEffect), expectedScalar, 1e-12)
    assertEqualsDouble(frobenius(k, expectedNeural), expectedScalar, 1e-12)
    assertMatrixClose(dense(right(query.effectForm).values), expectedEffect)
    assertMatrixClose(dense(right(query.neuralForm).values), expectedNeural)
    assert(expectedEffect(0, 0) < 0.0)
    assert(expectedEffect(0, 1) > 0.0)

  test("first-order contrasts and reversed pairs preserve their bilinear laws"):
    val leftEffects = axis("left-effects", SpaceRole.Latent, 2)
    val leftNeural = axis("left-neural", SpaceRole.Observed, 3)
    val rightEffects = axis("right-effects", SpaceRole.Latent, 3)
    val rightNeural = axis("right-neural", SpaceRole.Observed, 2)
    val contrasts = axis("contrasts", SpaceRole.Latent, 2)
    val (bLeft, bRight, h, k) = matrices
    val left = relation(leftEffects, leftNeural, bLeft, "b-left")
    val rightRelation = relation(rightEffects, rightNeural, bRight, "b-right")
    val weights = DMat.dense(2, 2, Vector(2.0, -1.0, 0.5, 3.0))
    val first = FirstOrderQuery(leftEffects, contrasts, table(leftEffects, contrasts, weights, "contrast"))
    val firstValues = dense(right(first(left)).values)
    val expectedFirst = DMat.dense(2, 3, Vector(
      2.0 * bLeft(0, 0) + 0.5 * bLeft(1, 0), 2.0 * bLeft(0, 1) + 0.5 * bLeft(1, 1), 2.0 * bLeft(0, 2) + 0.5 * bLeft(1, 2),
      -bLeft(0, 0) + 3.0 * bLeft(1, 0), -bLeft(0, 1) + 3.0 * bLeft(1, 1), -bLeft(0, 2) + 3.0 * bLeft(1, 2)
    ))
    assertMatrixClose(firstValues, expectedFirst)
    assertMatrixClose(dense(right(first(left)).values.star), transpose(expectedFirst))

    val pair = RelationPair(left, rightRelation)
    val hTable = table(leftEffects, rightEffects, h, "h")
    val kTable = table(leftNeural, rightNeural, k, "k")
    val query = SecondOrderQuery(pair, Some(hTable), Some(kTable))
    val reversed = query.reverse
    val expectedEffect = effectOracle(bLeft, bRight, k)
    val expectedNeural = neuralOracle(bLeft, bRight, h)
    assertEqualsDouble(right(reversed.scalar).value, right(query.scalar).value, 1e-12)
    assertMatrixClose(dense(right(reversed.effectForm).values), transpose(expectedEffect))
    assertMatrixClose(dense(right(reversed.neuralForm).values), transpose(expectedNeural))
    assertMatrixClose(dense(right(query.open.reverse.closeNeural(kTable.star)).values), transpose(expectedEffect))
    assertMatrixClose(dense(right(query.effectForm).values.star), transpose(expectedEffect))
    assertMatrixClose(dense(right(query.neuralForm).values.star), transpose(expectedNeural))

  test("foreign same-sized closure axes do not typecheck"):
    val errors = typeCheckErrors("""
import multivar.core.{SemanticSpace, Table}
import scalafim.fmri.mvpa.relation.OpenRelationTransport
def invalid[EL <: SemanticSpace, NL <: SemanticSpace, ER <: SemanticSpace, NR <: SemanticSpace, Foreign <: SemanticSpace](
  open: OpenRelationTransport[EL, NL, ER, NR], foreign: Table[Foreign, ER]
) = open.closeExperimental(foreign)
""")
    assert(errors.nonEmpty)

  test("constructing relations and opening a transport do not read operators"):
    val effects = axis("effects", SpaceRole.Latent, 2)
    val neural = axis("neural", SpaceRole.Observed, 3)
    var reads = 0
    val poison = new DoubleLinearOperator:
      val rows = 2
      val cols = 3
      def applyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        throw new IllegalStateException("operator was read")
    val relationValue = right(Relation(effects, neural, operatorTable(effects, neural, poison, "poison"), origins, Vector.fill(2)(EffectEstimability.Estimable)))
    val pair = RelationPair(relationValue, relationValue)
    val open = SecondOrderQuery(pair).open
    assertEquals(reads, 0)
    assertEquals(open.pair.left.effectAxis.size, 2)

  test("one-shot scalar closure is refused before reading either aliased endpoint"):
    val effects = axis("one-shot-effects", SpaceRole.Latent, 1)
    val neural = axis("one-shot-neural", SpaceRole.Observed, 1)
    var reads = 0
    val poison = new DoubleLinearOperator:
      val rows = 1
      val cols = 1
      def applyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        throw new IllegalStateException("one-shot endpoint was read")
    val endpoint = right(Relation(effects, neural, operatorTable(effects, neural, poison, "one-shot"), oneShotOrigins, Vector(EffectEstimability.Estimable)))
    val pair = RelationPair(endpoint, endpoint)
    val closure = SecondOrderQuery(pair, Some(table(effects, effects, DMat.eye(1), "one-shot-h")), Some(table(neural, neural, DMat.eye(1), "one-shot-k")))
    closure.scalar match
      case Left(EvidenceError.InvalidSource(_)) => ()
      case other => fail(s"expected one-shot scalar refusal, got $other")
    assertEquals(reads, 0)

  test("scalar closure reads lazy tables in width-one batches"):
    final class WidthOneOperator(value: DMat) extends DoubleLinearOperator:
      val rows = value.rows
      val cols = value.cols
      var applications = 0
      var maximumForwardBatchWidth = 0
      var maximumTransposeBatchWidth = 0
      def applyTo(input: DVec, output: MutableDVec): Unit =
        applications += 1
        value.applyTo(input, output)
      override def applyTo(input: DMat): Either[LinAlgError, DMat] =
        maximumForwardBatchWidth = Math.max(maximumForwardBatchWidth, input.cols)
        super.applyTo(input)
      override def transposeApplyTo(input: DMat): Either[LinAlgError, DMat] =
        maximumTransposeBatchWidth = Math.max(maximumTransposeBatchWidth, input.cols)
        super.transposeApplyTo(input)
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
        applications += 1
        value.transposeApplyTo(input, output)
      override def adjoint: DoubleLinearOperator =
        val outer = this
        new DoubleLinearOperator:
          val rows = outer.cols
          val cols = outer.rows
          def applyTo(input: DVec, output: MutableDVec): Unit = outer.transposeApplyTo(input, output)
          override def applyTo(input: DMat): Either[LinAlgError, DMat] = outer.transposeApplyTo(input)
          override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = outer.applyTo(input, output)
          override def transposeApplyTo(input: DMat): Either[LinAlgError, DMat] = outer.applyTo(input)

    val leftEffects = axis("left-effects", SpaceRole.Latent, 2)
    val leftNeural = axis("left-neural", SpaceRole.Observed, 3)
    val rightEffects = axis("right-effects", SpaceRole.Latent, 3)
    val rightNeural = axis("right-neural", SpaceRole.Observed, 2)
    val (bLeft, bRight, h, k) = matrices
    val leftOperator = new WidthOneOperator(bLeft)
    val rightOperator = new WidthOneOperator(bRight)
    val hOperator = new WidthOneOperator(h)
    val kOperator = new WidthOneOperator(k)
    val pair = RelationPair(
      right(Relation(leftEffects, leftNeural, operatorTable(leftEffects, leftNeural, leftOperator, "lazy-left"), origins, Vector.fill(2)(EffectEstimability.Estimable))),
      right(Relation(rightEffects, rightNeural, operatorTable(rightEffects, rightNeural, rightOperator, "lazy-right"), origins, Vector.fill(3)(EffectEstimability.Estimable)))
    )
    val query = SecondOrderQuery(pair, Some(operatorTable(leftEffects, rightEffects, hOperator, "lazy-h")), Some(operatorTable(leftNeural, rightNeural, kOperator, "lazy-k")))
    assertEquals(leftOperator.applications + rightOperator.applications + hOperator.applications + kOperator.applications, 0)
    assertEqualsDouble(right(query.scalar).value, scalarOracle(bLeft, bRight, h, k), 1e-12)
    val operators = Vector(leftOperator, rightOperator, hOperator, kOperator)
    assert(operators.forall(_.applications > 0))
    assertEquals(leftOperator.maximumForwardBatchWidth, 1)
    assertEquals(rightOperator.maximumTransposeBatchWidth, 1)
    assertEquals(hOperator.maximumTransposeBatchWidth, 1)
    assertEquals(kOperator.maximumForwardBatchWidth, 1)
