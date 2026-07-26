package scalafim.multivar
package core

import scalafim.multivar.core.*

import scala.compiletime.testing.typeCheckErrors

import gale.linalg.{DMat, DVec, DoubleLinearOperator, LinAlgError, LinearOperator, MutableDVec}
import gale.sparse.Sparse

class OperatorCoreSuite extends munit.FunSuite:

  private final case class TestOperator(matrix: DMat) extends DoubleLinearOperator:
    override def rows: Int = matrix.rows
    override def cols: Int = matrix.cols
    override def applyTo(input: DVec, output: MutableDVec): Unit = multiply(matrix, input, output)
    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = multiply(matrix.t, input, output)

    private def multiply(value: DMat, input: DVec, output: MutableDVec): Unit =
      if input.length != value.cols then throw LinAlgError.VectorLengthMismatch(value.cols, input.length)
      if output.length != value.rows then throw LinAlgError.VectorLengthMismatch(value.rows, output.length)
      var row = 0
      while row < value.rows do
        var sum = 0.0
        var col = 0
        while col < value.cols do
          sum += value(row, col) * input(col)
          col += 1
        output(row) = sum
        row += 1

  test("secondOrder and compress agree with direct dense products"):
    val sourceRows = space("source-rows", SpaceRole.Samples, 3)
    val targetRows = space("target-rows", SpaceRole.Samples, 2)
    val sourceFeatures = space("source-features", SpaceRole.Observed, 2)
    val targetFeatures = space("target-features", SpaceRole.Observed, 3)
    val sourceComponents = space("source-components", SpaceRole.Latent, 1)
    val targetComponents = space("target-components", SpaceRole.Latent, 2)
    type OS = sourceRows.Id
    type OT = targetRows.Id
    type CS = sourceFeatures.Id
    type CT = targetFeatures.Id
    type KS = sourceComponents.Id
    type KT = targetComponents.Id

    val xsMatrix = matrix(Vector(Vector(1.0, 2.0), Vector(0.0, -1.0), Vector(3.0, 0.5)))
    val xtMatrix = matrix(Vector(Vector(2.0, 0.0, 1.0), Vector(-1.0, 3.0, 0.5)))
    val linkMatrix = matrix(Vector(Vector(1.0, 0.0), Vector(0.5, -1.0), Vector(0.0, 2.0)))
    val wsMatrix = matrix(Vector(Vector(1.0), Vector(-0.5)))
    val wtMatrix = matrix(Vector(Vector(1.0, 0.0), Vector(0.0, 1.0), Vector(0.5, -0.5)))

    val xs: OpTable[OS, CS, UncheckedEvidence] = accepted(
      Op.fromDense(
        xsMatrix,
        CoordinateEvidence.dual(sourceFeatures.evidence),
        CoordinateEvidence.primal(sourceRows.evidence),
        OperatorRoleWitness.table,
        id("xs")
      )
    )
    val xt: OpTable[OT, CT, UncheckedEvidence] = accepted(
      Op.fromDense(
        xtMatrix,
        CoordinateEvidence.dual(targetFeatures.evidence),
        CoordinateEvidence.primal(targetRows.evidence),
        OperatorRoleWitness.table,
        id("xt")
      )
    )
    val link: OpRowLink[OS, OT, UncheckedEvidence] = accepted(
      Op.fromDense(
        linkMatrix,
        CoordinateEvidence.primal(targetRows.evidence),
        CoordinateEvidence.dual(sourceRows.evidence),
        OperatorRoleWitness.rowLink,
        id("link")
      )
    )
    val ws: OpFrame[CS, KS, UncheckedEvidence] = accepted(
      Op.fromDense(
        wsMatrix,
        CoordinateEvidence.primal(sourceComponents.evidence),
        CoordinateEvidence.dual(sourceFeatures.evidence),
        OperatorRoleWitness.frame,
        id("ws")
      )
    )
    val wt: OpFrame[CT, KT, UncheckedEvidence] = accepted(
      Op.fromDense(
        wtMatrix,
        CoordinateEvidence.primal(targetComponents.evidence),
        CoordinateEvidence.dual(targetFeatures.evidence),
        OperatorRoleWitness.frame,
        id("wt")
      )
    )

    val second = OperatorAlgebra.secondOrder(xs, link, xt)
    val compressed = OperatorAlgebra.compress(ws, second, wt)
    assertEquals(second.role.value, OperatorRole.Cross)
    assertEquals(compressed.role.value, OperatorRole.Component)
    assertMatrix(accepted(second.toDense), xsMatrix.t * linkMatrix * xtMatrix)
    assertMatrix(accepted(compressed.toDense), wsMatrix.t * xsMatrix.t * linkMatrix * xtMatrix * wtMatrix)

  test("FunctionalFrame derives scores and axes from one weight operator"):
    val rows = space("frame-rows", SpaceRole.Samples, 3)
    val features = space("frame-features", SpaceRole.Observed, 2)
    val components = space("frame-components", SpaceRole.Latent, 1)
    type O = rows.Id
    type C = features.Id
    type K = components.Id
    val xMatrix = matrix(Vector(Vector(1.0, 2.0), Vector(0.0, -1.0), Vector(3.0, 0.5)))
    val wMatrix = matrix(Vector(Vector(1.0), Vector(-0.5)))
    val qMatrix = matrix(Vector(Vector(2.0, 0.0), Vector(0.0, 0.5)))
    val x: OpTable[O, C, UncheckedEvidence] = accepted(
      Op.fromDense(xMatrix, CoordinateEvidence.dual(features.evidence), CoordinateEvidence.primal(rows.evidence), OperatorRoleWitness.table, id("frame-x"))
    )
    val w: OpFrame[C, K, UncheckedEvidence] = accepted(
      Op.fromDense(wMatrix, CoordinateEvidence.primal(components.evidence), CoordinateEvidence.dual(features.evidence), OperatorRoleWitness.frame, id("frame-w"))
    )
    val q = certifiedCometric(features.evidence, qMatrix, "frame-q")
    val frame = FunctionalFrame(w, Some(q))

    assertMatrix(accepted(frame.scores(x).toDense), xMatrix * wMatrix)
    assertMatrix(accepted(frame.axes.get.toDense), qMatrix * wMatrix)
    assertEquals(frame.scores(x).role.value, OperatorRole.Score)
    assertEquals(frame.axes.get.role.value, OperatorRole.Axis)

  test("algebraic dual preserves certified evidence and differs from metric adjoint"):
    val source = space("adjoint-source", SpaceRole.Observed, 2)
    val target = space("adjoint-target", SpaceRole.Observed, 2)
    type S = source.Id
    type T = target.Id
    val a = matrix(Vector(Vector(1.0, 2.0), Vector(-1.0, 0.5)))
    val op = accepted(
      Op.fromDense(
        a,
        CoordinateEvidence.primal(source.evidence),
        CoordinateEvidence.primal(target.evidence),
        OperatorRoleWitness.table,
        id("adjoint-a")
      )
    )
    val sourceMetric = certifiedMetric(source.evidence, matrix(Vector(Vector(2.0, 0.0), Vector(0.0, 1.0))), "adjoint-ms")
    val targetMetric = certifiedMetric(target.evidence, matrix(Vector(Vector(1.0, 0.0), Vector(0.0, 3.0))), "adjoint-mt")
    val metricAdjoint = accepted(OperatorAlgebra.metricAdjoint(op, sourceMetric, targetMetric))
    val expected = matrix(Vector(Vector(0.5, -1.5), Vector(2.0, 1.5)))

    assertMatrix(accepted(op.dual.toDense), a.t)
    assertMatrix(accepted(metricAdjoint.toDense), expected)
    assertNotEquals(matrixData(accepted(op.dual.toDense)), matrixData(accepted(metricAdjoint.toDense)))

    val certified = certifiedMetric(source.evidence, matrix(Vector(Vector(2.0, 0.0), Vector(0.0, 1.0))), "dual-certificate")
    val dual = certified.dual
    assertEquals(dual.certificate.status, EvidenceStatus.Certified)
    assertEquals(dual.certificate.valueIdentity, dual.valueIdentity)
    assert(dual.certificate.claims.forall(_.valueIdentity == dual.valueIdentity))

    val unprovenMetric = accepted(
      Op.fromDense(
        matrix(Vector(Vector(2.0, 0.0), Vector(0.0, 1.0))),
        CoordinateEvidence.primal(source.evidence),
        CoordinateEvidence.dual(source.evidence),
        OperatorRoleWitness.metric,
        id("assumed-metric")
      )
    )
    val assumed = accepted(Unsafe.assumeSpd(unprovenMetric, "external calibration contract"))
    assertEquals(assumed.certificate.status, EvidenceStatus.Assumed)
    assert(assumed.provenance.events.exists {
      case SemanticProvenanceEvent.UnsafeAssumption("spd", "external calibration contract") => true
      case _                                                                                   => false
    })
    assert(Unsafe.assumeSpd(unprovenMetric, " ").isLeft)

  test("representations remain structural across every supported operator kernel"):
    val from = space("representation-from", SpaceRole.Observed, 2)
    val to = space("representation-to", SpaceRole.Observed, 2)
    type From = from.Id
    type To = to.Id
    val dense = accepted(
      Op.fromDense(
        matrix(Vector(Vector(1.0, 0.0), Vector(0.0, 2.0))),
        CoordinateEvidence.primal(from.evidence),
        CoordinateEvidence.primal(to.evidence),
        OperatorRoleWitness.table,
        id("dense")
      )
    )
    val sparseMatrix =
      val builder = Sparse.coo(2, 2)
      builder.add(0, 0, 1.0)
      builder.add(1, 1, 2.0)
      builder.toCSR()
    val sparse = accepted(
      Op.fromLinearMap(
        sparseMatrix,
        CoordinateEvidence.primal(from.evidence),
        CoordinateEvidence.primal(to.evidence),
        OperatorRoleWitness.table,
        id("sparse")
      )
    )
    val diagonal = accepted(
      Op.fromLinearMap(
        Sparse.diagonal(1.0, 2.0),
        CoordinateEvidence.primal(from.evidence),
        CoordinateEvidence.primal(to.evidence),
        OperatorRoleWitness.table,
        id("diagonal")
      )
    )
    val lowRank = accepted(
      Op.lowRank(
        matrix(Vector(Vector(1.0), Vector(2.0))),
        matrix(Vector(Vector(3.0), Vector(-1.0))),
        CoordinateEvidence.primal(from.evidence),
        CoordinateEvidence.primal(to.evidence),
        OperatorRoleWitness.table,
        id("low-rank")
      )
    )
    val blockMap = acceptedLinear(GaleOperators.blockDiagonal(Vector(sparseMatrix, sparseMatrix)))
    val blockSpace = space("representation-block", SpaceRole.Observed, 4)
    type Block = blockSpace.Id
    val block = accepted(
      Op.fromLinearMap(
        blockMap,
        CoordinateEvidence.primal(blockSpace.evidence),
        CoordinateEvidence.primal(blockSpace.evidence),
        OperatorRoleWitness.table,
        id("block")
      )
    )
    val kroneckerSpace = space("representation-kronecker", SpaceRole.Observed, 4)
    type Kronecker = kroneckerSpace.Id
    val kroneckerMap = acceptedLinear(
      LinearOperator.kronecker(
        matrix(Vector(Vector(1.0, 2.0), Vector(0.0, -1.0))),
        Sparse.diagonal(3.0, 0.5)
      )
    )
    val kronecker = accepted(
      Op.fromLinearMap(
        kroneckerMap,
        CoordinateEvidence.primal(kroneckerSpace.evidence),
        CoordinateEvidence.primal(kroneckerSpace.evidence),
        OperatorRoleWitness.table,
        id("kronecker")
      )
    )
    val sparseView = acceptedMultivar(
      SparseMatrixView.fromTriplets(2, 2, Array(0, 1), Array(0, 1), Array(1.0, 2.0))
    )
    val affineView = acceptedMultivar(
      MatrixView.affine(
        sparseView,
        DVec.fromSeq(Vector(2.0, 0.5)),
        DVec.fromSeq(Vector(0.25, -1.0)),
        StoragePolicy.Operator
      )
    )
    val lazyAffine = accepted(
      Op.fromMatrixView(
        affineView,
        CoordinateEvidence.primal(from.evidence),
        CoordinateEvidence.primal(to.evidence),
        OperatorRoleWitness.table,
        id("lazy-affine")
      )
    )
    val matrixFree = accepted(
      Op.fromLinearMap(
        TestOperator(matrix(Vector(Vector(1.0, 0.0), Vector(0.0, 2.0)))),
        CoordinateEvidence.primal(from.evidence),
        CoordinateEvidence.primal(to.evidence),
        OperatorRoleWitness.table,
        id("matrix-free")
      )
    )

    assertEquals(dense.representation, OperatorRepresentation.Dense)
    assertEquals(sparse.representation, OperatorRepresentation.Sparse)
    assertEquals(diagonal.representation, OperatorRepresentation.Diagonal)
    assertEquals(lowRank.representation, OperatorRepresentation.LowRank)
    assertEquals(lowRank.dual.representation, OperatorRepresentation.LowRank)
    assertEquals(block.representation, OperatorRepresentation.Block)
    assertEquals(block.dual.representation, OperatorRepresentation.Block)
    assertEquals(kronecker.representation, OperatorRepresentation.Kronecker)
    assertEquals(kronecker.dual.representation, OperatorRepresentation.Kronecker)
    assertEquals(lazyAffine.representation, OperatorRepresentation.LazyAffine)
    assertEquals(matrixFree.representation, OperatorRepresentation.MatrixFree)
    assertMatrix(accepted(lowRank.toDense), matrix(Vector(Vector(3.0, -1.0), Vector(6.0, -2.0))))
    assertMatrix(accepted(kronecker.toDense), kroneckerMap.applyTo(DMat.eye(4)).toOption.get)

  test("composition obeys identity and associativity while block orientation is explicit"):
    val a = space("composition-a", SpaceRole.Observed, 2)
    val b = space("composition-b", SpaceRole.Observed, 2)
    val c = space("composition-c", SpaceRole.Observed, 2)
    val d = space("composition-d", SpaceRole.Observed, 2)
    type A = a.Id
    type B = b.Id
    type C = c.Id
    type D = d.Id
    val fMatrix = matrix(Vector(Vector(1.0, 2.0), Vector(0.0, -1.0)))
    val gMatrix = matrix(Vector(Vector(0.5, 0.0), Vector(3.0, 1.0)))
    val hMatrix = matrix(Vector(Vector(2.0, -1.0), Vector(1.0, 0.5)))
    val f = accepted(Op.fromDense(fMatrix, CoordinateEvidence.primal(a.evidence), CoordinateEvidence.primal(b.evidence), OperatorRoleWitness.table, id("composition-f")))
    val g = accepted(Op.fromDense(gMatrix, CoordinateEvidence.primal(b.evidence), CoordinateEvidence.primal(c.evidence), OperatorRoleWitness.table, id("composition-g")))
    val h = accepted(Op.fromDense(hMatrix, CoordinateEvidence.primal(c.evidence), CoordinateEvidence.primal(d.evidence), OperatorRoleWitness.table, id("composition-h")))
    val leftIdentity = accepted(Op.fromDense(DMat.eye(2), CoordinateEvidence.primal(a.evidence), CoordinateEvidence.primal(a.evidence), OperatorRoleWitness.table, id("composition-left-identity")))
    val rightIdentity = accepted(Op.fromDense(DMat.eye(2), CoordinateEvidence.primal(b.evidence), CoordinateEvidence.primal(b.evidence), OperatorRoleWitness.table, id("composition-right-identity")))

    assertMatrix(accepted(leftIdentity.andThen(f).toDense), fMatrix)
    assertMatrix(accepted(f.andThen(rightIdentity).toDense), fMatrix)
    assertMatrix(accepted(f.andThen(g).andThen(h).toDense), accepted(f.andThen(g.andThen(h)).toDense))

    val topRight = matrix(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0)))
    val bottomLeft = matrix(Vector(Vector(-1.0, 0.5), Vector(2.0, 1.0)))
    val blocks = acceptedLinear(
      GaleOperators.blockMatrix(
        Vector(2, 2),
        Vector(2, 2),
        Vector(
          LinearOperatorBlock(0, 1, topRight),
          LinearOperatorBlock(1, 0, bottomLeft)
        )
      )
    )
    val blockSpace = space("composition-blocks", SpaceRole.Observed, 4)
    val blocked = accepted(
      Op.fromLinearMap(
        blocks,
        CoordinateEvidence.primal(blockSpace.evidence),
        CoordinateEvidence.primal(blockSpace.evidence),
        OperatorRoleWitness.table,
        id("composition-blocks")
      )
    )
    assertMatrix(
      accepted(blocked.toDense),
      matrix(
        Vector(
          Vector(0.0, 0.0, 1.0, 2.0),
          Vector(0.0, 0.0, 3.0, 4.0),
          Vector(-1.0, 0.5, 0.0, 0.0),
          Vector(2.0, 1.0, 0.0, 0.0)
        )
      )
    )

  test("hub-factorized and materialized row links induce the same second-order operator"):
    val sourceRows = space("hub-source-rows", SpaceRole.Samples, 2)
    val targetRows = space("hub-target-rows", SpaceRole.Samples, 3)
    val entities = space("hub-entities", SpaceRole.Samples, 2)
    val sourceFeatures = space("hub-source-features", SpaceRole.Observed, 2)
    val targetFeatures = space("hub-target-features", SpaceRole.Observed, 2)
    type OS = sourceRows.Id
    type OT = targetRows.Id
    type E = entities.Id
    type CS = sourceFeatures.Id
    type CT = targetFeatures.Id
    val psMatrix = matrix(Vector(Vector(1.0, 0.0), Vector(0.5, 1.0)))
    val ptMatrix = matrix(Vector(Vector(1.0, 0.0, 0.5), Vector(0.0, 1.0, 1.0)))
    val entityMatrix = matrix(Vector(Vector(2.0, 0.25), Vector(0.25, 1.0)))
    val xsMatrix = matrix(Vector(Vector(1.0, 2.0), Vector(-1.0, 0.5)))
    val xtMatrix = matrix(Vector(Vector(0.0, 1.0), Vector(2.0, -1.0), Vector(1.5, 0.5)))
    val ps = accepted(Op.fromDense(psMatrix, CoordinateEvidence.primal(sourceRows.evidence), CoordinateEvidence.primal(entities.evidence), OperatorRoleWitness.table, id("hub-ps")))
    val pt = accepted(Op.fromDense(ptMatrix, CoordinateEvidence.primal(targetRows.evidence), CoordinateEvidence.primal(entities.evidence), OperatorRoleWitness.table, id("hub-pt")))
    val entityForm = certifiedMetric(entities.evidence, entityMatrix, "hub-entity-form")
    val factorized: OpRowLink[OS, OT, UncheckedEvidence] =
      pt.andThen(entityForm).andThen(ps.dual).retag(OperatorRoleWitness.rowLink, "hub-factorized-link")
    val materializedMatrix = psMatrix.t * entityMatrix * ptMatrix
    val materialized: OpRowLink[OS, OT, UncheckedEvidence] = accepted(
      Op.fromDense(
        materializedMatrix,
        CoordinateEvidence.primal(targetRows.evidence),
        CoordinateEvidence.dual(sourceRows.evidence),
        OperatorRoleWitness.rowLink,
        id("hub-materialized-link")
      )
    )
    val xs: OpTable[OS, CS, UncheckedEvidence] = accepted(
      Op.fromDense(xsMatrix, CoordinateEvidence.dual(sourceFeatures.evidence), CoordinateEvidence.primal(sourceRows.evidence), OperatorRoleWitness.table, id("hub-xs"))
    )
    val xt: OpTable[OT, CT, UncheckedEvidence] = accepted(
      Op.fromDense(xtMatrix, CoordinateEvidence.dual(targetFeatures.evidence), CoordinateEvidence.primal(targetRows.evidence), OperatorRoleWitness.table, id("hub-xt"))
    )

    assertMatrix(accepted(factorized.toDense), materializedMatrix)
    assertMatrix(
      accepted(OperatorAlgebra.secondOrder(xs, factorized, xt).toDense),
      accepted(OperatorAlgebra.secondOrder(xs, materialized, xt).toDense)
    )

  test("nominal spaces and primal-dual ports reject invalid operator composition"):
    val errors = typeCheckErrors("""
      import scalafim.multivar.core.*
      import scalafim.multivar.contract.*
      import scalafim.multivar.optimization.*
      import scalafim.multivar.solver.*
      import scalafim.multivar.lifecycle.*
      import scalafim.multivar.capability.*
      import scalafim.multivar.family.spectral.*
      import scalafim.multivar.family.paired.*
      import scalafim.multivar.family.canonical.*
      import scalafim.multivar.family.cpca.*
      import scalafim.multivar.family.sparse.*
      import scalafim.multivar.family.glrm.*
      import scalafim.multivar.family.multiblock.*
      import scalafim.multivar.family.kernel.*
      import scalafim.multivar.workflow.*
      val a = SpaceRef(MvSpace(SpaceId.unsafe("a"), SpaceRole.Observed, Dimension.unsafe(2)))
      val b = SpaceRef(MvSpace(SpaceId.unsafe("b"), SpaceRole.Observed, Dimension.unsafe(2)))
      val c = SpaceRef(MvSpace(SpaceId.unsafe("c"), SpaceRole.Observed, Dimension.unsafe(2)))
      type A = a.Id
      type B = b.Id
      type C = c.Id
      val f: Op[Primal[A], Primal[B], TableOperatorRole, UncheckedEvidence] = ???
      val g: Op[Primal[C], Dual[A], RowLinkOperatorRole, UncheckedEvidence] = ???
      f.andThen(g)
    """)
    assert(errors.nonEmpty)

    val evidenceErrors = typeCheckErrors("""
      import scalafim.multivar.core.*
      import scalafim.multivar.contract.*
      import scalafim.multivar.optimization.*
      import scalafim.multivar.solver.*
      import scalafim.multivar.lifecycle.*
      import scalafim.multivar.capability.*
      import scalafim.multivar.family.spectral.*
      import scalafim.multivar.family.paired.*
      import scalafim.multivar.family.canonical.*
      import scalafim.multivar.family.cpca.*
      import scalafim.multivar.family.sparse.*
      import scalafim.multivar.family.glrm.*
      import scalafim.multivar.family.multiblock.*
      import scalafim.multivar.family.kernel.*
      import scalafim.multivar.workflow.*
      val a = SpaceRef(MvSpace(SpaceId.unsafe("evidence-a"), SpaceRole.Observed, Dimension.unsafe(2)))
      val b = SpaceRef(MvSpace(SpaceId.unsafe("evidence-b"), SpaceRole.Observed, Dimension.unsafe(2)))
      type A = a.Id
      type B = b.Id
      val cross: Op[Primal[A], Dual[B], MetricOperatorRole, UncheckedEvidence] = ???
      val certificate: Certificate[SpdProperty] = ???
      Op.certifiedSpd(cross, certificate)
    """)
    assert(evidenceErrors.nonEmpty)

  private def certifiedMetric[S <: SemanticSpace](
      space: SpaceEvidence[S],
      value: DMat,
      name: String
  ): OpMetric[S, CertifiedSpd] =
    val linear = accepted(
      Lin.fromDenseMatrix(
        value,
        CoordinateEvidence.primal(space),
        CoordinateEvidence.dual(space),
        id(name)
      )
    )
    val certificate = accepted(FormCertificates.spd(linear))
    accepted(Op.certifiedSpd(Op.fromLin(linear, OperatorRoleWitness.metric), certificate))

  private def certifiedCometric[S <: SemanticSpace](
      space: SpaceEvidence[S],
      value: DMat,
      name: String
  ): OpCometric[S, CertifiedSpd] =
    val linear = accepted(
      Lin.fromDenseMatrix(
        value,
        CoordinateEvidence.dual(space),
        CoordinateEvidence.primal(space),
        id(name)
      )
    )
    val certificate = accepted(FormCertificates.spd(linear))
    accepted(Op.certifiedSpd(Op.fromLin(linear, OperatorRoleWitness.cometric), certificate))

  private def accepted[A](result: Either[SemanticError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def acceptedLinear[A](result: Either[LinAlgError, A]): A =
    result.fold(error => fail(error.getMessage), identity)

  private def acceptedMultivar[A](result: Either[MultivarError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def space(name: String, role: SpaceRole, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(name), role, Dimension.unsafe(dimension)))

  private def id(name: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(name))

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(rows)

  private def matrixData(value: DMat): Vector[Double] =
    value.valuesRowMajor.toVector

  private def assertMatrix(actual: DMat, expected: DMat, tolerance: Double = 1e-10): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1
