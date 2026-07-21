package scalafim.multivar

import scala.compiletime.testing.typeCheckErrors

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.linalg.LinAlgError
import gale.linalg.MutableDVec
import gale.sparse.CSR
import gale.sparse.Sparse

class SemanticAlgebraSuite extends munit.FunSuite:
  private final case class TestOperator(matrix: DMat) extends DoubleLinearOperator:
    override def rows: Int = matrix.rows
    override def cols: Int = matrix.cols
    override def applyTo(input: DVec, output: MutableDVec): Unit =
      if input.length != cols then throw LinAlgError.VectorLengthMismatch(cols, input.length)
      if output.length != rows then throw LinAlgError.VectorLengthMismatch(rows, output.length)
      var row = 0
      while row < rows do
        var sum = 0.0
        var col = 0
        while col < cols do
          sum += matrix(row, col) * input(col)
          col += 1
        output(row) = sum
        row += 1
    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      TestOperator(matrix.transpose).applyTo(input, output)

  private def accepted[A](result: Either[SemanticError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def acceptedMv[A](result: Either[MultivarError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def ref(id: String, role: SpaceRole, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(id), role, Dimension.unsafe(dimension)))

  private def source(id: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(id))

  private def csr(rows: Int, cols: Int, entries: (Int, Int, Double)*): CSR =
    val builder = Sparse.coo(rows, cols)
    entries.foreach { case (row, col, value) => builder.add(row, col, value) }
    builder.toCSR()

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

  test("space refs add nominal Scala identity while retaining stable serialized identity") {
    val first = ref("patients", SpaceRole.Samples, 2)
    val decoded = ref("patients", SpaceRole.Samples, 2)
    val unrelated = ref("features", SpaceRole.Observed, 2)

    assertEquals(first.descriptor, decoded.descriptor)
    assertEquals(first.descriptor.size, unrelated.descriptor.size)
    assertNotEquals(first.descriptor.id, unrelated.descriptor.id)

    val errors = typeCheckErrors("""
      import scalafim.multivar.*
      val a = SpaceRef(MvSpace(SpaceId.unsafe("a"), SpaceRole.Observed, Dimension.unsafe(2)))
      val b = SpaceRef(MvSpace(SpaceId.unsafe("b"), SpaceRole.Observed, Dimension.unsafe(2)))
      val c = SpaceRef(MvSpace(SpaceId.unsafe("c"), SpaceRole.Observed, Dimension.unsafe(2)))
      type A = a.Id
      type B = b.Id
      type C = c.Id
      val f: Lin[Primal[A], Primal[B]] = ???
      val g: Lin[Primal[C], Primal[A]] = ???
      f.andThen(g)
    """)
    assert(errors.nonEmpty)
  }

  test("directed composition and adjoint obey the typed duality laws") {
    val a = ref("a", SpaceRole.Observed, 2)
    val b = ref("b", SpaceRole.Observed, 3)
    val c = ref("c", SpaceRole.Observed, 2)
    type A = a.Id
    type B = b.Id
    type C = c.Id

    val f = accepted(
      Lin.fromLinearMap[Primal[A], Primal[B]](
        csr(3, 2, (0, 0, 1.0), (1, 1, 2.0), (2, 0, -1.0)),
        CoordinateEvidence.primal(a.evidence),
        CoordinateEvidence.primal(b.evidence),
        source("f")
      )
    )
    val g = accepted(
      Lin.fromLinearMap[Primal[B], Dual[C]](
        csr(2, 3, (0, 0, 2.0), (0, 2, 1.0), (1, 1, 3.0)),
        CoordinateEvidence.primal(b.evidence),
        CoordinateEvidence.dual(c.evidence),
        source("g")
      )
    )

    val composite = f.andThen(g)
    val dualComposite = composite.star
    val reversedDual = g.star.andThen(f.star)
    assertMatrix(accepted(dualComposite(DMat.eye(2))), accepted(reversedDual(DMat.eye(2))))

    val twice = composite.star.star
    assertEquals(twice.valueIdentity, composite.valueIdentity)
    assertEquals(twice.domain.descriptor, composite.domain.descriptor)
    assertEquals(twice.codomain.descriptor, composite.codomain.descriptor)
    assertMatrix(accepted(twice(DMat.eye(2))), accepted(composite(DMat.eye(2))))
  }

  test("table orientation is C-star to O and sparse storage survives the semantic adapter") {
    val rows = ref("observations", SpaceRole.Samples, 3)
    val columns = ref("genes", SpaceRole.Observed, 2)
    type Rows = rows.Id
    type Columns = columns.Id
    val sparse = acceptedMv(
      SparseMatrixView.fromTriplets(
        3,
        2,
        Array(0, 1, 2),
        Array(0, 1, 0),
        Array(1.0, 2.0, 3.0)
      )
    )
    val table: Table[Rows, Columns] = accepted(
      Table.fromMatrixView(sparse, rows.evidence, columns.evidence, source("rna-table"))
    )

    assertEquals(table.domain.descriptor.variance, CoordinateVariance.Dual)
    assertEquals(table.codomain.descriptor.variance, CoordinateVariance.Primal)
    assertEquals(table.descriptor.representation, OperatorRepresentation.Sparse)
    assertEquals(table.star.descriptor.representation, OperatorRepresentation.Sparse)
    assertMatrix(
      accepted(table(DMat.eye(2))),
      GaleNumerics.matrixFromRows(Seq(Seq(1.0, 0.0), Seq(0.0, 2.0), Seq(3.0, 0.0)))
    )

    val errors = typeCheckErrors("""
      import scalafim.multivar.*
      val rows = SpaceRef(MvSpace(SpaceId.unsafe("rows"), SpaceRole.Samples, Dimension.unsafe(2)))
      val cols = SpaceRef(MvSpace(SpaceId.unsafe("cols"), SpaceRole.Observed, Dimension.unsafe(2)))
      type Rows = rows.Id
      type Cols = cols.Id
      val table: Table[Rows, Cols] = ???
      val columnForm: Lin[Primal[Cols], Dual[Cols]] = ???
      table.andThen(columnForm)
    """)
    assert(errors.nonEmpty)
  }

  test("decoded operators reject same-dimensional but nominally different coordinates") {
    val rna = ref("rna-patients", SpaceRole.Samples, 2)
    val mri = ref("mri-patients", SpaceRole.Samples, 2)
    type Rna = rna.Id
    val result = Lin.decode[Primal[Rna], Dual[Rna]](
      csr(2, 2, (0, 0, 1.0), (1, 1, 1.0)),
      CoordinateEvidence.primal(rna.evidence),
      CoordinateEvidence.dual(rna.evidence),
      CoordinateDescriptor(mri.descriptor, CoordinateVariance.Primal),
      CoordinateDescriptor(mri.descriptor, CoordinateVariance.Dual),
      source("decoded-form"),
      SemanticProvenance.source("decoded-ir")
    )

    assert(result.isLeft)
    assert(result.left.toOption.exists(_.isInstanceOf[SemanticError.CoordinateMismatch]))
  }

  test("opaque linalg operators remain matrix-free semantic capabilities") {
    val from = ref("operator-from", SpaceRole.Observed, 2)
    val to = ref("operator-to", SpaceRole.Latent, 2)
    type From = from.Id
    type To = to.Id
    val operator = accepted(
      Lin.fromLinearMap[Primal[From], Primal[To]](
        TestOperator(GaleNumerics.matrixFromRows(Seq(Seq(1.0, 2.0), Seq(0.0, 1.0)))),
        CoordinateEvidence.primal(from.evidence),
        CoordinateEvidence.primal(to.evidence),
        source("opaque-operator")
      )
    )

    assertEquals(operator.descriptor.representation, OperatorRepresentation.MatrixFree)
    assertMatrix(
      accepted(operator(DMat.eye(2))),
      GaleNumerics.matrixFromRows(Seq(Seq(1.0, 2.0), Seq(0.0, 1.0)))
    )
  }

  test("form roles are nominal even when their numerical storage is identical") {
    val genes = ref("genes", SpaceRole.Observed, 2)
    type Genes = genes.Id
    val legacy = acceptedMv(
      MetricSpec.diagonal(
        DVec.fromSeq(Seq(2.0, 1.0)),
        Some(genes.descriptor)
      )
    )
    val primal = accepted(FormOperator.primal(legacy, genes.evidence, source("gene-primal-form")))
    val dual = accepted(FormOperator.dual(legacy, genes.evidence, source("gene-dual-form")))
    val spdPrimal = accepted(FormCertificates.spd(primal))
    val psdPrimal = accepted(FormCertificates.psd(primal))
    val psdDual = accepted(FormCertificates.psd(dual))
    val spdDual = accepted(FormCertificates.spd(dual))

    val bilinear: BilinearForm[Genes] = Form.bilinear(primal, genes.evidence)
    val metric: MetricForm[Genes, CertifiedSpd] = accepted(Form.metric(primal, genes.evidence, spdPrimal))
    val semiMetric: SemiMetric[Genes, CertifiedPsd] = accepted(Form.semiMetric(primal, genes.evidence, psdPrimal))
    val penalty: PenaltyForm[Genes, CertifiedPsd] = accepted(Form.penalty(primal, genes.evidence, psdPrimal))
    val precision: PrecisionForm[Genes, CertifiedSpd] = accepted(Form.precision(primal, genes.evidence, spdPrimal))
    val kernel: KernelForm[Genes, CertifiedPsd] = accepted(Form.kernel(dual, genes.evidence, psdDual))
    val covariance: CovarianceForm[Genes, CertifiedPsd] = accepted(Form.covariance(dual, genes.evidence, psdDual))
    val cometric: CometricForm[Genes, CertifiedSpd] = accepted(Form.cometric(dual, genes.evidence, spdDual))

    assertEquals(bilinear.descriptor.role, FormRole.BilinearForm)
    assertEquals(metric.descriptor.role, FormRole.Metric)
    assertEquals(semiMetric.descriptor.role, FormRole.SemiMetric)
    assertEquals(penalty.descriptor.role, FormRole.Penalty)
    assertEquals(precision.descriptor.role, FormRole.Precision)
    assertEquals(kernel.descriptor.role, FormRole.Kernel)
    assertEquals(covariance.descriptor.role, FormRole.Covariance)
    assertEquals(cometric.descriptor.role, FormRole.Cometric)
    assertEquals(metric.descriptor.evidence, EvidenceStatus.Certified)
    assertEquals(kernel.descriptor.direction.domain.variance, CoordinateVariance.Dual)
    assertEquals(metric.descriptor.direction.domain.variance, CoordinateVariance.Primal)

    val errors = typeCheckErrors("""
      import scalafim.multivar.*
      def consumesMetric[S <: SemanticSpace](value: MetricForm[S, CertifiedSpd]): Unit = ()
      val kernel: KernelForm[Nothing, CertifiedPsd] = ???
      consumesMetric(kernel)
    """)
    assert(errors.nonEmpty)
  }

  test("a structurally symmetric indefinite form cannot become a safe metric") {
    val space = ref("krein", SpaceRole.Observed, 2)
    type S = space.Id
    val matrix = GaleNumerics.matrixFromRows(Seq(Seq(1.0, 2.0), Seq(2.0, 1.0)))
    val legacy = acceptedMv(
      MetricSpec.denseSymmetric(matrix, MetricValidation.Structural, Some(space.descriptor))
    )
    val operator = accepted(FormOperator.primal(legacy, space.evidence, source("indefinite-form")))

    val symmetry = accepted(FormCertificates.symmetric(operator))
    val symmetric: SymmetricForm[S, CertifiedSymmetric] = accepted(Form.symmetric(operator, space.evidence, symmetry))
    assertEquals(symmetric.descriptor.structure, FormStructure.Symmetric)
    assert(FormCertificates.psd(operator).isLeft)
    assert(FormCertificates.spd(operator).isLeft)

    val indefiniteCertificate = accepted(FormCertificates.indefinite(operator))
    val indefinite: IndefiniteForm[S] = accepted(Form.indefinite(operator, space.evidence, indefiniteCertificate))
    assertEquals(indefinite.descriptor.role, FormRole.IndefiniteForm)
    assertEquals(indefinite.descriptor.positivity, PositivityStatus.Indefinite)
  }

  test("certificates bind claims and complete numerical context to one immutable value") {
    val space = ref("certificate-space", SpaceRole.Observed, 2)
    val metric = acceptedMv(MetricSpec.identity(2, Some(space.descriptor)))
    val first = accepted(FormOperator.primal(metric, space.evidence, source("first-value")))
    val second = accepted(FormOperator.primal(metric, space.evidence, source("second-value")))
    val certificate = accepted(FormCertificates.spd(first))

    assertEquals(certificate.runtime.valueIdentity, first.valueIdentity)
    assertEquals(certificate.runtime.context.norm, CertificateNorm.Frobenius)
    assertEquals(certificate.runtime.context.precision, NumericalPrecision.Float64)
    assert(certificate.runtime.context.method.nonEmpty)
    assert(certificate.runtime.context.backend.nonEmpty)
    assert(Form.metric(second, space.evidence, certificate).isLeft)

    val rank = accepted(Certificate.rank(first.valueIdentity, 2, 2, 0.0, 1.0, CertificateContext.portableFloat64))
    val orthogonal = accepted(Certificate.orthogonal(first.valueIdentity, 1e-12, 1.0, CertificateContext.portableFloat64))
    val converged = accepted(Certificate.converged(first.valueIdentity, 8, 1e-12, 1.0, CertificateContext.portableFloat64))
    assertEquals(rank.runtime.claim.property, "rank")
    assertEquals(orthogonal.runtime.claim.property, "orthogonal")
    assertEquals(converged.runtime.claim.property, "converged")
  }

  test("unsafe assumptions remain visible in both Scala evidence and runtime provenance") {
    val space = ref("unsafe-space", SpaceRole.Observed, 2)
    type S = space.Id
    val indefinite = acceptedMv(
      MetricSpec.denseSymmetric(
        GaleNumerics.matrixFromRows(Seq(Seq(1.0, 2.0), Seq(2.0, 1.0))),
        MetricValidation.Structural,
        Some(space.descriptor)
      )
    )
    val operator = accepted(FormOperator.primal(indefinite, space.evidence, source("unsafe-value")))
    val assumed: MetricForm[S, AssumedSpd] =
      accepted(Unsafe.assumeSpd(operator, space.evidence, "external theorem not checked in this process"))

    assertEquals(assumed.descriptor.evidence, EvidenceStatus.Assumed)
    assertEquals(assumed.descriptor.certificates, Vector.empty)
    assert(
      assumed.provenance.events.exists {
        case SemanticProvenanceEvent.UnsafeAssumption("spd", reason) => reason.contains("external theorem")
        case _                                                        => false
      }
    )
    assert(Unsafe.assumeSpd(operator, space.evidence, "   ").isLeft)

    val errors = typeCheckErrors("""
      import scalafim.multivar.*
      def certifiedOnly[S <: SemanticSpace](value: MetricForm[S, CertifiedSpd]): Unit = ()
      val assumed: MetricForm[Nothing, AssumedSpd] = ???
      certifiedOnly(assumed)
    """)
    assert(errors.nonEmpty)
  }
