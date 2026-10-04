package scalafim.fmri.mvpa.decomposition

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import munit.FunSuite
import multivar.core.{ComponentCount, CoordinateEvidence, Lin, Op, OperatorRoleWitness, SpaceRole, UncheckedEvidence, ValueId, ValueIdentity}
import multivar.family.paired.CcaRegularization
import multivar.core.PreprocessSpec
import scalafim.fmri.mvpa.{AxisRef, EvidenceSource, Observations}
import scalafim.fmri.mvpa.analysis.*
import scalafim.fmri.mvpa.relation.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class GlobalDecompositionsSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, role: SpaceRole, n: Int) =
    right(AxisRef.fromStableKeys(name, role, Vector.tabulate(n)(i => s"$name-$i"), "fixture", "unit", "raw"))

  private def relation[E, N](effects: AxisRef[E], neural: AxisRef[N], values: DMat, access: RelationAccess = RelationAccess.OwnedReplay("fixture")) =
    val table = right(Lin.fromDenseMatrix(values, CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(effects.evidence), ValueIdentity.source(ValueId.unsafe("signed-rectangular-v1"))))
    right(Relation(effects, neural, table,
      RelationOrigins(RelationSource("acq", "response", "readout", "preparation", "noise"), access),
      Vector.fill(effects.size)(EffectEstimability.Estimable)))

  private final class Counted(matrix: Option[DMat]) extends DoubleLinearOperator:
    val rows = matrix.fold(4)(_.rows)
    val cols = matrix.fold(2)(_.cols)
    var reads = 0
    def applyTo(input: DVec, output: MutableDVec): Unit =
      reads += 1
      matrix.fold(throw IllegalStateException("poison"))(_.applyTo(input, output))
    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      reads += 1
      matrix.fold(throw IllegalStateException("poison"))(_.transposeApplyTo(input, output))

  private final class Resource extends ObservationProductResource:
    def acquire() = Right(())
    def close() = Right(())

  private def observationSource(name: String) =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))

  private val productCosts = ObservationProviderCosts()
  private val productBudget = ResourceBudget(ResourceLimit.OwnedNumeric(100000L), MaterializationPolicy.AllowSourceCopy(100000L))

  test("neutral relation SVD reconstructs a signed rectangular estimate without PSD retagging"):
    val effects = axis("effect", SpaceRole.Latent, 3)
    val neural = axis("neural", SpaceRole.Observed, 2)
    val values = DMat.dense(3, 2, Vector(1.0, -2.0, 3.0, 4.0, -5.0, 6.0))
    val result = right(GlobalDecompositions.svd(effects, neural, relation(effects, neural, values), 2, DecompositionBudget(100L)))
    assertEquals(result.fit.nFeatures, 2)
    assertEquals(result.left.axis, effects.descriptor)
    assertEquals(result.right.axis, neural.descriptor)
    val reconstructed = result.left.values * result.right.values.t
    for row <- 0 until values.rows; column <- 0 until values.cols do
      assertEqualsDouble(reconstructed(row, column), values(row, column), 1e-10)

  test("relation SVD budget refuses before estimate evaluation"):
    val effects = axis("effect-budget", SpaceRole.Latent, 2)
    val neural = axis("neural-budget", SpaceRole.Observed, 2)
    var reads = 0
    val poison = new DoubleLinearOperator:
      val rows = 2
      val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        throw IllegalStateException("unadmitted relation read")
    val table = right(Lin.fromLinearMap(poison, CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(effects.evidence),
      ValueIdentity.source(ValueId.unsafe("poison-relation"))))
    val values = right(Relation(effects, neural, table, RelationOrigins(RelationSource("a", "b", "c", "d", "e"), RelationAccess.OwnedReplay("fixture")),
      Vector.fill(2)(EffectEstimability.Estimable)))
    assert(GlobalDecompositions.svd(effects, neural, values, 1, DecompositionBudget(1L)).isLeft)
    assertEquals(reads, 0)

  test("PCA centers a literal observation matrix, reconstructs it, and leaves no escaped source"):
    val samples = axis("pca-samples", SpaceRole.Samples, 4)
    val neural = axis("pca-neural", SpaceRole.Observed, 2)
    val values = DMat.dense(4, 2, Vector(9.0, 19.0, 9.0, 21.0, 11.0, 19.0, 11.0, 21.0))
    val counted = new Counted(Some(values))
    val observations = right(Observations.fromOperator(samples, neural, counted,
      ValueIdentity.source(ValueId.unsafe("pca-content-a")), observationSource("pca-a")))
    val result = ObservationProduct.withPrepared(samples, neural, observations,
      ObservationReplay.Scoped("pca", "v1"), new Resource, productCosts, ObservationProductRoute.Direct,
      productBudget, "pca direct"):
        product => GlobalDecompositions.pca(samples, neural, product, 2, PreprocessSpec.Center, DecompositionBudget(100L))
          .left.map(error => ObservationProductError.TaskFailure(error.toString))
    val artifact = right(result)
    val reconstructed = right(artifact.fit.reconstruct(values))
    for row <- 0 until values.rows; column <- 0 until values.cols do
      assertEqualsDouble(reconstructed(row, column), values(row, column), 1e-10)
    assertEqualsDouble(artifact.fit.explainedVariance(0), 4.0 / 3.0, 1e-10)
    assertEqualsDouble(artifact.fit.explainedVariance(1), 4.0 / 3.0, 1e-10)
    val centered = artifact.fit.scores * artifact.fit.loadings.t
    for row <- 0 until 4; column <- 0 until 2 do
      assertEqualsDouble(centered(row, column), values(row, column) - (if column == 0 then 10.0 else 20.0), 1e-10)
    assert(counted.reads > 0)
    val sameNeural: multivar.core.SpaceEvidence[neural.Id] = artifact.neural
    assertEquals(sameNeural.descriptor, neural.evidence.descriptor)

  test("PCA budget rejection does not read a poison product and content identity remains source-bound"):
    val samples = axis("pca-budget-samples", SpaceRole.Samples, 4)
    val neural = axis("pca-budget-neural", SpaceRole.Observed, 2)
    val poison = new Counted(None)
    val observations = right(Observations.fromOperator(samples, neural, poison,
      ValueIdentity.source(ValueId.unsafe("pca-content-poison")), observationSource("pca-poison")))
    val result = ObservationProduct.withPrepared(samples, neural, observations,
      ObservationReplay.Scoped("pca", "v1"), new Resource, productCosts, ObservationProductRoute.Direct,
      productBudget, "pca budget"):
        product => GlobalDecompositions.pca(samples, neural, product, 1, PreprocessSpec.Center, DecompositionBudget(1L))
          .left.map(error => ObservationProductError.TaskFailure(error.toString))
    assert(result.isLeft)
    assertEquals(poison.reads, 0)

  test("PCA refuses an expired product and distinguishes changed values with the same source declaration"):
    val samples = axis("identity-samples", SpaceRole.Samples, 4)
    val neural = axis("identity-neural", SpaceRole.Observed, 2)
    val values = DMat.dense(4, 2, Vector(-1, -1, -1, 1, 1, -1, 1, 1))
    val source = observationSource("same-declaration")
    val identity = ValueIdentity.source(ValueId.unsafe("same-external-value-declaration"))
    var escaped: Option[() => Either[GlobalDecompositionError, ObservationPcaArtifact[samples.Id, neural.Id]]] = None
    def fit(matrix: DMat) =
      val counted = new Counted(Some(matrix))
      val obs = right(Observations.fromOperator(samples, neural, counted, identity, source))
      val result = right(ObservationProduct.withPrepared(samples, neural, obs, ObservationReplay.Scoped("pca", "v1"), new Resource,
        productCosts, ObservationProductRoute.Direct, productBudget, "same values"):
          product =>
            escaped = Some(() => GlobalDecompositions.pca(samples, neural, product, 2, PreprocessSpec.Center, DecompositionBudget(100L)))
            GlobalDecompositions.pca(samples, neural, product, 2, PreprocessSpec.Center, DecompositionBudget(100L))
              .left.map(error => ObservationProductError.TaskFailure(error.toString))
      )
      val before = counted.reads
      assert(escaped.get.apply().isLeft)
      assertEquals(counted.reads, before)
      result
    val first = fit(values)
    val changed = fit(values * 2.0)
    assertEquals(first.evidenceIdentity, changed.evidenceIdentity)
    assertNotEquals(first.contentIdentity, changed.contentIdentity)

  test("PCA and relation artifacts retain nominal endpoint types"):
    val errors = scala.compiletime.testing.typeCheckErrors("""
      import multivar.core.*
      import scalafim.fmri.mvpa.decomposition.*
      def wrong[S <: SemanticSpace, N <: SemanticSpace, Other <: SemanticSpace](fit: ObservationPcaArtifact[S, N]): SpaceEvidence[Other] = fit.neural
    """)
    assert(errors.nonEmpty)

  test("neutral SVD decomposes signed effect and neural cross forms with their real nominal endpoints"):
    val el = axis("cross-effect-left", SpaceRole.Latent, 2)
    val er = axis("cross-effect-right", SpaceRole.Latent, 3)
    val nl = axis("cross-neural-left", SpaceRole.Observed, 2)
    val nr = axis("cross-neural-right", SpaceRole.Observed, 2)
    val left = relation(el, nl, DMat.eye(2))
    val rightRelation = relation(er, nr, DMat.dense(3, 2, Vector(1, 0, 0, 1, 1, -1)))
    val pair = RelationPair(left, rightRelation)
    val k = right(Lin.fromDenseMatrix(DMat.dense(2, 2, Vector(1, 0, 0, -1)), CoordinateEvidence.primal(nr.evidence),
      CoordinateEvidence.dual(nl.evidence), ValueIdentity.source(ValueId.unsafe("signed-neural-closure"))))
    val h = right(Lin.fromDenseMatrix(DMat.dense(2, 3, Vector(1, 0, 0, 0, -1, 0)), CoordinateEvidence.primal(er.evidence),
      CoordinateEvidence.dual(el.evidence), ValueIdentity.source(ValueId.unsafe("signed-effect-closure"))))
    val effect = right(SecondOrderQuery(pair, metric = Some(k)).effectForm)
    val neural = right(SecondOrderQuery(pair, query = Some(h)).neuralForm)
    val effectFit = right(GlobalDecompositions.svd(effect, 2, DecompositionBudget(100L)))
    val neuralFit = right(GlobalDecompositions.svd(neural, 2, DecompositionBudget(100L)))
    val leftWitness: multivar.core.SpaceEvidence[el.Id] = effectFit.left
    val rightWitness: multivar.core.SpaceEvidence[nr.Id] = neuralFit.right
    assertEquals(leftWitness.descriptor, el.evidence.descriptor)
    assertEquals(rightWitness.descriptor, nr.evidence.descriptor)
    val expectedEffect = DMat.dense(2, 3, Vector(1, 0, 1, 0, -1, 1))
    val expectedNeural = DMat.dense(2, 2, Vector(1, 0, 0, -1))
    val effectReconstruction = effectFit.leftScores * effectFit.rightLoadings.t
    val neuralReconstruction = neuralFit.leftScores * neuralFit.rightLoadings.t
    for row <- 0 until 2; column <- 0 until 3 do
      assertEqualsDouble(effectReconstruction(row, column), expectedEffect(row, column), 1e-10)
    for row <- 0 until 2; column <- 0 until 2 do
      assertEqualsDouble(neuralReconstruction(row, column), expectedNeural(row, column), 1e-10)

  test("explicit paired row geometries distinguish PLSC covariance from regularized CCA"):
    val rows = axis("paired-rows", SpaceRole.Samples, 4)
    val left = axis("paired-left", SpaceRole.Observed, 2)
    val rightAxis = axis("paired-right", SpaceRole.Observed, 2)
    def table(axis: AxisRef[String], values: DMat, id: String) =
      Op.fromLin(right(Lin.fromDenseMatrix(values, CoordinateEvidence.dual(axis.evidence), CoordinateEvidence.primal(rows.evidence), ValueIdentity.source(ValueId.unsafe(id)))), OperatorRoleWitness.table)
    val x = table(left, DMat.dense(4, 2, Vector(1, 0, 0, 1, -1, 0, 0, -1)), "paired-x")
    val y = table(rightAxis, DMat.dense(4, 2, Vector(2, 1, 0, -1, -2, -1, 0, 1)), "paired-y")
    val link = Op.fromLin(right(Lin.fromDenseMatrix(DMat.eye(4), CoordinateEvidence.primal(rows.evidence), CoordinateEvidence.dual(rows.evidence), ValueIdentity.source(ValueId.unsafe("paired-exact-rows")))), OperatorRoleWitness.rowLink)
    val components = right(ComponentCount(1))
    val plsc = right(GlobalDecompositions.plsc(rows.evidence, rows.evidence, left.evidence, rightAxis.evidence,
      x, y, link, link, link, RelationAccess.OwnedReplay("x"), RelationAccess.OwnedReplay("y"), components, 1.0, DecompositionBudget(1000L)))
    val cca = right(GlobalDecompositions.cca(rows.evidence, rows.evidence, left.evidence, rightAxis.evidence,
      x, y, link, link, link, components, right(CcaRegularization.symmetric(.1)), 1.0,
      RelationAccess.OwnedReplay("x"), RelationAccess.OwnedReplay("y"), DecompositionBudget(1000L)))
    assertEquals(plsc.method, multivar.family.paired.PairedLatentMethod.Plsc)
    assert(cca.method.isInstanceOf[multivar.family.paired.PairedLatentMethod.Cca])
    // Literal X'Y = [[4,2],[0,-2]]. The squared PLSC singular values
    // are the roots of z² - 24z + 64 = 0.
    assertEqualsDouble(plsc.fit.result.singularValues(0), math.sqrt(12.0 + 4.0 * math.sqrt(5.0)), 1e-10)
    // Ridge-normalized CCA has squared spectrum of
    // [[34,-.4],[-.4,32.4]] / (17.21 * 2.1), independently inverted by hand.
    assertEqualsDouble(cca.fit.result.singularValues(0), math.sqrt((33.2 + math.sqrt(.8)) / (17.21 * 2.1)), 1e-10)
    val doubled = Op.fromLin(right(Lin.fromDenseMatrix(DMat.eye(4) * 2.0, CoordinateEvidence.primal(rows.evidence),
      CoordinateEvidence.dual(rows.evidence), ValueIdentity.source(ValueId.unsafe("paired-double-source-geometry")))), OperatorRoleWitness.rowLink)
    val metricChanged = right(GlobalDecompositions.cca(rows.evidence, rows.evidence, left.evidence, rightAxis.evidence,
      x, y, doubled, link, link, components, right(CcaRegularization.symmetric(.1)), 1.0,
      RelationAccess.OwnedReplay("x"), RelationAccess.OwnedReplay("y"), DecompositionBudget(1000L)))
    assertEqualsDouble(metricChanged.fit.result.singularValues(0), math.sqrt((33.2 + math.sqrt(.8)) / (17.21 * 4.1)), 1e-10)
    assertNotEquals(metricChanged.evidenceIdentity, cca.evidenceIdentity)
    assertEquals(metricChanged.geometry.sourceMarginal.valueIdentity, doubled.valueIdentity)
    assertEquals(metricChanged.geometry.relationship.valueIdentity, link.valueIdentity)

  test("paired artifacts own their cross form after scoped input providers expire"):
    val rows = axis("scoped-paired-rows", SpaceRole.Samples, 4)
    val left = axis("scoped-paired-left", SpaceRole.Observed, 2)
    val target = axis("scoped-paired-right", SpaceRole.Observed, 2)
    val scope = new ScopeReplay("paired-fixture", "v1", "exact-paired-values")
    val x = new Counted(Some(DMat.dense(4, 2, Vector(1, 0, 0, 1, -1, 0, 0, -1))))
    val y = new Counted(Some(DMat.dense(4, 2, Vector(2, 1, 0, -1, -2, -1, 0, 1))))
    def table(axis: AxisRef[String], counted: Counted, id: String) =
      val guarded = new DoubleLinearOperator:
        val rows = counted.rows
        val cols = counted.cols
        def applyTo(input: DVec, output: MutableDVec): Unit =
          require(scope.isActive)
          counted.applyTo(input, output)
        override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
          require(scope.isActive)
          counted.transposeApplyTo(input, output)
      Op.fromLin(right(Lin.fromLinearMap(guarded, CoordinateEvidence.dual(axis.evidence), CoordinateEvidence.primal(rows.evidence),
        ValueIdentity.source(ValueId.unsafe(id)))), OperatorRoleWitness.table)
    val link = Op.fromLin(right(Lin.fromDenseMatrix(DMat.eye(4), CoordinateEvidence.primal(rows.evidence), CoordinateEvidence.dual(rows.evidence),
      ValueIdentity.source(ValueId.unsafe("scoped-paired-link")))), OperatorRoleWitness.rowLink)
    val first = table(left, x, "scoped-x")
    val second = table(target, y, "scoped-y")
    val access = RelationAccess.ScopedReplay(scope)
    val plsc = right(GlobalDecompositions.plsc(rows.evidence, rows.evidence, left.evidence, target.evidence,
      first, second, link, link, link, access, access, right(ComponentCount(1)), 1.0, DecompositionBudget(1000L)))
    val cca = right(GlobalDecompositions.cca(rows.evidence, rows.evidence, left.evidence, target.evidence,
      first, second, link, link, link, right(ComponentCount(1)), right(CcaRegularization.symmetric(.1)), 1.0, access, access, DecompositionBudget(1000L)))
    val before = x.reads + y.reads
    scope.expire()
    Vector(plsc, cca).foreach: artifact =>
      val cross = right(artifact.fit.cross.toDense)
      assertEqualsDouble(cross(0, 0), 4.0, 1e-12)
      assertEqualsDouble(cross(0, 1), 2.0, 1e-12)
      assertEqualsDouble(cross(1, 0), 0.0, 1e-12)
      assertEqualsDouble(cross(1, 1), -2.0, 1e-12)
      right(artifact.fit.sourceWeights)
      right(artifact.fit.targetWeights)
    assertEquals(x.reads + y.reads, before)
    assert(GlobalDecompositions.plsc(rows.evidence, rows.evidence, left.evidence, target.evidence,
      first, second, link, link, link, access, access, right(ComponentCount(1)), 1.0, DecompositionBudget(1000L)).isLeft)
    assertEquals(x.reads + y.reads, before)
