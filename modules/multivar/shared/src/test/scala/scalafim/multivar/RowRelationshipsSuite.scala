package scalafim.multivar

import scala.compiletime.testing.typeCheckErrors

import gale.linalg.DMat

class RowRelationshipsSuite extends munit.FunSuite:
  private def accepted[A](result: Either[AlignmentError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def acceptedSemantic[A](result: Either[SemanticError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def acceptedMv[A](result: Either[MultivarError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def ref(id: String, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(id), SpaceRole.Samples, Dimension.unsafe(dimension)))

  private def value(id: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(id))

  private def identityGeometry[S <: SemanticSpace](
      space: SpaceEvidence[S],
      id: String
  ): DiagramGeometry[S] =
    val legacy = acceptedMv(MetricSpec.identity(space.dimension, Some(space.descriptor)))
    val operator = acceptedSemantic(FormOperator.primal(legacy, space, value(id)))
    val certificate = acceptedSemantic(FormCertificates.spd(operator))
    DiagramGeometry.metric(acceptedSemantic(Form.metric(operator, space, certificate)))

  private def dense(view: MatrixView): DMat =
    acceptedMv(view.toDense(StoragePolicy.AllowDense))

  private def assertMatrix(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), 1e-12)
        col += 1
      row += 1

  test("exact, partial, and incidence relationships require named semantic conversions") {
    val source = ref("relationships.exact.source", 3)
    val target = ref("relationships.exact.target", 3)
    val exact = accepted(
      ExactBijection.fromPermutation(
        source.evidence,
        target.evidence,
        Vector(2, 0, 1),
        value("relationships.exact")
      )
    )

    assertEquals(exact.rowMap.descriptor.kind, RowRelationshipKind.ExactBijection)
    assertEquals(exact.toPartialInjection.rowMap.descriptor.kind, RowRelationshipKind.PartialInjection)
    assertEquals(
      exact.toPartialInjection.toIncidenceMap.rowMap.descriptor.kind,
      RowRelationshipKind.IncidenceMap
    )
    assertMatrix(
      dense(exact.rowMap.matrix),
      GaleNumerics.matrixFromRows(Vector(Vector(0.0, 1.0, 0.0), Vector(0.0, 0.0, 1.0), Vector(1.0, 0.0, 0.0)))
    )

    val errors = typeCheckErrors("""
      import scalafim.multivar.*
      def silentlyCoerce[A <: SemanticSpace, B <: SemanticSpace](
          relationship: ExactBijection[A, B]
      ): TypedRowLink[A, B] = relationship.rowMap
    """)
    assert(errors.nonEmpty)
  }

  test("partial injection records unmatched support and rejects duplicate targets") {
    val source = ref("relationships.partial.source", 4)
    val target = ref("relationships.partial.target", 3)
    val partial = accepted(
      PartialInjection.fromAssignments(
        source.evidence,
        target.evidence,
        Vector(Some(1), None, Some(2), None),
        value("relationships.partial")
      )
    )

    val support = partial.rowMap.descriptor.support
    assertEquals(support.matchedMass, 2.0)
    assertEquals(support.unmatchedSourceMass, 2.0)
    assertEquals(support.unmatchedTargetMass, 1.0)
    assert(!support.everySourceRepresented)
    assert(!support.everyTargetRepresented)
    assertEquals(partial.rowMap.matrix.storage, StorageKind.Sparse)
    assert(
      PartialInjection
        .fromAssignments(
          source.evidence,
          target.evidence,
          Vector(Some(1), Some(1), None, None),
          value("relationships.duplicate")
        )
        .left
        .exists(_.isInstanceOf[AlignmentError.DuplicateAssignment])
    )
  }

  test("equal row counts do not establish identity; verified and unsafe evidence remain visible") {
    val left = ref("relationships.same.left", 2)
    val right = ref("relationships.same.right", 2)
    val entities = MvSpace(SpaceId.unsafe("relationships.people"), SpaceRole.Samples, Dimension.unsafe(2))
    val keys = value("relationships.keys.v1")
    val verified = accepted(
      SameEntityEvidence.fromVerifiedIdentity(left.evidence, right.evidence, entities, keys)
    )
    val unsafe = accepted(
      Unsafe.assumeSameRows(left.evidence, right.evidence, entities, "legacy arrays were externally audited")
    )

    assertEquals(verified.exactIdentity.rowMap.descriptor.origin, AlignmentOrigin.ObservedKeys)
    assertEquals(unsafe.exactIdentity.rowMap.descriptor.origin, AlignmentOrigin.UnsafeAssumption)
    assert(unsafe.provenance.events.exists(_.isInstanceOf[SemanticProvenanceEvent.UnsafeAssumption]))
    assert(Unsafe.assumeSameRows(left.evidence, right.evidence, entities, "").isLeft)

    assertEquals(verified.entitySpace, entities)
    assertEquals(verified.exactIdentity.rowMap.descriptor.domain.size, 2)
    assertEquals(verified.exactIdentity.rowMap.descriptor.codomain.size, 2)
  }

  test("couplings preserve supplied marginals and never silently normalize") {
    val left = ref("relationships.coupling.left", 2)
    val right = ref("relationships.coupling.right", 3)
    val matrix = GaleNumerics.matrixFromRows(
      Vector(Vector(0.1, 0.2, 0.0), Vector(0.0, 0.3, 0.4))
    )
    val coupling = accepted(
      NonnegativeCoupling.fromMatrix(
        left.evidence,
        right.evidence,
        matrix,
        RelationshipNormalization.Unnormalized,
        value("relationships.coupling")
      )
    )

    assertEqualsDouble(coupling.marginals.left(0), 0.3, 1e-12)
    assertEqualsDouble(coupling.marginals.left(1), 0.7, 1e-12)
    assertEqualsDouble(coupling.marginals.right(0), 0.1, 1e-12)
    assertEqualsDouble(coupling.marginals.right(1), 0.5, 1e-12)
    assertEqualsDouble(coupling.marginals.right(2), 0.4, 1e-12)
    assertEqualsDouble(coupling.marginals.totalMass, 1.0, 1e-12)
    val probability = accepted(ProbabilisticCoupling.fromNonnegative(coupling))
    assertEquals(probability.rowLink.descriptor.normalization, RelationshipNormalization.UnitMass)

    val unnormalized = accepted(
      NonnegativeCoupling.fromMatrix(
        left.evidence,
        right.evidence,
        GaleNumerics.matrixFromRows(Vector(Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0))),
        RelationshipNormalization.Unnormalized,
        value("relationships.unnormalized")
      )
    )
    assert(ProbabilisticCoupling.fromNonnegative(unnormalized).isLeft)
    assertEqualsDouble(unnormalized.marginals.totalMass, 2.0, 1e-12)
  }

  test("signed links are not couplings and report absolute support mass") {
    val left = ref("relationships.signed.left", 2)
    val right = ref("relationships.signed.right", 2)
    val signed = accepted(
      SignedRowLink.fromMatrix(
        left.evidence,
        right.evidence,
        GaleNumerics.matrixFromRows(Vector(Vector(1.0, -1.0), Vector(0.0, 2.0))),
        value("relationships.signed")
      )
    )

    assertEquals(signed.rowLink.descriptor.kind, RowRelationshipKind.SignedRowLink)
    assertEquals(signed.rowLink.descriptor.marginals, None)
    assertEqualsDouble(signed.rowLink.descriptor.support.matchedMass, 4.0, 1e-12)
    assert(
      NonnegativeCoupling
        .fromMatrix(
          left.evidence,
          right.evidence,
          dense(signed.rowLink.matrix),
          RelationshipNormalization.Unnormalized,
          value("relationships.not-a-coupling")
        )
        .isLeft
    )
  }

  test("incidence maps lower to row links only through an explicit target form") {
    val observations = ref("relationships.incidence.observations", 3)
    val entities = ref("relationships.incidence.entities", 2)
    val incidence = accepted(
      IncidenceMap.fromEdges(
        observations.evidence,
        entities.evidence,
        Vector((0, 0), (1, 0), (2, 1)),
        value("relationships.incidence")
      )
    )
    val link = accepted(incidence.toRowLink(identityGeometry(entities.evidence, "relationships.entity.metric")))

    assertEquals(link.descriptor.kind, RowRelationshipKind.IncidenceInducedLink)
    assertEquals(link.operator.domain.descriptor.space, observations.descriptor)
    assertEquals(link.operator.codomain.descriptor.space, entities.descriptor)
    assertEquals(link.operator.codomain.descriptor.variance, CoordinateVariance.Dual)
  }

  test("hub alignment constructs adjoint pairwise links and ignores unmatched rows") {
    val left = ref("relationships.hub.left", 3)
    val right = ref("relationships.hub.right", 2)
    val entities = ref("relationships.hub.entities", 3)
    val leftMap = accepted(
      PartialInjection
        .fromAssignments(
          left.evidence,
          entities.evidence,
          Vector(Some(0), Some(1), None),
          value("relationships.hub.left-map")
        )
        .map(_.toIncidenceMap.rowMap)
    )
    val rightToEntity = accepted(
      IncidenceMap
        .fromEdges(
          right.evidence,
          entities.evidence,
          Vector((0, 0), (1, 1)),
          value("relationships.hub.right-map")
        )
        .map(_.rowMap)
    )
    val pair = accepted(
      HubAlignment.inducedPair(
        leftMap,
        identityGeometry(entities.evidence, "relationships.hub.metric"),
        rightToEntity
      )
    )
    assertEqualsDouble(pair.adjointCertificate.residual, 0.0, 0.0)
    assertEquals(pair.rightToLeft.operator.valueIdentity, pair.leftToRight.operator.valueIdentity.star)
    assertMatrix(
      dense(pair.leftToRight.matrix),
      GaleNumerics.matrixFromRows(Vector(Vector(1.0, 0.0), Vector(0.0, 1.0), Vector(0.0, 0.0)))
    )
    assertMatrix(
      dense(pair.rightToLeft.matrix),
      GaleNumerics.matrixFromRows(Vector(Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0)))
    )
  }

  test("identity hub alignment reduces exactly to the same-row form") {
    val left = ref("relationships.identity.left", 3)
    val right = ref("relationships.identity.right", 3)
    val entities = ref("relationships.identity.entities", 3)
    val leftMap = accepted(
      ExactBijection
        .fromPermutation(left.evidence, entities.evidence, Vector(0, 1, 2), value("relationships.identity.left-map"))
        .map(_.rowMap)
    )
    val rightMap = accepted(
      ExactBijection
        .fromPermutation(right.evidence, entities.evidence, Vector(0, 1, 2), value("relationships.identity.right-map"))
        .map(_.rowMap)
    )
    val link = accepted(
      HubAlignment.inducedLink(
        leftMap,
        identityGeometry(entities.evidence, "relationships.identity.metric"),
        rightMap
      )
    )

    assertMatrix(dense(link.matrix), DMat.eye(3))
  }

  test("entity-aligned studies carry hub and global PSD certificates with support reports") {
    val first = ref("relationships.study.first", 2)
    val second = ref("relationships.study.second", 2)
    val entities = ref("relationships.study.entities", 3)
    val firstMap = accepted(
      IncidenceMap
        .fromEdges(first.evidence, entities.evidence, Vector((0, 0), (1, 1)), value("relationships.study.first-map"))
        .map(_.rowMap)
    )
    val secondMap = accepted(
      IncidenceMap
        .fromEdges(second.evidence, entities.evidence, Vector((0, 1), (1, 2)), value("relationships.study.second-map"))
        .map(_.rowMap)
    )
    val study = accepted(
      EntityAlignedStudy.from(
        entities.evidence,
        identityGeometry(entities.evidence, "relationships.study.metric"),
        Vector(
          EntityMapEntry(BlockId.unsafe("first"), firstMap),
          EntityMapEntry(BlockId.unsafe("second"), secondMap)
        )
      )
    )

    assertEquals(study.report.entityCoverage, 3)
    assert(study.report.everyEntityRepresented)
    assertEquals(study.report.views.length, 2)
    assertEquals(study.hubCertificate.maps.length, 2)
    assert(study.globalBlockPsdCertificate.entityFormCertificates.nonEmpty)
    assert(study.globalBlockPsdCertificate.proof.contains("PSD"))
  }
