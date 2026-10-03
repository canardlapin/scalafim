package scalafim.fmri.mvpa.relation

import scala.compiletime.testing.typeCheckErrors

class RelationsSuite extends munit.FunSuite:
  test("residual methods require supplied residual moment precision and df capabilities"):
    val errors = typeCheckErrors("""
import multivar.core.SemanticSpace
import scalafim.fmri.mvpa.relation.*
def invalid[A, E <: SemanticSpace, N <: SemanticSpace](
  relation: Relation[E, N], source: RelationSource, value: A
)(using Estimability[A]) = relation.bindResidual(source, value)
""")
    assert(errors.nonEmpty)

  test("replay provenance must name its reader owner"):
    intercept[IllegalArgumentException]:
      RelationOrigins(RelationSource("acquisition", "response", "readout", "preparation", "noise"), RelationAccess.OwnedReplay(""))

  test("a restriction from a different identified effect axis cannot typecheck"):
    val errors = typeCheckErrors("""
import multivar.core.SemanticSpace
import resample4s.core.Reindexing
import scalafim.fmri.mvpa.ReindexingLeg
import scalafim.fmri.mvpa.relation.Relation
def invalid[E <: SemanticSpace, N <: SemanticSpace, Other <: SemanticSpace, K, R <: Reindexing](
  relation: Relation[E, N], foreign: ReindexingLeg[Other, K, R]
) = relation.restrict(foreign)
""")
    assert(errors.nonEmpty)
