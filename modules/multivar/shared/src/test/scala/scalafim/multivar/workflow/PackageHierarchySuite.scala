package scalafim.multivar
package workflow

import scala.compiletime.testing.typeCheckErrors

class PackageHierarchySuite extends munit.FunSuite:

  test("public concepts resolve from their semantic owners"):
    val errors = typeCheckErrors("""
      val matrix = null.asInstanceOf[scalafim.multivar.core.MatrixView]
      val contract = null.asInstanceOf[scalafim.multivar.contract.MathematicalModelContract]
      val program = null.asInstanceOf[scalafim.multivar.optimization.OperatorProgram]
      val evidence = null.asInstanceOf[scalafim.multivar.solver.VariationalFrameCertificate]
      val fitted = null.asInstanceOf[
        scalafim.multivar.lifecycle.FittedModel[?, ?, ?]
      ]
      val projection = null.asInstanceOf[scalafim.multivar.capability.FittedFrameTransform]
      val glrm = scalafim.multivar.family.glrm.GeneralizedLowRankProgram
      val multiblock = scalafim.multivar.family.multiblock.ExactMultiblockPrograms
      val spec = scalafim.multivar.workflow.ModelSpec
    """)

    assertEquals(errors, List.empty)

  test("the former flat namespace cannot silently regrow"):
    val errors = typeCheckErrors("""
      val matrix = null.asInstanceOf[scalafim.multivar.MatrixView]
      val contract = null.asInstanceOf[scalafim.multivar.MathematicalModelContract]
      val program = null.asInstanceOf[scalafim.multivar.OperatorProgram]
      val fitted = null.asInstanceOf[scalafim.multivar.FittedModel[?, ?, ?]]
      val glrm = scalafim.multivar.GeneralizedLowRankProgram
      val spec = scalafim.multivar.ModelSpec
    """)

    assert(errors.nonEmpty)

  test("family composition is named at the owning vertical boundary"):
    val obsoleteMultiblockEntry = typeCheckErrors("""
      scalafim.multivar.family.spectral.ExactSpectralPrograms.multisetQuadratic
    """)
    val pairedTransferInGenericCapability = typeCheckErrors("""
      val transfer = scalafim.multivar.capability.PairedTransfer
    """)
    val currentOwners = typeCheckErrors("""
      val multiblock = scalafim.multivar.family.multiblock.ExactMultiblockPrograms
      val transfer = scalafim.multivar.family.paired.PairedTransfer
    """)

    assert(obsoleteMultiblockEntry.nonEmpty)
    assert(pairedTransferInGenericCapability.nonEmpty)
    assertEquals(currentOwners, List.empty)
