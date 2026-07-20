package scalafim.fmri.fit

import scalafim.fmri.model.{
  ArOptions,
  ArStructure,
  FitConfig,
  MissingDataPolicy,
  NuisanceProjection,
  RobustOptions,
  RobustPsi,
  VolumeWeighting
}
import scalafim.linalg.DoubleMatrix

class ResponsePreparationSuite extends munit.FunSuite:

  test("default plan is inspectable and preserves block input identity") {
    val block = blockInput()
    val plan = ResponsePreparationPlan.fromConfig(FitConfig())
    val prepared = plan.prepare(block).toOption.get

    assertEquals(prepared.input.design.value.toRows, block.design.value.toRows)
    assertEquals(prepared.input.response.value.toRows, block.response.value.toRows)
    assertEquals(prepared.input.voxelIndices, block.voxelIndices)
    assertEquals(prepared.input.timepoints, block.timepoints)
    assertEquals(prepared.provenance.records.length, 6)
    assertEquals(prepared.provenance.deferred, Vector.empty)
    assert(prepared.provenance.applied.exists(_.step == ResponsePreparationStep.MissingData(MissingDataPolicy.Error)))
  }

  test("non-default plan records deferred preparation surfaces") {
    val config = FitConfig(
      robust = RobustOptions(psi = RobustPsi.Huber()),
      autocorrelation = ArOptions(
        structure = ArStructure.Ar(1),
        censoredTimepoints = Vector(1),
        rho = Some(0.2)
      ),
      volumeWeighting = VolumeWeighting.Fixed(Vector(1.0, 0.9, 1.1)),
      nuisanceProjection = NuisanceProjection.MatrixProjection(
        DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(0.0), Vector(1.0)))
      ),
      missingData = MissingDataPolicy.Propagate
    )

    val provenance = ResponsePreparationPlan.fromConfig(config).provenance

    assertEquals(provenance.records.length, 6)
    assertEquals(provenance.deferred.map(_.step).toSet, provenance.records.map(_.step).toSet)
  }

  test("fixed volume weights must align with selected timepoints") {
    val plan = ResponsePreparationPlan.fromConfig(
      FitConfig(volumeWeighting = VolumeWeighting.Fixed(Vector(1.0, 1.0)))
    )

    val result = plan.prepare(blockInput())

    assertEquals(
      result.left.toOption,
      Some(FitError.InvalidFitAxis("fixed volume weights", "length 2 does not match selected timepoints 3"))
    )
  }

  test("nuisance projection rows must align with selected timepoints") {
    val plan = ResponsePreparationPlan.fromConfig(
      FitConfig(
        nuisanceProjection = NuisanceProjection.MatrixProjection(
          DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(0.0)))
        )
      )
    )

    val result = plan.prepare(blockInput())

    assertEquals(
      result.left.toOption,
      Some(FitError.InvalidFitAxis("nuisance projection", "matrix rows 2 do not match selected timepoints 3"))
    )
  }

  private def blockInput(): FitBlockInput =
    FitBlockInput(
      design = DesignMatrix.unsafe(
        DoubleMatrix.fromRows(
          Vector(
            Vector(1.0, 0.0),
            Vector(1.0, 1.0),
            Vector(1.0, 2.0)
          )
        )
      ),
      response = ResponseBlock.unsafe(
        DoubleMatrix.fromRows(
          Vector(
            Vector(1.0, 2.0),
            Vector(3.0, 4.0),
            Vector(5.0, 6.0)
          )
        )
      ),
      voxelIndices = Vector(10, 11),
      timepoints = Vector(0, 1, 2)
    )
