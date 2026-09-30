package scalafim.fmri.model

class VoxelwiseReducedRankConfigSuite extends munit.FunSuite:
  test("voxelwise rank solver controls and confidence levels reject invalid scientific settings") {
    assert(ReducedRankSolverConfig(maxIterations = 0).isLeft)
    Vector(0.0, -1.0, Double.NaN, Double.PositiveInfinity).foreach { tolerance =>
      assert(ReducedRankSolverConfig(tolerance = tolerance).isLeft)
    }
    Vector(0.0, 1.0, Double.NaN, Double.PositiveInfinity).foreach { level =>
      assert(VoxelwiseReducedRankBootstrapConfig(confidenceLevel = level).isLeft)
    }
    val base = ReducedRankGlsConfig.unsafe(inference = ReducedRankInferencePolicy.EstimatesOnly)
    val changed = ReducedRankGlsConfig.unsafe(inference = ReducedRankInferencePolicy.EstimatesOnly,
      solver = ReducedRankSolverConfig.unsafe(maxIterations = 10))
    assertNotEquals(base, changed)
    assertEquals(base, ReducedRankGlsConfig.unsafe(inference = ReducedRankInferencePolicy.EstimatesOnly))
  }

  test("explicit voxelwise bootstrap preserves mode and validates block length") {
    VoxelwiseBootstrapMode.values.foreach { mode =>
      val config = VoxelwiseReducedRankBootstrapConfig.unsafe(
        resampling = ReducedRankBootstrapConfig.unsafe(replicates = 20, blockSize = 4, seed = 17),
        mode = mode
      )
      val policy = ReducedRankInferencePolicy.VoxelwiseBootstrap(config)
      assert(policy.validateFor(3).isLeft)
      assertEquals(policy.validateFor(4), Right(()))
      assertEquals(config.mode, mode)
    }
  }

  test("voxelwise bootstrap contrasts require a stable non-empty definition") {
    assert(VoxelwiseBootstrapContrast("", Vector(0 -> 1.0)).isLeft)
    assert(VoxelwiseBootstrapContrast("x", Vector.empty).isLeft)
    assert(VoxelwiseBootstrapContrast("x", Vector(-1 -> 1.0)).isLeft)
    assert(VoxelwiseBootstrapContrast("x", Vector(0 -> Double.NaN)).isLeft)
    assert(VoxelwiseBootstrapContrast("x", Vector(0 -> 1.0, 0 -> -1.0)).isLeft)
    assert(VoxelwiseBootstrapContrast("x", Vector(0 -> 0.0)).isLeft)
    val contrast = VoxelwiseBootstrapContrast.unsafe("difference", Vector(2 -> 1.0, 0 -> -1.0))
    assertEquals(contrast.weights, Vector(2 -> 1.0, 0 -> -1.0))
    assert(VoxelwiseReducedRankBootstrapConfig(contrasts = Vector(contrast, contrast)).isLeft)
  }
