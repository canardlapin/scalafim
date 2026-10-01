package scalafim.surface.reference

/** Exact asset identity is checked independently of anatomical registration.
  * The historical GM-placement experiment is not registration proof.
  */
class RealFrameEvidenceSuite extends munit.FunSuite, RealAssetGate:
  test("declared Conte69 anatomy cannot silently obtain an unqualified inverse bridge"):
    requireReal(RealAssets.evidencePresent, s"locked assets under ${RealAssets.root}")
    assert(RealAssets.fslrBasis.display.contains("unqualified"))
    val inverse = FrameBridge.displacement(RealAssets.pointMap,
      PointMapUse.Inverse(InversePolicy.make(1e-6, 50).toOption.get))
    assert(inverse.left.exists(_.message.contains("pointwise inverse")))
    assertEquals(RealAssets.hemispheres.map(_.reference.vertexCount), Vector(32492, 32492))
