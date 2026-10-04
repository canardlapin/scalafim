package scalafim.surface.reference

/** Exact asset identity is checked independently of anatomical registration.
  * The historical GM-placement experiment is not registration proof.
  */
class RealFrameEvidenceSuite extends munit.FunSuite, RealAssetGate:
  test("Conte69 anatomy carries the publisher's affine group-average basis; the inverse bridge is approximate"):
    requireReal(RealAssets.evidencePresent, s"locked assets under ${RealAssets.root}")
    RealAssets.fslrBasis match
      case FrameBasis.PublisherMethods(doi, locator, registration, aggregate, quotation) =>
        assertEquals(doi, "10.1093/cercor/bhr291")
        assert(locator.contains("p. 2245"))
        assertEquals((registration, aggregate), (PublishedRegistration.Affine, PublishedAggregate.GroupAverage(69)))
        assert(quotation.contains("MNI152_T1_1mm.nii.gz"))
      case other => fail(s"expected publisher methods, got $other")
    assert(RealAssets.fslrBasis.display.contains("matches no single anatomy"))
    val inverse = FrameBridge.displacement(RealAssets.pointMap, PointMapUse.Inverse(InversePolicy.Default))
    assertEquals(inverse.map(_.exactness),
      Right(BridgeExactness.Approximate(NumericalBridgeMethod.PointwiseFixedPoint(InversePolicy.Default))))
    assertEquals(RealAssets.hemispheres.map(_.reference.vertexCount), Vector(32492, 32492))
