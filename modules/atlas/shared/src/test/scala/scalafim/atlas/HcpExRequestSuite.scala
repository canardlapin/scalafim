package scalafim.atlas

class HcpExRequestSuite extends munit.FunSuite:
  test("HCPex request preserves native resolution-specific immutable assets"):
    val one = HcpExRequest()
    val two = HcpExRequest(VoxelResolution.TwoMm)
    assertEquals(one.ref.templateSpace, SpaceId.MNI152NLin2009cAsym)
    assertEquals(one.volume.fileName, "HCPex.nii.gz")
    assertEquals(two.volume.fileName, "HCPex_2mm.nii")
    assertEquals(one.labels.revision, HcpExRequest.revision)
    assertEquals(one.lut.sha256.length, 64)
