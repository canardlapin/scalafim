package external.consumer

import scala.compiletime.testing.typeCheckErrors

class SamplingGeometryBoundaryCompileSuite extends munit.FunSuite:
  test("external consumers cannot bypass checked provider volume-geometry admission"):
    val imports = typeCheckErrors("""
      import neuroim4s.spatial.VolumeGrid
      import neuroim4s.image.SampleSpaces
      import scalafim.spatial.SamplingGeometry
      val admitted = VolumeGrid.from(SampleSpaces(Vector(2, 2, 1))).toOption.get
      val space = admitted.space
      val mask = admitted.mask
      val mixed = SamplingGeometry.Volume(admitted)
    """)
    assertEquals(imports, Nil)
    val applyErrors = typeCheckErrors("""
      import neuroim4s.spatial.VolumeGrid
      import neuroim4s.image.SampleSpaces
      val admitted = VolumeGrid.from(SampleSpaces(Vector(2, 2, 1))).toOption.get
      VolumeGrid(admitted.space, admitted.mask)
    """)
    val constructorErrors = typeCheckErrors("""
      import neuroim4s.spatial.VolumeGrid
      import neuroim4s.image.SampleSpaces
      val admitted = VolumeGrid.from(SampleSpaces(Vector(2, 2, 1))).toOption.get
      new VolumeGrid(admitted.space, admitted.mask)
    """)
    assert(applyErrors.nonEmpty)
    assert(constructorErrors.nonEmpty)
