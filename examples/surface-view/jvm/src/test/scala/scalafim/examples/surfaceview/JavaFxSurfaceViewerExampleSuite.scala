package scalafim.examples.surfaceview

class JavaFxSurfaceViewerExampleSuite extends munit.FunSuite:
  test("production JVM GIFTI decoding matches the portable JVM/Scala.js fixture exactly"):
    val decoded = JavaFxSurfaceViewerExample.readGifti()
    val portable = SurfaceViewerExample.portableGiftiGeometry
    assertEquals(decoded.hemisphere, portable.hemisphere)
    assertEquals(decoded.kind, portable.kind)
    assert(decoded.mesh.hasSameTopology(portable.mesh))
    assertEquals(decoded.mesh.vertices, portable.mesh.vertices)
    assertEquals(decoded.surfaceToWorld, portable.surfaceToWorld)
    assertEquals(
      SurfaceViewerExample.semanticReceipt(SurfaceViewerExample.fromGifti(decoded).toOption.get),
      SurfaceViewerExample.semanticReceipt()
    )
