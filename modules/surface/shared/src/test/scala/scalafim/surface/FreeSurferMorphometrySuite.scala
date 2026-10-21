package scalafim.surface

import scalafim.surface.freesurfer.FreeSurferMorphometryCodec

class FreeSurferMorphometrySuite extends munit.FunSuite:
  // Mathematical triangle expressed in MNI world millimeters.
  private val geometry = SurfaceGeometry(TriangleMesh.fromRows(
    Vector(Vector(-30.0, 0.0, 20.0), Vector(-29.0, 0.0, 20.0), Vector(-30.0, 1.0, 20.0)),
    Vector((0, 1, 2))), Hemisphere.Left, SurfaceKind.White)
  private val modern = Array(255,255,255, 0,0,0,3, 0,0,0,1, 0,0,0,1,
    192,32,0,0, 0,0,0,0, 63,160,0,0).map(_.toByte)
  private val legacy = Array(0,0,3, 0,0,1, 255,6, 0,0, 0,125).map(_.toByte)

  test("modern float32 and legacy hundredths agree with explicit scalar oracle"):
    for bytes <- Vector(modern, legacy) do
      val field = FreeSurferMorphometryCodec.decode(bytes, geometry, "sulc").toOption.get
      assertEquals(field.label, "sulc")
      assert(field.geometry eq geometry)
      assertEquals(field.vertexIds.map(_.index), Vector(0,1,2))
      assertEquals(field.data.toVector, Vector(-2.5, 0.0, 1.25))
      bytes(bytes.length - 1) = 99.toByte
      assertEqualsDouble(field.data(2), 1.25, 0.0)

  test("invalid headers counts components and payloads fail before allocation"):
    val base = Array(255,255,255, 0,0,0,3, 0,0,0,1, 0,0,0,1,
      192,32,0,0, 0,0,0,0, 63,160,0,0).map(_.toByte)
    def changed(index: Int, value: Int): Array[Byte] =
      val copy = base.clone(); copy(index) = value.toByte; copy
    for bytes <- Vector(Array.emptyByteArray, base.take(8), base.dropRight(1), base :+ 0.toByte,
        changed(6,4), changed(10,2), changed(14,2), changed(3,127), changed(7,255)) do
      assert(FreeSurferMorphometryCodec.decode(bytes, geometry).isLeft)
    val zeroFaces = changed(10,0)
    assert(FreeSurferMorphometryCodec.decode(zeroFaces, geometry).isRight)

  test("nonfinite scalars are retained as data rather than converted to zero"):
    val bytes = Array(255,255,255, 0,0,0,3, 0,0,0,1, 0,0,0,1,
      127,192,0,0, 127,128,0,0, 255,128,0,0).map(_.toByte)
    val values = FreeSurferMorphometryCodec.decode(bytes, geometry).toOption.get.data
    assert(values(0).isNaN)
    assertEquals(values(1), Double.PositiveInfinity)
    assertEquals(values(2), Double.NegativeInfinity)
