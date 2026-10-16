package scalafim.image

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Files
import image4s.geometry.{ContinuousIndex, Grid}
import image4s.geometry.D3
import scalafim.image.io.Nifti
import scalafim.image.world.{SpaceEvidence, WorldSpace}

class NiftiUnresolvedDisplaySuite extends munit.FunSuite:
  test("standalone reads preserve samples and affine but never imply registration"):
    val bytes = ByteBuffer.allocate(352 + 24 * 4).order(ByteOrder.LITTLE_ENDIAN)
    bytes.putInt(0, 348)
    Vector(3, 2, 3, 4, 1, 1, 1, 1).zipWithIndex.foreach((v, i) => bytes.putShort(40 + 2 * i, v.toShort))
    bytes.putShort(70, 16.toShort)
    bytes.putShort(72, 32.toShort)
    Vector(1f, 2f, 3f, 4f, 1f, 1f, 1f, 1f).zipWithIndex.foreach((v, i) => bytes.putFloat(76 + 4 * i, v))
    bytes.putFloat(108, 352f)
    bytes.putFloat(112, 2f)
    bytes.putFloat(116, -1f)
    bytes.put(123, 2.toByte)
    bytes.putShort(254, 2.toShort)
    Vector(0f, -3f, 0f, 11f, 2f, 0f, 0f, -7f, 0f, 0f, 4f, 5f)
      .zipWithIndex.foreach((v, i) => bytes.putFloat(280 + 4 * i, v))
    "n+1\u0000".zipWithIndex.foreach((v, i) => bytes.put(344 + i, v.toByte))
    (0 until 24).foreach(i => bytes.putFloat(352 + 4 * i, i.toFloat))
    val path = Files.createTempFile("unresolved-display", ".nii")
    try
      Files.write(path, bytes.array())
      val a = Nifti.readUnresolvedVolume(path).fold(e => fail(e.message), identity)
      val b = Nifti.readUnresolvedVolume(path).fold(e => fail(e.message), identity)
      assert(SampleSpaces.worldOf(a.image.space).toOption.exists(_.isInstanceOf[WorldSpace.Unresolved]))
      assertNotEquals(SampleSpaces.worldOf(a.image.space), SampleSpaces.worldOf(b.image.space))
      assert(Grid.exactCongruence(a.image.grid, b.image.grid).isLeft)
      assertEqualsDouble(a.image(1, 0, 0), 1.0, 0.0)
      assertEqualsDouble(a.image(0, 0, 1), 11.0, 0.0)
      val point = a.image.grid.pointAt(ContinuousIndex.fromVector[D3](Vector(1.0, 2.0, 3.0)).toOption.get)
        .fold(e => fail(e.message), identity)
      assertEquals(point.coordinates, Vector(5.0, -5.0, 17.0))
      assert(Nifti.readVolume(path, SpaceEvidence()).isLeft)
      assert(Nifti.readUnresolvedVolume(path, Nifti.ReadOptions.default.copy(
        affinePolicy = image4s.nifti.NiftiAffinePolicy.RequireAgreement(-1.0))).isLeft)
    finally Files.deleteIfExists(path)
