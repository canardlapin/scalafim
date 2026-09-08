package scalafim.image.io

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Files
import scalafim.image.*

class NiftiWriteCoordinatesSuite extends munit.FunSuite:
  private val options = NiftiWriteOptions(NiftiCoordinateSystem.Mni152, NiftiSpatialUnits.Millimeters)
  // Crop beginning at template voxel (40,60,40) of the ds000001 2mm MNI grid.
  private val space = NeuroSpace(Vector(3, 1, 1), spacing = Some(Vector(2.0, 2.0, 2.0)),
    origin = Some(Vector(-16.5, -12.5, 1.5)))

  test("explicit MNI volume preserves crop affine and values with declared units") {
    val path = Files.createTempFile("nifti-mni-crop", ".nii")
    try
      val volume = NeuroVol.fromLinear(Array(1.0, 2.0, 3.0), space)
      Nifti.writeVol(path, volume, options)
      val bytes = ByteBuffer.wrap(Files.readAllBytes(path)).order(ByteOrder.LITTLE_ENDIAN)
      assertEquals(bytes.getShort(252).toInt, 0)
      assertEquals(bytes.getShort(254).toInt, 4)
      assertEquals(bytes.get(123).toInt, 2)
      assertEqualsDouble(bytes.getFloat(280).toDouble, 2.0, 1e-12)
      assertEqualsDouble(bytes.getFloat(292).toDouble, -16.5, 1e-12)
      assertEqualsDouble(bytes.getFloat(308).toDouble, -12.5, 1e-12)
      assertEqualsDouble(bytes.getFloat(324).toDouble, 1.5, 1e-12)
      assertEquals(Nifti.readVol(path).copyLegacyLinear.toVector, Vector(1.0, 2.0, 3.0))
      assertEquals(Nifti.readHeader(path).space, space)
    finally Files.deleteIfExists(path)
  }

  test("4D writer accepts explicit coordinates and historical defaults remain unchanged") {
    val path = Files.createTempFile("nifti-mni-vector", ".nii")
    try
      val vec = NeuroVec.fromLinear(Array.tabulate(6)(_.toDouble), space.addDim(2, Some(Axis.Time)))
      Nifti.writeVec(path, vec, options)
      assertEquals(Nifti.readHeader(path).sformCode, 4)
      assertEquals(Nifti.readVec(path).copyLegacyLinear.toVector, Vector.tabulate(6)(_.toDouble))
      Nifti.writeVec(path, vec)
      val bytes = ByteBuffer.wrap(Files.readAllBytes(path)).order(ByteOrder.LITTLE_ENDIAN)
      assertEquals(bytes.getShort(254).toInt, 1)
      assertEquals(bytes.get(123).toInt, 0)
    finally Files.deleteIfExists(path)
  }
