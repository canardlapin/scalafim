package scalafim.transform.itk

import scalafim.transform.field.VectorFieldNiftiCodec
import scalafim.transform.{NativeTransform, TransformFiles, TransformSource}
import scalafim.transform.oracle.OracleFixtures
import java.nio.file.Paths

/** Tie the shared ANTs oracle decodes to the exact native NIfTI/HDF5 container bytes. */
class AntsNativeContainerSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A])(using munit.Location): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  test("jHDF reads native Composite and InverseComposite exactly as the shared h5py dumps"):
    Vector("syn_Composite", "syn_InverseComposite").foreach: key =>
      val native = ok(ItkHdf5Container.read(OracleFixtures.bytes(s"ants_native/$key.h5")))
      val shared = ItkHdf5Dumps.parse(OracleFixtures.text(s"ants_native/$key.components.txt"))
      assertEquals(native, shared, key)

  test("native 5D fields load through the public container reader with unchanged values and provenance"):
    Vector("syn_0Warp", "syn_0InverseWarp").foreach: key =>
      val resource = s"ants_native/$key.nii.gz"
      val path = Paths.get(getClass.getResource(s"/${OracleFixtures.Root}/$resource").toURI)
      val shared = ok(VectorFieldNiftiCodec.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded(resource)))))
      val loaded = ok(TransformFiles.load(path))
      loaded.native match
        case NativeTransform.AntsField(native) =>
          assertEquals(native.raw.shape, shared.raw.shape, key)
          assertEquals(native.raw.intentCode, shared.raw.intentCode, key)
          assertEquals(native.raw.sformRowMajor, shared.raw.sformRowMajor, key)
          assertEquals(native.raw.qformRowMajor, shared.raw.qformRowMajor, key)
          assertEquals(native.raw.voxelCount, shared.raw.voxelCount, key)
          var i = 0L
          while i < shared.raw.voxelCount do
            assertEqualsDouble(native.raw.value(i), shared.raw.value(i), 0.0, s"$key value $i")
            i += 1
        case other => fail(s"$key loaded as ${other.format}")
      assertEquals(loaded.asset.sha256, Some(OracleFixtures.sha256Hex(resource)))
