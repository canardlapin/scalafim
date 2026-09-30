package scalafim.transform.itk

import scalafim.transform.oracle.OracleFixtures

/** jHDF decoding must equal the h5py dumps the shared (both-platform) interpretation tests run on. */
class ItkHdf5ContainerSuite extends munit.FunSuite:
  test("jHDF reads the same components as h5py for every HDF5 oracle file"):
    Vector("affine", "affine_warp", "warp_affine").foreach: name =>
      val fromJhdf = ItkHdf5Container.read(OracleFixtures.bytes(s"neurotransform/itk_oracle/$name.h5")).fold(e => fail(e.message), identity)
      val fromDump = ItkHdf5Dumps.parse(OracleFixtures.text(s"itk_hdf5/$name.components.txt"))
      assertEquals(fromJhdf, fromDump, name)

  test("non-HDF5 bytes are a typed failure"):
    assert(ItkHdf5Container.read(Array.fill[Byte](64)(1)).isLeft)
