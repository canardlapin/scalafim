package scalafim.transform.x5

import scalafim.transform.oracle.OracleFixtures

/** jHDF decoding of X5 files must equal the h5py dumps the shared interpretation tests use. */
class X5ContainerSuite extends munit.FunSuite:
  test("jHDF reads the same nodes and chains as h5py"):
    Vector("linear", "displacements", "deformations", "chain").foreach: name =>
      val fromJhdf = X5Container.read(OracleFixtures.bytes(s"x5/$name.x5")).fold(e => fail(e.message), identity)
      assertEquals(fromJhdf, X5Dumps.parse(OracleFixtures.text(s"x5/$name.nodes.txt")), name)

  test("ITK HDF5 files are not X5"):
    assert(X5Container.read(OracleFixtures.bytes("neurotransform/itk_oracle/affine.h5")).isLeft)
