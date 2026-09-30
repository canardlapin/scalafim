package scalafim.archive.hdf5

class Hdf5PlatformSuite extends munit.FunSuite:
  test("Scala.js refuses the JVM-native capability"):
    assert(JvmHdf5JniAdapter.open().left.toOption.exists(_.isInstanceOf[Hdf5Error.MissingCapability]))
