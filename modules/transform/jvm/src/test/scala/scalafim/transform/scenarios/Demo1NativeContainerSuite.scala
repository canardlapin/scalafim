package scalafim.transform.scenarios

import java.nio.file.Paths
import scalafim.transform.{NativeTransform, TransformFiles}
import scalafim.transform.itk.{ItkHdf5Container, ItkHdf5Dumps}
import scalafim.transform.oracle.OracleFixtures

/** Default offline JVM gate: committed native containers bind the shared dumps. */
class Demo1NativeContainerSuite extends munit.FunSuite:
  import Demo1NativeContract.*

  test("jHDF and the public loader bind both real-field crops to their complete shared dumps"):
    Vector("t1_to_template", "template_to_t1").foreach: name =>
      val path = s"$Root/$name.h5"
      val decoded = checked(ItkHdf5Container.read(OracleFixtures.bytes(path)))
      val dumped = ItkHdf5Dumps.parse(OracleFixtures.text(s"$Root/$name.components.txt"))
      assertEquals(decoded, dumped, name)
      val resource = Paths.get(getClass.getResource(s"/${OracleFixtures.Root}/$path").toURI)
      val loaded = checked(TransformFiles.load(resource))
      assertEquals(loaded.native, NativeTransform.ItkHdf5(dumped))
      assertEquals(loaded.asset.sha256, Some(OracleFixtures.sha256Hex(path)))

  test("explicit original-container qualification refuses invocation without its real assets"):
    val error = intercept[IllegalArgumentException](Demo1OriginalContainerQualification.main(Array.empty))
    assert(error.getMessage.contains("original forward and inverse HDF5 paths"))
