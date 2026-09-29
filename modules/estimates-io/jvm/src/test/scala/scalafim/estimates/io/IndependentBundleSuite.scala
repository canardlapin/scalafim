package scalafim.estimates.io

import java.nio.file.Files
import java.security.MessageDigest
import scalafim.archive.ContentDigest
import scalafim.estimates.*

/** Complete JSON, TSV and NIfTI bundles generated with Python's standard
  * library, independently of the ScalaFIM and image4s writers.
  */
class IndependentBundleSuite extends munit.FunSuite:
  private val revision = UnitRevisionId("00000000-0000-4000-8000-000000000054")
  private val unitId = UnitId("00000000-0000-4000-8000-000000000053")
  private val observation = ObservationId("row")
  private val a = EstimandId("A")
  private val b = EstimandId("B")
  private val prefix = s"units/${revision.value}"

  private def right[A](value: Either[EstimateError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes)
      .iterator.map(byte => f"${byte & 0xff}%02x").mkString

  private def readResource(name: String): Array[Byte] =
    val input = Option(getClass.getResourceAsStream(s"/estimate-golden/bundles/$name"))
      .getOrElse(fail(s"missing independently generated fixture $name"))
    try input.readAllBytes()
    finally input.close()

  private def open(caseName: String, manifestDigest: String): EstimateSource =
    val root = Files.createTempDirectory(s"scalafim-independent-$caseName-")
    val names = Vector(s"$prefix/estimands.json", s"$prefix/estimands.tsv",
      s"$prefix/observations.tsv", s"$prefix/values.nii", s"$prefix/validity.nii",
      s"$prefix/estimates.json") ++
      (if caseName == "deficient-rank" then Vector("evidence/estimable-subspace.tsv") else Vector.empty)
    names.foreach: name =>
      val path = root.resolve(name)
      Files.createDirectories(path.getParent)
      Files.write(path, readResource(s"$caseName/$name"))
    val manifest = Files.readAllBytes(root.resolve(s"$prefix/estimates.json"))
    assertEquals(sha256(manifest), manifestDigest)
    val reference = PinnedUnit(unitId, revision,
      FileReference(s"$prefix/estimates.json", ContentDigest.unsafeSha256(manifestDigest), manifest.length.toLong))
    right(LocalEstimateStore.open(root)).open(reference, ReadLimits(4)) match
      case Right(source) => source
      case Left(error) => fail(s"$caseName: ${error.message}")

  for (caseName, digest, product, values, codes) <- Vector(
    ("effects-only", "96b0b37d4015c75dfcade06331f7db81fb05991297e8206ffb912f59fa8820e3",
      ProductId("effect"), Vector(2.0, 4.0, 6.0, 8.0), Vector[Byte](0, 3, 0, 0)),
    ("statistic-only", "2e98bb4a8ebab81c6e6124d50d91caaaafc3519c6f31269a54cfb417db348913",
      ProductId("z-statistic"), Vector(1.0, 2.0, 3.0, 4.0), Vector[Byte](0, 0, 0, 0)),
    ("deficient-rank", "2839bf324554bc433b6d33bf369d5ffeb642debfb4219f18c6eb4cec95508621",
      ProductId("effect"), Vector(2.0, 4.0, 6.0, 8.0), Vector[Byte](0, 3, 0, 0))
  ) do
    test(s"independent complete $caseName bundle reopens with ordered scientific axes"):
      val source = open(caseName, digest)
      try
        val actual = new Array[Double](4)
        val validity = new Array[Byte](4)
        right(source.read(product, EstimateSelection(Vector(observation), Vector(a, b), Vector(0, 1)), actual, validity))
        assertEquals(actual.toVector, values)
        assertEquals(validity.toVector, codes)
        assertEquals(source.unit.products.map(_.id), Vector(product))
        if caseName == "statistic-only" then
          assertEquals(source.unit.statistics.size, 1)
          assertEquals(source.unit.statistics.head.effect, None)
          assertEquals(source.unit.statistics.head.standardError, None)
        if caseName == "deficient-rank" then
          assert(source.unit.estimability.isInstanceOf[EstimabilityEvidence.Subspace])
      finally right(source.close())
