package scalafim.estimates.io

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scalafim.archive.ContentDigest
import scalafim.estimates.*

/** The complete Core-1 JSON, TSV and NIfTI bytes come from a literal Python
  * standard-library generator, independent of either production encoder.
  */
class IndependentCoreBundleSuite extends munit.FunSuite:
  private val revision = UnitRevisionId("00000000-0000-4000-8000-000000000084")
  private val unitId = UnitId("00000000-0000-4000-8000-000000000083")
  private val prefix = s"units/${revision.value}"
  private val manifestName = s"$prefix/estimates.json"
  private val manifestHash = "e2d54370a163f98b44d670e9d87d46af1d1982b818a11190ad613bd53ad0706d"
  private val names = Vector("estimands.json", "estimands.tsv", "observations.tsv", "estimates.json",
    "effect-values.nii", "effect-validity.nii", "t-values.nii", "t-validity.nii").map(prefix + "/" + _)

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes)
      .iterator.map(byte => f"${byte & 0xff}%02x").mkString

  private def right[A](result: Either[EstimateError, A]): A = result.fold(e => fail(e.message), identity)

  private def copyBundle(): Path =
    val root = Files.createTempDirectory("scalafim-core-literal-")
    names.foreach: name =>
      val input = Option(getClass.getResourceAsStream(s"/estimate-golden/core-literal/$name"))
        .getOrElse(fail(s"missing independent Core fixture $name"))
      val bytes = try input.readAllBytes() finally input.close()
      val path = root.resolve(name)
      Files.createDirectories(path.getParent)
      Files.write(path, bytes)
    root

  private def pinned(root: Path): PinnedUnit =
    val bytes = Files.readAllBytes(root.resolve(manifestName))
    PinnedUnit(unitId, revision,
      FileReference(manifestName, ContentDigest.unsafeSha256(sha256(bytes)), bytes.length.toLong))

  private def rewriteManifest(root: Path)(edit: ujson.Value => Unit): Unit =
    val path = root.resolve(manifestName)
    val json = ujson.read(Files.readString(path, UTF_8))
    edit(json)
    Files.writeString(path, ujson.write(json, indent = 2) + "\n", UTF_8)

  test("independent complete Core-1 bundle binds known t mapping, df, axes, units and physical cells"):
    val root = copyBundle()
    assertEquals(sha256(Files.readAllBytes(root.resolve(manifestName))), manifestHash)
    val source = right(right(LocalEstimateStore.open(root)).open(pinned(root), ReadLimits(4)))
    try
      assertEquals(source.unit.catalog.entries.map(_.id.value), Vector("effect-A", "hypothesis-A"))
      assertEquals(source.unit.products.map(_.id.value), Vector("effect", "t-statistic"))
      assertEquals(source.unit.products.map(_.units), Vector("percent-signal", "dimensionless"))
      val semantics = source.unit.statistics.head
      assertEquals(semantics.product, ProductId("t-statistic"))
      assertEquals(semantics.distribution,
        ReferenceDistribution.StudentT(DegreesOfFreedom(DfRole.Reference, DfValue.Scalar(9.0),
          "literal nine residual degrees", false)))
      assertEquals(semantics.effect.map(_.correspondence), Some(StatisticCorrespondence.Known(
        Vector(HypothesisTarget(EstimandId("hypothesis-A"), Vector(EstimandId("effect-A")))))))
      val values = new Array[Double](2)
      val codes = new Array[Byte](2)
      val row = Vector(ObservationId("row-1"))
      right(source.read(ProductId("effect"), EstimateSelection(row, Vector(EstimandId("effect-A")), Vector(0, 1)), values, codes))
      assertEquals(values.toVector, Vector(2.0, 4.0))
      assertEquals(codes.toVector, Vector[Byte](0, 3))
      right(source.read(ProductId("t-statistic"), EstimateSelection(row, Vector(EstimandId("hypothesis-A")), Vector(0, 1)), values, codes))
      assertEquals(values.toVector, Vector(1.5, -2.0))
      assertEquals(codes.toVector, Vector[Byte](0, 0))
    finally right(source.close())

  test("literal Core-1 bundle refuses wrong wire version and missing product units"):
    val wrongVersion = copyBundle()
    rewriteManifest(wrongVersion)(json => json("WireVersion") = "2.0.0")
    assert(right(LocalEstimateStore.open(wrongVersion)).open(pinned(wrongVersion), ReadLimits(4)).isLeft)
    val missingUnits = copyBundle()
    rewriteManifest(missingUnits): json =>
      json("Content")("products")(0).obj.remove("units")
    assert(right(LocalEstimateStore.open(missingUnits)).open(pinned(missingUnits), ReadLimits(4)).isLeft)

  test("a newly digest-pinned TSV projection still must agree with authoritative JSON axes"):
    val root = copyBundle()
    val path = root.resolve(s"$prefix/estimands.tsv")
    val changed = Files.readString(path, UTF_8).replace("effect-A", "other-A").getBytes(UTF_8)
    Files.write(path, changed)
    rewriteManifest(root): json =>
      val reference = json("Content")("Tables")("estimands")
      reference("SHA256") = sha256(changed)
      reference("Bytes") = changed.length
    assert(right(LocalEstimateStore.open(root)).open(pinned(root), ReadLimits(4)).isLeft)

  test("malformed later gzip representation closes earlier pair handles and its raw input"):
    val root = copyBundle()
    val badName = s"$prefix/bad.nii.gz"
    val bytes = Array[Byte](0x1f, 0x8b.toByte, 8)
    Files.write(root.resolve(badName), bytes)
    rewriteManifest(root): json =>
      val reference = json("Content")("Representations")(1)("values")
      reference("Path") = badName
      reference("SHA256") = sha256(bytes)
      reference("Bytes") = bytes.length
    val fdDirectory = if Files.isDirectory(Path.of("/dev/fd")) then Path.of("/dev/fd") else Path.of("/proc/self/fd")
    def openDescriptors(): Long =
      val entries = Files.list(fdDirectory)
      try entries.count()
      finally entries.close()
    val before = openDescriptors()
    val store = right(LocalEstimateStore.open(root))
    for _ <- 0 until 32 do
      assert(store.open(pinned(root), ReadLimits(4)).isLeft)
    val after = openDescriptors()
    assert(after <= before + 8, s"partial representation opening leaked descriptors: before=$before after=$after")
