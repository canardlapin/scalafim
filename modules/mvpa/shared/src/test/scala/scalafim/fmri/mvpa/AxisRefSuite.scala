package scalafim.fmri.mvpa

import multivar.core.SpaceRole
import scala.compiletime.testing.typeCheckErrors

class AxisRefSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def axis(
      keys: Vector[String] = Vector("trial-91", "trial-7", "trial-32"),
      basis: String = "native",
      units: String = "trial",
      scale: String = "unscaled",
      lineage: Vector[String] = Vector("source:run-1")
  ): AxisRef[String] =
    right(
      AxisRef.fromStableKeys(
        "trials",
        SpaceRole.Samples,
        keys,
        basis,
        units,
        scale,
        lineage
      )
    )

  test("compact signatures are deterministic while complete records remain verified"):
    val first = axis()
    val restored = right(AxisRef.restore(first.toRecord))
    assertEquals(first.descriptor, restored.descriptor)
    assert(first.sameIdentityAs(restored))
    assertEquals(
      first.descriptor.coordinateSignature.value,
      "7e5c0746b7244c1a6239adcb9590adca455627847d8af57eb1195d97a5e8c745"
    )
    assertEquals(first.responseDomain.value, restored.responseDomain.value)
    assert(first.locus.sameRuntimeOwnerAs(restored.locus))
    assertEquals(first.toRecord.stableKeys, Vector("trial-91", "trial-7", "trial-32"))

    val point = right(first.locus.index(1))
    assertEquals(right(first.keyAtPoint(point)), "trial-7")
    assertEquals(first.ordinalOf("trial-32"), Some(2))
    assertEquals(first.keyAt(3), Left(EvidenceError.InvalidOrdinal(3, 3)))

  test("digest hexadecimal preserves independent SHA-256 framing and UTF-8 fixtures"):
    // Python hashlib over little-endian byte-length-prefixed UTF-8 fields;
    // the empty payload is the standard SHA-256 empty-message vector.
    assertEquals(AxisDigest.sha256Hex(_ => ()), "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    assertEquals(AxisDigest.sha256Hex(_.string("")), "df3f619804a92fdb4057192dc43dd748ea778adc52bc498ce80524c014b81119")
    assertEquals(AxisDigest.sha256Hex(writer => Vector("a" * 200, "café", "😀").foreach(writer.string)),
      "f90133bb1e6b9abd7a783a47e607a7135ce6fdcfe3214dac9fe8e0af00b140b9")

  test("ordered keys and complete scientific metadata participate in axis identity"):
    val baseline = axis()
    val variants = Vector(
      axis(keys = Vector("trial-32", "trial-7", "trial-91")),
      axis(basis = "aligned"),
      axis(units = "seconds"),
      axis(scale = "training-zscore"),
      axis(lineage = Vector("source:run-2"))
    )
    variants.foreach: variant =>
      assert(!baseline.sameIdentityAs(variant))
      assert(baseline.descriptor.coordinateSignature != variant.descriptor.coordinateSignature)
      assert(baseline.evidence.descriptor != variant.evidence.descriptor)

  test("decoded records and supplied indexes are checked before use"):
    val baseline = axis()
    val record = baseline.toRecord
    assert(AxisRef.restore(record.copy(coordinateSignature = "0" * 64)).isLeft)
    assert(AxisRef.restore(record.copy(coordinateSignature = record.coordinateSignature.toUpperCase)).isLeft)
    assert(AxisRef.restore(record.copy(stableKeys = record.stableKeys.reverse)).isLeft)
    val wrongIndex = right(AxisIndex.stableStrings(record.stableKeys.reverse))
    assert(AxisRef.decode(record, wrongIndex).isLeft)
    assert(
      AxisRef
        .fromStableKeys(
          "duplicate",
          SpaceRole.Samples,
          Vector("a", "a"),
          "native",
          "trial",
          "unscaled"
        )
        .isLeft
    )

  test("production axes are not subject to the prototype 64-coordinate bound"):
    val keys = Vector.tabulate(4096)(index => s"voxel-$index")
    val large = right(
      AxisRef.fromStableKeys(
        "whole-brain-mask",
        SpaceRole.Observed,
        keys,
        "voxel-centres-v1",
        "percent-signal-change",
        "unscaled",
        Vector("mask:sha256:fixture")
      )
    )
    assertEquals(large.size, 4096)
    assertEquals(right(large.keyAt(4095)), "voxel-4095")
    assertEquals(large.toRecord.stableKeys.last, "voxel-4095")

  test("locus points retain their runtime owner at compile time"):
    val errors = typeCheckErrors("""import scalafim.fmri.mvpa.*
def invalid(a: AxisRef[String], b: AxisRef[String], point: scalafim.locus.Point[b.Locus]) =
  a.keyAtPoint(point)
""")
    assertEquals(errors.length, 1)
    assert(errors.head.message.contains("a.Locus"))
