package scalafim.multivar.ir

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

class ConformanceFilesSuite extends munit.FunSuite:
  private val root = Path.of("modules", "multivar-ir", "conformance")

  private def read(relative: String): String =
    Files.readString(root.resolve(relative), StandardCharsets.UTF_8)

  test("published valid fixture is accepted by the normative codec") {
    assert(MultivarIrCodec.decode(read("valid/minimal-v0.1.json")).isRight)
  }

  test("published invalid fixtures produce their manifest rejection categories") {
    val direct = Vector(
      "invalid/domain-codomain-mismatch.json" -> RejectionCategory.DomainCodomainMismatch,
      "invalid/uncertified-positivity.json" -> RejectionCategory.UncertifiedPositivity,
      "invalid/incompatible-alignment-kind.json" -> RejectionCategory.IncompatibleAlignmentKind,
      "invalid/payload-tampered.json" -> RejectionCategory.PayloadTampered,
      "invalid/unknown-field.json" -> RejectionCategory.UnknownField
    )
    direct.foreach { case (file, expected) =>
      val actual = MultivarIrCodec.decode(read(file)).left.toOption.getOrElse(fail(s"$file was unexpectedly accepted"))
      assertEquals(actual.category, expected, file)
    }

    val quotient = MultivarIrCodec
      .decode(read("invalid/unsupported-singularity-for-reject-only.json"))
      .fold(error => fail(error.message), identity)
    val unsupported = IrValidator.validate(quotient, IrCapabilities(Set("reject"))).left.toOption.get
    assertEquals(unsupported.category, RejectionCategory.UnsupportedSingularity)
  }
