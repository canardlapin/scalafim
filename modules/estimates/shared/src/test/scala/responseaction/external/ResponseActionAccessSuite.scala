package responseaction.external

import scalafim.estimates.*

/** Compiled outside `scalafim.*`: the trust-bearing types cannot be forged. */
class ResponseActionAccessSuite extends munit.FunSuite:
  private def refused(errors: String, name: String): Unit =
    assert(errors.nonEmpty, s"$name must not compile outside scalafim")
    assert(!errors.contains("Not found"), s"$name failed for an unrelated reason: $errors")

  test("control: the public surface compiles from an external package"):
    assertNoDiff(compileErrors("ReadoutAxis.parse(Vector.empty)"), "")
    assertNoDiff(compileErrors("ResponseBindingCodec.decode(Array.emptyByteArray)"), "")
    assertNoDiff(compileErrors("(??? : ResponseSourceBinding).columns"), "")
    assertNoDiff(compileErrors("(??? : StatusSummary).counts"), "")
    assertNoDiff(compileErrors("ResponseActionEvidence.evaluate(???, ???, None)"), "")

  test("ResponseSourceBinding has no external apply, new, copy or fromProduct"):
    refused(compileErrors("ResponseSourceBinding(???, ???, ???, ???, ???, ???, ???, ???, ???, ???, ???, ???, ???)"), "apply")
    refused(compileErrors("new ResponseSourceBinding(???, ???, ???, ???, ???, ???, ???, ???, ???, ???, ???, ???, ???)"), "new")
    refused(compileErrors("(??? : ResponseSourceBinding).copy()"), "copy")
    refused(compileErrors("ResponseSourceBinding.fromProduct(???)"), "fromProduct")

  test("StatusSummary has no external apply, new, copy or fromProduct"):
    refused(compileErrors("StatusSummary(???, ???, ???, ???, ???, ???, ???)"), "apply")
    refused(compileErrors("new StatusSummary(???, ???, ???, ???, ???, ???, ???)"), "new")
    refused(compileErrors("(??? : StatusSummary).copy()"), "copy")
    refused(compileErrors("StatusSummary.fromProduct(???)"), "fromProduct")

  test("ReadoutAxis has no external apply, new, copy or fromProduct"):
    refused(compileErrors("ReadoutAxis(Vector.empty)"), "apply")
    refused(compileErrors("new ReadoutAxis(Vector.empty)"), "new")
    refused(compileErrors("(??? : ReadoutAxis).copy()"), "copy")
    refused(compileErrors("ReadoutAxis.fromProduct(???)"), "fromProduct")

  test("the digest helpers that derive provider identity are not public"):
    val errors = compileErrors("ResponseDigests.features(???)")
    assert(errors.contains("ResponseDigests"), errors)

  test("the decoder yields only an untrusted claim"):
    refused(compileErrors("val b: ResponseSourceBinding = ResponseBindingCodec.decode(Array.emptyByteArray).toOption.get"), "decode")
    refused(compileErrors("val b: ResponseSourceBinding = (??? : DecodedBindingClaim)"), "claim")
