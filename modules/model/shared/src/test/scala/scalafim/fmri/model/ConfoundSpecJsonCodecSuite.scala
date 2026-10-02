package scalafim.fmri.model

class ConfoundSpecJsonCodecSuite extends munit.FunSuite:
  test("strict versioned confound specification round trips every option"):
    val policy = FdCensorPolicy.make(0.5, 1, 2, 3, 0.4).toOption.getOrElse(fail("policy"))
    val source = ConfoundSpec.make(MotionExpansion.Friston24, 5, true, true, true, Some(policy)).toOption.getOrElse(fail("spec"))
    assertEquals(ConfoundSpecJsonCodec.decode(ConfoundSpecJsonCodec.encode(source)), Right(source))

  test("confound JSON rejects unknown and malformed fields"):
    assert(ConfoundSpecJsonCodec.decode("""{"version":1,"motion":"Raw6","acompcorComponents":0,"includeWhiteMatter":false,"includeCsf":false,"includeGlobalSignal":false,"censor":null,"extra":1}""").isLeft)
    assert(ConfoundSpecJsonCodec.decode("""{"version":1,"motion":"Raw6","acompcorComponents":0,"includeWhiteMatter":false,"includeCsf":false,"includeGlobalSignal":false,"censor":{"threshold":0.5}}""").isLeft)
