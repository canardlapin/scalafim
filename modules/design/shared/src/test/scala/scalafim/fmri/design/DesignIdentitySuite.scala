package scalafim.fmri.design

import scalafim.fmri.design.event.{Event, EventModel, EventTerm}
import scalafim.fmri.hrf.{Hrf, Hrfs, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame

class DesignIdentitySuite extends munit.FunSuite:
  test("single-run, two-run and FIR identity reproduction") {
    Vector(
      "single" -> design(Hrfs.SPMG1, Vector(16)),
      "two" -> design(Hrfs.SPMG1, Vector(8, 8)),
      "fir" -> design(Hrfs.fir(3, Seconds(6.0)), Vector(8, 8))
    ).foreach { case (name, schema) =>
      println(s"IDENTITY-$name-FINGERPRINT=${schema.fingerprint.value}")
      println(s"IDENTITY-$name-ENCODING=${schema.fingerprint.canonicalEncoding}")
      println(s"IDENTITY-$name-COLUMNS=${schema.columnIds.map(_.value).mkString(";")}")
    }
  }

  test("numeric audit fields retain literal IEEE-754 encodings") {
    val center = CenteringGroupReceipt("g", Vector(0), Vector(0), Some(1.0), CenteringOutcome.Applied)
    assertEquals(center.canonical, "g|rows=0|finite=0|center=4607182418800017408|outcome=applied")
    val policy = ModulatorOrthogonalization.ordered("task", "x", "y").toOption.get
    assertEquals(policy.canonical, "term=task;order=x,y;scope=whole-term;degenerate=retain-and-report;tolerance=4457293557087583675")
    val group = OrthogonalizationGroupReceipt("g", Vector(0), Vector.empty, 1, 2.0, 0.0, OrthogonalizationOutcome.Applied)
    assertEquals(group.canonical, "key=g;rows=0;parents=;rank=1;sourceNorm=4611686018427387904;residualNorm=0;outcome=Applied")
    assertEquals(MissingValuePolicy.ImputeConstant(2.0).canonical, "impute-constant:4611686018427387904")
    val column = ColumnId.unsafe("x")
    val preview = RankPreviewEvidence(RankPreviewMethod.PivotedQr, 1, 1, 1,
      RankToleranceConvention.ScaleAware, 1e-10, Vector(column), Vector(column), Vector.empty, Vector(2.0), Some(1.0))
    val audit = DesignAudit(rankPreview = Some(RankPreview.Available(preview)))
    assert(audit.canonical.contains("rank=PivotedQr:1:1:1:ScaleAware:4457293557087583675:pivots=x:independent=x:aliased=:diagonal=4611686018427387904:condition=4607182418800017408"))
  }

  test("old fingerprints are refused and a one-ULP numerical change retains distinct identity") {
    val schema = design(Hrfs.fir(3, Seconds(6.0)), Vector(8, 8))
    val old = schema.fingerprint.copy(value = "design-schema/v1:22221dc300375822")
    assert(DesignSchema.validate(schema.matrix, schema.rows, schema.columns, schema.audit, schema.rankPreview, old).isLeft)
    val values = schema.matrix.data.clone()
    values(0) = java.lang.Double.longBitsToDouble(1L)
    val changed = scalafim.fmri.hrf.linalg.Mat.unsafe(schema.matrix.rows, schema.matrix.cols, values)
    assertNotEquals(DesignFingerprint.from(changed, schema.rows, schema.columns, schema.audit), schema.fingerprint)
  }

  test("realistic SPMG structural identity is distinct from its numerical content") {
    val compiled = design(Hrfs.SPMG1, Vector(16))
    val actual = compiled.matrix.data.map(java.lang.Double.doubleToLongBits).toVector
    assertEquals(actual.length, singleMatrixBits.length)
    actual.zip(singleMatrixBits).foreach { case (observed, expected) =>
      assert((BigInt(observed) - BigInt(expected)).abs <= 1, s"SPMG value differs by more than one ULP: $observed vs $expected")
    }
    val matrix = scalafim.fmri.hrf.linalg.Mat.unsafe(16, 2, singleMatrixBits.map(java.lang.Double.longBitsToDouble).toArray)
    val snapshot = DesignSchema.validated(matrix, compiled.rows, compiled.columns, compiled.audit.copy(rankPreview = None)).toOption.get
    println(s"IDENTITY-single-fixed-FINGERPRINT=${snapshot.fingerprint.value}")
    println(s"IDENTITY-single-fixed-ENCODING=${snapshot.fingerprint.canonicalEncoding}")
  }

  // Literal matrix contents captured before the serialization repair. This is
  // a content-identity fixture, not a claim of universal arithmetic bit parity.
  private val singleMatrixBits: Vector[Long] = Vector(
    0L,0L,4555774828609193142L,0L,
    4579810022347009932L,0L,4589486336939214991L,4553063463407962378L,
    4593908018891858828L,4579810022347009942L,4595293611042985230L,4589499510130095525L,
    4595331447223893258L,4593919555501158014L,4594387919381477940L,4595293611042985229L,
    4592480243847689709L,4595321072291005538L,4589927193262952680L,4594390959099332238L,
    4586500958794902998L,4592480243847689710L,4582032258499777514L,4589931807517154160L,
    4574170675209113762L,4586490381474522134L,-4652173186659637944L,4582032258499777515L,
    -4646124714654090475L,4574229844653497104L,-4644026202262718823L,-4652129734914764700L
  )

  private def design(hrf: Hrf, blocks: Vector[Int]): DesignSchema =
    val frame = SamplingFrame(blockLens = blocks, tr = Vector(1.0))
    val term = EventTerm(
      events = Vector(Event.factor(Vector("A", "B"), "condition")),
      onsets = Vector(Seconds(1.0), Seconds(3.0)),
      blockIds = Vector(0, blocks.length - 1),
      termTag = Some("task")
    )
    EventModel.build(Vector(term.convolve(hrf, frame)), frame).designSchema
