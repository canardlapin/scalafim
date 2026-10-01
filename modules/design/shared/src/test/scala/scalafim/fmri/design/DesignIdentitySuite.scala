package scalafim.fmri.design

import scalafim.fmri.design.event.{Event, EventModel, EventTerm}
import scalafim.fmri.hrf.{BasisElementId, Hrf, Hrfs, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame

class DesignIdentitySuite extends munit.FunSuite:
  test("single-run, two-run and FIR structural identities have shared literal goldens") {
    Vector(
      "single" -> design(Hrfs.SPMG1, Vector(16)),
      "two" -> design(Hrfs.SPMG1, Vector(8, 8)),
      "fir" -> design(Hrfs.fir(3, Seconds(6.0)), Vector(8, 8))
    ).foreach { case (name, schema) =>
      assertEquals(schema.columnIds.map(_.value), columnIdGoldens(name))
      exactFingerprintGoldens.get(name).foreach(expected => assertEquals(schema.fingerprint.value, expected))
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

  test("legacy column identities are incompatible and a one-ULP change retains distinct content identity") {
    val schema = design(Hrfs.fir(3, Seconds(6.0)), Vector(8, 8))
    val legacyColumns = schema.columns.zip(legacyFirColumnIds).map { case (column, id) => column.copy(id = ColumnId.unsafe(id)) }
    assert(!schema.coefficientAxis.structurallyCompatible(schema.coefficientAxis.copy(columns = legacyColumns)))
    val values = schema.matrix.data.clone()
    values(0) = java.lang.Double.longBitsToDouble(1L)
    val changed = scalafim.fmri.hrf.linalg.Mat.unsafe(schema.matrix.rows, schema.matrix.cols, values)
    assertNotEquals(DesignFingerprint.from(changed, schema.rows, schema.columns, schema.audit), schema.fingerprint)
  }

  test("semantic basis identity ignores display names while legacy references retain theirs") {
    val semantic = BasisElementRef("display-A", BasisIndex.unsafeOneBased(1), elementId = Some(BasisElementId.unsafe("semantic")))
    assertEquals(semantic.canonical, "basis(element=semantic|1)")
    assertEquals(semantic.canonical, semantic.copy(basisId = "display-B").canonical)
    val legacy = semantic.copy(elementId = None)
    assertNotEquals(legacy.canonical, legacy.copy(basisId = "display-B").canonical)
    val base = Hrfs.SPMG1
    val renamed = Hrf.of("renamed display", base.nbasis, base.span, Some(base.descriptor), base.support)(base.apply)
    assertEquals(HrfAssignment.Shared(base).canonical, HrfAssignment.Shared(renamed).canonical)
    val original = design(base, Vector(16))
    val other = design(renamed, Vector(16))
    assertEquals(original.columnIds, other.columnIds)
    assertEquals(original.fingerprint, other.fingerprint)
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
    assertEquals(snapshot.fingerprint.value, "design-schema/v2:b27079652d98235d")
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

  // Literal v1 FIR column IDs captured by the baseline JVM reproducer.
  private val legacyFirColumnIds = Vector(
    "event|task||cell:{condition=A}||basis(fir%7C1%7Cfir-bin-1%252502d-0.0..2.0%7Celement=family=Known(Fir)%257Cbasis=3%257Cspan=6.0%257Cparams=Fir(3)%257Cderivative=Numeric%257Cpenalty=Roughness%257Cintegration=PiecewisePolynomial(Vector(0.0, 2.0, 4.0, 6.0),0)%257Ccomponents=[]%257Cfir-bin-1%252502d-0.0..2.0%257C1)|Task|global|ordinal=1",
    "event|task||cell:{condition=B}||basis(fir%7C1%7Cfir-bin-1%252502d-0.0..2.0%7Celement=family=Known(Fir)%257Cbasis=3%257Cspan=6.0%257Cparams=Fir(3)%257Cderivative=Numeric%257Cpenalty=Roughness%257Cintegration=PiecewisePolynomial(Vector(0.0, 2.0, 4.0, 6.0),0)%257Ccomponents=[]%257Cfir-bin-1%252502d-0.0..2.0%257C1)|Task|global|ordinal=2",
    "event|task||cell:{condition=A}||basis(fir%7C2%7Cfir-bin-2%252502d-2.0..4.0%7Celement=family=Known(Fir)%257Cbasis=3%257Cspan=6.0%257Cparams=Fir(3)%257Cderivative=Numeric%257Cpenalty=Roughness%257Cintegration=PiecewisePolynomial(Vector(0.0, 2.0, 4.0, 6.0),0)%257Ccomponents=[]%257Cfir-bin-2%252502d-2.0..4.0%257C2)|Task|global|ordinal=3",
    "event|task||cell:{condition=B}||basis(fir%7C2%7Cfir-bin-2%252502d-2.0..4.0%7Celement=family=Known(Fir)%257Cbasis=3%257Cspan=6.0%257Cparams=Fir(3)%257Cderivative=Numeric%257Cpenalty=Roughness%257Cintegration=PiecewisePolynomial(Vector(0.0, 2.0, 4.0, 6.0),0)%257Ccomponents=[]%257Cfir-bin-2%252502d-2.0..4.0%257C2)|Task|global|ordinal=4",
    "event|task||cell:{condition=A}||basis(fir%7C3%7Cfir-bin-3%252502d-4.0..6.0%7Celement=family=Known(Fir)%257Cbasis=3%257Cspan=6.0%257Cparams=Fir(3)%257Cderivative=Numeric%257Cpenalty=Roughness%257Cintegration=PiecewisePolynomial(Vector(0.0, 2.0, 4.0, 6.0),0)%257Ccomponents=[]%257Cfir-bin-3%252502d-4.0..6.0%257C3)|Task|global|ordinal=5",
    "event|task||cell:{condition=B}||basis(fir%7C3%7Cfir-bin-3%252502d-4.0..6.0%7Celement=family=Known(Fir)%257Cbasis=3%257Cspan=6.0%257Cparams=Fir(3)%257Cderivative=Numeric%257Cpenalty=Roughness%257Cintegration=PiecewisePolynomial(Vector(0.0, 2.0, 4.0, 6.0),0)%257Ccomponents=[]%257Cfir-bin-3%252502d-4.0..6.0%257C3)|Task|global|ordinal=6"
  )

  private val exactFingerprintGoldens = Map(
    "two" -> "design-schema/v2:93615b2e435af535",
    "fir" -> "design-schema/v2:8f2ee67682098523"
  )

  // Literal outputs from the reviewed wire format; independently FNV-checked.
  private val columnIdGoldens = Map(
    "single" -> Vector(
      "event|task||cell:{condition=A}||basis(element=hrf-descriptor/v2%257Cfamily=known(5:spmg1)%257Cbasis=1%257Cspan=bits:4627448617123184640%257Cparams=spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969)%257Cderivative=spmg(89:spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969),1:1)%257Cpenalty=identity%257Cintegration=spmg1(89:spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969))%257Cderivation=none%257Ccomponents=sequence()%257Ccanonical%257C1%7C1%7Ccanonical)|Task|run:1|ordinal=1",
      "event|task||cell:{condition=B}||basis(element=hrf-descriptor/v2%257Cfamily=known(5:spmg1)%257Cbasis=1%257Cspan=bits:4627448617123184640%257Cparams=spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969)%257Cderivative=spmg(89:spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969),1:1)%257Cpenalty=identity%257Cintegration=spmg1(89:spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969))%257Cderivation=none%257Ccomponents=sequence()%257Ccanonical%257C1%7C1%7Ccanonical)|Task|run:1|ordinal=2"
    ),
    "two" -> Vector(
      "event|task||cell:{condition=A}||basis(element=hrf-descriptor/v2%257Cfamily=known(5:spmg1)%257Cbasis=1%257Cspan=bits:4627448617123184640%257Cparams=spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969)%257Cderivative=spmg(89:spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969),1:1)%257Cpenalty=identity%257Cintegration=spmg1(89:spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969))%257Cderivation=none%257Ccomponents=sequence()%257Ccanonical%257C1%7C1%7Ccanonical)|Task|global|ordinal=1",
      "event|task||cell:{condition=B}||basis(element=hrf-descriptor/v2%257Cfamily=known(5:spmg1)%257Cbasis=1%257Cspan=bits:4627448617123184640%257Cparams=spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969)%257Cderivative=spmg(89:spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969),1:1)%257Cpenalty=identity%257Cintegration=spmg1(89:spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969))%257Cderivation=none%257Ccomponents=sequence()%257Ccanonical%257C1%7C1%7Ccanonical)|Task|global|ordinal=2"
    ),
    "fir" -> Vector(
      "event|task||cell:{condition=A}||basis(element=hrf-descriptor/v2%257Cfamily=known(3:fir)%257Cbasis=3%257Cspan=bits:4618441417868443648%257Cparams=fir(1:3)%257Cderivative=numeric%257Cpenalty=roughness%257Cintegration=piecewise-polynomial(102:sequence(6:bits:0,24:bits:4611686018427387904,24:bits:4616189618054758400,24:bits:4618441417868443648),1:0)%257Cderivation=none%257Ccomponents=sequence()%257Cfir-bin-01-bits:0..bits:4611686018427387904%257C1%7C1%7Cfir-bin-01-bits:0..bits:4611686018427387904)|Task|global|ordinal=1",
      "event|task||cell:{condition=B}||basis(element=hrf-descriptor/v2%257Cfamily=known(3:fir)%257Cbasis=3%257Cspan=bits:4618441417868443648%257Cparams=fir(1:3)%257Cderivative=numeric%257Cpenalty=roughness%257Cintegration=piecewise-polynomial(102:sequence(6:bits:0,24:bits:4611686018427387904,24:bits:4616189618054758400,24:bits:4618441417868443648),1:0)%257Cderivation=none%257Ccomponents=sequence()%257Cfir-bin-01-bits:0..bits:4611686018427387904%257C1%7C1%7Cfir-bin-01-bits:0..bits:4611686018427387904)|Task|global|ordinal=2",
      "event|task||cell:{condition=A}||basis(element=hrf-descriptor/v2%257Cfamily=known(3:fir)%257Cbasis=3%257Cspan=bits:4618441417868443648%257Cparams=fir(1:3)%257Cderivative=numeric%257Cpenalty=roughness%257Cintegration=piecewise-polynomial(102:sequence(6:bits:0,24:bits:4611686018427387904,24:bits:4616189618054758400,24:bits:4618441417868443648),1:0)%257Cderivation=none%257Ccomponents=sequence()%257Cfir-bin-02-bits:4611686018427387904..bits:4616189618054758400%257C2%7C2%7Cfir-bin-02-bits:4611686018427387904..bits:4616189618054758400)|Task|global|ordinal=3",
      "event|task||cell:{condition=B}||basis(element=hrf-descriptor/v2%257Cfamily=known(3:fir)%257Cbasis=3%257Cspan=bits:4618441417868443648%257Cparams=fir(1:3)%257Cderivative=numeric%257Cpenalty=roughness%257Cintegration=piecewise-polynomial(102:sequence(6:bits:0,24:bits:4611686018427387904,24:bits:4616189618054758400,24:bits:4618441417868443648),1:0)%257Cderivation=none%257Ccomponents=sequence()%257Cfir-bin-02-bits:4611686018427387904..bits:4616189618054758400%257C2%7C2%7Cfir-bin-02-bits:4611686018427387904..bits:4616189618054758400)|Task|global|ordinal=4",
      "event|task||cell:{condition=A}||basis(element=hrf-descriptor/v2%257Cfamily=known(3:fir)%257Cbasis=3%257Cspan=bits:4618441417868443648%257Cparams=fir(1:3)%257Cderivative=numeric%257Cpenalty=roughness%257Cintegration=piecewise-polynomial(102:sequence(6:bits:0,24:bits:4611686018427387904,24:bits:4616189618054758400,24:bits:4618441417868443648),1:0)%257Cderivation=none%257Ccomponents=sequence()%257Cfir-bin-03-bits:4616189618054758400..bits:4618441417868443648%257C3%7C3%7Cfir-bin-03-bits:4616189618054758400..bits:4618441417868443648)|Task|global|ordinal=5",
      "event|task||cell:{condition=B}||basis(element=hrf-descriptor/v2%257Cfamily=known(3:fir)%257Cbasis=3%257Cspan=bits:4618441417868443648%257Cparams=fir(1:3)%257Cderivative=numeric%257Cpenalty=roughness%257Cintegration=piecewise-polynomial(102:sequence(6:bits:0,24:bits:4611686018427387904,24:bits:4616189618054758400,24:bits:4618441417868443648),1:0)%257Cderivation=none%257Ccomponents=sequence()%257Cfir-bin-03-bits:4616189618054758400..bits:4618441417868443648%257C3%7C3%7Cfir-bin-03-bits:4616189618054758400..bits:4618441417868443648)|Task|global|ordinal=6"
    )
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
