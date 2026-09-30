package scalafim.estimates

import scalafim.image.SampleSpaces
import ResponseActionRefusal.*
import UnavailableReason.*

object ResponseActionFixture:
  def digest(label: String): ProviderDigest = ResponseDigests.provider("fixture/v1", label)
  def level(name: String): ConditionLevelId = ConditionLevelId(name)
  def axis(conditions: Vector[String], bins: Int): ReadoutAxis =
    ReadoutAxis.parse(for c <- conditions; b <- (0 until bins).toVector yield ReadoutRowId(level(c), b)).toOption.get

  val unit: UnitRevisionId = UnitRevisionId("00000000-0000-4000-8000-000000000201")
  val otherUnit: UnitRevisionId = UnitRevisionId("00000000-0000-4000-8000-000000000202")
  val observation: ObservationId = ObservationId("sub-01")
  val columns: Vector[ColumnId] = Vector("A_0", "A_1", "B_0", "B_1", "C_0", "C_1", "intercept").map(ColumnId.apply)
  val domain: EstimateDomain = EstimateDomain.make(SampleSpaces(Vector(4, 1, 1)), Vector(0, 2, 3), "scanner").toOption.get
  val features: ProviderDigest = ResponseDigests.features(domain)

  val binding: ResponseSourceBinding = ResponseSourceBinding(unit, observation, digest("design balanced 5/5/5 FIR2"),
    digest("preparation"), digest("noise: OLS"), digest("single run"), digest("readout A,B,C x FIR2"),
    axis(Vector("A", "B", "C"), 2), columns, columns.dropRight(1), features, RealizedNoise.White, RealizedCombination.SingleRun)

  val model: DeclaredResponseModel = DeclaredResponseModel(DeclaredTemporal.White, DeclaredCombination.SingleRun,
    SpatialClaim.KroneckerSeparable, DistributionClaim.Gaussian, ModelOrigin.Declared("preregistered white Gaussian model"))
  val cycle: ConditionAction = ConditionAction.Permute(Map(level("A") -> level("B"), level("B") -> level("C"), level("C") -> level("A")))
  val identityAction: ConditionAction = ConditionAction.Permute(Vector("A", "B", "C").map(c => level(c) -> level(c)).toMap)

  /** Every field is the most positive-looking value a consumer could supply. */
  val request: ResponseActionRequest = ResponseActionRequest(ResponseActionEvidence.Version, binding, Some(model), cycle,
    NullConstraint.Linear(digest("B = N Gamma, N = condition-invariant basis")))
  val summary: StatusSummary = StatusSummary(unit, new InferenceStatusScope.Fit(observation), features,
    Map(InferenceStatusCode.Estimable -> 3L), ScientificFact.Known("full-rank shared design, rank-revealing QR"))
  val scanned: Option[StatusEvidence] = Some(StatusEvidence.Scanned(summary))

  def evaluate(req: ResponseActionRequest = request, actual: ResponseSourceBinding = binding,
      status: Option[StatusEvidence] = scanned): ResponseActionRefusal = ResponseActionEvidence.evaluate(req, actual, status)

  /** Declares the expected binding and the realized binding identically. */
  def realized(noise: RealizedNoise = RealizedNoise.White,
      combination: RealizedCombination = RealizedCombination.SingleRun): (ResponseActionRequest, ResponseSourceBinding) =
    val b = binding.copy(realizedNoise = noise, realizedCombination = combination)
    (request.copy(expected = b), b)

class ResponseActionEvidenceSuite extends munit.FunSuite:
  import ResponseActionFixture.*

  test("the most positive-looking v1 request is still refused as Unavailable(NoPositiveContractInVersion)"):
    assertEquals(evaluate(), Unavailable(NoPositiveContractInVersion(ResponseActionEvidence.Version)))

  test("the v1 codomain has exactly five refusal cases and no positive case"):
    val mirror = summon[scala.deriving.Mirror.SumOf[ResponseActionRefusal]]
    val labels = scala.compiletime.constValueTuple[mirror.MirroredElemLabels].toList
    assertEquals(labels, List("UnsupportedVersion", "SourceMismatch", "ModelMismatch", "ActionMismatch", "Unavailable"))

  test("an unknown version is representable and refused first"):
    assertEquals(evaluate(request.copy(version = "scalafim.response-action/2")), UnsupportedVersion("scalafim.response-action/2"))
    assertEquals(evaluate(request.copy(version = "")), UnsupportedVersion(""))

  test("source mismatch table: one row per SourceField, in field order"):
    val reordered = EstimateDomain.make(SampleSpaces(Vector(4, 1, 1)), Vector(3, 0, 2), "scanner").toOption.get
    val reorderedFeatures = ResponseDigests.features(reordered)
    assertNotEquals(reorderedFeatures, features, "a permutation of the same sample-ID set must change feature identity")
    val swapped = columns.updated(0, columns(1)).updated(1, columns(0))
    val binding = ResponseActionFixture.binding
    def row(field: SourceField, actual: ResponseSourceBinding, expected: String, found: String) =
      (field, evaluate(actual = actual), SourceMismatch(field, expected, found))
    val bindingRows = Vector(
      row(SourceField.Unit, binding.copy(unit = otherUnit), unit.value, otherUnit.value),
      row(SourceField.Observation, binding.copy(observation = ObservationId("sub-02")), "sub-01", "sub-02"),
      row(SourceField.Design, binding.copy(design = digest("design unequal 9/4/2 FIR2")), binding.design.render,
        digest("design unequal 9/4/2 FIR2").render),
      row(SourceField.Preparation, binding.copy(preparation = digest("censored")), binding.preparation.render, digest("censored").render),
      row(SourceField.Noise, binding.copy(noise = digest("noise: GLS")), binding.noise.render, digest("noise: GLS").render),
      row(SourceField.RunCombination, binding.copy(runCombination = digest("joint runs")), binding.runCombination.render,
        digest("joint runs").render),
      row(SourceField.Readout, binding.copy(readout = digest("readout B,A,C")), binding.readout.render, digest("readout B,A,C").render),
      row(SourceField.ReadoutAxis, binding.copy(readoutAxis = axis(Vector("B", "A", "C"), 2)), binding.readoutAxis.render,
        "[B#0,B#1,A#0,A#1,C#0,C#1]"),
      // Same design digest, reordered columns.
      row(SourceField.Columns, binding.copy(columns = swapped), columns.map(_.value).mkString("[", ",", "]"),
        swapped.map(_.value).mkString("[", ",", "]")),
      row(SourceField.Selected, binding.copy(selected = columns.take(2)), columns.dropRight(1).map(_.value).mkString("[", ",", "]"), "[A_0,A_1]"),
      // Same sample-ID set, different physical order.
      row(SourceField.Features, binding.copy(features = reorderedFeatures), features.render, reorderedFeatures.render),
      row(SourceField.RealizedNoise, binding.copy(realizedNoise = RealizedNoise.EstimatedAr(1, NoiseScope.Global)), "White",
        "EstimatedAr(1,Global)"),
      row(SourceField.RealizedCombination, binding.copy(realizedCombination = RealizedCombination.Unrecorded), "SingleRun", "Unrecorded")
    )
    def statusRow(field: SourceField, s: StatusSummary, expected: String, found: String) =
      (field, evaluate(status = Some(StatusEvidence.Scanned(s))), SourceMismatch(field, expected, found))
    val statusRows = Vector(
      statusRow(SourceField.StatusUnit, summary.copy(unit = otherUnit), unit.value, otherUnit.value),
      statusRow(SourceField.StatusFeatures, summary.copy(features = reorderedFeatures), features.render, reorderedFeatures.render),
      statusRow(SourceField.StatusPlane, summary.copy(plane = new InferenceStatusScope.Fit(ObservationId("sub-02"))), "sub-01", "sub-02"),
      statusRow(SourceField.StatusOutsideSupport, summary.copy(counts = summary.counts + (InferenceStatusCode.OutsideSupport -> 1L)), "0", "1")
    )
    val table = bindingRows ++ statusRows
    assertEquals(table.map(_._1), SourceField.values.toVector, "the table must cover every SourceField in declared order")
    table.foreach((field, actual, expected) => assertEquals(actual, expected, s"field $field"))

  test("source fields are compared in declared order and the status scope comes last"):
    val all = binding.copy(unit = otherUnit, observation = ObservationId("sub-02"), realizedNoise = RealizedNoise.Robust)
    assertEquals(evaluate(actual = all), SourceMismatch(SourceField.Unit, unit.value, otherUnit.value))
    val late = binding.copy(realizedCombination = RealizedCombination.Unrecorded)
    val otherSummary = Some(StatusEvidence.Scanned(summary.copy(unit = otherUnit)))
    assertEquals(evaluate(actual = late, status = otherSummary),
      SourceMismatch(SourceField.RealizedCombination, "SingleRun", "Unrecorded"))
    val scope = summary.copy(features = digest("x"), plane = new InferenceStatusScope.Fit(ObservationId("sub-02")),
      counts = Map(InferenceStatusCode.OutsideSupport -> 2L))
    assertEquals(evaluate(status = Some(StatusEvidence.Scanned(scope))),
      SourceMismatch(SourceField.StatusFeatures, features.render, digest("x").render))

  test("absent or unscanned status evidence is a typed refusal, never Estimable"):
    assertEquals(evaluate(status = None), Unavailable(InferenceStatus(StatusAbsence.NotScanned)))
    val reasons = Vector(StatusAbsence.UnitHasNoEvidence, StatusAbsence.NoCoefficientEvidence, StatusAbsence.ColumnsDisagree,
      StatusAbsence.SelectedNotInferable(Vector(columns.head)), StatusAbsence.FitPlaneAbsent, StatusAbsence.NotScanned)
    reasons.foreach: reason =>
      assertEquals(evaluate(status = Some(StatusEvidence.Absent(reason))), Unavailable(InferenceStatus(reason)))
    // Absence outranks every later unavailable reason, including a missing model.
    assertEquals(evaluate(req = request.copy(model = None, action = identityAction), status = None),
      Unavailable(InferenceStatus(StatusAbsence.NotScanned)))

  test("model mismatch: declared temporal and combination against the realized record"):
    val estimated = RealizedNoise.EstimatedAr(1, NoiseScope.PerRun)
    val (whiteVsAr, arBinding) = realized(noise = estimated)
    assertEquals(evaluate(whiteVsAr, arBinding), ModelMismatch(ModelDefect.TemporalMismatch(DeclaredTemporal.White, estimated)))
    val spec = digest("AR(1) phi=0.4 exact init, resets at runs")
    val declared = DeclaredTemporal.FixedSigmaT(spec)
    val fixedOther = RealizedNoise.FixedAr(1, NoiseScope.Global, Some(digest("AR(1) phi=0.5")))
    val (sigmaReq, sigmaBinding) = realized(noise = fixedOther)
    val sigma = sigmaReq.copy(model = Some(model.copy(temporal = declared)))
    assertEquals(evaluate(sigma, sigmaBinding), ModelMismatch(ModelDefect.TemporalMismatch(declared, fixedOther)))
    val fixedSame = RealizedNoise.FixedAr(1, NoiseScope.Global, Some(spec))
    val (sameReq, sameBinding) = realized(noise = fixedSame)
    assertEquals(evaluate(sameReq.copy(model = Some(model.copy(temporal = declared))), sameBinding),
      Unavailable(NoPositiveContractInVersion(ResponseActionEvidence.Version)))
    val (whiteReq, whiteBinding) = realized()
    assertEquals(evaluate(whiteReq.copy(model = Some(model.copy(temporal = declared))), whiteBinding),
      ModelMismatch(ModelDefect.TemporalMismatch(declared, RealizedNoise.White)))
    val (combinationReq, combinationBinding) = realized(combination = RealizedCombination.EstimatedWeights)
    assertEquals(evaluate(combinationReq, combinationBinding),
      ModelMismatch(ModelDefect.CombinationMismatch(DeclaredCombination.SingleRun, RealizedCombination.EstimatedWeights)))
    val weights = DeclaredCombination.FixedWeights(digest("weights 0.5/0.5"))
    val realizedWeights = RealizedCombination.FixedWeights(digest("weights 0.7/0.3"))
    val (weightsReq, weightsBinding) = realized(combination = realizedWeights)
    assertEquals(evaluate(weightsReq.copy(model = Some(model.copy(combination = weights))), weightsBinding),
      ModelMismatch(ModelDefect.CombinationMismatch(weights, realizedWeights)))
    val (singleReq, singleBinding) = realized()
    assertEquals(evaluate(singleReq.copy(model = Some(model.copy(combination = weights))), singleBinding),
      ModelMismatch(ModelDefect.CombinationMismatch(weights, RealizedCombination.SingleRun)))
    // Temporal is checked before combination.
    val (bothReq, bothBinding) = realized(noise = estimated, combination = RealizedCombination.EstimatedWeights)
    assertEquals(evaluate(bothReq, bothBinding), ModelMismatch(ModelDefect.TemporalMismatch(DeclaredTemporal.White, estimated)))

  test("recorded but incomplete or data-dependent processing is unavailable, not mismatched"):
    val declared = model.copy(temporal = DeclaredTemporal.FixedSigmaT(digest("AR(1) exact")))
    def withNoise(noise: RealizedNoise, m: DeclaredResponseModel = declared) =
      val (req, b) = realized(noise = noise)
      evaluate(req.copy(model = Some(m)), b)
    assertEquals(withNoise(RealizedNoise.FixedAr(1, NoiseScope.Global, None)), Unavailable(NoiseIncomplete))
    assertEquals(withNoise(RealizedNoise.Unrecorded), Unavailable(NoiseIncomplete))
    assertEquals(withNoise(RealizedNoise.Unrecorded, model), Unavailable(NoiseIncomplete))
    assertEquals(withNoise(RealizedNoise.EstimatedAr(2, NoiseScope.PerFeature)), Unavailable(EstimatedWhitening))
    assertEquals(withNoise(RealizedNoise.Robust), Unavailable(EstimatedWhitening))
    assertEquals(withNoise(RealizedNoise.LearnedSubspace), Unavailable(LearnedResponseSubspace))
    // A white declaration admits no temporal processing at all.
    assertEquals(withNoise(RealizedNoise.FixedAr(1, NoiseScope.Global, None), model),
      ModelMismatch(ModelDefect.TemporalMismatch(DeclaredTemporal.White, RealizedNoise.FixedAr(1, NoiseScope.Global, None))))
    assertEquals(withNoise(RealizedNoise.LearnedSubspace, model),
      ModelMismatch(ModelDefect.TemporalMismatch(DeclaredTemporal.White, RealizedNoise.LearnedSubspace)))
    val weights = model.copy(combination = DeclaredCombination.FixedWeights(digest("w")))
    val (estimatedReq, estimatedBinding) = realized(combination = RealizedCombination.EstimatedWeights)
    assertEquals(evaluate(estimatedReq.copy(model = Some(weights)), estimatedBinding), Unavailable(EstimatedRunWeights))
    val (unrecordedReq, unrecordedBinding) = realized(combination = RealizedCombination.Unrecorded)
    assertEquals(evaluate(unrecordedReq, unrecordedBinding), Unavailable(CombinationIncomplete))

  test("action mismatch: unknown level, incomplete levels, non-bijection; identity is a policy refusal"):
    val a = level("A"); val b = level("B"); val c = level("C"); val z = level("Z")
    def act(map: Map[ConditionLevelId, ConditionLevelId]) = evaluate(request.copy(action = ConditionAction.Permute(map)))
    assertEquals(act(Map(a -> b, b -> z, c -> a)), ActionMismatch(ActionDefect.UnknownLevel(z)))
    assertEquals(act(Map(a -> b, b -> a, c -> c, z -> z)), ActionMismatch(ActionDefect.UnknownLevel(z)))
    assertEquals(act(Map(a -> b, b -> a)), ActionMismatch(ActionDefect.LevelsIncomplete(Vector(c))))
    assertEquals(act(Map.empty), ActionMismatch(ActionDefect.LevelsIncomplete(Vector(a, b, c))))
    assertEquals(act(Map(a -> b, b -> b, c -> a)), ActionMismatch(ActionDefect.NotBijection))
    assertEquals(act(Map(a -> b, b -> a, c -> c)), Unavailable(NoPositiveContractInVersion(ResponseActionEvidence.Version)))
    assertEquals(evaluate(request.copy(action = identityAction)), Unavailable(IdentityActionPolicy))

  test("readout axis construction refuses malformed rows and cannot be built invalid internally"):
    def rows(spec: (String, Int)*) = spec.toVector.map((c, b) => ReadoutRowId(level(c), b))
    assertEquals(ReadoutAxis.parse(Vector.empty), Left(ReadoutDefect.EmptyAxis))
    assertEquals(ReadoutAxis.parse(rows("A" -> 0, "A" -> 0)), Left(ReadoutDefect.DuplicateRow(ReadoutRowId(level("A"), 0))))
    assertEquals(ReadoutAxis.parse(rows("A" -> 0, "B" -> 0, "A" -> 1, "B" -> 1)), Left(ReadoutDefect.NotConditionMajor(level("A"))))
    assertEquals(ReadoutAxis.parse(rows("A" -> 1, "A" -> 0, "B" -> 0, "B" -> 1)), Left(ReadoutDefect.BinsNotSequential(level("A"))))
    assertEquals(ReadoutAxis.parse(rows("A" -> 0, "A" -> 2)), Left(ReadoutDefect.BinsNotSequential(level("A"))))
    assertEquals(ReadoutAxis.parse(rows("A" -> 0, "A" -> 1, "B" -> 0)), Left(ReadoutDefect.UnequalBins(level("B"), 2, 1)))
    assertEquals(ReadoutAxis.parse(rows("A" -> 0, "A" -> 1, "B" -> 0, "B" -> 1)).map(_.bins), Right(2))
    intercept[IllegalArgumentException](binding.copy(readoutAxis = axis(Vector("A"), 1)).copy(selected = Vector.empty))

  test("raw row permutations parse only as P tensor I_bins on a checked axis"):
    def rows(spec: (String, Int)*) = spec.toVector.map((c, b) => ReadoutRowId(level(c), b))
    val base = rows("A" -> 0, "A" -> 1, "B" -> 0, "B" -> 1, "C" -> 0, "C" -> 1)
    assertEquals(ConditionAction.parseRows(base, rows("B" -> 0, "B" -> 1, "C" -> 0, "C" -> 1, "A" -> 0, "A" -> 1)), Right(cycle))
    assertEquals(ConditionAction.parseRows(base, base).map(_.isIdentity), Right(true))
    assertEquals(ConditionAction.parseRows(base, rows("B" -> 0, "C" -> 1, "C" -> 0, "B" -> 1, "A" -> 0, "A" -> 1)),
      Left(ReadoutDefect.BinsSplit(level("A"))))
    assertEquals(ConditionAction.parseRows(base, rows("B" -> 1, "B" -> 0, "C" -> 0, "C" -> 1, "A" -> 0, "A" -> 1)),
      Left(ReadoutDefect.BinsPermuted(level("A"))))
    assertEquals(ConditionAction.parseRows(rows("A" -> 0, "A" -> 1, "B" -> 0), rows("B" -> 0, "A" -> 0, "A" -> 1)),
      Left(ReadoutDefect.UnequalBins(level("B"), 2, 1)))
    assertEquals(ConditionAction.parseRows(rows("A" -> 0, "B" -> 0, "A" -> 1, "B" -> 1), rows("B" -> 0, "A" -> 0, "B" -> 1, "A" -> 1)),
      Left(ReadoutDefect.NotConditionMajor(level("A"))))
    assertEquals(ConditionAction.parseRows(base, base.reverse.drop(1)), Left(ReadoutDefect.NotRowPermutation))
    assertEquals(ConditionAction.parseRows(base, base.updated(0, ReadoutRowId(level("Z"), 0))), Left(ReadoutDefect.NotRowPermutation))

  test("unavailable reasons are reported in declared order"):
    // Start with every unavailable cause present, then repair them one at a time.
    val inferred = model.copy(origin = ModelOrigin.InferredFromFit("fitted AR coefficients"), spatial = SpatialClaim.Unspecified)
    val (worstReq, worst) = realized(noise = RealizedNoise.Unrecorded, combination = RealizedCombination.Unrecorded)
    val badSummary = summary.copy(conditioning = ScientificFact.Unknown("learned subspace not retained"),
      counts = Map(InferenceStatusCode.Estimable -> 1L, InferenceStatusCode.Constant -> 1L, InferenceStatusCode.Unrecorded -> 1L))
    val base = worstReq.copy(model = Some(inferred), action = identityAction, nullConstraint = NullConstraint.Unencoded("no difference"))
    val steps: Vector[(UnavailableReason, (ResponseActionRequest, ResponseSourceBinding, Option[StatusEvidence]))] =
      val s0 = (base.copy(model = None), worst, None)
      val s1 = (base.copy(model = None), worst, Some(StatusEvidence.Scanned(badSummary)))
      val s2 = (base, worst, s1._3)
      val m3 = inferred.copy(origin = model.origin)
      val s3 = (base.copy(model = Some(m3)), worst, s1._3)
      val m4 = m3.copy(temporal = DeclaredTemporal.FixedSigmaT(digest("sigma")), combination = DeclaredCombination.FixedWeights(digest("w")))
      def step(noise: RealizedNoise, combination: RealizedCombination, status: Option[StatusEvidence] = s1._3) =
        val b = worst.copy(realizedNoise = noise, realizedCombination = combination)
        (base.copy(expected = b, model = Some(m4)), b, status)
      val exact = RealizedNoise.FixedAr(1, NoiseScope.Global, Some(digest("sigma")))
      val weights = RealizedCombination.FixedWeights(digest("w"))
      val estimated = RealizedNoise.EstimatedAr(1, NoiseScope.Global)
      val s4 = step(estimated, RealizedCombination.Unrecorded)
      val s5 = step(estimated, RealizedCombination.EstimatedWeights)
      val s6 = step(exact, RealizedCombination.EstimatedWeights)
      val s7 = step(RealizedNoise.LearnedSubspace, weights)
      val s8 = step(exact, weights)
      val known = Some(StatusEvidence.Scanned(badSummary.copy(conditioning = summary.conditioning)))
      val s9 = step(exact, weights, known)
      val clean = Some(StatusEvidence.Scanned(summary))
      val s10 = step(exact, weights, clean)
      val s11 = (s10._1.copy(nullConstraint = request.nullConstraint), s10._2, clean)
      val s12 = (s11._1.copy(action = cycle), s10._2, clean)
      val s13 = (s12._1.copy(model = Some(m4.copy(spatial = SpatialClaim.KroneckerSeparable))), s10._2, clean)
      Vector(InferenceStatus(StatusAbsence.NotScanned) -> s0, NoDeclaredModel -> s1, ModelInferredFromFit -> s2,
        NoiseIncomplete -> s3, CombinationIncomplete -> s4, EstimatedWhitening -> s5, EstimatedRunWeights -> s6,
        LearnedResponseSubspace -> s7, ConditioningUnknown -> s8, NonEstimableFeatures(2L) -> s9, NullNotEncoded -> s10,
        IdentityActionPolicy -> s11, SpatialJointUnrepresented -> s12,
        NoPositiveContractInVersion(ResponseActionEvidence.Version) -> s13)
    steps.foreach:
      case (reason, (req, actual, status)) => assertEquals(evaluate(req, actual, status), Unavailable(reason), s"$reason")
    // Combination-incomplete sits between noise-incomplete and estimated whitening.
    val (req, b) = realized(noise = RealizedNoise.EstimatedAr(1, NoiseScope.Global), combination = RealizedCombination.Unrecorded)
    assertEquals(evaluate(req.copy(model = Some(model.copy(temporal = DeclaredTemporal.FixedSigmaT(digest("s"))))), b),
      Unavailable(CombinationIncomplete))

  test("precedence: version, then source, model and action faults together"):
    val allFaults = request.copy(model = Some(model), action = ConditionAction.Permute(Map.empty))
    val faultyActual = binding.copy(unit = otherUnit, realizedNoise = RealizedNoise.EstimatedAr(1, NoiseScope.Global))
    val faultyStatus = Some(StatusEvidence.Scanned(summary.copy(unit = otherUnit)))
    assertEquals(evaluate(allFaults, faultyActual, faultyStatus), SourceMismatch(SourceField.Unit, unit.value, otherUnit.value))
    assertEquals(evaluate(allFaults.copy(version = "v0"), faultyActual, faultyStatus), UnsupportedVersion("v0"))
    val (modelReq, modelActual) = realized(noise = RealizedNoise.EstimatedAr(1, NoiseScope.Global))
    assertEquals(evaluate(modelReq.copy(action = ConditionAction.Permute(Map.empty)), modelActual, None),
      ModelMismatch(ModelDefect.TemporalMismatch(DeclaredTemporal.White, RealizedNoise.EstimatedAr(1, NoiseScope.Global))))
    assertEquals(evaluate(request.copy(action = ConditionAction.Permute(Map.empty), model = None), binding, None),
      ActionMismatch(ActionDefect.LevelsIncomplete(Vector(level("A"), level("B"), level("C")))))

  test("PLS literal fixtures all return Unavailable in v1"):
    def fixture(designLabel: String, conditions: Vector[String], bins: Int, spatial: SpatialClaim): ResponseActionRefusal =
      val levels = conditions.map(level)
      val cols = for c <- conditions; b <- (0 until bins).toVector yield ColumnId(s"${c}_fir$b")
      val b = binding.copy(design = digest(designLabel), readoutAxis = axis(conditions, bins), columns = cols, selected = cols)
      val shift = ConditionAction.Permute(levels.zip(levels.tail :+ levels.head).toMap)
      evaluate(request.copy(expected = b, action = shift, model = Some(model.copy(spatial = spatial))), b)
    val v1 = Unavailable(NoPositiveContractInVersion(ResponseActionEvidence.Version))
    assertEquals(fixture("balanced 5/5/5 FIR5", Vector("A", "B", "C"), 5, SpatialClaim.KroneckerSeparable), v1)
    assertEquals(fixture("unequal 9/4/2 FIR5", Vector("A", "B", "C"), 5, SpatialClaim.KroneckerSeparable), v1)
    // The identity-marginal counterexample: every marginal is identical, the joint is not Kronecker.
    assertEquals(fixture("balanced 5/5/5 FIR5", Vector("A", "B", "C"), 5,
      SpatialClaim.NonSeparable("condition-specific spatial correlation")), Unavailable(SpatialJointUnrepresented))
    // A consumer who (wrongly) claims separability for it is still refused.
    assertEquals(fixture("identity-marginal joint counterexample", Vector("A", "B"), 5, SpatialClaim.KroneckerSeparable), v1)

  test("exhaustive generated requests: first failure equals the head of an independent collect-all oracle"):
    val random = new scala.util.Random(20260930L)
    def pick[A](values: IndexedSeq[A]): A = values(random.nextInt(values.size))
    /** Half of the time the clean (first) value, otherwise any value. */
    def mostly[A](values: IndexedSeq[A]): A = if random.nextBoolean() then values.head else pick(values)
    val noises = Vector(RealizedNoise.White, RealizedNoise.FixedAr(1, NoiseScope.Global, None),
      RealizedNoise.FixedAr(1, NoiseScope.PerRun, Some(digest("s1"))), RealizedNoise.FixedAr(2, NoiseScope.Global, Some(digest("s2"))),
      RealizedNoise.EstimatedAr(1, NoiseScope.PerFeature), RealizedNoise.Robust, RealizedNoise.LearnedSubspace, RealizedNoise.Unrecorded)
    val combinations = Vector(RealizedCombination.SingleRun, RealizedCombination.FixedWeights(digest("w1")),
      RealizedCombination.FixedWeights(digest("w2")), RealizedCombination.EstimatedWeights, RealizedCombination.Unrecorded)
    val temporals = Vector(DeclaredTemporal.White, DeclaredTemporal.FixedSigmaT(digest("s1")), DeclaredTemporal.FixedSigmaT(digest("s2")))
    val declaredCombinations = Vector(DeclaredCombination.SingleRun, DeclaredCombination.FixedWeights(digest("w1")))
    val spatials = Vector(SpatialClaim.KroneckerSeparable, SpatialClaim.NonSeparable("x"), SpatialClaim.Unspecified)
    val origins = Vector(model.origin, ModelOrigin.InferredFromFit("fit"))
    val a = level("A"); val b = level("B"); val c = level("C")
    val actions = Vector(cycle, identityAction, ConditionAction.Permute(Map(a -> b, b -> a)),
      ConditionAction.Permute(Map(a -> b, b -> b, c -> a)), ConditionAction.Permute(Map(a -> level("Q"), b -> b, c -> c)))
    val absences = Vector(StatusAbsence.UnitHasNoEvidence, StatusAbsence.NoCoefficientEvidence, StatusAbsence.ColumnsDisagree,
      StatusAbsence.SelectedNotInferable(Vector(columns.head)), StatusAbsence.FitPlaneAbsent)
    val mutations: Vector[ResponseSourceBinding => ResponseSourceBinding] = Vector(identity, identity, identity,
      _.copy(unit = otherUnit), _.copy(observation = ObservationId("sub-02")), _.copy(design = digest("d2")),
      _.copy(preparation = digest("p2")), _.copy(noise = digest("n2")), _.copy(runCombination = digest("r2")),
      _.copy(readout = digest("o2")), _.copy(readoutAxis = axis(Vector("C", "B", "A"), 2)), _.copy(columns = columns.reverse),
      _.copy(selected = columns.take(1)), _.copy(features = digest("f2")), _.copy(realizedNoise = pick(noises)),
      _.copy(realizedCombination = pick(combinations)))
    var seen = Set.empty[String]
    var n = 0
    while n < 30000 do
      val expected = binding.copy(realizedNoise = mostly(noises), realizedCombination = mostly(combinations))
      val actual = mostly(mutations)(expected)
      val status: Option[StatusEvidence] = (if random.nextBoolean() then 7 else random.nextInt(8)) match
        case 0 => None
        case 1 => Some(StatusEvidence.Absent(pick(absences)))
        case _ => Some(StatusEvidence.Scanned(summary.copy(
          unit = if random.nextInt(12) == 0 then otherUnit else unit,
          features = if random.nextInt(12) == 0 then digest("f3") else features,
          plane = new InferenceStatusScope.Fit(if random.nextInt(12) == 0 then ObservationId("sub-03") else observation),
          counts = Map(InferenceStatusCode.Estimable -> 3L) ++
            (if random.nextInt(8) == 0 then Map(InferenceStatusCode.OutsideSupport -> 1L) else Map.empty) ++
            (if random.nextInt(4) == 0 then Map(InferenceStatusCode.Unrecorded -> 2L) else Map.empty),
          conditioning = if random.nextInt(4) == 0 then ScientificFact.Unknown("u") else summary.conditioning)))
      val declared = Option.when(random.nextInt(6) != 0)(DeclaredResponseModel(mostly(temporals), mostly(declaredCombinations),
        mostly(spatials), DistributionClaim.Gaussian, mostly(origins)))
      val req = ResponseActionRequest(if random.nextInt(20) == 0 then "v0" else ResponseActionEvidence.Version, expected, declared,
        mostly(actions), if random.nextInt(4) != 0 then request.nullConstraint else NullConstraint.Unencoded("label"))
      val result = ResponseActionEvidence.evaluate(req, actual, status)
      val all = ResponseActionOracle.failures(req, actual, status)
      assertEquals(ResponseActionOracle.key(result), ResponseActionOracle.key(all.head), s"case $n")
      assert(!result.isInstanceOf[Unavailable] || all.forall(_.isInstanceOf[Unavailable]), s"case $n")
      seen += result.productPrefix + (result match
        case Unavailable(reason) => reason.productPrefix
        case SourceMismatch(field, _, _) => field.toString
        case _ => "")
      n += 1
    // The generator reaches every stage and every non-status unavailable reason.
    Vector("UnsupportedVersion", "ModelMismatch", "ActionMismatch").foreach(stage => assert(seen.exists(_.startsWith(stage)), stage))
    SourceField.values.foreach(field => assert(seen.contains(s"SourceMismatch$field"), field.toString))
    Vector("InferenceStatus", "NoDeclaredModel", "ModelInferredFromFit", "NoiseIncomplete", "CombinationIncomplete",
      "EstimatedWhitening", "EstimatedRunWeights", "LearnedResponseSubspace", "ConditioningUnknown", "NonEstimableFeatures",
      "NullNotEncoded", "IdentityActionPolicy", "SpatialJointUnrepresented", "NoPositiveContractInVersion")
      .foreach(reason => assert(seen.contains(s"Unavailable$reason"), reason))

/** Collect-all reference written from the specification's decision tables. */
object ResponseActionOracle:
  private enum Kind:
    case Ok, Mismatch, Neutral

  /** (declared temporal, realized noise) → outcome. */
  private def temporal(declared: DeclaredTemporal, realized: RealizedNoise): Kind = (declared, realized) match
    case (DeclaredTemporal.White, RealizedNoise.White) => Kind.Ok
    case (DeclaredTemporal.White, RealizedNoise.Unrecorded) => Kind.Neutral
    case (DeclaredTemporal.White, _) => Kind.Mismatch
    case (DeclaredTemporal.FixedSigmaT(_), RealizedNoise.White) => Kind.Mismatch
    case (DeclaredTemporal.FixedSigmaT(s), RealizedNoise.FixedAr(_, _, Some(e))) => if s == e then Kind.Ok else Kind.Mismatch
    case (DeclaredTemporal.FixedSigmaT(_), _) => Kind.Neutral

  private def combination(declared: DeclaredCombination, realized: RealizedCombination): Kind = (declared, realized) match
    case (_, RealizedCombination.Unrecorded) => Kind.Neutral
    case (DeclaredCombination.SingleRun, RealizedCombination.SingleRun) => Kind.Ok
    case (DeclaredCombination.SingleRun, _) => Kind.Mismatch
    case (DeclaredCombination.FixedWeights(d), RealizedCombination.FixedWeights(r)) => if d == r then Kind.Ok else Kind.Mismatch
    case (DeclaredCombination.FixedWeights(_), RealizedCombination.EstimatedWeights) => Kind.Neutral
    case (DeclaredCombination.FixedWeights(_), RealizedCombination.SingleRun) => Kind.Mismatch

  def key(refusal: ResponseActionRefusal): Any = refusal match
    case SourceMismatch(field, _, _) => field
    case other => other

  def failures(req: ResponseActionRequest, a: ResponseSourceBinding, status: Option[StatusEvidence]): Vector[ResponseActionRefusal] =
    val e = req.expected
    val version = if req.version == ResponseActionEvidence.Version then Vector.empty else Vector(UnsupportedVersion(req.version))
    val sourcePairs: Vector[(SourceField, Any, Any)] = Vector((SourceField.Unit, e.unit, a.unit),
      (SourceField.Observation, e.observation, a.observation), (SourceField.Design, e.design, a.design),
      (SourceField.Preparation, e.preparation, a.preparation), (SourceField.Noise, e.noise, a.noise),
      (SourceField.RunCombination, e.runCombination, a.runCombination), (SourceField.Readout, e.readout, a.readout),
      (SourceField.ReadoutAxis, e.readoutAxis, a.readoutAxis), (SourceField.Columns, e.columns, a.columns),
      (SourceField.Selected, e.selected, a.selected), (SourceField.Features, e.features, a.features),
      (SourceField.RealizedNoise, e.realizedNoise, a.realizedNoise),
      (SourceField.RealizedCombination, e.realizedCombination, a.realizedCombination))
    val scopePairs: Vector[(SourceField, Any, Any)] = status match
      case Some(StatusEvidence.Scanned(s)) => Vector((SourceField.StatusUnit, a.unit, s.unit),
        (SourceField.StatusFeatures, a.features, s.features), (SourceField.StatusPlane, a.observation, s.plane.observation),
        (SourceField.StatusOutsideSupport, 0L, s.count(InferenceStatusCode.OutsideSupport)))
      case _ => Vector.empty
    // Rendering is checked by the table test; the oracle keys source refusals by field.
    val sourceRefusals = (sourcePairs ++ scopePairs).collect { case (field, x, y) if x != y => SourceMismatch(field, "", "") }
    val model = req.model.toVector.flatMap: m =>
      Vector(Option.when(temporal(m.temporal, a.realizedNoise) == Kind.Mismatch)(ModelMismatch(ModelDefect.TemporalMismatch(m.temporal, a.realizedNoise))),
        Option.when(combination(m.combination, a.realizedCombination) == Kind.Mismatch)(
          ModelMismatch(ModelDefect.CombinationMismatch(m.combination, a.realizedCombination)))).flatten
    val levels = a.readoutAxis.conditions.toSet
    val action = req.action match
      case ConditionAction.Permute(map) =>
        Vector(
          (map.keys.toVector.sortBy(_.value).find(!levels.contains(_)) orElse map.values.toVector.sortBy(_.value).find(!levels.contains(_)))
            .map(level => ActionMismatch(ActionDefect.UnknownLevel(level))),
          Option.when(!levels.subsetOf(map.keySet))(ActionMismatch(ActionDefect.LevelsIncomplete(a.readoutAxis.conditions.filterNot(map.contains)))),
          Option.when(map.values.toSet.size != map.size)(ActionMismatch(ActionDefect.NotBijection))).flatten
    val unavailable: Vector[UnavailableReason] = status match
      case None => Vector(InferenceStatus(StatusAbsence.NotScanned))
      case Some(StatusEvidence.Absent(reason)) => Vector(InferenceStatus(reason))
      case Some(StatusEvidence.Scanned(s)) =>
        req.model match
          case None => Vector(NoDeclaredModel)
          case Some(m) =>
            val nonEstimable = s.counts.collect { case (code, n) if code != InferenceStatusCode.Estimable && code != InferenceStatusCode.OutsideSupport => n }.sum
            Vector(
              Option.when(m.origin.isInstanceOf[ModelOrigin.InferredFromFit])(ModelInferredFromFit),
              Option.when(a.realizedNoise == RealizedNoise.Unrecorded || (a.realizedNoise match
                case RealizedNoise.FixedAr(_, _, exact) => exact.isEmpty
                case _ => false))(NoiseIncomplete),
              Option.when(a.realizedCombination == RealizedCombination.Unrecorded)(CombinationIncomplete),
              Option.when(a.realizedNoise == RealizedNoise.Robust || a.realizedNoise.isInstanceOf[RealizedNoise.EstimatedAr])(EstimatedWhitening),
              Option.when(a.realizedCombination == RealizedCombination.EstimatedWeights)(EstimatedRunWeights),
              Option.when(a.realizedNoise == RealizedNoise.LearnedSubspace)(LearnedResponseSubspace),
              Option.when(s.conditioning.isInstanceOf[ScientificFact.Unknown])(ConditioningUnknown),
              Option.when(nonEstimable > 0L)(NonEstimableFeatures(nonEstimable)),
              Option.when(req.nullConstraint.isInstanceOf[NullConstraint.Unencoded])(NullNotEncoded),
              Option.when(req.action.isIdentity)(IdentityActionPolicy),
              Option.when(m.spatial != SpatialClaim.KroneckerSeparable)(SpatialJointUnrepresented)).flatten
    version ++ sourceRefusals.take(1) ++ model ++ action ++ unavailable.map(Unavailable.apply) :+
      Unavailable(NoPositiveContractInVersion(req.version))
