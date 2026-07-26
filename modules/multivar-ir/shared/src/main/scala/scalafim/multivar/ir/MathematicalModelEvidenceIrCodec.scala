package scalafim.multivar.ir

object MathematicalModelEvidenceIrCodec:
  def encode(document: MathematicalModelEvidenceDocumentIr): String =
    IrJson.render(MathematicalModelEvidenceIrEncoder.document(document))

  def decode(text: String): Either[IrError, MathematicalModelEvidenceDocumentIr] =
    IrJson
      .parse(text)
      .flatMap(MathematicalModelEvidenceIrDecoder.document)
      .flatMap(MathematicalModelEvidenceIrValidator.validate)

private object MathematicalModelEvidenceIrEncoder:
  import IrJson.*

  def document(value: MathematicalModelEvidenceDocumentIr): IrJson =
    obj(
      "schema" -> Str(value.schema),
      "models" -> arr(value.models.map(model))
    )

  private def model(value: MathematicalModelEvidenceIr): IrJson =
    obj(
      "id" -> Str(value.id),
      "contract_id" -> Str(value.contractId),
      "family" -> Str(tag(value.family)),
      "estimand" -> Str(tag(value.estimand)),
      "operator_program_schema" -> Str(value.operatorProgramSchema),
      "program_id" -> Str(value.programId),
      "data_identity" -> Str(value.dataIdentity),
      "mask" -> mask(value.mask),
      "losses" -> arr(value.losses.map(lossBinding)),
      "geometries" -> arr(value.geometries.map(geometry)),
      "penalties" -> arr(value.penalties.map(penalty)),
      "assumptions" -> arr(value.assumptions.map(assumption)),
      "solver" -> solver(value.solver),
      "achieved_guarantee" -> guarantee(value.achievedGuarantee),
      "certificate_identities" -> strings(value.certificateIdentities),
      "reproducibility" -> reproducibility(value.reproducibility)
    )

  private def lossBinding(value: LossBindingEvidenceIr): IrJson =
    obj("feature_domain_identity" -> Str(value.featureDomainIdentity), "loss" -> loss(value.loss))

  private def loss(value: EntryLossEvidenceIr): IrJson =
    value match
      case EntryLossEvidenceIr.HalfSquared => tagged("half_squared")
      case EntryLossEvidenceIr.Huber(delta) => obj("kind" -> Str("huber"), "delta" -> Num(delta))
      case EntryLossEvidenceIr.BernoulliLogistic => tagged("bernoulli_logistic")
      case EntryLossEvidenceIr.PoissonLogLink => tagged("poisson_log_link")
      case EntryLossEvidenceIr.CumulativeOrdinal(levels, order) =>
        obj("kind" -> Str("cumulative_ordinal"), "levels" -> Num(levels), "order_identity" -> Str(order))
      case EntryLossEvidenceIr.Softmax(levels) => obj("kind" -> Str("softmax"), "levels" -> Num(levels))

  private def mask(value: ObservationMaskEvidenceIr): IrJson =
    value match
      case ObservationMaskEvidenceIr.Complete(identity) =>
        obj("kind" -> Str("complete"), "observation_identity" -> Str(identity))
      case ObservationMaskEvidenceIr.Explicit(identity, count, target) =>
        obj(
          "kind" -> Str("explicit"),
          "mask_identity" -> Str(identity),
          "observed_count" -> Num(count),
          "target" -> missingness(target)
        )
      case ObservationMaskEvidenceIr.Censored(identity, count, likelihood, target) =>
        obj(
          "kind" -> Str("censored"),
          "mask_identity" -> Str(identity),
          "observed_count" -> Num(count),
          "likelihood_identity" -> Str(likelihood),
          "target" -> missingness(target)
        )

  private def missingness(value: MissingnessTargetEvidenceIr): IrJson =
    value match
      case MissingnessTargetEvidenceIr.FixedMask => tagged("fixed_mask")
      case MissingnessTargetEvidenceIr.McarSimulation(identity) =>
        obj("kind" -> Str("mcar_simulation"), "generator_identity" -> Str(identity))
      case MissingnessTargetEvidenceIr.MarSensitivity(identity) =>
        obj("kind" -> Str("mar_sensitivity"), "mechanism_identity" -> Str(identity))
      case MissingnessTargetEvidenceIr.MnarSensitivity(identity) =>
        obj("kind" -> Str("mnar_sensitivity"), "selection_model_identity" -> Str(identity))

  private def geometry(value: GeometryBindingEvidenceIr): IrJson =
    obj(
      "role" -> Str(tag(value.role)),
      "operator_identity" -> Str(value.operatorIdentity),
      "certificate_identity" -> Str(value.certificateIdentity)
    )

  private def penalty(value: PenaltyBindingEvidenceIr): IrJson =
    obj(
      "owner" -> penaltyOwner(value.owner),
      "functional" -> Str(value.functional.stableKey),
      "weight" -> Num(value.weight),
      "operator_identity" -> value.operatorIdentity.fold[IrJson](Null)(Str.apply)
    )

  private def penaltyOwner(value: PenaltyOwnerEvidenceIr): IrJson =
    value match
      case PenaltyOwnerEvidenceIr.BlockDecoder(identity) =>
        obj("kind" -> Str("block_decoder"), "block_identity" -> Str(identity))
      case other => tagged(tag(other))

  private def assumption(value: TheoremAssumptionEvidenceIr): IrJson =
    obj(
      "theorem_id" -> Str(value.theoremId),
      "assumption_id" -> Str(value.assumptionId),
      "witness_identity" -> Str(value.witnessIdentity)
    )

  private def solver(value: SolverReceiptEvidenceIr): IrJson =
    obj(
      "family" -> Str(tag(value.family)),
      "implementation_version" -> Str(value.implementationVersion),
      "policy_identity" -> Str(value.policyIdentity),
      "trace_identity" -> Str(value.traceIdentity),
      "tolerance" -> tolerance(value.tolerance),
      "iteration_count" -> Num(value.iterationCount)
    )

  private def guarantee(value: AchievedGuaranteeEvidenceIr): IrJson =
    value match
      case AchievedGuaranteeEvidenceIr.ExactGlobal(identity) =>
        obj("kind" -> Str("exact_global"), "certificate_identity" -> Str(identity))
      case AchievedGuaranteeEvidenceIr.EpsilonGlobal(gap, identity) =>
        obj("kind" -> Str("epsilon_global"), "gap" -> Num(gap), "certificate_identity" -> Str(identity))
      case AchievedGuaranteeEvidenceIr.UniqueMinimizer(distance, identity) =>
        obj(
          "kind" -> Str("unique_minimizer"),
          "distance_bound" -> Num(distance),
          "certificate_identity" -> Str(identity)
        )
      case AchievedGuaranteeEvidenceIr.Stationary(residual, identity) =>
        obj("kind" -> Str("stationary"), "residual" -> Num(residual), "certificate_identity" -> Str(identity))
      case AchievedGuaranteeEvidenceIr.CoordinatewiseStationary(residuals, identity) =>
        obj(
          "kind" -> Str("coordinatewise_stationary"),
          "residuals" -> arr(residuals.map(Num.apply)),
          "certificate_identity" -> Str(identity)
        )
      case AchievedGuaranteeEvidenceIr.Feasible(residual, identity) =>
        obj("kind" -> Str("feasible"), "residual" -> Num(residual), "certificate_identity" -> Str(identity))
      case AchievedGuaranteeEvidenceIr.Unresolved(reason) =>
        obj("kind" -> Str("unresolved"), "reason" -> Str(reason))

  private def reproducibility(value: ReproducibilityReceiptIr): IrJson =
    obj(
      "generator_identity" -> Str(value.generatorIdentity),
      "seed" -> Num(value.seed.toDouble),
      "dependencies" -> arr(value.dependencies.map: dependency =>
        obj("name" -> Str(dependency.name), "version" -> Str(dependency.version))
      ),
      "condition_estimate" -> Num(value.conditionEstimate),
      "tolerance" -> tolerance(value.tolerance),
      "result_identity" -> Str(value.resultIdentity)
    )

  private def tolerance(value: ToleranceIr): IrJson =
    obj("absolute" -> Num(value.absolute), "relative" -> Num(value.relative))

  private def strings(values: Vector[String]): IrJson = arr(values.map(Str.apply))

  private def tagged(kind: String): IrJson = obj("kind" -> Str(kind))

  private def obj(fields: (String, IrJson)*): IrJson = Obj(fields.toVector)

  private def arr(values: Vector[IrJson]): IrJson = Arr(values)

  private def tag(value: Product): String =
    value.productPrefix.zipWithIndex.flatMap: (character, index) =>
      val prefix = if character.isUpper && index > 0 then "_" else ""
      prefix + character.toLower
    .mkString

private object MathematicalModelEvidenceIrDecoder:
  import IrJson.*

  def document(value: IrJson): Either[IrError, MathematicalModelEvidenceDocumentIr] =
    for
      current <- fields(value, "$", Set("schema", "models"))
      schema <- required(current, "schema", "$", string(_, "$.schema"))
      models <- required(current, "models", "$", vector(_, "$.models", model))
    yield MathematicalModelEvidenceDocumentIr(schema, models)

  private def model(value: IrJson, path: String): Either[IrError, MathematicalModelEvidenceIr] =
    val names = Set(
      "id",
      "contract_id",
      "family",
      "estimand",
      "operator_program_schema",
      "program_id",
      "data_identity",
      "mask",
      "losses",
      "geometries",
      "penalties",
      "assumptions",
      "solver",
      "achieved_guarantee",
      "certificate_identities",
      "reproducibility"
    )
    for
      current <- fields(value, path, names)
      id <- required(current, "id", path, string(_, s"$path.id"))
      contract <- required(current, "contract_id", path, string(_, s"$path.contract_id"))
      family <- required(current, "family", path, enumValue(_, s"$path.family", ModelFamilyEvidenceIr.values.toVector))
      estimand <- required(current, "estimand", path, enumValue(_, s"$path.estimand", ModelEstimandEvidenceIr.values.toVector))
      programSchema <- required(current, "operator_program_schema", path, string(_, s"$path.operator_program_schema"))
      program <- required(current, "program_id", path, string(_, s"$path.program_id"))
      data <- required(current, "data_identity", path, string(_, s"$path.data_identity"))
      currentMask <- required(current, "mask", path, mask(_, s"$path.mask"))
      losses <- required(current, "losses", path, vector(_, s"$path.losses", lossBinding))
      geometries <- required(current, "geometries", path, vector(_, s"$path.geometries", geometry))
      penalties <- required(current, "penalties", path, vector(_, s"$path.penalties", penalty))
      assumptions <- required(current, "assumptions", path, vector(_, s"$path.assumptions", assumption))
      currentSolver <- required(current, "solver", path, solver(_, s"$path.solver"))
      achieved <- required(current, "achieved_guarantee", path, guarantee(_, s"$path.achieved_guarantee"))
      certificates <- required(current, "certificate_identities", path, vector(_, s"$path.certificate_identities", string))
      receipt <- required(current, "reproducibility", path, reproducibility(_, s"$path.reproducibility"))
    yield MathematicalModelEvidenceIr(
      id,
      contract,
      family,
      estimand,
      programSchema,
      program,
      data,
      currentMask,
      losses,
      geometries,
      penalties,
      assumptions,
      currentSolver,
      achieved,
      certificates,
      receipt
    )

  private def lossBinding(value: IrJson, path: String): Either[IrError, LossBindingEvidenceIr] =
    for
      current <- fields(value, path, Set("feature_domain_identity", "loss"))
      domain <- required(current, "feature_domain_identity", path, string(_, s"$path.feature_domain_identity"))
      currentLoss <- required(current, "loss", path, loss(_, s"$path.loss"))
    yield LossBindingEvidenceIr(domain, currentLoss)

  private def loss(value: IrJson, path: String): Either[IrError, EntryLossEvidenceIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "half_squared" => exact(current, path, Set("kind")).map(_ => EntryLossEvidenceIr.HalfSquared)
        case "huber" =>
          exact(current, path, Set("kind", "delta"))
            .flatMap(value => required(value, "delta", path, number(_, s"$path.delta")))
            .map(EntryLossEvidenceIr.Huber.apply)
        case "bernoulli_logistic" => exact(current, path, Set("kind")).map(_ => EntryLossEvidenceIr.BernoulliLogistic)
        case "poisson_log_link" => exact(current, path, Set("kind")).map(_ => EntryLossEvidenceIr.PoissonLogLink)
        case "cumulative_ordinal" =>
          for
            checked <- exact(current, path, Set("kind", "levels", "order_identity"))
            levels <- required(checked, "levels", path, integer(_, s"$path.levels"))
            order <- required(checked, "order_identity", path, string(_, s"$path.order_identity"))
          yield EntryLossEvidenceIr.CumulativeOrdinal(levels, order)
        case "softmax" =>
          exact(current, path, Set("kind", "levels"))
            .flatMap(value => required(value, "levels", path, integer(_, s"$path.levels")))
            .map(EntryLossEvidenceIr.Softmax.apply)
        case _ => malformed(path, s"unknown entry loss '$kind'")

  private def mask(value: IrJson, path: String): Either[IrError, ObservationMaskEvidenceIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "complete" =>
          exact(current, path, Set("kind", "observation_identity"))
            .flatMap(value => required(value, "observation_identity", path, string(_, s"$path.observation_identity")))
            .map(ObservationMaskEvidenceIr.Complete.apply)
        case "explicit" =>
          for
            checked <- exact(current, path, Set("kind", "mask_identity", "observed_count", "target"))
            identity <- required(checked, "mask_identity", path, string(_, s"$path.mask_identity"))
            count <- required(checked, "observed_count", path, integer(_, s"$path.observed_count"))
            target <- required(checked, "target", path, missingness(_, s"$path.target"))
          yield ObservationMaskEvidenceIr.Explicit(identity, count, target)
        case "censored" =>
          for
            checked <- exact(current, path, Set("kind", "mask_identity", "observed_count", "likelihood_identity", "target"))
            identity <- required(checked, "mask_identity", path, string(_, s"$path.mask_identity"))
            count <- required(checked, "observed_count", path, integer(_, s"$path.observed_count"))
            likelihood <- required(checked, "likelihood_identity", path, string(_, s"$path.likelihood_identity"))
            target <- required(checked, "target", path, missingness(_, s"$path.target"))
          yield ObservationMaskEvidenceIr.Censored(identity, count, likelihood, target)
        case _ => malformed(path, s"unknown observation mask '$kind'")

  private def missingness(value: IrJson, path: String): Either[IrError, MissingnessTargetEvidenceIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "fixed_mask" => exact(current, path, Set("kind")).map(_ => MissingnessTargetEvidenceIr.FixedMask)
        case "mcar_simulation" =>
          taggedIdentity(current, path, "generator_identity").map(MissingnessTargetEvidenceIr.McarSimulation.apply)
        case "mar_sensitivity" =>
          taggedIdentity(current, path, "mechanism_identity").map(MissingnessTargetEvidenceIr.MarSensitivity.apply)
        case "mnar_sensitivity" =>
          taggedIdentity(current, path, "selection_model_identity").map(MissingnessTargetEvidenceIr.MnarSensitivity.apply)
        case _ => malformed(path, s"unknown missingness target '$kind'")

  private def geometry(value: IrJson, path: String): Either[IrError, GeometryBindingEvidenceIr] =
    for
      current <- fields(value, path, Set("role", "operator_identity", "certificate_identity"))
      role <- required(current, "role", path, enumValue(_, s"$path.role", GeometryRoleEvidenceIr.values.toVector))
      operator <- required(current, "operator_identity", path, string(_, s"$path.operator_identity"))
      certificate <- required(current, "certificate_identity", path, string(_, s"$path.certificate_identity"))
    yield GeometryBindingEvidenceIr(role, operator, certificate)

  private def penalty(value: IrJson, path: String): Either[IrError, PenaltyBindingEvidenceIr] =
    for
      current <- fields(value, path, Set("owner", "functional", "weight", "operator_identity"))
      owner <- required(current, "owner", path, penaltyOwner(_, s"$path.owner"))
      functional <- required(current, "functional", path, penaltyFunctional(_, s"$path.functional"))
      weight <- required(current, "weight", path, number(_, s"$path.weight"))
      operator <- required(current, "operator_identity", path, nullableString(_, s"$path.operator_identity"))
    yield PenaltyBindingEvidenceIr(owner, functional, weight, operator)

  private def penaltyFunctional(value: IrJson, path: String): Either[IrError, PenaltyFunctionalEvidenceIr] =
    string(value, path).flatMap: stableKey =>
      PenaltyFunctionalEvidenceIr
        .fromStableKey(stableKey)
        .toRight(IrError(RejectionCategory.Malformed, path, s"unknown penalty functional '$stableKey'"))

  private def penaltyOwner(value: IrJson, path: String): Either[IrError, PenaltyOwnerEvidenceIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "block_decoder" => taggedIdentity(current, path, "block_identity").map(PenaltyOwnerEvidenceIr.BlockDecoder.apply)
        case _ =>
          exact(current, path, Set("kind")).flatMap: _ =>
            enumTag(
              kind,
              path,
              Vector(
                PenaltyOwnerEvidenceIr.RowFactor,
                PenaltyOwnerEvidenceIr.ColumnFactor,
                PenaltyOwnerEvidenceIr.SharedRows,
                PenaltyOwnerEvidenceIr.ConvexMatrix
              )
            )

  private def assumption(value: IrJson, path: String): Either[IrError, TheoremAssumptionEvidenceIr] =
    for
      current <- fields(value, path, Set("theorem_id", "assumption_id", "witness_identity"))
      theorem <- required(current, "theorem_id", path, string(_, s"$path.theorem_id"))
      assumption <- required(current, "assumption_id", path, string(_, s"$path.assumption_id"))
      witness <- required(current, "witness_identity", path, string(_, s"$path.witness_identity"))
    yield TheoremAssumptionEvidenceIr(theorem, assumption, witness)

  private def solver(value: IrJson, path: String): Either[IrError, SolverReceiptEvidenceIr] =
    for
      current <- fields(
        value,
        path,
        Set("family", "implementation_version", "policy_identity", "trace_identity", "tolerance", "iteration_count")
      )
      family <- required(current, "family", path, enumValue(_, s"$path.family", SolverFamilyEvidenceIr.values.toVector))
      version <- required(current, "implementation_version", path, string(_, s"$path.implementation_version"))
      policy <- required(current, "policy_identity", path, string(_, s"$path.policy_identity"))
      trace <- required(current, "trace_identity", path, string(_, s"$path.trace_identity"))
      currentTolerance <- required(current, "tolerance", path, tolerance(_, s"$path.tolerance"))
      iterations <- required(current, "iteration_count", path, integer(_, s"$path.iteration_count"))
    yield SolverReceiptEvidenceIr(family, version, policy, trace, currentTolerance, iterations)

  private def guarantee(value: IrJson, path: String): Either[IrError, AchievedGuaranteeEvidenceIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "exact_global" =>
          certificateOnly(current, path).map(AchievedGuaranteeEvidenceIr.ExactGlobal.apply)
        case "epsilon_global" =>
          quantitative(current, path, "gap").map((value, identity) => AchievedGuaranteeEvidenceIr.EpsilonGlobal(value, identity))
        case "unique_minimizer" =>
          quantitative(current, path, "distance_bound").map((value, identity) => AchievedGuaranteeEvidenceIr.UniqueMinimizer(value, identity))
        case "stationary" =>
          quantitative(current, path, "residual").map((value, identity) => AchievedGuaranteeEvidenceIr.Stationary(value, identity))
        case "coordinatewise_stationary" =>
          for
            checked <- exact(current, path, Set("kind", "residuals", "certificate_identity"))
            residuals <- required(checked, "residuals", path, vector(_, s"$path.residuals", number))
            identity <- required(checked, "certificate_identity", path, string(_, s"$path.certificate_identity"))
          yield AchievedGuaranteeEvidenceIr.CoordinatewiseStationary(residuals, identity)
        case "feasible" =>
          quantitative(current, path, "residual").map((value, identity) => AchievedGuaranteeEvidenceIr.Feasible(value, identity))
        case "unresolved" =>
          exact(current, path, Set("kind", "reason"))
            .flatMap(value => required(value, "reason", path, string(_, s"$path.reason")))
            .map(AchievedGuaranteeEvidenceIr.Unresolved.apply)
        case _ => malformed(path, s"unknown achieved guarantee '$kind'")

  private def reproducibility(value: IrJson, path: String): Either[IrError, ReproducibilityReceiptIr] =
    for
      current <- fields(
        value,
        path,
        Set("generator_identity", "seed", "dependencies", "condition_estimate", "tolerance", "result_identity")
      )
      generator <- required(current, "generator_identity", path, string(_, s"$path.generator_identity"))
      seed <- required(current, "seed", path, long(_, s"$path.seed"))
      dependencies <- required(current, "dependencies", path, vector(_, s"$path.dependencies", dependency))
      condition <- required(current, "condition_estimate", path, number(_, s"$path.condition_estimate"))
      currentTolerance <- required(current, "tolerance", path, tolerance(_, s"$path.tolerance"))
      result <- required(current, "result_identity", path, string(_, s"$path.result_identity"))
    yield ReproducibilityReceiptIr(generator, seed, dependencies, condition, currentTolerance, result)

  private def dependency(value: IrJson, path: String): Either[IrError, DependencyVersionEvidenceIr] =
    for
      current <- fields(value, path, Set("name", "version"))
      name <- required(current, "name", path, string(_, s"$path.name"))
      version <- required(current, "version", path, string(_, s"$path.version"))
    yield DependencyVersionEvidenceIr(name, version)

  private def tolerance(value: IrJson, path: String): Either[IrError, ToleranceIr] =
    for
      current <- fields(value, path, Set("absolute", "relative"))
      absolute <- required(current, "absolute", path, number(_, s"$path.absolute"))
      relative <- required(current, "relative", path, number(_, s"$path.relative"))
    yield ToleranceIr(absolute, relative)

  private def certificateOnly(current: Map[String, IrJson], path: String): Either[IrError, String] =
    exact(current, path, Set("kind", "certificate_identity"))
      .flatMap(value => required(value, "certificate_identity", path, string(_, s"$path.certificate_identity")))

  private def quantitative(
      current: Map[String, IrJson],
      path: String,
      field: String
  ): Either[IrError, (Double, String)] =
    for
      checked <- exact(current, path, Set("kind", field, "certificate_identity"))
      value <- required(checked, field, path, number(_, s"$path.$field"))
      identity <- required(checked, "certificate_identity", path, string(_, s"$path.certificate_identity"))
    yield (value, identity)

  private def taggedIdentity(
      current: Map[String, IrJson],
      path: String,
      field: String
  ): Either[IrError, String] =
    exact(current, path, Set("kind", field)).flatMap(value => required(value, field, path, string(_, s"$path.$field")))

  private def tagged(value: IrJson, path: String): Either[IrError, (String, Map[String, IrJson])] =
    fields(value, path, None).flatMap: current =>
      required(current, "kind", path, string(_, s"$path.kind")).map(kind => (kind, current))

  private def exact(
      current: Map[String, IrJson],
      path: String,
      allowed: Set[String]
  ): Either[IrError, Map[String, IrJson]] =
    current.keys.find(name => !allowed.contains(name)) match
      case Some(name) => Left(IrError(RejectionCategory.UnknownField, s"$path.$name", "unknown field"))
      case None => Right(current)

  private def fields(
      value: IrJson,
      path: String,
      allowed: Set[String]
  ): Either[IrError, Map[String, IrJson]] = fields(value, path, Some(allowed))

  private def fields(
      value: IrJson,
      path: String,
      allowed: Option[Set[String]]
  ): Either[IrError, Map[String, IrJson]] =
    value match
      case Obj(values) =>
        val current = values.toMap
        allowed.flatMap(names => current.keys.find(name => !names.contains(name))) match
          case Some(name) => Left(IrError(RejectionCategory.UnknownField, s"$path.$name", "unknown field"))
          case None => Right(current)
      case _ => malformed(path, "expected object")

  private def required[A](
      fields: Map[String, IrJson],
      name: String,
      path: String,
      decode: IrJson => Either[IrError, A]
  ): Either[IrError, A] =
    fields.get(name).toRight(IrError(RejectionCategory.Malformed, s"$path.$name", "missing required field")).flatMap(decode)

  private def vector[A](
      value: IrJson,
      path: String,
      decode: (IrJson, String) => Either[IrError, A]
  ): Either[IrError, Vector[A]] =
    value match
      case Arr(values) =>
        values.zipWithIndex.foldLeft[Either[IrError, Vector[A]]](Right(Vector.empty)): (result, indexed) =>
          result.flatMap: collected =>
            decode(indexed._1, s"$path[${indexed._2}]").map(collected :+ _)
      case _ => malformed(path, "expected array")

  private def string(value: IrJson, path: String): Either[IrError, String] =
    value match
      case Str(current) => Right(current)
      case _ => malformed(path, "expected string")

  private def nullableString(value: IrJson, path: String): Either[IrError, Option[String]] =
    value match
      case Null => Right(None)
      case Str(current) => Right(Some(current))
      case _ => malformed(path, "expected string or null")

  private def number(value: IrJson, path: String): Either[IrError, Double] =
    value match
      case Num(current) => Right(current)
      case _ => malformed(path, "expected number")

  private def integer(value: IrJson, path: String): Either[IrError, Int] =
    number(value, path).flatMap: current =>
      if current == Math.rint(current) && current >= Int.MinValue && current <= Int.MaxValue then Right(current.toInt)
      else malformed(path, "expected integer")

  private def long(value: IrJson, path: String): Either[IrError, Long] =
    number(value, path).flatMap: current =>
      if current == Math.rint(current) && Math.abs(current) <= 9007199254740991.0 then Right(current.toLong)
      else malformed(path, "expected exactly representable integer")

  private def enumValue[A <: Product](value: IrJson, path: String, values: Vector[A]): Either[IrError, A] =
    string(value, path).flatMap(tag => enumTag(tag, path, values))

  private def enumTag[A <: Product](tag: String, path: String, values: Vector[A]): Either[IrError, A] =
    values.find(value => encodeTag(value) == tag).toRight(
      IrError(RejectionCategory.Malformed, path, s"unknown enum tag '$tag'")
    )

  private def encodeTag(value: Product): String =
    value.productPrefix.zipWithIndex.flatMap: (character, index) =>
      val prefix = if character.isUpper && index > 0 then "_" else ""
      prefix + character.toLower
    .mkString

  private def malformed[A](path: String, detail: String): Either[IrError, A] =
    Left(IrError(RejectionCategory.Malformed, path, detail))
