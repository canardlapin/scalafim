package scalafim.fmri.model

import scalafim.fmri.design.*
import scalafim.fmri.design.baseline.*
import scalafim.fmri.design.contrast.ContrastSpec
import scalafim.fmri.design.formula.{FormulaParser, ModelJsonCodec, ModelJsonError}
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.linalg.Mat
import scala.util.control.NonFatal
import ujson.{Arr, Bool, Null, Num, Obj, Str, Value}

/** Portable build profile. Unsupported runtime configuration fails explicitly;
  * it is never silently replaced with a default on either encode or decode.
  */
object ModelBuildSpecJsonCodec:
  private final case class Invalid(path: String, detail: String) extends IllegalArgumentException(detail)
  private def fail(path: String, detail: String): Nothing = throw Invalid(path, detail)
  private def checked[A](body: => A): Either[ModelJsonError, A] =
    try Right(body)
    catch
      case Invalid(path, detail) => Left(ModelJsonError(path, detail))
      case NonFatal(e) => Left(ModelJsonError("$", Option(e.getMessage).getOrElse("invalid model specification")))
  private def fields(value: Value, expected: Set[String], path: String): Unit =
    if value.obj.keySet.toSet != expected then fail(path, "missing or unknown fields")
  private def requirePortable(condition: Boolean, path: String): Unit =
    if !condition then fail(path, "not supported by portable build profile v1")
  private def admit[A, E](value: Either[E, A], path: String)(message: E => String): A =
    value.fold(error => fail(path, message(error)), identity)
  private def opt(value: Option[String]): Value = value.fold[Value](Null)(Str(_))
  private def readOpt(value: Value): Option[String] = if value == Null then None else Some(value.str)
  private def number(value: Double): Value =
    if !value.isFinite then fail("$", "non-finite number")
    Num(value)
  private def integer(value: Value): Int =
    if !value.num.isValidInt then fail("$", "expected integer")
    value.num.toInt
  private def baseline(value: BaselineBasis): Value = value match
    case BaselineBasis.Dct(cutoff) => Obj("kind" -> Str("dct"), "cutoff" -> number(cutoff.seconds))
    case other => Obj("kind" -> Str(other.id))
  private def readBaseline(value: Value): BaselineBasis = value("kind").str match
    case "dct" =>
      fields(value, Set("kind", "cutoff"), "$.baseline")
      BaselineBasis.Dct(admit(DctCutoffPeriod.fromSeconds(value("cutoff").num), "$.baseline.cutoff")(_.message))
    case other =>
      fields(value, Set("kind"), "$.baseline")
      BaselineBasis.parse(other)
  private def arCoefficients(value: ArCoefficientSpec): Value = value match
    case ArCoefficientSpec.Estimate => Obj("kind" -> Str("estimate"))
    case ArCoefficientSpec.Rho(rho) => Obj("kind" -> Str("rho"), "value" -> number(rho))
    case ArCoefficientSpec.Phi(values) => Obj("kind" -> Str("phi"), "values" -> Arr.from(values.map(number)))
  private def readArCoefficients(value: Value): ArCoefficientSpec = value("kind").str match
    case "estimate" => fields(value, Set("kind"), "$.strategy.ar.coefficients"); ArCoefficientSpec.Estimate
    case "rho" =>
      fields(value, Set("kind", "value"), "$.strategy.ar.coefficients")
      admit(ArCoefficientSpec.rho(value("value").num), "$.strategy.ar.coefficients")(_.message)
    case "phi" =>
      fields(value, Set("kind", "values"), "$.strategy.ar.coefficients")
      ArCoefficientSpec.Phi(value("values").arr.map(_.num).toVector)
    case other => fail("$.strategy.ar.coefficients", s"unknown AR coefficient policy $other")
  private def autocorrelation(value: AutocorrelationConfig): Value =
    Obj("order" -> Num(value.order.value), "iterations" -> Num(value.iterations), "global" -> Bool(value.global),
      "voxelwise" -> Bool(value.voxelwise), "exactFirst" -> Bool(value.exactFirst),
      "censoredTimepoints" -> Arr.from(value.censoredTimepoints.values.map(Num(_))), "coefficients" -> arCoefficients(value.coefficients))
  private def readAutocorrelation(value: Value): AutocorrelationConfig =
    fields(value, Set("order", "iterations", "global", "voxelwise", "exactFirst", "censoredTimepoints", "coefficients"), "$.strategy.ar")
    admit(AutocorrelationConfig(
      order = integer(value("order")), iterations = integer(value("iterations")), global = value("global").bool,
      voxelwise = value("voxelwise").bool, exactFirst = value("exactFirst").bool,
      censoredTimepoints = value("censoredTimepoints").arr.map(integer).toVector,
      coefficients = readArCoefficients(value("coefficients"))
    ), "$.strategy.ar")(_.message)
  private def lss(value: LssStrategyConfig): Value =
    Obj("trialTerm" -> opt(value.trialTerm.map(_.value)), "eps" -> number(value.eps), "rankTol" -> number(value.rankTol))
  private def readLss(value: Value): LssStrategyConfig =
    fields(value, Set("trialTerm", "eps", "rankTol"), "$.strategy.lss")
    LssStrategyConfig(value("trialTerm") match
      case Null => None
      case term => Some(term.str), value("eps").num, value("rankTol").num
    ).fold(error => fail("$.strategy.lss", error.message), identity)
  private def strategy(value: FitStrategy): Value = value match
    case FitStrategy.OrdinaryLeastSquares(controls) if controls == FitControls() => Str("ols")
    case FitStrategy.RunwiseLeastSquares(controls) if controls == FitControls() => Str("runwise-ols")
    case FitStrategy.SeparateRunsThenFixedEffects(controls) if controls == FitControls() => Str("fixed-effects-ols")
    case FitStrategy.GeneralizedLeastSquares(ar, controls) if controls == FitControls() => Obj("kind" -> Str("gls"), "ar" -> autocorrelation(ar))
    case FitStrategy.RunwiseGeneralizedLeastSquares(ar, controls) if controls == FitControls() => Obj("kind" -> Str("runwise-gls"), "ar" -> autocorrelation(ar))
    case FitStrategy.LeastSquaresSeparate(config, controls) if controls == FitControls() => Obj("kind" -> Str("lss"), "lss" -> lss(config))
    case _ => fail("$.strategy", "this strategy or its controls requires an extended portable profile")
  private def readStrategy(value: Value): FitStrategy = value match
    case Str("ols") => FitStrategy.OrdinaryLeastSquares()
    case Str("runwise-ols") => FitStrategy.RunwiseLeastSquares()
    case Str("fixed-effects-ols") => FitStrategy.SeparateRunsThenFixedEffects()
    case _ =>
      value("kind").str match
        case "gls" => fields(value, Set("kind", "ar"), "$.strategy"); FitStrategy.GeneralizedLeastSquares(readAutocorrelation(value("ar")))
        case "runwise-gls" => fields(value, Set("kind", "ar"), "$.strategy"); FitStrategy.RunwiseGeneralizedLeastSquares(readAutocorrelation(value("ar")))
        case "lss" => fields(value, Set("kind", "lss"), "$.strategy"); FitStrategy.LeastSquaresSeparate(readLss(value("lss")))
        case other => fail("$.strategy", s"unknown strategy $other")
  private def missing(value: MissingValuePolicy): Value = value match
    case MissingValuePolicy.ImputeConstant(v) => Obj("kind" -> Str("impute"), "value" -> number(v))
    case other => Obj("kind" -> Str(other.label))
  private def readMissing(value: Value): MissingValuePolicy = value("kind").str match
    case "reject" => fields(value, Set("kind"), "$.missingValuePolicy"); MissingValuePolicy.Reject
    case "zero-contribution" => fields(value, Set("kind"), "$.missingValuePolicy"); MissingValuePolicy.ZeroContribution
    case "drop-from-term" => fields(value, Set("kind"), "$.missingValuePolicy"); MissingValuePolicy.DropFromTerm
    case "impute" =>
      fields(value, Set("kind", "value"), "$.missingValuePolicy")
      val n = value("value").num
      val _ = number(n)
      MissingValuePolicy.ImputeConstant(n)
    case other => fail("$.missingValuePolicy", s"unknown policy $other")
  private def orthogonalization(value: ModulatorOrthogonalizationPlan): Value = Arr.from(value.policies.map: p =>
    val scope = p.scope match
      case OrthogonalizationScope.WholeTerm => Obj("kind" -> Str("term"))
      case OrthogonalizationScope.WithinRun => Obj("kind" -> Str("run"))
      case OrthogonalizationScope.WithinCells(factors) => Obj("kind" -> Str("cells"), "factors" -> Arr.from(factors.map(_.value)))
    Obj("term" -> Str(p.term.value), "order" -> Arr.from(p.order.map(_.value)), "scope" -> scope,
      "degenerate" -> Str(p.degenerate.toString), "tolerance" -> number(p.tolerance))
  )
  private def readOrthogonalization(value: Value): ModulatorOrthogonalizationPlan =
    val policies = value.arr.toVector.map: p =>
      fields(p, Set("term", "order", "scope", "degenerate", "tolerance"), "$.orthogonalization")
      val scope = p("scope")("kind").str match
        case "term" => fields(p("scope"), Set("kind"), "$.orthogonalization.scope"); OrthogonalizationScope.WholeTerm
        case "run" => fields(p("scope"), Set("kind"), "$.orthogonalization.scope"); OrthogonalizationScope.WithinRun
        case "cells" =>
          fields(p("scope"), Set("kind", "factors"), "$.orthogonalization.scope")
          OrthogonalizationScope.WithinCells(p("scope")("factors").arr.map(x => admit(FactorId(x.str), "$.orthogonalization.scope")(_.message)).toVector)
        case other => fail("$.orthogonalization.scope", s"unknown scope $other")
      val term = admit(TermId(p("term").str), "$.orthogonalization.term")(_.message)
      val order = p("order").arr.map(x => admit(ModulatorId(x.str), "$.orthogonalization.order")(_.message)).toVector
      admit(ModulatorOrthogonalization.ordered(term, order, scope, DegenerateModulatorPolicy.valueOf(p("degenerate").str), p("tolerance").num), "$.orthogonalization")(_.message)
    admit(ModulatorOrthogonalizationPlan.of(policies*), "$.orthogonalization")(_.message)
  private def nuisance(value: Option[NuisanceRegressors]): Value = value.fold[Value](Null): n =>
    Obj("matrices" -> Arr.from(n.matrices.map(m => Obj("rows" -> Num(m.rows), "cols" -> Num(m.cols), "values" -> Arr.from(m.data.map(number))))),
      "names" -> n.names.fold[Value](Null)(blocks => Arr.from(blocks.map(Arr.from(_)))),
      "check" -> Str(n.check.toString), "naAction" -> Str(n.naAction.toString),
      "tol" -> number(n.tol), "duplicateThreshold" -> number(n.duplicateThreshold))
  private def readNuisance(value: Value): Option[NuisanceRegressors] =
    if value == Null then None
    else
      fields(value, Set("matrices", "names", "check", "naAction", "tol", "duplicateThreshold"), "$.nuisance")
      val matrices = value("matrices").arr.map: m =>
        fields(m, Set("rows", "cols", "values"), "$.nuisance.matrices")
        val rows = integer(m("rows"))
        val cols = integer(m("cols"))
        val data = m("values").arr.map(_.num).toArray
        if rows < 1 || cols < 0 || rows.toLong * cols != data.length || !data.forall(_.isFinite) then fail("$.nuisance.matrices", "invalid matrix")
        Mat.unsafe(rows, cols, data)
      val names = if value("names") == Null then None else Some(value("names").arr.map(_.arr.map(_.str).toVector).toVector)
      Some(NuisanceRegressors(matrices.toVector, names, NuisanceCheck.valueOf(value("check").str), NaAction.valueOf(value("naAction").str), value("tol").num, value("duplicateThreshold").num))

  def encode(spec: ModelBuildSpec): Either[ModelJsonError, String] = checked:
    admit(FormulaParser.parseEither(spec.formula), "$.formula")(_.getMessage)
    // Only the literal built-in singleton is admitted: equal user-supplied
    // descriptors cannot certify an arbitrary executable HRF callback.
    requirePortable(spec.defaultHrf eq Hrfs.SPMG1, "$.defaultHrf")
    requirePortable(spec.factorSchemaBinding.isEmpty, "$.factorSchemaBinding")
    requirePortable(spec.hrfByCell.isEmpty, "$.hrfByCell")
    requirePortable(spec.hrfByPhase.isEmpty, "$.hrfByPhase")
    Obj("schema" -> Str("scalafim.model-build"), "version" -> Num(1), "formula" -> Str(spec.formula),
      "blockColumn" -> opt(spec.blockColumn), "durationColumn" -> opt(spec.durationColumn),
      "baseline" -> baseline(spec.baselineBasis), "baselineDegree" -> Num(spec.baselineDegree),
      "intercept" -> Str(spec.baselineIntercept.toString), "strategy" -> strategy(spec.strategy),
      "defaultHrf" -> Str("SPMG1"), "precision" -> number(spec.precision.value),
      "dropEmpty" -> Bool(spec.dropEmpty), "summate" -> Bool(spec.summate), "strict" -> Bool(spec.strict),
      "contrastSets" -> Obj.from(spec.contrastSets.toVector.sortBy(_._1).map: (name, set) =>
        name -> Arr.from(set.contrasts.map(c => ujson.read(admit(ModelJsonCodec.encodeContrast(c), "$.contrastSets")(_.message))))),
      "nuisance" -> nuisance(spec.nuisance),
      "factorLevels" -> Arr.from(spec.factorLevels.sets.map(s => Obj("factor" -> Str(s.factor.value), "levels" -> Arr.from(s.values)))),
      "emptyCellPolicy" -> Str(spec.emptyCellPolicy.toString), "missingValuePolicy" -> missing(spec.missingValuePolicy),
      "degenerateModulatorPolicy" -> Str(spec.degenerateModulatorPolicy.toString), "orthogonalization" -> orthogonalization(spec.orthogonalization)
    ).render()

  def decode(input: String): Either[ModelJsonError, ModelBuildSpec] = checked:
    val v = ujson.read(input)
    val expected = Set("schema", "version", "formula", "blockColumn", "durationColumn", "baseline", "baselineDegree", "intercept", "strategy", "defaultHrf", "precision", "dropEmpty", "summate", "strict", "contrastSets", "nuisance", "factorLevels", "emptyCellPolicy", "missingValuePolicy", "degenerateModulatorPolicy", "orthogonalization")
    if v.obj.keySet.toSet != expected then fail("$", "missing or unknown build fields")
    if v("schema").str != "scalafim.model-build" || v("version").num != 1 then fail("$", "unsupported model-build schema/version")
    if v("defaultHrf").str != "SPMG1" then fail("$.defaultHrf", "unsupported default HRF")
    val formula = v("formula").str
    admit(FormulaParser.parseEither(formula), "$.formula")(_.getMessage)
    val levels = v("factorLevels").arr.toVector.map: s =>
      fields(s, Set("factor", "levels"), "$.factorLevels")
      val factor = admit(FactorId(s("factor").str), "$.factorLevels.factor")(_.message)
      admit(FactorLevelSet.from(factor, s("levels").arr.map(_.str).toVector), "$.factorLevels")(_.message)
    val contrasts = v("contrastSets").obj.toVector.map: (name, sets) =>
      name -> ContrastSpec.ContrastSet(sets.arr.map(c => admit(ModelJsonCodec.decodeContrast(c.render()), "$.contrastSets")(_.message)).toVector)
    ModelBuildSpec(formula, readOpt(v("blockColumn")), readOpt(v("durationColumn")), readBaseline(v("baseline")),
      integer(v("baselineDegree")), Intercept.valueOf(v("intercept").str), readStrategy(v("strategy")), Hrfs.SPMG1,
      admit(Seconds.fromDouble(v("precision").num, "precision"), "$.precision")(_.message),
      v("dropEmpty").bool, v("summate").bool, v("strict").bool, contrasts.toMap, readNuisance(v("nuisance")),
      admit(FactorLevelRegistry.from(levels), "$.factorLevels")(_.message), EmptyCellPolicy.valueOf(v("emptyCellPolicy").str),
      missingValuePolicy = readMissing(v("missingValuePolicy")),
      degenerateModulatorPolicy = DegenerateModulatorPolicy.valueOf(v("degenerateModulatorPolicy").str),
      orthogonalization = readOrthogonalization(v("orthogonalization")))
