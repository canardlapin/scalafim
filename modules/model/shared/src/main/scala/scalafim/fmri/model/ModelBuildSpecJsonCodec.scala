package scalafim.fmri.model

import scalafim.fmri.design.*
import scalafim.fmri.design.baseline.*
import scalafim.fmri.design.contrast.ContrastSpec
import scalafim.fmri.design.formula.{FormulaParser, ModelJsonCodec, ModelJsonError, PortableJson}
import scalafim.fmri.design.formula.PortableJson.{Cursor, Result, admit, ensure, fail, traverse}
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.linalg.Mat
import ujson.{Arr, Bool, Null, Num, Obj, Str, Value}

/** Portable build profile. Unsupported runtime configuration fails explicitly;
  * it is never silently replaced with a default on either encode or decode.
  *
  * Decoding is strict (exact field sets, no duplicate keys, finite numbers,
  * canonical ids) and every error names the JSON path of the offending value.
  * Encoded text is byte-identical on the JVM and Scala.js.
  */
object ModelBuildSpecJsonCodec:
  private def requirePortable(condition: Boolean, path: String): Result[Unit] =
    ensure(condition, path, "not supported by portable build profile v1")

  private def opt(value: Option[String]): Value = value.fold[Value](Null)(Str(_))

  private def number(value: Double, path: String): Result[Value] = PortableJson.number(value, path)

  /** Exact, case-sensitive lookup of an enum case by its encoded name. */
  private[model] def enumCase[A](cursor: Cursor, kind: String, values: Array[A]): Result[A] =
    cursor.str.flatMap { name =>
      values.find(_.toString == name).toRight(
        ModelJsonError(cursor.path, s"unknown $kind '$name' (expected one of ${values.map(_.toString).mkString(", ")})"))
    }

  private def str(v: Cursor, name: String): Result[String] = v.field(name).flatMap(_.str)

  // ----------------------------------------------------------------- baseline

  private def baseline(value: BaselineBasis, path: String): Result[Value] = value match
    case BaselineBasis.Dct(cutoff) => number(cutoff.seconds, s"$path.cutoff").map(c => Obj("kind" -> Str("dct"), "cutoff" -> c))
    case other => Right(Obj("kind" -> Str(other.id)))

  private def readBaseline(value: Cursor): Result[BaselineBasis] =
    str(value, "kind").flatMap {
      case "dct" =>
        for
          _ <- value.fields(Set("kind", "cutoff"))
          cutoff <- value.field("cutoff")
          seconds <- cutoff.finite
          period <- admit(DctCutoffPeriod.fromSeconds(seconds), cutoff.path)(_.message)
        yield BaselineBasis.Dct(period)
      case "constant" => value.fields(Set("kind")).map(_ => BaselineBasis.Constant)
      case "poly" => value.fields(Set("kind")).map(_ => BaselineBasis.Poly)
      case "bs" => value.fields(Set("kind")).map(_ => BaselineBasis.Bs)
      case "ns" => value.fields(Set("kind")).map(_ => BaselineBasis.Ns)
      case other => fail(s"${value.path}.kind", s"unknown baseline basis '$other'")
    }

  // ----------------------------------------------------------------- strategy

  private def arCoefficients(value: ArCoefficientSpec, path: String): Result[Value] = value match
    case ArCoefficientSpec.Estimate => Right(Obj("kind" -> Str("estimate")))
    case ArCoefficientSpec.Rho(rho) => number(rho, s"$path.value").map(v => Obj("kind" -> Str("rho"), "value" -> v))
    case ArCoefficientSpec.Phi(values) => PortableJson.numbers(values, s"$path.values").map(v => Obj("kind" -> Str("phi"), "values" -> v))

  private def readArCoefficients(value: Cursor): Result[ArCoefficientSpec] =
    str(value, "kind").flatMap {
      case "estimate" => value.fields(Set("kind")).map(_ => ArCoefficientSpec.Estimate)
      case "rho" =>
        for
          _ <- value.fields(Set("kind", "value"))
          rho <- value.field("value").flatMap(_.finite)
          spec <- admit(ArCoefficientSpec.rho(rho), s"${value.path}.value")(_.message)
        yield spec
      case "phi" =>
        for
          _ <- value.fields(Set("kind", "values"))
          values <- value.field("values").flatMap(_.arr).flatMap(items => traverse(items)(_.finite))
        yield ArCoefficientSpec.Phi(values)
      case other => fail(s"${value.path}.kind", s"unknown AR coefficient policy $other")
    }

  private[model] def autocorrelation(value: AutocorrelationConfig, path: String): Result[Value] =
    arCoefficients(value.coefficients, s"$path.coefficients").map: coefficients =>
      val encoded = Obj("order" -> Num(value.order.value), "iterations" -> Num(value.iterations), "global" -> Bool(value.global),
        "voxelwise" -> Bool(value.voxelwise), "exactFirst" -> Bool(value.exactFirst),
        "censoredTimepoints" -> Arr.from(value.censoredTimepoints.values.map(Num(_))), "coefficients" -> coefficients)
      if value.censorTreatment != ArCensorTreatment.RestartWhitening then encoded("censorTreatment") = Str(value.censorTreatment.toString)
      if value.initialization == ArInitialization.Stationary then encoded("initialization") = Str("Stationary")
      value.biasCorrection match
        case ArBiasCorrection.Raw => ()
        case ArBiasCorrection.OlsDesign(ceiling) =>
          encoded("biasCorrection") = Obj("kind" -> Str("ols-design"), "ceiling" -> Num(ceiling.value))
        case ArBiasCorrection.OlsTailAnchored(maxLag) =>
          encoded("biasCorrection") = Obj("kind" -> Str("ols-tail-anchored"), "maxLag" -> Num(maxLag.value))
      encoded

  private def readBiasCorrection(value: Cursor): Result[ArBiasCorrection] =
    str(value, "kind").flatMap:
      case "ols-design" =>
        for
          _ <- value.fields(Set("kind", "ceiling"))
          ceiling <- value.field("ceiling").flatMap(_.integer)
          policy <- admit(ArBiasCorrection.olsDesign(ceiling), s"${value.path}.ceiling")(_.message)
        yield policy
      case "ols-tail-anchored" =>
        for
          _ <- value.fields(Set("kind", "maxLag"))
          maxLag <- value.field("maxLag").flatMap(_.integer)
          policy <- admit(ArBiasCorrection.olsTailAnchored(maxLag), s"${value.path}.maxLag")(_.message)
        yield policy
      case other => fail(s"${value.path}.kind", s"unknown AR bias correction $other")

  private[model] def readAutocorrelation(value: Cursor): Result[AutocorrelationConfig] =
    for
      fields <- value.obj
      _ <- value.fields(Set("order", "iterations", "global", "voxelwise", "exactFirst", "censoredTimepoints", "coefficients") ++
        (if fields.contains("biasCorrection") then Set("biasCorrection") else Set.empty) ++
        (if fields.contains("initialization") then Set("initialization") else Set.empty) ++
        (if fields.contains("censorTreatment") then Set("censorTreatment") else Set.empty))
      order <- value.field("order").flatMap(_.integer)
      iterations <- value.field("iterations").flatMap(_.integer)
      global <- value.field("global").flatMap(_.bool)
      voxelwise <- value.field("voxelwise").flatMap(_.bool)
      exactFirst <- value.field("exactFirst").flatMap(_.bool)
      censored <- value.field("censoredTimepoints").flatMap(_.arr).flatMap(items => traverse(items)(_.integer))
      coefficients <- value.field("coefficients").flatMap(readArCoefficients)
      policy <- if fields.contains("biasCorrection") then value.field("biasCorrection").flatMap(readBiasCorrection) else Right(ArBiasCorrection.Raw)
      initialization <- if fields.contains("initialization") then value.field("initialization").flatMap(c => enumCase(c, "AR initialization", ArInitialization.values)).map(Some(_)) else Right(None)
      censorTreatment <- if fields.contains("censorTreatment") then value.field("censorTreatment").flatMap(c => enumCase(c, "AR censor treatment", ArCensorTreatment.values)) else Right(ArCensorTreatment.RestartWhitening)
      config <- admit(AutocorrelationConfig(order, iterations, global, voxelwise, exactFirst, censored, coefficients, policy, initialization, censorTreatment), value.path)(_.message)
    yield config

  private[model] def lss(value: LssStrategyConfig, path: String): Result[Value] =
    for
      eps <- number(value.eps, s"$path.eps")
      rankTol <- number(value.rankTol, s"$path.rankTol")
    yield Obj("trialTerm" -> opt(value.trialTerm.map(_.value)), "eps" -> eps, "rankTol" -> rankTol)

  private[model] def readLss(value: Cursor): Result[LssStrategyConfig] =
    for
      _ <- value.fields(Set("trialTerm", "eps", "rankTol"))
      trialTerm <- value.field("trialTerm").flatMap(_.nullable(_.str))
      eps <- value.field("eps").flatMap(_.finite)
      rankTol <- value.field("rankTol").flatMap(_.finite)
      config <- admit(LssStrategyConfig(trialTerm, eps, rankTol), value.path)(_.message)
    yield config

  private def strategy(value: FitStrategy, path: String): Result[Value] = value match
    case FitStrategy.OrdinaryLeastSquares(controls) if controls == FitControls() => Right(Str("ols"))
    case FitStrategy.RunwiseLeastSquares(controls) if controls == FitControls() => Right(Str("runwise-ols"))
    case FitStrategy.SeparateRunsThenFixedEffects(controls) if controls == FitControls() => Right(Str("fixed-effects-ols"))
    case FitStrategy.GeneralizedLeastSquares(ar, controls) if controls == FitControls() =>
      autocorrelation(ar, s"$path.ar").map(a => Obj("kind" -> Str("gls"), "ar" -> a))
    case FitStrategy.RunwiseGeneralizedLeastSquares(ar, controls) if controls == FitControls() =>
      autocorrelation(ar, s"$path.ar").map(a => Obj("kind" -> Str("runwise-gls"), "ar" -> a))
    case FitStrategy.LeastSquaresSeparate(config, controls) if controls == FitControls() =>
      lss(config, s"$path.lss").map(l => Obj("kind" -> Str("lss"), "lss" -> l))
    case _ => fail(path, "this strategy or its controls requires an extended portable profile")

  private def readStrategy(value: Cursor): Result[FitStrategy] = value.value match
    case Str("ols") => Right(FitStrategy.OrdinaryLeastSquares())
    case Str("runwise-ols") => Right(FitStrategy.RunwiseLeastSquares())
    case Str("fixed-effects-ols") => Right(FitStrategy.SeparateRunsThenFixedEffects())
    case Str(other) => fail(value.path, s"unknown strategy $other")
    case _ =>
      str(value, "kind").flatMap {
        case "gls" =>
          for _ <- value.fields(Set("kind", "ar")); ar <- value.field("ar").flatMap(readAutocorrelation)
          yield FitStrategy.GeneralizedLeastSquares(ar)
        case "runwise-gls" =>
          for _ <- value.fields(Set("kind", "ar")); ar <- value.field("ar").flatMap(readAutocorrelation)
          yield FitStrategy.RunwiseGeneralizedLeastSquares(ar)
        case "lss" =>
          for _ <- value.fields(Set("kind", "lss")); config <- value.field("lss").flatMap(readLss)
          yield FitStrategy.LeastSquaresSeparate(config)
        case other => fail(s"${value.path}.kind", s"unknown strategy $other")
      }

  // ----------------------------------------------------------- missing values

  private def missing(value: MissingValuePolicy, path: String): Result[Value] = value match
    case MissingValuePolicy.ImputeConstant(v) => number(v, s"$path.value").map(n => Obj("kind" -> Str("impute"), "value" -> n))
    case other => Right(Obj("kind" -> Str(other.label)))

  private def readMissing(value: Cursor): Result[MissingValuePolicy] =
    str(value, "kind").flatMap {
      case "reject" => value.fields(Set("kind")).map(_ => MissingValuePolicy.Reject)
      case "zero-contribution" => value.fields(Set("kind")).map(_ => MissingValuePolicy.ZeroContribution)
      case "drop-from-term" => value.fields(Set("kind")).map(_ => MissingValuePolicy.DropFromTerm)
      case "impute" =>
        for _ <- value.fields(Set("kind", "value")); n <- value.field("value").flatMap(_.finite)
        yield MissingValuePolicy.ImputeConstant(n)
      case other => fail(s"${value.path}.kind", s"unknown policy $other")
    }

  // --------------------------------------------------------- orthogonalization

  private def orthogonalization(value: ModulatorOrthogonalizationPlan, path: String): Result[Value] =
    traverse(value.policies.toVector.zipWithIndex) { (p, index) =>
      val scope = p.scope match
        case OrthogonalizationScope.WholeTerm => Obj("kind" -> Str("term"))
        case OrthogonalizationScope.WithinRun => Obj("kind" -> Str("run"))
        case OrthogonalizationScope.WithinCells(factors) => Obj("kind" -> Str("cells"), "factors" -> Arr.from(factors.map(_.value)))
      number(p.tolerance, s"$path[$index].tolerance").map: tolerance =>
        Obj("term" -> Str(p.term.value), "order" -> Arr.from(p.order.map(_.value)), "scope" -> scope,
          "degenerate" -> Str(p.degenerate.toString), "tolerance" -> tolerance)
    }.map(Arr.from(_))

  private def id[A, E](cursor: Cursor)(parse: String => Either[E, A])(message: E => String)(show: A => String): Result[A] =
    cursor.str.flatMap { raw =>
      admit(parse(raw), cursor.path)(message).flatMap(id =>
        if show(id) == raw then Right(id) else fail(cursor.path, s"id '$raw' is not canonical"))
    }

  private def readScope(scope: Cursor): Result[OrthogonalizationScope] =
    str(scope, "kind").flatMap {
      case "term" => scope.fields(Set("kind")).map(_ => OrthogonalizationScope.WholeTerm)
      case "run" => scope.fields(Set("kind")).map(_ => OrthogonalizationScope.WithinRun)
      case "cells" =>
        for
          _ <- scope.fields(Set("kind", "factors"))
          items <- scope.field("factors").flatMap(_.arr)
          factors <- traverse(items)(item => id(item)(FactorId(_))(_.message)(_.value))
        yield OrthogonalizationScope.WithinCells(factors)
      case other => fail(s"${scope.path}.kind", s"unknown scope $other")
    }

  private def readOrthogonalization(value: Cursor): Result[ModulatorOrthogonalizationPlan] =
    for
      items <- value.arr
      policies <- traverse(items) { p =>
        for
          _ <- p.fields(Set("term", "order", "scope", "degenerate", "tolerance"))
          scope <- p.field("scope").flatMap(readScope)
          term <- p.field("term").flatMap(c => id(c)(TermId(_))(_.message)(_.value))
          order <- p.field("order").flatMap(_.arr).flatMap(xs => traverse(xs)(x => id(x)(ModulatorId(_))(_.message)(_.value)))
          degenerate <- p.field("degenerate").flatMap(enumCase(_, "degenerate modulator policy", DegenerateModulatorPolicy.values))
          tolerance <- p.field("tolerance").flatMap(_.finite)
          policy <- admit(ModulatorOrthogonalization.ordered(term, order, scope, degenerate, tolerance), p.path)(_.message)
        yield policy
      }
      plan <- admit(ModulatorOrthogonalizationPlan.of(policies*), value.path)(_.message)
    yield plan

  // ------------------------------------------------------------------ nuisance

  private def nuisance(value: Option[NuisanceRegressors], path: String): Result[Value] = value match
    case None => Right(Null)
    case Some(n) =>
      for
        matrices <- traverse(n.matrices.zipWithIndex) { (m, index) =>
          PortableJson.numbers(m.data.toVector, s"$path.matrices[$index].values").map(values =>
            Obj("rows" -> Num(m.rows), "cols" -> Num(m.cols), "values" -> values))
        }
        tol <- number(n.tol, s"$path.tol")
        duplicateThreshold <- number(n.duplicateThreshold, s"$path.duplicateThreshold")
      yield Obj("matrices" -> Arr.from(matrices),
        "names" -> n.names.fold[Value](Null)(blocks => Arr.from(blocks.map(Arr.from(_)))),
        "check" -> Str(n.check.toString), "naAction" -> Str(n.naAction.toString),
        "tol" -> tol, "duplicateThreshold" -> duplicateThreshold)

  private def readMatrix(m: Cursor): Result[Mat] =
    for
      _ <- m.fields(Set("rows", "cols", "values"))
      rows <- m.field("rows").flatMap(_.integer)
      cols <- m.field("cols").flatMap(_.integer)
      values <- m.field("values").flatMap(_.arr).flatMap(items => traverse(items)(_.finite))
      _ <- ensure(rows >= 1 && cols >= 0 && rows.toLong * cols == values.length, m.path,
        s"invalid matrix: $rows x $cols needs ${rows.toLong * cols} values, got ${values.length}")
    yield Mat.unsafe(rows, cols, values.toArray)

  /** Every `NuisanceRegressors` invariant is checked here so construction cannot throw. */
  private def readNuisance(value: Cursor): Result[Option[NuisanceRegressors]] =
    if value.isNull then Right(None)
    else
      val at = (name: String) => s"${value.path}.$name"
      for
        _ <- value.fields(Set("matrices", "names", "check", "naAction", "tol", "duplicateThreshold"))
        matrixItems <- value.field("matrices").flatMap(_.arr)
        _ <- ensure(matrixItems.nonEmpty, at("matrices"), "nuisance matrices must be non-empty")
        matrices <- traverse(matrixItems)(readMatrix)
        names <- value.field("names").flatMap(_.nullable(_.arr.flatMap(blocks =>
          traverse(blocks)(block => block.arr.flatMap(items => traverse(items)(item =>
            item.str.flatMap(name => admit(ColumnId(name), item.path)(_.message).map(_ => name))))))))
        _ <- names.fold[Result[Unit]](Right(())) { blocks =>
          if blocks.length != matrices.length then fail(at("names"), "nuisance names must match nuisance matrices")
          else blocks.zip(matrices).zipWithIndex.collectFirst {
            case ((columns, matrix), index) if columns.length != matrix.cols => index
          }.fold[Result[Unit]](Right(()))(index => fail(s"${at("names")}[$index]", "nuisance names must match nuisance matrix columns"))
        }
        check <- value.field("check").flatMap(enumCase(_, "nuisance check", NuisanceCheck.values))
        naAction <- value.field("naAction").flatMap(enumCase(_, "NA action", NaAction.values))
        tol <- value.field("tol").flatMap(_.finite)
        _ <- ensure(tol >= 0.0, at("tol"), "nuisance tolerance must be non-negative and finite")
        duplicateThreshold <- value.field("duplicateThreshold").flatMap(_.finite)
        _ <- ensure(duplicateThreshold >= 0.0 && duplicateThreshold <= 1.0, at("duplicateThreshold"),
          "nuisance duplicate threshold must be finite and in [0, 1]")
      yield Some(NuisanceRegressors(matrices, names, check, naAction, tol, duplicateThreshold))

  // ------------------------------------------------------------------- public

  def encode(spec: ModelBuildSpec): Either[ModelJsonError, String] =
    for
      _ <- ensure(spec.derived.isEmpty, "$.derived", "derived declarations require the scalafim.model-document envelope")
      formula <- formulaValue(spec, "$.formula")
      body <- encodeBody(spec, "$", strategy)
      text <- PortableJson.render(Obj.from(Vector("schema" -> Str("scalafim.model-build"), "version" -> Num(1), "formula" -> formula) ++ body))
    yield text

  private[model] def formulaValue(spec: ModelBuildSpec, path: String): Result[Value] =
    admit(FormulaParser.parseEither(spec.formula), path)(_.getMessage).map(_ => Str(spec.formula))

  /** Every build field except the schema header, formula and derived plan.
    * `root` is the JSON path of the object these fields belong to.
    */
  private[model] def encodeBody(spec: ModelBuildSpec, root: String, encodeStrategy: (FitStrategy, String) => Result[Value]): Result[Vector[(String, Value)]] =
    for
      // Only the literal built-in singleton is admitted: equal user-supplied
      // descriptors cannot certify an arbitrary executable HRF callback.
      _ <- requirePortable(spec.defaultHrf eq Hrfs.SPMG1, s"$root.defaultHrf")
      _ <- requirePortable(spec.factorSchemaBinding.isEmpty, s"$root.factorSchemaBinding")
      _ <- requirePortable(spec.hrfByCell.isEmpty, s"$root.hrfByCell")
      _ <- requirePortable(spec.hrfByPhase.isEmpty, s"$root.hrfByPhase")
      baselineValue <- baseline(spec.baselineBasis, s"$root.baseline")
      strategyValue <- encodeStrategy(spec.strategy, s"$root.strategy")
      precision <- number(spec.precision.value, s"$root.precision")
      contrastSets <- traverse(spec.contrastSets.toVector.sortBy(_._1)) { (name, set) =>
        traverse(set.contrasts.zipWithIndex)((c, index) => ModelJsonCodec.contrastEnvelope(c, s"$root.contrastSets.$name[$index]"))
          .map(values => name -> Arr.from(values))
      }
      nuisanceValue <- nuisance(spec.nuisance, s"$root.nuisance")
      missingValue <- missing(spec.missingValuePolicy, s"$root.missingValuePolicy")
      orthogonalizationValue <- orthogonalization(spec.orthogonalization, s"$root.orthogonalization")
    yield Vector(
      "blockColumn" -> opt(spec.blockColumn), "durationColumn" -> opt(spec.durationColumn),
      "baseline" -> baselineValue, "baselineDegree" -> Num(spec.baselineDegree),
      "intercept" -> Str(spec.baselineIntercept.toString), "strategy" -> strategyValue,
      "defaultHrf" -> Str("SPMG1"), "precision" -> precision,
      "dropEmpty" -> Bool(spec.dropEmpty), "summate" -> Bool(spec.summate), "strict" -> Bool(spec.strict),
      "contrastSets" -> Obj.from(contrastSets),
      "nuisance" -> nuisanceValue,
      "factorLevels" -> Arr.from(spec.factorLevels.sets.map(s => Obj("factor" -> Str(s.factor.value), "levels" -> Arr.from(s.values)))),
      "emptyCellPolicy" -> Str(spec.emptyCellPolicy.toString), "missingValuePolicy" -> missingValue,
      "degenerateModulatorPolicy" -> Str(spec.degenerateModulatorPolicy.toString), "orthogonalization" -> orthogonalizationValue
    )

  private[model] val bodyFields = Set("blockColumn", "durationColumn", "baseline", "baselineDegree",
    "intercept", "strategy", "defaultHrf", "precision", "dropEmpty", "summate", "strict", "contrastSets", "nuisance",
    "factorLevels", "emptyCellPolicy", "missingValuePolicy", "degenerateModulatorPolicy", "orthogonalization")

  private def optionalColumn(v: Cursor, name: String): Result[Option[String]] =
    v.field(name).flatMap(c => c.nullable(_.str).flatMap {
      case Some(text) => admit(ColumnId(text), c.path)(_.message).map(_ => Some(text))
      case None => Right(None)
    })

  def decode(input: String): Either[ModelJsonError, ModelBuildSpec] =
    for
      v <- PortableJson.parse(input)
      _ <- v.fields(bodyFields ++ Set("schema", "version", "formula"))
      schema <- str(v, "schema")
      _ <- ensure(schema == "scalafim.model-build", "$.schema", "unsupported model-build schema")
      version <- v.field("version").flatMap(_.integer)
      _ <- ensure(version == 1, "$.version", "unsupported model-build version")
      formula <- v.field("formula").flatMap(readFormula)
      spec <- readBody(v, formula, readStrategy)
    yield spec

  private[model] def readFormula(cursor: Cursor): Result[String] =
    cursor.str.flatMap(formula => admit(FormulaParser.parseEither(formula), cursor.path)(_.getMessage).map(_ => formula))

  /** Read the build fields of `v`; the caller has checked its exact field set. */
  private[model] def readBody(v: Cursor, formula: String, readStrategy: Cursor => Result[FitStrategy]): Result[ModelBuildSpec] =
    val at = (name: String) => s"${v.path}.$name"
    for
      defaultHrf <- str(v, "defaultHrf")
      _ <- ensure(defaultHrf == "SPMG1", at("defaultHrf"), "unsupported default HRF")
      blockColumn <- optionalColumn(v, "blockColumn")
      durationColumn <- optionalColumn(v, "durationColumn")
      baselineBasis <- v.field("baseline").flatMap(readBaseline)
      baselineDegree <- v.field("baselineDegree").flatMap(_.integer)
      _ <- ensure(baselineDegree >= 1, at("baselineDegree"), "baseline degree must be at least 1")
      intercept <- v.field("intercept").flatMap(enumCase(_, "intercept", Intercept.values))
      fitStrategy <- v.field("strategy").flatMap(readStrategy)
      precisionCursor <- v.field("precision")
      precisionValue <- precisionCursor.finite
      precision <- admit(Seconds.fromDouble(precisionValue, "precision"), precisionCursor.path)(_.message)
      dropEmpty <- v.field("dropEmpty").flatMap(_.bool)
      summate <- v.field("summate").flatMap(_.bool)
      strict <- v.field("strict").flatMap(_.bool)
      contrastEntries <- v.field("contrastSets").flatMap(_.entries)
      contrasts <- traverse(contrastEntries) { (name, sets) =>
        sets.arr.flatMap(items => traverse(items)(ModelJsonCodec.readContrastEnvelope)).map(cs => name -> ContrastSpec.ContrastSet(cs))
      }
      nuisance <- v.field("nuisance").flatMap(readNuisance)
      levelItems <- v.field("factorLevels").flatMap(_.arr)
      levels <- traverse(levelItems) { s =>
        for
          _ <- s.fields(Set("factor", "levels"))
          factor <- s.field("factor").flatMap(c => id(c)(FactorId(_))(_.message)(_.value))
          values <- s.field("levels").flatMap(_.arr).flatMap(items => traverse(items)(_.str))
          set <- admit(FactorLevelSet.from(factor, values), s.path)(_.message)
        yield set
      }
      registry <- admit(FactorLevelRegistry.from(levels), at("factorLevels"))(_.message)
      emptyCellPolicy <- v.field("emptyCellPolicy").flatMap(enumCase(_, "empty-cell policy", EmptyCellPolicy.values))
      missingValuePolicy <- v.field("missingValuePolicy").flatMap(readMissing)
      degenerate <- v.field("degenerateModulatorPolicy").flatMap(enumCase(_, "degenerate modulator policy", DegenerateModulatorPolicy.values))
      orthogonalizationPlan <- v.field("orthogonalization").flatMap(readOrthogonalization)
    yield ModelBuildSpec(formula, blockColumn, durationColumn, baselineBasis,
      baselineDegree, intercept, fitStrategy, Hrfs.SPMG1, precision,
      dropEmpty, summate, strict, contrasts.toMap, nuisance,
      registry, emptyCellPolicy,
      missingValuePolicy = missingValuePolicy,
      degenerateModulatorPolicy = degenerate,
      orthogonalization = orthogonalizationPlan)
