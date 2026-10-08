package scalafim.fmri.model

import gale.linalg.Matrix
import scalafim.fmri.design.{ColumnId, RunContrastCombination}
import scalafim.fmri.design.baseline.{BaselineBasis, DctCutoffPeriod, Intercept}
import scalafim.fmri.design.formula.{Arg, ArgValue, DerivedColumn, DerivedEventPlan, DerivedMissingRows, FormulaParser, FormulaPrinter, PortableJson}
import ujson.{Null, Obj}

/** A text-form problem. `line` is one-based when the problem is in parsed text. */
final case class ModelTextError(line: Option[Int], detail: String):
  def message: String = line.fold(detail)(number => s"line $number: $detail")

/** Canonical text lines for the portable pieces of a [[ModelDocument]] that a
  * studio shows and edits as text:
  *
  * {{{
  * formula: onset ~ hrf(level, id = bins)
  * missing_rows: drop
  * derive: reward: number = (gain - loss)
  * baseline: dct(cutoff = 128, degree = 1, intercept = "runwise")
  * confounds: confounds(motion = "friston24", acompcor = 5, white_matter = FALSE, csf = TRUE, global_signal = FALSE,
  *   censor = fd(threshold = 0.5, before = 1, after = 2, min_segment = 5, max_fraction = 0.25))
  * estimation: runwise_gls(ar = ar(...), weights = none(), projection = none(), missing = "error")
  * runs: fixed_effects()
  * base: fnv64-<16 hex digits>
  * }}}
  *
  * (Each entry is one physical line; the wrap above is for reading only.)
  * Values use the admitted formula expression grammar and its printer, so
  * numbers are platform-stable and every rendered line re-parses losslessly.
  * Every argument is explicit: parsing rejects unknown, duplicate or missing
  * arguments rather than filling defaults.
  *
  * Pieces with no text form — block and duration columns, precision,
  * contrasts, nuisance matrices, factor levels, empty-cell, missing-value and
  * degenerate-modulator policies, orthogonalization — come from the `base`
  * document on parse. The contract is that `base` agrees with the rendered
  * document on all of them: the `base:` line is a platform-stable digest of
  * exactly those fields, and [[parse]] fails at that line when `base`
  * differs, so text can never silently be combined with another model.
  */
object ModelDocumentText:
  private type Result[A] = Either[ModelTextError, A]

  private val singleKeys = Vector("formula", "missing_rows", "baseline", "confounds", "estimation", "runs", "base")

  /** Build fields that do have a text form, excluded from the base digest. */
  private val textBuildFields = Set("baseline", "baselineDegree", "intercept", "strategy")

  /** Digest of every build field without a text form, from its canonical
    * model-document JSON (FNV-1a 64 over the UTF-16 text, as design
    * fingerprints use; an identity, not a security hash).
    */
  def baseDigest(document: ModelDocument): Either[ModelTextError, String] =
    ModelBuildSpecJsonCodec.encodeBody(document.build, "$.build", (_, _) => Right(Null))
      .flatMap(fields => PortableJson.render(Obj.from(fields.filterNot((key, _) => textBuildFields(key)))))
      .left.map(error => ModelTextError(None, s"base document has no portable digest: ${error.message}"))
      .map { text =>
        var hash = -3750763034362895579L
        var index = 0
        while index < text.length do
          hash = (hash ^ text.charAt(index).toLong) * 1099511628211L
          index += 1
        val hex = java.lang.Long.toHexString(hash)
        s"fnv64-${"0" * (16 - hex.length)}$hex"
      }

  // ------------------------------------------------------------------ render

  def render(document: ModelDocument): Either[ModelTextError, Vector[String]] =
    val build = document.build
    def print(value: ArgValue): Result[String] =
      FormulaPrinter.expressionTextEither(value).left.map(error => ModelTextError(None, error.message))
    for
      _ <- if build.formula.exists(c => c == '\n' || c == '\r') then Left(ModelTextError(None, "formula text must be a single line")) else Right(())
      _ <- if build.formula != build.formula.trim then Left(ModelTextError(None, "formula text must not have leading or trailing whitespace")) else Right(())
      digest <- baseDigest(document)
      baselineText <- print(baseline(build.baselineBasis, build.baselineDegree, build.baselineIntercept))
      confoundText <- print(document.confounds.fold(call("none"))(confounds))
      strategyValue <- estimation(build.strategy)
      estimationText <- print(strategyValue)
      runsText <- print(runs(document.runCombination))
    yield Vector(s"formula: ${build.formula}", s"missing_rows: ${build.derived.missingRows.label}") ++
      build.derived.columns.map(column => s"derive: ${column.text}") ++
      Vector(s"baseline: $baselineText", s"confounds: $confoundText", s"estimation: $estimationText", s"runs: $runsText", s"base: $digest")

  private def call(name: String, args: (String, ArgValue)*): ArgValue =
    ArgValue.Call(name, args.toVector.map((key, value) => Arg(Some(key), value)))

  private def num(value: Double): ArgValue = ArgValue.Num(value)
  private def int(value: Int): ArgValue = ArgValue.Num(value.toDouble)
  private def bool(value: Boolean): ArgValue = ArgValue.Bool(value)
  private def text(value: String): ArgValue = ArgValue.Str(value)
  private def vector(values: Seq[ArgValue]): ArgValue = ArgValue.Call("c", values.toVector.map(Arg(None, _)))

  /** `RawAndDerivative12` is written `raw_and_derivative12`. */
  private def snake(name: String): String =
    name.zipWithIndex.map((c, i) => if c.isUpper && i > 0 then s"_${c.toLower}" else c.toLower.toString).mkString

  private def baseline(basis: BaselineBasis, degree: Int, intercept: Intercept): ArgValue =
    val common = Vector("degree" -> int(degree), "intercept" -> text(snake(intercept.toString)))
    basis match
      case BaselineBasis.Dct(cutoff) => call("dct", (("cutoff" -> num(cutoff.seconds)) +: common)*)
      case other => call(other.id, common*)

  private def confounds(spec: ConfoundSpec): ArgValue =
    call("confounds",
      "motion" -> text(snake(spec.motion.toString)), "acompcor" -> int(spec.acompcorComponents),
      "white_matter" -> bool(spec.includeWhiteMatter), "csf" -> bool(spec.includeCsf), "global_signal" -> bool(spec.includeGlobalSignal),
      "censor" -> spec.censor.fold(call("none"))(policy => call("fd",
        "threshold" -> num(policy.threshold), "before" -> int(policy.before), "after" -> int(policy.after),
        "min_segment" -> int(policy.minimumRetainedSegment), "max_fraction" -> num(policy.maximumCensoredFraction))))

  private def estimation(strategy: FitStrategy): Result[ArgValue] =
    def with_(name: String, controls: FitControls, fields: (String, ArgValue)*): ArgValue =
      call(name, (fields.toVector ++ controlArgs(controls))*)
    strategy match
      case FitStrategy.OrdinaryLeastSquares(c) => Right(with_("ols", c))
      case FitStrategy.RunwiseLeastSquares(c) => Right(with_("runwise_ols", c))
      case FitStrategy.SeparateRunsThenFixedEffects(c) => Right(with_("fixed_effects_ols", c))
      case FitStrategy.GeneralizedLeastSquares(ar, c) => Right(with_("gls", c, "ar" -> autocorrelation(ar)))
      case FitStrategy.RunwiseGeneralizedLeastSquares(ar, c) => Right(with_("runwise_gls", c, "ar" -> autocorrelation(ar)))
      case FitStrategy.LeastSquaresSeparate(lss, c) =>
        Right(with_("lss", c, (lss.trialTerm.toVector.map(term => "trial_term" -> text(term.value)) ++
          Vector("eps" -> num(lss.eps), "rank_tol" -> num(lss.rankTol)))*))
      case other => Left(ModelTextError(None, s"strategy ${other.engine} has no portable text form"))

  private def autocorrelation(ar: AutocorrelationConfig): ArgValue =
    call("ar",
      "order" -> int(ar.order.value), "iterations" -> int(ar.iterations), "global" -> bool(ar.global),
      "voxelwise" -> bool(ar.voxelwise), "exact_first" -> bool(ar.exactFirst),
      "censored" -> vector(ar.censoredTimepoints.values.map(int)),
      "coefficients" -> (ar.coefficients match
        case ArCoefficientSpec.Estimate => call("estimate")
        case ArCoefficientSpec.Rho(rho) => call("rho", "value" -> num(rho))
        case ArCoefficientSpec.Phi(values) => call("phi", "values" -> vector(values.map(num)))
      ),
      "bias" -> (ar.biasCorrection match
        case ArBiasCorrection.Raw => call("raw")
        case ArBiasCorrection.OlsDesign(ceiling) => call("ols_design", "ceiling" -> int(ceiling.value))
      ))

  private def controlArgs(controls: FitControls): Vector[(String, ArgValue)] =
    val weights = controls.volumeWeighting match
      case ModelVolumeWeighting.Disabled => call("none")
      case ModelVolumeWeighting.Estimated(estimator) =>
        val function = estimator.function match
          case DvarsWeightFunction.InverseSquared => call("inverse_squared")
          case DvarsWeightFunction.SoftThreshold(t, k) => call("soft_threshold", "threshold" -> num(t.value), "steepness" -> num(k.value))
          case DvarsWeightFunction.TukeyBisquare(t) => call("tukey_bisquare", "threshold" -> num(t.value))
        call("dvars", "function" -> function, "scope" -> text(snake(estimator.scope.toString)))
      case ModelVolumeWeighting.Fixed(values, alignment) =>
        call("fixed", "values" -> vector(values.values.map(num)), "alignment" -> text(snake(alignment.toString)))
    val projection = controls.nuisanceProjection match
      case ModelNuisanceProjection.Disabled => call("none")
      case ModelNuisanceProjection.MatrixProjection(matrix, lambda) =>
        val m = matrix.matrix
        val data = new Array[Double](m.rows * m.cols)
        m.copyRowMajorTo(data)
        val l = lambda match
          case Regularization.Auto => call("auto")
          case Regularization.Gcv => call("gcv")
          case Regularization.Fixed(value) => call("fixed", "value" -> num(value))
        call("matrix", "rows" -> int(m.rows), "cols" -> int(m.cols), "values" -> vector(data.toVector.map(num)), "lambda" -> l)
    Vector("weights" -> weights, "projection" -> projection, "missing" -> text(snake(controls.missingData.toString)))

  private def runs(combination: RunContrastCombination): ArgValue = combination match
    case RunContrastCombination.FixedEffects => call("fixed_effects")
    case RunContrastCombination.Concatenated(columns) => call("concatenated", "columns" -> vector(columns.map(c => text(c.value))))

  // ------------------------------------------------------------------- parse

  /** Parse rendered lines onto `base`, replacing the formula, derived plan,
    * baseline basis/degree/intercept, confounds, strategy and run combination.
    * Blank lines are ignored; each key other than `derive` appears exactly once.
    */
  def parse(lines: Vector[String], base: ModelDocument): Either[ModelTextError, ModelDocument] =
    val entries = lines.zipWithIndex.collect { case (line, index) if line.trim.nonEmpty => (index + 1, line) }
    for
      keyed <- traverse(entries) { (number, line) =>
        line.indexOf(':') match
          case cut if cut > 0 => Right((number, line.substring(0, cut).trim, line.substring(cut + 1).trim))
          case _ => Left(ModelTextError(Some(number), "expected 'key: value'"))
      }
      _ <- keyed.find((_, key, _) => key != "derive" && !singleKeys.contains(key)).fold[Result[Unit]](Right(())) { (number, key, _) =>
        Left(ModelTextError(Some(number), s"unknown key '$key'"))
      }
      single <- traverse(singleKeys) { key =>
        keyed.filter(_._2 == key) match
          case Vector((number, _, value)) => Right(key -> (number, value))
          case Vector() => Left(ModelTextError(None, s"missing '$key' line"))
          case repeated => Left(ModelTextError(Some(repeated(1)._1), s"duplicate '$key' line"))
      }.map(_.toMap)
      (formulaLine, formula) = single("formula")
      _ <- FormulaParser.parseEither(formula).left.map(error => ModelTextError(Some(formulaLine), error.message))
      policy <- single("missing_rows") match
        case (number, value) => DerivedMissingRows.fromLabel(value).toRight(ModelTextError(Some(number), s"unknown missing-row policy '$value'"))
      deriveLines = keyed.filter(_._2 == "derive")
      columns <- traverse(deriveLines) { (number, _, value) =>
        DerivedColumn.parse(value).left.map(error => ModelTextError(Some(number), error.message))
      }
      derived <- DerivedEventPlan.from(columns, policy).left.map { error =>
        val repeated = columns.indices.find(index => columns.take(index).exists(_.id == columns(index).id))
        ModelTextError(repeated.map(index => deriveLines(index)._1), error.message)
      }
      baselineParts <- expression(single("baseline")).flatMap((number, value) => readBaseline(value).left.map(at(number)))
      confoundSpec <- expression(single("confounds")).flatMap((number, value) => readConfounds(value).left.map(at(number)))
      strategy <- expression(single("estimation")).flatMap((number, value) => readEstimation(value).left.map(at(number)))
      combination <- expression(single("runs")).flatMap((number, value) => readRuns(value).left.map(at(number)))
      expected <- baseDigest(base)
      _ <- single("base") match
        case (_, digest) if digest == expected => Right(())
        case (number, digest) => Left(ModelTextError(Some(number),
          s"base document differs from the rendered one in fields without a text form (text has $digest, base has $expected)"))
    yield
      val (basis, degree, intercept) = baselineParts
      ModelDocument(
        base.build.copy(formula = formula, derived = derived, baselineBasis = basis, baselineDegree = degree,
          baselineIntercept = intercept, strategy = strategy),
        confoundSpec,
        combination
      )

  private def at(number: Int)(detail: String): ModelTextError = ModelTextError(Some(number), detail)

  private def expression(entry: (Int, String)): Result[(Int, ArgValue)] =
    val (number, value) = entry
    FormulaParser.parseExpression(value).left.map(error => ModelTextError(Some(number), error.getMessage)).map(number -> _)

  private def traverse[A, B](values: Vector[A])(f: A => Either[ModelTextError, B]): Either[ModelTextError, Vector[B]] =
    values.foldLeft[Either[ModelTextError, Vector[B]]](Right(Vector.empty))((acc, value) => for previous <- acc; next <- f(value) yield previous :+ next)

  private type Read[A] = Either[String, A]

  private def all[A, B](values: Vector[A])(f: A => Read[B]): Read[Vector[B]] =
    values.foldLeft[Read[Vector[B]]](Right(Vector.empty))((acc, value) => for previous <- acc; next <- f(value) yield previous :+ next)

  /** A call's named arguments: exactly `required`, plus any of `optional`. */
  private final case class Named(fun: String, args: Map[String, ArgValue]):
    def get(name: String): Read[ArgValue] = args.get(name).toRight(s"$fun: missing argument '$name'")
    def number(name: String): Read[Double] = get(name).flatMap {
      case ArgValue.Num(value) => Right(value)
      case other => Left(s"$fun: '$name' must be a number, got $other")
    }
    def integer(name: String): Read[Int] = number(name).flatMap(value =>
      if value.isValidInt then Right(value.toInt) else Left(s"$fun: '$name' must be an integer"))
    def flag(name: String): Read[Boolean] = get(name).flatMap {
      case ArgValue.Bool(value) => Right(value)
      case other => Left(s"$fun: '$name' must be TRUE or FALSE, got $other")
    }
    def string(name: String): Read[String] = get(name).flatMap {
      case ArgValue.Str(value) => Right(value)
      case other => Left(s"$fun: '$name' must be a string, got $other")
    }
    def choice[A](name: String, values: Array[A]): Read[A] = string(name).flatMap(value =>
      values.find(v => snake(v.toString) == value).toRight(
        s"$fun: unknown $name '$value' (expected one of ${values.map(v => snake(v.toString)).mkString(", ")})"))
    def numbers(name: String): Read[Vector[Double]] = get(name).flatMap(elements).flatMap(items => all(items) {
      case ArgValue.Num(value) => Right(value)
      case other => Left(s"$fun: '$name' must contain numbers, got $other")
    })
    def integers(name: String): Read[Vector[Int]] = numbers(name).flatMap(values => all(values)(value =>
      if value.isValidInt then Right(value.toInt) else Left(s"$fun: '$name' must contain integers")))
    def strings(name: String): Read[Vector[String]] = get(name).flatMap(elements).flatMap(items => all(items) {
      case ArgValue.Str(value) => Right(value)
      case other => Left(s"$fun: '$name' must contain strings, got $other")
    })

  private def elements(value: ArgValue): Read[Vector[ArgValue]] = value match
    case ArgValue.Call("c", args) if args.forall(_.name.isEmpty) => Right(args.map(_.value))
    case other => Left(s"expected c(...), got $other")

  private def named(value: ArgValue, required: Set[String], optional: Set[String] = Set.empty): Read[Named] = value match
    case ArgValue.Call(fun, args) =>
      val names = args.flatMap(_.name)
      if names.length != args.length then Left(s"$fun: every argument must be named")
      else if names.distinct.length != names.length then Left(s"$fun: duplicate argument")
      else
        val missing = required.diff(names.toSet).toVector.sorted
        val unknown = names.toSet.diff(required ++ optional).toVector.sorted
        if missing.nonEmpty then Left(s"$fun: missing argument(s) ${missing.mkString(", ")}")
        else if unknown.nonEmpty then Left(s"$fun: unknown argument(s) ${unknown.mkString(", ")}")
        else Right(Named(fun, args.map(a => a.name.get -> a.value).toMap))
    case other => Left(s"expected a call, got $other")

  private def fun(value: ArgValue): String = value match
    case ArgValue.Call(name, _) => name
    case _ => ""

  private def readBaseline(value: ArgValue): Read[(BaselineBasis, Int, Intercept)] =
    val common = Set("degree", "intercept")
    for
      args <- named(value, if fun(value) == "dct" then common + "cutoff" else common)
      basis <- args.fun match
        case "dct" => args.number("cutoff").flatMap(seconds => DctCutoffPeriod.fromSeconds(seconds).left.map(_.message)).map(BaselineBasis.Dct(_))
        case name => Vector(BaselineBasis.Constant, BaselineBasis.Poly, BaselineBasis.Bs, BaselineBasis.Ns).find(_.id == name)
          .toRight(s"unknown baseline basis '$name'")
      degree <- args.integer("degree")
      _ <- if degree >= 1 then Right(()) else Left("baseline degree must be at least 1")
      intercept <- args.choice("intercept", Intercept.values)
    yield (basis, degree, intercept)

  private def readConfounds(value: ArgValue): Read[Option[ConfoundSpec]] =
    fun(value) match
      case "none" => named(value, Set.empty).map(_ => None)
      case "confounds" =>
        for
          args <- named(value, Set("motion", "acompcor", "white_matter", "csf", "global_signal", "censor"))
          motion <- args.choice("motion", MotionExpansion.values)
          components <- args.integer("acompcor")
          whiteMatter <- args.flag("white_matter")
          csf <- args.flag("csf")
          global <- args.flag("global_signal")
          censorValue <- args.get("censor")
          censor <- fun(censorValue) match
            case "none" => named(censorValue, Set.empty).map(_ => None)
            case "fd" =>
              for
                fd <- named(censorValue, Set("threshold", "before", "after", "min_segment", "max_fraction"))
                threshold <- fd.number("threshold")
                before <- fd.integer("before")
                after <- fd.integer("after")
                minimum <- fd.integer("min_segment")
                maximum <- fd.number("max_fraction")
                policy <- FdCensorPolicy.make(threshold, before, after, minimum, maximum).left.map(_.message)
              yield Some(policy)
            case other => Left(s"unknown censor policy '$other'")
          spec <- ConfoundSpec.make(motion, components, whiteMatter, csf, global, censor).left.map(_.message)
        yield Some(spec)
      case other => Left(s"unknown confound specification '$other'")

  private val controlNames = Set("weights", "projection", "missing")

  private def readEstimation(value: ArgValue): Read[FitStrategy] =
    def controlsOf(args: Named): Read[FitControls] =
      for
        weights <- args.get("weights").flatMap(readWeights)
        projection <- args.get("projection").flatMap(readProjection)
        missing <- args.choice("missing", MissingDataPolicy.values)
      yield FitControls(weights, projection, missing)
    def plain(build: FitControls => FitStrategy): Read[FitStrategy] =
      named(value, controlNames).flatMap(controlsOf).map(build)
    def gls(build: (AutocorrelationConfig, FitControls) => FitStrategy): Read[FitStrategy] =
      for
        args <- named(value, controlNames + "ar")
        ar <- args.get("ar").flatMap(readAutocorrelation)
        controls <- controlsOf(args)
      yield build(ar, controls)
    fun(value) match
      case "ols" => plain(FitStrategy.OrdinaryLeastSquares(_))
      case "runwise_ols" => plain(FitStrategy.RunwiseLeastSquares(_))
      case "fixed_effects_ols" => plain(FitStrategy.SeparateRunsThenFixedEffects(_))
      case "gls" => gls(FitStrategy.GeneralizedLeastSquares(_, _))
      case "runwise_gls" => gls(FitStrategy.RunwiseGeneralizedLeastSquares(_, _))
      case "lss" =>
        for
          args <- named(value, controlNames ++ Set("eps", "rank_tol"), Set("trial_term"))
          trialTerm <- if args.args.contains("trial_term") then args.string("trial_term").map(Some(_)) else Right(None)
          eps <- args.number("eps")
          rankTol <- args.number("rank_tol")
          config <- LssStrategyConfig(trialTerm, eps, rankTol).left.map(_.message)
          controls <- controlsOf(args)
        yield FitStrategy.LeastSquaresSeparate(config, controls)
      case other => Left(s"unknown estimation strategy '$other'")

  private def readAutocorrelation(value: ArgValue): Read[AutocorrelationConfig] =
    for
      args <- named(value, Set("order", "iterations", "global", "voxelwise", "exact_first", "censored", "coefficients", "bias"))
      _ <- if args.fun == "ar" then Right(()) else Left(s"expected ar(...), got ${args.fun}")
      order <- args.integer("order")
      iterations <- args.integer("iterations")
      global <- args.flag("global")
      voxelwise <- args.flag("voxelwise")
      exactFirst <- args.flag("exact_first")
      censored <- args.integers("censored")
      coefficientsValue <- args.get("coefficients")
      coefficients <- fun(coefficientsValue) match
        case "estimate" => named(coefficientsValue, Set.empty).map(_ => ArCoefficientSpec.Estimate)
        case "rho" => named(coefficientsValue, Set("value")).flatMap(_.number("value")).flatMap(rho => ArCoefficientSpec.rho(rho).left.map(_.message))
        case "phi" => named(coefficientsValue, Set("values")).flatMap(_.numbers("values")).map(ArCoefficientSpec.Phi(_))
        case other => Left(s"unknown AR coefficient policy '$other'")
      biasValue <- args.get("bias")
      bias <- fun(biasValue) match
        case "raw" => named(biasValue, Set.empty).map(_ => ArBiasCorrection.Raw)
        case "ols_design" => named(biasValue, Set("ceiling")).flatMap(_.integer("ceiling")).flatMap(c => ArBiasCorrection.olsDesign(c).left.map(_.message))
        case other => Left(s"unknown AR bias correction '$other'")
      config <- AutocorrelationConfig(order, iterations, global, voxelwise, exactFirst, censored, coefficients, bias).left.map(_.message)
    yield config

  private def readWeights(value: ArgValue): Read[ModelVolumeWeighting] =
    fun(value) match
      case "none" => named(value, Set.empty).map(_ => ModelVolumeWeighting.Disabled)
      case "dvars" =>
        for
          args <- named(value, Set("function", "scope"))
          functionValue <- args.get("function")
          function <- fun(functionValue) match
            case "inverse_squared" => named(functionValue, Set.empty).map(_ => DvarsWeightFunction.InverseSquared)
            case "soft_threshold" =>
              for
                f <- named(functionValue, Set("threshold", "steepness"))
                t <- f.number("threshold").flatMap(v => VolumeWeightThreshold(v).left.map(_.message))
                k <- f.number("steepness").flatMap(v => SoftThresholdSteepness(v).left.map(_.message))
              yield DvarsWeightFunction.SoftThreshold(t, k)
            case "tukey_bisquare" =>
              named(functionValue, Set("threshold")).flatMap(_.number("threshold")).flatMap(v => VolumeWeightThreshold(v).left.map(_.message))
                .map(DvarsWeightFunction.TukeyBisquare(_))
            case other => Left(s"unknown DVARS weight function '$other'")
          scope <- args.choice("scope", DvarsWeightScope.values)
        yield ModelVolumeWeighting.Estimated(DvarsWeightEstimator(function, scope))
      case "fixed" =>
        for
          args <- named(value, Set("values", "alignment"))
          values <- args.numbers("values")
          alignment <- args.choice("alignment", FixedWeightAlignment.values)
          weights <- ModelVolumeWeighting.fixed(values, alignment).left.map(_.message)
        yield weights
      case other => Left(s"unknown volume weighting '$other'")

  private def readProjection(value: ArgValue): Read[ModelNuisanceProjection] =
    fun(value) match
      case "none" => named(value, Set.empty).map(_ => ModelNuisanceProjection.Disabled)
      case "matrix" =>
        for
          args <- named(value, Set("rows", "cols", "values", "lambda"))
          rows <- args.integer("rows")
          cols <- args.integer("cols")
          values <- args.numbers("values")
          _ <- if rows >= 1 && cols >= 1 && rows.toLong * cols == values.length then Right(())
            else Left(s"matrix: $rows x $cols needs ${rows.toLong * cols} values, got ${values.length}")
          matrix <- NuisanceMatrix(Matrix.dense(rows, cols, values)).left.map(_.message)
          lambdaValue <- args.get("lambda")
          lambda <- fun(lambdaValue) match
            case "auto" => named(lambdaValue, Set.empty).map(_ => Regularization.Auto)
            case "gcv" => named(lambdaValue, Set.empty).map(_ => Regularization.Gcv)
            case "fixed" => named(lambdaValue, Set("value")).flatMap(_.number("value")).flatMap(v =>
              if v >= 0.0 && v.isFinite then Right(Regularization.Fixed(v)) else Left("regularization must be non-negative and finite"))
            case other => Left(s"unknown regularization '$other'")
        yield ModelNuisanceProjection.MatrixProjection(matrix, lambda)
      case other => Left(s"unknown nuisance projection '$other'")

  private def readRuns(value: ArgValue): Read[RunContrastCombination] =
    fun(value) match
      case "fixed_effects" => named(value, Set.empty).map(_ => RunContrastCombination.FixedEffects)
      case "concatenated" =>
        for
          args <- named(value, Set("columns"))
          names <- args.strings("columns")
          columns <- all(names)(name => ColumnId(name).left.map(_.message))
          _ <- if columns.nonEmpty && columns.distinct.length == columns.length then Right(()) else Left("provide distinct, nonempty task columns")
        yield RunContrastCombination.Concatenated(columns)
      case other => Left(s"unknown run combination '$other'")
