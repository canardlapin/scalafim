package scalafim.fmri.model

import gale.linalg.Matrix
import scalafim.fmri.design.RunContrastCombination
import scalafim.fmri.design.formula.{ModelJsonCodec, ModelJsonError, PortableJson}
import scalafim.fmri.design.formula.PortableJson.{Cursor, Result, admit, ensure, fail, traverse}
import ujson.{Bool, Num, Obj, Str, Value}

/** One saved model: every portable piece a studio needs to reopen it.
  *
  * `build` carries the event formula, its derived declarations and their
  * missing-row policy, the baseline, contrasts, nuisance matrices and the fit
  * strategy including its [[FitControls]]. Confound preparation and the run
  * combination are model choices that live outside the build spec, so the
  * document binds them here rather than leaving them to separate files.
  */
final case class ModelDocument(
    build: ModelBuildSpec,
    confounds: Option[ConfoundSpec] = None,
    runCombination: RunContrastCombination = RunContrastCombination.FixedEffects
)

/** Strict, versioned model document: `scalafim.model-document`, version 1.
  *
  * {{{
  * {"schema":"scalafim.model-document","version":1,
  *  "formula":"onset ~ ...",
  *  "derived":{"missingRows":"drop","columns":["reward: number = gain - loss"]},
  *  "build":{ the scalafim.model-build v1 fields after "formula", except that
  *            "strategy" is always {"kind":...,"controls":{...}} },
  *  "confounds":null | {"motion":"Friston24",...,"censor":null | {...}},
  *  "runCombination":{"type":"fixed-effects"}}
  * }}}
  *
  * Decoding is strict: exact field sets at every level, no duplicate keys,
  * finite numbers, canonical ids; each error names the JSON path of the
  * offending value. Encoding rejects anything the decoder could not read back,
  * and encoded text is byte-identical on the JVM and Scala.js because numbers
  * are rendered through [[scalafim.fmri.design.PortableNumber]].
  *
  * [[decode]] also upgrades a legacy `scalafim.model-build` v1 document,
  * which has no derived plan, confounds, run combination or fit controls.
  */
object ModelDocumentJsonCodec:
  val Schema: String = "scalafim.model-document"
  val Version: Int = 1

  private val documentFields = Set("schema", "version", "formula", "derived", "build", "confounds", "runCombination")

  def encode(document: ModelDocument): Either[ModelJsonError, String] =
    for
      formula <- ModelBuildSpecJsonCodec.formulaValue(document.build, "$.formula")
      body <- ModelBuildSpecJsonCodec.encodeBody(document.build, "$.build", strategy)
      confounds <- PortableJson.optional(document.confounds)(spec => confoundsValue(spec, "$.confounds"))
      runCombination <- ModelJsonCodec.runCombinationValue(document.runCombination, "$.runCombination")
      text <- PortableJson.render(Obj(
        "schema" -> Str(Schema), "version" -> Num(Version), "formula" -> formula,
        "derived" -> ModelJsonCodec.derivedEventsValue(document.build.derived),
        "build" -> Obj.from(body), "confounds" -> confounds, "runCombination" -> runCombination
      ))
    yield text

  def decode(input: String): Either[ModelJsonError, ModelDocument] =
    for
      root <- PortableJson.parse(input)
      schema <- root.field("schema").flatMap(_.str)
      document <- schema match
        case Schema => readDocument(root)
        case "scalafim.model-build" => ModelBuildSpecJsonCodec.decode(input).map(ModelDocument(_))
        case other => fail("$.schema", s"unknown model document schema '$other'")
    yield document

  private def readDocument(root: Cursor): Result[ModelDocument] =
    for
      _ <- root.fields(documentFields)
      version <- root.field("version").flatMap(_.integer)
      _ <- ensure(version == Version, "$.version", s"unsupported model document version (expected $Version)")
      formula <- root.field("formula").flatMap(ModelBuildSpecJsonCodec.readFormula)
      derived <- root.field("derived").flatMap(ModelJsonCodec.readDerivedEvents)
      build <- root.field("build")
      _ <- build.fields(ModelBuildSpecJsonCodec.bodyFields)
      spec <- ModelBuildSpecJsonCodec.readBody(build, formula, readStrategy)
      confounds <- root.field("confounds").flatMap(_.nullable(readConfounds))
      runCombination <- root.field("runCombination").flatMap(ModelJsonCodec.readRunCombination)
    yield ModelDocument(spec.copy(derived = derived), confounds, runCombination)

  private def number(value: Double, path: String): Result[Value] = PortableJson.number(value, path)

  private def kind(value: Cursor): Result[String] = value.field("kind").flatMap(_.str)

  private def unknown[A](value: Cursor, what: String, name: String): Result[A] =
    fail(s"${value.path}.kind", s"unknown $what '$name'")

  // ----------------------------------------------------------------- strategy

  /** Every portable strategy is an object with its controls, defaults included. */
  private def strategy(value: FitStrategy, path: String): Result[Value] =
    def withControls(name: String, controls: FitControls, fields: (String, Value)*): Result[Value] =
      controlsValue(controls, s"$path.controls").map(c => Obj.from(Vector("kind" -> Str(name)) ++ fields ++ Vector("controls" -> c)))
    value match
      case FitStrategy.OrdinaryLeastSquares(controls) => withControls("ols", controls)
      case FitStrategy.RunwiseLeastSquares(controls) => withControls("runwise-ols", controls)
      case FitStrategy.SeparateRunsThenFixedEffects(controls) => withControls("fixed-effects-ols", controls)
      case FitStrategy.GeneralizedLeastSquares(ar, controls) =>
        ModelBuildSpecJsonCodec.autocorrelation(ar, s"$path.ar").flatMap(a => withControls("gls", controls, "ar" -> a))
      case FitStrategy.RunwiseGeneralizedLeastSquares(ar, controls) =>
        ModelBuildSpecJsonCodec.autocorrelation(ar, s"$path.ar").flatMap(a => withControls("runwise-gls", controls, "ar" -> a))
      case FitStrategy.LeastSquaresSeparate(config, controls) =>
        ModelBuildSpecJsonCodec.lss(config, s"$path.lss").flatMap(l => withControls("lss", controls, "lss" -> l))
      case other => fail(path, s"strategy ${other.engine} has no portable model document representation")

  private def readStrategy(value: Cursor): Result[FitStrategy] =
    def controls = value.field("controls").flatMap(readControls)
    kind(value).flatMap {
      case "ols" => for _ <- value.fields(Set("kind", "controls")); c <- controls yield FitStrategy.OrdinaryLeastSquares(c)
      case "runwise-ols" => for _ <- value.fields(Set("kind", "controls")); c <- controls yield FitStrategy.RunwiseLeastSquares(c)
      case "fixed-effects-ols" => for _ <- value.fields(Set("kind", "controls")); c <- controls yield FitStrategy.SeparateRunsThenFixedEffects(c)
      case "gls" =>
        for _ <- value.fields(Set("kind", "ar", "controls")); ar <- value.field("ar").flatMap(ModelBuildSpecJsonCodec.readAutocorrelation); c <- controls
        yield FitStrategy.GeneralizedLeastSquares(ar, c)
      case "runwise-gls" =>
        for _ <- value.fields(Set("kind", "ar", "controls")); ar <- value.field("ar").flatMap(ModelBuildSpecJsonCodec.readAutocorrelation); c <- controls
        yield FitStrategy.RunwiseGeneralizedLeastSquares(ar, c)
      case "lss" =>
        for _ <- value.fields(Set("kind", "lss", "controls")); config <- value.field("lss").flatMap(ModelBuildSpecJsonCodec.readLss); c <- controls
        yield FitStrategy.LeastSquaresSeparate(config, c)
      case other => unknown(value, "strategy", other)
    }

  // ----------------------------------------------------------------- controls

  private def controlsValue(value: FitControls, path: String): Result[Value] =
    for
      volume <- volumeWeighting(value.volumeWeighting, s"$path.volumeWeighting")
      projection <- nuisanceProjection(value.nuisanceProjection, s"$path.nuisanceProjection")
    yield Obj("volumeWeighting" -> volume, "nuisanceProjection" -> projection, "missingData" -> Str(value.missingData.toString))

  private def readControls(value: Cursor): Result[FitControls] =
    for
      _ <- value.fields(Set("volumeWeighting", "nuisanceProjection", "missingData"))
      volume <- value.field("volumeWeighting").flatMap(readVolumeWeighting)
      projection <- value.field("nuisanceProjection").flatMap(readNuisanceProjection)
      missingData <- value.field("missingData").flatMap(ModelBuildSpecJsonCodec.enumCase(_, "missing-data policy", MissingDataPolicy.values))
    yield FitControls(volume, projection, missingData)

  private def volumeWeighting(value: ModelVolumeWeighting, path: String): Result[Value] = value match
    case ModelVolumeWeighting.Disabled => Right(Obj("kind" -> Str("disabled")))
    case ModelVolumeWeighting.Estimated(estimator) =>
      dvarsFunction(estimator.function, s"$path.function").map(f =>
        Obj("kind" -> Str("estimated"), "function" -> f, "scope" -> Str(estimator.scope.toString)))
    case ModelVolumeWeighting.Fixed(weights, alignment) =>
      PortableJson.numbers(weights.values, s"$path.weights").map(w =>
        Obj("kind" -> Str("fixed"), "weights" -> w, "alignment" -> Str(alignment.toString)))

  private def readVolumeWeighting(value: Cursor): Result[ModelVolumeWeighting] =
    kind(value).flatMap {
      case "disabled" => value.fields(Set("kind")).map(_ => ModelVolumeWeighting.Disabled)
      case "estimated" =>
        for
          _ <- value.fields(Set("kind", "function", "scope"))
          function <- value.field("function").flatMap(readDvarsFunction)
          scope <- value.field("scope").flatMap(ModelBuildSpecJsonCodec.enumCase(_, "DVARS weight scope", DvarsWeightScope.values))
        yield ModelVolumeWeighting.Estimated(DvarsWeightEstimator(function, scope))
      case "fixed" =>
        for
          _ <- value.fields(Set("kind", "weights", "alignment"))
          list <- value.field("weights")
          values <- list.arr.flatMap(items => traverse(items)(_.finite))
          weights <- admit(TimepointWeights(values), list.path)(_.message)
          alignment <- value.field("alignment").flatMap(ModelBuildSpecJsonCodec.enumCase(_, "weight alignment", FixedWeightAlignment.values))
        yield ModelVolumeWeighting.Fixed(weights, alignment)
      case other => unknown(value, "volume weighting", other)
    }

  private def dvarsFunction(value: DvarsWeightFunction, path: String): Result[Value] = value match
    case DvarsWeightFunction.InverseSquared => Right(Obj("kind" -> Str("inverse-squared")))
    case DvarsWeightFunction.SoftThreshold(threshold, steepness) =>
      for t <- number(threshold.value, s"$path.threshold"); k <- number(steepness.value, s"$path.steepness")
      yield Obj("kind" -> Str("soft-threshold"), "threshold" -> t, "steepness" -> k)
    case DvarsWeightFunction.TukeyBisquare(threshold) =>
      number(threshold.value, s"$path.threshold").map(t => Obj("kind" -> Str("tukey-bisquare"), "threshold" -> t))

  private def readDvarsFunction(value: Cursor): Result[DvarsWeightFunction] =
    def threshold = value.field("threshold").flatMap(c => c.finite.flatMap(t => admit(VolumeWeightThreshold(t), c.path)(_.message)))
    kind(value).flatMap {
      case "inverse-squared" => value.fields(Set("kind")).map(_ => DvarsWeightFunction.InverseSquared)
      case "soft-threshold" =>
        for
          _ <- value.fields(Set("kind", "threshold", "steepness"))
          t <- threshold
          k <- value.field("steepness").flatMap(c => c.finite.flatMap(k => admit(SoftThresholdSteepness(k), c.path)(_.message)))
        yield DvarsWeightFunction.SoftThreshold(t, k)
      case "tukey-bisquare" => for _ <- value.fields(Set("kind", "threshold")); t <- threshold yield DvarsWeightFunction.TukeyBisquare(t)
      case other => unknown(value, "DVARS weight function", other)
    }

  private def nuisanceProjection(value: ModelNuisanceProjection, path: String): Result[Value] = value match
    case ModelNuisanceProjection.Disabled => Right(Obj("kind" -> Str("disabled")))
    case ModelNuisanceProjection.MatrixProjection(matrix, lambda) =>
      val m = matrix.matrix
      val data = new Array[Double](m.rows * m.cols)
      m.copyRowMajorTo(data)
      for
        values <- PortableJson.numbers(data.toVector, s"$path.values")
        l <- regularization(lambda, s"$path.lambda")
      yield Obj("kind" -> Str("matrix"), "rows" -> Num(m.rows), "cols" -> Num(m.cols), "values" -> values, "lambda" -> l)

  private def readNuisanceProjection(value: Cursor): Result[ModelNuisanceProjection] =
    kind(value).flatMap {
      case "disabled" => value.fields(Set("kind")).map(_ => ModelNuisanceProjection.Disabled)
      case "matrix" =>
        for
          _ <- value.fields(Set("kind", "rows", "cols", "values", "lambda"))
          rows <- value.field("rows").flatMap(_.integer)
          cols <- value.field("cols").flatMap(_.integer)
          values <- value.field("values").flatMap(_.arr).flatMap(items => traverse(items)(_.finite))
          _ <- ensure(rows >= 1 && cols >= 1 && rows.toLong * cols == values.length, value.path,
            s"invalid matrix: $rows x $cols needs ${rows.toLong * cols} values, got ${values.length}")
          matrix <- admit(NuisanceMatrix(Matrix.dense(rows, cols, values)), value.path)(_.message)
          lambda <- value.field("lambda").flatMap(readRegularization)
        yield ModelNuisanceProjection.MatrixProjection(matrix, lambda)
      case other => unknown(value, "nuisance projection", other)
    }

  private def regularization(value: Regularization, path: String): Result[Value] = value match
    case Regularization.Auto => Right(Obj("kind" -> Str("auto")))
    case Regularization.Gcv => Right(Obj("kind" -> Str("gcv")))
    case Regularization.Fixed(lambda) =>
      if lambda < 0.0 then fail(s"$path.value", "regularization must be non-negative")
      else number(lambda, s"$path.value").map(v => Obj("kind" -> Str("fixed"), "value" -> v))

  private def readRegularization(value: Cursor): Result[Regularization] =
    kind(value).flatMap {
      case "auto" => value.fields(Set("kind")).map(_ => Regularization.Auto)
      case "gcv" => value.fields(Set("kind")).map(_ => Regularization.Gcv)
      case "fixed" =>
        for
          _ <- value.fields(Set("kind", "value"))
          lambda <- value.field("value").flatMap(_.finite)
          _ <- ensure(lambda >= 0.0, s"${value.path}.value", "regularization must be non-negative")
        yield Regularization.Fixed(lambda)
      case other => unknown(value, "regularization", other)
    }

  // ---------------------------------------------------------------- confounds

  private def confoundsValue(spec: ConfoundSpec, path: String): Result[Value] =
    for
      censor <- PortableJson.optional(spec.censor) { policy =>
        for
          threshold <- number(policy.threshold, s"$path.censor.threshold")
          fraction <- number(policy.maximumCensoredFraction, s"$path.censor.maximumCensoredFraction")
        yield Obj("threshold" -> threshold, "before" -> Num(policy.before), "after" -> Num(policy.after),
          "minimumRetainedSegment" -> Num(policy.minimumRetainedSegment), "maximumCensoredFraction" -> fraction)
      }
    yield Obj("motion" -> Str(spec.motion.toString), "acompcorComponents" -> Num(spec.acompcorComponents),
      "includeWhiteMatter" -> Bool(spec.includeWhiteMatter), "includeCsf" -> Bool(spec.includeCsf),
      "includeGlobalSignal" -> Bool(spec.includeGlobalSignal), "censor" -> censor)

  private def readConfounds(value: Cursor): Result[ConfoundSpec] =
    for
      _ <- value.fields(Set("motion", "acompcorComponents", "includeWhiteMatter", "includeCsf", "includeGlobalSignal", "censor"))
      motion <- value.field("motion").flatMap(ModelBuildSpecJsonCodec.enumCase(_, "motion expansion", MotionExpansion.values))
      components <- value.field("acompcorComponents").flatMap(_.integer)
      whiteMatter <- value.field("includeWhiteMatter").flatMap(_.bool)
      csf <- value.field("includeCsf").flatMap(_.bool)
      global <- value.field("includeGlobalSignal").flatMap(_.bool)
      censor <- value.field("censor").flatMap(_.nullable(readCensor))
      spec <- admit(ConfoundSpec.make(motion, components, whiteMatter, csf, global, censor), s"${value.path}.acompcorComponents")(_.message)
    yield spec

  private def readCensor(value: Cursor): Result[FdCensorPolicy] =
    for
      _ <- value.fields(Set("threshold", "before", "after", "minimumRetainedSegment", "maximumCensoredFraction"))
      threshold <- value.field("threshold").flatMap(_.finite)
      before <- value.field("before").flatMap(_.integer)
      after <- value.field("after").flatMap(_.integer)
      minimum <- value.field("minimumRetainedSegment").flatMap(_.integer)
      maximum <- value.field("maximumCensoredFraction").flatMap(_.finite)
      policy <- admit(FdCensorPolicy.make(threshold, before, after, minimum, maximum), value.path)(_.message)
    yield policy
