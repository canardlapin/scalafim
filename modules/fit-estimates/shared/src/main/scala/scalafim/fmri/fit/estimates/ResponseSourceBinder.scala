package scalafim.fmri.fit.estimates

import scalafim.estimates.*
import scalafim.fmri.ar.{NoisePooling, WhiteningMethod}
import scalafim.fmri.fit.{ArDiagnostics, DenseFmriFitResult, FixedEffectsFmriFitResult, FmriFitResult, FixedEffectsWeighting}
import scalafim.fmri.model.{CoefficientScope, FitEngine}

/** One row of the selected structural readout: a readout row label bound to a
  * scalar estimand of the materialized unit.
  */
final case class ReadoutRow(row: ReadoutRowId, estimand: EstimandId)

/** Identity that a bare [[FmriFitResult]] lacks: the materialized unit (its
  * revision, observations, bindings and ordered domain), the observation, the
  * selected readout and the ordered physical sample IDs. Every field is
  * verified against native provenance by [[ResponseSourceBinder.bind]].
  */
final case class MaterializedUnitBinding(
    unit: EstimateUnit,
    observation: ObservationId,
    readout: Vector[ReadoutRow],
    orderedSampleIds: Vector[Int]
)

enum SampleAxisSource:
  case Domain
  case FitResult

enum BindError:
  case MissingCoefficientAxis
  case UnknownObservation(observation: ObservationId)
  case EmptyReadout
  case UnknownReadoutEstimand(estimand: EstimandId)
  case ReadoutNotRealized(estimand: EstimandId)
  case ReadoutNotScalar(estimand: EstimandId)
  case ColumnAxisDisagrees(native: Vector[ColumnId], bound: Vector[ColumnId])
  case InvalidReadoutAxis(defect: ReadoutDefect)
  case NoSelectedColumns
  case SampleCountDisagrees(against: SampleAxisSource, expected: Int, actual: Int)
  case SampleOrderDisagrees(against: SampleAxisSource)
  case PreparationUnrecorded
  case NotCanonical(field: String, detail: String)

  def message: String = this match
    case MissingCoefficientAxis => "fit result carries no structural coefficient axis"
    case UnknownObservation(observation) => s"observation ${observation.value} is not in the materialized unit"
    case EmptyReadout => "selected readout has no rows"
    case UnknownReadoutEstimand(estimand) => s"readout estimand ${estimand.value} has no unit binding"
    case ReadoutNotRealized(estimand) => s"readout estimand ${estimand.value} has no effect product on the observation"
    case ReadoutNotScalar(estimand) => s"readout estimand ${estimand.value} is not a single readout row"
    case ColumnAxisDisagrees(native, bound) =>
      s"bound columns ${bound.map(_.value).mkString(",")} differ from native axis ${native.map(_.value).mkString(",")}"
    case InvalidReadoutAxis(defect) => s"invalid readout axis: ${defect.message}"
    case NoSelectedColumns => "readout selects no coefficient column"
    case SampleCountDisagrees(against, expected, actual) => s"ordered sample count $actual differs from $against count $expected"
    case SampleOrderDisagrees(against) => s"ordered sample IDs differ from the $against order"
    case PreparationUnrecorded => "fit result records no response-preparation provenance"
    case NotCanonical(field, detail) => s"$field provenance has no canonical encoding: $detail"

/** Binds a native fit result to an explicit materialized unit and derives the
  * source identity digests. It never reads coefficients, fitted values,
  * covariance, residual variance or estimability evidence.
  */
object ResponseSourceBinder:
  val DesignSchema: String = "scalafim.response-design/1"
  val PreparationSchema: String = "scalafim.response-preparation/1"
  val NoiseSchema: String = "scalafim.realized-noise/1"
  val CombinationSchema: String = "scalafim.run-combination/1"
  val ReadoutSchema: String = "scalafim.selected-readout/1"

  def bind(fit: FmriFitResult, m: MaterializedUnitBinding): Either[BindError, ResponseSourceBinding] =
    val unit = m.unit
    for
      axis <- fit.coefficientAxis.toRight(BindError.MissingCoefficientAxis)
      columns = axis.columnIds.map(id => ColumnId(id.value))
      observation <- unit.observations.find(_.id == m.observation).toRight(BindError.UnknownObservation(m.observation))
      _ <- if m.readout.nonEmpty then Right(()) else Left(BindError.EmptyReadout)
      bindings <- m.readout.foldLeft[Either[BindError, Vector[EstimandBinding]]](Right(Vector.empty)): (acc, row) =>
        acc.flatMap(found => readoutBinding(unit, m.observation, row.estimand, columns).map(found :+ _))
      readoutAxis <- ReadoutAxis.parse(m.readout.map(_.row)).left.map(BindError.InvalidReadoutAxis.apply)
      selected = columns.indices.filter(c => bindings.exists(_.weights(c) != 0.0)).map(columns).toVector
      _ <- if selected.nonEmpty then Right(()) else Left(BindError.NoSelectedColumns)
      _ <- sameOrder(SampleAxisSource.Domain, unit.domain.support, m.orderedSampleIds)
      _ <- sameOrder(SampleAxisSource.FitResult, fit.voxelIndices, m.orderedSampleIds)
      preparationRecord <- fit.preparationProvenance.toRight(BindError.PreparationUnrecorded)
      preparation <- canonical("preparation", preparationRecord).map(ResponseDigests.provider(PreparationSchema, _))
      noiseRecord <- canonical("noise", noiseProvenance(fit))
      combinationRecord <- canonical("run combination", combinationProvenance(fit, observation))
      designRecord <- designIdentity(axis)
    yield
      val design = ResponseDigests.provider(DesignSchema, designRecord)
      val readoutRows = m.readout.zip(bindings).map((row, binding) =>
        ResponseDigests.token(row.row.condition.value) + ResponseDigests.token(row.row.bin.toString) +
          ResponseDigests.token(row.estimand.value) + ResponseDigests.token(binding.weights.map(ResponseDigests.number).mkString(",")))
      val readout = ResponseDigests.provider(ReadoutSchema,
        ResponseDigests.token(columns.map(_.value).mkString(",")) + readoutRows.mkString)
      new ResponseSourceBinding(unit.revision, m.observation, design, preparation,
        ResponseDigests.provider(NoiseSchema, noiseRecord), ResponseDigests.provider(CombinationSchema, combinationRecord),
        readout, readoutAxis, columns, selected, ResponseDigests.features(unit.domain), realizedNoise(fit),
        realizedCombination(fit, observation))

  /** The native `DesignFingerprint` text renders rank-preview doubles with
    * `toString`, which differs between JVM and Scala.js. The design identity is
    * therefore re-derived from the fingerprint's exact matrix shape and IEEE-754
    * value bits plus the structural rows, columns and audit rendered by
    * [[CanonicalProvenance]].
    */
  private def designIdentity(axis: scalafim.fmri.design.CoefficientAxis): Either[BindError, String] =
    val encoding = axis.designFingerprint.canonicalEncoding
    val matrixStart = encoding.indexOf("|matrix=")
    val rowsStart = encoding.indexOf("|rows=")
    val valuesStart = encoding.lastIndexOf("|values=")
    if matrixStart < 0 || rowsStart < matrixStart || valuesStart < rowsStart then
      Left(BindError.NotCanonical("design", "unrecognized native fingerprint encoding"))
    else
      canonical("design", (axis.rowLayout, axis.columns, axis.audit)).map: structure =>
        ResponseDigests.token(encoding.substring(matrixStart, rowsStart)) +
          ResponseDigests.token(encoding.substring(valuesStart)) + structure

  private def readoutBinding(
      unit: EstimateUnit,
      observation: ObservationId,
      estimand: EstimandId,
      columns: Vector[ColumnId]
  ): Either[BindError, EstimandBinding] =
    unit.bindings.find(_.estimand == estimand).toRight(BindError.UnknownReadoutEstimand(estimand)).flatMap: binding =>
      val realized = unit.products.exists(p => p.kind == ProductKind.Effect && p.observations.contains(observation) &&
        p.targets.estimands.contains(estimand))
      if !realized then Left(BindError.ReadoutNotRealized(estimand))
      else if binding.rows != 1 then Left(BindError.ReadoutNotScalar(estimand))
      else if binding.columnIds != columns then Left(BindError.ColumnAxisDisagrees(columns, binding.columnIds))
      else Right(binding)

  private def sameOrder(against: SampleAxisSource, native: Vector[Int], ordered: Vector[Int]): Either[BindError, Unit] =
    if native.size != ordered.size then Left(BindError.SampleCountDisagrees(against, native.size, ordered.size))
    else if native != ordered then Left(BindError.SampleOrderDisagrees(against))
    else Right(())

  private def canonical(field: String, value: Any): Either[BindError, String] =
    CanonicalProvenance.render(value).left.map(BindError.NotCanonical(field, _))

  /** The native noise record: engine, flags and, for dense results, the
    * recorded AR whitening provenance and robust configuration.
    */
  private def noiseProvenance(fit: FmriFitResult): Any =
    val detail = fit match
      case dense: DenseFmriFitResult =>
        (dense.autocorrelation, dense.robustDiagnostics.map(d => (d.psi, d.scaleScope, d.iterations)))
      case _ => ("no dense noise record", None)
    (fit.engine, fit.summary.robust, fit.summary.autocorrelated, detail)

  private def combinationProvenance(fit: FmriFitResult, observation: Observation): Any =
    val policy = fit match
      case fixed: FixedEffectsFmriFitResult => Some(fixed.policy)
      case _ => None
    (fit.engine, fit.summary.coefficientScope, observation.acquisitions.map(_.value), policy)

  /** Native AR diagnostics record AR coefficients but no MA terms, so a fixed
    * whitening is never certified exact here (`exact = None`).
    */
  private[estimates] def realizedNoise(fit: FmriFitResult): RealizedNoise =
    val autocorrelation: Option[ArDiagnostics] = fit match
      case dense: DenseFmriFitResult => dense.autocorrelation
      case _ => None
    val robust = fit.summary.robust || fit.engine == FitEngine.RobustLeastSquares || (fit match
      case dense: DenseFmriFitResult => dense.robustDiagnostics.nonEmpty
      case _ => false)
    fit.engine match
      case FitEngine.LatentSketch | FitEngine.ReducedRankGls => RealizedNoise.LearnedSubspace
      case _ if robust => RealizedNoise.Robust
      case _ =>
        autocorrelation match
          case Some(ar) =>
            val pooling =
              if !ar.sharedNormalizedCovariance then NoiseScope.PerFeature
              else ar.whitening.pooling match
                case NoisePooling.Global => NoiseScope.Global
                case NoisePooling.Run => NoiseScope.PerRun
            ar.whitening.method match
              case WhiteningMethod.Fixed => RealizedNoise.FixedAr(ar.order, pooling, None)
              case WhiteningMethod.Estimated => RealizedNoise.EstimatedAr(ar.order, pooling)
          case None if fit.summary.autocorrelated => RealizedNoise.Unrecorded
          case None =>
            fit.engine match
              case FitEngine.OrdinaryLeastSquares | FitEngine.LeastSquaresSeparate | FitEngine.RunwiseLeastSquares |
                  FitEngine.FixedEffects => RealizedNoise.White
              case _ => RealizedNoise.Unrecorded

  /** Only a shared fit of a single acquisition is a recorded single run.
    * Inverse-covariance fixed effects use estimated weights. A joint or
    * runwise multi-run fit has no representable combination in v1.
    */
  private[estimates] def realizedCombination(fit: FmriFitResult, observation: Observation): RealizedCombination =
    fit match
      case fixed: FixedEffectsFmriFitResult =>
        fixed.policy.weighting match
          case FixedEffectsWeighting.InverseCovariance => RealizedCombination.EstimatedWeights
      case _ =>
        if observation.acquisitions.size == 1 && fit.summary.coefficientScope == CoefficientScope.SharedAcrossRuns then
          RealizedCombination.SingleRun
        else RealizedCombination.Unrecorded

/** Platform-stable canonical text for immutable provenance records. Numbers are
  * encoded by IEEE-754 bits after widening to `Double`, so JVM and Scala.js
  * agree; maps and sets are ordered by their canonical text. Anything that is
  * not a primitive, string, collection or product is refused.
  */
private[estimates] object CanonicalProvenance:
  def render(value: Any): Either[String, String] =
    val out = new java.lang.StringBuilder()
    def put(tag: String, text: String): Unit =
      val _ = out.append(tag).append(ResponseDigests.token(text))
    def nested(value: Any, path: String): Either[String, String] =
      val saved = out.length
      loop(value, path).map: _ =>
        val text = out.substring(saved)
        out.setLength(saved)
        text
    def all(values: Iterable[Any], path: String): Either[String, Unit] =
      values.zipWithIndex.foldLeft[Either[String, Unit]](Right(()))((acc, entry) => acc.flatMap(_ => loop(entry._1, s"$path[${entry._2}]")))
    def loop(value: Any, path: String): Either[String, Unit] = value match
      case null => Left(s"$path is null")
      case text: String => Right(put("s", text))
      case flag: Boolean => Right(put("b", flag.toString))
      case character: Char => Right(put("c", character.toInt.toString))
      case long: Long => Right(put("j", long.toString))
      case int: Int => Right(put("n", ResponseDigests.number(int.toDouble)))
      case short: Short => Right(put("n", ResponseDigests.number(short.toDouble)))
      case byte: Byte => Right(put("n", ResponseDigests.number(byte.toDouble)))
      case float: Float => Right(put("n", ResponseDigests.number(float.toDouble)))
      case double: Double => Right(put("n", ResponseDigests.number(double)))
      case map: scala.collection.Map[?, ?] =>
        map.toVector.foldLeft[Either[String, Vector[String]]](Right(Vector.empty)): (acc, entry) =>
          acc.flatMap(entries => nested(entry, s"$path{}").map(entries :+ _))
        .map: entries =>
          put("m", entries.size.toString)
          entries.sorted.foreach(entry => out.append(entry))
      case set: scala.collection.Set[?] =>
        set.toVector.foldLeft[Either[String, Vector[String]]](Right(Vector.empty)): (acc, item) =>
          acc.flatMap(items => nested(item, s"$path{}").map(items :+ _))
        .map: items =>
          put("u", items.size.toString)
          items.sorted.foreach(item => out.append(item))
      case array: Array[?] =>
        put("a", array.length.toString)
        all(array.toVector, path)
      case seq: scala.collection.Seq[?] =>
        put("a", seq.size.toString)
        all(seq, path)
      case product: Product =>
        put("p", product.productPrefix)
        put("a", product.productArity.toString)
        all(product.productIterator.toVector, s"$path.${product.productPrefix}")
      case other => Left(s"$path has unsupported type ${other.getClass.getName}")
    loop(value, "record").map(_ => out.toString)
