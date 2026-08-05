package scalafim.fmri.laws

import org.scalacheck.{Gen, Shrink}
import scalafim.fmri.design.{FactorLevelRegistry, MissingValuePolicy}
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.fit.RunPartition
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

enum GeneratedKernelFamily:
  case Spmg1, Spmg2, Spmg3, Fir, Tent, Bspline

final case class KernelCase private (
    family: GeneratedKernelFamily,
    basisWidth: Int,
    span: Double
):
  def hrf: Hrf =
    family match
      case GeneratedKernelFamily.Spmg1   => Hrfs.SPMG1
      case GeneratedKernelFamily.Spmg2   => Hrfs.SPMG2
      case GeneratedKernelFamily.Spmg3   => Hrfs.SPMG3
      case GeneratedKernelFamily.Fir     => Hrfs.fir(nBasis = basisWidth, span = Seconds(span))
      case GeneratedKernelFamily.Tent    => Hrfs.tent(nBasis = basisWidth, span = Seconds(span))
      case GeneratedKernelFamily.Bspline =>
        Hrfs.bspline(nBasis = basisWidth, span = Seconds(span), degree = math.min(3, basisWidth - 1))

object KernelCase:
  def from(
      family: GeneratedKernelFamily,
      basisWidth: Int,
      span: Double
  ): Option[KernelCase] =
    if basisWidth >= 2 && basisWidth <= 8 && span >= 8.0 && span <= 40.0 && span.isFinite then
      Some(new KernelCase(family, basisWidth, span))
    else None

  given Shrink[KernelCase] = Shrink.withLazyList { value =>
    LazyList(
      from(value.family, 2, value.span),
      from(value.family, value.basisWidth, 12.0),
      from(GeneratedKernelFamily.Spmg1, 2, 12.0)
    ).flatten.filterNot(_ == value).distinct
  }

final case class AcquisitionCase private (
    blockLengths: Vector[Int],
    repetitionTimes: Vector[Double],
    startTimes: Vector[Double],
    precision: Double,
    samplingFrame: SamplingFrame
):
  def mixed: Boolean = repetitionTimes.distinct.length > 1
  def timepoints: Int = blockLengths.sum

object AcquisitionCase:
  def from(
      blockLengths: Vector[Int],
      repetitionTimes: Vector[Double],
      startTimes: Vector[Double],
      precision: Double
  ): Option[AcquisitionCase] =
    SamplingFrame
      .validated(blockLengths, repetitionTimes, startTimes, precision)
      .toOption
      .map(frame => new AcquisitionCase(blockLengths, repetitionTimes, startTimes, precision, frame))

  given Shrink[AcquisitionCase] = Shrink.withLazyList { value =>
    val oneBlock =
      from(
        Vector(math.max(6, value.blockLengths.head / 2)),
        Vector(value.repetitionTimes.head),
        Vector(value.startTimes.head),
        math.min(value.precision, value.repetitionTimes.head / 4.0)
      )
    val shorter =
      from(
        value.blockLengths.map(length => math.max(6, length / 2)),
        value.repetitionTimes,
        value.startTimes,
        value.precision
      )
    LazyList(oneBlock, shorter).flatten.filterNot(_ == value).distinct
  }

final case class StimulusCase private (
    onsets: Vector[Double],
    durations: Vector[Double],
    amplitudes: Vector[Double],
    shift: Double,
    scale: Double
):
  def isImpulse: Boolean = durations.forall(_ == 0.0)
  def isEpoch: Boolean = durations.exists(_ > 0.0)

object StimulusCase:
  def from(
      onsets: Vector[Double],
      durations: Vector[Double],
      amplitudes: Vector[Double],
      shift: Double,
      scale: Double
  ): Option[StimulusCase] =
    val aligned = onsets.nonEmpty && onsets.length == durations.length && onsets.length == amplitudes.length
    val finite =
      onsets.forall(value => value >= 0.0 && value.isFinite) &&
        durations.forall(value => value >= 0.0 && value.isFinite) &&
        amplitudes.forall(_.isFinite) && shift >= 0.0 && shift.isFinite && scale.isFinite
    if aligned && finite && onsets == onsets.sorted && math.abs(scale) >= 0.125 then
      Some(new StimulusCase(onsets, durations, amplitudes, shift, scale))
    else None

  given Shrink[StimulusCase] = Shrink.withLazyList { value =>
    val keep = math.max(1, value.onsets.length / 2)
    LazyList(
      from(
        value.onsets.take(keep),
        value.durations.take(keep),
        value.amplitudes.take(keep),
        value.shift,
        value.scale
      ),
      from(value.onsets, value.durations.map(_ => 0.0), value.amplitudes, value.shift, value.scale),
      from(value.onsets, value.durations, value.amplitudes.map(math.signum), 0.0, math.signum(value.scale))
    ).flatten.filterNot(_ == value).distinct
  }

final case class EpochDuration private (value: Double):
  require(value > 0.0 && value.isFinite, "epoch duration must be positive and finite")

object EpochDuration:
  def from(value: Double): Option[EpochDuration] =
    if value > 0.0 && value.isFinite then Some(new EpochDuration(value)) else None

  given Shrink[EpochDuration] = Shrink.withLazyList { value =>
    LazyList(0.5, 1.0, 2.0)
      .filter(_ < value.value)
      .flatMap(from)
  }

final case class EventTableCase private (
    acquisition: AcquisitionCase,
    onsets: Vector[Double],
    probeOnsets: Vector[Double],
    durations: Vector[Double],
    conditions: Vector[String],
    modulators: Vector[Double],
    blockIds: Vector[Int],
    parentIds: Vector[String],
    factorLevels: FactorLevelRegistry
):
  val missingPolicy: MissingValuePolicy = MissingValuePolicy.ZeroContribution

  def table: DataTable =
    DataTable.fromColumns(
      "onset" -> Column.Doubles(onsets),
      "probe_onset" -> Column.Doubles(probeOnsets),
      "duration" -> Column.Doubles(durations),
      "condition" -> Column.Strings(conditions),
      "rt" -> Column.Doubles(modulators),
      "trial_id" -> Column.Strings(parentIds)
    )

  def permuted(order: Vector[Int]): EventTableCase =
    require(order.sorted == onsets.indices.toVector, "event order must be a permutation")
    new EventTableCase(
      acquisition,
      order.map(onsets),
      order.map(probeOnsets),
      order.map(durations),
      order.map(conditions),
      order.map(modulators),
      order.map(blockIds),
      order.map(parentIds),
      factorLevels
    )

object EventTableCase:
  def from(
      acquisition: AcquisitionCase,
      onsets: Vector[Double],
      probeOnsets: Vector[Double],
      durations: Vector[Double],
      conditions: Vector[String],
      modulators: Vector[Double],
      blockIds: Vector[Int],
      parentIds: Vector[String]
  ): Option[EventTableCase] =
    val lengths = Vector(
      onsets.length,
      probeOnsets.length,
      durations.length,
      conditions.length,
      modulators.length,
      blockIds.length,
      parentIds.length
    )
    val validLengths = lengths.headOption.exists(length => length >= 4 && lengths.forall(_ == length))
    val validValues =
      onsets.forall(value => value >= 0.0 && value.isFinite) &&
        probeOnsets.forall(value => value >= 0.0 && value.isFinite) &&
        durations.forall(value => value >= 0.0 && value.isFinite) &&
        conditions.toSet == Set("A", "B") &&
        modulators.count(_.isNaN) <= 1 && modulators.forall(value => value.isFinite || value.isNaN) &&
        blockIds.forall(index => index >= 0 && index < acquisition.blockLengths.length) &&
        parentIds.distinct.length == parentIds.length
    if validLengths && validValues then
      FactorLevelRegistry
        .of("condition" -> Vector("A", "B"))
        .toOption
        .map { factorLevels =>
          new EventTableCase(
            acquisition,
            onsets,
            probeOnsets,
            durations,
            conditions,
            modulators,
            blockIds,
            parentIds,
            factorLevels
          )
        }
    else None

  given Shrink[EventTableCase] = Shrink.withLazyList { value =>
    val indices = (0 until math.min(4, value.onsets.length)).toVector
    val compact =
      from(
        value.acquisition,
        indices.map(value.onsets),
        indices.map(value.probeOnsets),
        indices.map(value.durations),
        Vector("A", "B", "A", "B").take(indices.length),
        indices.map(value.modulators),
        indices.map(value.blockIds),
        indices.map(value.parentIds)
      )
    val withoutMissing =
      from(
        value.acquisition,
        value.onsets,
        value.probeOnsets,
        value.durations,
        value.conditions,
        value.modulators.map(value => if value.isNaN then 0.0 else value),
        value.blockIds,
        value.parentIds
      )
    LazyList(compact, withoutMissing).flatten.filterNot(_ == value).distinct
  }

final case class LinearCase private (
    rows: Int,
    predictors: Int,
    responses: Int,
    seed: Int,
    columnScales: Vector[Double],
    coefficients: Vector[Vector[Double]]
):
  lazy val designRows: Vector[Vector[Double]] =
    Vector.tabulate(rows) { row =>
      Vector.tabulate(predictors) { column =>
        val raw =
          if row < predictors then if row == column then 1.0 else 0.0
          else ((((row + 1) * (column + 3) + seed * (column + 1)) % 13) - 6).toDouble / 6.0
        raw * columnScales(column)
      }
    }

  lazy val responseRows: Vector[Vector[Double]] =
    designRows.map { row =>
      Vector.tabulate(responses) { response =>
        var sum = 0.0
        var predictor = 0
        while predictor < predictors do
          sum += row(predictor) * coefficients(predictor)(response)
          predictor += 1
        sum
      }
    }

  val scaleEvidence: Double =
    math.max(
      designRows.iterator.flatten.map(math.abs).maxOption.getOrElse(1.0),
      responseRows.iterator.flatten.map(math.abs).maxOption.getOrElse(1.0)
    )

  val conditionEvidence: Double =
    columnScales.max / columnScales.min * (rows + predictors).toDouble

object LinearCase:
  def from(
      rows: Int,
      predictors: Int,
      responses: Int,
      seed: Int,
      columnScales: Vector[Double],
      coefficients: Vector[Vector[Double]]
  ): Option[LinearCase] =
    val valid =
      predictors >= 1 && predictors <= 5 && rows >= predictors + 2 && responses >= 1 && responses <= 5 &&
        columnScales.length == predictors && columnScales.forall(value => value > 0.0 && value.isFinite) &&
        coefficients.length == predictors && coefficients.forall(_.length == responses) &&
        coefficients.flatten.forall(_.isFinite)
    if valid then Some(new LinearCase(rows, predictors, responses, seed, columnScales, coefficients))
    else None

  given Shrink[LinearCase] = Shrink.withLazyList { value =>
    val smallerPredictors = math.max(1, value.predictors - 1)
    val smallerResponses = math.max(1, value.responses - 1)
    val rebuilt =
      from(
        rows = math.max(smallerPredictors + 2, value.rows / 2),
        predictors = smallerPredictors,
        responses = smallerResponses,
        seed = 0,
        columnScales = value.columnScales.take(smallerPredictors).map(_ => 1.0),
        coefficients = value.coefficients.take(smallerPredictors).map(_.take(smallerResponses))
      )
    LazyList(rebuilt).flatten.filterNot(_ == value)
  }

final case class WeightedLinearCase private (system: LinearCase, weights: Vector[Double]):
  require(weights.length == system.rows, "weights must align with design rows")
  require(weights.forall(value => value > 0.0 && value.isFinite), "generated weights must be positive and finite")

  def squareRootWeights: Vector[Double] = weights.map(math.sqrt)

object WeightedLinearCase:
  def from(system: LinearCase, weights: Vector[Double]): Option[WeightedLinearCase] =
    if weights.length == system.rows && weights.forall(value => value > 0.0 && value.isFinite) then
      Some(new WeightedLinearCase(system, weights))
    else None

  given Shrink[WeightedLinearCase] = Shrink.withLazyList { value =>
    LazyList(from(value.system, Vector.fill(value.system.rows)(1.0))).flatten.filterNot(_ == value)
  }

final case class CensorCase private (
    acquisition: AcquisitionCase,
    censoredTimepoints: Vector[Int]
):
  val selectedTimepoints: Vector[Int] =
    (0 until acquisition.timepoints).filterNot(censoredTimepoints.toSet).toVector

  val partitions: Vector[RunPartition] =
    RunPartition.fromSamplingFrame(acquisition.samplingFrame, selectedTimepoints)

object CensorCase:
  def from(acquisition: AcquisitionCase, censoredTimepoints: Vector[Int]): Option[CensorCase] =
    val total = acquisition.timepoints
    val valid =
      censoredTimepoints == censoredTimepoints.distinct.sorted &&
        censoredTimepoints.forall(index => index >= 0 && index < total) &&
        total - censoredTimepoints.length >= acquisition.blockLengths.length * 3
    if valid then Some(new CensorCase(acquisition, censoredTimepoints)) else None

  given Shrink[CensorCase] = Shrink.withLazyList { value =>
    LazyList(
      from(value.acquisition, value.censoredTimepoints.take(1)),
      from(value.acquisition, Vector.empty)
    ).flatten.filterNot(_ == value).distinct
  }

final case class RankStressCase private (
    rows: Int,
    scale: Double,
    perturbation: Double
):
  def designRows: Vector[Vector[Double]] =
    Vector.tabulate(rows) { row =>
      val centered = row.toDouble - (rows - 1).toDouble / 2.0
      val alternate = if row % 2 == 0 then 1.0 else -1.0
      Vector(
        scale,
        scale * centered,
        scale * (centered + perturbation * alternate)
      )
    }

  def exactlyDeficient: Boolean = perturbation == 0.0

object RankStressCase:
  def from(rows: Int, scale: Double, perturbation: Double): Option[RankStressCase] =
    if rows >= 6 && rows <= 64 && scale > 0.0 && scale.isFinite &&
      perturbation >= 0.0 && perturbation.isFinite
    then Some(new RankStressCase(rows, scale, perturbation))
    else None

  given Shrink[RankStressCase] = Shrink.withLazyList { value =>
    LazyList(
      from(6, 1.0, value.perturbation),
      from(math.max(6, value.rows / 2), value.scale, 0.0)
    ).flatten.filterNot(_ == value).distinct
  }

object FirstLevelGenerators:
  private def valid[A](value: Option[A]): Gen[A] =
    value match
      case Some(generated) => Gen.const(generated)
      case None            => Gen.fail

  val kernelCase: Gen[KernelCase] =
    for
      family <- Gen.oneOf(GeneratedKernelFamily.values.toVector)
      width <- Gen.choose(2, 6)
      span <- Gen.oneOf(12.0, 18.0, 24.0, 32.0)
      generated <- valid(KernelCase.from(family, width, span))
    yield generated

  val acquisitionCase: Gen[AcquisitionCase] =
    for
      blocks <- Gen.choose(1, 3)
      lengths <- Gen.listOfN(blocks, Gen.choose(8, 28)).map(_.toVector)
      trs <- Gen.listOfN(blocks, Gen.oneOf(0.8, 1.0, 1.5, 2.0)).map(_.toVector)
      precision <- Gen.oneOf(0.05, 0.1, 0.2)
      starts = trs.map(_ / 2.0)
      generated <- valid(AcquisitionCase.from(lengths, trs, starts, precision))
    yield generated

  val stimulusCase: Gen[StimulusCase] =
    for
      count <- Gen.choose(1, 6)
      first <- Gen.choose(0, 8).map(_.toDouble)
      gap <- Gen.choose(2, 8).map(_.toDouble)
      epoch <- Gen.oneOf(false, true)
      duration <- if epoch then Gen.oneOf(0.25, 0.5, 1.0, 2.0, 4.0) else Gen.const(0.0)
      amplitudes <- Gen.listOfN(count, Gen.oneOf(-2.0, -1.0, -0.5, 0.5, 1.0, 2.0)).map(_.toVector)
      shift <- Gen.oneOf(0.0, 0.5, 1.0, 2.0, 4.0)
      scale <- Gen.oneOf(-3.0, -1.5, -0.5, 0.5, 1.5, 3.0)
      onsets = Vector.tabulate(count)(index => first + index * gap)
      generated <- valid(StimulusCase.from(onsets, Vector.fill(count)(duration), amplitudes, shift, scale))
    yield generated

  val epochDuration: Gen[EpochDuration] =
    Gen.oneOf(0.5, 1.0, 2.0, 4.0).flatMap(value => valid(EpochDuration.from(value)))

  val eventTableCase: Gen[EventTableCase] =
    for
      blocks <- Gen.choose(1, 2)
      perBlock <- Gen.choose(4, 6)
      tr <- Gen.oneOf(1.0, 1.5, 2.0)
      length <- Gen.choose(24, 40)
      acquisition <- valid(
        AcquisitionCase.from(
          Vector.fill(blocks)(length),
          Vector.fill(blocks)(tr),
          Vector.fill(blocks)(tr / 2.0),
          0.1
        )
      )
      count = blocks * perBlock
      missing <- Gen.option(Gen.choose(0, count - 1))
      baseModulators <- Gen.listOfN(count, Gen.choose(-20, 20).map(_.toDouble / 10.0)).map(_.toVector)
      blockIds = Vector.tabulate(count)(index => index / perBlock)
      within = Vector.tabulate(count)(index => index % perBlock)
      gap = math.min(6.0, (length * tr - 4.0) / (perBlock + 1).toDouble)
      onsets = within.map(index => 1.0 + index * gap)
      probes = onsets.map(_ + 1.0)
      conditions = Vector.tabulate(count)(index => if index % 2 == 0 then "A" else "B")
      modulators = baseModulators.zipWithIndex.map { case (value, index) =>
        if missing.contains(index) then Double.NaN else value
      }
      parents = Vector.tabulate(count)(index => s"trial-${index + 1}")
      generated <- valid(
        EventTableCase.from(
          acquisition,
          onsets,
          probes,
          Vector.fill(count)(0.5),
          conditions,
          modulators,
          blockIds,
          parents
        )
      )
    yield generated

  val linearCase: Gen[LinearCase] =
    for
      predictors <- Gen.choose(1, 4)
      rows <- Gen.choose(predictors + 3, predictors + 16)
      responses <- Gen.choose(1, 4)
      seed <- Gen.choose(0, 1000)
      exponents <- Gen.listOfN(predictors, Gen.choose(-2, 2)).map(_.toVector)
      coefficients <- Gen
        .listOfN(predictors * responses, Gen.choose(-30, 30).map(_.toDouble / 7.0))
        .map(_.toVector.grouped(responses).map(_.toVector).toVector)
      scales = exponents.map(exponent => math.pow(10.0, exponent.toDouble))
      generated <- valid(LinearCase.from(rows, predictors, responses, seed, scales, coefficients))
    yield generated

  val weightedLinearCase: Gen[WeightedLinearCase] =
    for
      system <- linearCase
      weights <- Gen
        .listOfN(system.rows, Gen.choose(1, 16).map(_.toDouble / 4.0))
        .map(_.toVector)
      generated <- valid(WeightedLinearCase.from(system, weights))
    yield generated

  val censorCase: Gen[CensorCase] =
    for
      acquisition <- acquisitionCase.suchThat(_.blockLengths.forall(_ >= 8))
      candidates = acquisition.blockLengths.zipWithIndex.flatMap { case (length, run) =>
        val start = acquisition.blockLengths.take(run).sum
        Vector(start + length / 3, start + (2 * length) / 3)
      }
      count <- Gen.choose(0, math.min(3, candidates.length))
      selected <- Gen.pick(count, candidates).map(_.toVector.sorted)
      generated <- valid(CensorCase.from(acquisition, selected))
    yield generated

  val rankStressCase: Gen[RankStressCase] =
    for
      rows <- Gen.choose(6, 30)
      exponent <- Gen.choose(-6, 6)
      perturbation <- Gen.oneOf(0.0, 1e-12, 1e-10, 1e-8, 1e-6)
      generated <- valid(RankStressCase.from(rows, math.pow(10.0, exponent.toDouble), perturbation))
    yield generated

  val rankDeficientCase: Gen[RankStressCase] =
    for
      rows <- Gen.choose(6, 30)
      exponent <- Gen.choose(-6, 6)
      generated <- valid(RankStressCase.from(rows, math.pow(10.0, exponent.toDouble), perturbation = 0.0))
    yield generated
