package scalafim.fmri.fit.profile

import gale.linalg.DMat
import scalafim.dataset.{DatasetFieldId, DatasetValue, FmriDataset, ResolvedDataSelection}
import scalafim.fmri.ar.{CoefficientScope, InitialConditionPolicy, WhiteningMethod, WhiteningPlan}
import scalafim.fmri.design.event.{CategoricalEvent, ContinuousEvent, Event, EventSchedule, EventTerm}
import scalafim.fmri.design.hrf.KernelBasisProvenance
import scalafim.fmri.fit.{CanonicalTemporalWhitening, ResponsePreparationIdentity, ResponsePreparationPlan}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{AmplitudeStructure, FitConfig, ProfileCriterion, ProfileHrfPlan, ProfileHrfSource}

/** Canonical identities describe the prepared numerical inputs. They are not
  * serialization formats or security hashes. Version changes intentionally
  * invalidate identities retained from the previous format.
  */
private[profile] object ProfileFitIdentity:
  import KernelBasisProvenance.{field, matrix, number, option, record}

  def ints(values: Seq[Int]): String = record("indices", values.map(_.toString)*)
  def strings(values: Seq[String]): String = record("labels", values*)
  def numbers(values: Seq[Double]): String = record("values", values.map(number)*)
  def mat(value: Mat): String = matrix(value.rows, value.cols, value.data)
  def dmat(value: DMat): String =
    val values = new Array[Double](value.rows * value.cols)
    value.copyRowMajorTo(values)
    matrix(value.rows, value.cols, values)

  def frame(value: SamplingFrame): String =
    val SamplingFrame(lens, tr, start, precision) = value
    record("frame", s"blockLens=${ints(lens)}", s"tr=${numbers(tr.map(_.value))}",
      s"startTime=${numbers(start.map(_.value))}", s"precision=${number(precision.value)}")

  def schedule(value: EventSchedule): String =
    record("schedule", value.events.map(e => record("event", e.id.value,
      number(e.onset.value), number(e.durationSeconds.value), e.blockId.toString))*)

  def event(value: Event): String = value match
    case CategoricalEvent(name, codes, levels) => record("categorical", name, ints(codes), strings(levels))
    case ContinuousEvent(name, values, tags, basis, modulators, mainEffect) =>
      // The realized basis values are the scientific input here; no captured
      // fitting implementation or display rendering participates in identity.
      val basisIdentity = basis.map(b => record("parametric_basis", b.argName, b.name,
        b.basisClass, strings(b.columns), strings(b.registryKeys), mat(b.y)))
      record("continuous", name, mat(values), strings(tags), option(basisIdentity),
        strings(modulators.map(_.value)), option(mainEffect.map(_.toString)))

  def term(value: EventTerm): String =
    val EventTerm(events, _, _, _, tag, phase, provenance) = value
    val rows = provenance.map: row =>
      val scalafim.fmri.design.EventRowProvenance(parent, phase, source, block, onset, duration) = row
      record("source_row", parent.value, option(phase.map(_.value)), source.toString,
        block.toString, number(onset.value), number(duration.value))
    val design = value.designMatrix(dropEmpty = false)
    record("term", schedule(value.schedule), record("events", events.map(event)*),
      option(tag), option(phase.map(_.value)), record("source_rows", rows*),
      strings(design.conditionTags), mat(design.data))

  def whitening(value: WhiteningPlan): String =
    val WhiteningPlan(scope, covered, initial, method) = value
    def coefficients(c: scalafim.fmri.ar.ArmaCoefficients): String =
      val scalafim.fmri.ar.ArmaCoefficients(phi, theta) = c
      record("arma", numbers(phi), numbers(theta))
    val scoped = scope match
      case CoefficientScope.Global(c) => record("global", coefficients(c))
      case CoefficientScope.ByRun(cs) => record("by_run", cs.map(coefficients)*)
    val first = initial match
      case InitialConditionPolicy.Identity => record("identity")
      case InitialConditionPolicy.ExactAr1 => record("exact_ar1")
      case InitialConditionPolicy.PrecomputedScale(scale) => record("precomputed_scale", number(scale))
    val methodId = method match
      case WhiteningMethod.Fixed => "fixed"
      case WhiteningMethod.Estimated => "estimated"
    val segments = covered.segments.map: s =>
      val scalafim.fmri.ar.TimeSegment(start, end, run) = s
      record("segment", start.toString, end.toString, run.toString)
    record("whitening", scoped, record("segments", segments*), covered.rows.toString, first, methodId)

  def config(value: FitConfig): String =
    val FitConfig(_, _, _, _, _, lss) = value
    val scalafim.fmri.model.LssConfig(trial, eps, tolerance) = lss
    record("fit_config", ResponsePreparationIdentity.provenance(ResponsePreparationPlan.fromConfig(value).provenance),
      record("lss", option(trial), number(eps), number(tolerance)))

  def datasetValue(value: DatasetValue): String = value match
    case DatasetValue.Text(text) => record("text", text)
    case DatasetValue.Number(n, source) => record("number", number(n), option(source))
    case DatasetValue.Integer(n, source) => record("integer", n.toString, option(source))
    case DatasetValue.Bool(b, source) => record("bool", b.toString, option(source))

  def fields(values: Map[DatasetFieldId, DatasetValue]): String =
    val entries = values.toVector.sortBy(_._1.value).map: (key, value) =>
      record("field", key.value, datasetValue(value))
    record("fields", entries*)

  def space(value: image4s.SampleSpace[? <: image4s.geometry.Frame[image4s.geometry.D3], image4s.geometry.D3]): String =
    import image4s.{AxisCoordinatesRecord, AxisRecord}
    import image4s.geometry.{CoordinateConvention, LengthUnit}
    val grid = value.grid
    val physical = grid.frame
    val unit = physical.unit match
      case LengthUnit.Millimeter => "millimeter"
      case LengthUnit.Meter => "meter"
      case LengthUnit.Micrometer => "micrometer"
    val convention = physical.convention match
      case CoordinateConvention.Unspecified => "unspecified"
      case CoordinateConvention.RAS => "ras"
      case CoordinateConvention.LPS => "lps"
    val axes = value.nonSpatialAxes.records.map: axis =>
      val AxisRecord(name, kind, coordinates) = axis
      val sampling = coordinates match
        case AxisCoordinatesRecord.Ordinal(extent) => record("ordinal", extent.toString)
        case AxisCoordinatesRecord.OrdinalValues(values) => record("ordinal_values", ints(values))
        case AxisCoordinatesRecord.Regular(extent, origin, step, unit) => record("regular", extent.toString, number(origin), number(step), unit)
        case AxisCoordinatesRecord.Explicit(values, unit) => record("explicit", numbers(values), unit)
        case AxisCoordinatesRecord.Categorical(labels) => record("categorical", strings(labels))
      record("axis", name, kind, sampling)
    record("space", ints(grid.shape), matrix(4, 4, grid.indexToFrame.rowMajor.toArray),
      option(grid.persistentId.map(_.value)), option(physical.persistentId.map(_.value)),
      physical.spatialRank.toString, unit, convention, record("axes", axes*))

  def dataset(value: FmriDataset): String =
    val time = value.timeAxis.blocks.map: block =>
      val scalafim.dataset.DatasetTimeBlock(ordinal, run, start, length) = block
      record("block", ordinal.value.toString, run.value, start.value.toString, length.toString)
    val scalafim.dataset.DatasetShape(sampling, timepoints) = value.shape
    record("dataset", value.id.value, space(sampling), timepoints.toString, ints(value.voxelDomain.indices),
      frame(value.samplingFrame), record("events", value.events.typedRows.map(r => fields(r.values))*),
      record("time_axis", time*), fields(value.metadata.typedValues), option(value.metadata.provenance.map(_.source)))

  def drive(source: ProfileHrfSource): String = source match
    case ProfileHrfSource.FixedCondition(_, convolved) => record("fixed", term(convolved.term))
    case ProfileHrfSource.TrialEvents(_, drive, _, _) =>
      record("trials", schedule(drive.schedule), strings(drive.conditionLabels.map(_.value)),
        strings(drive.trialLabels.map(_.value)), ints(drive.membership.conditionOfTrial),
        strings(drive.conditionForTrial.map(_.value)))

  def criterion(value: ProfileCriterion): String = value match
    case ProfileCriterion.PenalizedProfile(sigma2) => record("penalized_profile", number(sigma2))
    case ProfileCriterion.TrialRandomEffectsML(sigma2) => record("trial_random_effects_ml", number(sigma2))

  def amplitudes(value: AmplitudeStructure): String = value match
    case AmplitudeStructure.ConditionMeans => record("condition_means")
    case AmplitudeStructure.ConditionCenteredTrials(alpha) => record("condition_centered_trials", number(alpha.value), number(alpha.lambda))

  private[profile] def compactComparisonMarker(route: String, policy: ProfileDecodePolicy): String =
    if route == "direct-condition-compact" && CompactComparisonWorkspaceReceipt.enabled(policy.prior, policy.budget) then
      s"|compact-comparison=${field(CompactComparisonWorkspaceReceipt.PolicyId)}"
    else ""

  def canonical(plan: ProfileHrfPlan, selected: ResolvedDataSelection,
      temporal: CanonicalTemporalWhitening, policy: ProfileDecodePolicy, execution: ExecutionBudget,
      route: String, exactReadout: Boolean, ml: Option[(Double, Double)]): String =
    val (data, nuisance, fitConfig) = plan.source match
      case ProfileHrfSource.FixedCondition(fixed, _) => (fixed.model.dataset, fixed.model.designMatrix, fixed.config)
      case ProfileHrfSource.TrialEvents(dataset, _, baseline, config) => (dataset, baseline.designMatrix, config)
    val temporalId = temporal match
      case CanonicalTemporalWhitening.Iid => record("iid")
      case CanonicalTemporalWhitening.Shared(plan) => whitening(plan)
    val basis = plan.basis
    val basisValues = Array.tabulate(basis.fineCount * basis.rank)(i => basis.value(i % basis.rank, i / basis.rank))
    val grid = NodeGrid(basis.family.chart, basis.spec.nodesPerAxis)
    val scratch = new Array[Double](basis.family.jetComponents * basis.fineCount)
    val coefficients = new Array[Double](basis.family.jetComponents * basis.rank)
    val coefficientRecords = Vector.tabulate(grid.count): node =>
      basis.coefficientJetInto(grid.point(node), scratch, coefficients, basis.family.jetComponents)
      matrix(basis.family.jetComponents, basis.rank, coefficients)
    val mlIdentity = ml.map: (sigma2, lambda) =>
      record("trial_ml_evidence", "criterion-form=J=E+sigma2*D", s"sigma2=${number(sigma2)}",
        s"native-lambda=${number(lambda)}", "raw-energy=prepared-sparse-residual-plus-centered-penalty",
        "response=worker-owned-copy", "conditional-sd=sqrt(diag(2*sigma2*inverse(HJ)))",
        "terminal-evidence=required-at-returned-shape")
    val ExecutionBudget(blockSize, workers) = execution
    // Keep exact-readout last: public execution strips this legacy assertion
    // from its preparation binding, preserving every ML identity input.
    s"profile-fit/v3|dataset=${field(dataset(data))}|selected=${record("selected", ints(selected.timepoints), ints(selected.voxels))}|" +
      s"drive=${field(drive(plan.source))}|basis=${field(basis.provenance.canonical)}|basis-lags=${matrix(1, basis.fineCount, basis.lags)}|" +
      s"basis-values=${matrix(basis.fineCount, basis.rank, basisValues)}|basis-coefficients=${record("coefficient_jets", coefficientRecords*)}|" +
      s"nuisance=${mat(nuisance)}|config=${field(config(fitConfig))}|whitening=$temporalId|amplitudes=${amplitudes(plan.amplitudes)}|" +
      s"criterion=${criterion(plan.criterion)}|grid=${ints(policy.nodesPerAxis)}|decode=${field(ConditionProfileProvenance.budgetCanonical(policy.budget))}|" +
      s"prior=${ConditionProfileProvenance.priorCanonical(policy.prior)}|admission=${option(policy.observedAdmission.map(_.fingerprint))}|" +
      s"execution=${record("execution", blockSize.toString, workers.toString)}|route=${field(route)}|ml=${option(mlIdentity)}" +
      compactComparisonMarker(route, policy) + s"|exact-readout=$exactReadout"
