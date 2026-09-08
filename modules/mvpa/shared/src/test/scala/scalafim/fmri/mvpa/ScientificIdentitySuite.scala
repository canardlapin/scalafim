package scalafim.fmri.mvpa

import resample4s.core.IndexSpace
import resample4s.core.Injection

private[mvpa] class ToyScientificSource(
    val samples: AxisRef[SampleId],
    val features: AxisRef[FeatureId],
    val identity: ScientificSourceIdentity
) extends ScientificSource:
  override type Neural = features.Id
  override type NeuralKey = FeatureId

  override val neuralAxisName: ScientificAxisName =
    ScientificPlanFixtures.NeuralAxisName

  override def neuralAxis: AxisRef.Aux[FeatureId, Neural] =
    features

private[mvpa] final class ToyEvidenceDesign(
    val identity: DesignIdentity,
    val referencedAxes: Vector[DesignAxisReference]
) extends EvidenceDesign

private[mvpa] final case class ToyEstimate(value: Double)

private[mvpa] enum ToyRejection:
  case Degenerate

private[mvpa] enum ToyFailure:
  case Numerical

private[mvpa] final class ToyEstimand(parameter: String) extends Estimand[ToyScientificSource, ToyEvidenceDesign]:
  override type Result = ToyEstimate
  override type Rejection = ToyRejection
  override type Failure = ToyFailure

  override val identity: EstimandIdentity =
    EstimandIdentity(
      EstimandKind.unsafe("toy-estimate"),
      Vector("parameter" -> parameter)
    ).toOption.get

  override val defaultBoundaries: RequestedBoundaries =
    ScientificPlanFixtures.boundaries("estimate")

  override def rejectionMessage(value: ToyRejection): String =
    value match
      case ToyRejection.Degenerate => "the toy estimate is degenerate"

  override def failureMessage(value: ToyFailure): String =
    value match
      case ToyFailure.Numerical => "the toy numerical kernel failed"

private[mvpa] object ScientificPlanFixtures:
  val SampleAxisName: ScientificAxisName =
    ScientificAxisName.unsafe("samples")

  val NeuralAxisName: ScientificAxisName =
    ScientificAxisName.unsafe("neural")

  def source(
      sourceVersion: String = "observations-v1",
      salt: Int = 701
  ): ToyScientificSource =
    val samples = MvpaLawFixtures.sampleAxis(4, salt)
    val features = MvpaLawFixtures.featureAxis(4, salt + 1)
    val identity = ScientificSourceIdentity(
      ScientificSourceKind.unsafe("observations"),
      Vector(
        ScientificSourceAxis(NeuralAxisName, features.identity),
        ScientificSourceAxis(SampleAxisName, samples.identity)
      ),
      Vector("version" -> sourceVersion, "owner" -> "fixture")
    ).toOption.get
    new ToyScientificSource(samples, features, identity)

  def design(
      source: ToyScientificSource,
      policy: String = "leave-one-run-out",
      reverseReferences: Boolean = false
  ): ToyEvidenceDesign =
    val references = Vector(
      DesignAxisReference(SampleAxisName, source.samples.identity),
      DesignAxisReference(NeuralAxisName, source.features.identity)
    )
    new ToyEvidenceDesign(
      DesignIdentity(
        DesignKind.unsafe("validation"),
        Vector("policy" -> policy)
      ).toOption.get,
      if reverseReferences then references.reverse else references
    )

  def frame(
      source: ToyScientificSource,
      id: String = "language-roi",
      positions: Vector[Int] = Vector(0, 2)
  ): MeasurementFrame[source.Neural, source.NeuralKey, NoRendition.type] =
    val indexSpace = IndexSpace.of(source.features.size).toOption.get
    val injection = Injection
      .from(IArray.unsafeFromArray(positions.toArray), indexSpace)
      .toOption
      .get
    val measurement = Measurement
      .hardSelection(source.features, MeasurementId.unsafe(id), injection)
      .toOption
      .get
    MeasurementFrame(source.features)(
      Vector(MeasurementEntry(measurement, NoRendition))
    ).toOption.get

  def boundaries(ids: String*): RequestedBoundaries =
    RequestedBoundaries(
      ids.map: id =>
        OutputBoundaryIdentity(OutputBoundaryId.unsafe(id)).toOption.get
    ).toOption.get

  def normalization(id: String): NormalizationIdentity =
    NormalizationIdentity(
      Vector(
        NormalizationStepIdentity(
          NormalizationStepId.unsafe(id),
          Vector("scope" -> "training-evidence")
        ).toOption.get
      )
    )

  def specification(
      sourceValue: ToyScientificSource = source(),
      designPolicy: String = "leave-one-run-out",
      frameId: String = "language-roi",
      framePositions: Vector[Int] = Vector(0, 2),
      estimandParameter: String = "balanced-accuracy",
      normalizationValue: NormalizationIdentity = NormalizationIdentity.none,
      boundariesValue: RequestedBoundaries = boundaries("estimate"),
      reverseDesignReferences: Boolean = false
  ): ScientificSpecification[
    ToyScientificSource,
    ToyEvidenceDesign,
    ToyEstimand,
    NoRendition.type
  ] =
    ScientificSpecification
      .requesting(sourceValue)(
        design(
          sourceValue,
          policy = designPolicy,
          reverseReferences = reverseDesignReferences
        ),
        frame(sourceValue, frameId, framePositions),
        ToyEstimand(estimandParameter),
        normalizationValue,
        boundariesValue
      )
      .toOption
      .get

class ScientificIdentitySuite extends munit.FunSuite:
  import ScientificPlanFixtures.*

  test("component identity is canonical over unordered fields and named source axes"):
    val value = source()
    val rebuilt = ScientificSourceIdentity(
      ScientificSourceKind.unsafe("observations"),
      Vector(
        ScientificSourceAxis(SampleAxisName, value.samples.identity),
        ScientificSourceAxis(NeuralAxisName, value.features.identity)
      ),
      Vector("owner" -> "fixture", "version" -> "observations-v1")
    ).toOption.get

    assertEquals(value.identity, rebuilt)
    assertEquals(value.identity.canonicalHex, rebuilt.canonicalHex)
    assertEquals(
      value.identity.axes.map(_.name.value),
      Vector("neural", "samples")
    )

  test("scientific plan identity is independent of declaration order"):
    val forward = specification()
    val reversed = specification(reverseDesignReferences = true)

    assertEquals(forward.identity, reversed.identity)
    assertEquals(forward.identity.canonicalHex, reversed.identity.canonicalHex)
    assertEquals(
      forward.identity.designAxes.map(_.name.value),
      Vector("neural", "samples")
    )

  test("every scientific component contributes to plan identity"):
    val baseline = specification()
    val changedSource = specification(sourceValue = source("observations-v2"))
    val changedDesign = specification(designPolicy = "group-k-fold")
    val changedFrame = specification(frameId = "motor-roi", framePositions = Vector(1, 3))
    val changedEstimand = specification(estimandParameter = "log-loss")
    val changedNormalization = specification(
      normalizationValue = normalization("fold-standardization")
    )
    val changedBoundaries = specification(
      boundariesValue = boundaries("estimate", "out-of-fold-predictions")
    )

    val alternatives = Vector(
      changedSource,
      changedDesign,
      changedFrame,
      changedEstimand,
      changedNormalization,
      changedBoundaries
    )
    assert(alternatives.forall(_.identity.fingerprint != baseline.identity.fingerprint))

  test("plan identity has a versioned cross-platform golden encoding"):
    val plan = specification().identity

    assertEquals(ScientificSourceIdentity.Protocol, "scalafim-mvpa-source/v1")
    assertEquals(DesignIdentity.Protocol, "scalafim-mvpa-design/v1")
    assertEquals(EstimandIdentity.Protocol, "scalafim-mvpa-estimand/v1")
    assertEquals(ScientificPlanIdentity.Protocol, "scalafim-mvpa-scientific-plan/v1")
    assertEquals(
      plan.fingerprint.value,
      "scalafim-mvpa-plan-v1-a40c2d12a4ecb0ce8295cb80e17cd65fc74648f6075e14106babc4cb9becb923"
    )

  test("malformed identity components fail closed"):
    val value = source()
    val duplicateSource = ScientificSourceIdentity(
      ScientificSourceKind.unsafe("observations"),
      Vector(
        ScientificSourceAxis(SampleAxisName, value.samples.identity),
        ScientificSourceAxis(SampleAxisName, value.features.identity)
      )
    )
    val duplicateBoundary =
      OutputBoundaryIdentity(OutputBoundaryId.unsafe("estimate")).toOption.get

    assert(
      ScientificSourceIdentity(
        ScientificSourceKind.unsafe("observations"),
        Vector.empty
      ).left.exists(_ == ScientificIdentityError.EmptySourceAxes)
    )
    assert(duplicateSource.left.exists:
      case ScientificIdentityError.DuplicateSourceAxis(name) => name == SampleAxisName
      case _                                                 => false)
    assert(
      RequestedBoundaries(Vector.empty).left.exists(
        _ == ScientificIdentityError.EmptyRequestedBoundaries
      )
    )
    assert(RequestedBoundaries(Vector(duplicateBoundary, duplicateBoundary)).left.exists:
      case ScientificIdentityError.DuplicateOutputBoundary(id) => id.value == "estimate"
      case _                                                   => false)

  test("specification rejects unknown, foreign, duplicate, and misdeclared axes"):
    val value = source()
    val validFrame = frame(value)
    val estimand = ToyEstimand("balanced-accuracy")
    val unknownName = ScientificAxisName.unsafe("subjects")
    val unknownDesign = new ToyEvidenceDesign(
      DesignIdentity(DesignKind.unsafe("validation")).toOption.get,
      Vector(DesignAxisReference(unknownName, value.samples.identity))
    )
    val foreignSamples = MvpaLawFixtures.sampleAxis(4, 999)
    val foreignDesign = new ToyEvidenceDesign(
      DesignIdentity(DesignKind.unsafe("validation")).toOption.get,
      Vector(DesignAxisReference(SampleAxisName, foreignSamples.identity))
    )
    val duplicateDesign = new ToyEvidenceDesign(
      DesignIdentity(DesignKind.unsafe("validation")).toOption.get,
      Vector(
        DesignAxisReference(SampleAxisName, value.samples.identity),
        DesignAxisReference(SampleAxisName, value.samples.identity)
      )
    )
    val misdeclaredIdentity = ScientificSourceIdentity(
      ScientificSourceKind.unsafe("observations"),
      Vector(
        ScientificSourceAxis(SampleAxisName, value.samples.identity),
        ScientificSourceAxis(NeuralAxisName, foreignSamples.identity)
      )
    ).toOption.get
    val misdeclared = new ToyScientificSource(
      value.samples,
      value.features,
      misdeclaredIdentity
    )

    assert(ScientificSpecification(value)(unknownDesign, validFrame, estimand).left.exists:
      case ScientificSpecificationError.UnknownDesignAxis(name) => name == unknownName
      case _                                                    => false)
    assert(ScientificSpecification(value)(foreignDesign, validFrame, estimand).left.exists:
      case ScientificSpecificationError.DesignAxisMismatch(name, _, _) =>
        name == SampleAxisName
      case _ => false)
    assert(
      ScientificPlanIdentity
        .fromSpecification(
          value.identity,
          unknownDesign.identity,
          unknownDesign.referencedAxes,
          validFrame.identity,
          estimand.identity,
          NormalizationIdentity.none,
          estimand.defaultBoundaries
        )
        .left
        .exists:
          case ScientificIdentityError.UnknownDesignAxis(name) => name == unknownName
          case _                                               => false
    )
    assert(
      ScientificPlanIdentity
        .fromSpecification(
          value.identity,
          foreignDesign.identity,
          foreignDesign.referencedAxes,
          validFrame.identity,
          estimand.identity,
          NormalizationIdentity.none,
          estimand.defaultBoundaries
        )
        .left
        .exists:
          case ScientificIdentityError.DesignAxisMismatch(name, _, _) =>
            name == SampleAxisName
          case _ => false
    )
    assert(ScientificSpecification(value)(duplicateDesign, validFrame, estimand).left.exists:
      case ScientificSpecificationError.Identity(
            ScientificIdentityError.DuplicateDesignAxis(name)
          ) =>
        name == SampleAxisName
      case _ => false)
    assert(
      ScientificSpecification(misdeclared)(
        design(misdeclared),
        frame(misdeclared),
        estimand
      ).left.exists:
        case ScientificSpecificationError.SourceAxisMismatch(name, _, _) =>
          name == NeuralAxisName
        case _ => false
    )
