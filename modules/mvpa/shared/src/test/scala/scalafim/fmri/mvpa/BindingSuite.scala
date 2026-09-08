package scalafim.fmri.mvpa

private[mvpa] final class LabeledToySource(
    val samplesValue: AxisRef[SampleId],
    featuresValue: AxisRef[FeatureId],
    identityValue: ScientificSourceIdentity,
    val target: Column[samplesValue.Id, String]
) extends ToyScientificSource(samplesValue, featuresValue, identityValue)

private[mvpa] enum LabelCountRejection:
  case OneClass(observed: String)

private[mvpa] enum LabelCountFailure:
  case Kernel(detail: String)

private[mvpa] final case class PreparedLabelCount(
    sampleCount: Int,
    classCount: Int
)

private[mvpa] case object LabelCountEstimand extends Estimand[LabeledToySource, ToyEvidenceDesign]:
  override type Result = Int
  override type Rejection = LabelCountRejection
  override type Failure = LabelCountFailure

  override val identity: EstimandIdentity =
    EstimandIdentity(EstimandKind.unsafe("label-count")).toOption.get

  override val defaultBoundaries: RequestedBoundaries =
    ScientificPlanFixtures.boundaries("class-count")

  override def rejectionMessage(value: LabelCountRejection): String =
    value match
      case LabelCountRejection.OneClass(observed) =>
        s"at least two classes are required; observed only '$observed'"

  override def failureMessage(value: LabelCountFailure): String =
    value match
      case LabelCountFailure.Kernel(detail) => detail

private[mvpa] object LabelCountCompiler:
  given [Rendition]: Compile[
    LabeledToySource,
    ToyEvidenceDesign,
    LabelCountEstimand.type,
    Rendition
  ] with
    override type Prepared = PreparedLabelCount

    override def prepare(
        specification: ScientificSpecification[
          LabeledToySource,
          ToyEvidenceDesign,
          LabelCountEstimand.type,
          Rendition
        ]
    ): Either[specification.Rejection, PreparedLabelCount] =
      val labels = specification.source.target.toVector
      val classes = labels.distinct
      classes match
        case Vector(single) => Left(LabelCountRejection.OneClass(single))
        case _              => Right(PreparedLabelCount(labels.length, classes.length))

class BindingSuite extends munit.FunSuite:
  import LabelCountCompiler.given
  import ScientificPlanFixtures.*

  private def labeledSource(labels: Vector[String]): LabeledToySource =
    val raw = source()
    val target = Column(raw.samples, labels).toOption.get
    new LabeledToySource(raw.samples, raw.features, raw.identity, target)

  test("binding admits compatible capabilities into an always-valid typed plan"):
    val sourceValue = labeledSource(Vector("face", "place", "face", "place"))
    val specification = ScientificSpecification(sourceValue)(
      design(sourceValue),
      frame(sourceValue),
      LabelCountEstimand
    ).toOption.get
    val bound = BoundScientificPlan.bind(specification).toOption.get
    val facadeBound = Mvpa.bind(specification).toOption.get

    assertEquals(bound.identity, specification.identity)
    assertEquals(bound.prepared, PreparedLabelCount(sampleCount = 4, classCount = 2))
    assertEquals(facadeBound.identity, bound.identity)
    assertEquals(facadeBound.prepared, bound.prepared)
    val result: bound.Result = 2
    assertEquals(result, 2)

  test("estimand capability rejection remains structured and renders actionable context"):
    val sourceValue = labeledSource(Vector.fill(4)("face"))
    val specification = ScientificSpecification(sourceValue)(
      design(sourceValue),
      frame(sourceValue),
      LabelCountEstimand
    ).toOption.get

    val rejection = Mvpa.bind(specification).swap.toOption.get
    assert(rejection match
      case BindError.EstimandRejected(identity, LabelCountRejection.OneClass(label), explanation) =>
        identity == LabelCountEstimand.identity &&
        label == "face" &&
        explanation == "at least two classes are required; observed only 'face'")
    assertEquals(
      rejection.message,
      "estimand 'label-count' rejected the scientific specification: at least two classes are required; observed only 'face'"
    )

  test("target ownership and estimand-required source capabilities are static"):
    val foreignTargetErrors = compileErrors("""
      import scalafim.fmri.mvpa.*

      def illegal[K1, K2](
          owner: AxisRef[K1],
          foreign: AxisRef[K2],
          target: Column[owner.Id, String]
      ): Column[foreign.Id, String] = target
    """)
    val missingCapabilityErrors = compileErrors("""
      import scalafim.fmri.mvpa.*

      trait PlainSource extends ScientificSource
      trait LabeledSource extends ScientificSource
      trait Validation extends EvidenceDesign
      trait NeedsLabels extends Estimand[LabeledSource, Validation]

      def illegal(
          source: PlainSource,
          design: Validation,
          frame: MeasurementFrame[source.Neural, source.NeuralKey, NoRendition.type],
          estimand: NeedsLabels
      ) = ScientificSpecification(source)(design, frame, estimand)
    """)

    assert(foreignTargetErrors.nonEmpty, clue(foreignTargetErrors))
    assert(missingCapabilityErrors.nonEmpty, clue(missingCapabilityErrors))

  test("a bound plan has no public constructor or unvalidated factory"):
    val constructorErrors = compileErrors("""
      import scalafim.fmri.mvpa.*

      def forge[
          S <: ScientificSource,
          D <: EvidenceDesign,
          E <: Estimand[S, D],
          R,
          P
      ](
          specification: ScientificSpecification[S, D, E, R],
          prepared: P
      ) = new BoundScientificPlan(specification, prepared)
    """)
    val unvalidatedFactoryErrors = compileErrors("""
      scalafim.fmri.mvpa.BoundScientificPlan.admitted
    """)

    assert(constructorErrors.nonEmpty)
    assert(unvalidatedFactoryErrors.nonEmpty)
