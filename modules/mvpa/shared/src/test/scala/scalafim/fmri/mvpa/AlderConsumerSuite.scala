package scalafim.fmri.mvpa

import alder.application.*
import alder.data.{Split as AlderSplit, *}
import alder.kernel.{Seed as AlderSeed, *}
import alder.metrics.*
import alder.preprocess.*
import cats.Id
import cats.data.EitherT
import resample4s.core.{Seed as ResampleSeed, *}
import resample4s.designs.KFold as ResampleKFold

/** External-consumer rehearsal for the predictive lifecycle owned by Alder.
  *
  * This suite deliberately lives outside `package alder`: ScalaFIM must be able to use only Alder's public protocol. It
  * is not an alternative predictive lifecycle implementation.
  */
final class AlderConsumerSuite extends munit.FunSuite:
  private final case class Point(x: Double, y: Double) derives Coordinates, Schema

  private type Observation = Example[Point, Double, String]

  private val component =
    ComponentDescriptor(
      ComponentId("scalafim.mvpa.first-standardized-coordinate"),
      ComponentVersion("1"),
      AuditValue.record(),
      BackendFingerprint("scala-reference", "1", AuditValue.record())
    )

  private final class FirstCoordinateLearner
      extends Learner[
        Id,
        Dense[Standardized[Point]],
        Double,
        String,
        Double
      ]:
    type FitError = Nothing
    type RunError = Nothing
    type Model = Pipe[Dense[Standardized[Point]], Nothing, Double]

    def fit[U <: Use.Fit](
        data: NonEmptyData[
          U,
          Example[Dense[Standardized[Point]], Double, String]
        ]
    )(using context: FitContext): FitResult[Id, FitError, Trained[Model]] =
      val model: Model = Pipe.total(value => value(0))
      EitherT.right(context.complete(model, data, component))

  private def source: Data[Use.Unsplit, Observation] =
    InMemoryData.unsplit(
      Vector.tabulate(8) { index =>
        val value = index.toDouble + 1.0
        Example(Point(value, value * value), value, s"sample-$index")
      },
      new DataFingerprint(
        FingerprintPolicy.ContentDigest("fixture-v1"),
        "0011223344556677"
      )
    )

  private def validationRows(value: Long): Rows =
    Rows(value) match
      case Right(rows) => rows
      case Left(error) => fail(s"invalid validation-row fixture: $error")

  test("clean consumer reaches exact-plan binding and the audited Alder lifecycle") {
    val data = source
    val indexSpace = IndexSpace.of(data.size.toInt) match
      case Right(value) => value
      case Left(error)  => fail(s"invalid index-space fixture: $error")
    val compiled =
      ResampleKFold(4).compile(indexSpace, ResampleSeed.fromLong(43L)) match
        case Right(value) => value
        case Left(error)  => fail(s"unexpected exact-plan failure: $error")
    val population =
      Resample4sResampler.populationFingerprint(data.fingerprint) match
        case Right(value) => value
        case Left(error)  => fail(s"unexpected population mapping: $error")
    val exact =
      Resample4sResampler.fromCompiled[Observation](compiled, population)(using
        DigestAlgorithm.fnv1a64
      ) match
        case Right(value) => value
        case Left(error)  => fail(s"unexpected exact-plan binding: $error")

    assertEquals(exact.fingerprint.policy, FingerprintPolicy.ContentDigest("fnv1a64/v1"))
    assert(exact.fingerprint.digest.nonEmpty)

    val split =
      AlderSplit.validation(
        data,
        ValidationSpec(validationRows(2L)),
        AlderSeed(43L)
      ) match
        case Right(value) => value
        case Left(error)  => fail(s"unexpected validation split: $error")
    val scaler =
      StandardScaler.sync[Point](ZeroVariance.Reject) match
        case Right(value) => value
        case Left(error)  => fail(s"unexpected scaler construction: $error")
    val workflow = scaler.learnWith(new FirstCoordinateLearner)
    given FitContext =
      FitContext.root(
        AlderSeed(43L),
        PlanFingerprint.content("sha256", "scalafim-alder-consumer-v1"),
        Schema[Point].fingerprint,
        NumericMode.Deterministic
      )
    val trained = workflow.fit(split.train).value match
      case Right(value) => value
      case Left(error)  => fail(s"unexpected workflow fit: $error")
    val sources =
      EvaluationSources.validation(split.train, split.validation.data) match
        case Right(value) => value
        case Left(error)  => fail(s"unexpected evaluation sources: $error")
    val evaluated =
      Evaluation.validated(
        workflow,
        trained,
        sources,
        RegressionMetrics.rmse[String]
      ) match
        case Right(value) => value
        case Left(error)  => fail(s"unexpected scored validation: $error")
    val selection = evaluated.select(SingleCandidate)
    val promoted =
      Refit.after(selection).from(evaluated.evaluation.allObserved) match
        case Right(value) => value
        case Left(error)  => fail(s"unexpected selected refit: $error")

    assertEquals(split.train.size, 6L)
    assertEquals(split.validation.size, 2L)
    assertEquals(evaluated.evaluation.scored.size, 2L)
    assert(evaluated.evaluation.score.value.isFinite)
    assertEquals(selection.evaluation, evaluated.evaluation.receipt.id)
    assertEquals(promoted.size, 8L)
    assertEquals(
      selection.sources.map(_.role),
      Vector(ObservedSourceRole.Train, ObservedSourceRole.Validation)
    )
    assertEquals(trained.audit.children.length, 2)
  }
