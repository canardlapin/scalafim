package scalafim.fmri.laws

import org.scalacheck.Test
import org.scalacheck.rng.Seed

enum LawRunProfile(
    val label: String,
    val successfulTests: Int,
    val maximumSize: Int
):
  case PullRequest extends LawRunProfile("pull-request", successfulTests = 48, maximumSize = 28)
  case Calibration extends LawRunProfile("calibration", successfulTests = 300, maximumSize = 96)

object LawRunProfile:
  val current: LawRunProfile =
    sys.env.get("SCALAFIM_LAW_PROFILE") match
      case None | Some("") | Some("pr") | Some("pull-request") => LawRunProfile.PullRequest
      case Some("calibration")                                 => LawRunProfile.Calibration
      case Some(other)                                         =>
        throw new IllegalArgumentException(
          s"SCALAFIM_LAW_PROFILE must be 'pull-request' or 'calibration', got '$other'"
        )

  val initialSeed: String =
    val configured =
      sys.env
        .get("SCALAFIM_LAW_SEED")
        .orElse(
          sys.env.get("SCALAFIM_LAW_SEED_LONG").map { raw =>
            val value = raw.toLongOption.getOrElse(
              throw new IllegalArgumentException("SCALAFIM_LAW_SEED_LONG must be a signed 64-bit integer")
            )
            Seed(value).toBase64
          }
        )
        .getOrElse(Seed(0x5ca1af1L).toBase64)
    require(
      Seed.fromBase64(configured).isSuccess,
      "SCALAFIM_LAW_SEED must be a ScalaCheck base64 seed"
    )
    configured

/** Shared deterministic execution policy for the generated law court.
  *
  * MUnit ScalaCheck includes the initial seed and the shrunken arguments in a failure. Re-exporting that seed through
  * `SCALAFIM_LAW_SEED` reproduces the same counterexample on either runtime.
  */
trait GeneratedLawSuite extends munit.ScalaCheckSuite:
  private val profile = LawRunProfile.current

  override def scalaCheckInitialSeed: String =
    LawRunProfile.initialSeed

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(profile.successfulTests)
      .withMinSize(0)
      .withMaxSize(profile.maximumSize)
      .withWorkers(1)
