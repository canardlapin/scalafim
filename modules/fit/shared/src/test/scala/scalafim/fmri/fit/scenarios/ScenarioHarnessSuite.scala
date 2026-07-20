package scalafim.fmri.fit.scenarios

import scalafim.fmri.fit.GaleTestSyntax.*

class ScenarioHarnessSuite extends munit.FunSuite:

  test("clean observations produce a CI-passable scenario"):
    val result =
      ScenarioHarness.result(
        "scenario.clean",
        Vector(ScenarioHarness.fact("all checks", passed = true, detail = "ok"))
      )

    assertEquals(result.status, ScenarioStatus.Pass)
    assert(result.ciPass)
    assert(result.ciPass(ScenarioPolicy.PassOnly))

  test("failed observations force ScenarioStatus.Fail even without caveats"):
    val result =
      ScenarioHarness.result(
        "scenario.failed-observation",
        Vector(ScenarioHarness.fact("shape", passed = false, detail = "wrong columns"))
      )

    assertEquals(result.status, ScenarioStatus.Fail)
    assert(!result.ciPass)
    assert(!result.ciPass(ScenarioPolicy.allowCaveats("fit.public-api-gap")))

  test("non-blocking caveats require explicit policy to pass CI"):
    val caveat =
      ScenarioCaveat(
        id = "fit.public-api-gap",
        kind = CaveatKind.PublicApiGap,
        severity = CaveatSeverity.Actionable,
        owner = "fit",
        followUp = Some("add public censor-mask API"),
        detail = "scenario uses a row-subset seam until censor intent is first-class"
      )
    val result =
      ScenarioHarness.result(
        "scenario.with-caveat",
        Vector(ScenarioHarness.fact("numeric gates", passed = true, detail = "all pass")),
        caveats = Vector(caveat)
      )

    assertEquals(result.status, ScenarioStatus.PassWithCaveats)
    assert(!result.ciPass)
    assert(!result.ciPass(ScenarioPolicy.PassOnly))
    assert(result.ciPass(ScenarioPolicy.allowCaveats("fit.public-api-gap")))
    assert(!result.ciPass(ScenarioPolicy.allowCaveats("fit.other-gap")))
    assert(result.render.contains("caveat=fit.public-api-gap"))

  test("blocking caveats force ScenarioStatus.Fail"):
    val result =
      ScenarioHarness.result(
        "scenario.blocking-caveat",
        Vector(ScenarioHarness.fact("numeric gates", passed = true, detail = "all pass")),
        caveats = Vector(
          ScenarioCaveat(
            id = "fit.stale-fixture",
            kind = CaveatKind.FixtureFreshness,
            severity = CaveatSeverity.Blocking,
            owner = "fit",
            followUp = Some("regenerate fixture"),
            detail = "reference fixture is stale"
          )
        )
      )

    assertEquals(result.status, ScenarioStatus.Fail)
    assert(!result.ciPass)
    assert(!result.ciPass(ScenarioPolicy.allowCaveats("fit.stale-fixture")))

  test("policies cannot declare failures acceptable"):
    intercept[IllegalArgumentException]:
      ScenarioPolicy(Set(ScenarioStatus.Fail), Set.empty)
