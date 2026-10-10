package scalafim.atlas

import scalafim.surface.CorticalHemisphere
import scalafim.surface.reference.*

/** The platform-independent contract of the standard fsLR 32k route: its lock, typed frame basis, frozen policy and
  * explicit override, refusals and digest-bound identity. Assembly from real assets is JVM-only (see the JVM
  * `StandardSurfaceRouteFilesSuite` and `Fslr32kRouteBudgetSuite`).
  */
class StandardSurfaceRoutesSuite extends munit.FunSuite:
  private val lock = Fslr32kFrom2009c

  test("the lock binds the point map to the admitted 6Asym -> 2009c composite"):
    assertEquals(lock.transform, TemplateFlowXfm.Mni6ToMni2009c)
    assert(lock.transform.agreesWithName)
    assert(lock.pointMapDirectory.endsWith(lock.transform.sha256))
    assertEquals(lock.requiredPaths.length, 5)
    assert(lock.requiredPaths.contains(s"${lock.pointMapDirectory}/manifest.json"))
    assertEquals(lock.sourceFrame.template.value, "MNI152NLin2009cAsym")
    assertEquals(lock.anatomyFrame.template.value, "MNI152NLin6Asym")
    assertEquals(lock.sourceFrame.release, lock.catalogRevision)
    assertEquals(lock.hemispheres.map(_.hemisphere), Vector(CorticalHemisphere.Left, CorticalHemisphere.Right))
    assertEquals(lock.lockFor(CorticalHemisphere.Right).midthickness.archivePath,
      "tpl-fsLR/tpl-fsLR_den-32k_hemi-R_midthickness.surf.gii")

  test("the anatomy frame is the publisher's methods tier: affine, 69-subject Conte69 average in MNI152NLin6Asym"):
    lock.anatomyBasis match
      case FrameBasis.PublisherMethods(doi, locator, registration, aggregate, quotation) =>
        assertEquals(doi, "10.1093/cercor/bhr291")
        assert(locator.contains("p. 2245"))
        assertEquals((registration, aggregate), (PublishedRegistration.Affine, PublishedAggregate.GroupAverage(69)))
        assert(quotation.contains("MNI152_T1_1mm.nii.gz"))
      case other => fail(s"expected publisher methods, got $other")
    assert(lock.anatomyBasis.display.contains("matches no single anatomy"))
    for h <- lock.hemispheres do
      assertEquals(h.declaration.frame, lock.anatomyFrame)
      assertEquals(h.declaration.basis, lock.anatomyBasis)
      assertEquals(h.declaration.asset, h.midthickness)

  test("the frozen policy is the reframe4s default that passed P1-P6; an override is explicit and reasoned"):
    assertEquals(StandardRoutePolicy.Frozen.inverse, InversePolicy.Default)
    assert(StandardRoutePolicy.Frozen.isFrozen)
    assert(StandardRoutePolicy.Frozen.label.contains("bd-01M3WCQD1MFW1WRTJP6C6A2ZFS"))
    val looser = InversePolicy.make(1e-6, 50).fold(e => fail(e.message), identity)
    assert(StandardRoutePolicy.overriding(looser, "  ").left.exists(_.isInstanceOf[StandardRouteRefusal.PolicyRefused]))
    assert(StandardRoutePolicy.overriding(InversePolicy.Default, "same as frozen")
      .left.exists(_.isInstanceOf[StandardRouteRefusal.PolicyRefused]))
    val overridden = StandardRoutePolicy.overriding(looser, "coarser tolerance for a preview").fold(r => fail(r.message), identity)
    assert(!overridden.isFrozen)
    assertEquals(overridden.qualification, PolicyQualification.Override("coarser tolerance for a preview"))
    assert(overridden.label.contains("not budget-qualified"))

  test("identity is the SHA-256 of its canonical text and changes with any line"):
    val a = StandardRouteIdentity.of(Vector("route=x", "policy=frozen"))
    val b = StandardRouteIdentity.of(Vector("route=x", "policy=frozen"))
    val c = StandardRouteIdentity.of(Vector("route=x", "policy=override"))
    assertEquals(a, b)
    assertNotEquals(a.sha256, c.sha256)
    assertEquals(a.sha256, AssetSha256.of("route=x\npolicy=frozen\n".getBytes("UTF-8")))
    assert(a.token.startsWith("scalafim-route:sha256:"))
    assertEquals(a.token.length, "scalafim-route:sha256:".length + 64)

  test("refusals say what failed and never imply a download"):
    val missing = StandardRouteRefusal.AssetsMissing(lock.requiredPaths, Vector("/cache"))
    assert(missing.message.contains("nothing is downloaded"))
    assert(missing.message.contains(lock.requiredPaths.head))
    val refused = StandardRouteRefusal.AssetRefused("x.surf.gii", ReferenceError.DigestMismatch("x.surf.gii", "a" * 64, "b" * 64))
    assert(refused.message.contains("x.surf.gii"))
    val gate = StandardRouteRefusal.PlacementGate(CorticalHemisphere.Left, 3,
      InversePlacementSummary(10, Map(InverseFailureKind.NotQueried -> 3), Some(1e-9), Some(4)))
    assert(gate.message.contains("3 cortical vertices"))
