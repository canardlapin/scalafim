package scalafim.image.io

import java.nio.file.{Path, Paths}

import image4s.nifti.NiftiAffinePolicy
import scalafim.image.world.{SpaceEvidence, SpaceResolver, WorldSpace, XformCode}

class NiftiSpaceEvidenceSuite extends munit.FunSuite:
  // nibabel-oblique.nii stores qform_code = 1 (scanner) and sform_code = 2 (aligned).
  private def fixture(name: String): Path =
    val resource = Option(getClass.getResource(s"/scalafim/image/io/$name")).getOrElse(fail(s"missing fixture $name"))
    Paths.get(resource.toURI)

  private val header = Nifti.readHeader(fixture("nibabel-oblique.nii")).fold(e => fail(e.message), identity)

  test("evidence follows the affine the policy selects, not the other xform"):
    assertEquals(NiftiSpaceEvidence.fromHeader(header, NiftiAffinePolicy.PreferSform), Right(SpaceEvidence(xform = Some(XformCode.AlignedAnatomical))))
    assertEquals(NiftiSpaceEvidence.fromHeader(header, NiftiAffinePolicy.PreferQform), Right(SpaceEvidence(xform = Some(XformCode.ScannerAnatomical))))

  test("the mirrored selection agrees with the affine image4s actually selected"):
    Vector(NiftiAffinePolicy.PreferSform, NiftiAffinePolicy.PreferQform).foreach: policy =>
      val decoded = Nifti
        .readVolume(fixture("nibabel-oblique.nii"), Nifti.ReadOptions.default.copy(affinePolicy = policy))
        .fold(e => fail(e.message), identity)
      val digest = NiftiSpaceEvidence.geometry(header, policy).fold(e => fail(e.message), identity)
      val bits = decoded.affineSelection.affine.rowMajor.take(12).map(v => java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(v + 0.0)))
      assert(digest.canonical.endsWith(bits.mkString(",")), s"$policy: ${digest.canonical}")

  test("without a native context the evidence resolves to the unresolved space"):
    val evidence = NiftiSpaceEvidence.fromHeader(header, NiftiAffinePolicy.PreferSform).fold(e => fail(e.message), identity)
    assertEquals(SpaceResolver.resolve(evidence), Right(WorldSpace.Unresolved))
