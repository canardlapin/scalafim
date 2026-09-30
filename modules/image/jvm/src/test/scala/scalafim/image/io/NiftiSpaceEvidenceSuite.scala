package scalafim.image.io

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path, Paths}

import image4s.geometry.{Affine, D3, Frame}
import image4s.nifti.{NiftiAffinePolicy, NiftiAffineSource}
import scalafim.image.SampleSpaces
import scalafim.image.world.{
  DatasetNamespace,
  SpaceError,
  SpaceEvidence,
  SpaceResolver,
  Spaces,
  SubjectId,
  WorldSpace,
  XformCode
}

class NiftiSpaceEvidenceSuite extends munit.FunSuite:
  // nibabel-oblique.nii stores qform_code = 1 (scanner) and sform_code = 2 (aligned).
  private def fixture(name: String): Path =
    val resource = Option(getClass.getResource(s"/scalafim/image/io/$name")).getOrElse(fail(s"missing fixture $name"))
    Paths.get(resource.toURI)

  private val header = Nifti.readHeader(fixture("nibabel-oblique.nii")).fold(e => fail(e.message), identity)

  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  /** A minimal little-endian NIfTI-1 file whose qform (scaling 2,3,4, offset 10,20,30), sform (scaling 5,6,7, offset
    * -1,-2,-3) and pixdim fallback (scaling 2,3,4) are all distinguishable, with the given xform codes.
    */
  private def crafted(qformCode: Int, sformCode: Int, volumes: Int = 1): Path =
    val voxels = 2 * 2 * 2 * volumes
    val buffer = ByteBuffer.allocate(352 + 4 * voxels).order(ByteOrder.LITTLE_ENDIAN)
    buffer.putInt(0, 348)
    val dims = Vector(if volumes > 1 then 4 else 3, 2, 2, 2, volumes, 1, 1, 1)
    dims.zipWithIndex.foreach((value, index) => buffer.putShort(40 + 2 * index, value.toShort))
    buffer.putShort(70, 16.toShort) // float32
    buffer.putShort(72, 32.toShort)
    Vector(1.0f, 2.0f, 3.0f, 4.0f, 1.0f, 1.0f, 1.0f, 1.0f).zipWithIndex.foreach((value, index) => buffer.putFloat(76 + 4 * index, value))
    buffer.putFloat(108, 352.0f) // vox_offset
    buffer.putFloat(112, 1.0f) // scl_slope
    buffer.put(123, (2 | 8).toByte) // millimetres, seconds
    buffer.putShort(252, qformCode.toShort)
    buffer.putShort(254, sformCode.toShort)
    Vector(10.0f, 20.0f, 30.0f).zipWithIndex.foreach((value, index) => buffer.putFloat(268 + 4 * index, value))
    Vector(5.0f, 0.0f, 0.0f, -1.0f, 0.0f, 6.0f, 0.0f, -2.0f, 0.0f, 0.0f, 7.0f, -3.0f).zipWithIndex.foreach((value, index) =>
      buffer.putFloat(280 + 4 * index, value)
    )
    "n+1\u0000".zipWithIndex.foreach((c, index) => buffer.put(344 + index, c.toByte))
    (0 until voxels).foreach(index => buffer.putFloat(352 + 4 * index, index.toFloat))
    val path = Files.createTempFile("scalafim-evidence", ".nii")
    path.toFile.deleteOnExit()
    Files.write(path, buffer.array())
    path

  private val policies = Vector(
    NiftiAffinePolicy.PreferSform,
    NiftiAffinePolicy.PreferQform,
    NiftiAffinePolicy.RequireAgreement(1.0e9),
    NiftiAffinePolicy.UseExplicit(Affine.identity[D3])
  )

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

  test("the mirrored selection matches image4s-nifti's selectAffine on every qform/sform/scaling case"):
    val cases = Vector((0, 0), (1, 0), (0, 4), (1, 4))
    cases.foreach: (qformCode, sformCode) =>
      val path = crafted(qformCode, sformCode)
      val stored = ok(Nifti.readHeader(path))
      policies.foreach: policy =>
        val decoded = ok(Nifti.readVolume(path, Nifti.ReadOptions.default.copy(affinePolicy = policy)))
        val (source, affine) = NiftiSpaceEvidence.selectedSource(stored.native, policy)
        val clue = s"q=$qformCode s=$sformCode $policy"
        assertEquals(source, decoded.affineSelection.source, clue)
        assertEquals(affine.rowMajor, decoded.affineSelection.affine.rowMajor, clue)
        assertEquals(NiftiSpaceEvidence.fromHeader(stored, policy), NiftiSpaceEvidence.fromSelection(decoded.header, decoded.affineSelection.source), clue)
    // The cases reach every selectable source.
    val reached = cases.flatMap((q, s) => policies.map(p => NiftiSpaceEvidence.selectedSource(ok(Nifti.readHeader(crafted(q, s))).native, p)._1)).toSet
    assertEquals(reached, Set(NiftiAffineSource.Sform, NiftiAffineSource.Qform, NiftiAffineSource.Fallback, NiftiAffineSource.Explicit))

  test("without a native context the evidence resolves to the unresolved space"):
    val evidence = NiftiSpaceEvidence.fromHeader(header, NiftiAffinePolicy.PreferSform).fold(e => fail(e.message), identity)
    assertEquals(SpaceResolver.resolve(evidence), Right(WorldSpace.Unresolved))

  test("readVolumeIn places a template file in its template world, keeping geometry and data"):
    val path = crafted(qformCode = 1, sformCode = 4)
    val legacy = ok(Nifti.readVolume(path))
    val placed = ok(Nifti.readVolumeIn(path, SpaceEvidence(bidsSpace = Some("MNI152NLin2009cAsym"))))
    assertEquals(SampleSpaces.worldOf(legacy.image.space), Right(WorldSpace.Unresolved))
    assertEquals(SampleSpaces.worldOf(placed.image.space), Right(ok(WorldSpace.template("MNI152NLin2009cAsym"))))
    assert(Frame.alignOwners[D3, Frame[D3], Frame[D3]](placed.image.grid.frame, Spaces.MNI152NLin2009cAsym).isRight)
    assertEquals(placed.image.grid.indexToFrame.rowMajor, legacy.image.grid.indexToFrame.rowMajor)
    assertEquals(placed.image.grid.shape, legacy.image.grid.shape)
    assertEqualsDouble(placed.image(1, 1, 1), legacy.image(1, 1, 1), 0.0)

  test("readVolumeIn refuses unresolved, ambiguous and contradictory evidence"):
    val path = crafted(qformCode = 1, sformCode = 4)
    def spaceError(result: Either[NiftiImageReadError, ?]): SpaceError =
      result match
        case Left(NiftiImageReadError.Space(error)) => error
        case other                                  => fail(s"expected a world-space failure, got $other")
    // The selected sform says MNI_152 without saying which MNI152.
    assert(spaceError(Nifti.readVolumeIn(path, SpaceEvidence())).isInstanceOf[SpaceError.AmbiguousTemplate])
    // Under PreferQform the file is scanner-anatomical; with no native context it identifies no world.
    val qform = Nifti.ReadOptions.default.copy(affinePolicy = NiftiAffinePolicy.PreferQform)
    assert(spaceError(Nifti.readVolumeIn(path, SpaceEvidence(), qform)).isInstanceOf[SpaceError.NoWorldSpace])
    // A template label contradicts a scanner-anatomical xform, and a supplied code must match the header's.
    assert(spaceError(Nifti.readVolumeIn(path, SpaceEvidence(bidsSpace = Some("MNI152NLin2009cAsym")), qform)).isInstanceOf[SpaceError.ConflictingEvidence])
    assert(spaceError(Nifti.readVolumeIn(path, SpaceEvidence(xform = Some(XformCode.ScannerAnatomical)))).isInstanceOf[SpaceError.ConflictingEvidence])

  test("an unknown header code is no evidence, so a supplied code is admitted"):
    val path = crafted(qformCode = 0, sformCode = 0)
    val placed = ok(Nifti.readVolumeIn(path, SpaceEvidence(xform = Some(XformCode.Mni152), bidsSpace = Some("MNI152NLin2009cAsym"))))
    assertEquals(SampleSpaces.worldOf(placed.image.space), Right(ok(WorldSpace.template("MNI152NLin2009cAsym"))))

  test("two subjects' native volumes get different worlds and no longer align"):
    val path = crafted(qformCode = 1, sformCode = 0)
    val stored = ok(Nifti.readHeader(path))
    def subject(id: String) =
      ok(NiftiSpaceEvidence.nativeContext(stored, NiftiAffinePolicy.PreferSform, ok(DatasetNamespace("ds")), ok(SubjectId(id)), None, Map("suffix" -> "T1w")))
    val first = ok(Nifti.readVolumeIn(path, SpaceEvidence(bidsSpace = Some("T1w"), native = Some(subject("sub-01")))))
    val again = ok(Nifti.readVolumeIn(path, SpaceEvidence(native = Some(subject("sub-01")))))
    val second = ok(Nifti.readVolumeIn(path, SpaceEvidence(native = Some(subject("sub-02")))))
    SampleSpaces.worldOf(first.image.space) match
      case Right(WorldSpace.SubjectNative(_, sub, _, _)) => assertEquals(sub.value, "sub-01")
      case other                                         => fail(s"expected a subject-native world, got $other")
    assert(Frame.alignOwners[D3, Frame[D3], Frame[D3]](first.image.grid.frame, again.image.grid.frame).isRight, "one subject, one world")
    assert(Frame.alignOwners[D3, Frame[D3], Frame[D3]](first.image.grid.frame, second.image.grid.frame).isLeft, "two subjects, two worlds")
    // The legacy read still puts both in the shared unresolved world.
    val legacy = ok(Nifti.readVolume(path))
    assert(Frame.alignOwners[D3, Frame[D3], Frame[D3]](legacy.image.grid.frame, ok(Nifti.readVolume(path)).image.grid.frame).isRight)

  test("readSeriesIn places a 4D series in its world, keeping its time axis"):
    val path = crafted(qformCode = 1, sformCode = 4, volumes = 3)
    val placed = ok(Nifti.readSeriesIn(path, SpaceEvidence(bidsSpace = Some("MNI152NLin6Asym"))))
    assertEquals(SampleSpaces.worldOf(placed.image.space), Right(ok(WorldSpace.template("MNI152NLin6Asym"))))
    assertEquals(placed.image.nVolumes, 3)
    assertEqualsDouble(placed.image(1, 0, 0, 2), ok(Nifti.readSeries(path)).image(1, 0, 0, 2), 0.0)
