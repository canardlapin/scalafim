package scalafim.locus

/** The public ScalaFIM aliases expose overlap from the pinned region owner. */
class RegionOverlapConsumerSuite extends munit.FunSuite:
  test("regions on a restored neuroimaging domain use upstream overlap metrics"):
    val leftOwner = DomainFactory.unsafeRestore(SpaceKey.unsafe("overlap:voxels"), 8).space
    val rightOwner = DomainFactory.unsafeRestore(SpaceKey.unsafe("overlap:voxels"), 8).space
    val left = Region.fromOrdinals(leftOwner, Vector(0, 2, 4, 6)).toOption.get
    val right = Region.fromOrdinals(rightOwner, Vector(1, 2, 6)).toOption.get
    assertEqualsDouble(left.diceChecked(right).toOption.get, 4.0 / 7.0, 1e-15)
    assertEqualsDouble(left.jaccardChecked(right).toOption.get, 2.0 / 5.0, 1e-15)

  test("empty overlap cannot bypass distinct neuroimaging domain ownership"):
    val voxels = DomainFactory.unsafeEphemeral("voxels", 8).value
    val vertices = DomainFactory.unsafeEphemeral("vertices", 8).value
    assert(Region.empty(voxels).diceChecked(Region.empty(vertices)).isLeft)
    assert(Region.empty(voxels).jaccardChecked(Region.empty(vertices)).isLeft)
    assertEqualsDouble(Region.empty(voxels).dice(Region.empty(voxels)), 1.0, 0.0)
