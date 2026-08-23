package scalafim.image

import image4s.locus.GridDomainLayout
import locus4s.DomainRegistry

class VolumeDomainMigrationSuite extends munit.FunSuite:
  private val asymmetricSpace =
    VolumeSpace(NeuroSpace(Vector(2, 3, 5)))

  private val legacyToCanonicalTable =
    Vector(
      0, 15, 5, 20, 10, 25,
      1, 16, 6, 21, 11, 26,
      2, 17, 7, 22, 12, 27,
      3, 18, 8, 23, 13, 28,
      4, 19, 9, 24, 14, 29
    )

  test("2 x 3 x 5 court fixes both coordinate-to-ordinal conventions"):
    val bridge =
      VolumeOrdinalBridge
        .register(asymmetricSpace, DomainRegistry.empty)
        .toOption
        .get
        .value

    assertEquals(
      bridge.canonicalLayout,
      GridDomainLayout.RowMajorLastAxisFastestV1
    )
    assertEquals(
      Vector.tabulate(asymmetricSpace.nVoxels): legacy =>
        bridge.canonicalPoint(legacy).toOption.get.ordinal,
      legacyToCanonicalTable
    )

    var z = 0
    while z < 5 do
      var y = 0
      while y < 3 do
        var x = 0
        while x < 2 do
          val coordinate = VoxelCoord(x, y, z)
          val legacy = x + 2 * (y + 3 * z)
          val canonical = ((x * 3) + y) * 5 + z
          val point = bridge.canonicalPointAt(coordinate).toOption.get
          assertEquals(point.ordinal, canonical)
          assertEquals(
            bridge.canonicalPoint(legacy).toOption.get,
            point
          )
          assertEquals(bridge.coordinateOf(point), coordinate)
          assertEquals(bridge.legacyOrdinal(point), legacy)
          x += 1
        y += 1
      z += 1

  test("forward and inverse maps form a certified bijection"):
    val bridge =
      VolumeOrdinalBridge
        .register(asymmetricSpace, DomainRegistry.empty)
        .toOption
        .get
        .value

    bridge.legacySpace.foreachIndex: legacy =>
      val canonical = bridge.toCanonical(legacy)
      assertEquals(bridge.toLegacy(canonical), legacy)
    bridge.canonicalSpace.foreachIndex: canonical =>
      val legacy = bridge.toLegacy(canonical)
      assertEquals(bridge.toCanonical(legacy), canonical)

  test("versioned legacy records convert and canonical records round trip"):
    val bridge =
      VolumeOrdinalBridge
        .register(asymmetricSpace, DomainRegistry.empty)
        .toOption
        .get
        .value
    val legacy =
      VolumeOrdinalRecord(
        bridge.gridDomainRecord.grid,
        VolumeOrdinalLayout.ScalaFimFirstAxisFastestV1,
        Vector(0, 1, 6, 29)
      )
    val canonical = bridge.canonicalRecord(Vector(0, 15, 1, 29))

    assertNotEquals(legacy, canonical)
    assertEquals(
      bridge.canonicalOrdinals(legacy),
      Right(Vector(0, 15, 1, 29))
    )
    assertEquals(
      bridge.canonicalOrdinals(canonical),
      Right(Vector(0, 15, 1, 29))
    )

  test("regions and ordered selections transport without changing voxels"):
    val domain = VolumeDomain.canonical(asymmetricSpace).value
    val legacyRegion =
      VoxelRegion
        .make(asymmetricSpace, Array(0, 1, 6, 29))
        .toOption
        .get
    val legacySelection =
      VoxelSelection
        .make(asymmetricSpace, Array(29, 1, 6, 0))
        .toOption
        .get
    val region = domain.region(legacyRegion).toOption.get
    val selection = domain.selection(legacySelection).toOption.get

    assertEquals(
      region.ordinalsInDomainOrder.toVector,
      Vector(0, 1, 15, 29)
    )
    assertEquals(
      selection.ordinals.toVector,
      Vector(29, 15, 1, 0)
    )
    assertEquals(domain.voxelRegion(region), Right(legacyRegion))
    assertEquals(
      domain.voxelSelection(selection),
      Right(legacySelection)
    )

    val restoredRegion =
      domain.restoreRegion(domain.recordRegion(region)).toOption.get
    val restoredSelection =
      domain
        .restoreSelection(domain.recordSelection(selection))
        .toOption
        .get
    assertEquals(restoredRegion, region)
    assertEquals(restoredSelection, selection)

  test("unknown layouts and out-of-range ordinals fail closed"):
    val bridge =
      VolumeOrdinalBridge
        .register(asymmetricSpace, DomainRegistry.empty)
        .toOption
        .get
        .value
    val unknown =
      VolumeOrdinalRecord(
        bridge.gridDomainRecord.grid,
        VolumeOrdinalLayout.Unrecognized("producer-private-order/v9"),
        Vector(0)
      )
    val invalidLegacy =
      VolumeOrdinalRecord(
        bridge.gridDomainRecord.grid,
        VolumeOrdinalLayout.ScalaFimFirstAxisFastestV1,
        Vector(30)
      )

    assert(
      bridge.canonicalOrdinals(unknown).left.exists:
        case VolumeOrdinalBridgeError.UnsupportedLayout(_) => true
        case _ => false
    )
    assert(
      bridge.canonicalOrdinals(invalidLegacy).left.exists:
        case VolumeOrdinalBridgeError.InvalidLegacyOrdinal(30, 30) => true
        case _ => false
    )

  test("exact grid record prevents same-sized foreign-volume restoration"):
    val first =
      VolumeOrdinalBridge
        .register(asymmetricSpace, DomainRegistry.empty)
        .toOption
        .get
        .value
    val translated =
      VolumeSpace(
        NeuroSpace(
          Vector(2, 3, 5),
          trans = Some(
            DMat.fromRows(
              Vector(
                Vector(1.0, 0.0, 0.0, 7.0),
                Vector(0.0, 1.0, 0.0, 0.0),
                Vector(0.0, 0.0, 1.0, 0.0),
                Vector(0.0, 0.0, 0.0, 1.0)
              )
            )
          )
        )
      )
    val foreign =
      VolumeOrdinalBridge
        .register(translated, DomainRegistry.empty)
        .toOption
        .get
        .value
    val record = foreign.canonicalRecord(Vector(0, 1))

    assertNotEquals(
      first.gridDomainRecord.domain.key,
      foreign.gridDomainRecord.domain.key
    )
    assert(
      first.canonicalOrdinals(record).left.exists:
        case VolumeOrdinalBridgeError.GridRecordMismatch(_, _) => true
        case _ => false
    )

  test("caller-owned registry canonicalizes equal exact grids"):
    val first =
      VolumeOrdinalBridge
        .register(asymmetricSpace, DomainRegistry.empty)
        .toOption
        .get
    val independentlyBuilt =
      VolumeSpace(NeuroSpace(Vector(2, 3, 5)))
    val restored =
      VolumeOrdinalBridge
        .register(independentlyBuilt, first.registry)
        .toOption
        .get
    val isolated =
      VolumeOrdinalBridge
        .register(independentlyBuilt, DomainRegistry.empty)
        .toOption
        .get

    assert(
      first.value.canonicalSpace
        .sameRuntimeOwnerAs(restored.value.canonicalSpace)
    )
    assert(
      !first.value.canonicalSpace
        .sameRuntimeOwnerAs(isolated.value.canonicalSpace)
    )
    assert(
      first.value.canonicalSpace
        .samePersistentIdentityAs(isolated.value.canonicalSpace)
    )
