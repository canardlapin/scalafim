package scalafim.image

import narr.NArray
import spire.std.int.given

class DomainValiditySuite extends munit.FunSuite:

  private val space = NeuroSpace(Vector(3, 3, 1))
  private val volumeSpace = VolumeSpace(space)

  test("cluster ids are positive domain values") {
    assertEquals(ClusterId.make(0), Left(ClusterIdError.NonPositive(0)), clue = "")
    assertEquals(ClusterId.make(-2), Left(ClusterIdError.NonPositive(-2)), clue = "")
    assertEquals(ClusterId.make(3).map(_.value), Right(3), clue = "")
  }

  test("cluster volumes reject non-positive assignments") {
    val mask = Mask.fromIndices(space, NArray[Int](0, 1))

    val error = intercept[IllegalArgumentException] {
      ClusteredNeuroVol(mask, NArray[Int](1, 0))
    }
    assert(error.getMessage.contains("cluster id must be positive"), clue = "")
  }

  test("cluster vectors cannot disagree with their cluster volume") {
    val mask = Mask.fromIndices(space, NArray[Int](0, 1))
    val clusters = ClusteredNeuroVol(mask, NArray[Int](1, 2))
    val timeSpace = space.addDim(2, Some(Axis.Time))
    val series = NDArray[Int](NArray[Int](10, 20, 30, 40), Vector(2, 2))
    val inconsistentMap = NArrayUtil.fillConst[Int](space.spatialDims.product, 0)
    inconsistentMap(0) = 2
    inconsistentMap(1) = 1

    val error = intercept[IllegalArgumentException] {
      ClusteredNeuroVec(clusters, series, inconsistentMap, timeSpace)
    }
    assert(error.getMessage.contains("disagrees with cluster volume"), clue = "")
  }

  test("ROI windows validate that the selected center is the parent voxel") {
    val coords = ROICoords(Vector(Vector(0, 0, 0), Vector(1, 0, 0)))
    val result =
      ROIVolWindow.make(
        space,
        coords,
        NArray[Int](1, 1),
        centerIndex = 1,
        parentIndex = 0
      )

    assertEquals(
      result,
      Left(ROIVolWindowError.CenterMismatch(1, 0, 1)),
      clue = ""
    )
  }

  test("checked searchlight extraction reports an excluded center") {
    val values = NArrayUtil.fillConst[Int](space.spatialDims.product, 1)
    values(4) = 0
    val volume = NeuroVol.fromLinear[Int](values, space)
    val center =
      SearchlightCenter
        .make(volumeSpace, VoxelCoord(1, 1, 0))
        .fold(error => fail(error.message), identity)
    val radius =
      SearchlightRadius.make(1.0).fold(error => fail(error.message), identity)

    val result =
      Searchlight.sphericalRoiChecked(
        volume,
        center,
        radius,
        support = SearchlightValueSupport.NonZero
      )

    assertEquals(
      result,
      Left(SearchlightError.CenterExcluded(VoxelCoord(1, 1, 0))),
      clue = ""
    )
  }

  test("checked searchlight policies produce valid mask-bound windows") {
    val mask = Mask.fromIndices(space, NArray[Int](0, 4))
    val radius =
      SearchlightRadius.make(1.0).fold(error => fail(error.message), identity)

    val windows =
      Searchlight
        .searchlightChecked(
          mask,
          radius,
          SearchlightCenterDomain.MaskVoxels,
          SearchlightSupport.InsideMask
        )
        .fold(error => fail(error.message), identity)
        .toVector

    assertEquals(windows.length, 2, clue = "")
    assert(windows.forall(window => window.region.size == 1), clue = "")
    assert(windows.forall(window => window.region.contains(window.centerVoxel)), clue = "")
  }

  test("checked searchlight policies reject centers outside constrained support") {
    val mask = Mask.fromIndices(space, NArray[Int](0))
    val radius =
      SearchlightRadius.make(1.0).fold(error => fail(error.message), identity)

    assertEquals(
      Searchlight
        .searchlightChecked(
          mask,
          radius,
          SearchlightCenterDomain.AllVoxels,
          SearchlightSupport.InsideMask
        ),
      Left(
        SearchlightError.IncompatiblePolicies(
          SearchlightCenterDomain.AllVoxels,
          SearchlightSupport.InsideMask
        )
      ),
      clue = ""
    )
  }

  test("searchlight radii reject non-positive and non-finite values") {
    assertEquals(
      SearchlightRadius.make(0.0),
      Left(SearchlightError.InvalidRadius(0.0)),
      clue = ""
    )
    assert(SearchlightRadius.make(Double.PositiveInfinity).isLeft, clue = "")
    assert(SearchlightRadius.make(Double.NaN).isLeft, clue = "")
  }
