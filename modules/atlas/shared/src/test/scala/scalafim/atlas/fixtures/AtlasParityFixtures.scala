package scalafim.atlas.fixtures

import narr.NArray
import scalafim.atlas.*
import scalafim.image.*

object AtlasParityFixtures:
  final case class RegionExpected(
    id: Int,
    label: String,
    labelFull: String,
    hemisphere: Option[Hemisphere],
    network: Option[String],
    voxelCount: Int
  )

  final case class OverlapExpected(
    region1: Int,
    region2: Int,
    dice: Double,
    jaccard: Double,
    nOverlap: Int,
    nRegion1: Int,
    nRegion2: Int
  )

  val dims: Vector[Int] =
    Vector(5, 5, 5)

  val ref: VolumeAtlasRef =
    AtlasRef.volume(
      family = "neuroatlas-parity",
      model = "NonContiguousFixture",
      templateSpace = SpaceId.MNI152,
      coordSpace = SpaceId.MNI152,
      confidence = Confidence.Exact,
      notes = Some("Synthetic fixture mirroring neuroatlas non-contiguous ID parity tests.")
    )

  val regions: RegionIndex =
    RegionIndex(
      Vector(
        Region(
          RegionId(10),
          "RegionA",
          labelFull = Some("left_RegionA"),
          hemisphere = Some(Hemisphere.Left),
          network = Some(NetworkId("NetA")),
          color = Some(Rgb(255, 0, 0))
        ),
        Region(
          RegionId(50),
          "RegionB",
          labelFull = Some("right_RegionB"),
          hemisphere = Some(Hemisphere.Right),
          network = Some(NetworkId("NetA")),
          color = Some(Rgb(0, 255, 0))
        ),
        Region(
          RegionId(90),
          "RegionC",
          labelFull = Some("midline_RegionC"),
          hemisphere = Some(Hemisphere.Midline),
          network = Some(NetworkId("NetB")),
          color = Some(Rgb(0, 0, 255))
        )
      )
    )

  val regionExpectations: Vector[RegionExpected] =
    Vector(
      RegionExpected(10, "RegionA", "left_RegionA", Some(Hemisphere.Left), Some("NetA"), 8),
      RegionExpected(50, "RegionB", "right_RegionB", Some(Hemisphere.Right), Some("NetA"), 8),
      RegionExpected(90, "RegionC", "midline_RegionC", Some(Hemisphere.Midline), Some("NetB"), 2)
    )

  val parcelMeans: Map[Int, Double] =
    Map(10 -> 5.0, 50 -> 9.0, 90 -> 13.0)

  val vecSeries: Map[Int, Vector[Double]] =
    Map(
      10 -> Vector(5.0, 10.0, 15.0),
      50 -> Vector(9.0, 18.0, 27.0),
      90 -> Vector(13.0, 26.0, 39.0)
    )

  val overlapExpectations: Vector[OverlapExpected] =
    Vector(
      OverlapExpected(50, 202, 1.0, 1.0, 8, 8, 8),
      OverlapExpected(90, 303, 1.0, 1.0, 2, 2, 2),
      OverlapExpected(10, 101, 2.0 / 3.0, 0.5, 4, 8, 4)
    )

  def atlas(): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(ref, regions, NeuroVol.fromLinear(labelData(), space), label = "noncontig")

  def comparisonAtlas(): VolumeAtlas =
    val comparisonRegions =
      RegionIndex(
        Vector(
          Region(RegionId(101), "RegionA-left-half", hemisphere = Some(Hemisphere.Left)),
          Region(RegionId(202), "RegionB-copy", hemisphere = Some(Hemisphere.Right)),
          Region(RegionId(303), "RegionC-copy", hemisphere = Some(Hemisphere.Midline))
        )
      )
    VolumeAtlas.fromLabelVolume(
      ref.copy(family = "neuroatlas-parity-comparison", model = "ComparisonFixture"),
      comparisonRegions,
      NeuroVol.fromLinear(comparisonLabelData(), space),
      label = "comparison"
    )

  def dataVolume(): NeuroVol[Double] =
    val labels = labelData()
    val out = NArray.ofSize[Double](labels.length)
    var i = 0
    while i < labels.length do
      out(i) = parcelMeans.getOrElse(labels(i), 0.0)
      i += 1
    NeuroVol.fromLinear(out, space, label = "parcel-means")

  def dataVec(nTime: Int = 3): NeuroVec[Double] =
    val labels = labelData()
    val spatialNels = dims.product
    val out = NArray.ofSize[Double](spatialNels * nTime)
    var t = 0
    while t < nTime do
      var i = 0
      while i < spatialNels do
        out(i + t * spatialNels) = parcelMeans.getOrElse(labels(i), 0.0) * (t + 1).toDouble
        i += 1
      t += 1
    NeuroVec.fromLinear(out, space.addDim(nTime, Some(Axis.Time)), label = "parcel-series")

  def fullMask(): NeuroVol[Boolean] =
    NeuroVol.fromLinear(NArrayUtil.fillConst[Boolean](dims.product, true), space, label = "full")

  def emptyMask(): NeuroVol[Boolean] =
    NeuroVol.fromLinear(NArrayUtil.fillConst[Boolean](dims.product, false), space, label = "empty")

  def labelData(): NArray[Int] =
    val out = NArrayUtil.fillConst[Int](dims.product, 0)
    fillBlock(out, 0 to 1, 0 to 1, 0 to 1, 10)
    fillBlock(out, 3 to 4, 3 to 4, 3 to 4, 50)
    fillBlock(out, 2 to 2, 2 to 2, 0 to 1, 90)
    out

  private def comparisonLabelData(): NArray[Int] =
    val out = NArrayUtil.fillConst[Int](dims.product, 0)
    fillBlock(out, 0 to 0, 0 to 1, 0 to 1, 101)
    fillBlock(out, 3 to 4, 3 to 4, 3 to 4, 202)
    fillBlock(out, 2 to 2, 2 to 2, 0 to 1, 303)
    out

  private def space: NeuroSpace =
    NeuroSpace(
      dims = dims,
      spacing = Some(Vector(1.0, 1.0, 1.0)),
      origin = Some(Vector(0.0, 0.0, 0.0))
    )

  private def fillBlock(out: NArray[Int], xs: Range, ys: Range, zs: Range, id: Int): Unit =
    for
      x <- xs
      y <- ys
      z <- zs
    do out(Indexing.gridToIndex3D(dims, x, y, z)) = id
