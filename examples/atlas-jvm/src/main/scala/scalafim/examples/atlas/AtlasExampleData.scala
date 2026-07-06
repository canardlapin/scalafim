package scalafim.examples.atlas

import narr.NArray
import scalafim.atlas.*
import scalafim.image.*

object AtlasExampleData:
  val dims: Vector[Int] =
    Vector(4, 4, 2)

  val space: NeuroSpace =
    NeuroSpace(
      dims = dims,
      spacing = Some(Vector(2.0, 2.0, 2.0)),
      origin = Some(Vector(0.0, 0.0, 0.0))
    )

  val regions: RegionIndex =
    RegionIndex(
      Vector(
        Region(
          RegionId(1),
          "Visual",
          labelFull = Some("left_visual"),
          hemisphere = Some(Hemisphere.Left),
          network = Some(NetworkId("YeoVisual")),
          color = Some(Rgb(220, 80, 80))
        ),
        Region(
          RegionId(2),
          "Somatomotor",
          labelFull = Some("right_somatomotor"),
          hemisphere = Some(Hemisphere.Right),
          network = Some(NetworkId("YeoSomMot")),
          color = Some(Rgb(80, 160, 220))
        ),
        Region(
          RegionId(3),
          "Default",
          labelFull = Some("bilateral_default"),
          hemisphere = Some(Hemisphere.Bilateral),
          network = Some(NetworkId("YeoDefault")),
          color = Some(Rgb(110, 190, 110))
        )
      )
    )

  val ref: AtlasRef =
    AtlasRef(
      family = "example",
      model = "ToyAtlas",
      representation = AtlasRepresentation.Volume,
      templateSpace = SpaceId.MNI152,
      coordSpace = SpaceId.MNI152,
      resolution = Some("2mm"),
      provenance = Some("scalafim examples"),
      source = Some("synthetic"),
      lineage = Some("Generated in memory for runnable examples."),
      confidence = Confidence.Exact,
      notes = Some("Tiny atlas with three parcels and background voxels.")
    )

  def atlas(): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(ref, regions, labelVolume(), label = "toy-atlas")

  def labelVolume(): NeuroVol[Int] =
    NeuroVol.fromLinear(labelData(), space, label = "toy-labels")

  def statMap(): NeuroVol[Double] =
    val labels = labelData()
    val out = NArray.ofSize[Double](labels.length)
    var i = 0
    while i < labels.length do
      out(i) =
        labels(i) match
          case 1 => 10.0
          case 2 => 20.0
          case 3 => 30.0
          case _ => 0.0
      i += 1
    NeuroVol.fromLinear(out, space, label = "toy-stat")

  private def labelData(): NArray[Int] =
    val out = NArrayUtil.fillConst[Int](dims.product, 0)
    fillBlock(out, 0 to 1, 0 to 1, 0 to 0, 1)
    fillBlock(out, 2 to 3, 0 to 1, 0 to 0, 2)
    fillBlock(out, 1 to 2, 2 to 3, 1 to 1, 3)
    out

  private def fillBlock(out: NArray[Int], xs: Range, ys: Range, zs: Range, id: Int): Unit =
    for
      x <- xs
      y <- ys
      z <- zs
    do out(Indexing.gridToIndex3D(dims, x, y, z)) = id
