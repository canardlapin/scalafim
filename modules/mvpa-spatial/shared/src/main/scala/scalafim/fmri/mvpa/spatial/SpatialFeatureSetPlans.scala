package scalafim.fmri.mvpa.spatial

import scalafim.atlas.VolumeAtlas
import scalafim.fmri.mvpa.*
import scalafim.image.{Mask, NeuroVol, ROIVolWindow, Searchlight}
import scalafim.surface.{FragmentedParcelPolicy, LabeledSurface, MeshTopology, ParcelUnit, SurfaceParcels}

import scala.util.control.NonFatal

object SpatialFeatureSetPlans:

  def fromVolumeLabels(
      name: String,
      labels: NeuroVol[Int],
      background: Set[Int] = Set(0)
  ): Either[MvpaError, FeatureSetPlan] =
    val byLabel = scala.collection.mutable.Map.empty[Int, scala.collection.mutable.ArrayBuffer[Int]]
    val values = labels.values.data
    var lin = 0
    while lin < values.length do
      val label = values(lin)
      if !background(label) then
        if label < 0 then
          return Left(MvpaError.InvalidFeatureSetPlan("volume labels must be non-negative after background removal"))
        byLabel.getOrElseUpdate(label, scala.collection.mutable.ArrayBuffer.empty) += lin
      lin += 1

    val ids = byLabel.keys.toVector.sorted
    buildSets(ids) { id =>
      FeatureSet(
        RoiId(id),
        byLabel(id).toVector,
        label = Some(id.toString)
      )
    }.flatMap(sets => FeatureSetPlan.regional(name, sets))

  def fromVolumeAtlas(
      name: String,
      atlas: VolumeAtlas
  ): Either[MvpaError, FeatureSetPlan] =
    val byLabel = scala.collection.mutable.Map.empty[Int, scala.collection.mutable.ArrayBuffer[Int]]
    val labels = atlas.labelVolume.values.data
    var lin = 0
    while lin < labels.length do
      val label = labels(lin)
      if label != 0 then
        byLabel.getOrElseUpdate(label, scala.collection.mutable.ArrayBuffer.empty) += lin
      lin += 1

    buildSets(atlas.regions.regions) { region =>
      FeatureSet(
        RoiId(region.id.value),
        byLabel.getOrElse(region.id.value, scala.collection.mutable.ArrayBuffer.empty).toVector,
        label = Some(region.label)
      )
    }.flatMap(sets => FeatureSetPlan.regional(name, sets))

  def fromRoiWindows(
      name: String,
      windows: Seq[ROIVolWindow[?]]
  ): Either[MvpaError, FeatureSetPlan] =
    captureInvalid("ROI windows") {
      buildSets(windows.toVector) { window =>
        val linear = window.coords.linearIndices(window.space)
        val indices = Vector.tabulate(linear.length)(i => linear(i))
        val label =
          if window.label.trim.nonEmpty then window.label.trim
          else s"searchlight_${window.parentIndex}"
        FeatureSet(
          RoiId(window.parentIndex),
          indices,
          center = Some(window.parentIndex),
          label = Some(label)
        )
      }.flatMap(sets => FeatureSetPlan.searchlight(name, sets))
    }

  def fromSearchlightMask(
      name: String,
      mask: Mask.MaskVol,
      radius: Double,
      constrainToMask: Boolean = true,
      label: String = ""
  ): Either[MvpaError, FeatureSetPlan] =
    captureInvalid("searchlight mask") {
      fromRoiWindows(
        name,
        Searchlight.searchlight(mask, radius, nonzero = constrainToMask, label = label).toVector
      )
    }

  def fromSurfaceParcels(
      name: String,
      parcels: Seq[ParcelUnit]
  ): Either[MvpaError, FeatureSetPlan] =
    buildSets(parcels.toVector.zipWithIndex) { case (parcel, ordinal) =>
      val label = parcel.info.map(_.name).getOrElse(parcel.key.display)
      FeatureSet(
        RoiId(ordinal),
        parcel.vertices.map(_.index),
        label = Some(label)
      )
    }.flatMap(sets => FeatureSetPlan.regional(name, sets))

  def fromLabeledSurface(
      name: String,
      labeled: LabeledSurface,
      topology: MeshTopology,
      policy: FragmentedParcelPolicy = FragmentedParcelPolicy.Error,
      ignoredLabels: Set[Int] = Set.empty
  ): Either[MvpaError, FeatureSetPlan] =
    captureInvalid("labeled surface") {
      fromSurfaceParcels(
        name,
        SurfaceParcels.units(labeled, topology, policy, ignoredLabels)
      )
    }

  private def buildSets[A](
      items: Seq[A]
  )(build: A => Either[MvpaError, FeatureSet]): Either[MvpaError, Vector[FeatureSet]] =
    val out = Vector.newBuilder[FeatureSet]
    var error: MvpaError | Null = null
    val iterator = items.iterator
    while iterator.hasNext && error == null do
      build(iterator.next()) match
        case Right(featureSet) => out += featureSet
        case Left(e) => error = e
    error match
      case null => Right(out.result())
      case e => Left(e)

  private def captureInvalid[A](label: String)(body: => Either[MvpaError, A]): Either[MvpaError, A] =
    try body
    catch
      case NonFatal(error) =>
        Left(MvpaError.InvalidFeatureSetPlan(s"$label: ${error.getMessage}"))
