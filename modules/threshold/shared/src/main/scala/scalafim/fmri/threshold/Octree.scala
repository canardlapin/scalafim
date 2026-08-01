package scalafim.fmri.threshold

object Octree:

  def root(field: MaskedField, priors: PriorWeights): Either[ThresholdError, ThresholdRegion] =
    if field.size != priors.length then
      return Left(ThresholdError.ShapeMismatch("field/priors", field.size.toString, priors.length.toString))
    val indices = Array.tabulate(field.size)(identity)
    ThresholdRegion.fromIndices(0, indices, field, priors)

  def split(
    parent: ThresholdRegion,
    field: MaskedField,
    priors: PriorWeights,
    minPriorMass: Double = 1e-10
  ): Either[ThresholdError, Vector[ThresholdRegion]] =
    if !minPriorMass.isFinite || minPriorMass < 0.0 then
      return Left(ThresholdError.InvalidArgument("minPriorMass", "must be finite and non-negative"))
    if field.size != priors.length then
      return Left(ThresholdError.ShapeMismatch("field/priors", field.size.toString, priors.length.toString))
    if !field.activeSpace.sameRuntimeOwnerAs(parent.membership.space) then
      return Left(
        ThresholdError.ShapeMismatch(
          "field/region support",
          // `descriptor` rather than `id`: a region's domain need not be
          // persistent, and this is a diagnostic either way.
          field.activeSpace.descriptor.toString,
          parent.membership.space.descriptor.toString
        )
      )
    if parent.bbox.isSingleton then return Right(Vector.empty)

    val xm = parent.bbox.midX
    val ym = parent.bbox.midY
    val zm = parent.bbox.midZ
    val builders = Array.fill(8)(Array.newBuilder[Int])
    val masses = Array.fill(8)(0.0)
    val parentIdx = parent.indexArray

    var i = 0
    while i < parentIdx.length do
      val idx = parentIdx(i)
      if idx < 0 || idx >= field.size then return Left(ThresholdError.IndexOutOfBounds(idx, field.size))
      val (xx, yy, zz) = field.coord(idx)
      val child =
        (if xx > xm then 1 else 0) +
          (if yy > ym then 2 else 0) +
          (if zz > zm then 4 else 0)
      builders(child) += idx
      masses(child) += priors(idx)
      i += 1

    val out = Vector.newBuilder[ThresholdRegion]
    var child = 0
    while child < 8 do
      val idx = builders(child).result()
      if idx.nonEmpty && masses(child) > minPriorMass then
        ThresholdRegion.fromIndices(child, idx, field, priors) match
          case Left(err) => return Left(err)
          case Right(region) => out += region
      child += 1
    Right(out.result())
