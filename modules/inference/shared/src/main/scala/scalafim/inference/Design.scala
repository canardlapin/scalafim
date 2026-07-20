package scalafim.inference

final case class RowPartition private (
    rowCount: RowCount,
    groups: Vector[Vector[RowIx]]
):
  def groupIndexByRow: Array[Int] =
    val out = new Array[Int](rowCount.value)
    var groupIndex = 0
    while groupIndex < groups.length do
      val group = groups(groupIndex)
      var i = 0
      while i < group.length do
        out(group(i).value) = groupIndex
        i += 1
      groupIndex += 1
    out

object RowPartition:
  def from(rows: RowCount, groups: Iterable[Iterable[Int]]): Either[InferenceError, RowPartition] =
    val raw = groups.iterator.map(_.toVector).toVector
    if raw.isEmpty then Left(InferenceError.InvalidPartition("at least one group is required"))
    else if raw.exists(_.isEmpty) then Left(InferenceError.InvalidPartition("groups must be non-empty"))
    else
      val seen = Array.fill(rows.value)(false)
      val checked = Vector.newBuilder[Vector[RowIx]]
      var groupIndex = 0
      var error = Option.empty[InferenceError]
      while groupIndex < raw.length && error.isEmpty do
        val group = raw(groupIndex)
        val out = Vector.newBuilder[RowIx]
        var i = 0
        while i < group.length && error.isEmpty do
          val row = group(i)
          if row < 0 || row >= rows.value then
            error = Some(InferenceError.InvalidPartition(s"row $row is outside [0, ${rows.value})"))
          else if seen(row) then
            error = Some(InferenceError.InvalidPartition(s"row $row appears more than once"))
          else
            seen(row) = true
            out += RowIx.unsafe(row)
          i += 1
        checked += out.result()
        groupIndex += 1
      var row = 0
      while row < rows.value && error.isEmpty do
        if !seen(row) then error = Some(InferenceError.InvalidPartition(s"row $row is not covered"))
        row += 1
      error.toLeft(RowPartition(rows, checked.result()))

final case class ClusterPartition private (value: RowPartition):
  def rowCount: RowCount = value.rowCount
  def clusters: Vector[Vector[RowIx]] = value.groups

object ClusterPartition:
  def from(rows: RowCount, clusters: Iterable[Iterable[Int]]): Either[InferenceError, ClusterPartition] =
    RowPartition.from(rows, clusters).map(ClusterPartition(_))

final case class StrataPartition private (value: RowPartition):
  def rowCount: RowCount = value.rowCount
  def strata: Vector[Vector[RowIx]] = value.groups

object StrataPartition:
  def from(rows: RowCount, strata: Iterable[Iterable[Int]]): Either[InferenceError, StrataPartition] =
    RowPartition.from(rows, strata).map(StrataPartition(_))

final case class NestedPartitions private (
    inner: RowPartition,
    outer: RowPartition
)

object NestedPartitions:
  def from(
      inner: RowPartition,
      outer: RowPartition
  ): Either[InferenceError, NestedPartitions] =
    if inner.rowCount != outer.rowCount then
      Left(InferenceError.RowCountMismatch(
        "nested outer partition",
        inner.rowCount.value,
        outer.rowCount.value
      ))
    else
      val outerByRow = outer.groupIndexByRow
      var innerIndex = 0
      var error = Option.empty[InferenceError]
      while innerIndex < inner.groups.length && error.isEmpty do
        val group = inner.groups(innerIndex)
        val expectedOuter = outerByRow(group.head.value)
        var i = 1
        while i < group.length && error.isEmpty do
          val actualOuter = outerByRow(group(i).value)
          if actualOuter != expectedOuter then
            error = Some(InferenceError.InvalidPartition(
              s"inner group $innerIndex crosses outer groups $expectedOuter and $actualOuter"
            ))
          i += 1
        innerIndex += 1
      error.toLeft(NestedPartitions(inner, outer))

enum WhiteningRequirement:
  case NotRequired
  case Required

enum SamplingUnits:
  case Rows(count: RowCount)
  case Clusters(partition: ClusterPartition)

  def rowCount: RowCount =
    this match
      case Rows(count)         => count
      case Clusters(partition) => partition.rowCount

enum Exchangeability:
  case Unrestricted
  case WithinBlocks(partition: RowPartition)
  case WithinStrata(partition: StrataPartition)

enum Conditioning:
  case Unadjusted
  case Nuisance(
      reference: ConditioningRef,
      rows: RowCount,
      whitening: WhiteningRequirement
  )

sealed trait DesignKind

object DesignKind:
  sealed trait ExchangeableRows extends DesignKind
  sealed trait WithinBlockRows extends DesignKind
  sealed trait WithinStrataRows extends DesignKind
  sealed trait ClusterRows extends DesignKind
  sealed trait ConditionedRows extends DesignKind

final case class ResamplingDesign[K <: DesignKind] private (
    units: SamplingUnits,
    exchangeability: Exchangeability,
    conditioning: Conditioning
):
  def rowCount: RowCount = units.rowCount

object ResamplingDesign:
  def exchangeableRows(rows: RowCount): ResamplingDesign[DesignKind.ExchangeableRows] =
    ResamplingDesign(SamplingUnits.Rows(rows), Exchangeability.Unrestricted, Conditioning.Unadjusted)

  def withinBlocks(partition: RowPartition): ResamplingDesign[DesignKind.WithinBlockRows] =
    ResamplingDesign(
      SamplingUnits.Rows(partition.rowCount),
      Exchangeability.WithinBlocks(partition),
      Conditioning.Unadjusted
    )

  def withinStrata(partition: StrataPartition): ResamplingDesign[DesignKind.WithinStrataRows] =
    ResamplingDesign(
      SamplingUnits.Rows(partition.rowCount),
      Exchangeability.WithinStrata(partition),
      Conditioning.Unadjusted
    )

  def clustered(partition: ClusterPartition): ResamplingDesign[DesignKind.ClusterRows] =
    ResamplingDesign(SamplingUnits.Clusters(partition), Exchangeability.Unrestricted, Conditioning.Unadjusted)

  def nuisanceAdjusted(
      rows: RowCount,
      reference: ConditioningRef,
      whitening: WhiteningRequirement
  ): ResamplingDesign[DesignKind.ConditionedRows] =
    ResamplingDesign(
      SamplingUnits.Rows(rows),
      Exchangeability.Unrestricted,
      Conditioning.Nuisance(reference, rows, whitening)
    )

  def nuisanceAdjustedWithinBlocks(
      partition: RowPartition,
      reference: ConditioningRef,
      whitening: WhiteningRequirement
  ): ResamplingDesign[DesignKind.ConditionedRows] =
    ResamplingDesign(
      SamplingUnits.Rows(partition.rowCount),
      Exchangeability.WithinBlocks(partition),
      Conditioning.Nuisance(reference, partition.rowCount, whitening)
    )

  def nuisanceAdjustedWithinStrata(
      partition: StrataPartition,
      reference: ConditioningRef,
      whitening: WhiteningRequirement
  ): ResamplingDesign[DesignKind.ConditionedRows] =
    ResamplingDesign(
      SamplingUnits.Rows(partition.rowCount),
      Exchangeability.WithinStrata(partition),
      Conditioning.Nuisance(reference, partition.rowCount, whitening)
    )
