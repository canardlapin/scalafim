package scalafim.inference

import ComponentSet.components

opaque type ComponentSet = Vector[ComponentIx]

object ComponentSet:
  def from(values: Iterable[ComponentIx]): Either[InferenceError, ComponentSet] =
    val entries = values.toVector
    if entries.isEmpty then Left(InferenceError.EmptyComponentSet("component set"))
    else
      var i = 1
      var error = Option.empty[InferenceError]
      while i < entries.length && error.isEmpty do
        val previous = entries(i - 1).value
        val next = entries(i).value
        if previous == next then error = Some(InferenceError.DuplicateComponent(next))
        else if previous > next then error = Some(InferenceError.UnorderedComponents(previous, next))
        i += 1
      error.toLeft(entries)

  def one(component: ComponentIx): ComponentSet =
    Vector(component)

  extension (value: ComponentSet)
    inline def components: Vector[ComponentIx] = value
    inline def size: Int = value.length
    inline def head: ComponentIx = value.head

    def requireWithin(rank: Int): Either[InferenceError, ComponentSet] =
      value.find(_.value >= rank) match
        case Some(component) => Left(InferenceError.ComponentOutOfRange(component.value, rank))
        case None            => Right(value)

opaque type SubspaceComponents = ComponentSet

object SubspaceComponents:
  def from(value: ComponentSet): Either[InferenceError, SubspaceComponents] =
    if value.size >= 2 then Right(value)
    else Left(InferenceError.EmptyComponentSet("subspace must contain at least two components"))

  extension (value: SubspaceComponents)
    inline def components: ComponentSet = value

opaque type DeclaredUnitGroups = Vector[ComponentSet]

object DeclaredUnitGroups:
  def from(values: Iterable[ComponentSet]): Either[InferenceError, DeclaredUnitGroups] =
    val groups = values.toVector
    if groups.isEmpty then Left(InferenceError.EmptyComponentSet("declared unit groups"))
    else
      val seen = scala.collection.mutable.HashSet.empty[Int]
      var groupIndex = 0
      var error = Option.empty[InferenceError]
      while groupIndex < groups.length && error.isEmpty do
        val components = groups(groupIndex).components
        var i = 0
        while i < components.length && error.isEmpty do
          val component = components(i).value
          if seen(component) then error = Some(InferenceError.DuplicateComponent(component))
          else seen += component
          i += 1
        groupIndex += 1
      error.toLeft(groups)

  extension (value: DeclaredUnitGroups)
    inline def groups: Vector[ComponentSet] = value

enum Identifiability:
  case OrientableAxis
  case UnorientedSubspace

enum LatentUnit:
  case Axis(unitId: UnitId, component: ComponentIx)
  case Subspace(unitId: UnitId, components: SubspaceComponents)

  def id: UnitId =
    this match
      case Axis(value, _)     => value
      case Subspace(value, _) => value

  def componentSet: ComponentSet =
    this match
      case Axis(_, component)     => ComponentSet.one(component)
      case Subspace(_, components) => components.components

  def identifiability: Identifiability =
    this match
      case Axis(_, _)     => Identifiability.OrientableAxis
      case Subspace(_, _) => Identifiability.UnorientedSubspace

enum UnitPolicy:
  case SingleAxes
  case GroupNearTies(relativeGap: RelativeGap)
  case Declared(groups: DeclaredUnitGroups)

final case class LatentUnitResult(
    unit: LatentUnit,
    roots: Vector[Double],
    selected: Boolean
)

object LatentUnitFormation:
  def form(
      roots: Iterable[Double],
      selected: Iterable[Boolean],
      policy: UnitPolicy
  ): Either[InferenceError, Vector[LatentUnitResult]] =
    val values = roots.toVector
    val choices = selected.toVector
    for
      _ <- validateRoots(values)
      _ <-
        if choices.length == values.length then Right(())
        else Left(InferenceError.InvalidUnit(
          s"selection length ${choices.length} does not match root count ${values.length}"
        ))
      groups <- groupsFor(values, policy)
    yield groups.zipWithIndex.map { case (group, index) =>
      val unitId = UnitId.unsafe(s"u${index + 1}")
      val members = group.components
      val unit =
        if members.length == 1 then LatentUnit.Axis(unitId, members.head)
        else LatentUnit.Subspace(unitId, acceptedSubspace(group))
      LatentUnitResult(
        unit,
        members.map(component => values(component.value)),
        members.forall(component => choices(component.value))
      )
    }

  private def validateRoots(values: Vector[Double]): Either[InferenceError, Unit] =
    if values.isEmpty then Left(InferenceError.InvalidSpectrum("latent roots must be non-empty"))
    else
      var i = 0
      var previous = Double.PositiveInfinity
      while i < values.length do
        val value = values(i)
        if !value.isFinite then return Left(InferenceError.InvalidSpectrum(s"root $i is not finite: $value"))
        if value < 0.0 then return Left(InferenceError.InvalidSpectrum(s"root $i is negative: $value"))
        if value > previous then
          return Left(InferenceError.InvalidSpectrum(s"root $i increases from $previous to $value"))
        previous = value
        i += 1
      Right(())

  private def groupsFor(
      roots: Vector[Double],
      policy: UnitPolicy
  ): Either[InferenceError, Vector[ComponentSet]] =
    policy match
      case UnitPolicy.SingleAxes =>
        Right(Vector.tabulate(roots.length)(index => ComponentSet.one(ComponentIx.unsafe(index))))
      case UnitPolicy.GroupNearTies(relativeGap) =>
        val numericalZero = Math.max(1.0, roots.sum) * Math.ulp(1.0)
        val comparable = roots.map(value => if value <= numericalZero then 0.0 else value)
        val out = Vector.newBuilder[ComponentSet]
        var start = 0
        var index = 0
        while index < roots.length do
          val closes =
            index == roots.length - 1 ||
              (comparable(index) - comparable(index + 1)) /
                Math.max(comparable(index), Math.ulp(1.0)) >= relativeGap.value
          if closes then
            out += acceptedSet(Vector.tabulate(index - start + 1)(offset => ComponentIx.unsafe(start + offset)))
            start = index + 1
          index += 1
        Right(out.result())
      case UnitPolicy.Declared(groups) =>
        val ordered = groups.groups.sortBy(_.head.value)
        var expected = 0
        var groupIndex = 0
        while groupIndex < ordered.length do
          val members = ordered(groupIndex).components
          var i = 0
          while i < members.length do
            val actual = members(i).value
            if actual != expected then
              return Left(InferenceError.InvalidUnit(
                s"declared groups must cover components 0 through ${roots.length - 1}; expected $expected, got $actual"
              ))
            expected += 1
            i += 1
          groupIndex += 1
        if expected != roots.length then
          Left(InferenceError.InvalidUnit(
            s"declared groups cover $expected of ${roots.length} components"
          ))
        else Right(ordered)

  private def acceptedSet(values: Vector[ComponentIx]): ComponentSet =
    ComponentSet.from(values) match
      case Right(value) => value
      case Left(error)  => throw IllegalStateException(error.message)

  private def acceptedSubspace(value: ComponentSet): SubspaceComponents =
    SubspaceComponents.from(value) match
      case Right(result) => result
      case Left(error)   => throw IllegalStateException(error.message)
