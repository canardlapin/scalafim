package scalafim.multivar

/** Capability carried by a pure target map. Capabilities are ordered from the
  * most algebraically useful (`Linear`) to evaluation-only (`General`).
  */
enum MapCapability:
  case Linear
  case Affine
  case Smooth
  case General

  def combine(other: MapCapability): MapCapability =
    if ordinal >= other.ordinal then this else other

/** Runtime description retained after the statically typed expression graph is
  * erased into an [[OperatorProgram]].
  */
final case class TypedMapDescriptor(
    name: String,
    capability: MapCapability,
    operatorIdentities: Vector[ValueIdentity],
    equivariance: FrameSymmetry
):
  require(name.trim.nonEmpty, "typed map name must be non-empty")

/** A deterministic total map. Estimators, projections, proximal algorithms,
  * and solver iterations deliberately do not implement this interface.
  */
sealed trait TypedMap[A, B]:
  def descriptor: TypedMapDescriptor
  def apply(value: A): B

  final def andThen[C](next: TypedMap[B, C]): TypedGeneralMap[A, C] =
    TypedGeneralMap.instance(
      TypedMapDescriptor(
        s"${descriptor.name} >>> ${next.descriptor.name}",
        MapCapability.General,
        descriptor.operatorIdentities ++ next.descriptor.operatorIdentities,
        FrameSymmetry.meet(descriptor.equivariance, next.descriptor.equivariance)
      ),
      value => next(apply(value))
    )

sealed trait TypedLinearMap[A, B] extends TypedMap[A, B]:
  def dual: TypedLinearMap[B, A]

  final def andThenLinear[C](next: TypedLinearMap[B, C]): TypedLinearMap[A, C] =
    val forward = this
    TypedLinearMap.instance(
      TypedMapDescriptor(
        s"${descriptor.name} >>> ${next.descriptor.name}",
        MapCapability.Linear,
        descriptor.operatorIdentities ++ next.descriptor.operatorIdentities,
        FrameSymmetry.meet(descriptor.equivariance, next.descriptor.equivariance)
      ),
      value => next(forward(value)),
      value => forward.dual(next.dual(value))
    )

sealed trait TypedAffineMap[A, B] extends TypedMap[A, B]

sealed trait TypedSmoothMap[A, B] extends TypedMap[A, B]:
  def jvp(at: A, tangent: A): B
  def vjp(at: A, cotangent: B): A

sealed trait TypedGeneralMap[A, B] extends TypedMap[A, B]

object TypedLinearMap:
  def identity[A](name: String = "identity"): TypedLinearMap[A, A] =
    lazy val result: TypedLinearMap[A, A] =
      instance(
        TypedMapDescriptor(name, MapCapability.Linear, Vector.empty, FrameSymmetry.Orthogonal),
        value => value,
        value => value
      )
    result

  def instance[A, B](
      currentDescriptor: TypedMapDescriptor,
      forward: A => B,
      transpose: B => A
  ): TypedLinearMap[A, B] =
    require(currentDescriptor.capability == MapCapability.Linear, "linear map descriptor must be linear")
    new TypedLinearMap[A, B]:
      def apply(value: A): B = forward(value)
      def dual: TypedLinearMap[B, A] =
        TypedLinearMap.instance(
          currentDescriptor.copy(name = s"dual(${currentDescriptor.name})"),
          transpose,
          forward
        )
      val descriptor: TypedMapDescriptor = currentDescriptor

object TypedAffineMap:
  def instance[A, B](currentDescriptor: TypedMapDescriptor, evaluate: A => B): TypedAffineMap[A, B] =
    require(currentDescriptor.capability == MapCapability.Affine, "affine map descriptor must be affine")
    new TypedAffineMap[A, B]:
      val descriptor: TypedMapDescriptor = currentDescriptor
      def apply(value: A): B = evaluate(value)

object TypedSmoothMap:
  def instance[A, B](
      currentDescriptor: TypedMapDescriptor,
      evaluate: A => B,
      forwardDifferential: (A, A) => B,
      reverseDifferential: (A, B) => A
  ): TypedSmoothMap[A, B] =
    require(currentDescriptor.capability == MapCapability.Smooth, "smooth map descriptor must be smooth")
    new TypedSmoothMap[A, B]:
      val descriptor: TypedMapDescriptor = currentDescriptor
      def apply(value: A): B = evaluate(value)
      def jvp(at: A, tangent: A): B = forwardDifferential(at, tangent)
      def vjp(at: A, cotangent: B): A = reverseDifferential(at, cotangent)

object TypedGeneralMap:
  def instance[A, B](currentDescriptor: TypedMapDescriptor, evaluate: A => B): TypedGeneralMap[A, B] =
    new TypedGeneralMap[A, B]:
      val descriptor: TypedMapDescriptor = currentDescriptor
      def apply(value: A): B = evaluate(value)

/** A statically typed target-expression graph. Products make multi-parameter
  * targets ordinary typed maps instead of special callback nodes.
  */
sealed trait TypedExpression[A]:
  def parameterIds: Vector[ParameterId]
  def capability: MapCapability
  def operations: Vector[String]
  def operatorIdentities: Vector[ValueIdentity]
  def equivariance: FrameSymmetry

  final def through[B](map: TypedMap[A, B]): TypedExpression[B] =
    AppliedExpression(map, this)

  final def product[B](other: TypedExpression[B]): TypedExpression[(A, B)] =
    ProductExpression(this, other)

final case class ParameterExpression[A](id: ParameterId, semanticType: String) extends TypedExpression[A]:
  require(semanticType.trim.nonEmpty, "parameter semantic type must be non-empty")
  val parameterIds: Vector[ParameterId] = Vector(id)
  val capability: MapCapability = MapCapability.Linear
  val operations: Vector[String] = Vector(s"parameter:$semanticType")
  val operatorIdentities: Vector[ValueIdentity] = Vector.empty
  val equivariance: FrameSymmetry = FrameSymmetry.Orthogonal

final case class AppliedExpression[A, B](map: TypedMap[A, B], argument: TypedExpression[A]) extends TypedExpression[B]:
  val parameterIds: Vector[ParameterId] = argument.parameterIds
  val capability: MapCapability = argument.capability.combine(map.descriptor.capability)
  val operations: Vector[String] = argument.operations :+ map.descriptor.name
  val operatorIdentities: Vector[ValueIdentity] = argument.operatorIdentities ++ map.descriptor.operatorIdentities
  val equivariance: FrameSymmetry = FrameSymmetry.meet(argument.equivariance, map.descriptor.equivariance)

final case class ProductExpression[A, B](left: TypedExpression[A], right: TypedExpression[B])
    extends TypedExpression[(A, B)]:
  val parameterIds: Vector[ParameterId] = (left.parameterIds ++ right.parameterIds).distinct
  val capability: MapCapability = left.capability.combine(right.capability)
  val operations: Vector[String] = left.operations ++ right.operations :+ "product"
  val operatorIdentities: Vector[ValueIdentity] = left.operatorIdentities ++ right.operatorIdentities
  val equivariance: FrameSymmetry = FrameSymmetry.meet(left.equivariance, right.equivariance)

enum ConvexityTrait:
  case Convex
  case Nonconvex
  case Unknown

enum SmoothnessTrait:
  case Smooth
  case Nonsmooth
  case Unknown

enum HomogeneityTrait:
  case None
  case DegreeOne
  case DegreeTwo

enum SeparabilityTrait:
  case Elementwise
  case Rowwise
  case Blockwise
  case Spectral
  case Nonseparable

enum OracleCapability:
  case Gradient
  case HessianVector
  case Proximal
  case Conic

final case class FunctionalTraits(
    convexity: ConvexityTrait,
    smoothness: SmoothnessTrait,
    homogeneity: HomogeneityTrait,
    separability: SeparabilityTrait,
    capabilities: Set[OracleCapability],
    invariance: FrameSymmetry
):
  def supports(capability: OracleCapability): Boolean = capabilities.contains(capability)

enum SetConvexity:
  case Convex
  case Nonconvex
  case Unknown

enum SetStructure:
  case Euclidean
  case Cone
  case Manifold
  case Discrete

enum SetCapability:
  case Projection
  case Conic
  case NormalCone

final case class FeasibleSetTraits(
    convexity: SetConvexity,
    closed: Boolean,
    structure: SetStructure,
    separability: SeparabilityTrait,
    capabilities: Set[SetCapability],
    invariance: FrameSymmetry
):
  def supports(capability: SetCapability): Boolean = capabilities.contains(capability)

/** A typed functional can only advertise capabilities furnished by its sealed
  * constructor. Lowerings consume those traits; they are not solver operations
  * embedded in the semantic expression graph.
  */
final class TypedFunctional[Z] private (
    val kind: FunctionalKind,
    val traits: FunctionalTraits
)

object TypedFunctional:
  def l1[Z]: TypedFunctional[Z] = known(FunctionalKind.L1)
  def groupL21[Z]: TypedFunctional[Z] = known(FunctionalKind.GroupL21)
  def squaredNorm[Z](geometry: ValueIdentity): TypedFunctional[Z] = known(FunctionalKind.SquaredNorm(geometry))
  def elasticNet[Z](fraction: UnitFraction): TypedFunctional[Z] = known(FunctionalKind.ElasticNet(fraction))
  def huber[Z](delta: PenaltyWeight): TypedFunctional[Z] = known(FunctionalKind.Huber(delta))
  def totalVariation[Z]: TypedFunctional[Z] = known(FunctionalKind.TotalVariation)
  def nuclearNorm[Z]: TypedFunctional[Z] = known(FunctionalKind.NuclearNorm)
  def negativeLogDet[Z]: TypedFunctional[Z] = known(FunctionalKind.NegativeLogDet)

  private def known[Z](kind: FunctionalKind): TypedFunctional[Z] =
    new TypedFunctional(kind, kind.traits)

final class TypedFeasibleSet[Z] private (
    val kind: FeasibleSetKind,
    val traits: FeasibleSetTraits
)

object TypedFeasibleSet:
  def zero[Z]: TypedFeasibleSet[Z] = known(FeasibleSetKind.ZeroSubspace)
  def nonnegative[Z]: TypedFeasibleSet[Z] = known(FeasibleSetKind.NonnegativeOrthant)
  def simplex[Z]: TypedFeasibleSet[Z] = known(FeasibleSetKind.Simplex)
  def box[Z](bounds: ClosedInterval): TypedFeasibleSet[Z] = known(FeasibleSetKind.Box(bounds))
  def normBall[Z](radius: PenaltyWeight): TypedFeasibleSet[Z] = known(FeasibleSetKind.NormBall(radius))
  def psd[Z]: TypedFeasibleSet[Z] = known(FeasibleSetKind.PsdCone)
  def stiefel[Z]: TypedFeasibleSet[Z] = known(FeasibleSetKind.Stiefel)
  def fixedSupport[Z](indices: IndexSet): TypedFeasibleSet[Z] = known(FeasibleSetKind.FixedSupport(indices))
  def rankBounded[Z](rank: ComponentCount): TypedFeasibleSet[Z] = known(FeasibleSetKind.RankBounded(rank))

  private def known[Z](kind: FeasibleSetKind): TypedFeasibleSet[Z] =
    new TypedFeasibleSet(kind, kind.traits)
