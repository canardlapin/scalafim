package scalafim.graph.linalg

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.sparse.CSR
import gale.spectral.EigenDecomposition
import scalafim.graph.Direction
import scalafim.graph.Component
import scalafim.graph.Graph
import scalafim.graph.VertexBasis
import scalafim.graph.VertexIx

final class VertexOperator[K, V] private[linalg] (
    val basis: VertexBasis[K, V],
    val matrix: CSR
):
  require(matrix.rows == basis.size && matrix.cols == basis.size, "vertex operator must be square in its vertex basis")

  def map: DoubleLinearOperator =
    matrix

final class VertexSignal[K, V] private[linalg] (
    val basis: VertexBasis[K, V],
    val values: DVec
):
  require(values.length == basis.size, "vertex signal length must match basis size")

  def apply(index: VertexIx): Double =
    require(basis.contains(index), s"vertex index ${index.toInt} out of bounds for signal size ${basis.size}")
    values(index.toInt)

  def valueOf(key: K): Option[Double] =
    basis.indexOf(key).map(apply)

final case class EdgeReference[D <: Direction, K](from: K, to: K)

final class EdgeBasis[D <: Direction, K] private[linalg] (
    val direction: D,
    val edges: Vector[EdgeReference[D, K]]
):
  def size: Int =
    edges.length

object EdgeBasis:
  private[linalg] def fromGraph[D <: Direction, K, V, E](graph: Graph[D, K, V, E]): EdgeBasis[D, K] =
    val references = graph.edges.map: edge =>
      EdgeReference[D, K](
        graph.basis.keyAt(edge.endpoints.first),
        graph.basis.keyAt(edge.endpoints.second)
      )
    new EdgeBasis(graph.direction, references)

final class IncidenceOperator[D <: Direction, K, V] private[linalg] (
    val vertexBasis: VertexBasis[K, V],
    val edgeBasis: EdgeBasis[D, K],
    val matrix: CSR
):
  require(matrix.rows == vertexBasis.size, "incidence rows must match vertex basis size")
  require(matrix.cols == edgeBasis.size, "incidence columns must match edge basis size")

  def map: DoubleLinearOperator =
    matrix

enum NormalizedLaplacian:
  case Symmetric
  case RandomWalk

enum ZeroStrengthPolicy:
  case KeepZeroRow
  case IdentityOnZeroStrength
  case Error

enum SpectralLaplacian:
  case Combinatorial
  case SymmetricNormalized(zeroStrengthPolicy: ZeroStrengthPolicy)

enum EmbeddingEigenvectors:
  case IncludeSmallest
  case DropExpectedNullspace

/** Which algebraic end supplies a count-limited spectrum. Gale fixes the
  * returned layout to ascending algebraic order for either choice.
  */
enum SpectralEnd:
  case Smallest
  case Largest

final class WeightedSupport[K, V] private[linalg] (
    val basis: VertexBasis[K, V],
    val components: Vector[Component[K]],
    val zeroWeightEdgeCount: Int,
    val zeroStrengthVertices: Vector[K]
):
  require(zeroWeightEdgeCount >= 0, "zero-weight edge count must be non-negative")

final class VertexSpectrum[K, V] private[linalg] (
    val basis: VertexBasis[K, V],
    val result: EigenDecomposition,
    val operator: SpectralLaplacian,
    val support: WeightedSupport[K, V],
    val expectedZeroEigenvalueMultiplicity: Int
):
  require(result.eigenvectors.rows == basis.size, "spectrum vectors must use the vertex basis")
  require(expectedZeroEigenvalueMultiplicity >= 0, "expected nullity must be non-negative")

final class SpectralEmbedding[K, V] private[linalg] (
    val basis: VertexBasis[K, V],
    val eigenvalues: DVec,
    val coordinates: DMat,
    val residualNorms: DVec,
    val operator: SpectralLaplacian,
    val support: WeightedSupport[K, V],
    val droppedEigenvectors: Int
):
  require(coordinates.rows == basis.size, "embedding rows must use the vertex basis")
  require(coordinates.cols == eigenvalues.length, "embedding columns must match eigenvalues")
  require(residualNorms.length == eigenvalues.length, "embedding residuals must match eigenvalues")
  require(droppedEigenvectors >= 0, "dropped eigenvector count must be non-negative")
