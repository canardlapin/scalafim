package scalafim.surface

import locus4s.FiniteDomain
import locus4s.Index
import locus4s.Region
import locus4s.Selection
import locus4s.data.Field
import locus4s.data.SectionView
import mesh4s.TriangleTopology

/** Exact typed vertex support beneath the legacy surface-data facades.
  *
  * `topology.vertices` is the only vertex-domain owner. `selection` retains
  * the caller's row order as a typed injection, while `support` is the
  * corresponding extensional region.
  */
sealed trait SurfaceVertexSelection:
  type Vertex

  val topology: TriangleTopology { type Vertex = SurfaceVertexSelection.this.Vertex }
  val selection: Selection[Vertex]

  final def vertices: FiniteDomain[Vertex] =
    topology.vertices

  final def support: Region[Vertex] =
    selection.support

  final def hasSameRuntimeOwnerAs(that: SurfaceVertexSelection): Boolean =
    vertices.sameRuntimeOwnerAs(that.vertices)

/** Sparse-or-full values attached to one exact topology vertex owner. */
sealed trait SurfaceVertexField[+A] extends SurfaceVertexSelection:
  val optionalValues: Field[Vertex, Option[A]]
  val section: SectionView[Vertex, Option[A]]

  /** Present exactly when the support is the whole vertex domain. */
  def fullValues: Option[Field[Vertex, A]]

  private[surface] def rowAtOrdinal(ordinal: Int): Int

/** Matrix rows attached through a typed selection into one vertex owner. */
sealed trait SurfaceVertexMatrix[+A] extends SurfaceVertexSelection:
  val columns: Int
  val rows: Field[selection.I, Vector[A]]

  final def apply(position: Index[selection.I], column: Int): A =
    require(column >= 0 && column < columns, "surface matrix column out of range")
    rows(position)(column)

private[surface] object SurfaceVertexData:

  def field[A](
      geometry: SurfaceGeometry,
      indices: Array[Int],
      data: Array[A]
  ): SurfaceVertexField[A] =
    val owner = geometry.mesh.topology
    val selected =
      Selection
        .fromOrdinals(owner.vertices, indices)
        .fold(
          error => throw new IllegalStateException(error.message),
          identity
        )
    val rowByOrdinal = Array.fill(owner.vertices.size)(-1)
    var row = 0
    while row < indices.length do
      rowByOrdinal(indices(row)) = row
      row += 1

    val optional =
      Field.view(owner.vertices): vertex =>
        val selectedRow = rowByOrdinal(vertex.ordinal)
        if selectedRow < 0 then None else Some(data(selectedRow))
    val restricted = optional.restrict(selected.support)
    val total =
      if selected.support.isWhole then
        Some:
          Field.view(owner.vertices): vertex =>
            data(rowByOrdinal(vertex.ordinal))
      else None

    new SurfaceVertexField[A]:
      type Vertex = owner.Vertex

      val topology = owner
      val selection: Selection[Vertex] = selected
      val optionalValues: Field[Vertex, Option[A]] = optional
      val section: SectionView[Vertex, Option[A]] = restricted

      def fullValues: Option[Field[Vertex, A]] =
        total

      private[surface] def rowAtOrdinal(ordinal: Int): Int =
        if ordinal < 0 || ordinal >= rowByOrdinal.length then -1
        else rowByOrdinal(ordinal)

  def matrix[A](
      geometry: SurfaceGeometry,
      indices: Array[Int],
      data: Array[A],
      columnCount: Int
  ): SurfaceVertexMatrix[A] =
    val owner = geometry.mesh.topology
    val selected =
      Selection
        .fromOrdinals(owner.vertices, indices)
        .fold(
          error => throw new IllegalStateException(error.message),
          identity
        )

    new SurfaceVertexMatrix[A]:
      type Vertex = owner.Vertex

      val topology = owner
      val selection: Selection[Vertex] = selected
      val columns: Int = columnCount
      val rows: Field[selection.I, Vector[A]] =
        Field.view(selection.positions): position =>
          val offset = position.ordinal * columns
          Vector.tabulate(columns)(column => data(offset + column))
