package scalafim.examples.surfaceview

import scalafim.surface.*
import scalafim.surface.view.*

final case class CorticalMorphCorpusEntry(
  fileName: String,
  sha256: String,
  hemisphere: Hemisphere,
  kind: SurfaceKind
)

final case class CorticalMorphCase(
  from: SurfaceKind,
  to: SurfaceKind,
  fraction: SurfaceMorphFraction,
  plan: SurfaceRenderPlan
):
  def label: String = s"${from.label}-to-${to.label}@${fraction.value}"

/** Live visual-QA fixture built from matching real fsaverage5 cortical
  * surfaces. The spherical state is a deterministic radial projection of the
  * same real topology, keeping the acceptance corpus small while exercising
  * the most extreme geometry transition.
  */
object CorticalSurfaceMorphAcceptance:
  val Corpus: Vector[CorticalMorphCorpusEntry] = Vector(
    CorticalMorphCorpusEntry(
      "lh.white.gii",
      "b3043744a8ea99d8b497599294f1bdcc0852af648d34460b85bbf2d1d704f500",
      Hemisphere.Left,
      SurfaceKind.White
    ),
    CorticalMorphCorpusEntry(
      "lh.pial.gii",
      "b425b4914362af4aaf43bb5d022afd39a0c5f6ec4601e353631cdd737884c951",
      Hemisphere.Left,
      SurfaceKind.Pial
    ),
    CorticalMorphCorpusEntry(
      "lh.inflated.gii",
      "5a6d3f1fc87ab588133bc1656ef2e30d8cdbd89a45effeba6a3bf210433f0520",
      Hemisphere.Left,
      SurfaceKind.Inflated
    ),
    CorticalMorphCorpusEntry(
      "rh.white.gii",
      "7e448f16d6deb90b954c671540bcf41e6ef3c35c9a87cc6fc64672dff7e9e232",
      Hemisphere.Right,
      SurfaceKind.White
    ),
    CorticalMorphCorpusEntry(
      "rh.pial.gii",
      "6142dfcec7533aaeb962b1de829e21948bb35c85645a08f2c4b7ea503d655ee9",
      Hemisphere.Right,
      SurfaceKind.Pial
    ),
    CorticalMorphCorpusEntry(
      "rh.inflated.gii",
      "26abe85f568b4cd63dfa87e0e9d52d3cc473c086e3b3bc1e0415ddf32823f208",
      Hemisphere.Right,
      SurfaceKind.Inflated
    )
  )

  val Fractions: Vector[SurfaceMorphFraction] =
    Vector(0.0, 0.25, 0.5, 0.75, 1.0).map(SurfaceMorphFraction.unsafe)

  val Transitions: Vector[(SurfaceKind, SurfaceKind)] = Vector(
    SurfaceKind.White -> SurfaceKind.Pial,
    SurfaceKind.Pial -> SurfaceKind.Inflated,
    SurfaceKind.Inflated -> SurfaceKind.Sphere
  )

  def build(
    leftWhite: SurfaceGeometry,
    leftPial: SurfaceGeometry,
    leftInflated: SurfaceGeometry,
    rightWhite: SurfaceGeometry,
    rightPial: SurfaceGeometry,
    rightInflated: SurfaceGeometry
  ): Either[SurfaceViewError, SurfaceViewerExample] =
    CorticalSurfaceAcceptance.build(
      family(leftWhite, leftPial, leftInflated),
      family(rightWhite, rightPial, rightInflated)
    )

  def cases(example: SurfaceViewerExample): Either[SurfaceViewError, Vector[CorticalMorphCase]] =
    val builder = Vector.newBuilder[CorticalMorphCase]
    var transition = 0
    while transition < Transitions.length do
      val (from, to) = Transitions(transition)
      var fraction = 0
      while fraction < Fractions.length do
        val amount = Fractions(fraction)
        plan(example, from, to, amount) match
          case Left(error) => return Left(error)
          case Right(value) => builder += CorticalMorphCase(from, to, amount, value)
        fraction += 1
      transition += 1
    Right(builder.result())

  def sphere(source: SurfaceGeometry, radius: Double = 70.0): SurfaceGeometry =
    require(radius.isFinite && radius > 0.0, "sphere radius must be finite and positive")
    val coordinates = source.mesh.coordinates
    var centerX = 0.0
    var centerY = 0.0
    var centerZ = 0.0
    var offset = 0
    while offset < coordinates.length do
      centerX += coordinates(offset)
      centerY += coordinates(offset + 1)
      centerZ += coordinates(offset + 2)
      offset += 3
    centerX /= source.vertexCount.toDouble
    centerY /= source.vertexCount.toDouble
    centerZ /= source.vertexCount.toDouble

    val projected = new Array[Double](coordinates.length)
    offset = 0
    while offset < coordinates.length do
      val x = coordinates(offset) - centerX
      val y = coordinates(offset + 1) - centerY
      val z = coordinates(offset + 2) - centerZ
      val length = math.sqrt(x * x + y * y + z * z)
      require(length > 0.0, s"vertex ${offset / 3} lies at the family center")
      val scale = radius / length
      projected(offset) = centerX + x * scale
      projected(offset + 1) = centerY + y * scale
      projected(offset + 2) = centerZ + z * scale
      offset += 3

    val indices = new Array[Int](source.mesh.faceIndices.length)
    var index = 0
    while index < indices.length do
      indices(index) = source.mesh.faceIndices(index)
      index += 1
    SurfaceGeometry(
      TriangleMesh.fromArrays(projected, indices),
      source.hemisphere,
      SurfaceKind.Sphere,
      source.surfaceToWorld
    )

  private def family(
    white: SurfaceGeometry,
    pial: SurfaceGeometry,
    inflated: SurfaceGeometry
  ): SurfaceSet =
    require(white.kind == SurfaceKind.White, s"expected white geometry; got ${white.kind.label}")
    require(pial.kind == SurfaceKind.Pial, s"expected pial geometry; got ${pial.kind.label}")
    require(inflated.kind == SurfaceKind.Inflated, s"expected inflated geometry; got ${inflated.kind.label}")
    SurfaceSet.of(
      SurfaceKind.Pial,
      pial,
      SurfaceKind.White -> white,
      SurfaceKind.Inflated -> inflated,
      SurfaceKind.Sphere -> sphere(inflated)
    )

  private def plan(
    example: SurfaceViewerExample,
    from: SurfaceKind,
    to: SurfaceKind,
    fraction: SurfaceMorphFraction
  ): Either[SurfaceViewError, SurfaceRenderPlan] =
    val ids = Vector(SurfaceViewerExample.LeftSurface, SurfaceViewerExample.RightSurface)
    val fixed = ids.foldLeft[Either[SurfaceViewError, SurfaceViewerState]](Right(example.state)):
      (current, id) => current.flatMap(SurfaceViewer.reduce(example.model, _, SurfaceViewerAction.SetGeometryState(id, from)))
    val started = ids.foldLeft(fixed):
      (current, id) => current.flatMap(SurfaceViewer.reduce(example.model, _, SurfaceViewerAction.BeginGeometryMorph(id, to)))
    val advanced = ids.foldLeft(started):
      (current, id) => current.flatMap(
        SurfaceViewer.reduce(example.model, _, SurfaceViewerAction.SetGeometryMorphFraction(id, fraction))
      )
    advanced.flatMap(SurfaceCompiler.compile(example.model, _))
