package scalafim.surface.view

import scalafim.surface.*

import scala.util.Random

class Mesh4sRendererLawSuite extends munit.FunSuite:

  test("generated camera changes reuse the exact renderer packet and mesh4s-derived index buffer"):
    val random = new Random(0x52454e444552L)
    var trial = 0
    while trial < 16 do
      val geometry = gridGeometry(3 + random.nextInt(7), 3 + random.nextInt(7), random, SurfaceKind.Pial)
      val surface = SurfaceId.unsafe(s"camera-law-$trial")
      val asset = SurfaceAsset.make(surface, geometry).toOption.get
      val model = SurfaceViewerModel.make(Vector(asset), Vector.empty).toOption.get
      val initial = SurfaceViewerState.initial(model)
      val before = SurfaceCompiler.compile(model, initial).toOption.get
      val moved = SurfaceViewer.reduce(
        model,
        initial,
        SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal)
      ).toOption.get
      val after = SurfaceCompiler.compile(model, moved).toOption.get
      assert(after.meshes.head eq before.meshes.head)
      assert(after.meshes.head.positions eq before.meshes.head.positions)
      assert(after.meshes.head.normals eq before.meshes.head.normals)
      assert(after.meshes.head.indices eq before.meshes.head.indices)
      assert(after.meshes.head.indices.unsafeArray.sameElements(geometry.mesh.faceIndices))
      trial += 1

  test("generated coordinate morphs replace positions while retaining one topology index rendition"):
    val random = new Random(0x4d4f525048L)
    var trial = 0
    while trial < 12 do
      val white = gridGeometry(3 + random.nextInt(6), 3 + random.nextInt(6), random, SurfaceKind.White)
      val coordinates = white.mesh.coordinates
      var offset = 0
      while offset < coordinates.length do
        coordinates(offset) += random.nextDouble() - 0.5
        coordinates(offset + 1) += random.nextDouble() - 0.5
        coordinates(offset + 2) += 0.5 + random.nextDouble()
        offset += 3
      val pial = SurfaceGeometry(
        white.mesh.withCoordinatesEither(coordinates).toOption.get,
        white.hemisphere,
        SurfaceKind.Pial,
        white.surfaceToWorld
      )
      val surface = SurfaceId.unsafe(s"morph-law-$trial")
      val family = SurfaceSet.of(SurfaceKind.White, white, SurfaceKind.Pial -> pial)
      val asset = SurfaceAsset.make(surface, family).toOption.get
      val model = SurfaceViewerModel.make(Vector(asset), Vector.empty).toOption.get
      val initial = SurfaceViewerState.initial(model)
      val cold = SurfaceCompiler.compile(model, initial).toOption.get
      val started = SurfaceViewer.reduce(
        model,
        initial,
        SurfaceViewerAction.BeginGeometryMorph(surface, SurfaceKind.Pial)
      ).toOption.get
      Vector(0.1, 0.25, 0.5, 0.75, 0.9).foreach: fraction =>
        val state = SurfaceViewer.reduce(
          model,
          started,
          SurfaceViewerAction.SetGeometryMorphFraction(surface, SurfaceMorphFraction.unsafe(fraction))
        ).toOption.get
        val plan = SurfaceCompiler.compile(model, state).toOption.get
        assert(plan.meshes.head.indices eq cold.meshes.head.indices)
        assertEquals(plan.meshes.head.resourceKey, cold.meshes.head.resourceKey)
        assert(!(plan.meshes.head.positions eq cold.meshes.head.positions))
      trial += 1

  private def gridGeometry(
    columns: Int,
    rows: Int,
    random: Random,
    kind: SurfaceKind
  ): SurfaceGeometry =
    val coordinates = new Array[Double](columns * rows * 3)
    var row = 0
    while row < rows do
      var column = 0
      while column < columns do
        val vertex = row * columns + column
        val offset = vertex * 3
        coordinates(offset) = column.toDouble
        coordinates(offset + 1) = row.toDouble
        coordinates(offset + 2) = random.nextDouble() * 0.2
        column += 1
      row += 1
    val faces = new Array[Int]((columns - 1) * (rows - 1) * 6)
    var offset = 0
    row = 0
    while row < rows - 1 do
      var column = 0
      while column < columns - 1 do
        val lowerLeft = row * columns + column
        val lowerRight = lowerLeft + 1
        val upperLeft = lowerLeft + columns
        val upperRight = upperLeft + 1
        faces(offset) = lowerLeft
        faces(offset + 1) = lowerRight
        faces(offset + 2) = upperLeft
        faces(offset + 3) = lowerRight
        faces(offset + 4) = upperRight
        faces(offset + 5) = upperLeft
        offset += 6
        column += 1
      row += 1
    SurfaceGeometry(
      TriangleMesh.fromArrays(coordinates, faces),
      Hemisphere.Left,
      kind
    )
