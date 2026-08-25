package scalafim.surface.io

import scalafim.surface.*

object DcbcSurfaceOracle:
  val ResourceName = "/surface/oracle/fs_LR.32k.L.midthickness.surf.gii.gz"
  val RepositoryRelativePath =
    "modules/surface/shared/src/test/resources/surface/oracle/fs_LR.32k.L.midthickness.surf.gii.gz"
  val ContainerBytes = 545150
  val ContainerSha256 = "8928304299d5974b1a8112755895b04bfdbda023bb3191a53ecd79256224a590"
  val CoordinateSha256 = "82753b5d52a9cb1f1e85ef4ca9a929ccd524ba18fa4b7463ac75289b39a2bc1d"
  val FaceSha256 = "deed589d455f57561f56f699dd6c999a06692738bd08f719d1607efab63606df"

  private val CoordinateSamples = Vector(
    0 -> Point3D(-4.590286731719971, -43.81596755981445, 33.267940521240234),
    1 -> Point3D(-18.42593765258789, -40.85874557495117, 66.55389404296875),
    42 -> Point3D(-0.4739222526550293, -13.160460472106934, 18.509164810180664),
    8191 -> Point3D(-48.41954803466797, -17.208614349365234, 40.72233963012695),
    16384 -> Point3D(-44.0891227722168, -64.20576477050781, 36.2178840637207),
    32491 -> Point3D(-59.53241729736328, -46.70512390136719, -7.482700824737549)
  )

  private val FaceSamples = Vector(
    0 -> (68, 12, 0),
    1 -> (180, 12, 68),
    42 -> (200, 33, 32),
    32767 -> (16779, 16725, 16778),
    64979 -> (9, 8440, 21432)
  )

  def failures(geometry: SurfaceGeometry): Vector[String] =
    val found = Vector.newBuilder[String]
    def check(condition: Boolean, message: => String): Unit =
      if !condition then found += message
    def near(actual: Double, expected: Double, tolerance: Double, label: String): Unit =
      check(math.abs(actual - expected) <= tolerance, s"$label: expected $expected, found $actual")

    check(geometry.vertexCount == 32492, s"vertex count: ${geometry.vertexCount}")
    check(geometry.faceCount == 64980, s"face count: ${geometry.faceCount}")
    check(geometry.hemisphere == Hemisphere.Left, s"hemisphere: ${geometry.hemisphere}")
    check(geometry.kind == SurfaceKind.Midthickness, s"kind: ${geometry.kind}")
    check(geometry.mesh.realization.topology eq geometry.mesh.topology, "realization has a foreign topology owner")
    check(geometry.mesh.topology.isConnected, "topology is disconnected")
    check(geometry.mesh.topology.isClosed, "topology is open")
    check(geometry.mesh.topology.eulerCharacteristic == 2, s"Euler characteristic: ${geometry.mesh.topology.eulerCharacteristic}")

    CoordinateSamples.foreach: (index, expected) =>
      val actual = geometry.mesh.vertex(VertexId.unsafe(index))
      near(actual.x, expected.x, 1e-9, s"vertex $index x")
      near(actual.y, expected.y, 1e-9, s"vertex $index y")
      near(actual.z, expected.z, 1e-9, s"vertex $index z")

    FaceSamples.foreach: (index, expected) =>
      val actual = geometry.mesh.face(FaceId.unsafe(index))
      check(
        (actual.a.index, actual.b.index, actual.c.index) == expected,
        s"face $index: expected $expected, found ${(actual.a.index, actual.b.index, actual.c.index)}"
      )

    var minimumX = Double.PositiveInfinity
    var minimumY = Double.PositiveInfinity
    var minimumZ = Double.PositiveInfinity
    var maximumX = Double.NegativeInfinity
    var maximumY = Double.NegativeInfinity
    var maximumZ = Double.NegativeInfinity
    var sumX = 0.0
    var sumY = 0.0
    var sumZ = 0.0
    var vertex = 0
    while vertex < geometry.vertexCount do
      val point = geometry.mesh.vertex(VertexId.unsafe(vertex))
      minimumX = math.min(minimumX, point.x)
      minimumY = math.min(minimumY, point.y)
      minimumZ = math.min(minimumZ, point.z)
      maximumX = math.max(maximumX, point.x)
      maximumY = math.max(maximumY, point.y)
      maximumZ = math.max(maximumZ, point.z)
      sumX += point.x
      sumY += point.y
      sumZ += point.z
      vertex += 1

    Vector(
      (minimumX, -66.0409927368164, "minimum x"),
      (minimumY, -102.38394165039062, "minimum y"),
      (minimumZ, -45.413211822509766, "minimum z"),
      (maximumX, 0.6730416417121887, "maximum x"),
      (maximumY, 68.10617065429688, "maximum y"),
      (maximumZ, 76.07806396484375, "maximum z")
    ).foreach:
      case (actual, expected, label) => near(actual, expected, 1e-9, label)
    Vector(
      (sumX, -935399.9494784717, "sum x"),
      (sumY, -703251.0416978241, "sum y"),
      (sumZ, 612579.9932797732, "sum z")
    ).foreach:
      case (actual, expected, label) => near(actual, expected, 1e-5, label)

    var row = 0
    while row < 4 do
      var column = 0
      while column < 4 do
        val expected = if row == column then 1.0 else 0.0
        near(geometry.surfaceToWorld(row, column), expected, 0.0, s"affine ($row,$column)")
        column += 1
      row += 1
    found.result()
