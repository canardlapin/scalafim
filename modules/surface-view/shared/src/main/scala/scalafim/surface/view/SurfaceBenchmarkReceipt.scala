package scalafim.surface.view

enum SurfaceRuntimePlatform:
  case Jvm, ScalaJs

final case class SurfaceRuntimeMetadata(
  platform: SurfaceRuntimePlatform,
  runtimeName: String,
  runtimeVersion: String,
  operatingSystem: String,
  architecture: String,
  graphicsApi: String,
  graphicsDevice: String
)

final case class SurfaceBenchmarkCase private (
  vertices: Int,
  layers: Int,
  path: SurfaceAdmissionPath
)

object SurfaceBenchmarkCase:
  def make(vertices: Int, layers: Int, path: SurfaceAdmissionPath): Either[String, SurfaceBenchmarkCase] =
    if vertices <= 0 then Left(s"benchmark vertex count must be positive; got $vertices")
    else if layers <= 0 then Left(s"benchmark layer count must be positive; got $layers")
    else Right(new SurfaceBenchmarkCase(vertices, layers, path))

  def unsafe(vertices: Int, layers: Int, path: SurfaceAdmissionPath): SurfaceBenchmarkCase =
    make(vertices, layers, path).fold(error => throw new IllegalArgumentException(error), identity)

final case class SurfaceTimingSummary private (
  phase: SurfaceRenderPhase,
  samples: Int,
  minimumNanos: Long,
  p50Nanos: Long,
  p95Nanos: Long,
  maximumNanos: Long
)

object SurfaceTimingSummary:
  def from(phase: SurfaceRenderPhase, samples: IndexedSeq[Long]): Either[String, SurfaceTimingSummary] =
    if samples.isEmpty then Left(s"$phase timing summary requires at least one sample")
    else if samples.exists(_ < 0L) then Left(s"$phase timing summary contains a negative sample")
    else
      val sorted = samples.sorted
      Right(new SurfaceTimingSummary(
        phase,
        sorted.length,
        sorted.head,
        percentile(sorted, 0.50),
        percentile(sorted, 0.95),
        sorted.last
      ))

  private def percentile(sorted: IndexedSeq[Long], probability: Double): Long =
    sorted(math.min(sorted.length - 1, math.ceil(probability * sorted.length).toInt - 1))

final case class SurfaceBenchmarkReceipt(
  benchmark: SurfaceBenchmarkCase,
  metadata: SurfaceRuntimeMetadata,
  observations: Vector[SurfaceBackendObservation],
  timings: Vector[SurfaceTimingSummary]
)

object SurfaceBenchmarkMatrix:
  val VertexCounts: Vector[Int] = Vector(32768, 163842)
  val LayerCounts: Vector[Int] = Vector(1, 4, 8)
  val Paths: Vector[SurfaceAdmissionPath] = Vector(
    SurfaceAdmissionPath.ColdLoad,
    SurfaceAdmissionPath.CameraOnly,
    SurfaceAdmissionPath.StyleUpdate,
    SurfaceAdmissionPath.LayerDataUpdate,
    SurfaceAdmissionPath.TimepointUpdate,
    SurfaceAdmissionPath.Pick,
    SurfaceAdmissionPath.Resize,
    SurfaceAdmissionPath.Snapshot
  )

  val Cases: Vector[SurfaceBenchmarkCase] =
    for
      vertices <- VertexCounts
      layers <- LayerCounts
      path <- Paths
    yield SurfaceBenchmarkCase.unsafe(vertices, layers, path)

object SurfaceBenchmarkJson:
  def encode(receipt: SurfaceBenchmarkReceipt): String =
    val observations = receipt.observations.map(encodeObservation).mkString("[", ",", "]")
    val timings = receipt.timings.map: timing =>
      s"""{"phase":${quoted(timing.phase.toString)},"samples":${timing.samples},"minimumNanos":${timing.minimumNanos},"p50Nanos":${timing.p50Nanos},"p95Nanos":${timing.p95Nanos},"maximumNanos":${timing.maximumNanos}}"""
    .mkString("[", ",", "]")
    val benchmark = receipt.benchmark
    val metadata = receipt.metadata
    s"""{"schema":"scalafim.surface-benchmark.v1","benchmark":{"vertices":${benchmark.vertices},"layers":${benchmark.layers},"path":${quoted(benchmark.path.toString)}},"metadata":{"platform":${quoted(metadata.platform.toString)},"runtimeName":${quoted(metadata.runtimeName)},"runtimeVersion":${quoted(metadata.runtimeVersion)},"operatingSystem":${quoted(metadata.operatingSystem)},"architecture":${quoted(metadata.architecture)},"graphicsApi":${quoted(metadata.graphicsApi)},"graphicsDevice":${quoted(metadata.graphicsDevice)}},"observations":$observations,"timings":$timings}"""

  def encodeObservation(observation: SurfaceBackendObservation): String =
    val features = observation.capabilities.features.toVector.map(_.toString).sorted.map(quoted).mkString("[", ",", "]")
    val caveats = observation.capabilities.caveats.map(quoted).mkString("[", ",", "]")
    val events = observation.events.map(encodeEvent).mkString("[", ",", "]")
    val timings = observation.timings.map: timing =>
      s"""{"phase":${quoted(timing.phase.toString)},"elapsedNanos":${timing.elapsedNanos}}"""
    .mkString("[", ",", "]")
    s"""{"backend":${quoted(observation.capabilities.id.value)},"revision":${observation.revision.value},"path":${quoted(observation.path.toString)},"features":$features,"caveats":$caveats,"geometryUploads":${observation.geometryUploads},"layerUploads":${observation.layerUploads},"uploadedBytes":${observation.uploadedBytes},"drawCalls":${observation.drawCalls},"events":$events,"timings":$timings}"""

  private def encodeEvent(event: SurfaceResourceEvent): String =
    event match
      case SurfaceResourceEvent.MeshUploaded(key, vertices, triangles, bytes) =>
        s"""{"kind":"MeshUploaded","key":${quoted(key.value)},"vertices":$vertices,"triangles":$triangles,"bytes":$bytes}"""
      case SurfaceResourceEvent.LayerUploaded(key, values, width, height, bytes) =>
        s"""{"kind":"LayerUploaded","key":${quoted(key.value)},"values":$values,"width":$width,"height":$height,"bytes":$bytes}"""
      case SurfaceResourceEvent.CacheHit(key) =>
        s"""{"kind":"CacheHit","key":${quoted(key.value)}}"""
      case SurfaceResourceEvent.ResourcesDisposed(keys) =>
        val encoded = keys.map(key => quoted(key.value)).mkString("[", ",", "]")
        s"""{"kind":"ResourcesDisposed","keys":$encoded}"""
      case SurfaceResourceEvent.DrawSubmitted(drawCalls, triangles) =>
        s"""{"kind":"DrawSubmitted","drawCalls":$drawCalls,"triangles":$triangles}"""
      case SurfaceResourceEvent.Resized(width, height) =>
        s"""{"kind":"Resized","width":$width,"height":$height}"""
      case SurfaceResourceEvent.Picked(surface, face, vertex) =>
        s"""{"kind":"Picked","surface":${quoted(surface.value)},"face":$face,"vertex":$vertex}"""

  private def quoted(value: String): String =
    val out = new StringBuilder(value.length + 2)
    out += '"'
    value.foreach:
      case '"' => out ++= "\\\""
      case '\\' => out ++= "\\\\"
      case '\b' => out ++= "\\b"
      case '\f' => out ++= "\\f"
      case '\n' => out ++= "\\n"
      case '\r' => out ++= "\\r"
      case '\t' => out ++= "\\t"
      case character if character < ' ' => out ++= f"\\u${character.toInt}%04x"
      case character => out += character
    out += '"'
    out.result()
