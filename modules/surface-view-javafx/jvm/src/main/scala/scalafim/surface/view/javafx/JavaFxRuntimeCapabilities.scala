package scalafim.surface.view.javafx

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Paths}
import java.security.MessageDigest

import javafx.beans.Observable

import scala.util.Try

enum JavaFxPrismPipeline(val id: String):
  case Metal extends JavaFxPrismPipeline("mtl")
  case Direct3d extends JavaFxPrismPipeline("d3d")
  case Es2 extends JavaFxPrismPipeline("es2")
  case Software extends JavaFxPrismPipeline("sw")
  case Unknown extends JavaFxPrismPipeline("unknown")

object JavaFxPrismPipeline:
  private val Initialized = """(?m)^Initialized prism pipeline:\s+([A-Za-z0-9_.$]+)\s*$""".r

  def observe(verboseLog: String): Either[String, (JavaFxPrismPipeline, String)] =
    Initialized.findAllMatchIn(verboseLog).map(_.group(1)).toVector.distinct match
      case Vector(className) => Right((fromClassName(className), className))
      case Vector()          => Left("Prism verbose log contains no initialized pipeline")
      case classes => Left(s"Prism verbose log contains multiple initialized pipelines: ${classes.mkString(", ")}")

  private def fromClassName(className: String): JavaFxPrismPipeline =
    className match
      case "com.sun.prism.mtl.MTLPipeline" => JavaFxPrismPipeline.Metal
      case "com.sun.prism.d3d.D3DPipeline" => JavaFxPrismPipeline.Direct3d
      case "com.sun.prism.es2.ES2Pipeline" => JavaFxPrismPipeline.Es2
      case "com.sun.prism.sw.SWPipeline"   => JavaFxPrismPipeline.Software
      case _                               => JavaFxPrismPipeline.Unknown

final case class JavaFxArtifactReceipt(
    component: String,
    module: String,
    implementationVersion: Option[String],
    codeSource: Option[String],
    sha256: Option[String]
)

final case class JavaFxRuntimeCapabilityReceipt(
    javaFxRuntimeVersion: String,
    javaRuntimeName: String,
    javaRuntimeVersion: String,
    osName: String,
    osVersion: String,
    osArch: String,
    requestedPipelines: Vector[String],
    noFallback: Boolean,
    verbose: Boolean,
    observedPipeline: JavaFxPrismPipeline,
    observedPipelineClass: String,
    relevantJvmArguments: Vector[String],
    moduleMutationArguments: Vector[String],
    artifacts: Vector[JavaFxArtifactReceipt]
):
  def fallbackUsed: Boolean = requestedPipelines.headOption.exists(_ != observedPipeline.id)

  /** Runtime and packaging preflight only. An empty result does not establish colour, visual, interaction, or
    * resource qualification. Every failure is returned so a receipt cannot hide another packaging error.
    */
  def stockMetalFailures(
      expectedJavaFxVersion: String,
      expectedArtifactSha256: Map[String, String]
  ): Vector[String] =
    val requiredArtifacts = Set("javafx-base", "javafx-graphics")
    val observedComponents = artifacts.map(_.component)
    val artifactFailures = Vector(
      Option.when(expectedArtifactSha256.keySet != requiredArtifacts)(
        s"expected artifact checksums must name exactly ${requiredArtifacts.toVector.sorted.mkString(",")}, found ${expectedArtifactSha256.keySet.toVector.sorted.mkString(",")}"
      ),
      Option.when(
        observedComponents.toSet != requiredArtifacts || observedComponents.distinct.size != observedComponents.size
      )(
        s"runtime artifacts must contain exactly one javafx-base and one javafx-graphics, found ${observedComponents.mkString(",")}"
      )
    ).flatten ++ requiredArtifacts.toVector.sorted.flatMap: component =>
      for
        expected <- expectedArtifactSha256.get(component).toVector
        artifact <- artifacts.find(_.component == component).toVector
        failure <- Vector(
          Option.when(artifact.module != component.replace('-', '.'))(
            s"$component loaded from module ${artifact.module}, not ${component.replace('-', '.')}"
          ),
          Option.when(artifact.codeSource.isEmpty)(s"$component has no observable code source"),
          Option.when(artifact.sha256 != Some(expected))(
            s"$component SHA-256 ${artifact.sha256.getOrElse("<unavailable>")} does not match expected $expected"
          )
        ).flatten
      yield failure
    Vector(
      Option.when(javaFxRuntimeVersion != expectedJavaFxVersion)(
        s"JavaFX runtime $javaFxRuntimeVersion does not match expected $expectedJavaFxVersion"
      ),
      Option.when(!osName.startsWith("Mac OS"))(s"stock Metal qualification requires macOS, found $osName"),
      Option.when(osArch != "aarch64")(s"stock Metal qualification requires macOS arm64, found $osArch"),
      Option.when(requestedPipelines.headOption != Some("mtl"))(
        s"Metal is not the first requested Prism pipeline: ${requestedPipelines.mkString(",")}"
      ),
      Option.when(!noFallback)("prism.noFallback must be true"),
      Option.when(!verbose)("prism.verbose must be true so the active pipeline is observable"),
      Option.when(observedPipeline != JavaFxPrismPipeline.Metal)(
        s"active Prism pipeline is $observedPipelineClass, not MTLPipeline"
      ),
      Option.when(fallbackUsed)(
        s"Prism fell back from ${requestedPipelines.headOption.getOrElse("<none>")} to ${observedPipeline.id}"
      ),
      Option.when(moduleMutationArguments.nonEmpty)(
        s"module mutation is active: ${moduleMutationArguments.mkString(" ")}"
      )
    ).flatten ++ artifactFailures

  def toJson: String =
    def escaped(value: String): String =
      value.flatMap:
        case '"'                    => "\\\""
        case '\\'                   => "\\\\"
        case '\n'                   => "\\n"
        case '\r'                   => "\\r"
        case '\t'                   => "\\t"
        case char if char.isControl => f"\\u${char.toInt}%04x"
        case char                   => char.toString
    def quoted(value: String): String = s"\"${escaped(value)}\""
    def optional(value: Option[String]): String = value.fold("null")(quoted)
    def strings(values: Vector[String]): String = values.map(quoted).mkString("[", ",", "]")
    val artifactJson = artifacts.map: artifact =>
      s"""{"component":${quoted(artifact.component)},"module":${quoted(
          artifact.module
        )},"implementationVersion":${optional(artifact.implementationVersion)},"codeSource":${optional(
          artifact.codeSource
        )},"sha256":${optional(artifact.sha256)}}"""
    s"""{"javaFxRuntimeVersion":${quoted(javaFxRuntimeVersion)},"javaRuntimeName":${quoted(
        javaRuntimeName
      )},"javaRuntimeVersion":${quoted(javaRuntimeVersion)},"osName":${quoted(osName)},"osVersion":${quoted(
        osVersion
      )},"osArch":${quoted(osArch)},"requestedPipelines":${strings(
        requestedPipelines
      )},"noFallback":$noFallback,"verbose":$verbose,"observedPipeline":${quoted(
        observedPipeline.id
      )},"observedPipelineClass":${quoted(
        observedPipelineClass
      )},"fallbackUsed":$fallbackUsed,"relevantJvmArguments":${strings(
        relevantJvmArguments
      )},"moduleMutationArguments":${strings(moduleMutationArguments)},"artifacts":${artifactJson.mkString(
        "[",
        ",",
        "]"
      )}}"""

object JavaFxRuntimeCapabilityReceipt:
  def current(verboseLog: String): Either[String, JavaFxRuntimeCapabilityReceipt] =
    JavaFxPrismPipeline
      .observe(verboseLog)
      .map: (pipeline, pipelineClass) =>
        val inputArguments = ManagementFactory.getRuntimeMXBean.getInputArguments.toArray(new Array[String](0)).toVector
        val relevant = inputArguments.filter: argument =>
          argument.startsWith("-Dprism.") || argument.startsWith("--patch-module") ||
            argument.startsWith("--add-exports") || argument.startsWith("--add-opens") ||
            argument.startsWith("--enable-native-access")
        val mutations = relevant.filter: argument =>
          argument.startsWith("--patch-module") || argument.startsWith("--add-exports") || argument.startsWith(
            "--add-opens"
          )
        JavaFxRuntimeCapabilityReceipt(
          javaFxRuntimeVersion = Option(System.getProperty("javafx.runtime.version"))
            .orElse(Option(classOf[_root_.javafx.scene.Scene].getPackage.getImplementationVersion))
            .getOrElse("unknown"),
          javaRuntimeName = System.getProperty("java.runtime.name", "unknown"),
          javaRuntimeVersion = System.getProperty("java.runtime.version", "unknown"),
          osName = System.getProperty("os.name", "unknown"),
          osVersion = System.getProperty("os.version", "unknown"),
          osArch = System.getProperty("os.arch", "unknown"),
          requestedPipelines = System.getProperty("prism.order", "").split(',').toVector.map(_.trim).filter(_.nonEmpty),
          noFallback = java.lang.Boolean.getBoolean("prism.noFallback"),
          verbose = java.lang.Boolean.getBoolean("prism.verbose"),
          observedPipeline = pipeline,
          observedPipelineClass = pipelineClass,
          relevantJvmArguments = relevant,
          moduleMutationArguments = mutations,
          artifacts = Vector(
            artifact("javafx-base", classOf[Observable]),
            artifact("javafx-graphics", classOf[_root_.javafx.scene.Scene])
          )
        )

  private def artifact(component: String, clazz: Class[?]): JavaFxArtifactReceipt =
    val location = Option(clazz.getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
    JavaFxArtifactReceipt(
      component = component,
      module = Option(clazz.getModule.getName).getOrElse("unnamed"),
      implementationVersion = Option(clazz.getPackage.getImplementationVersion),
      codeSource = location.map(_.toExternalForm),
      sha256 = location
        .filter(_.getProtocol == "file")
        .flatMap: url =>
          Try:
            val digest = MessageDigest.getInstance("SHA-256")
            val input = Files.newInputStream(Paths.get(url.toURI))
            try
              val buffer = new Array[Byte](64 * 1024)
              var read = input.read(buffer)
              while read >= 0 do
                if read > 0 then digest.update(buffer, 0, read)
                read = input.read(buffer)
              digest.digest().map(byte => f"${byte & 0xff}%02x").mkString
            finally input.close()
          .toOption
    )

enum JavaFxStockMetalRefusal:
  case Runtime(failures: Vector[String])
  case UnqualifiedClosure(javaFxVersion: String)

  def message: String = this match
    case Runtime(failures) => failures.mkString("stock Metal runtime refusal: ", "; ", "")
    case UnqualifiedClosure(version) => s"stock JavaFX $version Metal has no complete product qualification"

/** No public constructor can mint product approval from Scene3D availability or a runtime-only receipt. */
final class JavaFxStockMetalApproval private ()

object JavaFxStockMetalAdmission:
  def assess(
      receipt: JavaFxRuntimeCapabilityReceipt,
      expectedJavaFxVersion: String,
      expectedArtifactSha256: Map[String, String]
  ): Either[JavaFxStockMetalRefusal, JavaFxStockMetalApproval] =
    val failures = receipt.stockMetalFailures(expectedJavaFxVersion, expectedArtifactSha256)
    if failures.nonEmpty then Left(JavaFxStockMetalRefusal.Runtime(failures))
    else Left(JavaFxStockMetalRefusal.UnqualifiedClosure(receipt.javaFxRuntimeVersion))
