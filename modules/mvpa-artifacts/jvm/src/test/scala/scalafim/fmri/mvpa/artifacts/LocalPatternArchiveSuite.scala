package scalafim.fmri.mvpa.artifacts

import gale.linalg.DMat
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Files
import multivar.core.SpaceRole
import scalafim.archive.io.LocalObjectStore
import scalafim.estimates.FileReference
import scalafim.fmri.mvpa.AxisRef
import scalafim.fmri.mvpa.pattern.*

class LocalPatternArchiveSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(e => fail(e.toString), identity)
  private def axis(name: String, keys: Vector[String]) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed, keys, "coordinate", "one", "one"))
  private def fixture =
    val neural = axis("neural", Vector("n1", "n2"))
    val target = axis("target", Vector("q1"))
    val components = axis("components", Vector("r1"))
    val training = axis("training", Vector("s1", "s2"))
    val factors = right(PatternFactors(neural, target, components, DMat.dense(2, 1, Vector(-0.0, 2.0)), DMat.dense(1, 1, Vector(3.0)), GaugeEvidence.PendingNumericalCheck))
    val geometry = right(TargetGeometry.continuous(target, right(AxisValues(target, Vector(1.0))), right(AxisValues(target, Vector(1.0))), Vector("all" -> right(AxisValues(target, Vector(1.0))))))
    val binding = right(TrainingBinding(training.descriptor, "synthetic", "abc"))
    val diagnostics = right(PatternFitDiagnostics(Vector(3.0, 2.0), "synthetic", Vector("fixture")))
    val artifact = right(PatternArtifact(factors, geometry, CenteringPolicy.ExplicitIntercept(right(AxisValues(neural, Vector(10.0, 20.0))), "target-centered"), DegenerateTargetPolicy.RecordExperimental("fixture"), ResidualCovarianceCapability.NotFitted, binding, Vector("training-v1"), diagnostics))
    val declaration = PatternScientificDeclaration("plan-v1", "measurement-v1", "value-v1", Vector("source" -> "r1"), 7L, "reducer-v1", Vector("u1"), Vector("u1"))
    (declaration, artifact, training)

  private def categoricalFixture =
    val neural = axis("cat-neural", Vector("n1", "n2"))
    val target = axis("cat-target", Vector("q1"))
    val components = axis("cat-components", Vector("r1"))
    val conditions = axis("cat-conditions", Vector("c1", "c2"))
    val training = axis("cat-training", Vector("s1", "s2"))
    val factors = right(PatternFactors(neural, target, components, DMat.dense(2, 1, Vector(1.0, 2.0)), DMat.dense(1, 1, Vector(1.0)), GaugeEvidence.PendingNumericalCheck))
    val geometry = right(TargetGeometry.categorical(conditions, target, DMat.dense(2, 1, Vector(1.0, -1.0)), right(AxisValues(conditions, Vector(0.5, 0.5)))))
    val artifact = right(PatternArtifact(factors, geometry, CenteringPolicy.CenteredBeforeFit("neural-center", "target-center"), DegenerateTargetPolicy.Refuse, ResidualCovarianceCapability.NotFitted, right(TrainingBinding(training.descriptor, "categorical", "digest")), Vector("cat-training"), right(PatternFitDiagnostics(Vector(1.0), "categorical", Vector.empty))))
    val declaration = PatternScientificDeclaration("cat-plan", "cat-measurement", "cat-value", Vector("cat-source" -> "r1"), 9L, "cat-reducer", Vector("u1", "u2"), Vector("u1", "u2"))
    (declaration, artifact, training)

  test("writer publishes verified external matrices then restores all supported policies"):
    val root = Files.createTempDirectory("scalafim-pattern-profile-")
    val archive = right(LocalPatternArchive.open(root))
    val (declaration, artifact, training) = fixture
    val profile = right(archive.write("fit-1", declaration, artifact, training))
    assert(Files.exists(root.resolve(profile.metadata.path)))
    assert(profile.payloads.forall(p => Files.exists(root.resolve(p.reference.path))))
    val a = profile.payloads.find(_.name == "A").get
    assertEquals(ByteBuffer.wrap(Files.readAllBytes(root.resolve(a.reference.path))).order(ByteOrder.LITTLE_ENDIAN).getLong(), java.lang.Double.doubleToRawLongBits(-0.0))
    val restored = right(archive.read(profile.metadata, training))
    assertEquals(restored.profile.declaration, declaration)
    assertEquals(CompletedPatternProfile.compatible(profile, restored.profile), Right(()))
    assertEquals(restored.artifact.interpretation, InterpretationStatus.ExperimentalFitOnly)
    assertEquals(right(restored.artifact.forwardMean(right(AxisValues(restored.artifact.factors.targetAxis, Vector(2.0))))).values, Vector(10.0, 32.0))

  test("corrupt payload is rejected before matrix allocation"):
    val root = Files.createTempDirectory("scalafim-pattern-corrupt-")
    val archive = right(LocalPatternArchive.open(root))
    val (declaration, artifact, training) = fixture
    val profile = right(archive.write("fit-1", declaration, artifact, training))
    val payload = profile.payloads.find(_.name == "A").get
    val file = new java.io.RandomAccessFile(root.resolve(payload.reference.path).toFile, "rw")
    try
      file.seek(0)
      file.writeByte(file.readByte() ^ 0x01)
    finally file.close()
    assert(archive.read(profile.metadata, training).isLeft)

  test("categorical centered artifacts round-trip their target policy and axes"):
    val root = Files.createTempDirectory("scalafim-pattern-categorical-")
    val archive = right(LocalPatternArchive.open(root))
    val (declaration, artifact, training) = categoricalFixture
    val profile = right(archive.write("categorical", declaration, artifact, training))
    val restored = right(archive.read(profile.metadata, training))
    assertEquals(CompletedPatternProfile.compatible(profile, restored.profile), Right(()))
    assertEquals(restored.artifact.centering, CenteringPolicy.CenteredBeforeFit("neural-center", "target-center"))
    restored.artifact.target match
      case TargetGeometry.Categorical(value) =>
        assertEquals(value.conditions.toRecord.stableKeys, Vector("c1", "c2"))
        assertEquals(value.priors.values, Vector(0.5, 0.5))
      case _ => fail("expected categorical target")

  test("aggregate budget, declared payload shape, and metadata hash are refused before reuse"):
    val root = Files.createTempDirectory("scalafim-pattern-rejections-")
    val (declaration, artifact, training) = fixture
    assert(LocalPatternArchive.open(root, PatternArchiveLimits(1024, 3)).flatMap(_.write("over-budget", declaration, artifact, training)).isLeft)
    val archive = right(LocalPatternArchive.open(root))
    val profile = right(archive.write("shape", declaration, artifact, training))
    val metadata = Files.readString(root.resolve(profile.metadata.path))
    val wrongShape = metadata.replaceFirst("\\\"Rows\\\": 2", "\\\"Rows\\\": 1")
    Files.writeString(root.resolve(profile.metadata.path), wrongShape)
    assert(archive.read(profile.metadata, training).isLeft)
    val altered = right(LocalObjectStore.open(root)).inspect(profile.metadata.path).fold(e => fail(e.message), identity)
    val alteredReference = FileReference(altered.path, altered.digest, altered.bytes)
    assert(archive.read(alteredReference, training).isLeft)

  test("caller cannot substitute a different training axis"):
    val root = Files.createTempDirectory("scalafim-pattern-training-")
    val archive = right(LocalPatternArchive.open(root))
    val (declaration, artifact, training) = fixture
    val profile = right(archive.write("fit-1", declaration, artifact, training))
    val foreign = axis("foreign", Vector("x1", "x2"))
    assert(archive.read(profile.metadata, foreign).isLeft)

  test("a fresh child JVM writes then reopens the artifact without a fitter"):
    val root = Files.createTempDirectory("scalafim-pattern-process-")
    val executable = java.nio.file.Path.of(System.getProperty("java.home"), "bin", "java").toString
    val classpath = System.getProperty("java.class.path")
    def run(mode: String): String =
      val process = new ProcessBuilder(executable, "-cp", classpath, "scalafim.fmri.mvpa.artifacts.PatternArchiveProcessProbe", mode, root.toString).redirectErrorStream(true).start()
      val output = new String(process.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
      assertEquals(process.waitFor(), 0, output)
      output
    assert(run("write").contains("WRITE_PASS"))
    assert(run("read").contains("READ_PASS readWithoutFit=true seedExact=true"))
