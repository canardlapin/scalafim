package scalafim.fmri.mvpa.artifacts

import gale.linalg.DMat
import java.nio.file.Path
import multivar.core.SpaceRole
import scalafim.archive.io.LocalObjectStore
import scalafim.estimates.FileReference
import scalafim.fmri.mvpa.AxisRef
import scalafim.fmri.mvpa.pattern.*

/** Fresh-JVM producer/consumer probe that reopens external payloads. */
object PatternArchiveProcessProbe:
  private def right[A](value: Either[?, A]): A = value.fold(e => throw new IllegalStateException(e.toString), identity)
  private def axis(name: String, keys: Vector[String]) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed, keys, "coordinate", "one", "one"))
  private def trainingAxis = axis("training", Vector("s1", "s2"))
  private def fixture =
    val neural = axis("neural", Vector("n1", "n2"))
    val target = axis("target", Vector("q1"))
    val components = axis("components", Vector("r1"))
    val training = trainingAxis
    val factors = right(PatternFactors(neural, target, components, DMat.dense(2, 1, Vector(-0.0, 2.0)), DMat.dense(1, 1, Vector(3.0)), GaugeEvidence.PendingNumericalCheck))
    val geometry = right(TargetGeometry.continuous(target, right(AxisValues(target, Vector(1.0))), right(AxisValues(target, Vector(1.0))), Vector("all" -> right(AxisValues(target, Vector(1.0))))))
    val artifact = right(PatternArtifact(factors, geometry, CenteringPolicy.ExplicitIntercept(right(AxisValues(neural, Vector(10.0, 20.0))), "target-centered"), DegenerateTargetPolicy.RecordExperimental("probe"), ResidualCovarianceCapability.NotFitted, right(TrainingBinding(training.descriptor, "probe", "fingerprint")), Vector("probe-training"), right(PatternFitDiagnostics(Vector(2.0), "probe", Vector.empty))))
    val declaration = PatternScientificDeclaration("plan", "measurement", "value", Vector("source" -> "revision"), Long.MaxValue, "reducer", Vector("u"), Vector("u"))
    (declaration, artifact, training)
  def main(args: Array[String]): Unit =
    require(args.length == 2, "write|read root")
    val root = Path.of(args(1))
    if args(0) == "write" then
      val (declaration, artifact, training) = fixture
      right(right(LocalPatternArchive.open(root)).write("probe", declaration, artifact, training))
      println("WRITE_PASS")
    else
      require(args(0) == "read", "write|read")
      val training = trainingAxis
      val objects = right(LocalObjectStore.open(root))
      val objectValue = right(objects.inspect("patterns/probe/profile.json"))
      val ref = FileReference(objectValue.path, objectValue.digest, objectValue.bytes)
      val restored = right(right(LocalPatternArchive.open(root)).read(ref, training))
      val values = right(restored.artifact.forwardMean(right(AxisValues(restored.artifact.factors.targetAxis, Vector(2.0))))).values
      require(values == Vector(10.0, 32.0), "scalar artifact values differ")
      require(restored.profile.declaration.seed == Long.MaxValue, "seed lost precision")
      require(restored.artifact.interpretation == InterpretationStatus.ExperimentalFitOnly, "interpretation differs")
      println("READ_PASS readWithoutFit=true seedExact=true")
