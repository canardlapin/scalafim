package scalafim.fmri.mvpa.artifacts

import gale.linalg.DMat
import java.nio.file.Files
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.AxisRef
import scalafim.fmri.mvpa.pattern.*

class PatternPredictionArchiveSuite extends munit.FunSuite:
  private def right[A](v: Either[?,A]): A = v.fold(e=>fail(e.toString),identity)
  private def axis(n:String, keys:Vector[String])=right(AxisRef.fromStableKeys(n,SpaceRole.Observed,keys,"fixture","one","raw"))
  private def id(n:String)=ValueIdentity.source(ValueId.unsafe(n))
  private def close(a:Double,b:Double)=assertEqualsDouble(a,b,1e-10)
  private def declaration=PatternScientificDeclaration("p","m","v",Vector("source"->"r"),1L,"reducer",Vector("u"),Vector("u"))

  test("continuous prediction survives an archive round trip when external covariance and Sigma are supplied again"):
    val n=axis("n",Vector("n1","n2")); val q=axis("q",Vector("q1","q2")); val r=axis("r",Vector("r1")); val s=axis("s",Vector("s1","s2"))
    val a=DMat.dense(2,1,Vector(1.0,2.0)); val c=DMat.dense(2,1,Vector(.6,.8)); val nonunit=right(AxisValues(q,Vector(2.0,3.0)))
    val geometry=right(TargetGeometry.continuous(q,nonunit,nonunit,Vector("block"->nonunit)))
    val factors=right(PatternFactors(n,q,r,a,c,GaugeEvidence.PendingNumericalCheck)); val offset=right(AxisValues(n,Vector(10.0,-4.0)))
    val artifact=right(PatternArtifact(factors,geometry,CenteringPolicy.ExplicitIntercept(offset,"target"),DegenerateTargetPolicy.Refuse,ResidualCovarianceCapability.DiagonalPlusLowRank(n.descriptor,1),right(TrainingBinding(s.descriptor,"f","d")),Vector("f"),right(PatternFitDiagnostics(Vector(0.0),"f",Vector.empty))))
    val covariance=right(ResidualCovariance.fromFactors(n,Vector(2.0,1.0),DMat.dense(2,1,Vector(1.0,1.0)))); val prior=right(TargetPriorCovariance(q,DMat.dense(2,2,Vector(2.0,.3,.3,1.0)),id("sigma"),"declared-metric"))
    val before=right(PatternPrediction.fromArtifact(n,q,r,artifact,covariance,Some(prior))); val root=Files.createTempDirectory("prediction-archive-"); val stored=right(right(LocalPatternArchive.open(root)).write("fit",declaration,artifact,s)); val restored=right(right(LocalPatternArchive.open(root)).read(stored.metadata,s)).artifact
    val after=right(PatternPrediction.fromArtifact(n,q,r,restored,covariance,Some(prior))); val x=right(AxisValues(n,Vector(11.0,-4.5)))
    close(right(before.rawScores(x)).values.values.head,right(after.rawScores(x)).values.values.head); close(right(before.calibratedScores(x)).values.values.head,right(after.calibratedScores(x)).values.values.head)
    val beforeDecoded = right(before.decode(x)).values.values
    val afterDecoded = right(after.decode(x)).values.values
    beforeDecoded.zip(afterDecoded).foreach((a, b) => close(a, b))
    val y = right(AxisValues(q, Vector(1.0, -1.0)))
    right(before.encode(y)).values.zip(right(after.encode(y)).values).foreach((a, b) => close(a, b))
    assertEquals(before.numericalIdentity, after.numericalIdentity)
    restored.target match
      case TargetGeometry.Continuous(v) =>
        assertEquals(v.metricDiagonal.values,Vector(2.0,3.0)); assertEquals(v.blockWeights.head._2.values,Vector(2.0,3.0))
      case _ => fail("continuous")

  test("categorical class keys and priors remain prediction inputs after restoration"):
    val n=axis("cn",Vector("n1","n2")); val q=axis("cq",Vector("q")); val r=axis("cr",Vector("r")); val s=axis("cs",Vector("s1","s2")); val classes=axis("class",Vector("z","a","m"))
    val geo=right(TargetGeometry.categorical(classes,q,DMat.dense(3,1,Vector(-1.0,0.0,1.0)),right(AxisValues(classes,Vector(.2,.3,.5))))); val factors=right(PatternFactors(n,q,r,DMat.dense(2,1,Vector(1.0,2.0)),DMat.dense(1,1,Vector(1.0)),GaugeEvidence.PendingNumericalCheck))
    val artifact=right(PatternArtifact(factors,geo,CenteringPolicy.CenteredBeforeFit("x","y"),DegenerateTargetPolicy.Refuse,ResidualCovarianceCapability.DiagonalPlusLowRank(n.descriptor,1),right(TrainingBinding(s.descriptor,"f","d")),Vector("f"),right(PatternFitDiagnostics(Vector(0.0),"f",Vector.empty))))
    val cov=right(ResidualCovariance.fromFactors(n,Vector(2.0,1.0),DMat.dense(2,1,Vector(1.0,1.0)))); val root=Files.createTempDirectory("class-prediction-archive-"); val ref=right(right(LocalPatternArchive.open(root)).write("fit",declaration,artifact,s)); val restored=right(right(LocalPatternArchive.open(root)).read(ref.metadata,s)).artifact
    val before=right(PatternPrediction.fromArtifact(n,q,r,artifact,cov)); val after=right(PatternPrediction.fromArtifact(n,q,r,restored,cov)); val x=right(AxisValues(n,Vector(1.0,-.5))); val a=right(before.classify(x)); val b=right(after.classify(x))
    assertEquals(b.keys,Vector("z","a","m")); assertEquals(b.probabilities,a.probabilities)
    restored.target match
      case TargetGeometry.Categorical(v) => assertEquals(v.priors.values,Vector(.2,.3,.5))
      case _ => fail("categorical")
