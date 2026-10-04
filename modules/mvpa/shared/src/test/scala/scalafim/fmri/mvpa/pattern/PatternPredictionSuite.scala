package scalafim.fmri.mvpa.pattern

import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import munit.FunSuite
import scalafim.fmri.mvpa.AxisRef

class PatternPredictionSuite extends FunSuite:
  private def right[A](v: Either[?, A]): A = v.fold(e => fail(e.toString), identity)
  private def axis(name: String, n: Int) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(n)(i => s"$name-$i"), "fixture", "one", "raw"))
  private def id(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private def close(a: Double, b: Double, tol: Double = 1e-10) = assertEqualsDouble(a, b, tol)

  private final class Gaussian:
    val neural=axis("neural",2); val target=axis("target",2); val components=axis("component",1); val samples=axis("samples",2)
    val a=DMat.dense(2,1,Vector(1.0,2.0)); val c=DMat.dense(2,1,Vector(.6,.8))
    val unit=right(AxisValues(target,Vector(1.0,1.0)))
    val geometry=right(TargetGeometry.continuous(target,unit,unit,Vector("all" -> unit)))
    val factors=right(PatternFactors(neural,target,components,a,c,GaugeEvidence.PendingNumericalCheck))
    val covariance=right(ResidualCovariance.fromFactors(neural,Vector(2.0,1.0),DMat.dense(2,1,Vector(1.0,1.0))))
    val artifact=right(PatternArtifact(factors,geometry,CenteringPolicy.CenteredBeforeFit("x","y"),DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.DiagonalPlusLowRank(neural.descriptor,1),right(TrainingBinding(samples.descriptor,"fixture","digest")),Vector("fixture"),right(PatternFitDiagnostics(Vector(0.0),"fixture",Vector.empty))))
    val prior=right(TargetPriorCovariance(target,DMat.dense(2,2,Vector(2.0,.3,.3,1.0)),id("sigma-y"),"target-metric"))
    val prediction=right(PatternPrediction.fromArtifact(neural,target,components,artifact,covariance,Some(prior)))

  test("Gaussian heads match the independent two by two joint inverse oracle"):
    val f=new Gaussian; val x=right(AxisValues(f.neural,Vector(1.0,-.5)))
    // Psi^-1 x=(.5,-.5), hence u=A^T Psi^-1 x=-.5 and G=2.
    close(right(f.prediction.rawScores(x)).values.values.head,-.5)
    close(right(f.prediction.calibratedScores(x)).values.values.head,-.25)
    val b00=.6; val b01=.8; val b10=1.2; val b11=1.6
    val s00=2.0; val s01=.3; val s11=1.0
    val h00=3+b00*b00*s00+2*b00*b01*s01+b01*b01*s11
    val h01=1+b00*b10*s00+(b00*b11+b01*b10)*s01+b01*b11*s11
    val h11=2+b10*b10*s00+2*b10*b11*s01+b11*b11*s11
    val det=h00*h11-h01*h01; val x0=(h11*1.0-h01*(-.5))/det; val x1=(-h01*1.0+h00*(-.5))/det
    val expected0=s00*(b00*x0+b10*x1)+s01*(b01*x0+b11*x1)
    val expected1=s01*(b00*x0+b10*x1)+s11*(b01*x0+b11*x1)
    val decoded=right(f.prediction.decode(x)).values.values
    close(decoded(0),expected0,1e-9); close(decoded(1),expected1,1e-9)
    val posterior=right(f.prediction.posteriorScores(x)).values.values.head
    assert(math.abs(posterior+.5) > 1e-8, "posterior is distinct from raw score")
    val encoded=right(f.prediction.encode(right(AxisValues(f.target,Vector(1.0,-1.0))))).values
    close(encoded(0),-.2); close(encoded(1),-.4)
    assertEquals(f.prediction.work.solveDimension,1)
    assertEquals(f.prediction.work.retainedFilterCells,2L)

  test("prior covariance rejects asymmetric singular foreign and over-budget declarations"):
    val f=new Gaussian
    assert(TargetPriorCovariance(f.target,DMat.dense(2,2,Vector(1.0,.1,.2,1.0)),id("asym"),"r").isLeft)
    assert(TargetPriorCovariance(f.target,DMat.dense(2,2,Vector(1.0,1.0,1.0,1.0)),id("singular"),"r").isLeft)
    assert(TargetPriorCovariance(f.target,DMat.eye(2),id("budget"),"r",PatternPredictionPolicy(1e-12,1L)).isLeft)
    val foreign=axis("foreign",2); val p=right(TargetPriorCovariance(foreign,DMat.eye(2),id("foreign"),"r"))
    assert(PatternPrediction.fromArtifact(f.neural,f.target,f.components,f.artifact,f.covariance,Some(p)).isLeft)

  test("categorical posterior preserves declared class order and normalized direct scores"):
    val neural=axis("n",2); val target=axis("q",1); val component=axis("r",1); val conditions=right(AxisRef.fromStableKeys("class",SpaceRole.Observed,Vector("z","a","m"),"fixture","one","raw")); val samples=axis("s",2)
    val codes=DMat.dense(3,1,Vector(-1.0,0.0,1.0)); val geo=right(TargetGeometry.categorical(conditions,target,codes,right(AxisValues(conditions,Vector(.2,.3,.5)))))
    val factors=right(PatternFactors(neural,target,component,DMat.dense(2,1,Vector(1.0,2.0)),DMat.dense(1,1,Vector(1.0)),GaugeEvidence.PendingNumericalCheck))
    val covariance=right(ResidualCovariance.fromFactors(neural,Vector(2.0,1.0),DMat.dense(2,1,Vector(1.0,1.0))))
    val artifact=right(PatternArtifact(factors,geo,CenteringPolicy.CenteredBeforeFit("x","y"),DegenerateTargetPolicy.Refuse,ResidualCovarianceCapability.DiagonalPlusLowRank(neural.descriptor,1),right(TrainingBinding(samples.descriptor,"f","d")),Vector("f"),right(PatternFitDiagnostics(Vector(0.0),"f",Vector.empty))))
    val result=right(PatternPrediction.fromArtifact(neural,target,component,artifact,covariance).flatMap(_.classify(right(AxisValues(neural,Vector(1.0,-.5))))))
    // u=-.5 and G=2, hence log p_k + code_k*(-.5) - code_k^2.
    val expected=Vector(math.log(.2)-.5,math.log(.3),math.log(.5)-1.5)
    assertEquals(result.keys,Vector("z","a","m")); result.logScores.zip(expected).foreach((a,b)=>close(a,b)); close(result.probabilities.sum,1.0); assert(result.probabilities.forall(_.isFinite))

  test("explicit intercept is removed from neural input and restored by encoding"):
    val f=new Gaussian; val offset=right(AxisValues(f.neural,Vector(10.0,-4.0)))
    val artifact=right(PatternArtifact(f.factors,f.geometry,CenteringPolicy.ExplicitIntercept(offset,"y"),DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.DiagonalPlusLowRank(f.neural.descriptor,1),right(TrainingBinding(f.samples.descriptor,"f","d")),Vector("f"),right(PatternFitDiagnostics(Vector(0.0),"f",Vector.empty))))
    val p=right(PatternPrediction.fromArtifact(f.neural,f.target,f.components,artifact,f.covariance,Some(f.prior)))
    val centered=right(p.rawScores(right(AxisValues(f.neural,Vector(11.0,-4.5))))).values.values.head
    close(centered,-.5)
    val encoded=right(p.encode(right(AxisValues(f.target,Vector(1.0,-1.0))))).values
    close(encoded(0),9.8); close(encoded(1),-4.4)
    assert(p.rawScores(right(AxisValues(axis("foreign-input",2),Vector(11.0,-4.5)))).isLeft)

  test("deficient loading filters refuse calibrated scores but retain raw and Gaussian posterior heads"):
    val f=new Gaussian; val deficient=right(PatternFactors(f.neural,f.target,f.components,DMat.zeros(2,1),f.c,GaugeEvidence.PendingNumericalCheck))
    val artifact=right(PatternArtifact(deficient,f.geometry,CenteringPolicy.CenteredBeforeFit("x","y"),DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.DiagonalPlusLowRank(f.neural.descriptor,1),right(TrainingBinding(f.samples.descriptor,"f","d")),Vector("f"),right(PatternFitDiagnostics(Vector(0.0),"f",Vector.empty))))
    val p=right(PatternPrediction.fromArtifact(f.neural,f.target,f.components,artifact,f.covariance,Some(f.prior)))
    val x=right(AxisValues(f.neural,Vector(1.0,-.5)))
    assert(p.calibratedScores(x).isLeft); close(right(p.rawScores(x)).values.values.head,0.0); close(right(p.posteriorScores(x)).values.values.head,0.0)

  test("restored equal descriptors are admitted, while foreign reconstructed descriptors are refused"):
    val f=new Gaussian
    val n=axis("neural",2); val q=axis("target",2); val r=axis("component",1)
    assert(PatternPrediction.fromArtifact(n,q,r,f.artifact,f.covariance,Some(f.prior)).isRight)
    assert(PatternPrediction.fromArtifact(axis("other-neural",2),q,r,f.artifact,f.covariance,Some(f.prior)).isLeft)

  test("explicit covariance is used once despite nonunit target geometry metadata"):
    val f=new Gaussian
    val metric=right(AxisValues(f.target,Vector(2.0,3.0)))
    val geometry=right(TargetGeometry.continuous(f.target,metric,metric,Vector("block" -> metric)))
    val artifact=right(PatternArtifact(f.factors,geometry,CenteringPolicy.CenteredBeforeFit("x","y"),DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.DiagonalPlusLowRank(f.neural.descriptor,1),right(TrainingBinding(f.samples.descriptor,"f","d")),Vector("f"),right(PatternFitDiagnostics(Vector(0.0),"f",Vector.empty))))
    val p=right(PatternPrediction.fromArtifact(f.neural,f.target,f.components,artifact,f.covariance,Some(f.prior)))
    val x=right(AxisValues(f.neural,Vector(1.0,-.5)))
    val ordinary=right(f.prediction.decode(x)).values.values; val nonunit=right(p.decode(x)).values.values
    ordinary.zip(nonunit).foreach((a,b)=>close(a,b,1e-10))
    assert(PatternPrediction.fromArtifact(f.neural,f.target,f.components,f.artifact,f.covariance,Some(f.prior),PatternPredictionPolicy(1e-12,0L)).isLeft)

  test("scalar rescaling retains the finite joint Gaussian decode and derived overflow is typed"):
    val neural=axis("scale-n",1); val target=axis("scale-q",1); val component=axis("scale-r",1); val samples=axis("scale-s",2)
    val one=right(AxisValues(target,Vector(1.0))); val geometry=right(TargetGeometry.continuous(target,one,one,Vector("all" -> one)))
    def artifact(c: Double) =
      val factors=right(PatternFactors(neural,target,component,DMat.dense(1,1,Vector(1e150)),DMat.dense(1,1,Vector(c)),GaugeEvidence.PendingNumericalCheck))
      right(PatternArtifact(factors,geometry,CenteringPolicy.CenteredBeforeFit("x","y"),DegenerateTargetPolicy.Refuse,
        ResidualCovarianceCapability.DiagonalPlusLowRank(neural.descriptor,1),right(TrainingBinding(samples.descriptor,"f","d")),Vector("f"),right(PatternFitDiagnostics(Vector(0.0),"f",Vector.empty))))
    val covariance=right(ResidualCovariance.fromFactors(neural,Vector(1.0),DMat.zeros(1,1)))
    val prior=right(TargetPriorCovariance(target,DMat.dense(1,1,Vector(1.0)),id("scale-prior"),"unit"))
    val model=right(PatternPrediction.fromArtifact(neural,target,component,artifact(1e-150),covariance,Some(prior)))
    val value=right(model.decode(right(AxisValues(neural,Vector(1e-250))))).values.values.head
    assertEqualsDouble(value / 5e-251,1.0,1e-10)
    assert(PatternPrediction.fromArtifact(neural,target,component,artifact(1e200),covariance,Some(prior)).isLeft)

  test("noncommuting rank-two target and precision Grams match the dense joint inverse oracle"):
    val n=axis("nc-n",2); val q=axis("nc-q",2); val r=axis("nc-r",2); val s=axis("nc-s",2)
    val a=DMat.dense(2,2,Vector(1.0,.3,.2,1.4)); val c=DMat.dense(2,2,Vector(.8,-.6,.6,.8)); val unit=right(AxisValues(q,Vector(1.0,1.0)))
    val geo=right(TargetGeometry.continuous(q,unit,unit,Vector("all" -> unit))); val psi=right(ResidualCovariance.fromFactors(n,Vector(2.0,1.0),DMat.dense(2,1,Vector(1.0,1.0))))
    val factors=right(PatternFactors(n,q,r,a,c,GaugeEvidence.PendingNumericalCheck)); val art=right(PatternArtifact(factors,geo,CenteringPolicy.CenteredBeforeFit("x","y"),DegenerateTargetPolicy.Refuse,ResidualCovarianceCapability.DiagonalPlusLowRank(n.descriptor,1),right(TrainingBinding(s.descriptor,"f","d")),Vector("f"),right(PatternFitDiagnostics(Vector(0.0),"f",Vector.empty))))
    val sigma=DMat.dense(2,2,Vector(2.0,.4,.4,1.0)); val prior=right(TargetPriorCovariance(q,sigma,id("noncommuting"),"unit")); val model=right(PatternPrediction.fromArtifact(n,q,r,art,psi,Some(prior)))
    val f00=1.0*.8+.3*(-.6); val f01=1.0*.6+.3*.8; val f10=.2*.8+1.4*(-.6); val f11=.2*.6+1.4*.8
    val h00=3+f00*f00*2+2*f00*f01*.4+f01*f01; val h01=1+f00*f10*2+(f00*f11+f01*f10)*.4+f01*f11; val h11=2+f10*f10*2+2*f10*f11*.4+f11*f11
    val x0=1.0; val x1 = -.5; val det=h00*h11-h01*h01; val z0=(h11*x0-h01*x1)/det; val z1=(-h01*x0+h00*x1)/det
    val e0=2*(f00*z0+f10*z1)+.4*(f01*z0+f11*z1); val e1=.4*(f00*z0+f10*z1)+(f01*z0+f11*z1)
    val phi=c.t*sigma*c; val g=a.t*right(psi.applyPrecision(a)); assert(math.abs((phi*g-g*phi)(0,1))>1e-8,"Phi and G must not commute")
    val actual=right(model.decode(right(AxisValues(n,Vector(x0,x1))))).values.values; close(actual(0),e0,1e-9); close(actual(1),e1,1e-9)
    val changed=right(TargetPriorCovariance(q,sigma,id("noncommuting-revision-2"),"unit")); val other=right(PatternPrediction.fromArtifact(n,q,r,art,psi,Some(changed)))
    assert(model.numericalIdentity != other.numericalIdentity,"prior value identity participates in predictive identity")
