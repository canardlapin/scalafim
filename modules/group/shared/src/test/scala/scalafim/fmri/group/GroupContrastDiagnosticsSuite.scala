package scalafim.fmri.group

import gale.linalg.Matrix
import scalafim.dataset.SubjectId

class GroupContrastDiagnosticsSuite extends munit.FunSuite:
  private def get[A](a: Either[GroupError,A]): A = a.fold(e => fail(e.message), identity)
  private def ids(n: Int) = Vector.tabulate(n)(i => SubjectId(s"subject-$i"))
  private def design(n: Int, quarter: Boolean) = get(GroupDesign.fromMatrix(
    Matrix.tabulate(n,3) { (i,j) => j match
      case 0 => 1.0
      case 1 => if i < (if quarter then math.max(2,n/4) else n/2) then 1.0 else 0.0
      case _ => math.cos((i+.5)*2*math.Pi/n)
    }, Vector("intercept","group","covariate")))
  private def df(d: GroupContrastDiagnostics): Double = d.cr2WorkingInformation match
    case GroupCr2WorkingInformation.Available(v) => v
    case GroupCr2WorkingInformation.Unavailable(why) => fail(why)
  private def same(a: GroupContrastDiagnostics,b: GroupContrastDiagnostics,sign: Double=1.0): Unit =
    val byId=b.rows.map(r=>r.subject->r).toMap
    a.rows.foreach { r =>
      val s=byId(r.subject)
      assertEqualsDouble(s.leverage,r.leverage,1e-11)
      assertEqualsDouble(s.standardizedInfluence,sign*r.standardizedInfluence,1e-11)
      assertEqualsDouble(s.workingVarianceShare,r.workingVarianceShare,1e-11)
    }
    assertEqualsDouble(df(a),df(b),1e-10)
    assertEqualsDouble(a.workingEffectiveContributors,b.workingEffectiveContributors,1e-10)

  test("singleton CR2 information agrees with independent clubSandwich 0.7.0 for every design term") {
    // tools/group-nuisance/reference.R: lm, vcovCR(type=CR2,target=I), coef_test.
    val cases=Vector(
      (8,false,Vector(2.828032979976443,4.950515463917525,3.676875957120981)),
      (8,true,Vector(2.7885883224142844,2.1252727127217703,2.5241928543874046)),
      (20,false,Vector(8.761093793177299,16.974952213844997,11.501362509763235)),
      (20,true,Vector(9.436648933048462,7.5492784146570315,7.630609674535675)),
      (80,false,Vector(38.751794779682044,76.99371762454301,51.455806600704705)),
      (80,true,Vector(43.344247352858616,35.2300806792383,34.948891738204125)))
    cases.foreach { (n,quarter,expected) =>
      val x=design(n,quarter)
      x.termNames.zipWithIndex.foreach { (term,j) =>
        val d=get(GroupContrastDiagnostics.review(ids(n),x,GroupContrast.term(term)))
        assertEqualsDouble(df(d),expected(j),1e-9)
        assertEquals(d.residualDf,n-3)
        assertEqualsDouble(d.rows.map(_.leverage).sum,3.0,1e-12)
        assertEqualsDouble(d.rows.map(_.workingVarianceShare).sum,1.0,1e-12)
        assertEquals(d.nSubjects,n)
      }
    }
  }

  test("signed influence and leverage agree with independent R hat matrix and contrast solution") {
    val d=get(GroupContrastDiagnostics.review(ids(8),design(8,true),GroupContrast.term("group")))
    val h=Vector(.5255852991110277,.5255852991110279,.17617059822205536,.340886267851963,
      .340886267851963,.17617059822205544,.2926421651850463,.6220735044448613)
    val a=Vector(.41764216518504615,.5823578348149538,-.1164715669629908,.04824410266691692,
      .04824410266691699,-.1164715669629906,-.34941470088897253,-.51413037051888)
    val norm=math.sqrt(a.map(v=>v*v).sum)
    d.rows.zipWithIndex.foreach { (row,i) =>
      assertEquals(row.subject,ids(8)(i))
      assertEqualsDouble(row.leverage,h(i),1e-12)
      assertEqualsDouble(row.standardizedInfluence,a(i)/norm,1e-12)
      assertEqualsDouble(row.workingVarianceShare,math.pow(a(i)/norm,2),1e-12)
    }
  }

  test("analytical intercept design has equal influence, n effective contributors and n-1 working df") {
    val d=get(GroupContrastDiagnostics.review(ids(12),GroupDesign.intercept(12),GroupContrast.term("(Intercept)")))
    assertEqualsDouble(df(d),11.0,1e-11)
    assertEqualsDouble(d.workingEffectiveContributors,12.0,1e-11)
    d.rows.foreach { r =>
      assertEqualsDouble(r.leverage,1.0/12,1e-12)
      assertEqualsDouble(r.standardizedInfluence,1.0/math.sqrt(12),1e-12)
    }
  }

  test("subject reorder and equivalent design recoding preserve subject-bound information") {
    val x=design(20,true);val c=GroupContrast.unsafe("question",Map("group"->1.0,"covariate"-> -.3))
    val original=get(GroupContrastDiagnostics.review(ids(20),x,c))
    val order=Vector.tabulate(20)(i=>(i*7)%20)
    val reordered=get(GroupDesign.fromMatrix(Matrix.tabulate(20,3)((i,j)=>x.matrix(order(i),j)),x.termNames))
    same(original,get(GroupContrastDiagnostics.review(order.map(ids(20)),reordered,c)))
    val a=Matrix.tabulate(3,3) { (i,j) => Vector(Vector(1.0,10.0,-5.0),Vector(0.0,3.0,1.0),Vector(0.0,0.0,2.0))(i)(j) }
    val recoded=get(GroupDesign.fromMatrix(x.matrix*a,x.termNames))
    // Xnew=X A, so cnew=A' c; this preserves the named scientific question.
    val cnew=GroupContrast.unsafe("question",Map("group"->3.0,"covariate"-> .4))
    same(original,get(GroupContrastDiagnostics.review(ids(20),recoded,cnew)))
  }

  test("contrast scale and reflection preserve information even at extreme finite units") {
    val x=design(20,true)
    val original=get(GroupContrastDiagnostics.review(ids(20),x,GroupContrast.term("group")))
    for scale <- Vector(1e-300,1e300,-1e300) do
      val c=GroupContrast.unsafe("scaled",Map("group"->scale))
      same(original,get(GroupContrastDiagnostics.review(ids(20),x,c)),math.signum(scale))
    val units=get(GroupDesign.fromMatrix(Matrix.tabulate(20,3)((i,j)=>x.matrix(i,j)*(if j==1 then 1e-150 else if j==2 then 1e150 else 1.0)),x.termNames))
    same(original,get(GroupContrastDiagnostics.review(ids(20),units,GroupContrast.term("group"))))
  }

  test("unit leverage remains reviewable and has explicit unavailable working inference information") {
    val x=get(GroupDesign.fromMatrix(Matrix.tabulate(8,2)((i,j)=>if j==0 || i==0 then 1.0 else 0.0),Vector("intercept","isolated")))
    val d=get(GroupContrastDiagnostics.review(ids(8),x,GroupContrast.term("intercept")))
    assertEqualsDouble(d.rows.head.leverage,1.0,1e-12)
    assert(d.cr2WorkingInformation.isInstanceOf[GroupCr2WorkingInformation.Unavailable])
    assert(d.rows.forall(r=>r.workingVarianceShare.isFinite && r.standardizedInfluence.isFinite))
  }

  test("malformed identity, rank and contrast inputs return typed errors") {
    val x=design(8,true);val c=GroupContrast.term("group")
    assert(GroupContrastDiagnostics.review(ids(8).updated(1,ids(8).head),x,c).left.exists(_.isInstanceOf[GroupError.DuplicateSubjects]))
    assert(GroupContrastDiagnostics.review(ids(7),x,c).left.exists(_.isInstanceOf[GroupError.SubjectMismatch]))
    assertEquals(GroupContrastDiagnostics.review(ids(8),x,GroupContrast.term("absent")),Left(GroupError.UnknownContrastTerm("absent")))
    assert(GroupContrastDiagnostics.review(ids(8),x,GroupContrast.difference("zero","group","group")).left.exists(_.isInstanceOf[GroupError.EmptyContrast]))
    val singular=get(GroupDesign.fromMatrix(Matrix.tabulate(8,2)((_,_)=>1.0),Vector("a","b")))
    assert(GroupContrastDiagnostics.review(ids(8),singular,GroupContrast.term("a")).isLeft)
    assert(GroupContrastDiagnostics.review(ids(2),get(GroupDesign.fromMatrix(Matrix.eye(2),Vector("a","b"))),GroupContrast.term("a")).isLeft)
  }
