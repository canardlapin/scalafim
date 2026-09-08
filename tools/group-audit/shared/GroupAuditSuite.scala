package scalafim.fmri.group

import gale.linalg.{DMat, DVec, Matrix}
import scalafim.dataset.SubjectId
import scalafim.fmri.fit.{ResidualDegreesOfFreedom, TContrastResult}
import scalafim.image.NeuroSpace

class GroupAuditSuite extends munit.FunSuite:
  private def value[A](x: Either[GroupError,A]): A = x.fold(e => fail(e.message), identity)
  private def matrix(rows: Vector[Vector[Double]]): DMat =
    val b = Matrix.newBuilder(rows.length, rows.head.length)
    rows.indices.foreach(i => rows(i).indices.foreach(j => b(i,j) = rows(i)(j)))
    b.result()
  private val names = Vector("(Intercept)","group","age","covariate")
  private val subjects = Vector.tabulate(24)(i => SubjectId(s"s$i"))
  private val modes = Vector("OLS" -> GroupWeighting.Unweighted, "FE" -> GroupWeighting.InverseVariance, "DL" -> GroupWeighting.RandomEffects())
  private def run(x: Vector[Vector[Double]], y: Vector[Vector[Double]], v: Vector[Vector[Double]], mode: GroupWeighting): Either[GroupError,GroupFit] =
    for
      design <- GroupDesign.fromMatrix(matrix(x), names.take(x.head.length))
      data <- GroupData.withVariances(subjects.take(x.length), GroupSpace.SampleAxis(y.head.length), "effect", matrix(y), matrix(v))
      model <- GroupModel.build(data,design,mode)
      fit <- GroupEngine.fit(model)
    yield fit.fit("effect").get
  private def close(a: Double, b: Double, tol: Double = 2e-9): Unit =
    assert(a.isFinite && b.isFinite && math.abs(a-b) <= tol*(1+math.abs(b)), s"actual=$a expected=$b")
  private val contrast = GroupContrast.unsafe("adjusted difference",Map("group"->1.0,"age"-> -0.5,"covariate"->0.2))

  modes.foreach { (label, mode) =>
    test(s"independent R reference: $label coefficients, SE, contrast p and heterogeneity") {
      val fit = value(run(GroupAuditFixture.x,GroupAuditFixture.y,GroupAuditFixture.v,mode))
      val result = value(contrast.evaluate(fit))
      val refs = GroupAuditReference.rows.filter(_.head == label)
      refs.foreach { row =>
        val s = row(1).toInt
        for j <- 0 until 4 do
          close(fit.coefficients(j,s),row(8+j).toDouble)
          close(fit.standardErrors(j,s),row(12+j).toDouble)
        close(result.estimates(s),row(2).toDouble)
        close(result.standardErrors(s),row(3).toDouble)
        close(result.statistics(s),row(4).toDouble)
        close(result.pValues(s),row(5).toDouble,2e-7)
        if label != "OLS" then
          close(fit.heterogeneity.get.tau2(s),row(6).toDouble)
          close(fit.heterogeneity.get.q(s),row(7).toDouble)
      }
    }
    test(s"participant permutation and independent sample blocks: $label") {
      val fit = value(run(GroupAuditFixture.x,GroupAuditFixture.y,GroupAuditFixture.v,mode))
      val reversed = value(run(GroupAuditFixture.x.reverse,GroupAuditFixture.y.reverse,GroupAuditFixture.v.reverse,mode))
      val block = value(run(GroupAuditFixture.x,GroupAuditFixture.y.map(_.slice(2,5)),GroupAuditFixture.v.map(_.slice(2,5)),mode))
      for j <- 0 until 4; s <- 0 until 7 do
        close(reversed.coefficients(j,s),fit.coefficients(j,s))
        close(reversed.standardErrors(j,s),fit.standardErrors(j,s))
      for j <- 0 until 4; s <- 0 until 3 do close(block.coefficients(j,s),fit.coefficients(j,s+2))
    }
    test(s"covariate unit change preserves the same estimand: $label") {
      val expected = value(contrast.evaluate(value(run(GroupAuditFixture.x,GroupAuditFixture.y,GroupAuditFixture.v,mode))))
      val scale = 1e-6
      val x = GroupAuditFixture.x.map(row => row.updated(2,row(2)*scale))
      val changed = value(run(x,GroupAuditFixture.y,GroupAuditFixture.v,mode))
      val c = GroupContrast.unsafe("same effect",Map("group"->1.0,"age"->(-0.5*scale),"covariate"->0.2))
      val actual = value(c.evaluate(changed))
      for s <- 0 until 7 do
        close(actual.estimates(s),expected.estimates(s))
        close(actual.standardErrors(s),expected.standardErrors(s))
    }
  }

  test("weighted rank deficiency is a typed failure rather than a successful all-NaN map") {
    val x = GroupAuditFixture.x.map(row => row.updated(3,row(2)))
    for mode <- Vector(GroupWeighting.InverseVariance,GroupWeighting.RandomEffects()) do
      val result = run(x,GroupAuditFixture.y,GroupAuditFixture.v,mode)
      assert(result.isLeft,s"rank deficient weighted fit was Right; coefficients=${result.toOption.map(_.coefficients(0,0))}")
  }
  test("subtracting a term from itself cannot become a negative term estimate") {
    val fit = value(run(GroupAuditFixture.x,GroupAuditFixture.y,GroupAuditFixture.v,GroupWeighting.Unweighted))
    val result = GroupContrast.difference("self difference","group","group").evaluate(fit)
    assert(result.isLeft || result.toOption.get.estimates.toSeq.forall(_ == 0.0),s"actual=${result.toOption.map(_.estimates.toSeq)}")
  }
  test("validating contrast constructor returns typed errors for non-finite and empty weights") {
    assert(GroupContrast.fromStrings("bad",Map("age"->Double.NaN)).isLeft)
    assert(GroupContrast.fromStrings("empty",Map.empty).isLeft)
  }
  test("two-sample labels cannot silently create duplicate intercept terms") {
    val d = GroupDesign.twoSample(Vector("control","(Intercept)","control","(Intercept)"))
    assert(d.isLeft || d.toOption.get.termNames.distinct.size == d.toOption.get.termNames.size,s"actual=$d")
  }

  private val people = Vector("a","b","c").map(SubjectId(_))
  private val space = GroupSpace.VoxelAxis(NeuroSpace(Vector(2,1,1)),Vector(0,1))
  private def t(ys: Vector[Double], indices: Vector[Int], name: String = "effect", se: Double = 1.0): TContrastResult =
    TContrastResult(name,DVec.fromSeq(ys),DVec.fromSeq(ys.map(_=>se)),DVec.fromSeq(ys.map(_/se)),ResidualDegreesOfFreedom.unsafe(80),indices)
  test("first-level bridge aligns or rejects reordered voxel identities") {
    val results = Map((people(0),"effect")->t(Vector(1,10),Vector(0,1)),(people(1),"effect")->t(Vector(20,2),Vector(1,0)),(people(2),"effect")->t(Vector(3,30),Vector(0,1)))
    val data = FirstLevel.groupData(space,people,Vector("effect"),results)
    data.foreach { d =>
      val fit = value(GroupEngine.fit(value(GroupModel.build(d,GroupDesign.intercept(3))))).fit("effect").get
      close(fit.coefficients(0,0),2.0)
      close(fit.coefficients(0,1),20.0)
    }
  }
  test("first-level bridge rejects substituted contrast identity") {
    val results = people.map(p => (p,"effect")->t(Vector(1,2),Vector(0,1),name="different effect")).toMap
    assert(FirstLevel.groupData(space,people,Vector("effect"),results).isLeft)
  }
  test("first-level bridge does not turn a negative standard error into valid variance") {
    val results = people.map(p => (p,"effect")->t(Vector(1,2),Vector(0,1),se= -1.0)).toMap
    assert(FirstLevel.groupData(space,people,Vector("effect"),results).isLeft)
  }
