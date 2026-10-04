package scalafim.transform.qualification

import image4s.geometry.{D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.*
import scalafim.transform.fsl.*
import scalafim.transform.nifti.NiftiRaw

class P312FreshFlirtSuite extends munit.FunSuite:
  private val dir = "/private/tmp/claude-502/-Users-bbuchsbaum-code-scala-scalafim/088206fa-94ad-412d-8607-93a8ef15952c/scratchpad/p312/"
  private def ok[E, A](r: Either[E, A]): A = r.fold(e => fail(s"$e"), identity)
  private def bytes(n: String) = java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(dir + n))
  private def geom(n: String) = ok(FslHeaderGeometry(ok(NiftiRaw.parse(IArray.unsafeFromArray(bytes(n))))))
  private val source: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("s")))
  private val reference: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("r")))
  test("fresh FLIRT"):
    val grids = FslGrids[source.type, reference.type](source, geom("src.nii"), reference, geom("ref.nii"))
    println(s"P312 src selected=${geom("src.nii").selected} neuro=${geom("src.nii").neurological}; ref selected=${geom("ref.nii").selected} neuro=${geom("ref.nii").neurological}")
    val t = ok(FlirtInterpretation.interpret(ok(FlirtCodec.decode(TransformSource.Text(String(bytes("fresh.mat"), "UTF-8")))), grids))
    var worst = 0.0
    scala.io.Source.fromFile(dir + "fresh_points.tsv").getLines().foreach: line =>
      val v = line.split("\t").map(_.toDouble).toVector
      val m = ok(t.mapPoint(ok(Point.fromVector(source, v.take(3))).asInstanceOf[Point[source.type, D3]])).coordinates
      worst = math.max(worst, m.zip(v.drop(3)).map((a, e) => math.abs(a - e)).max)
    println(f"P312 fresh FLIRT max dev $worst%.3e mm")
    assert(worst < 1e-6)
