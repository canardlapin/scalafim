package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import java.io.{BufferedOutputStream,DataOutputStream}
import java.nio.file.{Files,Path}
import java.util.concurrent.{CountDownLatch,TimeUnit}
import _root_.javafx.application.Platform
import _root_.javafx.scene.{Group,ParallelCamera,Scene,SceneAntialiasing,SnapshotParameters,SubScene}
import _root_.javafx.scene.image.WritableImage
import _root_.javafx.scene.paint.Color
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

/** A native atlas fixture with optional independently checked vertex lighting. The external oracle reads only original
  * geometry and packed vertex colours, never native texture coordinates.
  */
object JavaFxAffineColorProbe:
  private final case class Fixture(positions: Array[Float], colors: Array[Int], faces: Array[Int], boundary: Array[Int], normals: Array[Float])

  private def fixture(family: String, order: String, oblique: Boolean): Fixture =
    val n = if family == "ramp128" then 128 else if family == "ramp32" then 32 else 1
    val vertices = (n+1)*(n+1)
    val positions = new Array[Float](vertices*3)
    val colors = new Array[Int](vertices)
    val normals = new Array[Float](vertices*3)
    var y = 0
    while y <= n do
      var x = 0
      while x <= n do
        val index = y*(n+1)+x
        val dx = 216.0*x/n-108
        val dy = 216.0*y/n-108
        val angle = math.toRadians(17)
        positions(index*3) = (128+(if oblique then math.cos(angle)*dx-math.sin(angle)*dy*0.67 else dx)).toFloat
        positions(index*3+1) = (128+(if oblique then math.sin(angle)*dx+math.cos(angle)*dy*0.67 else dy)).toFloat
        positions(index*3+2) = (if oblique then dy*math.sqrt(1-0.67*0.67) else 0).toFloat
        val color = if family == "rgb" then
          Vector(Rgba32.unsafe(255,0,0),Rgba32.unsafe(0,255,0),Rgba32.unsafe(0,0,255),Rgba32.unsafe(255,255,255))(index)
        else
          val g = if family == "diagonal" then (if index==0 then 0 else 255) else math.round(32+192.0*x/n).toInt
          Rgba32.unsafe(g,g,g)
        colors(index) = color.toPackedInt
        // A declared smooth normal field, independent of the planar geometry.
        // It spans both light-facing and back-facing normals.
        val nx = 0.8*math.cos(2*math.Pi*x/n)
        val ny = 0.6*math.sin(2*math.Pi*y/n)
        val nz = 0.35+(x-y).toDouble/n
        val length = math.sqrt(nx*nx+ny*ny+nz*nz)
        normals(index*3) = (nx/length).toFloat
        normals(index*3+1) = (ny/length).toFloat
        normals(index*3+2) = (nz/length).toFloat
        x += 1
      y += 1
    val faces = new Array[Int](n*n*6)
    val count = n*n*2
    var face = 0
    while face < count do
      val source = if order=="shuffled" then (face*103)&(count-1) else face
      val cell = source/2
      val a = (cell/n)*(n+1)+cell%n
      val corners = if source%2==0 then Array(a,a+1,a+n+1) else Array(a+1,a+n+2,a+n+1)
      var c = 0
      while c < 3 do
        faces(face*3+c) = corners((c+(if order=="cyclic" then 1 else 0))%3)
        c += 1
      face += 1
    Fixture(positions,colors,faces,Array(0,n,(n+1)*(n+1)-1,n*(n+1)),normals)

  private def saveOriginal(path: Path, f: Fixture, lighting: SurfaceLighting): Unit =
    val out = new DataOutputStream(new BufferedOutputStream(Files.newOutputStream(path)))
    try
      out.writeInt(if lighting == SurfaceLighting.Unlit then 23 else 24)
      out.writeInt(f.colors.length)
      out.writeInt(f.faces.length/3)
      f.boundary.foreach(out.writeInt)
      f.positions.foreach(out.writeFloat)
      f.colors.foreach(out.writeInt)
      f.faces.foreach(out.writeInt)
      lighting match
        case SurfaceLighting.Unlit => ()
        case SurfaceLighting.Directional(ambient,diffuse,x,y,z) =>
          f.normals.foreach(out.writeFloat)
          Vector(ambient.value,diffuse.value,x,y,z).foreach(out.writeDouble)
    finally out.close()

  private def render(f: Fixture, encoding: JavaFxAtlasEncoding, aa: SceneAntialiasing,
      maxTextureSize: Int, lighting: SurfaceLighting, retainedSeed: Boolean): WritableImage =
    val id = SurfaceId.unsafe("color-oracle")
    val geometry = SurfaceGeometry(TriangleMesh.fromArrays(f.positions.map(_.toDouble),f.faces),Hemisphere.Left,SurfaceKind.Inflated)
    val layer = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("original-rgb"),id,geometry,f.colors.toVector.map(Rgba32.fromPackedInt)).toOption.get
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(id,geometry).toOption.get),Vector(layer)).toOption.get
    val state = SurfaceViewerState.initial(model).copy(lighting=lighting)
    val compiled = SurfaceCompiler.compile(model,state).toOption.get
    val plan = compiled.copy(meshes=compiled.meshes.map(_.copy(normals=new FloatBufferView(f.normals))))
    require(java.util.Arrays.equals(plan.meshes.head.positions.unsafeArray,f.positions))
    val config = JavaFxAtlasConfig.make(maxTextureSize=maxTextureSize,encoding=encoding).toOption.get
    val mode = if lighting==SurfaceLighting.Unlit then JavaFxMaterialMode.Unlit else JavaFxMaterialMode.Lit
    val previous = Option.when(retainedSeed):
      val primaries = Vector(Rgba32.unsafe(255,0,0),Rgba32.unsafe(0,255,0),Rgba32.unsafe(0,0,255))
      val seedLayer = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("seed-rgb"),id,geometry,
        Vector.tabulate(f.colors.length)(i => primaries(i%3))).toOption.get
      val seedModel = SurfaceViewerModel.make(model.surfaces,Vector(seedLayer)).toOption.get
      val seedCompiled = SurfaceCompiler.compile(seedModel,SurfaceViewerState.initial(seedModel).copy(lighting=lighting)).toOption.get
      val seed = seedCompiled.copy(meshes=seedCompiled.meshes.map(_.copy(normals=new FloatBufferView(f.normals))))
      JavaFxSurfaceProbe.compile(seed,mode,config).toOption.get
    val probe = JavaFxSurfaceProbe.compileRetaining(plan,mode,config,previous).toOption.get
    println(s"atlases=${probe.chunks.map(c => s"${c.atlas.width}x${c.atlas.height}").mkString(",")} faces=${probe.chunks.map(_.renderedFaceCount).sum}")
    // Isolate the scientific atlas/geometry lowering from application camera fit.
    // ParallelCamera expresses this fixture directly in known pixel coordinates.
    val root = new Group()
    probe.chunks.foreach: chunk =>
      chunk.view.getParent.asInstanceOf[Group].getChildren.remove(chunk.view)
      root.getChildren.add(chunk.view)
    val sub = new SubScene(root,256,256,true,aa)
    sub.setCamera(new ParallelCamera())
    sub.setFill(Color.WHITE)
    val scene = new Scene(new Group(sub),256,256)
    scene.getRoot.applyCss()
    scene.getRoot.layout()
    val image = sub.snapshot(new SnapshotParameters(),new WritableImage(256,256))
    root.getChildren.clear()
    sub.setRoot(new Group())
    scene.setRoot(new Group())
    image

  def main(args: Array[String]): Unit =
    val output = Path.of(args(0))
    Files.createDirectories(output)
    val encoding = JavaFxAtlasEncoding.valueOf(args(1))
    val maxTextureSize = args.lift(2).fold(256)(_.toInt)
    val lightingName = args.lift(3).getOrElse("Unlit")
    val retainedSeed = args.lift(4).contains("seeded")
    require(args.lift(4).forall(value => value == "fresh" || value == "seeded"))
    require(!retainedSeed || encoding.retainsLayout, "Seeded refinement requires retained encoding")
    val lighting = lightingName match
      case "Unlit" => SurfaceLighting.Unlit
      case "Default" => SurfaceLighting.Default
      case "Soft" => SurfaceLighting.directional(0.72,0.28,-0.4,-0.5,0.7681145747868608).toOption.get
      case other => throw new IllegalArgumentException(s"unknown fixture lighting: $other")
    require(JavaFxAtlasConfig.make(maxTextureSize=maxTextureSize,encoding=encoding).isRight)
    Files.writeString(output.resolve("fixture-config.json"),
      s"""{"maxTextureSize":$maxTextureSize,"lighting":"$lightingName","encoding":"$encoding","retainedSeed":$retainedSeed}
""")
    for name <- Vector("com.sun.prism.es2.ES2PhongMaterial","com.sun.prism.es2.ES2PhongShader") do
      val source = Class.forName(name).getProtectionDomain.getCodeSource.getLocation.toString
      require(source==System.getProperty("probe.expectedOrigin"),s"unexpected $name source: $source")
      println(s"$name=$source")
    val done = new CountDownLatch(1)
    @volatile var failure: Option[Throwable] = None
    Platform.startup(() => ())
    Platform.runLater: () =>
      try
        for family <- Vector("diagonal","rgb","ramp32","ramp128")
            order <- Vector("original","cyclic","shuffled")
            oblique <- Vector(false,true)
            aa <- Vector(SceneAntialiasing.DISABLED,SceneAntialiasing.BALANCED) do
          val name = s"$family-$order-${if oblique then "oblique" else "front"}-$aa"
          val f = fixture(family,order,oblique)
          saveOriginal(output.resolve(name+".bin"),f,lighting)
          val image = render(f,encoding,aa,maxTextureSize,lighting,retainedSeed)
          val colors = scala.collection.mutable.Set.empty[Int]
          var y = 0
          while y < 256 do
            var x = 0
            while x < 256 do
              colors.add(image.getPixelReader.getArgb(x,y))
              x += 1
            y += 1
          require(colors.size>32,s"blank/degenerate $name frame")
          val bitmap = new BufferedImage(256,256,BufferedImage.TYPE_INT_ARGB)
          var row = 0
          while row < 256 do
            var column = 0
            while column < 256 do
              bitmap.setRGB(column,row,image.getPixelReader.getArgb(column,row))
              column += 1
            row += 1
          ImageIO.write(bitmap,"png",output.resolve(name+".png").toFile)
          println(s"PASS snapshot $name ${f.faces.length/3} original faces ${colors.size} colours")
        println("PASS 48 native atlas fixtures; no Stage shown")
      catch case error: Throwable => failure=Some(error)
      finally done.countDown()
    try
      require(done.await(180,TimeUnit.SECONDS),"native color fixture timed out")
      failure.foreach(throw _)
    finally Platform.exit()
