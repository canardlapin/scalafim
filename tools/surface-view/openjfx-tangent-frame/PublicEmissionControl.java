import javafx.application.*;
import javafx.animation.AnimationTimer;
import javafx.scene.*;
import javafx.scene.image.*;
import javafx.scene.paint.*;
import javafx.scene.shape.*;
import javafx.scene.transform.*;
import javafx.stage.Stage;
import java.nio.file.Path;
import java.nio.file.Files;
import java.awt.image.BufferedImage;
import javax.imageio.ImageIO;
public class PublicEmissionControl {
 public static void main(String[] args) { Application.launch(App.class,args); }
 public static class App extends Application {
 public void start(Stage stage) throws Exception {
  Path out=Path.of(getParameters().getRaw().get(0)); Files.createDirectory(out);

  TriangleMesh mesh=new TriangleMesh(VertexFormat.POINT_NORMAL_TEXCOORD);
  mesh.getPoints().setAll(new float[]{-1f,-1f,0f,1f,-1f,0f,-1f,1f,0f,-1f,-1f,2f}); mesh.getNormals().setAll(new float[]{0.5773502588272095f,0.5773502588272095f,0.5773502588272095f,-1f,0f,0f,0f,-1f,0f,0f,0f,-1f});
  mesh.getTexCoords().setAll(new float[]{0.03125f,0.375f,0.09375f,0.375f,0.03125f,0.125f,0.28125f,0.375f,0.34375f,0.375f,0.28125f,0.125f,0.53125f,0.375f,0.59375f,0.375f,0.53125f,0.125f,0.78125f,0.375f,0.84375f,0.375f,0.78125f,0.125f}); mesh.getFaces().setAll(new int[]{0,0,0,1,1,1,2,2,2,0,0,3,2,2,4,3,3,5,0,0,6,3,3,7,1,1,8,1,1,9,3,3,10,2,2,11});
  WritableImage image=new WritableImage(64,64);
  paint(image,225,70,20);
  PhongMaterial material=new PhongMaterial(Color.BLACK);
  material.setSpecularColor(Color.TRANSPARENT); material.setSelfIlluminationMap(image);
  MeshView view=new MeshView(mesh); view.setCullFace(CullFace.NONE); view.setMaterial(material);
  view.getTransforms().add(new Affine(0,-143.99999618530273,0,160,0,0,-143.99999618530273,303.99999618530273,0.3200031816959381,0,0,-158.72319161891937));
  Group group=new Group(view);

  SubScene sub=new SubScene(group,320,320,true,SceneAntialiasing.BALANCED);
  sub.setFill(Color.WHITE); sub.setCamera(new ParallelCamera());
  stage.setScene(new Scene(new Group(sub))); stage.show();
  AnimationTimer timer=new AnimationTimer() {
   int pulses=0;
   public void handle(long now) {
    if(++pulses==3 || pulses==6) {
     try {
      WritableImage snap=sub.snapshot(null,null);
      BufferedImage bitmap=new BufferedImage(320,320,BufferedImage.TYPE_INT_ARGB);
      int orange=0,blue=0,black=0;
      for(int y=0;y<320;y++) for(int x=0;x<320;x++) {
       int pixel=snap.getPixelReader().getArgb(x,y); bitmap.setRGB(x,y,pixel);
       if(pixel==0xffe14614) orange++; if(pixel==0xff1e46dc) blue++; if(pixel==0xff000000) black++;
      }
      ImageIO.write(bitmap,"png",out.resolve(pulses+".png").toFile());
      System.out.println("pulses="+pulses+" orange="+orange+" blue="+blue+" black="+black);
      if (black != 0 || (pulses==3 ? orange : blue) < 10000)
       throw new AssertionError("Native emission did not preserve the exact texture color");
      if(pulses==3) paint(image,30,70,220);
      else {stop();stage.close();Platform.exit();}
     } catch(Throwable e) {e.printStackTrace();stop();stage.close();Platform.exit();System.exit(1);}
    }
   }
  }; timer.start();
 }
 static void paint(WritableImage image,int r,int g,int b) { for(int y=0;y<64;y++) for(int x=0;x<64;x++) image.getPixelWriter().setColor(x,y,Color.rgb(r,g,b)); }
 }
}
