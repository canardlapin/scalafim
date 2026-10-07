import com.jogamp.opengl.GLAutoDrawable;
import com.jogamp.opengl.GLCapabilities;
import com.jogamp.opengl.GLDrawableFactory;
import com.jogamp.opengl.GLEventListener;
import com.jogamp.opengl.GLProfile;
import com.jogamp.opengl.GLOffscreenAutoDrawable;
import com.jogamp.opengl.GL;
import com.jogamp.opengl.GL3;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Path;

/** Bounded native-context probe. This is not a SurfaceRenderPlan provider or product admission. */
public final class NativeContextProbe implements GLEventListener {
    private String vendor;
    private String renderer;
    private String version;
    private int[] pixel;

    @Override public void init(GLAutoDrawable drawable) {
        GL3 gl = drawable.getGL().getGL3();
        vendor = gl.glGetString(GL.GL_VENDOR);
        renderer = gl.glGetString(GL.GL_RENDERER);
        version = gl.glGetString(GL.GL_VERSION);
        System.out.println("native-vendor=" + vendor);
        System.out.println("native-renderer=" + renderer);
        System.out.println("native-version=" + version);
    }

    @Override public void display(GLAutoDrawable drawable) {
        GL3 gl = drawable.getGL().getGL3();
        gl.glViewport(0, 0, 64, 64);
        gl.glClearColor(0.25f, 0.5f, 0.75f, 1.0f);
        gl.glClear(GL.GL_COLOR_BUFFER_BIT);
        ByteBuffer bytes = ByteBuffer.allocateDirect(4);
        gl.glReadPixels(32, 32, 1, 1, GL.GL_RGBA, GL.GL_UNSIGNED_BYTE, bytes);
        pixel = new int[] {bytes.get(0) & 255, bytes.get(1) & 255, bytes.get(2) & 255, bytes.get(3) & 255};
        int[] expected = {64, 128, 191, 255};
        for (int channel = 0; channel < 4; channel++) {
            if (Math.abs(pixel[channel] - expected[channel]) > 1) {
                throw new IllegalStateException("native readback mismatch at channel " + channel + ": " + pixel[channel]);
            }
        }
        int error = gl.glGetError();
        if (error != GL.GL_NO_ERROR) throw new IllegalStateException("OpenGL error " + error);
    }

    @Override public void reshape(GLAutoDrawable drawable, int x, int y, int width, int height) {}
    @Override public void dispose(GLAutoDrawable drawable) {}

    private static String quoted(String value) {
        return "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\"";
    }

    public static void main(String[] args) throws Exception {
        Path output = Path.of(args[0]);
        GLOffscreenAutoDrawable drawable = null;
        NativeContextProbe probe = new NativeContextProbe();
        try {
            GLProfile profile = GLProfile.get(GLProfile.GL3);
            GLCapabilities caps = new GLCapabilities(profile);
            caps.setHardwareAccelerated(true);
            caps.setDoubleBuffered(false);
            caps.setSampleBuffers(false);
            caps.setRedBits(8);
            caps.setGreenBits(8);
            caps.setBlueBits(8);
            caps.setAlphaBits(8);
            drawable = GLDrawableFactory.getFactory(profile).createOffscreenAutoDrawable(null, caps, null, 64, 64);
            drawable.addGLEventListener(probe);
            drawable.display();
            if (probe.pixel == null) throw new IllegalStateException("no native readback was produced");
            Files.writeString(output, "{\"status\":\"PassContextOnly\",\"vendor\":" + quoted(probe.vendor)
                + ",\"renderer\":" + quoted(probe.renderer) + ",\"version\":" + quoted(probe.version)
                + ",\"capabilities\":" + quoted(drawable.getChosenGLCapabilities().toString())
                + ",\"pixel\":[" + probe.pixel[0] + "," + probe.pixel[1] + "," + probe.pixel[2] + "," + probe.pixel[3]
                + "],\"surfaceRenderPlanProvider\":false,\"scientificQualification\":false}\n");
        } catch (Throwable error) {
            Files.writeString(output, "{\"status\":\"Unsupported\",\"reason\":" + quoted(error.toString())
                + ",\"surfaceRenderPlanProvider\":false,\"scientificQualification\":false}\n");
            throw error;
        } finally {
            if (drawable != null) drawable.destroy();
        }
    }
}
