import com.jogamp.opengl.*;
import java.awt.image.BufferedImage;
import java.io.DataInputStream;
import java.io.InputStream;
import java.nio.*;
import java.nio.file.*;
import java.util.List;
import javax.imageio.ImageIO;

/** Original-triangle GPU color kernel only; does not implement a SurfaceRenderPlan provider. */
public final class NativeAffineProbe implements GLEventListener {
    private final Path input;
    private final Path output;
    private final List<Path> fixtures;
    private int program;
    private int vao;
    private final int[] buffers = new int[2];
    private int next;

    private NativeAffineProbe(Path input, Path output) throws Exception {
        this.input = input;
        this.output = output;
        try (var paths = Files.list(input)) {
            fixtures = paths.filter(p -> p.toString().endsWith(".bin")).sorted().toList();
        }
        if (fixtures.size() != 48) throw new IllegalArgumentException("expected exactly 48 original fixtures");
    }

    private static int shader(GL3 gl, int kind, String text) {
        int shader = gl.glCreateShader(kind);
        gl.glShaderSource(shader, 1, new String[] {text}, null, 0);
        gl.glCompileShader(shader);
        int[] status = new int[1];
        gl.glGetShaderiv(shader, GL3.GL_COMPILE_STATUS, status, 0);
        if (status[0] == 0) {
            byte[] log = new byte[8192];
            gl.glGetShaderInfoLog(shader, log.length, null, 0, log, 0);
            throw new IllegalStateException("shader failed: " + new String(log));
        }
        return shader;
    }

    @Override public void init(GLAutoDrawable drawable) {
        GL3 gl = drawable.getGL().getGL3();
        System.out.println("vendor=" + gl.glGetString(GL.GL_VENDOR));
        System.out.println("renderer=" + gl.glGetString(GL.GL_RENDERER));
        System.out.println("version=" + gl.glGetString(GL.GL_VERSION));
        int[] maximum = new int[1];
        gl.glGetIntegerv(GL3.GL_MAX_SAMPLES, maximum, 0);
        if (maximum[0] < 4) throw new IllegalStateException("four-sample MSAA unavailable");
        int vertex = shader(gl, GL3.GL_VERTEX_SHADER,
            "#version 150\nin vec3 originalPosition; in vec4 originalColor; out vec4 vertexColor;\n"
            + "void main(){gl_Position=vec4(originalPosition.x/128.0-1.0,1.0-originalPosition.y/128.0,originalPosition.z/512.0,1.0);vertexColor=originalColor;}\n");
        int fragment = shader(gl, GL3.GL_FRAGMENT_SHADER,
            "#version 150\ncentroid in vec4 vertexColor; out vec4 pixelColor; void main(){pixelColor=vertexColor;}\n");
        program = gl.glCreateProgram();
        gl.glAttachShader(program, vertex);
        gl.glAttachShader(program, fragment);
        gl.glBindAttribLocation(program, 0, "originalPosition");
        gl.glBindAttribLocation(program, 1, "originalColor");
        gl.glBindFragDataLocation(program, 0, "pixelColor");
        gl.glLinkProgram(program);
        int[] linked = new int[1];
        gl.glGetProgramiv(program, GL3.GL_LINK_STATUS, linked, 0);
        if (linked[0] == 0) {
            byte[] log = new byte[8192];
            gl.glGetProgramInfoLog(program, log.length, null, 0, log, 0);
            throw new IllegalStateException("link failed: " + new String(log));
        }
        gl.glDeleteShader(vertex);
        gl.glDeleteShader(fragment);
        int[] names = new int[1];
        gl.glGenVertexArrays(1, names, 0);
        vao = names[0];
        gl.glGenBuffers(2, buffers, 0);
        gl.glDisable(GL.GL_BLEND);
        gl.glDisable(GL.GL_DEPTH_TEST);
        gl.glDisable(GL.GL_CULL_FACE);
        gl.glDisable(GL.GL_DITHER);
        gl.glDisable(GL3.GL_FRAMEBUFFER_SRGB);
    }

    private static FloatBuffer floats(int count) {
        return ByteBuffer.allocateDirect(count * 4).order(ByteOrder.nativeOrder()).asFloatBuffer();
    }

    private static int framebuffer(GL3 gl, int samples, int[] storage) {
        int[] name = new int[1];
        gl.glGenFramebuffers(1, name, 0);
        gl.glGenRenderbuffers(1, storage, 0);
        gl.glBindRenderbuffer(GL.GL_RENDERBUFFER, storage[0]);
        if (samples == 0) gl.glRenderbufferStorage(GL.GL_RENDERBUFFER, GL.GL_RGBA8, 256, 256);
        else gl.glRenderbufferStorageMultisample(GL.GL_RENDERBUFFER, samples, GL.GL_RGBA8, 256, 256);
        gl.glBindFramebuffer(GL.GL_FRAMEBUFFER, name[0]);
        gl.glFramebufferRenderbuffer(GL.GL_FRAMEBUFFER, GL.GL_COLOR_ATTACHMENT0, GL.GL_RENDERBUFFER, storage[0]);
        if (gl.glCheckFramebufferStatus(GL.GL_FRAMEBUFFER) != GL.GL_FRAMEBUFFER_COMPLETE)
            throw new IllegalStateException("incomplete color framebuffer");
        int[] observed = new int[1];
        gl.glGetRenderbufferParameteriv(GL.GL_RENDERBUFFER, GL3.GL_RENDERBUFFER_SAMPLES, observed, 0);
        if (observed[0] != samples) throw new IllegalStateException("requested sample count was not provided");
        return name[0];
    }

    @Override public void display(GLAutoDrawable drawable) {
        if (next >= fixtures.size()) return;
        GL3 gl = drawable.getGL().getGL3();
        Path source = fixtures.get(next++);
        String stem = source.getFileName().toString().replace(".bin", "");
        try {
            int vertices;
            int faces;
            FloatBuffer data;
            IntBuffer indices;
            try (InputStream stream = Files.newInputStream(source); DataInputStream in = new DataInputStream(stream)) {
                if (in.readInt() != 23) throw new IllegalArgumentException("only frozen unlit v23 fixtures supported");
                vertices = in.readInt();
                faces = in.readInt();
                if (vertices <= 0 || vertices > 20000 || faces <= 0 || faces > 40000)
                    throw new IllegalArgumentException("fixture exceeds bounded original mesh shape");
                for (int i = 0; i < 4; i++) in.readInt();
                float[] positions = new float[vertices * 3];
                for (int i = 0; i < positions.length; i++) positions[i] = in.readFloat();
                data = floats(vertices * 7);
                for (int i = 0; i < vertices; i++) {
                    int rgba = in.readInt();
                    if ((rgba & 255) != 255) throw new IllegalArgumentException("nonopaque original color");
                    data.put(positions[i * 3]).put(positions[i * 3 + 1]).put(positions[i * 3 + 2]);
                    data.put((rgba >>> 24) / 255.0f).put(((rgba >>> 16) & 255) / 255.0f)
                        .put(((rgba >>> 8) & 255) / 255.0f).put(1.0f);
                }
                data.flip();
                indices = ByteBuffer.allocateDirect(faces * 12).order(ByteOrder.nativeOrder()).asIntBuffer();
                for (int i = 0; i < faces * 3; i++) {
                    int index = in.readInt();
                    if (index < 0 || index >= vertices) throw new IllegalArgumentException("invalid original face index");
                    indices.put(index);
                }
                indices.flip();
                if (in.read() != -1) throw new IllegalArgumentException("unexpected fixture tail");
            }
            int samples = stem.endsWith("BALANCED") ? 4 : 0;
            int[] renderStorage = new int[1];
            int[] resolveStorage = new int[1];
            int render = framebuffer(gl, samples, renderStorage);
            int resolve = samples == 0 ? render : framebuffer(gl, 0, resolveStorage);
            try {
                gl.glBindFramebuffer(GL.GL_FRAMEBUFFER, render);
                gl.glViewport(0, 0, 256, 256);
                if (samples > 0) gl.glEnable(GL.GL_MULTISAMPLE); else gl.glDisable(GL.GL_MULTISAMPLE);
                gl.glClearColor(1, 1, 1, 1);
                gl.glClear(GL.GL_COLOR_BUFFER_BIT);
                gl.glUseProgram(program);
                gl.glBindVertexArray(vao);
                gl.glBindBuffer(GL.GL_ARRAY_BUFFER, buffers[0]);
                gl.glBufferData(GL.GL_ARRAY_BUFFER, (long) vertices * 28, data, GL3.GL_STREAM_DRAW);
                gl.glEnableVertexAttribArray(0);
                gl.glVertexAttribPointer(0, 3, GL.GL_FLOAT, false, 28, 0L);
                gl.glEnableVertexAttribArray(1);
                gl.glVertexAttribPointer(1, 4, GL.GL_FLOAT, false, 28, 12L);
                gl.glBindBuffer(GL.GL_ELEMENT_ARRAY_BUFFER, buffers[1]);
                gl.glBufferData(GL.GL_ELEMENT_ARRAY_BUFFER, (long) faces * 12, indices, GL3.GL_STREAM_DRAW);
                gl.glDrawElements(GL.GL_TRIANGLES, faces * 3, GL.GL_UNSIGNED_INT, 0L);
                if (samples > 0) {
                    gl.glBindFramebuffer(GL3.GL_READ_FRAMEBUFFER, render);
                    gl.glBindFramebuffer(GL3.GL_DRAW_FRAMEBUFFER, resolve);
                    gl.glBlitFramebuffer(0, 0, 256, 256, 0, 0, 256, 256, GL.GL_COLOR_BUFFER_BIT, GL.GL_NEAREST);
                }
                gl.glBindFramebuffer(GL.GL_FRAMEBUFFER, resolve);
                ByteBuffer pixels = ByteBuffer.allocateDirect(256 * 256 * 4);
                gl.glReadPixels(0, 0, 256, 256, GL.GL_RGBA, GL.GL_UNSIGNED_BYTE, pixels);
                if (gl.glGetError() != GL.GL_NO_ERROR) throw new IllegalStateException("OpenGL draw/readback error");
                BufferedImage image = new BufferedImage(256, 256, BufferedImage.TYPE_INT_ARGB);
                for (int y = 0; y < 256; y++) for (int x = 0; x < 256; x++) {
                    int offset = ((255 - y) * 256 + x) * 4;
                    int r = pixels.get(offset) & 255, g = pixels.get(offset + 1) & 255;
                    int b = pixels.get(offset + 2) & 255, a = pixels.get(offset + 3) & 255;
                    image.setRGB(x, y, (a << 24) | (r << 16) | (g << 8) | b);
                }
                ImageIO.write(image, "png", output.resolve(stem + ".png").toFile());
                Files.copy(source, output.resolve(source.getFileName()), StandardCopyOption.REPLACE_EXISTING);
                System.out.println("frame=" + stem + " originalVertices=" + vertices + " originalFaces=" + faces + " samples=" + samples);
            } finally {
                gl.glDeleteFramebuffers(1, new int[] {render}, 0);
                gl.glDeleteRenderbuffers(1, renderStorage, 0);
                if (samples > 0) {
                    gl.glDeleteFramebuffers(1, new int[] {resolve}, 0);
                    gl.glDeleteRenderbuffers(1, resolveStorage, 0);
                }
            }
        } catch (Exception error) {
            throw new IllegalStateException("fixture " + stem, error);
        }
    }

    @Override public void reshape(GLAutoDrawable drawable, int x, int y, int width, int height) {}
    @Override public void dispose(GLAutoDrawable drawable) {
        GL3 gl = drawable.getGL().getGL3();
        gl.glDeleteBuffers(2, buffers, 0);
        gl.glDeleteVertexArrays(1, new int[] {vao}, 0);
        gl.glDeleteProgram(program);
    }

    public static void main(String[] args) throws Exception {
        Path output = Path.of(args[1]);
        Files.createDirectories(output);
        NativeAffineProbe probe = new NativeAffineProbe(Path.of(args[0]), output);
        GLProfile profile = GLProfile.get(GLProfile.GL3);
        GLCapabilities capabilities = new GLCapabilities(profile);
        capabilities.setHardwareAccelerated(true);
        capabilities.setDoubleBuffered(false);
        GLOffscreenAutoDrawable drawable = GLDrawableFactory.getFactory(profile)
            .createOffscreenAutoDrawable(null, capabilities, null, 64, 64);
        try {
            drawable.addGLEventListener(probe);
            while (probe.next < probe.fixtures.size()) drawable.display();
            System.out.println("PASS 48 original-triangle GPU frames; SurfaceRenderPlan provider remains unimplemented");
        } finally {
            drawable.destroy();
        }
    }
}
