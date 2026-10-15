import java.lang.instrument.Instrumentation;

/** Test-only shallow object sizing for the identity-deduplicated graph audit. */
public final class PhrfMemoryAgent {
    private static Instrumentation instrumentation;
    public static void premain(String arguments, Instrumentation value) {
        instrumentation = value;
    }
    public static long sizeOf(Object value) {
        if (instrumentation == null) throw new IllegalStateException("agent not attached");
        return instrumentation.getObjectSize(value);
    }
}
