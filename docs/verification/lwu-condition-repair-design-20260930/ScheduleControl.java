import java.util.Arrays;
import java.util.Random;
class ScheduleControl {
  public static void main(String[] arguments) {
    Random random = new Random(20260910L);
    double[] onsets = new double[300];
    for (int i=0; i<300; i++) onsets[i]=random.nextInt(5760)/10.0;
    Arrays.sort(onsets);
    for (double onset:onsets) System.out.println(Double.toHexString(onset));
  }
}
