package scalafim.fmri.hrf

opaque type Seconds = Double

object Seconds:
  inline def apply(value: Double): Seconds = value

  extension (s: Seconds)
    inline def value: Double = s
    inline def +(o: Seconds): Seconds = s + o
    inline def -(o: Seconds): Seconds = s - o
    inline def *(k: Double): Seconds = s * k
    inline def /(k: Double): Seconds = s / k

  given Ordering[Seconds] with
    def compare(a: Seconds, b: Seconds): Int =
      java.lang.Double.compare(a.value, b.value)

extension (d: Double)
  inline def s: Seconds = Seconds(d)
  inline def value: Double = d

extension (i: Int)
  inline def s: Seconds = Seconds(i.toDouble)

extension (l: Long)
  inline def s: Seconds = Seconds(l.toDouble)
