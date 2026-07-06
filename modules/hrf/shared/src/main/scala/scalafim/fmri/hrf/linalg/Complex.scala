package scalafim.fmri.hrf.linalg

private[hrf] final case class Complex(re: Double, im: Double):
  def +(o: Complex): Complex = Complex(re + o.re, im + o.im)
  def -(o: Complex): Complex = Complex(re - o.re, im - o.im)
  def *(o: Complex): Complex =
    Complex(re * o.re - im * o.im, re * o.im + im * o.re)
  def scale(k: Double): Complex = Complex(re * k, im * k)
  def conj: Complex = Complex(re, -im)

private[hrf] object Complex:
  val zero: Complex = Complex(0.0, 0.0)
  def expi(theta: Double): Complex = Complex(math.cos(theta), math.sin(theta))
