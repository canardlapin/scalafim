import json, sys
from pathlib import Path
p = json.loads(Path(sys.argv[1]).read_text())
out = Path(sys.argv[2])
out.mkdir(exist_ok=True)
def xs(x): return x if isinstance(x,list) else [x]
def vector(x): return 'Vector('+','.join(repr(float(v)) for v in xs(x))+')'
def mat(x):
    data=','.join(repr(float(v)) for v in x['data'])
    chunks=[data[i:i+4000] for i in range(0,len(data),4000)]
    return f'matrix({x["rows"]}, {x["cols"]}, Vector('+','.join(json.dumps(c) for c in chunks)+').mkString)'
def val(name,value): return f'  private val {name} = {value}\n'
source='''package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix}

/** Temporary observation probe: confirms the audited old semantics, not a CI admission for them. */
class FmriArRecentChangesProbeSuite extends munit.FunSuite:
  private def value[A](v: Either[ArError, A]): A = v.fold(e => fail(e.message), identity)
  private def matrix(rows: Int, cols: Int, csv: String): DMat =
    val entries = csv.split(",").map(_.toDouble)
    Matrix.tabulate(rows, cols)((r, c) => entries(r * cols + c))
  private def entries(m: DMat): Vector[Double] = Vector.tabulate(m.rows * m.cols)(i => m(i / m.cols, i % m.cols))
  private def close(actual: Vector[Double], expected: Vector[Double], tol: Double = 1e-10): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).foreach((a, e) => assertEqualsDouble(a, e, tol))
  private def report(name: String, actual: Vector[Double], reference: Vector[Double]): Unit =
    println(s"CROSSREF|$name|${actual.mkString(",")}|${reference.mkString(",")}")
'''
source+=val('bicInput',mat(p['bic']['input']))
source+=val('diagInput',mat(p['diagnostics']['input']))
source+=val('poolInput',mat(p['pooling']['input']))
source+=val('biasInput',mat(p['bias']['input']))
source+=val('biasDesign',mat(p['bias']['design']))
source+=val('biasCensor','Set('+','.join(map(str,p['bias']['censor_zero']))+')')
source+=f'''  test("BIC reproduces the old order instead of the corrected penalty"):
    val plan = value(ArEstimation.fitNoise(bicInput, TimeSegments.continuous(200), ArFitOptions(order = ArOrder.Auto(6))))
    assertEquals(plan.arOrder, {p['bic']['old_order']})
    report("bic-order", Vector(plan.arOrder.toDouble), Vector({p['bic']['new_order']}.0))

  test("mean and median diagnostics reproduce aggregation before ACF"):
'''
for mode in ['Mean','Median']:
    k=mode.lower()
    source+=f'''    val {k} = entries(AcorrDiagnostics.compute(diagInput, 3, AcfAggregation.{mode}).acf)
    close({k}, {vector(p['diagnostics']['old_'+k])})
    report("diagnostics-{k}", {k}, {vector(p['diagnostics']['new_'+k])})
'''
source+=f'''
  test("run-aware unaggregated diagnostics also use the old pair-count normalization"):
    val actual = entries(value(AcorrDiagnostics.compute(diagInput, TimeSegments.fromRunLengths(Vector(40, 40)), 3, AcfAggregation.None)).acf)
    close(actual, entries({mat(p['diagnostics']['old_run_none'])}))
    report("diagnostics-run-none", actual, entries({mat(p['diagnostics']['new_run_none'])}))

  test("global AR2 pooling reproduces coefficient averaging rather than pooled correlations"):
    val segments = TimeSegments.fromRunLengths(Vector(40, 60, 30))
    val plan = value(ArEstimation.fitNoise(poolInput, segments, ArFitOptions(order = ArOrder.Fixed(2))))
    val actual = plan.coefficients.head.phi
    close(actual, {vector(p['pooling']['old_phi'])})
    report("global-pooling-ar2", actual, {vector(p['pooling']['new_phi'])})
    val ar1 = value(ArEstimation.fitNoise(poolInput, segments, ArFitOptions(order = ArOrder.Fixed(1)))).coefficients.head.phi
    close(ar1, {vector(p['pooling']['new_ar1'])})
    report("global-pooling-ar1-control", ar1, {vector(p['pooling']['new_ar1'])})
'''
for name, key in [('ar2','ar2_whitening'),('arma','arma_whitening'),('ar1','ar1_whitening')]:
    w=p[key]
    source+=f'''
  test("{name} stationary-start covariance compared with dense Cholesky"):
    val covariance = {mat(w['covariance'])}
    val exact = {mat(w['exact'])}
    val segments = TimeSegments.fromRunLengths(Vector(6, 6))
    val plan = value(WhiteningPlan.global(ArmaCoefficients({vector(w['phi'])}, {vector(w['theta']) if xs(w['theta']) else 'Vector.empty'}), segments))
    val identity = Matrix.tabulate(12, 12)((r, c) => if r == c then 1.0 else 0.0)
    val actual = value(WhiteningTransform.matrix(plan, identity))
    val transformed = actual * covariance * actual.t
    val exactCov = exact * covariance * exact.t
    val diagonal = Vector.tabulate(6)(i => transformed(i, i))
    val reference = Vector.tabulate(6)(i => exactCov(i, i))
    close(reference, Vector.fill(6)(1.0))
    {'close(entries(actual), entries(exact))' if name == 'ar1' else 'assert(math.abs(diagonal.head - 1.0) > 0.1)'}
    report("{name}-startup-variance", diagonal, reference)
    report("{name}-whitening-max-difference", Vector(entries(actual).zip(entries(exact)).map((a,e) => math.abs(a-e)).max), Vector(0.0))
'''
source+=f'''
  test("lag25 correction reproduces the exact old solve and the bias map itself is unchanged"):
    val layout = value(NoiseEstimationLayout.excludingRows(TimeSegments.fromRunLengths(Vector(150, 150)), 300, biasCensor))
    val options = ArFitOptions(order = ArOrder.Fixed(1))
    val corrected = value(NoiseFit.estimate(biasInput, layout, options, EstimationPolicy.DesignCorrected(biasDesign, CorrectionBudget.Fixed(25))))
    val actual = corrected.plan.coefficients.head.phi
    close(actual, {vector(p['bias']['old_phi'])}, 1e-8)
    report("bias-lag25", actual, {vector(p['bias']['new_phi'])})
    val matrices = corrected.biasMatrices.get.byRun
'''
for i,m in enumerate(p['bias']['matrices'].values()):
    source+=f'''    close(entries(matrices({i})), entries({mat(m)}), 1e-10)
'''
source+=f'''
  test("adaptive AR1 and AR2 correction budgets cannot activate the new tail solve"):
    val layout = value(NoiseEstimationLayout.excludingRows(TimeSegments.fromRunLengths(Vector(150, 150)), 300, biasCensor))
    val corrected = value(NoiseFit.estimate(biasInput, layout, ArFitOptions(order = ArOrder.Fixed(1)), EstimationPolicy.DesignCorrected(biasDesign, CorrectionBudget.Adaptive(25))))
    assertEquals(corrected.biasMatrices.get.lag, 5)
    close(corrected.plan.coefficients.head.phi, {vector(p['bias']['new_lag5_phi'])})
    assertEquals(value(AcvfBias.prepare(biasDesign, layout, CorrectionBudget.Adaptive(25), 2)).matrices.lag, 5)
    report("bias-adaptive5-control", corrected.plan.coefficients.head.phi, {vector(p['bias']['new_lag5_phi'])})

  test("fixed order, unit MA roots and zero terminal AR coefficients have checked boundaries"):
    val fixed = value(ArEstimation.fitNoise(bicInput, TimeSegments.continuous(200), ArFitOptions(order = ArOrder.Fixed(8))))
    assertEquals(fixed.arOrder, 8)
    assert(WhiteningPlan.global(ArmaCoefficients(Vector.empty, Vector(-1.0)), TimeSegments.continuous(10)).isLeft)
    val stable = value(Pacf.enforceStationaryChecked(Vector(1.5, 0.0), StationarityBound.Default))
    value(Pacf.validateStationary(stable))
    assertEquals(stable.length, 2)
    report("fixed8-unit-root-zero-leading-controls", Vector(fixed.arOrder.toDouble, stable.head, stable.last), Vector(8.0))
'''
import re
source = re.sub(r'value\(WhiteningPlan.global\((.+?), segments\)\)', r'value(WhiteningPlan.withScope(CoefficientScope.Global(\1), segments))', source)
source = source.replace('WhiteningPlan.global(ArmaCoefficients(Vector.empty, Vector(-1.0)), TimeSegments.continuous(10)).isLeft', 'WhiteningPlan.withScope(CoefficientScope.Global(ArmaCoefficients(Vector.empty, Vector(-1.0))), TimeSegments.continuous(10)).isLeft')
(out/'FmriArRecentChangesProbeSuite.scala').write_text(source)
print('Generated', out/'FmriArRecentChangesProbeSuite.scala', 'bytes',len(source))
