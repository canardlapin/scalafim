"""Independent decimal/rational full-orbit oracle; no Scala/library formula calls."""
import itertools
import json
from decimal import Decimal, getcontext
from fractions import Fraction
from pathlib import Path

getcontext().prec = 60
y = [2, -7, 11, 4, -3, 8, 1, 6]
v = [1, 2, 4, 8, 1, 2, 4, 8]
results = []
for name in ["EqualSubjects", "FixedInverseVariance"]:
    weights = [Fraction(1, x if name == "FixedInverseVariance" else 1) for x in v]
    terms = [w * x for w, x in zip(weights, y)]
    observed = abs(sum(terms))
    distribution = [abs(sum(s * x for s, x in zip(signs, terms)))
                    for signs in itertools.product([-1, 1], repeat=len(y))]
    exceedances = sum(x >= observed for x in distribution)
    estimate = sum(terms) / sum(weights)
    numerator = sum(terms)
    squared = sum(x * x for x in terms)
    score = (Decimal(numerator.numerator) / Decimal(numerator.denominator)
             / (Decimal(squared.numerator) / Decimal(squared.denominator)).sqrt())
    results.append(dict(weighting=name, exceedances=exceedances, total=len(distribution),
                        p=exceedances/len(distribution), estimate=float(estimate), score=str(score)))
out = Path(__file__).with_name("reference.json")
out.write_text(json.dumps(dict(effects=y, variances=v, results=results), indent=2) + "\n")
print(out.read_text())
