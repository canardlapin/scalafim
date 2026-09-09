"""Summarize retained CSV counts; account for shared Monte Carlo plans."""
import collections
import csv
import json
import math
import sys
from pathlib import Path
from scipy.stats import beta, t

groups = collections.defaultdict(list)
keys = ("method", "n", "df", "tau", "errors", "precision", "question")
for row in csv.DictReader(Path(sys.argv[1]).open()):
    groups[tuple(row[k] for k in keys)].append(row)
family = sum(key[0] != "PM-mKH" and key[-1] != "power" for key in groups)
out = []
for key, rows in groups.items():
    n = sum(int(r["studies"]) for r in rows)
    k = sum(int(r["rejections"]) for r in rows)
    p = k / n
    rates = [int(r["rejections"]) / int(r["studies"]) for r in rows]
    se = (math.sqrt(p * (1-p) / n) if len(rows) == 1 else
          math.sqrt(sum((x-p)**2 for x in rates) / (len(rows) * (len(rows)-1))))
    if len(rows) == 1:
        lo = float(beta.ppf(.025, k, n-k+1)) if k else 0.
        hi = float(beta.ppf(.975, k+1, n-k)) if k < n else 1.
        lower = float(beta.ppf(.001/family, k, n-k+1)) if k else 0.
        interval = "Clopper-Pearson binomial"
    else:
        lo = max(0., p-float(t.ppf(.975, len(rows)-1))*se)
        hi = min(1., p+float(t.ppf(.975, len(rows)-1))*se)
        lower = max(0., p-float(t.ppf(1-.001/family, len(rows)-1))*se)
        interval = "t approximation clustered by independent sign plan"
    out.append(dict(zip(keys, key), studies=n, rejected=k, failures=sum(int(r["failures"]) for r in rows),
                    rate=p, se=se, batches=len(rows), lo95=lo, hi95=hi,
                    simultaneousLower=lower, interval=interval))
Path(sys.argv[2]).write_text(json.dumps(out, indent=2) + "\n")
print(f"{len(out)} summaries; {family} primary null/coverage comparisons")
