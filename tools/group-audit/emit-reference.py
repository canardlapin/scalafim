from pathlib import Path
import csv,json
base=Path.cwd()
rows=list(csv.reader((base/'reference.csv').open()))[1:]
(base/'GroupAuditReference.scala').write_text('package scalafim.fmri.group\nobject GroupAuditReference:\n  val rows = Vector('+','.join('Vector('+','.join(json.dumps(x) for x in row)+')' for row in rows)+')\n')
