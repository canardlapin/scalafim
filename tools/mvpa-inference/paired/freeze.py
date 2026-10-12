#!/usr/bin/env python3
"""Freeze all paired-pilot assignments before any pilot input is generated."""
import argparse
import gzip
import hashlib
import json
from pathlib import Path
import re
import subprocess
from protocol import *


def runtime_inventory(root,log):
    text=Path(log).read_text()
    paths=re.findall(r'Attributed\(([^\n)]+)\)',text)
    if not paths: raise ValueError('resolved Test/fullClasspath output missing')
    files={}
    for raw in paths:
        p=Path(raw)
        if p.is_file(): files[str(p)]=sha(p)
        elif p.is_dir():
            for f in sorted(p.rglob('*')):
                if f.is_file(): files[str(f)]=sha(f)
        else: raise ValueError('missing classpath entry: '+raw)
    # Classpath order matters even when all bytes match.
    return dict(ordered_entries=paths,files=files,
                ordered_identity=hashlib.sha256(json.dumps(paths,separators=(',',':')).encode()).hexdigest())


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--historical',type=Path,required=True)
    p.add_argument('--output',type=Path,required=True)
    p.add_argument('--current-classpath',type=Path,required=True)
    p.add_argument('--historical-classpath',type=Path,required=True)
    p.add_argument('--probe',type=Path,required=True)
    a=p.parse_args();root=Path(__file__).resolve().parents[3];historical=a.historical.resolve()
    proposal=json.loads((root/'docs/verification/umvpa-rank-integration-20261008/paired-inventory-proposal.json').read_text())
    cells=inventory(proposal);validate_inventory(cells,proposal)
    probe=json.loads(a.probe.read_text())
    if not probe.get('preflight_pass'): raise ValueError('resource/adapter preflight must pass before pilot freeze')
    sources={}
    for name,repo in [('current',root),('historical',historical)]:
        verify_sources(repo,probe['source_snapshots'][name]['locks'])
    for name,repo,classpath in [('current',root,a.current_classpath),('historical',historical,a.historical_classpath)]:
        locks=source_locks(repo)
        changed=subprocess.check_output(['git','diff','HEAD','--name-only','--',*locks],cwd=repo,text=True)
        if changed.strip(): raise ValueError('uncommitted source: '+changed)
        commit=subprocess.check_output(['git','rev-parse','HEAD'],cwd=repo,text=True).strip()
        provider=re.search(r'lazy val multivarRevision = "([0-9a-f]{40})"',(repo/'build.sbt').read_text()).group(1)
        if provider!=PROVIDERS[name]: raise ValueError('wrong provider revision')
        sources[name]=dict(commit=commit,provider_revision=provider,locks=locks,runtime=runtime_inventory(repo,classpath))
    assignments=[assignment('pilot',c['id'],i) for c in cells for i in range(200)]
    if len({s['root_seed64'] for s in assignments})!=12800 or len({tuple(s['r_noise_state']) for s in assignments})!=12800:
        raise ValueError('stream collision')
    raw=(''.join(json.dumps(s,sort_keys=True,separators=(',',':'))+'\n' for s in assignments)).encode()
    compressed=gzip.compress(raw,mtime=0)
    seed_path=a.output.with_name('assignments.jsonl.gz')
    with seed_path.open('xb') as f:f.write(compressed)
    manifest=dict(schema='scalafim-rank-paired-pilot/v1',role='exploratory-pilot-only',namespace=NAMESPACE,
        cells=cells,methods=METHODS,limits=LIMITS,sources=sources,
        assignments_file=seed_path.name,assignments_sha256=sha(seed_path),
        probe_file=str(a.probe.resolve()),probe_sha256=sha(a.probe),
        paired_policy='All methods receive the identical archived bytes. Historical and repaired permutation share their predeclared transform stream; Gaussian hypothesis streams differ.',
        failure_policy='Any source/resource/engineering failure stops the campaign; retain every assigned, failed and missing slot. No retries or replacements.',
        analysis_policy='First-true-null raw rejection, closed false rank overestimation, closed H3 power in standard R3, all stage decisions, paired method differences with descriptive uncertainty. No scientific admission.',
        original_inventory_sha256=sha(root/'docs/verification/umvpa-rank-integration-20261008/paired-inventory-proposal.json'))
    write_once(a.output,manifest)
    print(json.dumps(dict(cells=64,datasets=12800,method_evaluations=38400,assignments_sha256=sha(seed_path))))

if __name__=='__main__':main()
