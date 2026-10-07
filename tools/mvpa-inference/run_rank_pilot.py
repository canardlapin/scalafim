#!/usr/bin/env python3
"""Execute only a source-frozen, bounded rank pilot; never confirmation.

A dedicated worktree server holds fixed environment paths. Each cell replaces
only the case-list contents after the preceding command has completed. Records
append to one immutable campaign stream, then are copied to per-cell receipts.
"""
import argparse
import gzip
import json
import os
from pathlib import Path
import signal
import subprocess
import tarfile
import time

from calibration_protocol import canonical, file_locks, seed_record, summarize, validate_manifest
from run_calibration import write_once


def shutdown(root, log):
    subprocess.run(['python3', 'tools/build/sbt-warm', '--shutdown'], cwd=root,
                   stdout=log, stderr=subprocess.STDOUT, check=True, timeout=60)


def monitor(root, command, env, log, limits):
    started = time.monotonic()
    peak = 0
    samples = 0
    server = None
    violation = None
    worker = subprocess.Popen(command, cwd=root, env=env, stdout=log,
                              stderr=subprocess.STDOUT, start_new_session=True)
    try:
        while worker.poll() is None:
            if time.monotonic() - started > limits['max_wall_seconds']:
                violation = 'wall-limit'
                break
            active = root / 'project/target/active.json'
            if server is None and active.exists():
                uri = json.loads(active.read_text()).get('uri', '')
                if uri.startswith('local://'):
                    found = subprocess.run(['lsof', '-t', '-a', '-U', uri[8:]], capture_output=True, text=True)
                    ids = {int(v) for v in found.stdout.split() if v.isdigit()}
                    if len(ids) == 1:
                        server = ids.pop()
            if server is not None:
                result = subprocess.run(['ps', '-p', str(server), '-o', 'rss='], capture_output=True, text=True)
                if result.stdout.strip():
                    peak = max(peak, int(result.stdout.strip()) * 1024)
                    samples += 1
                    if peak > limits['max_rss_bytes']:
                        violation = 'rss-limit'
                        break
            time.sleep(.2)
    except Exception as error:
        violation = 'monitoring-error: ' + str(error)
    finally:
        if violation:
            if server is not None:
                try:
                    os.kill(server, signal.SIGTERM)
                except ProcessLookupError:
                    pass
            try:
                os.killpg(worker.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
        try:
            code = worker.wait(timeout=15)
        except subprocess.TimeoutExpired:
            os.killpg(worker.pid, signal.SIGKILL)
            code = worker.wait()
    if not samples:
        violation = violation or 'server-rss-unobserved'
    return dict(worker_exit=code, elapsed_seconds=time.monotonic()-started, server_pid=server,
                rss_samples=samples, observed_peak_rss_bytes=peak, resource_refusal=violation,
                rss_scope='Resident sbt JVM; sampled every 0.2s. Excludes thin client and generator.',
                command=command)


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--manifest', type=Path, required=True)
    p.add_argument('--packet', type=Path, required=True)
    args = p.parse_args()
    root = Path(__file__).resolve().parents[2]
    manifest = json.loads(args.manifest.read_text())
    validate_manifest(manifest, root, 'pilot')
    assert manifest['status'] == 'source-frozen-expanded-pilot-only'
    approval = manifest['resource_approval']
    assert approval['worker_processes'] == 1 and approval['cpus'] == 4
    assert approval['max_wall_seconds'] <= 900 and approval['max_rss_bytes'] <= 3*1024**3
    cells = manifest['cells']
    assert [c['id'] for c in cells] == approval['scope'] and len(cells) == 61
    assert all(c['procedure'] == 'rank' and c['definition_status'] == 'frozen' for c in cells)
    packet = args.packet.resolve()
    packet.mkdir(parents=True, exist_ok=True)
    scratch = root / 'target/umvpa-expanded-pilot'
    scratch.mkdir(parents=True, exist_ok=False)
    inputs = scratch / 'current-case-files.txt'
    records = scratch / 'records.jsonl'
    records.touch(exist_ok=False)
    env = dict(os.environ, LC_ALL='C', LANG='C', SBT_WARM_HEAP='2g', SBT_WARM_CPUS='4',
               SCALAFIM_CALIBRATION_CASE_LIST=str(inputs), SCALAFIM_CALIBRATION_OUTPUT=str(records),
               OMP_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1', VECLIB_MAXIMUM_THREADS='1')
    env.pop('SCALAFIM_CALIBRATION_CASE_FILE', None)
    commit = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=root, text=True).strip()
    command = ['python3', 'tools/build/sbt-warm',
               'mvpaJVM/testOnly scalafim.fmri.mvpa.inference.RankConfirmationSuite']
    started = time.monotonic()
    previous_group = None
    completed = []
    with (packet/'server-lifecycle.log').open('xb') as lifecycle:
        shutdown(root, lifecycle)
        try:
            for cell in cells:
                if time.monotonic()-started > approval['campaign_max_wall_seconds']:
                    raise RuntimeError('campaign wall limit; remaining cells are missing, never replaced')
                validate_manifest(manifest, root, 'pilot')
                parameters = cell['parameters']
                group = tuple(parameters[key] for key in ('n','p','q','nuisance'))
                if previous_group is not None and group != previous_group:
                    shutdown(root, lifecycle)
                previous_group = group
                destination = packet / cell['id']
                destination.mkdir(exist_ok=False)
                assignments = destination/'assignments.tsv'
                with assignments.open('x') as out:
                    out.write('phase\tscenario_id\tdataset_index\troot_seed64\tsha256_utf8\tnoise_seed64\t' + '\t'.join('r_state'+str(i) for i in range(1,7))+'\n')
                    for i in range(200):
                        s = seed_record('pilot',cell['id'],i)
                        out.write('\t'.join(map(str, ['pilot',cell['id'],i,s['root_seed64'],s['sha256_utf8'],s['child_seeds64']['noise']]+s['r_noise_state']))+'\n')
                data = scratch/cell['id']
                generation = ['/usr/local/bin/Rscript','tools/mvpa-inference/generate_known_truth.R',
                              '--emit-case-batch','--manifest',str(args.manifest.resolve()),
                              '--cell',cell['id'],'--seed-records',str(assignments),'--phase','pilot',
                              '--draws','199','--out-dir',str(data)]
                generation_started = time.monotonic()
                with (destination/'generator.log').open('xb') as log:
                    generated = subprocess.run(generation,cwd=root,env=env,stdout=log,stderr=subprocess.STDOUT,timeout=120)
                write_once(destination/'generation-receipt.json',dict(command=generation,exit_code=generated.returncode,
                    elapsed_seconds=time.monotonic()-generation_started,source_commit=commit))
                if generated.returncode:
                    raise RuntimeError('generation failed; retained unchanged')
                paths = sorted(data.glob('dataset-*.tsv'))
                assert len(paths)==200
                inputs.write_text(''.join(str(path)+'\n' for path in paths))
                write_once(destination/'input-sha256.json',file_locks(root,[str(path.relative_to(root)) for path in paths]))
                # Exact inputs are retained in small per-cell archives, as well as seed receipts.
                with tarfile.open(destination/'inputs.tar.gz','w:gz',compresslevel=6) as archive:
                    for path in sorted(data.iterdir()):
                        archive.add(path,arcname=path.name)
                offset = records.stat().st_size
                with (destination/'worker.log').open('xb') as log:
                    receipt = monitor(root,command,env,log,approval)
                with records.open('rb') as source:
                    source.seek(offset)
                    raw = source.read()
                with gzip.open(destination/'records.jsonl.gz','xb') as out:
                    out.write(raw)
                rows = [json.loads(line) for line in raw.splitlines()]
                summary = summarize(cell,'pilot',rows)
                write_once(destination/'summary.json',summary)
                validate_manifest(manifest, root, 'pilot')
                receipt.update(cell=cell['id'],phase='pilot',source_commit=commit,expected_records=200,
                    records_written=len(rows),resource_limits=approval,source_locks=manifest['source_locks'],
                    scientific_release='unavailable',status='completed' if receipt['worker_exit']==0 and not receipt['resource_refusal'] and summary['outcome']=='ready-for-independent-rate-adjudication' else 'failed-or-incomplete')
                write_once(destination/'process-receipt.json',receipt)
                print(json.dumps(dict(cell=cell['id'],status=receipt['status'],seconds=receipt['elapsed_seconds'],rss=receipt['observed_peak_rss_bytes'],datasets=len(rows))),flush=True)
                completed.append(cell['id'])
                if receipt['status'] != 'completed':
                    raise RuntimeError('pilot failure retained; no retry or seed replacement')
        finally:
            shutdown(root,lifecycle)
            write_once(packet/'campaign-receipt.json',dict(source_commit=commit,completed_cells=completed,
                missing_cells=[c['id'] for c in cells if c['id'] not in completed],
                elapsed_seconds=time.monotonic()-started,scientific_release='unavailable'))


if __name__ == '__main__':
    main()
