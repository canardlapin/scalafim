#!/usr/bin/env python3
"""Replay the bounded macOS ARM64 JOGL context/color spike; no provider admission."""
from pathlib import Path
import argparse
import hashlib
import json
import platform
import shutil
import subprocess
import sys
import tarfile
import time
import urllib.request

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]

def command(args, log, timeout):
    started = time.monotonic()
    with log.open('w') as stream:
        try:
            result = subprocess.run(args, stdout=stream, stderr=subprocess.STDOUT, timeout=timeout)
            code = result.returncode
        except subprocess.TimeoutExpired:
            code = 124
    return dict(command=args, exit_code=code, seconds=round(time.monotonic()-started, 3))

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--work', type=Path, required=True)
    parser.add_argument('--context-only', action='store_true')
    parser.add_argument('--fixtures', type=Path, help='optional byte-identical SurfaceRenderPlan-produced v23 fixtures')
    args = parser.parse_args()
    if platform.system() != 'Darwin' or platform.machine() != 'arm64':
        raise SystemExit('Unsupported: this frozen runtime closure is macOS ARM64 only')
    work = args.work.resolve()
    if work.exists() and any(work.iterdir()):
        raise SystemExit('Use an empty work directory so old frames cannot satisfy a new run')
    work.mkdir(parents=True, exist_ok=True)
    runtime = work/'runtime'; runtime.mkdir()
    lock = json.loads((HERE/'runtime.lock.json').read_text())
    for artifact in lock:
        target = runtime/artifact['file']
        urllib.request.urlretrieve(artifact['url'], target)
        if hashlib.sha256(target.read_bytes()).hexdigest() != artifact['sha256']:
            raise SystemExit('Runtime digest mismatch: '+artifact['file'])
    java_home = subprocess.check_output(['/usr/libexec/java_home', '-v', '21'], text=True).strip()
    java = Path(java_home)/'bin'
    classes = work/'classes'; classes.mkdir()
    cp = ':'.join(str(p) for p in sorted(runtime.glob('*.jar')))
    compile_args = [str(java/'javac'), '-Xlint:all', '-cp', cp, '-d', str(classes),
                    str(HERE/'NativeContextProbe.java'), str(HERE/'NativeAffineProbe.java')]
    runs = [command(compile_args, work/'compile.log', 30)]
    if runs[-1]['exit_code']:
        raise SystemExit('Setup failed: Java compilation; see '+str(work/'compile.log'))
    launch = [str(java/'java'), '-XstartOnFirstThread', '-Djava.awt.headless=true', '-Xmx256m',
              '-cp', str(classes)+':'+cp, 'com.jogamp.newt.util.MainThread']
    runs.append(command(launch+['NativeContextProbe', str(work/'context.json')], work/'context.log', 45))
    if runs[-1]['exit_code']:
        (work/'runs.json').write_text(json.dumps(runs, indent=2)+'\n')
        raise SystemExit('Unsupported native closure: context/readback failed; see '+str(work))
    if args.context_only:
        (work/'runs.json').write_text(json.dumps(runs, indent=2)+'\n')
        print('PassContextOnly; no SurfaceRenderPlan or scientific qualification')
        return
    inputs = work/'inputs'; inputs.mkdir()
    archive = ROOT/'docs/verification/javafx-stock-metal-20260929-evidence.tar.gz'
    with tarfile.open(archive) as bundle:
        originals = [m for m in bundle.getmembers() if m.isfile()
                     and 'fx-24.0.1-es2-archived-control' in m.name and m.name.endswith('.bin')]
        if len(originals) != 48:
            raise SystemExit('Setup failed: frozen archive must contain exactly 48 originals')
        for member in originals:
            target = inputs/Path(member.name).name
            original = bundle.extractfile(member).read()
            if args.fixtures:
                candidate = args.fixtures.resolve()/target.name
                if candidate.read_bytes() != original:
                    raise SystemExit('Plan fixture differs from frozen original: '+target.name)
                shutil.copy2(candidate, target)
            else:
                target.write_bytes(original)
    if args.fixtures and len(list(args.fixtures.glob('*.bin'))) != 48:
        raise SystemExit('Setup failed: expected exactly 48 plan fixtures')
    runs.append(command(launch+['NativeAffineProbe', str(inputs), str(work/'frames')], work/'native-affine.log', 90))
    if runs[-1]['exit_code']:
        (work/'runs.json').write_text(json.dumps(runs, indent=2)+'\n')
        raise SystemExit('Unsupported native closure: original-triangle draw failed')
    python = work/'python'
    subprocess.run([sys.executable, '-m', 'venv', str(python)], check=True)
    interpreter = str(python/'bin/python')
    runs.append(command([interpreter, '-m', 'pip', 'install', '--disable-pip-version-check',
                         'numpy==2.2.6', 'Pillow==11.3.0'], work/'python-install.log', 120))
    if runs[-1]['exit_code']:
        raise SystemExit('Setup failed: isolated oracle dependencies')
    runs.append(command([interpreter, str(ROOT/'tools/surface-view/javafx-affine-color/check-oracle.py'), str(work/'frames')],
                        work/'oracle-self-check.log', 30))
    runs.append(command([interpreter, str(ROOT/'tools/surface-view/javafx-affine-color/oracle.py'), str(work/'frames')],
                        work/'oracle.log', 180))
    (work/'runs.json').write_text(json.dumps(runs, indent=2)+'\n')
    if any(r['exit_code'] for r in runs):
        raise SystemExit('Color kernel unqualified; preserve failed receipts in '+str(work))
    print('PassColorKernelOnly: 48 frozen fixtures; full provider, scientific workflow and consumer admission pending')

if __name__ == '__main__':
    main()
