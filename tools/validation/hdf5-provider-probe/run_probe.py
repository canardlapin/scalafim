#!/usr/bin/env python3
"""Fail-closed local macOS official-JNI probe with immutable per-command receipts."""
import argparse
import hashlib
import json
import os
import platform
import re
import signal
import subprocess
import sys
import tarfile
import time
import urllib.request
import zipfile
from datetime import datetime, timezone
from pathlib import Path

if not __debug__:
    raise RuntimeError('UnsupportedRuntime: Python optimization disables probe assertions')

HERE = Path(__file__).resolve().parent
SHA = '7402939a854b643022e239dfa544a200c048b798bb0861890010db48a72efc73'
ARCHIVE = 'hdf5-2.2.0-macos15_clang.tar.gz'
JARS = ('jarhdf5-2.2.0.jar', 'slf4j-api-2.0.16.jar', 'slf4j-nop-2.0.16.jar')
NATIVES = ('libhdf5_java.dylib', 'libhdf5.320.2.0.dylib')


def sha(path):
    h = hashlib.sha256()
    with path.open('rb') as f:
        for b in iter(lambda: f.read(1048576), b''):
            h.update(b)
    return h.hexdigest()


def write(path, value):
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + '\n')


def setup(cache):
    if platform.system() != 'Darwin' or platform.machine() != 'arm64':
        raise RuntimeError('UnsupportedHost: only this macOS ARM64 archive lane is qualified')
    cache.mkdir(parents=True, exist_ok=True)
    metadata = cache / 'release.json'
    if not metadata.exists():
        metadata.write_bytes(urllib.request.urlopen('https://api.github.com/repos/HDFGroup/hdf5/releases/tags/2.2.0', timeout=60).read())
    release = json.loads(metadata.read_text())
    assert release['tag_name'] == '2.2.0'
    assets = {a['name']: a for a in release['assets']}
    for name in (ARCHIVE, 'hdf5-2.2.0.sha256sums.txt'):
        url = assets[name]['browser_download_url']
        assert url.startswith('https://github.com/HDFGroup/hdf5/releases/download/2.2.0/')
        dest = cache / name
        if not dest.exists():
            with urllib.request.urlopen(url, timeout=60) as r, dest.open('xb') as f:
                while b := r.read(1048576):
                    f.write(b)
    archive = cache / ARCHIVE
    assert sha(archive) == SHA and archive.stat().st_size == assets[ARCHIVE]['size'], 'archive identity'
    assert f'{SHA}  {ARCHIVE}' in (cache / 'hdf5-2.2.0.sha256sums.txt').read_text(), 'publisher hash'
    # Identity checks happen before extraction, Java class loading, or any payload work.
    if not (cache / 'provider').exists():
        with tarfile.open(archive) as t:
            t.extractall(cache / 'outer', filter='data')
        inner = list((cache / 'outer').rglob('HDF5-2.2.0-Darwin.tar.gz'))
        assert len(inner) == 1
        with tarfile.open(inner[0]) as t:
            t.extractall(cache / 'provider', filter='data')
    libs = list((cache / 'provider').rglob('libhdf5_java.dylib'))
    assert len(libs) == 1
    lib = libs[0].parent
    with zipfile.ZipFile(lib / JARS[0]) as z:
        major = int.from_bytes(z.read('hdf/hdf5lib/H5.class')[6:8], 'big')
    assert major == 65, 'JNI jar requires classfile65'
    settings = (lib / 'libhdf5.settings').read_text()
    assert re.search(r'Threadsafety:\s+OFF', settings), 'threadsafety build policy changed'
    fingerprint = {
        'provider': 'HDFGroup official JNI 2.2.0', 'release_metadata_url': 'https://api.github.com/repos/HDFGroup/hdf5/releases/tags/2.2.0',
        'archive_url': assets[ARCHIVE]['browser_download_url'], 'archive_sha256': SHA,
        'archive_bytes': archive.stat().st_size, 'classfile_major': major, 'threadsafety': False,
        'lane': 'macOS ARM64; runtime JDK25, JDK21 not run',
        'artifacts': {name: {'sha256': sha(lib / name), 'bytes': (lib / name).stat().st_size} for name in JARS + NATIVES},
    }
    lock = json.loads((HERE / 'provider-lock.json').read_text())
    assert fingerprint == lock, 'extracted artifact closure differs from provider lock'
    return lib, fingerprint


def main():
    p = argparse.ArgumentParser()
    p.add_argument('--cache', type=Path, required=True)
    p.add_argument('--java-home', type=Path, required=True)
    p.add_argument('--python', type=Path, required=True, help='Existing h5py environment; never installs packages')
    p.add_argument('--logged-runner', type=Path, required=True)
    p.add_argument('--loader-only', action='store_true')
    a = p.parse_args()
    cache = a.cache.resolve()
    run = cache / ('run-' + datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%SZ') + '-' + str(os.getpid()))
    run.mkdir(parents=True)
    manifest = {'run': str(run), 'driver_pid': os.getpid(), 'started_utc': datetime.now(timezone.utc).isoformat(),
                'platform': platform.platform(), 'source_sha256': {f.name: sha(f) for f in (HERE / 'Hdf5ProviderProbe.java', HERE / 'check_hdf5_provider.py', Path(__file__), HERE / 'provider-lock.json')},
                'java_home_requested': str(a.java_home.resolve()), 'python_requested': str(a.python), 'commands': [], 'status': 'running',
                'axes': ['row', 'column'], 'value_formula': 'row*1048576 + column*0.125 - 17.25',
                'validity_formula': '(row+column)%4', 'chunks': [1, 32768], 'block_elements': 65536, 'heap_limit_bytes': 67108864,
                'unavailable': ['JDK21 execution', 'Linux execution', 'macOS x86_64 execution', 'physical disk read bytes', 'HDF5 native allocation accounting', 'power-loss durability', 'estimate schema/consumer/production packaging']}
    write(run / 'manifest.json', manifest)

    def invoke(name, command, env=None, expected=0):
        # Native jobs are synchronous and retained. Source is frozen from here through completion.
        for filename, digest in manifest['source_sha256'].items():
            assert sha(HERE / filename) == digest, 'source changed during run'
        log = run / (name + '.log')
        argv = [sys.executable, str(a.logged_runner), '--log', str(log), '--timeout', '120', '--'] + [str(x) for x in command]
        r = subprocess.run(argv, env=env)
        meta = json.loads(Path(str(log) + '.meta.json').read_text())
        raw = log.read_text(errors='replace')
        receipt = {'name': name, 'log': str(log), 'meta': meta, 'expected_exit': expected,
                   'owned_java_pids': [int(x) for x in re.findall(r'OWNED_PID (\d+)', raw)],
                   'maximum_rss_bytes': int(m.group(1)) if (m := re.search(r'OS_RUSAGE maximum_rss_bytes=(\d+)', raw)) else None,
                   'log_sha256': sha(log)}
        manifest['commands'].append(receipt)
        write(run / 'manifest.json', manifest)
        assert r.returncode == expected, (name, r.returncode, expected, log)
        return receipt

    try:
        lib, manifest['provider'] = setup(cache)
        # Complete architecture/LC_LOAD_DYLIB/LC_RPATH evidence; all output stays in receipts.
        for name in NATIVES:
            invoke('file-' + name, ['file', lib / name])
            invoke('dependencies-' + name, ['otool', '-L', lib / name])
            invoke('loadcommands-' + name, ['otool', '-l', lib / name])
        invoke('jdk-version', [a.java_home / 'bin/java', '-XshowSettings:properties', '-version'])
        invoke('oracle-version', [a.python, '-c', 'import h5py,numpy,sys; print(sys.executable,sys.version,h5py.__version__,h5py.version.hdf5_version,numpy.__version__)'])
        classes = run / 'classes'; classes.mkdir()
        cp = os.pathsep.join(str(lib / n) for n in JARS)
        invoke('compile', [a.java_home / 'bin/javac', '--release', '21', '-Xlint:all', '-Werror', '-cp', cp, '-d', classes, HERE / 'Hdf5ProviderProbe.java'])
        java = [a.java_home / 'bin/java', '-Xmx64m', '--enable-native-access=ALL-UNNAMED', '-XX:NativeMemoryTracking=summary',
                '-XX:+UnlockDiagnosticVMOptions', '-XX:+PrintNMTStatistics', '-Djava.library.path=' + str(lib), '-cp', str(classes) + os.pathsep + cp, 'Hdf5ProviderProbe']
        timed_java = [sys.executable, Path(__file__).resolve(), '--measure-child'] + java
        env = os.environ.copy()
        env['DYLD_PRINT_LIBRARIES'] = '1'
        env['HDF5_PLUGIN_PRELOAD'] = '::'  # No external third-party filter plugins.
        invoke('loader', timed_java + ['loader'], env=env)
        # Actual dyld closure includes bundled JNI/core plus JDK and shared-cache OS libraries.
        dyld = (run / 'loader.log').read_text()
        java_pids = re.findall(r'OWNED_PID (\d+)', dyld)
        assert len(java_pids) == 1
        loaded = sorted(set(re.findall(r'dyld\[' + java_pids[0] + r'\]:\s+<[^>]+>\s+([^\n]+)', dyld)))
        manifest['loaded_native_images'] = [{'path': x, 'sha256': sha(Path(x)) if Path(x).is_file() else None,
                                            'hash_unavailable_reason': None if Path(x).is_file() else 'macOS dyld shared cache image has no standalone file'} for x in loaded]
        assert any('libhdf5_java.dylib' in x for x in loaded) and any('libhdf5.320' in x for x in loaded), 'dyld JNI/core closure'
        if not a.loader_only:
            childenv = os.environ.copy(); childenv['HDF5_PLUGIN_PRELOAD'] = '::'
            small = run / 'small.h5'
            invoke('small-write', timed_java + ['small', small], env=childenv)
            invoke('small-oracle', [a.python, HERE / 'check_hdf5_provider.py', 'small', small])
            literal = run / 'literal.h5'
            invoke('literal-author', [a.python, HERE / 'check_hdf5_provider.py', 'literal', literal])
            invoke('literal-read', timed_java + ['literal', literal], env=childenv)
            invoke('failure-laws', timed_java + ['failures', run, small], env=childenv)
            # Bigger payload is admitted only after measured first 128MiB writer+reader cost.
            first_seconds = 0
            for rows in (128, 256):
                old = run / f'payload-{rows}.h5'
                start = time.monotonic()
                invoke(f'write-{rows}', timed_java + ['write', old, rows], env=childenv)
                relocated = run / f'relocated-{rows}'; relocated.mkdir()
                new = relocated / 'payload.h5'
                old.rename(new); assert not old.exists()
                invoke(f'read-{rows}', timed_java + ['read', new, rows], env=childenv)
                invoke(f'oracle-{rows}', [a.python, HERE / 'check_hdf5_provider.py', 'big', new, '--rows', rows])
                elapsed = time.monotonic() - start
                manifest[f'payload_{rows}'] = {'path': str(new), 'sha256': sha(new), 'staging_file_bytes': new.stat().st_size,
                                             'old_path_absent': not old.exists(), 'total_elapsed_seconds': elapsed}
                write(run / 'manifest.json', manifest)
                if rows == 128:
                    first_seconds = elapsed
                    if first_seconds > 60:
                        manifest['larger_payload'] = 'skipped: first lane exceeded 60 second cost budget'
                        break
            # Two task-owned processes, a barrier, exclusive native create. No broad process operations.
            barrier, racefile = run / 'race.go', run / 'race.h5'
            jobs = []
            for i in range(2):
                logfile = run / f'race-{i}.log'
                cmd = [sys.executable, str(a.logged_runner), '--log', str(logfile), '--timeout', '30', '--'] + [str(x) for x in timed_java + ['race', racefile, barrier, run / f'race-{i}.ready']]
                jobs.append(subprocess.Popen(cmd, env=childenv))
            manifest['race_wrapper_pids'] = [j.pid for j in jobs]; write(run / 'manifest.json', manifest)
            deadline = time.monotonic() + 10
            while not all((run / f'race-{i}.ready').exists() for i in range(2)):
                if time.monotonic() > deadline: raise RuntimeError('race readiness timeout; wrappers retain ownership until timeout')
                time.sleep(.02)
            barrier.write_text('go')
            returns = [j.wait() for j in jobs]
            for i in range(2):
                log = run / f'race-{i}.log'; raw = log.read_text()
                manifest['commands'].append({'name': f'race-{i}', 'log': str(log), 'log_sha256': sha(log), 'meta': json.loads(Path(str(log)+'.meta.json').read_text()),
                                             'owned_java_pids': [int(x) for x in re.findall(r'OWNED_PID (\d+)', raw)], 'expected_exit': returns[i]})
            write(run / 'manifest.json', manifest)
            assert sorted(returns) == [0, 4], returns
            race_logs = [(run / f'race-{i}.log').read_text() for i in range(2)]
            assert sum(x.count('RACE_CREATED') for x in race_logs) == 1
            assert sum(x.count('RACE_REFUSED_NATIVE_EXCL') for x in race_logs) == 1
            manifest['exclusive_creation_race'] = {'exits': returns, 'exactly_one_creator': True, 'native_exclusive_refusal': 'errno17/EEXIST from captured native error stack'}
            aborted = run / 'abort_owned_staging.h5'
            invoke('abort-owned-staging', timed_java + ['abort', aborted], env=childenv, expected=17)
            assert aborted.exists() and not (run / 'complete-unit.json').exists()
            manifest['abort'] = {'staging_bytes': aborted.stat().st_size, 'complete_unit_published': False,
                                 'scope': 'process-abort/staging only, no transaction or power-loss claim'}
        manifest['status'] = 'pass'; manifest['finished_utc'] = datetime.now(timezone.utc).isoformat()
    except BaseException as e:
        manifest['status'] = 'blocked-or-failed'; manifest['error'] = repr(e)
        write(run / 'manifest.json', manifest)
        raise
    write(run / 'manifest.json', manifest)
    print('PROBE_MANIFEST', run / 'manifest.json', 'status=' + manifest['status'])


def measure_child(argv):
    # wait4 reports this exact owned child's usage without macOS time's sysctl.
    started = time.monotonic()
    child = subprocess.Popen(argv)
    def forward(signum, _frame):
        try:
            os.kill(child.pid, signum)
        except ProcessLookupError:
            pass
    for sig in (signal.SIGTERM, signal.SIGINT):
        signal.signal(sig, forward)
    _, status, usage = os.wait4(child.pid, 0)
    child.returncode = os.waitstatus_to_exitcode(status)
    print(f'OS_RUSAGE maximum_rss_bytes={usage.ru_maxrss} input_blocks={usage.ru_inblock} output_blocks={usage.ru_oublock} elapsed_seconds={time.monotonic()-started:.6f}', flush=True)
    return child.returncode if child.returncode >= 0 else 128-child.returncode


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == '--measure-child':
        sys.exit(measure_child(sys.argv[2:]))
    main()
