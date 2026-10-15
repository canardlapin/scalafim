#!/usr/bin/env python3
"""Use a previously exported sbt Test/fullClasspath, no dependency downloads."""
import pathlib, re, subprocess, sys, tempfile
packet = pathlib.Path(__file__).resolve().parent
root = packet.parents[2]
lines = re.sub(r'\x1b\[[0-9;]*[A-Za-z]', '', pathlib.Path(sys.argv[1]).read_text()).splitlines()
classpath = next(line.strip() for line in reversed(lines) if line.strip().startswith('/') and '/test-classes:' in line)
with tempfile.TemporaryDirectory(prefix='phrf-memory-') as temporary:
    tmp = pathlib.Path(temporary)
    subprocess.run(['javac', '-d', str(tmp), str(packet/'PhrfMemoryAgent.java')], check=True)
    (tmp/'MANIFEST.MF').write_text('Manifest-Version: 1.0\nPremain-Class: PhrfMemoryAgent\n\n')
    subprocess.run(['jar', 'cfm', str(tmp/'agent.jar'), str(tmp/'MANIFEST.MF'), '-C', str(tmp), 'PhrfMemoryAgent.class'], check=True)
    subprocess.run(['java', '-Xmx2g', '-XX:ActiveProcessorCount=4', '-javaagent:'+str(tmp/'agent.jar'),
        '--add-opens=java.base/java.lang=ALL-UNNAMED', '--add-opens=java.base/java.util=ALL-UNNAMED',
        '-cp', classpath, 'scalafim.fmri.laws.profile.TrialReferencePlacementMain', str(packet), 'stress'],
        cwd=root, check=True)
