"""Startup-inclusive, worktree-owned JVM sampling with bounded termination."""
import hashlib
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import time


def base_for(root):
    cache=Path(os.environ.get('SBT_WARM_HOME',Path.home()/'.cache/scalafim-sbt'))
    return cache/'bases'/hashlib.sha256(str(root.resolve()).encode()).hexdigest()[:16]


_commands = {}


def owned_jvms(root):
    marker='-Dsbt.global.base='+str(base_for(root))
    ps=subprocess.check_output(['ps','-axo','pid=,rss=,command='],text=True)
    found={}
    for line in ps.splitlines():
        parts=line.split(None,2)
        if len(parts)==3 and re.search(r'(?:^|/)java(?:\s|$)',parts[2]) and marker in parts[2].split():
            found[int(parts[0])]=int(parts[1])*1024
            _commands[int(parts[0])]=parts[2]
    return found


def stop_owned(root):
    # Never signal a broad command pattern or a PID belonging to another root.
    for sig,seconds in [(signal.SIGTERM,5),(signal.SIGKILL,2)]:
        for pid in owned_jvms(root):
            try: os.kill(pid,sig)
            except ProcessLookupError: pass
        until=time.monotonic()+seconds
        while owned_jvms(root) and time.monotonic()<until: time.sleep(.2)
        if not owned_jvms(root): return
    raise RuntimeError('owned JVM did not stop')


def shutdown(root,log,env):
    result=subprocess.run(['python3','tools/build/sbt-warm','--shutdown'],cwd=root,env=env,
        stdout=log,stderr=subprocess.STDOUT,timeout=65)
    until=time.monotonic()+10
    while owned_jvms(root) and time.monotonic()<until:
        time.sleep(.2)
    if result.returncode or owned_jvms(root):
        raise RuntimeError('server shutdown did not establish an empty provider lifetime')


def host_profile():
    vm=subprocess.check_output(['vm_stat'],text=True)
    page=int(re.search(r'page size of (\d+) bytes',vm).group(1))
    pages={k:int(v) for k,v in re.findall(r'^([^:]+):\s+(\d+)\.',vm,re.M)}
    return dict(physical_memory_bytes=int(subprocess.check_output(['sysctl','-n','hw.memsize'],text=True)),
        available_bytes=page*sum(pages.get(k,0) for k in ('Pages free','Pages inactive','Pages speculative')),
        machine=subprocess.check_output(['uname','-a'],text=True).strip(),
        java=subprocess.check_output(['java','-version'],stderr=subprocess.STDOUT,text=True),
        R=subprocess.check_output(['Rscript','--version'],stderr=subprocess.STDOUT,text=True))


def monitor(root,command,env,log,limits,deadline,other_root):
    start=time.monotonic(); samples=[]; violation=None; seen=set(); commands={}; aggregate_samples=[]
    if owned_jvms(other_root): raise RuntimeError('other provider is still resident')
    worker=subprocess.Popen(command,cwd=root,env=env,stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
    try:
        while True:
            now=time.monotonic()
            own=owned_jvms(root)
            aggregate_samples.append(dict(seconds=now-start,rss_bytes=sum(own.values())))
            if owned_jvms(other_root): violation='other-provider-resident'
            servers={pid:rss for pid,rss in own.items() if '--rt-ext-dir' not in _commands.get(pid,'').split()}
            if len(servers)>1: violation='multiple-owned-JVMs'
            if sum(own.values())>limits['rss_bytes']: violation='rss-limit'
            for pid,rss in own.items():
                helper='--rt-ext-dir' in _commands.get(pid,'').split()
                samples.append(dict(seconds=now-start,pid=pid,rss_bytes=rss,startup_helper=helper))
                if not helper: seen.add(pid)
                if str(pid) not in commands:
                    commands[str(pid)]=_commands.get(pid,'unknown')
                if rss>limits['rss_bytes']: violation='rss-limit'
            if now-start>limits['cell_wall_seconds']: violation='cell-wall-limit'
            if now>deadline: violation='campaign-wall-limit'
            if violation or worker.poll() is not None: break
            time.sleep(limits['polling_seconds'])
    except BaseException as error:
        violation='monitor-error:'+type(error).__name__+':'+str(error)
    finally:
        if violation:
            stop_owned(root)
            if worker.poll() is None:
                try: os.killpg(worker.pid,signal.SIGTERM)
                except ProcessLookupError: pass
        try: code=worker.wait(timeout=10)
        except subprocess.TimeoutExpired:
            os.killpg(worker.pid,signal.SIGKILL);code=worker.wait()
    if not samples: violation=violation or 'unobserved-JVM'
    if len(seen)!=1: violation=violation or 'changed-JVM-lifetime'
    flags={}
    for pid in owned_jvms(root):
        read=subprocess.run(['jcmd',str(pid),'VM.flags'],capture_output=True,text=True,timeout=15)
        flags[str(pid)]=read.stdout+read.stderr
        if read.returncode or '-XX:ActiveProcessorCount=4' not in read.stdout or '-XX:MaxHeapSize=3221225472' not in read.stdout:
            violation=violation or 'effective-JVM-flags-mismatch'
    if len(flags)!=1: violation=violation or 'missing-effective-worker-flags'
    return dict(command=command,worker_exit=code,elapsed_seconds=time.monotonic()-start,
        resource_refusal=violation,peak_rss_bytes=max((s['rss_bytes'] for s in aggregate_samples),default=0),
        aggregate_samples=aggregate_samples,
        samples=samples,effective_jvm_flags=flags,process_commands=commands,
        scope='Owned JVMs sampled from launch by unique global-base identity every 0.2s, including startup. Aggregate sampled RSS is capped. The sbt --rt-ext-dir discovery helper is recorded and counted for memory, but is not a numerical worker lifetime. One resident numerical JVM, 3 GiB effective heap. Not an instantaneous memory bound. Thin client and R generation recorded separately.')
