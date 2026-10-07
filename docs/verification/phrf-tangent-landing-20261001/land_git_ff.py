"""Owner-authorized local fast-forward with narrow owned-path recovery."""
import argparse, hashlib, json, os, shutil, subprocess
from pathlib import Path

def git(root,*args,check=True):
    return subprocess.run(["git","-C",str(root),*args],capture_output=True,check=check)

def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest() if path.is_file() else None

def file_state(path):
    if path.is_symlink():
        return {"kind":"symlink","target":os.readlink(path),"mode":path.lstat().st_mode & 0o777}
    if not path.exists():return {"kind":"absent"}
    if path.is_file():return {"kind":"file","sha256":sha(path),"mode":path.stat().st_mode & 0o777}
    return {"kind":"directory","mode":path.stat().st_mode & 0o777}

def outside_states(root,changed):
    # Separate NUL-delimited listings avoid porcelain rename records and
    # collapsed untracked directories. --no-renames exposes both staged sides.
    commands=[("ls-files","--modified","--deleted","-z"),
        ("ls-files","--others","--exclude-standard","-z"),
        ("diff","--cached","--no-renames","--name-only","-z")]
    names=set()
    for args in commands:
        names.update(os.fsdecode(p) for p in git(root,*args).stdout.split(b"\0") if p)
    return {p:file_state(root/p) for p in sorted(names-set(changed))}

def check_outside(root,before):
    unchanged=[k for k,v in before.items() if file_state(root/k)==v]
    unexpected=[k for k,v in before.items() if file_state(root/k)!=v]
    return unchanged,unexpected

def save(path,value):
    temp=path.with_suffix(".prepared")
    with temp.open("w") as f:json.dump(value,f,indent=2);f.write("\n");f.flush();os.fsync(f.fileno())
    os.replace(temp,path)

def run(root,base,candidate,allowed,snapshot,backups,journal,receipt):
    assert git(root,"symbolic-ref","HEAD").stdout.strip()==b"refs/heads/main"
    assert git(root,"rev-parse","HEAD").stdout.decode().strip()==base
    git(root,"merge-base","--is-ancestor",base,candidate)
    changed=git(root,"diff","--name-only",base,candidate).stdout.decode().splitlines()
    assert set(changed)==set(json.loads(allowed.read_text()))
    before=json.loads(snapshot.read_text());owned=json.loads(backups.read_text())
    assert set(owned)<=set(changed) and len(owned)==3
    for path in changed:
        assert sha(root/path)==before.get(path), "target changed or appeared: "+path
        assert not (root/path).is_symlink()
    for path,b in owned.items():
        assert sha(Path(b["backup"]))==b["sha256"]==sha(root/path)
    def index_entries():
        rows=git(root,"ls-files","--stage","-z").stdout.split(b"\0")
        return {os.fsdecode(r.split(b"\t",1)[1]):r.decode() for r in rows if r}
    old_index=index_entries();old_outside={k:v for k,v in old_index.items() if k not in changed}
    staged=git(root,"diff","--cached","--name-only").stdout.decode().splitlines()
    assert not set(staged)&set(changed)
    # Capture current unrelated dirty bytes, including any peer changes made
    # while this candidate's builds ran. No command below touches these paths.
    outside=outside_states(root,changed)
    state={"base":base,"candidate":candidate,"changedPaths":changed,"ownedBackups":owned,
        "phase":"prepared","unrelatedIndexEntries":old_outside,"unrelatedWorkingStates":outside,
        "protocol":"standard git restore of three owned paths then git merge --ff-only with autostash disabled; no manual ref or index replacement"}
    save(journal,state)
    try:
        assert git(root,"rev-parse","HEAD").stdout.decode().strip()==base
        for path in changed:assert sha(root/path)==before.get(path)
        git(root,"restore","--source="+base,"--worktree","--",*owned)
        state["phase"]="owned-baseline-restored";save(journal,state)
        result=git(root,"-c","merge.autoStash=false","merge","--ff-only","--no-autostash","--no-edit",candidate,check=False)
        state["mergeExit"]=result.returncode;state["mergeStdout"]=result.stdout.decode();state["mergeStderr"]=result.stderr.decode()
        if result.returncode:raise RuntimeError("fast-forward refused; see retained journal")
        state["phase"]="fast-forward-complete";save(journal,state)
    except BaseException:
        head=git(root,"rev-parse","HEAD").stdout.decode().strip()
        if head==base:
            for path,b in owned.items():
                shutil.copy2(b["backup"],root/path);os.chmod(root/path,b["mode"])
            assert all(sha(root/k)==v["sha256"] for k,v in owned.items())
            intact=all(sha(root/k)==before.get(k) for k in changed) and index_entries()==old_index
            state["phase"]=("failed-before-head-advance-owned-backups-restored" if intact else
                "unexpected-partial-state-retain-journal-and-backups")
        else:
            state["phase"]="unexpected-partial-state-retain-journal-and-backups"
            state["actualHead"]=head
        save(journal,state);raise
    assert git(root,"rev-parse","HEAD").stdout.decode().strip()==candidate
    assert {k:v for k,v in index_entries().items() if k not in changed}==old_outside
    # Check both selected index and working-copy contents against the immutable
    # commit. This does not claim the user's unrelated dirty tree was tested.
    assert not git(root,"diff",candidate,"--",*changed).stdout
    assert not git(root,"diff","--cached",candidate,"--",*changed).stdout
    unchanged,unexpected=check_outside(root,outside)
    state["outsideUnchanged"]=unchanged;state["unexpectedOutOfScopeChanges"]=unexpected
    if unexpected:
        state["phase"]="landed-with-unattributed-outside-changes-preservation-unverified"
        save(journal,state)
        raise RuntimeError("outside changes require investigation; journal/backups retained")
    state["phase"]="verified";save(journal,state)
    out={"baseMain":base,"landedCommit":candidate,"selectedPaths":changed,
        "selectedWorkingCopyAndIndexMatchCommit":True,"unrelatedIndexEntriesPreserved":True,
        "unchangedOutOfScopeDirtyFiles":unchanged,"unexpectedOutOfScopeChanges":unexpected,
        "journal":str(journal),"noRemoteOperation":True}
    assert not receipt.exists();save(receipt,out)
    print(json.dumps({"landedCommit":candidate,"selectedPaths":len(changed),"unchangedOutside":len(unchanged),"unexpectedOutsideChanges":unexpected}))

if __name__=="__main__":
    p=argparse.ArgumentParser(description=__doc__)
    for k in ["root","allowed","snapshot","backups","journal","receipt"]:p.add_argument("--"+k,required=True,type=Path)
    for k in ["base","candidate"]:p.add_argument("--"+k,required=True)
    a=p.parse_args();run(a.root,a.base,a.candidate,a.allowed,a.snapshot,a.backups,a.journal,a.receipt)
