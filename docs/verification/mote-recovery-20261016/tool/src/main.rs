use std::{collections::{BTreeMap, BTreeSet}, fs, path::Path};
use mote::{canonical, ids, op::Op, reducer, state::{Bead, State}};
use serde_json::{json, Value};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
type Entry = (String, Vec<u8>);

fn projection(bead: &Bead) -> Value {
    let mut notes: Vec<_> = bead.notes.iter().map(|n| n.op_id.clone()).collect();
    notes.sort();
    json!({"title":bead.title,"status":bead.status,"priority":bead.priority,
        "body":bead.body,"assignee":bead.assignee,"tags":bead.tags,
        "deps":bead.deps,"rels":bead.rels,"notes":notes,"deleted":bead.deleted_at_ts})
}
fn projections(state: &State) -> BTreeMap<String, Value> {
    state.beads.iter().map(|(id,b)| (id.clone(),projection(b))).collect()
}
fn read_entries(root: &Path) -> Result<(Vec<Entry>, Vec<Entry>)> {
    let mut records: Vec<_> = fs::read_dir(root.join("authority"))?.collect::<std::io::Result<Vec<_>>>()?;
    records.sort_by_key(|e| e.file_name());
    let mut prefix=Vec::new();
    let mut seen=BTreeSet::new();
    let mut numbered=0;
    for entry in records {
        let name=entry.file_name().to_string_lossy().to_string();
        if name.len()!=25 || !name[..20].bytes().all(|b|b.is_ascii_digit()) {
            return Err(format!("unexpected authority entry: {name}").into());
        }
        if name!=format!("{numbered:020}.json") {return Err("authority gap".into());}
        numbered+=1;
        let bytes=fs::read(entry.path())?;
        if numbered==1 {
            let format: Value=serde_json::from_slice(&fs::read(root.join("FORMAT.json"))?)?;
            if format["authority"]["genesis_hash"]!=blake3::hash(&bytes).to_hex().as_str() {
                return Err("genesis binding mismatch".into());
            }
        }
        let record: Value=serde_json::from_slice(&bytes)?;
        for item in record["files"].as_array().ok_or("bad admission files")? {
            let name=item[0].as_str().ok_or("bad name")?.to_string();
            ids::parse(&name)?;
            if !seen.insert(name.clone()) {return Err("duplicate admission".into());}
            let bytes=fs::read(root.join("ops").join(&name))?;
            if item[1]!=blake3::hash(&bytes).to_hex().as_str() {return Err(format!("changed admitted op: {name}").into());}
            prefix.push((name,bytes));
        }
    }
    let mut extras=Vec::new();
    for entry in fs::read_dir(root.join("ops"))? {
        let entry=entry?; let name=entry.file_name().to_string_lossy().to_string();
        if seen.contains(&name) {continue;}
        let parts=ids::parse(&name)?;
        let bytes=fs::read(entry.path())?;
        let op: Op=serde_json::from_slice(&bytes)?;
        if op.op_id()!=name.trim_end_matches(".json") {return Err("envelope mismatch".into());}
        let allowed=["note","release","claim","tag_add","patch","reserve_close","reserve_open","session_start","close","create","session_end","dep_add","rel_add","session_heartbeat"];
        if !allowed.contains(&op.kind_name()) {return Err(format!("extra kind requires review: {}",op.kind_name()).into());}
        let mut value: Value=serde_json::from_slice(&bytes)?;
        value["op"]=json!("");
        if ids::hash6(&canonical::encode(&value))!=parts.hash {return Err(format!("op hash mismatch: {name}").into());}
        extras.push((name,bytes));
    }
    extras.sort_by(|a,b|a.0.cmp(&b.0));
    Ok((prefix,extras))
}
fn analyze(root: &Path, report: &Path) -> Result<()> {
    let (prefix,extras)=read_entries(root)?;
    let before=reducer::replay(prefix.clone());
    let appended=reducer::replay(prefix.iter().chain(&extras).cloned());
    let mut chronological: Vec<_>=prefix.iter().chain(&extras).cloned().collect();
    chronological.sort_by(|a,b|a.0.cmp(&b.0));
    let sorted=reducer::replay(chronological);
    let a=projections(&before); let b=projections(&appended); let c=projections(&sorted);
    let changes: Vec<_>=b.iter().filter(|(id,v)| a.get(*id)!=Some(*v)).map(|(id,v)|json!({"id":id,"before":a.get(id),"appended":v,"chronologicalDiagnostic":c.get(id)})).collect();
    let chronology_differences: Vec<_>=b.iter().filter(|(id,v)| c.get(*id)!=Some(*v)).map(|(id,_)|id.clone()).collect();
    let candidate_unchanged=format!("{:?}",before.candidates)==format!("{:?}",appended.candidates);
    let roles_unchanged=format!("{:?}",before.roles)==format!("{:?}",appended.roles) && format!("{:?}",before.role_assignments)==format!("{:?}",appended.role_assignments);
    let report_value=json!({"schema":"scalafim.mote-recovery-analysis.v1","policy":"Existing admission prefix unchanged; extras appended in filename order, not claimed as original historical order","prefixOperations":prefix.len(),"extraOperations":extras.len(),"beforeIssues":a.len(),"appendedIssues":b.len(),"chronologicalDiagnosticIssues":c.len(),"candidateRecordsUnchanged":candidate_unchanged,"roleAuthorityUnchanged":roles_unchanged,"chronologicalDiagnosticDifferences":chronology_differences,"changedIssueProjections":changes});
    fs::write(report,serde_json::to_vec_pretty(&report_value)?)?;
    println!("{}",json!({"prefix":prefix.len(),"extras":extras.len(),"beforeIssues":a.len(),"afterIssues":b.len(),"changedIssues":changes.len(),"chronologicalDifferences":chronology_differences,"candidateRecordsUnchanged":candidate_unchanged,"roleAuthorityUnchanged":roles_unchanged}));
    Ok(())
}

fn preview(root: &Path, destination: &Path, limit: usize) -> Result<()> {
    let (prefix, extras)=read_entries(root)?;
    if !destination.exists() {
        fs::create_dir(destination)?;
        for name in ["ops","authority","local","tmp"] { fs::create_dir(destination.join(name))?; }
        fs::copy(root.join("FORMAT.json"),destination.join("FORMAT.json"))?;
        for entry in fs::read_dir(root.join("authority"))? {
            let entry=entry?; fs::copy(entry.path(),destination.join("authority").join(entry.file_name()))?;
        }
        for (name,bytes) in &prefix { fs::write(destination.join("ops").join(name),bytes)?; }
    }
    let store=mote::Store::open(destination)?;
    let existing=store.list_op_filenames()?;
    let expected: Vec<_>=prefix.iter().chain(&extras).map(|(n,_)|n.clone()).collect();
    if existing!=expected[..existing.len()] {return Err("preview does not match expected prefix".into());}
    let completed=existing.len()-prefix.len();
    let started=std::time::Instant::now();
    for (index,(name,bytes)) in extras.iter().enumerate().skip(completed).take(limit) {
        // This is Mote's native, locked, journaled publisher. Neither operation
        // bytes nor admission record contents are constructed by this tool.
        mote::publish::publish_bytes(&store,&ids::OpName::from_string(name.trim_end_matches(".json").to_string())?,bytes)?;
        if index<3 || (index+1)%25==0 || index+1==extras.len() {
            eprintln!("native publication: {}/{} extras; elapsed {:.1}s",index+1,extras.len(),started.elapsed().as_secs_f64());
        }
    }
    let report=mote::fsck::run(&store,false)?;
    if !report.is_clean() {return Err(format!("preview fsck failed: {report:?}").into());}
    let names=store.list_op_filenames()?;
    for (name,bytes) in prefix.iter().chain(&extras).take(names.len()) {
        if fs::read(destination.join("ops").join(name))?!=*bytes {return Err("operation bytes changed".into());}
    }
    for entry in fs::read_dir(root.join("authority"))? {
        let entry=entry?;
        if fs::read(entry.path())?!=fs::read(destination.join("authority").join(entry.file_name()))? {return Err("original authority record changed".into());}
    }
    let actual=reducer::replay_store(&store)?;
    let expected_state=reducer::replay(prefix.iter().chain(&extras).take(names.len()).cloned());
    if format!("{actual:?}")!=format!("{expected_state:?}") {return Err("native preview differs from simulated state".into());}
    println!("{}",json!({"previewStore":destination,"operations":names.len(),"publishedExtras":names.len()-prefix.len(),"fsckClean":true,"allOperationBytesUnchanged":true,"originalAuthorityRecordsUnchanged":true,"nativeReplayMatchesSimulation":true,"durationSeconds":started.elapsed().as_secs_f64()}));
    Ok(())
}
fn main() -> Result<()> {
    let args: Vec<_>=std::env::args().collect();
    match args.get(1).map(String::as_str) {
        Some("analyze") if args.len()==4 => analyze(Path::new(&args[2]),Path::new(&args[3])),
        Some("preview") if args.len()==5 => preview(Path::new(&args[2]),Path::new(&args[3]),args[4].parse()?),
        _ => Err("usage: scalafim-mote-recovery analyze STORE REPORT.json | preview STORE NEW_STORE LIMIT".into()),
    }
}
