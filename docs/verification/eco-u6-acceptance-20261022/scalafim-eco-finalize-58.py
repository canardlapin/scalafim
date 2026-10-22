from pathlib import Path
import json,hashlib,gzip,shutil,datetime,re
base=Path('/Volumes/My Passport for Mac/codex-work/scalafim-eco-u6-20261022');out=Path('/Users/bbuchsbaum/code/scala/scalafim-eco-surface-20261021/docs/verification/eco-u6-acceptance-20261022');source='92154d81c39b6a384be195dde0e933ed0dff29ca'
campaign=json.loads((base/'campaign-receipt-57.json').read_text());assert campaign['status']=='Pass' and campaign['qualified_source_commit']==source
for src in [base/'campaign-receipt-57.json',base/'resume-inputs-56.json',base/'compile-resume-inputs-55.json',Path('/tmp/scalafim-eco-compile-resume-55.py'),Path('/tmp/scalafim-eco-resume-56.py'),Path('/tmp/scalafim-eco-campaign-receipt-57.py')]:shutil.copy2(src,out/src.name)
for name in ['scalafim-eco-u6-compile-55.log','scalafim-eco-u6-compile-55.log.meta.json','scalafim-eco-u6-resume-56.log','scalafim-eco-u6-resume-56.log.meta.json']:
 src=Path('/tmp')/name
 with gzip.GzipFile(filename=str(out/(name+'.gz')),mode='wb',mtime=0) as f:f.write(src.read_bytes())
if (out/'partial-receipt.json').exists():
 original=json.loads((out/'partial-receipt.json').read_text());(out/'checkpoint-before-final.json').write_text(json.dumps({'historical_checkpoint':True,'qualification':'Snapshot before final completion; not current job/status authority.','snapshot':original},indent=2)+'\n');(out/'partial-receipt.json').unlink()
matrix=json.loads((out/'seven-families.json').read_text());matrix['status']='Pass';matrix['qualified_source']=source;matrix['root_runtime_source']=source;matrix['published_candidate']=source;matrix['campaign_segments']=[{'source':s['source_commit'],'batches':s['counted_batches']} for s in campaign['segments']]
for family in matrix['families']:
 family['status']='Pass'
 if family['id']!=6:
  if 'receipt' in family:family['initial_owning_receipt']=family['receipt']
  family['receipt']='campaign-receipt-57.json'
matrix['global_gate']='Complete three-segment37-batch campaign; exact140nonzero behavioral summaries/144success markers,5JVM+4JS new results and six existing functional/MVPA court executions. Standalone app separately passed.'
(out/'seven-families.json').write_text(json.dumps(matrix,indent=2)+'\n')
binding={'qualified_source':source,'compile_all':{'source':source,'status':'Pass','receipt':'scalafim-eco-u6-compile-55.log.gz'},'campaign_kind':'Three segments, both failed runs retained','segments':matrix['campaign_segments'],'source_deltas':{'31a42_to_616860':'Root imaging test alias+two callers renamed; standalone owner proof repaired outside Root build and separately executed atf0.','616860_to_92154d':'Only JVM MotionBids9wildcard defaults+globdoc and17assertionlines in existing MotionIoSuite case. Providerpins/strictloader/math unchanged; fresh compile-all and fullmotionJVM/JS executed at921.'},'standalone':{'executed_source':'f0b67d349ec6f444969749845b15d3e58d8996bb','byte_identical_tool_at_qualified_source':True,'receipt':'standalone-receipt-45.json'},'qualification':'Earlier25successful batch input closures unaffected by Root-only MotionBids delta. Count17+8+12 exactly; no single-run or single-source full-suite claim.'}
(out/'input-closure-binding.json').write_text(json.dumps(binding,indent=2)+'\n')
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
independent=json.loads((out/'independent-receipt-40.json').read_text());assert independent['total_passed']==71
standalone=json.loads((out/'standalone-receipt-45.json').read_text())
nonexecutions=[{'batch':b['batch'],'source':b['source_commit'],'entry':entry} for b in campaign['completed_batches'] for entry in b['declared_nonexecutions']]
(out/'declared-nonexecutions.json').write_text(json.dumps({'skipped':campaign['skipped'],'ignored':campaign['ignored'],'entries':nonexecutions},indent=2)+'\n')
qualification={'schema':'scalafim.eco-u6-qualification.v1','status':'Pass','scope':'Local modularization acceptance; publication, native/browser and scientific boundaries explicit','qualified_source':source,'topic_branch':'eco/u5-surface-20261021','created_utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),'compile_all':campaign['compile_all'],'bounded_regression':{'kind':'segmented17+8+12','batches':37,'positive_behavioral_summaries':140,'success_markers':144,'passed':campaign['passed'],'skipped':campaign['skipped'],'ignored':campaign['ignored'],'receipt':'campaign-receipt-57.json','receipt_sha256':sha(out/'campaign-receipt-57.json')},'seven_families':{'status':'Pass','receipt':'seven-families.json','receipt_sha256':sha(out/'seven-families.json')},'independent_consumers':{'passed':71,'skipped':0,'classpath_scope':'27Compile sections; public API/test-internals also source-reviewed','provider_source_owners':15,'qualified_private_files':120,'receipt':'independent-receipt-40.json','receipt_sha256':sha(out/'independent-receipt-40.json')},'standalone':{'status':'Pass','scope':'Synthetic JVM NIfTI4D/ITK/Java2D app,41Compile/runtime entries','receipt':'standalone-receipt-45.json','receipt_sha256':sha(out/'standalone-receipt-45.json')},'adverse_evidence':['focused25 compile-only private-constructor refusal','focused28 fixture admission/centroid oracle failures','independent26 default1GiB heap exhaustion after20passes','standalone42 existential owner proof compilation refusal','full34 alias collision after17completed batches','resume51 MotionIoSuite1failure after8completed batches','conservative20GiB preflight refusals and gzip random-seek verifier interruption'], 'limits':'carried-forward-limits.json; historical-display-review.md; declared-nonexecutions.json','final_review':{'status':'Pending','qualification':'All executed gates completed; independent final receipt review still required before tracker closure.'},'publication':{'source_topic_branch_published':True,'main_merged':False,'hosted_ci':'See separately refreshed hosting-observation.json; no hosted success inferred from local gates.'}}
observations=[];ignored_contexts=[]
for segment in campaign['segments']:
 raw=Path(segment['raw_log']).read_text();starts=list(re.finditer(r'^\[full-repository-tests\] running (.+)$',raw,re.M))
 for i,m in enumerate(starts[:segment['counted_batches']]):
  body=raw[m.end():starts[i+1].start() if i+1<len(starts) else len(raw)]
  lines=re.findall(r'^\[milestone\] (?:Gaussian|LWU).+$',body,re.M)
  for event in re.finditer(r'^==> i .+$',body,re.M):
   before=body[:event.start()].splitlines()[-4:];after=body[event.end():].splitlines()[:3];ignored_contexts.append({'batch':m[1],'source_commit':segment['source_commit'],'event':event[0],'preceding_lines':before,'following_lines':after,'qualification':'Context only; do not infer an unnamed ignored test identity from log adjacency.'})
  if lines:observations.append({'batch':m[1],'source_commit':segment['source_commit'],'raw_log':Path(segment['raw_log']).name+'.gz','observed_lines':lines})
(out/'ignored-event-contexts.json').write_text(json.dumps({'qualification':'Ignored events retained as emitted; framework may report unknown test names. No execution or identity inferred.','events':ignored_contexts},indent=2)+'\n')
(out/'scientific-observations.json').write_text(json.dumps({'scope':'Actual final-campaign diagnostic observations, not new scientific qualification; original fixtures/budgets/flags retained.','observations':observations},indent=2)+'\n')
limits=json.loads((out/'carried-forward-limits.json').read_text());limits['u4_scientific_boundary']['final_campaign_observations']='scientific-observations.json; prior93/81percent values are historical and must not be described as current measurements.';(out/'carried-forward-limits.json').write_text(json.dumps(limits,indent=2)+'\n')
qualification['scientific_observations']='scientific-observations.json; observed below-target diagnostics retained, no flags/tolerances weakened.'
(out/'qualification.json').write_text(json.dumps(qualification,indent=2)+'\n')
passed=f"{campaign['passed']:,}";skips=campaign['skipped'];ignored=campaign['ignored']
(out/'README.md').write_text(f'''# U6 local modularization acceptance

Executed gates pass at qualified source `{source}`. The final independent receipt review is recorded separately before tracker closure. Source is published on `eco/u5-surface-20261021`; main is unmerged. Hosted CI is separate evidence.

| Gate | Executed result |
| --- | --- |
| Final-source compile-all | JVM and Scala.js Pass, compile55 |
| Bounded full regression | 37 batches, {passed} passed, {skips} declared skips, {ignored} ignored |
| Independent public consumers | 71 passed, 0 skipped across neuroim4s, neuroatlas4s, mvpa4s and neurosurf4s |
| Connected workflow matrix | All seven scenario families Pass |
| Standalone volume app | Actual NIfTI4D/ITK file IO, transform, Java2D/pick and refusal witness Pass |

The full regression is a **three-segment campaign**: 17 batches at `31a42a690`, 8 at `616860a2e`, and 12 at `92154d81c`. Two failed runs are retained. Full34 stopped before imaging tests because a provider alias replaced the Root alias. Its repaired six imaging targets then passed. Resume51 stopped at a real motion BIDS selection failure: regex `.*` defaults were being passed to provider glob selectors. Nine defaults now use `*`; the sessionless/default/glob/nonmatch regression and owning JVM/JS module gates passed. The failed partial batch is excluded entirely and rerun. No scientific tolerance or strict admission was weakened.

The collector checks 140 positive project summaries and 144 success markers, exact batch order, both platform-specific functional/prediction/RSA courts,5 JVM + 4 JS new scenario results, raw metadata/invocations and cross-bound input manifests. Three test targets have no own tests (two pure MVPA example consumers and surface spatial IO); they supply compilation evidence only. Their exact pinned source inventories are recorded. The surface examples have three real smoke tests and are required.

The independent consumers record 27 Compile classpath sections, 15 clean source owners at exact revisions, and 120 qualified private artifact files (including metadata/checksums, not 120 runtime artifacts). Builds use isolated external caches and exact source pins, without sibling overrides or a machine-wide Ivy store. Source/build review additionally excludes provider test internals and checks minimal-core optional dependency boundaries. Root compile ownership checks cover 561 production files and 24 actual Compile closures.

`qualification.json` and `campaign-receipt-57.json` are the primary summaries. `seven-families.json` binds the workflow courts. Raw logs and actual exit metadata are gzip-compressed alongside source/input archives and executed drivers. `declared-nonexecutions.json` lists every skip/ignored case. Earlier failures and resource-custody receipts remain available; old checkpoints are labeled historical.

`size-and-boundaries.md` reports the exact before/after measurement: ScalaFIM 49 -> 34 source-bearing module directories and 214,313 -> 132,082 physical production Scala lines. Per-library ownership areas and dependency declaration syntax counts are qualified; this is not a claim of net ecosystem shrink or resolved-edge reduction.

Historical native/browser receipts retain their exact earlier sources, runtimes, failures and limits in `historical-display-review.md`. They are not final `73a244f` artifact visual executions. Synthetic software-raster/Java2D workflows do not establish native/WebGL, real-cortex, performance or broader scientific acceptance. Existing atlas-route residuals/missing 15-map replay, platform-specific legacy affine IDs, below-target LWU observations and disabled scientific flags remain explicit in `carried-forward-limits.json`.
''')
print('Prepared final executed evidence:',passed,'passed',skips,'skips',ignored,'ignored; final independent review pending')
