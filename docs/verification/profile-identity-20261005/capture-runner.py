from pathlib import Path
import subprocess, re, json, sys, os, datetime
repo=Path('/Users/bbuchsbaum/code/scala/scalafim')
suite=repo/'modules/fit/shared/src/test/scala/scalafim/fmri/fit/profile/ProfileFitIdentitySuite.scala'
logs=Path('/private/tmp/scalafim-profile-identity-20261005'); logs.mkdir(exist_ok=True)
original=suite.read_text()
patterns={
 'ProfileGolden': 'prepared().provenance',
 'FixedGolden': 'prepared(fixed, requested).provenance',
 'MlGolden': 'prepared(mlPlan).provenance',
 'GeometryGolden': 'geometry(whitening = Some(whitening), nuisance = Some(nuisance))',
}
instrumented=original
for key, expression in patterns.items():
 old=f'    assertEquals({expression}, ProfileFitIdentitySuite.{key})'
 new=f'    val actual = {expression}\n    println("IDENTITY_GOLDEN_{key}=" + actual)\n    assertEquals(actual, ProfileFitIdentitySuite.{key})'
 assert old in instrumented,key
 instrumented=instrumented.replace(old,new)
suite.write_text(instrumented)
env=os.environ.copy(); env['COURSIER_REPOSITORIES']='https://repo.maven.apache.org/maven2'
command=['python3','tools/build/sbt-warm','fitJVM/testOnly scalafim.fmri.fit.profile.ProfileFitIdentitySuite']
def run(cmd,name):
 start=datetime.datetime.now(datetime.timezone.utc).isoformat()
 with (logs/name).open('w') as out:
  result=subprocess.run(cmd,cwd=repo,env=env,stdout=out,stderr=subprocess.STDOUT)
 text=(logs/name).read_text()
 print(name,'exit',result.returncode,flush=True)
 print(text[-2500:],flush=True)
 return result.returncode,text,dict(command=cmd,exit_code=result.returncode,started_at_utc=start,log=name)
try:
 code, output, capture=run(command,'capture-jvm.log')
 goldens={key: re.search(r'IDENTITY_GOLDEN_'+key+r'=(.*)',output).group(1) for key in patterns if re.search(r'IDENTITY_GOLDEN_'+key+r'=(.*)',output)}
 if len(goldens)!=4 or not re.search(r'Failed\s*[: ]\s*4',output):
  print('Calibration incomplete; no literals written.',flush=True)
  sys.exit(1)
 fixed=original
 for key, value in goldens.items():
  marker=re.search(r'  val '+key+r' = ".*"',fixed).group(0)
  fixed=fixed.replace(marker,'  val '+key+' = '+json.dumps(value,ensure_ascii=False))
 suite.write_text(fixed)
 (logs/'goldens.json').write_text(json.dumps(goldens,indent=2)+'\n')
 commands=['python3','tools/build/sbt-warm',
  'fitJVM/testOnly scalafim.fmri.fit.profile.ProfileFitIdentitySuite scalafim.fmri.fit.profile.ProfileHrfFitSuite',
  'fitJS/testOnly scalafim.fmri.fit.profile.ProfileFitIdentitySuite scalafim.fmri.fit.profile.ProfileHrfFitSuite']
 code, output, focused=run(commands,'focused-jvm-js.log')
 (logs/'runs.json').write_text(json.dumps([capture,focused],indent=2)+'\n')
 sys.exit(code)
finally:
 if suite.read_text()==instrumented: suite.write_text(original)
