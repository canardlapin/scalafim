# Unified MVPA foundation spike

Test-only admission court for lead-owned M0.02 and M0.03 in the
[unified MVPA epic](../../docs/plans/unified-mvpa-epic.md). This proves candidate
integration boundaries before production signatures are frozen. The root
build now pins and consumes the admitted providers through this unpublished
module. It does not implement the replacement MVPA engine.

## Run

The production dependency closure is exercised directly from the repository
root after bootstrapping Multivar's exact artifact dependencies:

```sh
./tools/prepare-pinned-dependencies.sh
sbt mvpaFoundationAdmissionJVM/test
sbt mvpaFoundationAdmissionJS/test
```

The retained standalone runner supplies a stricter source-manifest court. With
sbt, Java, Node and sibling Git checkouts containing its exact revisions:

```sh
node tools/run-umvpa-foundation-spike.mjs --platform jvm --upstream
node tools/run-umvpa-foundation-spike.mjs --workspace /absolute/path/printed/by/first/run --platform js --upstream
```

`--siblings /path/to/scala` changes the checkout search root. `--prepare-only`
extracts sources without starting sbt. `--suite scalafim.umvpaspike.IdentityPrototypeSuite`
narrows consumer test execution. Without `--upstream`, only the consumer
suites run. `--platform both` runs platforms sequentially in separate bounded
sbt processes; it never invokes the production all-tests aggregate.

The runner:

- Archives exact Git revisions into a newly owned temporary sibling layout;
  Provider uncommitted files are not used or changed.
- Copies the actual current response/locus main sources unchanged into that
  temporary consumer, recording their bytes and dirty-tree provenance. This
  avoids loading unrelated production build dependencies. The root admission
  module, not this snapshot mechanism, certifies dependency resolution.
- Rebuilds the exact Gale artifact required by candidate Multivar into a
  private temporary Ivy home. Nothing is published remotely or into global
  Ivy. Reused artifacts are checked by digest.
- Runs `spikeJVM/test` and `spikeJS/test` from the standalone consumer build,
  with optional focused upstream laws in separate processes.
- Retains source hashes, invocation inputs, command lines, exit codes and
  uniquely named logs in `spike-receipt.json`. A receipt always has
  `productionAdmission: false`: this source-level court does not by itself
  establish a resolvable production dependency graph.

This directory retains its own sbt build for the standalone manifest runner
and is also wired into the root as `mvpaFoundationAdmissionJVM/JS`. Both forms
are unpublished; no artifact from this directory should be published.

## What is exercised

`ProviderAdmissionSuite` uses the real Multivar `Lin`/`Table`, Gale operators,
resample4s exact-once grouped plans, and Alder's public external-plugin API.
It checks typed orientation, independent matrix expectations, deterministic
partition membership, zero-copy brain-row views, composition-safe
own-target-free cross-fitting, role restrictions and rich fitted-artifact
access. The exact resample4s design is passed to Alder's public
`Resample4sResampler.fromDesign`; Alder binds it at the actual normalized stage
seed. A strict `fromCompiled` negative control proves a plan prebound at the
root seed still rejects the composed stage. See the
[admission record](../../docs/plans/unified-mvpa-foundation-admission.md).

`IdentityPrototypeSuite` exercises runtime-loaded nominal axes, checked
metadata binding, typed scalar/multiresponse targets, occurrence-preserving
draws, response-index bridging, dependent heterogeneous measurement spaces,
source/representation distinctions and bounded metadata inspection. A poison
operator proves inspection and composition perform zero neural reads; an
explicit numerical request trips the same sentinel.

## Deliberate limits and retirement

The prototype is under `src/test`, limited to 64 coordinates, uses a
collision-free full descriptor encoding, and allocates tiny dense restriction
matrices. None of those choices is a scalable production identity/index
implementation. It creates no registry, solver, resampling engine, fitted
preparation lifecycle or compatibility façade. Metadata supplied by a caller
remains declared, not content-verified.

The batched source uses row views, but Alder's current resampler collects
vectors of row references and materializes fold membership. It is not a
whole-brain streaming/resource qualification. Numerical fixtures here are
analytic adapter laws, not R parity or inferential calibration.

Move the successful laws onto the admitted production API in M1.01–M1.06 and
remove the temporary prototype shapes once replaced. The positive stage-bound
consumer law must follow the eventual production pin; do not ship a second
axis model alongside the new foundation.
