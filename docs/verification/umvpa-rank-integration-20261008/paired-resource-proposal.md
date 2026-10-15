# Paired rank pilot: prospective resource revision

Status: proposal only; no campaign source/stream freeze and no pilot data consumed.

Keep the [prospective scientific inventory](../../plans/unified-mvpa-rank-method-plan-v2.md) unchanged: 64 cells, 200 independent datasets per cell, B199 and all three named methods on each dataset. The machine-readable [inventory proposal](paired-inventory-proposal.json) contains 12,800 unique datasets and 38,400 method evaluations, with no seed assignments. R0/R1/R2/R3 definitions retain the v1 populations; the weak, strong and near-unit families are the previously proposed additions.

Replace the unsupported one-hour aggregate planning limit with a proposed **four-hour hard cap**, retaining one active worker, four configured processors, 3 GiB JVM heap, 4 GiB resident-worker memory and 900 seconds per cell invocation. This changes a prospective resource limit, not a population, acceptance threshold or sample size. Exhausting any cap makes the campaign incomplete; do not drop cells, redraw inputs or report only completed cells as a successful comparison.

The cost sensitivity from the eight fixtures gives 100 minutes using observed means, or 169 minutes using observed maxima. It assigns measured n640 costs to n160/n320 as an explicit provisional assumption; neither estimate is a bound. Up to 32 planned server startups at a 60-second planning allowance add about 32 minutes, leaving roughly 39 minutes under four hours for generation, serialization and variation above the observed maxima calculation. Actual monitors override these estimates.

Before opening pilot streams:

1. Implement and source-freeze the paired runner and both provider adapters. Bind the historical provider to ab811e2 and current methods to edb05de; retain the exact full revisions, resolved dependency/classpath inventories, old/new adapter identities and matrix hashes. Check a common set of deterministic numeric fixtures before attributing discrepancies to method geometry.
2. Assign the v2 pilot namespace once. Generate each dataset once and present identical archived X/Y/Z values to all three methods. Preserve separate method identities, deterministic stream paths and paired analysis; historical and repaired permutations may share their predeclared transform stream. Do not reuse the eight resource fixtures as independent pilot observations.
3. Execute providers serially with at most one JVM resident. Reuse each server within a `(n,p,q,nuisance)` group, then shut it down before the next group/provider. There are 16 such groups per provider family: 32 planned server lifetimes. Fix environment paths while running a server and change only the case-list contents between cells.
4. Record adapter, parsing, generation and startup times separately; monitor the entire active worker from startup, or explicitly preflight startup before admitting its resource scope. The resource probe's sampler began at socket discovery and does not establish an early-startup memory bound.
5. Commit the actual campaign manifest, source locks, complete seed inventory, host profile and hard-stop/failure policy before execution. Retain every assignment, numerical failure and missing cell. Analyze first-true-null rejection, false rank overestimation, stagewise raw/closed decisions, standard-R3 closed H3 power and paired method differences with their uncertainty.

This pilot may inform method/sample-size selection. It cannot enable admitted rank inference or replace the separately reviewed final confirmation protocol. The unsuccessful v1 qualification and the n80/.20 power limitation remain part of the record.
