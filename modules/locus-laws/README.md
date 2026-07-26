# locus-laws

`locus-laws` is ScalaFIM's reusable, cross-platform test-support artifact for
finite indexed spaces. It has no production API role.

The artifact provides:

- dense reference regions, total maps, relations, fields, sections, and
  supported parcellations whose definitions do not depend on production
  storage;
- explicitly bounded exhaustive enumerators;
- pure law groups that return structured failures;
- differential conversion helpers for locus implementations and downstream
  adapters;
- exact and tolerant numeric comparison policies.

Exhaustive enumeration rejects a request before allocating when its case count
would exceed the configured bound. ScalaCheck supplements the small exhaustive
models with deterministic, bounded sparse constructor sequences.

Run it directly with:

```sh
sbt locusLawsJVM/test
sbt locusLawsJS/test
```
