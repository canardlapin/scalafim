# Frame ecosystem and extraction readiness

Frame incubates in ScalaFIM under the `scalafim.frame` package and
`scalafim-frame` artifact family. `frame4s` is only a candidate extracted name.
Neither Typelevel membership nor `org.typelevel` coordinates are claimed.

## Extraction contract

The core and FS2 modules have no dependency on ScalaFIM domain modules or Gale.
`tools/frame-extraction-check.sh` copies only those two modules into a temporary
standalone build, mechanically renames `scalafim.frame` to `frame4s`, rejects
any remaining `scalafim.*` import, and runs JVM and Scala.js tests. The rehearsal
must not edit schema, plan, execution, storage, or source/sink code.

The extracted repository would initially retain neutral coordinates. A
Typelevel conversation should be affiliate-first and should happen only after
maintainers, release signing, compatibility policy, and support expectations
are real. Acceptance by an external organization is not a release gate.

## License and provenance

The intended extracted license is Apache License 2.0, an OSI-approved license
commonly used in the Scala ecosystem. Before external publication, the
maintainer must add the complete license text and copyright notice to the
standalone repository and audit every copied file and dependency for compatible
provenance. Generated fixtures must record their generator and upstream license.
Contributors must certify that they have the right to submit their changes; a
Developer Certificate of Origin sign-off is the default proposed mechanism.

This document records release intent, not a retroactive license change for
unrelated ScalaFIM modules.

## Conduct, security, and maintenance

An extracted project must adopt the current Contributor Covenant and publish a
private security-reporting address before accepting outside contributions.
Security reports should receive acknowledgement within seven days; embargo and
disclosure timing are agreed with the reporter. Public issues are appropriate
for ordinary correctness and performance bugs, not undisclosed vulnerabilities.

At least two maintainers should be able to release. A release requires clean
JVM and Scala.js tests, the standalone extraction rehearsal, dependency review,
and published semantic/benchmark receipts. If maintenance capacity falls below
that level, the project should say so prominently and avoid compatibility
promises it cannot sustain.

## Compatibility intent

During `0.x`, source compatibility is best-effort and semantic changes require
release notes and migration examples. The following are treated as especially
stable: logical null semantics, named-tuple schema meaning, plan purity,
resource ownership, and the distinction between logical and physical explain.
Binary compatibility checking should be introduced before `1.0`; no binary
compatibility promise is made by the incubation snapshot.

Serialized plans are not yet a public wire format. Arrow IPC compatibility is
delegated to Apache Arrow specifications and tested through the JVM adapter.
CSV behavior is controlled by explicit schema, delimiter, null-token, and
coercion options rather than ambient inference.

## Scope and support

Frame owns a small typed relational algebra, Arrow-compatible local storage,
lawful normalization, a semantic reference interpreter, and resource-safe
source/sink protocols. It does not promise a production vectorized engine,
distributed execution, spill, Parquet pushdown, dataframe convenience parity,
or statistical modeling. Production engines and additional formats are
optional adapters and must report accepted and residual capabilities.
