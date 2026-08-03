# Response/Representation/Archive Phase 9 Record

Status: complete; repository-wide JVM/Scala.js gates passed  
Issue: `bd-01KYAA8PQ71BZHQ55BEFGVNYH3`  
Governing plan: [response-representation-archive-architecture.md](response-representation-archive-architecture.md)  
Baseline: [Phase 8 record](response-representation-archive-phase-8.md)

Phase 9 normalizes the format-neutral archive manifest without changing
scientific reconstruction or physical payload bytes. LNA manifests are
adapted through a pure format boundary, Zarr revisions emit the same
normalized identities directly, and canonical-manifest publication
is admitted only through a checked transactional write plan.

## 1. Independent identities

The pure `ArchiveManifest` now carries four separate identity layers:

```text
archive format       ArchiveFormatKey
logical object       ObjectTypeId + object schema major
representation       RepresentationKey + persisted descriptor envelope
logical payload      PayloadRoleId + scalar type + shape
```

`ArchiveFormatKey` has the strict form `name@major`.
`ObjectTypeId` has the strict form `namespace/name`. Representation keys retain
their `namespace/name@major` contract. Canonical writes additionally require
every payload role to be namespaced.

The first live bindings declare:

```text
LNA:
  format          lna-hdf5@2
  object          org.scalafim/fmri-response, schema 2
  representation org.scalafim/lna-pipeline@2 or
                 org.scalafim/temporal-dct@1
  payload roles   org.scalafim.lna/*

Zarr dense BOLD:
  format          neuroarchive-zarr@1
  object          org.scalafim/fmri-response, schema 1
  representation org.scalafim/dense-bold@1
  payload role    org.scalafim.zarr/canonical-response
```

Container, object, representation, and payload-role versions are neither
inferred from nor aliases of one another.

`PersistedRepresentation` is now a first-class manifest field containing only
the representation key, its canonical descriptor, and its canonical output
schema. Runtime projection uses this field directly. Representation families
still receive no full manifest and archive structure remains inspectable when
the representation is unknown.

## 2. Admitted canonical manifest

`CanonicalArchiveManifestCodec` implements the deterministic
`org.scalafim/neuroarchive-manifest@1` encoding. It uses explicit
length-framing and type markers rather than an ambient object serializer. The
codec:

- orders canonical object keys by UTF-8 bytes;
- orders payload and integrity entries by typed identity;
- represents every `Float64` by its 16 raw hexadecimal bytes;
- preserves NaN payloads and negative zero;
- separates archive, object, and representation versions;
- rejects trailing data and structurally valid non-canonical encodings.

An unknown representation key therefore round-trips and remains structurally
valid without installing executable code.

`LogicalPayloadIdentity` intentionally excludes physical-layout attributes.
The same logical payload retains identity when chunks, shards, compression, or
container-specific placement changes.

## 3. LNA adaptation and current readers

`LnaArchiveManifestAdapter` is a pure function from the versioned
`LnaManifest` wire model to the normalized `ArchiveManifest`. It maps typed
dataset roles to namespaced LNA roles and promotes the persisted
temporal-DCT descriptor and response schema into the narrow first-class
envelope. Generic LNA manifests receive the factual
`org.scalafim/lna-pipeline@2` representation.

`LnaArchiveDriver` performs this adaptation after reading LNA HDF5. LNA
fixtures therefore read through the same driver, while a rendered normalized
manifest parses only through the canonical codec. No LNA-specific branch was
added to the canonical writer.

`ZarrArchiveDriver` constructs the normalized revision directly from the
checked canonical-BOLD profile. Its shared typed payload-role constant is also
used by response interop, preventing the storage and decoding contracts from
drifting to different string literals.

## 4. Transactional canonical writing

`CanonicalArchiveDocument` requires an exact one-to-one match between manifest
descriptors and owned logical payload bytes. `CanonicalArchiveWritePlan`
rejects unqualified roles and emits this fixed order:

```text
begin staging
write payloads in payload-id order
write the canonical manifest
publish
```

`CanonicalArchiveWriter` interprets the plan against a
`CanonicalArchiveSink[F]`. The sink owns a resource-scoped transaction and
atomic publication. Every proper plan prefix exits that resource without
publication; the shared crash-injection fixture observes the destination as
absent after every prefix. Only the full plan can return a `Published`
receipt.

This is a format-neutral publication orchestrator, not a hidden filesystem,
HDF5, or Zarr writer. Physical sinks and atomic rename/object-store mechanisms
remain in their container modules. LNA and Zarr writers keep factual profile
names.

## 5. Executable evidence

The focused final state passed:

```text
archive JVM / JS                  9 / 9
archive-lna JVM / JS             11 / 8
archive-zarr JVM / JS            18 / 16
interop archived response JVM   139
interop archived response JS    102
Phase 8 boundary guard             3
compileAll                     passed warning-clean
testAll                        passed
Phase 0 numerical corpus        valid, 13 cases
git diff --check                clean
```

The fixtures cover pure LNA-to-normalized-manifest adaptation, LNA and Zarr
driver revisions, unknown representations, exact raw floating bits,
layout-independent logical identity, rejection of unqualified canonical writes,
and absence after every proper canonical-write prefix.

Phase 9 changes neither the LNA physical payload codec nor the NeuroArchive
Zarr byte profile and writer. The pinned external Zarr/Python, zarr-java, and
BIDS-validator evidence in
[the Zarr extraction receipt](../benchmarks/zarr-z8-profile-extraction.md)
therefore remains the applicable external conformance baseline; fresh
byte-identical profile, publication, HDF5, JVM, and Scala.js suites passed in
the whole-tree run.

Static production scans found no unchecked cast, warning suppression,
response-level tensor, scientific reconstruction in archive core, or
representation metadata scraping in runtime projection. Ordinary `Any`
matches are confined to lawful equality implementations.
