# scalafim-archive

Dependency-light, cross-platform archive contracts for ScalaFIM.

The module owns only format-neutral vocabulary:

- immutable revision, manifest, provenance, publication, and integrity values;
- separate major-versioned archive-format, object-schema, and representation
  identities;
- exact canonical values, including raw IEEE-754 floating bits;
- namespaced typed payload roles, layout-independent logical payload
  identities, plans, executors, and observed results;
- resource-safe `ArchiveDriver[F]` and `OpenArchive[F]` boundaries;
- archive-native object, range, length, byte, and cache receipts;
- structural and content-validation scopes.

`CanonicalArchiveManifestCodec` is the admitted deterministic
`org.scalafim/neuroarchive-manifest@1` encoding. It preserves raw `Float64`
bits, canonical UTF-8 object-key order, sorted payload identities, and rejects
trailing or merely equivalent non-canonical input. `CanonicalArchiveWriter`
turns a fully checked document into an ordered staging/payload/manifest/publish
program. Its resource-owned transaction guarantees that every proper prefix
remains absent; container modules provide the physical sink and atomic publish
mechanism.

An archive revision never owns a live handle. `ArchiveDriver[F]` acquires an
`ArchiveResource`, while the pure revision remains safe to inspect after
release. Unknown representation keys remain structurally validatable and are
resolved by an application registry outside this module.

This artifact contains no LNA/HDF5, Zarr, dataset, or scientific reconstruction
code. LNA schema and physical IO live in `archive-lna`; Zarr profile code lives
in `archive-zarr`; representation decoding and compatibility assembly live in
`interop-archived-response`.

Run it directly with:

```sh
sbt archiveJVM/test
sbt archiveJS/test
```
