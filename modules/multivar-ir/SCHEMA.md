# Multivar IR schema policy

`scalafim-multivar-ir` is the language-neutral boundary for the typed `multivar`
semantics. The current wire version is **0.1**. The Scala model and shared codec
are normative; `schema/multivar-ir-v0.1.schema.json` is the corresponding JSON
Schema for non-Scala consumers.

## Evolution

- The `major` component changes when an existing meaning, tag, orientation, or
  required field changes incompatibly.
- The `minor` component changes only for additive evolution.
- A 0.1 decoder accepts exactly 0.1. It rejects later versions with
  `schema_version_mismatch`; silent best-effort decoding is forbidden.
- `unknown_fields` is `reject` in 0.1. Unknown fields at every object level are
  rejected with `unknown_field`. A future minor version may add an explicit
  preservation mode, but 0.1 never drops unknown semantics silently.
- Numeric payload storage is not semantic identity. Inline dense and sparse
  payloads are hashed over their canonical logical values. External payloads
  carry a URI, media type, dimensions, and mandatory lowercase SHA-256 digest;
  a resolver must verify the digest before constructing numerical objects.
- Every form declares `scale_semantics`. Shape metrics carry an opaque
  `gauge_id`; only metrics from a shared fit/reconciliation may share that ID.

## Stable rejection categories

Every binding must report these category tags for the same invalid document:

- `domain_codomain_mismatch` for space, dimension, or primal/dual endpoint errors;
- `uncertified_positivity` for PSD/SPD claims without a matching value-bound certificate;
- `unsupported_singularity` when a declared singularity policy is unavailable;
- `incompatible_alignment_kind` when a row map, row link, coupling, or signed relation is used as another kind;
- `payload_tampered` for an inline digest mismatch;
- `schema_version_mismatch`, `unknown_field`, and `malformed` for wire-level failures.

## Fixture corpus

`conformance/manifest.json` lists valid and invalid documents and the expected
category. The same cases are embedded in `ConformanceCorpus` so the identical
suite runs on both JVM and Scala.js. Python and R bindings should consume the
JSON files and reproduce the category listed in the manifest.
