# uMVPA M1.06 native read policy

`AlderPredictiveAdmission.nativeTables` is the only admission path that reads an
`Observations` or `MultiResponse` semantic table. The caller supplies complete
metadata with its declared `DataFingerprint`, a verified `NativeAxisMapping`,
and a `NativeReadPolicy` containing both the maximum rectangular column width
and the numeric-cell budget. `SingleApplication` is the default access mode:
it refuses a policy that would apply either table more than once. A caller that
owns replay must declare `OwnedReplay` with a nonempty owner identity before
multiple blocks are permitted. This is a caller/provider declaration, not a
certificate that an arbitrary native operator is stream-safe.

Before retained row arrays or a table basis are allocated, admission verifies
the sample axis, mapping, table shapes, and metadata length. It then reserves

```
retained = rows * (inputColumns + targetColumns)
workspace = width * (2 * max(inputColumns, targetColumns) + 3 * rows)
required = retained + workspace
```

with overflow-safe `Long` arithmetic. `workspace` is an adapter-visible
authorization bound, not a measurement of process peak memory. It deliberately
includes the rectangular basis, an input and output column-vector allowance,
the Gale output builder, and a result snapshot allowance. Gale currently
transfers the builder backing store to the returned `DMat`, and its column views
are O(1); the policy nevertheless reserves separate cells for those stages.
Wrapper-object allocation, Scala collection headers, already-owned metadata,
and arbitrary user-operator internal allocation are outside this
adapter-visible numeric-buffer budget.

The read uses `Table.apply` with a `columns x width` rectangular identity basis,
then copies the resulting `rows x width` block into retained row arrays. No
full `DMat.eye(columns)` is created. In `SingleApplication` mode, full-width
admission performs one `Table.apply` per table; narrower policies are refused
before a callback. In declared replay mode, each source-column basis vector is
applied once per complete materialization. A non-fatal table or allocation
failure returns `NativeReadFailure` with logical block calls and separately
reported returned and copied cells; fatal errors continue to propagate.

The root fingerprint commits to the declared mapping identity, metadata
fingerprint, both axis descriptors, both `ValueIdentity` trees, source ids, and
complete provenance-node/parent declarations. It is intentionally a declared
source identity, not a hash of native payload values. Consequently a changed
input or target value identity, source id, or provenance declaration changes the
training root fingerprint even when the numerical payload is equal.

Focused shared tests in `NativeReadPolicySuite` cover rectangular blocks and
retained values, replay ownership, poison single-application refusal before
reads, full-width single-application admission, metadata/axis/provenance/value
identity sensitivity, later-input and target failure receipts, and large-shape
overflow refusal. They are added for the parent integration run; this delegated
slice did not invoke sbt.
