# Independent Core-3 literal fixture

`python3 generate.py` authors JSON, TSV and NIfTI directly with Python standard
library JSON/struct/gzip. It never invokes the production Scala encoder or writer.
`SHA256SUMS` freezes the generator, descriptions, expected values and all bytes.
`expected.json` records the independent plane/sample permutation calculation.

The 10 x 1 x 1 grid uses a nontrivial scanner RAS affine. Sample 0 is a support
hole; the other nine fit samples exercise codes 1 through 9. Two explicitly
identified hypotheses have different exclusions. They are synthetic explanatory
states, not records from a qualified fitter. Numerical validity is independently
NotComputed inside support, including where the status is Estimable. Conditional
inference names only the task column; the learned response subspace is Unknown.

The physical stack is little-endian NIfTI-1 UInt8, identity scaling, three ordered
planes, 352-byte data offset and 382-byte total length. `single-3d.nii` independently
tests the permitted singleton 3D form; deterministic gzip tests owned staging.
Catalog, TSV, physical payload and manifest references contain literal SHA256 and
length values. No fit dependency, product validity inference or subspace proof is
used in readback.
