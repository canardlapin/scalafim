# Spatial Transform Parity: Epic Map

This page maps the plan in [spatial-transform-parity.md](spatial-transform-parity.md) to its mote tickets.
Mote holds the live status and dependencies; this page is a static index as filed on 2026-09-24.

- **Epic:** `bd-01M39Q2YNKRTJTD8PZ4694DS52`
- **List everything:** `mote ls --tag spatial-transforms`
- **Tags:** `stp-p0`…`stp-p8` and `stp-u` (phase); `stp-gate`, `stp-task`,
  `stp-upstream`, `stp-qualification` (kind)
- **Edges:** "is part of" links (`rel`) never block work; `blocks` links (`dep`)
  define the order of execution.
- **Order of execution:**
  - each gate waits on all of its packets
  - the epic waits on all ten gates
  - P0.01 is the only packet that can start now

**Existing tickets reused rather than duplicated:**

- `bd-01M37FQFRRF1TW2REJPWS8BRM8`: NLin6↔2009c inter-template warp. Linked under the P7
  gate and blocks both P7.07 and the P7 gate.
- `bd-01M37FQGV8ZPT30X37TA4MM8R7`: GIFTI frame metadata gaps. Linked under the epic
  without blocking anything; related to P1.04.

## Condensed graph

```text
P0 ─► P1 ─► P2 ─► P3 ─► P5 ─► P8
│                  │
│                  └──► P7 ──► P8
├─► P4 (oracle sets) ─► P2.03, P3.x, P5.03, P6.03
├─► U1 ─► P3.09 (FNIRT coefficients)
├─► U2 ─► U3 ;  U4 ─► U5     ─► P6 (with P3)
└─► P7.04, P7.05 may start early
```

## P0: Contracts, build skeleton and vendored oracles

Gate: `bd-01M39Q3SE8MK5KXFH463R01ATH`

| Key | Mote id | Title | Blocked by |
|---|---|---|---|
| P0.01 | `bd-01M39Q41X6RYFF49Q8J5K5E66X` | ADR: frame authority, naming, Hemisphere, kernel owners, scope and oracle licensing | – |
| P0.02 | `bd-01M39Q42QT2TJGRABKQCJVYRTS` | Build skeleton: transform crossProject, new edges, transformBoundaryCheck, test deps | P0.01 |
| P0.03 | `bd-01M39Q43JP6262F8TZEYKGWE5V` | Correct stale docs/module-relations.md transform ownership | P0.01 |
| P0.04 | `bd-01M39Q44D0D6FDWJGG00JA0C6D` | Vendor neurotransform native-tool oracle fixtures with manifests | P0.02 |

## P1: World spaces and end-to-end frame typing

Gate: `bd-01M39Q3T9WSTNNCRT1NDGSSVES`

| Key | Mote id | Title | Blocked by |
|---|---|---|---|
| P1.01 | `bd-01M39Q457RDC60GJZN7NVX67BZ` | Move SubjectId, SessionId, TemplateName into scalafim.image.space | P0.02 |
| P1.02 | `bd-01M39Q462V4Z0MZVN98F40KRWD` | WorldSpace identity: namespaces, reference acquisitions, fresh declarations, persistent restoration | P1.01 |
| P1.03 | `bd-01M39Q46XDVGNVBDJYGMRS9QW5` | Placed dependent pair, Rebind, and static Spaces singletons | P1.02 |
| P1.04 | `bd-01M39Q47R6CFKG1C91JD9CYK58` | SpaceResolver over NIfTI codes, GIFTI target space, BIDS space- and overrides | P1.02 |
| P1.05 | `bd-01M39Q48K25GMBSFTXBJBZPHD0` | Frame-preserving GridSpec[F], SpatialPullback[T,S], WorldPoint syntax; migrate resampling APIs | P1.03 |
| P1.06 | `bd-01M39Q49DSKGDVR4V3BNFQEBTB` | Per-layer viewer frames and one typed bounding box | P1.05 |
| P1.07 | `bd-01M39Q4A8S22MC2JMQQ25Z31E6` | Frame-erasure gate and independent type-discipline review | P1.04, P1.05, P1.06 |

## P2: Convention kernel and orientation

Gate: `bd-01M39Q3V4PWMM6T9YGJ8RH5CEZ`

| Key | Mote id | Title | Blocked by |
|---|---|---|---|
| P2.01 | `bd-01M39Q4B3N0JEA3PMPQYD1NSAA` | ToolCoordinates convention kernel with full FSL and FreeSurfer geometry | P1.07, P2.03 |
| P2.02 | `bd-01M39Q4BY0ZX3GFPRFE4DCPPSB` | AxisCodes and typed reorientation of data plus grid | P1.07 |
| P2.03 | `bd-01M39Q4CRMNPCZQZVDM6V126DP` | Convention oracles: oblique FreeSurfer tkRAS and conflicting qform/sform FSL fixtures | P4.01, P4.02 |

## P3: WorldTransform and read codecs

Gate: `bd-01M39Q3VZ7GFHKRXSG2D9EZMRX`

| Key | Mote id | Title | Blocked by |
|---|---|---|---|
| P3.01 | `bd-01M39Q4DKJBDHZT21DHBRCKK4T` | WorldTransform ADT, PushAvailability, provenance, resample entry point | P2.01, P2.02 |
| P3.02 | `bd-01M39Q4EE5HV4ENNMMWKXKFQ81` | Codec, Interpretation, Expression framework; TransformSource; detection; ConversionContext | P3.01 |
| P3.03 | `bd-01M39Q4F8Z4SC0B8EKP14KS7NH` | ITK text and MATLAB v4 codecs with rigid/similarity parameterisations | P3.02, P0.04, P4.03 |
| P3.04 | `bd-01M39Q4G3QTSAQPCQ8SB667K4E` | FSL FLIRT codec and interpretation | P3.02, P2.03, P4.04 |
| P3.05 | `bd-01M39Q4GYN6MD0ND46EQ24A3QF` | AFNI aff12.1D series codec with cardinal correction | P3.02, P0.04 |
| P3.06 | `bd-01M39Q4HS7N1CK1MP3VTPQX30P` | FreeSurfer LTA, talairach.xfm and register.dat codecs | P3.02, P2.01, P4.02 |
| P3.07 | `bd-01M39Q4JM62CMZ9G4VGRJS58CW` | Generalise ITK HDF5 composite reading (any affines, rigid kinds) | P3.02, P0.04, P4.03 |
| P3.08 | `bd-01M39Q4KFC2RT4R8M9MPTD5004` | Dense field NIfTI codecs: ANTs 4D/5D, FNIRT dense rel/abs, AFNI 3dQwarp | P3.02, P0.04, P4.03 |
| P3.09 | `bd-01M39Q4MARKJCCFAEYZB6Z4WKN` | FNIRT coefficient codec (cubic and quadratic, --aff) | P3.02, U1, P0.04, P4.04 |
| P3.10 | `bd-01M39Q4N5GZ1VG68HFDWQC1WPJ` | X5 read codec (linear and densefield nodes, chains) | P3.02, P4.05 |
| P3.11 | `bd-01M39Q4P09CV1ZBNZZS1700PK1` | Collapse TransformAssetLoader and ItkAffine onto codecs; spatial ingest as adapters | P3.03, P3.04, P3.05, P3.06, P3.07, P3.08 |
| P3.12 | `bd-01M39Q4PTKRSHPHK9W7TYV322V` | Independent qualification of the read path | P3.09, P3.10, P3.11 |

## P4: Native-tool oracle fixtures

Gate: `bd-01M39Q3WV1NP2ZXJ2DW5E8SM9X`

| Key | Mote id | Title | Blocked by |
|---|---|---|---|
| P4.01 | `bd-01M39Q4QNH7HVDAFEMXE5DMKPT` | Oracle generator infrastructure and decoded shared fixtures | P0.04 |
| P4.02 | `bd-01M39Q4RG0D0CWYPB7PSYKF2J1` | FreeSurfer 7 oracle set | P4.01 |
| P4.03 | `bd-01M39Q4SAGDQQH0ZPQH6YC2TA1` | ANTs 2.6 oracle set | P4.01 |
| P4.04 | `bd-01M39Q4T55NBQN08W247MJCRC3` | FSL 6 oracle set (FLIRT, FNIRT dense and coefficients, --jac) | P4.01 |
| P4.05 | `bd-01M39Q4V0CDNBJGCEWNKWXS1JJ` | nitransforms cross-implementation check set | P4.01 |

## P5: Writers and conversion

Gate: `bd-01M39Q3XPYP41HRYNEZ08N275W`

| Key | Mote id | Title | Blocked by |
|---|---|---|---|
| P5.01 | `bd-01M39Q4VV0SV5E30XT9G74JCRW` | Encoders for every read format with value-exact round trips | P3.12 |
| P5.02 | `bd-01M39Q4WNXM2XZHE3KQR015DCK` | Expression and the D4b conversion matrix; Transforms.convert | P5.01 |
| P5.03 | `bd-01M39Q4XG9BHN0NTPTEYECC7RP` | Writer acceptance protocol: goldens, native-tool receipts, CI freshness check | P5.02, P4.02, P4.03, P4.04 |

## U: Upstream reframe4s warp algebra

Gate: `bd-01M39Q3YHK4TWENWQT8QJKK2AQ`

| Key | Mote id | Title | Blocked by |
|---|---|---|---|
| U1 | `bd-01M39Q4YC68YTWSQW1YM0JB738` | reframe4s: tensor-product B-spline field evaluation (quadratic, cubic) | P0.02 |
| U2 | `bd-01M39Q4Z70G3VC20RC4BHWJ9E5` | reframe4s: dense map composition to a field on a chosen lattice | P0.02 |
| U3 | `bd-01M39Q502W6WFT9AZY6G833BEZ` | reframe4s: numerical dense-map inversion producing InverseEstimate evidence | U2 |
| U4 | `bd-01M39Q50Y70KD4J91T5W2ENW2H` | reframe4s: Jacobian and log-Jacobian determinant fields with fold status | P0.02 |
| U5 | `bd-01M39Q51SY3RVCM4B2B114CQBS` | reframe4s: Jacobian-modulated resampling (jacobian, sqrtJacobian) | U4 |

## P6: Warp algebra adapters

Gate: `bd-01M39Q3ZCM2BDP6TBV607RQE0P`

| Key | Mote id | Title | Blocked by |
|---|---|---|---|
| P6.01 | `bd-01M39Q52NV8Q98GNW45GQX8KTM` | WorldTransform.materialize and field composition adapters | P3.12, U2 |
| P6.02 | `bd-01M39Q53JNRCWQG7ZRPWWK74EE` | invertNumerically and PushAvailability.Estimated with qualification gates | P6.01, U3 |
| P6.03 | `bd-01M39Q54DA1JW62CYQDXQP231D` | Jacobian determinant adapters qualified against FSL --jac | P3.12, U4, P4.04 |
| P6.04 | `bd-01M39Q558PV9WWEEANN8TKEMQS` | Modulated resample adapter and law tests | P6.03, U5 |

## P7: Graph unification and neurofunctor parity

Gate: `bd-01M39Q407C5WFB8AVTGG8BHNES`

| Key | Mote id | Title | Blocked by |
|---|---|---|---|
| P7.01 | `bd-01M39Q564A749H21GBHQ779GVH` | Atlas SpaceTransforms becomes a manifest populating SpatialGraph | P3.12 |
| P7.02 | `bd-01M39Q56ZH3NY2MVPSW2F63ZHG` | Routing parity: inverse quality cost, forward-first fallback, inspectPath/allPaths | P7.01 |
| P7.03 | `bd-01M39Q57TZ2CWDDT4XGTRCR3EK` | QC and operator parity: roundTrip, commutes, weight metrics, backproject, hybrid blocks | P7.02 |
| P7.04 | `bd-01M39Q58NZ92RPSWC7Z8HES7FV` | Surface resampling: barycentric kernel, sphere helpers, fsaverage<->fsLR plans, template domains | P2.02, P0.01 |
| P7.05 | `bd-01M39Q59GYTZGCFVZE5PZ7CZS8` | Overlap metrics (Dice, Jaccard) via locus4s region algebra | P0.01 |
| P7.06 | `bd-01M39Q5AC27QFA880VN48GJCAG` | Export neurofunctor law fixtures as triplets | P7.03 |
| P7.07 | `bd-01M39Q5B7AHPK325MZV3PS249J` | Scenario contracts: fMRIPrep chain, FSL chain, surface chain | P7.03, P7.04, P3.06, P3.09, `bd-01M37FQFRRF1TW2REJPWS8BRM8` |

## P8: Surfaces, viewers and documentation

Gate: `bd-01M39Q41222N0SQGE191WF64BF`

| Key | Mote id | Title | Blocked by |
|---|---|---|---|
| P8.01 | `bd-01M39Q5C2CJGRW01CPWH3GZ63N` | Surface frames and surface->volume ribbon fill | P7.04, P2.01 |
| P8.02 | `bd-01M39Q5CXH0TYQYECRD8FSHT6J` | Viewers: linked cursor through WorldTransform, typed surface camera | P7.07 |
| P8.03 | `bd-01M39Q5DR9N3XJDXQV59MQ3XNG` | Guide: coordinate spaces and transforms with compiled examples | P5.02, P7.07 |
| P8.04 | `bd-01M39Q5EKTWVNNJ3ZHF3SSRV20` | Housekeeping: README, module-relations, halfflow geometry copy request | P8.03 |
| P8.05 | `bd-01M39Q5FEXJX6HM0XB656G1AE0` | Final independent integration review | P1.07, P3.12, P5.03, P6.04, P7.07, P8.01, P8.02, P8.04 |
