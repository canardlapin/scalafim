# AGENTS.md

Guidance for coding agents working in **scalafim** — a Scala 3 system for fMRI /
neuroimaging computation, cross-compiled to the JVM and Scala.js. Read
[`vision.md`](vision.md) for intent and [`docs/module-relations.md`](docs/module-relations.md)
for the module map; this file is the working contract.

## Layout

- Everything lives under `modules/<name>/` as a `crossProject` (`CrossType.Full`).
- Source roots: `modules/<name>/{shared,jvm,js}/src/{main,test}/scala/...`.
  - `shared` — portable, JVM-only-dependency-free numeric core (the default home for code).
  - `jvm` — platform IO and JVM-only deps (e.g. NIfTI/atlas loaders, file backends, Breeze).
  - `js` — Scala.js-specific adapters and platform integrations.
- Packages are `scalafim.*` (`scalafim.linalg`, `scalafim.fmri.hrf`, `scalafim.image`, …).
- Module dependency edges are declared in `build.sbt`; keep them acyclic and minimal.

## Build & test

- Toolchain: Scala **3.7.4**, sbt **1.11.7**, [MUnit](https://scalameta.org/munit/) for tests.
- Commands:
  - `sbt compileAll` / `sbt testAll` — every module, both platforms.
  - `sbt <module>{JVM,JS}/test` — one platform, e.g. `sbt hrfJVM/test`, `sbt imageJS/test`.
- **A feature is not done until it compiles and its tests pass on _both_ JVM and JS.**
  Never verify only one platform. Run the tests — evidence over assertion.
- `scalacOptions` include `-deprecation -feature -unchecked`; keep the build warning-clean.

## Scala 3 style

Match the surrounding code — it is consistent, so imitate it rather than inventing.

Use Scala 3 to move errors out of runtime and into the compiler wherever practical.
Prefer APIs that make invalid states unrepresentable through precise ADTs, opaque
domain types, smart constructors, exhaustive `enum` matches, and explicit
`given`/`using` capabilities. Runtime checks still belong at trust boundaries and
construction points, but the core model should express its invariants in types.

- **Significant-indentation syntax**, no braces: `:`, `then`, `do`. Two-space indent.
- **Model the domain with algebraic types.** `enum` for closed alternatives (incl. error
  ADTs with a `def message`); small `final case class`es for records.
- **Opaque types** for domain scalars and ids (`Seconds`, `VertexId`, `DatasetId`) with a
  smart `apply` and `inline` `extension` operators — don't pass bare `Double`/`Int`/`String`.
- **Smart constructors** live on the companion `object` and validate. Two tiers:
  - public entry points return `Either[SomeError, T]` (or validate via `require`);
  - internal zero-copy `.unsafe(...)` constructors skip checks for hot paths.
- Enforce invariants with `require(...)` at construction; keep instances always-valid.
- **Represent failure directly** — an error `enum` or `Either`, not ad-hoc option lists,
  nulls, or thrown exceptions across API boundaries.
- Use `given`/`using` for typeclass-style capability (e.g. `Ordering`), not for smuggling config.
- There is no scalafmt config; keep diffs stylistically local and consistent with the file.

## Performance discipline

- Hot numeric kernels use **primitive `Array[Double]`**, row-major indexing, and `while`
  loops with explicit allocation discipline (see `linalg`). Keep the *public* API idiomatic
  and readable; keep the *inner loop* allocation-free.
- Shared code must avoid JVM-only numeric deps (Breeze, JTransforms) on hot paths — those
  belong behind a `jvm` boundary. If you need one in `shared`, it's a design smell; stop.
- Linear algebra solver contracts and portable reference implementations belong in `linalg`;
  do not add private eigensolver/SVD/inverse helper families in domain modules. JVM-only
  libraries such as Breeze belong behind explicit adapter modules such as `linalg-breeze` and typed solver
  capabilities. See [`docs/plans/linalg-backend-strategy.md`](docs/plans/linalg-backend-strategy.md).

## Testing

- One suite per unit: `class FooSuite extends munit.FunSuite`, file `FooSuite.scala`,
  under `.../src/test/...` mirroring the main package.
- Compare floating-point with `assertEqualsDouble(actual, expected, tol)` and an explicit
  `tol` — never `==` on doubles.
- **Numerical parity fixtures** anchor behavior that must match the R ecosystem
  (`neuroim2`, `fmrihrf`, `fmridesign`, `fmrireg`, `fmridataset`; sources under `~/code/`).
  When porting statistical behavior, add a fixture-backed test rather than trusting the port.
- Scenario tests are realistic workflow contracts, not loose assertion piles. Follow
  [`docs/plans/scenario-parity-harness.md`](docs/plans/scenario-parity-harness.md):
  every active scenario should return one `ScenarioResult` truth value, default to
  clean `Pass` only, represent known gaps as declared caveats, and require an
  explicit policy before `PassWithCaveats` is CI-acceptable.
- Put platform-independent tests in `shared` so they run on both JVM and JS; reserve
  `jvm`/`js` test dirs for platform-specific behavior (IO, native shims).

## Practices

- **Not a bare R port.** A feature belongs here when it has a clean Scala 3 API, passing
  JVM+JS tests, parity fixtures where R behavior must be matched, and a module boundary that
  still makes sense as the system grows. The R packages are references, not straitjackets —
  don't inherit S3 surfaces or list-shaped config.
- Prefer small composable types with explicit invariants; a model should be inspectable
  before it runs and a fit should return typed results.
- Add code to the lowest module that makes sense; don't reach across boundaries or create cycles.
- Keep `README.md` module blurbs and `build.sbt` aggregates/aliases in sync when you add a module.

## GitHub identity and publication

- The canonical GitHub repository is `canardlapin/scalafim`. Do not publish this
  checkout through the machine's default `bbuchsbaum` GitHub account.
- Repo-local Git authoring must use `canardlapin` and
  `307091466+canardlapin@users.noreply.github.com`. Verify `git config --local
  user.name`, `user.email`, and `github.account` before committing or pushing.
- Use `tools/github/gh-repo` for GitHub CLI operations. It selects the isolated
  `gh-canardlapin` profile without switching the global `gh` account, so work in
  other repositories can continue under `bbuchsbaum` concurrently.
- `origin` should be `https://github.com/canardlapin/scalafim.git`, with the
  repo-local credential helper bound to the same isolated profile. Credentials
  belong in the user's GitHub CLI configuration, never in this repository.

## Cursor Cloud specific instructions

Toolchain is baked into the VM snapshot: JDK 21, `sbt` 1.11.7 (on `PATH`), and
Node.js 22 (required by every `*JS/test` target via Scala.js `NodeJSEnv`). The
startup update script runs `sbt update`; on first project load sbt clones the
pinned GitHub source dependencies (`ravel`, `gale`, `locus4s`, `image4s`,
`graph4s`, `multivar`, `intaglio`, `zarr4s`, plus transitive `bids4s`) into
`~/.sbt/1.0/staging` — those clones are cached in the snapshot. Standard build,
test, and per-module commands live in `README.md` ("Common Commands") and the
`compileAll`/`testAll`/`examplesTest` aliases in `build.sbt`.

- **Do NOT run `sbt testAll` in one shot on this VM.** It links and runs all ~35
  Scala.js module test suites inside a single long-lived sbt JVM (`.jvmopts`
  pins `-Xmx6g`), spawning a Node runner per JS module and accumulating heap
  until the ~15 GiB VM exhausts memory and thrashes so hard the whole machine
  becomes unresponsive (even `echo` hangs). Instead run tests in **bounded
  batches so sbt exits and frees memory between batches** — e.g. a handful of
  `<module>JVM/test` in one `sbt` invocation, then a handful of `<module>JS/test`
  in another. JVM suites are cheap; the JS link + Node runners are the memory
  hog. `sbt compileAll` (both platforms) is fine and stays well under memory; it
  is also warning-clean and serves as the lint gate (`-deprecation -feature
  -unchecked`).
- If a run does wedge the VM, recover by killing the sbt launcher by its
  specific PID (`pgrep -f sbt-launch`); its child Node runners exit with it.
  Never `pkill -f` and never touch the `/exec-daemon` Node process.
- Headless VM: the JavaFX modules/examples (`imageViewJavafxJVM`,
  `surfaceViewJavafxJVM`, and the `examples/surface-view` JavaFX app) need
  OpenJFX natives + a display and are not runnable here; they are excluded from
  `compileAll`/`testAll` anyway. The core library and its Java2D/Canvas/Three.js
  view backends need no display.
- Quick end-to-end smoke of core functionality (atlas → MVPA) without a GUI:
  `sbt "workflowExamplesJVM/runMain scalafim.examples.workflows.runAtlasMvpaWorkflow"`
  (also `atlasExamplesJVM/runMain scalafim.examples.atlas.{queryToyAtlas,atlasToMvpaRegions}`).
