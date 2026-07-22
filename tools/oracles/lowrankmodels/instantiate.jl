using Pkg
using UUIDs

const LOW_RANK_MODELS_UUID = UUID("15d4e49f-4837-5ea3-a885-5b28bfa376dc")
const LOW_RANK_MODELS_REPOSITORY = "https://github.com/madeleineudell/LowRankModels.jl.git"
const LOW_RANK_MODELS_COMMIT = "a18f0df45f1a6ce37634bf4e347062b6090397eb"

if VERSION < v"1.6" || VERSION >= v"1.11"
  error("LowRankModels oracle requires Julia 1.6 through 1.10; got $VERSION")
end

Pkg.activate(@__DIR__)
Pkg.add(
  PackageSpec(
    name = "LowRankModels",
    uuid = LOW_RANK_MODELS_UUID,
    url = LOW_RANK_MODELS_REPOSITORY,
    rev = LOW_RANK_MODELS_COMMIT
  )
)
Pkg.add(PackageSpec(name = "JSON3", version = "1"))
Pkg.resolve()
Pkg.instantiate()

resolved = Pkg.dependencies()[LOW_RANK_MODELS_UUID]
resolved.version == v"1.1.1" || error("expected LowRankModels 1.1.1, got $(resolved.version)")
resolved.git_revision == LOW_RANK_MODELS_COMMIT ||
  error("expected LowRankModels commit $LOW_RANK_MODELS_COMMIT, got $(resolved.git_revision)")

Pkg.status()
