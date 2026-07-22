using JSON3
using LinearAlgebra
using LowRankModels
using Pkg
using UUIDs

const UPSTREAM_REPOSITORY = "https://github.com/madeleineudell/LowRankModels.jl.git"
const UPSTREAM_COMMIT = "a18f0df45f1a6ce37634bf4e347062b6090397eb"
const SCHEMA_VERSION = "scalafim.lowrankmodels.encoding-oracle.v1"
const LOW_RANK_MODELS_UUID = UUID("15d4e49f-4837-5ea3-a885-5b28bfa376dc")

function row_objective(losses, regularizer, decoder, data, observed, code)
  result = LowRankModels.evaluate(regularizer, code)
  for feature in eachindex(losses)
    if observed[feature]
      result += LowRankModels.evaluate(losses[feature], dot(code, view(decoder, feature, :)), data[feature])
    end
  end
  result
end

function row_gradient(losses, decoder, data, observed, code)
  result = zeros(length(code))
  for feature in eachindex(losses)
    if observed[feature]
      natural = dot(code, view(decoder, feature, :))
      contribution = LowRankModels.grad(losses[feature], natural, data[feature])
      result .+= contribution .* view(decoder, feature, :)
    end
  end
  result
end

function loss_lipschitz(kind, decoder, observed)
  curvature = kind == "quadratic" ? 1.0 : 0.25
  sum(
    observed[feature] ? curvature * sum(abs2, view(decoder, feature, :)) : 0.0
    for feature in axes(decoder, 1)
  )
end

function proximal_residual(losses, regularizer, decoder, data, observed, code, step)
  gradient = row_gradient(losses, decoder, data, observed, code)
  candidate = LowRankModels.prox(regularizer, code .- step .* gradient, step)
  norm(code - candidate) / step
end

function solve_row(losses, regularizer, decoder, data, observed, initial, kind)
  lipschitz = loss_lipschitz(kind, decoder, observed)
  lipschitz > 0 || error("row fixture requires positive smooth curvature")
  maximum_step = 0.99 / lipschitz
  step = maximum_step
  code = copy(initial)
  objective = row_objective(losses, regularizer, decoder, data, observed, code)
  trajectory = [objective]
  residual = Inf
  iterations = 0
  for iteration in 1:50000
    gradient = row_gradient(losses, decoder, data, observed, code)
    candidate = LowRankModels.prox(regularizer, code .- step .* gradient, step)
    candidate_objective = row_objective(losses, regularizer, decoder, data, observed, candidate)
    while candidate_objective > objective + 1e-14 * max(1.0, objective)
      step *= 0.5
      step > 1e-15 || error("row fixture line search exhausted")
      candidate = LowRankModels.prox(regularizer, code .- step .* gradient, step)
      candidate_objective = row_objective(losses, regularizer, decoder, data, observed, candidate)
    end
    code = candidate
    objective = candidate_objective
    push!(trajectory, objective)
    residual = proximal_residual(losses, regularizer, decoder, data, observed, code, step)
    iterations = iteration
    if residual <= 1e-11
      break
    end
    step = min(maximum_step, step * 1.05)
  end
  residual <= 1e-9 || error("row fixture failed to converge: residual $residual")
  code, objective, residual, step, iterations, trajectory
end

function fixture_case(id, kind, data, observed, decoder, penalty_kind, weight, initial)
  feature_count, rank = size(decoder)
  @assert length(data) == feature_count
  @assert length(observed) == feature_count
  @assert length(initial) == rank
  losses = if kind == "quadratic"
    LowRankModels.Loss[LowRankModels.QuadLoss(0.5) for _ in 1:feature_count]
  elseif kind == "logistic"
    LowRankModels.Loss[LowRankModels.LogisticLoss(1.0) for _ in 1:feature_count]
  else
    error("unsupported encoding fixture loss $kind")
  end
  julia_data = kind == "logistic" ? Bool.(data) : copy(data)
  regularizer = if penalty_kind == "ridge"
    LowRankModels.QuadReg(weight / 2)
  elseif penalty_kind == "l1"
    LowRankModels.OneReg(weight)
  else
    error("unsupported encoding penalty $penalty_kind")
  end
  code, total, residual, residual_step, iterations, trajectory = solve_row(
    losses,
    regularizer,
    decoder,
    julia_data,
    observed,
    initial,
    kind
  )
  natural = [dot(code, view(decoder, feature, :)) for feature in 1:feature_count]
  decoded = kind == "quadratic" ? copy(natural) : [1 / (1 + exp(-value)) for value in natural]
  observed_loss = sum(
    observed[feature] ? LowRankModels.evaluate(losses[feature], natural[feature], julia_data[feature]) : 0.0
    for feature in 1:feature_count
  )
  row_penalty = LowRankModels.evaluate(regularizer, code)
  abs(total - observed_loss - row_penalty) <= 1e-10 || error("row objective accounting mismatch")
  (
    id = id,
    loss = kind,
    rank = rank,
    data = collect(data),
    observed = collect(observed),
    decoder = [collect(decoder[row, :]) for row in axes(decoder, 1)],
    penalty = (kind = penalty_kind, scalaWeight = weight, juliaScale = penalty_kind == "ridge" ? weight / 2 : weight),
    initial = collect(initial),
    solution = collect(code),
    naturalParameters = natural,
    decoded = decoded,
    objective = (observedEntryLoss = observed_loss, rowPenalty = row_penalty, total = total),
    proxGradientResidual = residual,
    residualStep = residual_step,
    iterations = iterations,
    objectiveTrajectory = trajectory
  )
end

function fixture_cases()
  dense_ridge = fixture_case(
    "quadratic-dense-ridge",
    "quadratic",
    [0.7, -1.2, 0.4],
    [true, true, true],
    [1.0 0.2; -0.4 0.8; 0.3 -0.5],
    "ridge",
    0.3,
    [0.5, -0.5]
  )
  sparse_l1 = fixture_case(
    "quadratic-sparse-l1",
    "quadratic",
    [0.9, 0.0, -0.2, 1.1],
    [true, false, true, true],
    reshape([1.0, -0.7, 0.4, 1.2], 4, 1),
    "l1",
    0.15,
    [-0.3]
  )
  logistic_ridge = fixture_case(
    "logistic-sparse-ridge",
    "logistic",
    [1.0, 0.0, 1.0],
    [true, false, true],
    [1.0 0.2; -0.3 0.7; -0.5 0.9],
    "ridge",
    0.25,
    [0.2, -0.4]
  )
  [dense_ridge, sparse_l1, logistic_ridge]
end

function main(args)
  output = isempty(args) ?
    normpath(joinpath(@__DIR__, "../../../modules/multivar/shared/src/test/resources/lowrankmodels/encoding-v1.json")) :
    abspath(args[1])
  package = Pkg.dependencies()[LOW_RANK_MODELS_UUID]
  fixture = (
    schemaVersion = SCHEMA_VERSION,
    provenance = (
      repository = UPSTREAM_REPOSITORY,
      commit = UPSTREAM_COMMIT,
      juliaVersion = string(VERSION),
      packageVersion = string(package.version)
    ),
    cases = fixture_cases()
  )
  mkpath(dirname(output))
  open(output, "w") do io
    JSON3.pretty(io, fixture)
    write(io, '\n')
  end
  println(output)
end

main(ARGS)
