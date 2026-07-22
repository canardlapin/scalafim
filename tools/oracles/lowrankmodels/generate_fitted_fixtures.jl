using JSON3
using LinearAlgebra
using LowRankModels
using Pkg
using UUIDs

const UPSTREAM_REPOSITORY = "https://github.com/madeleineudell/LowRankModels.jl.git"
const UPSTREAM_COMMIT = "a18f0df45f1a6ce37634bf4e347062b6090397eb"
const SCHEMA_VERSION = "scalafim.lowrankmodels.fitted-oracle.v1"
const LOW_RANK_MODELS_UUID = UUID("15d4e49f-4837-5ea3-a885-5b28bfa376dc")
const DATA = [1.0 2.0; 2.0 4.0]
const RIDGE = 0.4
const CHECKPOINTS = [0, 1, 2, 5, 10, 25, 50, 100, 200]

function full_objective(u, v)
  reconstruction = u * transpose(v)
  residual = reconstruction - DATA
  0.5 * sum(abs2, residual) + 0.5 * RIDGE * (sum(abs2, u) + sum(abs2, v))
end

function stationarity(u, v)
  residual = u * transpose(v) - DATA
  gradient_u = residual * v + RIDGE * u
  gradient_v = transpose(residual) * u + RIDGE * v
  max(norm(gradient_u), norm(gradient_v))
end

function fit_at(initial_u, initial_v, iterations)
  losses = LowRankModels.Loss[LowRankModels.QuadLoss(0.5), LowRankModels.QuadLoss(0.5)]
  row_regularizers = LowRankModels.Regularizer[
    LowRankModels.QuadReg(RIDGE / 2),
    LowRankModels.QuadReg(RIDGE / 2)
  ]
  column_regularizers = LowRankModels.Regularizer[
    LowRankModels.QuadReg(RIDGE / 2),
    LowRankModels.QuadReg(RIDGE / 2)
  ]
  model = LowRankModels.GLRM(
    DATA,
    losses,
    row_regularizers,
    column_regularizers,
    1,
    X = reshape(copy(initial_u), 1, :),
    Y = reshape(copy(initial_v), 1, :),
    scale = false,
    offset = false
  )
  parameters = LowRankModels.ProxGradParams(
    1.0,
    max_iter = iterations,
    inner_iter = 10,
    abs_tol = -1.0,
    rel_tol = -1.0,
    min_stepsize = 1e-12
  )
  x, y, history = LowRankModels.fit!(model, parameters, verbose = false)
  u = collect(transpose(x)[:, 1])
  v = collect(transpose(y)[:, 1])
  reconstruction = u * transpose(v)
  (
    u = u,
    v = v,
    reconstruction = reconstruction,
    fullObjective = full_objective(u, v),
    libraryObjective = LowRankModels.objective(model),
    stationarity = stationarity(u, v),
    reportedHistory = collect(history.objective)
  )
end

function fixture_start(id, initial_u, initial_v)
  checkpoints = map(CHECKPOINTS) do iteration
    fit = fit_at(initial_u, initial_v, iteration)
    (
      iteration = iteration,
      fullObjective = fit.fullObjective,
      stationarity = fit.stationarity
    )
  end
  fit = fit_at(initial_u, initial_v, CHECKPOINTS[end])
  abs(fit.fullObjective - fit.libraryObjective) <= 1e-10 || error("library objective mismatch for $id")
  (
    id = id,
    initial = (rowCodes = initial_u, decoder = initial_v),
    fittedFactors = (rowCodes = fit.u, decoder = fit.v),
    reconstruction = [collect(fit.reconstruction[row, :]) for row in axes(fit.reconstruction, 1)],
    decoded = [collect(fit.reconstruction[row, :]) for row in axes(fit.reconstruction, 1)],
    fullObjective = fit.fullObjective,
    libraryObjective = fit.libraryObjective,
    stationarity = fit.stationarity,
    objectiveCheckpoints = checkpoints,
    upstreamReportedHistory = fit.reportedHistory,
    upstreamReportedFinal = fit.reportedHistory[end],
    upstreamHistoryObjectiveDiscrepancy = fit.reportedHistory[end] - fit.fullObjective
  )
end

function main(args)
  output = isempty(args) ?
    normpath(joinpath(@__DIR__, "../../../modules/multivar/shared/src/test/resources/lowrankmodels/fitted-v1.json")) :
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
    program = (
      loss = "quadratic",
      rank = 1,
      data = [collect(DATA[row, :]) for row in axes(DATA, 1)],
      rowPenalty = (kind = "ridge", scalaWeight = RIDGE, juliaScale = RIDGE / 2),
      decoderPenalty = (kind = "ridge", scalaWeight = RIDGE, juliaScale = RIDGE / 2)
    ),
    algorithm = (
      implementation = "LowRankModels.fit!(GLRM, ProxGradParams)",
      stepsize = 1.0,
      maximumIterations = CHECKPOINTS[end],
      innerIterations = 10,
      absoluteTolerance = -1.0,
      relativeTolerance = -1.0,
      minimumStepsize = 1e-12,
      stoppingPolicy = "fixed-budget; negative tolerances disable the upstream decrease stop"
    ),
    starts = [
      fixture_start("positive", [1.0, 0.5], [0.7, 1.3]),
      fixture_start("negative-sign", [-1.0, -0.5], [-0.7, -1.3]),
      fixture_start("skew", [2.0, -0.3], [0.2, 2.0])
    ],
    limitations = [
      "Factors are retained only to independently recompute objectives and stationarity; parity never requires literal factor equality.",
      "LowRankModels.jl ConvergenceHistory.objective is not the full GLRM objective for this fitted program, so full-objective checkpoints are recomputed from fresh deterministic fit! prefixes.",
      "Negative fit tolerances deliberately disable the upstream history-based early-stop rule; fitted comparisons use a fixed 200-iteration budget."
    ]
  )
  mkpath(dirname(output))
  open(output, "w") do io
    JSON3.pretty(io, fixture)
    write(io, '\n')
  end
  println(output)
end

main(ARGS)
