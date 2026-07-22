using JSON3
using LinearAlgebra
using LowRankModels
using Pkg
using UUIDs

const UPSTREAM_REPOSITORY = "https://github.com/madeleineudell/LowRankModels.jl.git"
const UPSTREAM_COMMIT = "a18f0df45f1a6ce37634bf4e347062b6090397eb"
const SCHEMA_VERSION = "scalafim.lowrankmodels.oracle.v1"
const LOW_RANK_MODELS_UUID = UUID("15d4e49f-4837-5ea3-a885-5b28bfa376dc")

matrix_rows(matrix) = [collect(matrix[row, :]) for row in axes(matrix, 1)]
mask_rows(mask) = [collect(mask[row, :]) for row in axes(mask, 1)]

function penalty_spec(kind, scala_weight)
  julia_scale = kind == "squared-frobenius" ? scala_weight / 2 : scala_weight
  (kind = kind, scalaWeight = scala_weight, juliaScale = julia_scale)
end

function regularizer(spec)
  if spec.kind == "none"
    LowRankModels.ZeroReg()
  elseif spec.kind == "l1"
    LowRankModels.OneReg(spec.juliaScale)
  elseif spec.kind == "squared-frobenius"
    LowRankModels.QuadReg(spec.juliaScale)
  else
    error("unsupported fixture regularizer $(spec.kind)")
  end
end

function regularization_parts(glrm, X, Y)
  yidxs = LowRankModels.get_yidxs(glrm.losses)
  row_penalty = sum(
    LowRankModels.evaluate(glrm.rx[row], view(X, :, row))
    for row in axes(X, 2)
  )
  decoder_penalty = sum(
    LowRankModels.evaluate(glrm.ry[feature], view(Y, :, yidxs[feature]))
    for feature in eachindex(glrm.ry)
  )
  row_penalty, decoder_penalty
end

function scalar_loss(kind)
  if kind == "quadratic"
    LowRankModels.QuadLoss(0.5)
  elseif kind == "logistic"
    LowRankModels.LogisticLoss(1.0)
  elseif kind == "poisson"
    LowRankModels.PoissonLoss()
  else
    error("unsupported scalar fixture loss $kind")
  end
end

function julia_scalar_data(kind, data)
  kind == "logistic" ? Bool.(data) : copy(data)
end

function scalar_decoded(kind, natural)
  if kind == "quadratic"
    copy(natural)
  elseif kind == "logistic"
    @. 1 / (1 + exp(-natural))
  elseif kind == "poisson"
    exp.(natural)
  else
    error("unsupported scalar fixture loss $kind")
  end
end

function poisson_response_constant(data, mask)
  result = 0.0
  for column in axes(data, 2), row in axes(data, 1)
    if mask[row, column]
      observed = data[row, column]
      result += observed == 0 ? 0.0 : observed * (log(observed) - 1)
    end
  end
  result
end

function scalar_case(id, kind, data, mask, row_codes, decoder;
                     row_penalty = penalty_spec("l1", 0.2),
                     decoder_penalty = penalty_spec("squared-frobenius", 0.4))
  m, n = size(data)
  rank = size(row_codes, 2)
  @assert size(mask) == size(data)
  @assert size(decoder) == (n, rank)

  loss_values = LowRankModels.Loss[scalar_loss(kind) for _ in 1:n]
  row_regularizers = LowRankModels.Regularizer[regularizer(row_penalty) for _ in 1:m]
  decoder_regularizers = LowRankModels.Regularizer[regularizer(decoder_penalty) for _ in 1:n]
  X = Matrix(permutedims(row_codes))
  Y = Matrix(permutedims(decoder))
  julia_data = julia_scalar_data(kind, data)
  observed = findall(mask)
  glrm = LowRankModels.GLRM(
    julia_data,
    loss_values,
    row_regularizers,
    decoder_regularizers,
    rank;
    X = X,
    Y = Y,
    obs = observed,
    offset = false,
    scale = false
  )

  natural = X' * Y
  gradients = Matrix{Union{Nothing, Float64}}(nothing, m, n)
  for column in 1:n, row in 1:m
    if mask[row, column]
      gradients[row, column] = LowRankModels.grad(
        loss_values[column],
        natural[row, column],
        julia_data[row, column]
      )
    end
  end
  observed_loss = LowRankModels.objective(glrm, X, Y; include_regularization = false)
  row_value, decoder_value = regularization_parts(glrm, X, Y)
  response_constant = kind == "poisson" ? poisson_response_constant(data, mask) : 0.0

  (
    id = id,
    loss = kind,
    rank = rank,
    data = matrix_rows(data),
    observed = mask_rows(mask),
    rowCodes = matrix_rows(row_codes),
    decoder = matrix_rows(decoder),
    naturalParameters = matrix_rows(natural),
    naturalGradients = matrix_rows(gradients),
    decoded = matrix_rows(scalar_decoded(kind, natural)),
    rowPenalty = row_penalty,
    decoderPenalty = decoder_penalty,
    objective = (
      observedEntryLoss = observed_loss,
      rowPenalty = row_value,
      decoderPenalty = decoder_value,
      total = observed_loss + row_value + decoder_value,
      responseConstant = response_constant
    ),
    notes = kind == "poisson" ?
      ["Compare observedEntryLoss - responseConstant; natural gradients compare directly."] :
      String[]
  )
end

function softmax(values)
  shift = maximum(values)
  weights = exp.(values .- shift)
  weights ./ sum(weights)
end

function categorical_case()
  data = reshape([0.0, 2.0, 1.0, 0.0], 4, 1)
  mask = reshape([true, false, true, true], 4, 1)
  row_codes = [
    0.5 -0.2
    1.0  0.3
   -0.4  0.8
    0.2  0.6
  ]
  decoder = [
     0.7 -0.1
    -0.3  0.9
     0.2 -0.5
  ]
  rank = size(row_codes, 2)
  levels = size(decoder, 1)
  row_penalty = penalty_spec("l1", 0.2)
  decoder_penalty = penalty_spec("squared-frobenius", 0.4)
  loss = LowRankModels.MultinomialLoss(levels)
  X = Matrix(permutedims(row_codes))
  Y = Matrix(permutedims(decoder))
  julia_data = Int.(data .+ 1)
  glrm = LowRankModels.GLRM(
    julia_data,
    LowRankModels.Loss[loss],
    LowRankModels.Regularizer[regularizer(row_penalty) for _ in axes(data, 1)],
    LowRankModels.Regularizer[regularizer(decoder_penalty)],
    rank;
    X = X,
    Y = Y,
    obs = findall(mask),
    offset = false,
    scale = false
  )

  natural = X' * Y
  natural_by_cell = [[collect(natural[row, :])] for row in axes(natural, 1)]
  gradients = Vector{Any}(undef, size(data, 1))
  decoded = Vector{Any}(undef, size(data, 1))
  for row in axes(data, 1)
    current = collect(natural[row, :])
    gradients[row] = mask[row, 1] ?
      [collect(LowRankModels.grad(loss, current, julia_data[row, 1]))] :
      [fill(nothing, levels)]
    decoded[row] = [softmax(current)]
  end
  observed_loss = LowRankModels.objective(glrm, X, Y; include_regularization = false)
  row_value, decoder_value = regularization_parts(glrm, X, Y)

  (
    id = "categorical-masked-rank2",
    loss = "categorical",
    levels = levels,
    rank = rank,
    data = matrix_rows(data),
    observed = mask_rows(mask),
    rowCodes = matrix_rows(row_codes),
    decoder = matrix_rows(decoder),
    naturalParameters = natural_by_cell,
    naturalGradients = gradients,
    decoded = decoded,
    rowPenalty = row_penalty,
    decoderPenalty = decoder_penalty,
    objective = (
      observedEntryLoss = observed_loss,
      rowPenalty = row_value,
      decoderPenalty = decoder_value,
      total = observed_loss + row_value + decoder_value,
      responseConstant = 0.0
    ),
    notes = [
      "Fixture data uses ScalaFIM zero-based levels; the generator adds one for Julia.",
      "Compare centered natural parameters or decoded probabilities, not the common-shift gauge."
    ]
  )
end

function fixture_cases()
  quadratic = scalar_case(
    "quadratic-masked-rank2",
    "quadratic",
    [1.0 -0.5; 2.0 0.0; -1.0 1.5],
    Bool[true true; true false; true true],
    [0.5 -0.2; 1.0 0.3; -0.4 0.8],
    [0.7 -0.1; -0.3 0.9]
  )
  logistic = scalar_case(
    "logistic-masked-rank2",
    "logistic",
    [1.0 0.0; 0.0 1.0; 1.0 1.0; 0.0 0.0],
    Bool[true true; true false; true true; false true],
    [0.4 -0.3; 0.8 0.2; -0.5 0.7; 0.1 0.6],
    [0.9 -0.4; -0.2 0.8]
  )
  poisson = scalar_case(
    "poisson-masked-rank2",
    "poisson",
    [0.0 2.0; 3.0 1.0; 1.0 4.0],
    Bool[true true; true false; true true],
    [0.2 -0.1; 0.6 0.3; -0.2 0.5],
    [0.4 -0.2; 0.1 0.7]
  )
  [quadratic, logistic, poisson, categorical_case()]
end

function main(args)
  output = isempty(args) ?
    normpath(joinpath(@__DIR__, "../../../modules/multivar/shared/src/test/resources/lowrankmodels/objective-v1.json")) :
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
