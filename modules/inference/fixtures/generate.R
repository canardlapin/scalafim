args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args) >= 1L) normalizePath(args[[1L]]) else normalizePath(getwd())
multifer_source <- if (length(args) >= 2L) normalizePath(args[[2L]]) else normalizePath("/Users/bbuchsbaum/code/multifer")

pkgload::load_all(multifer_source, quiet = TRUE)

fixture_dir <- file.path(repo, "modules", "inference", "fixtures", "v1")
scala_dir <- file.path(
  repo,
  "modules", "inference", "shared", "src", "test", "scala",
  "scalafim", "inference"
)
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(scala_dir, recursive = TRUE, showWarnings = FALSE)

fmt_num <- function(x) {
  if (length(x) == 0L) return(character(0L))
  vapply(x, function(value) {
    if (is.nan(value)) return("NaN")
    if (is.infinite(value)) return(if (value > 0) "Inf" else "-Inf")
    sprintf("%.17g", value)
  }, character(1L))
}

collapse_num <- function(x) paste(fmt_num(x), collapse = ",")
collapse_int <- function(x) paste(as.integer(x), collapse = ",")
collapse_bool <- function(x) paste(tolower(as.character(x)), collapse = ",")

write_tsv <- function(value, name) {
  utils::write.table(
    value,
    file = file.path(fixture_dir, name),
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = ""
  )
}

matrix_long <- function(case, domain, value) {
  grid <- expand.grid(row = seq_len(nrow(value)), col = seq_len(ncol(value)))
  data.frame(
    case = case,
    domain = domain,
    row_one_based = grid$row,
    col_one_based = grid$col,
    value = vapply(seq_len(nrow(grid)), function(i) value[grid$row[i], grid$col[i]], numeric(1L)),
    stringsAsFactors = FALSE
  )
}

draw_sequence <- function(values) {
  index <- 0L
  function() {
    index <<- index + 1L
    values[[index]]
  }
}

source_files <- c(
  "R/mc_pvalue.R",
  "R/mc_sequential.R",
  "R/unit_formation.R",
  "R/alignment.R",
  "R/infer_plan.R",
  "R/engine_oneblock.R",
  "R/engine_cross.R"
)
source_hashes <- vapply(
  file.path(multifer_source, source_files),
  digest::digest,
  character(1L),
  file = TRUE,
  algo = "sha256"
)
git_commit <- system2(
  "git",
  c("-C", multifer_source, "rev-parse", "HEAD"),
  stdout = TRUE,
  stderr = FALSE
)
git_dirty <- length(system2(
  "git",
  c("-C", multifer_source, "status", "--porcelain"),
  stdout = TRUE,
  stderr = FALSE
)) > 0L
source_files_dirty <- length(system2(
  "git",
  c("-C", multifer_source, "diff", "--name-only", "--", source_files),
  stdout = TRUE,
  stderr = FALSE
)) > 0L || length(system2(
  "git",
  c("-C", multifer_source, "diff", "--cached", "--name-only", "--", source_files),
  stdout = TRUE,
  stderr = FALSE
)) > 0L
rng <- RNGkind()

package_names <- c("multifer", "multivarious", "clue", "RSpectra")
package_versions <- vapply(package_names, function(package_name) {
  if (package_name == "multifer") return(as.character(packageVersion("multifer")))
  if (!requireNamespace(package_name, quietly = TRUE)) return("not-installed")
  as.character(utils::packageVersion(package_name))
}, character(1L))

provenance <- rbind(
  data.frame(key = "fixture_schema", value = "scalafim-inference-r-v1"),
  data.frame(key = "reference", value = "multifer"),
  data.frame(key = "reference_commit", value = git_commit[[1L]]),
  data.frame(key = "reference_dirty", value = tolower(as.character(git_dirty))),
  data.frame(key = "reference_source_files_dirty", value = tolower(as.character(source_files_dirty))),
  data.frame(key = "r_version", value = R.version.string),
  data.frame(key = "rng_kind", value = paste(rng, collapse = "/")),
  data.frame(key = paste0("package_", package_names), value = package_versions),
  data.frame(key = paste0("sha256_", source_files), value = unname(source_hashes))
)
write_tsv(provenance, "provenance.tsv")

fixed_specs <- list(
  list(
    name = "greater_with_ties",
    observed = 2,
    null = c(0, 2, 3, -1, 2),
    alternative = "greater"
  ),
  list(
    name = "less_lower_tail",
    observed = -1,
    null = c(0, -2, 1, -1, 3),
    alternative = "less"
  ),
  list(
    name = "two_sided_magnitude",
    observed = -2,
    null = c(-3, -2, -1, 0, 2, 4),
    alternative = "two_sided"
  )
)

fixed_cases <- lapply(fixed_specs, function(spec) {
  result <- mc_p_value(
    spec$observed,
    draw_sequence(spec$null),
    length(spec$null),
    spec$alternative
  )
  c(spec, list(result = result))
})

write_tsv(
  do.call(rbind, lapply(fixed_cases, function(case) data.frame(
    case = case$name,
    alternative = case$alternative,
    observed = case$observed,
    null_values = collapse_num(case$null),
    exceedances = case$result$r,
    draws = case$result$B,
    p_value = case$result$p_value,
    mc_se = case$result$mc_se,
    stringsAsFactors = FALSE
  ))),
  "fixed_monte_carlo.tsv"
)

sequential_specs <- list(
  list(
    name = "early_non_rejection",
    observed = 1,
    null = c(2, 1.5, 1.2, 0, 2.5, 1.1, rep(0, 14)),
    B_max = 20L,
    alpha = 0.2,
    batch_size = 3L
  ),
  list(
    name = "full_budget_rejection",
    observed = 5,
    null = seq(-2, 2, length.out = 20L),
    B_max = 20L,
    alpha = 0.2,
    batch_size = 6L
  )
)

sequential_cases <- lapply(sequential_specs, function(spec) {
  result <- mc_sequential_bc(
    spec$observed,
    draw_sequence(spec$null),
    B_max = spec$B_max,
    alpha = spec$alpha,
    batch_size = spec$batch_size,
    alternative = "greater"
  )
  c(spec, list(result = result))
})

write_tsv(
  do.call(rbind, lapply(sequential_cases, function(case) data.frame(
    case = case$name,
    alternative = "greater",
    observed = case$observed,
    offered_null_values = collapse_num(case$null),
    consumed_null_values = collapse_num(case$result$null_values),
    B_max = case$result$B_max,
    alpha = case$alpha,
    h = case$result$h,
    batch_size = case$batch_size,
    batch_schedule = collapse_int(case$result$batch_schedule),
    exceedances = case$result$r,
    drawn = case$result$drawn,
    stop_reason = case$result$stop_reason,
    p_value = case$result$p_value,
    mc_se = case$result$mc_se,
    stringsAsFactors = FALSE
  ))),
  "sequential_monte_carlo.tsv"
)

unit_specs <- list(
  list(
    name = "single_axes_default",
    roots = c(5, 4.99, 1),
    selected = c(TRUE, TRUE, FALSE),
    group_near_ties = FALSE,
    tie_threshold = 0.01
  ),
  list(
    name = "near_tie_partial_selection",
    roots = c(5, 4.99, 1),
    selected = c(TRUE, FALSE, TRUE),
    group_near_ties = TRUE,
    tie_threshold = 0.01
  ),
  list(
    name = "consecutive_tie_chain",
    roots = c(5, 4.96, 4.92, 1),
    selected = c(TRUE, TRUE, TRUE, FALSE),
    group_near_ties = TRUE,
    tie_threshold = 0.01
  )
)

unit_cases <- lapply(unit_specs, function(spec) {
  result <- form_units(
    spec$roots,
    selected = spec$selected,
    group_near_ties = spec$group_near_ties,
    tie_threshold = spec$tie_threshold
  )
  c(spec, list(result = result, members = attr(result, "members")))
})

write_tsv(
  do.call(rbind, lapply(unit_cases, function(case) {
    do.call(rbind, lapply(seq_len(nrow(case$result)), function(i) data.frame(
      case = case$name,
      roots = collapse_num(case$roots),
      selected_input = collapse_bool(case$selected),
      group_near_ties = case$group_near_ties,
      tie_threshold = case$tie_threshold,
      unit_id = case$result$unit_id[[i]],
      unit_type = case$result$unit_type[[i]],
      members_one_based = collapse_int(case$members[[i]]),
      identifiable = case$result$identifiable[[i]],
      selected = case$result$selected[[i]],
      stringsAsFactors = FALSE
    )))
  })),
  "unit_formation.tsv"
)

reference_unique <- rbind(diag(3), c(0, 0, 0))
replicate_unique <- reference_unique[, c(2, 3, 1), drop = FALSE]
replicate_unique[, 1] <- -replicate_unique[, 1]
replicate_unique[, 3] <- -replicate_unique[, 3]
reference_ambiguous <- diag(2)
replicate_ambiguous <- matrix(1 / sqrt(2), nrow = 2, ncol = 2)

alignment_specs <- list(
  list(name = "permuted_sign_flips", reference = reference_unique, replicate = replicate_unique),
  list(name = "ambiguous_equal_scores", reference = reference_ambiguous, replicate = replicate_ambiguous)
)

alignment_cases <- lapply(alignment_specs, function(spec) {
  permutation <- match_components(spec$reference, spec$replicate, diagnostics = TRUE)
  matched <- spec$replicate[, permutation, drop = FALSE]
  aligned <- align_sign(spec$reference, matched)
  c(spec, list(
    permutation = as.integer(permutation),
    aligned = aligned,
    score = attr(permutation, "match_score"),
    margin = attr(permutation, "match_margin"),
    ambiguous = attr(permutation, "ambiguous_match"),
    method = attr(permutation, "match_method")
  ))
})

write_tsv(
  do.call(rbind, lapply(alignment_cases, function(case) data.frame(
    case = case$name,
    rows = nrow(case$reference),
    cols = ncol(case$reference),
    reference_row_major = collapse_num(as.numeric(t(case$reference))),
    replicate_row_major = collapse_num(as.numeric(t(case$replicate))),
    permutation_one_based = collapse_int(case$permutation),
    aligned_row_major = collapse_num(as.numeric(t(case$aligned))),
    match_score = case$score,
    match_margin = case$margin,
    ambiguous = case$ambiguous,
    method = case$method,
    stringsAsFactors = FALSE
  ))),
  "alignment.tsv"
)

theta <- pi / 6
angle_specs <- list(
  list(
    name = "axis_30_degrees",
    a = matrix(c(1, 0), ncol = 1),
    b = matrix(c(cos(theta), sin(theta)), ncol = 1)
  ),
  list(
    name = "orthogonal_axes",
    a = matrix(c(1, 0, 0), ncol = 1),
    b = matrix(c(0, 1, 0), ncol = 1)
  ),
  list(
    name = "planes_one_tilted_axis",
    a = cbind(c(1, 0, 0), c(0, 1, 0)),
    b = cbind(c(1, 0, 0), c(0, cos(theta), sin(theta)))
  )
)

angle_cases <- lapply(angle_specs, function(spec) {
  c(spec, list(angles = principal_angles(spec$a, spec$b)))
})

write_tsv(
  do.call(rbind, lapply(angle_cases, function(case) data.frame(
    case = case$name,
    rows = nrow(case$a),
    a_cols = ncol(case$a),
    b_cols = ncol(case$b),
    a_row_major = collapse_num(as.numeric(t(case$a))),
    b_row_major = collapse_num(as.numeric(t(case$b))),
    angles_radians = collapse_num(case$angles),
    stringsAsFactors = FALSE
  ))),
  "principal_angles.tsv"
)

u <- c(-3, -2, -1, 0, 0, 1, 2, 3)
v <- c(-1, 1, -1, 1, 1, -1, 1, -1)
x <- cbind(2 * u + 0.5 * v, -1.5 * u + v, 0.8 * v, 0.3 * u - 0.2 * v)
y <- cbind(1.5 * u - 0.2 * v, -0.5 * u + 2 * v, 0.4 * u + 0.1 * v, -0.3 * u - 0.6 * v)

pca_recipe <- infer_recipe(
  geometry = "oneblock",
  relation = "variance",
  adapter = "prcomp_oneblock"
)
pca_result <- run_oneblock_ladder(
  pca_recipe,
  x,
  B = 39L,
  B_total = 117L,
  batch_size = 5L,
  alpha = 0.1,
  max_steps = 3L,
  seed = 1729L,
  auto_subspace = TRUE,
  tie_threshold = 0.01
)

plsc_recipe <- infer_recipe(
  geometry = "cross",
  relation = "covariance",
  adapter = "cross_svd"
)
plsc_result <- run_cross_ladder(
  plsc_recipe,
  x,
  y,
  B = 39L,
  B_total = 117L,
  batch_size = 5L,
  alpha = 0.1,
  max_steps = 3L,
  seed = 2718L,
  auto_subspace = TRUE,
  tie_threshold = 0.01
)

ladder_specs <- list(
  list(
    name = "pca_exact_rank_two",
    family = "pca",
    target = "Vitale P3 tail-ratio on squared variance roots",
    null = "independent within-column row permutations of the deflated residual",
    seed = 1729L,
    x = x,
    y = NULL,
    result = pca_result
  ),
  list(
    name = "plsc_exact_rank_two",
    family = "plsc",
    target = "squared leading singular value of the deflated cross-product",
    null = "row permutation of Y relative to X",
    seed = 2718L,
    x = x,
    y = y,
    result = plsc_result
  )
)

write_tsv(matrix_long("pca_exact_rank_two", "X", x), "pca_ladder_input.tsv")
write_tsv(
  rbind(
    matrix_long("plsc_exact_rank_two", "X", x),
    matrix_long("plsc_exact_rank_two", "Y", y)
  ),
  "plsc_ladder_input.tsv"
)

write_tsv(
  do.call(rbind, lapply(ladder_specs, function(case) data.frame(
    case = case$name,
    family = case$family,
    root_one_based = seq_along(case$result$roots_observed),
    value = case$result$roots_observed,
    stringsAsFactors = FALSE
  ))),
  "ladder_roots.tsv"
)

ladder_steps <- function(case) {
  do.call(rbind, lapply(case$result$ladder_result$step_results, function(step) data.frame(
    case = case$name,
    family = case$family,
    target = case$target,
    null = case$null,
    seed = case$seed,
    alpha = 0.1,
    B_per_step = step$B,
    B_total = 117L,
    step_one_based = step$step,
    observed_stat = step$observed_stat,
    p_value = step$p_value,
    mc_se = step$mc_se,
    exceedances = step$r,
    drawn = step$drawn,
    h = step$h,
    stop_reason = step$stop_reason,
    batch_schedule = collapse_int(step$batch_schedule),
    null_values = collapse_num(step$null_values),
    selected = step$selected,
    stringsAsFactors = FALSE
  )))
}
write_tsv(do.call(rbind, lapply(ladder_specs, ladder_steps)), "ladder_steps.tsv")

ladder_units <- function(case) {
  units <- case$result$units
  members <- attr(units, "members")
  do.call(rbind, lapply(seq_len(nrow(units)), function(i) data.frame(
    case = case$name,
    unit_id = units$unit_id[[i]],
    unit_type = units$unit_type[[i]],
    members_one_based = collapse_int(members[[i]]),
    identifiable = units$identifiable[[i]],
    selected = units$selected[[i]],
    stringsAsFactors = FALSE
  )))
}
write_tsv(do.call(rbind, lapply(ladder_specs, ladder_units)), "ladder_units.tsv")

scala_string <- function(value) {
  escaped <- gsub("\\\\", "\\\\\\\\", value)
  escaped <- gsub('"', '\\\\"', escaped)
  paste0('"', escaped, '"')
}

scala_double <- function(value) {
  text <- fmt_num(value)
  if (text == "NaN") return("Double.NaN")
  if (text == "Inf") return("Double.PositiveInfinity")
  if (text == "-Inf") return("Double.NegativeInfinity")
  if (!grepl("[.eE]", text)) paste0(text, ".0") else text
}

scala_vector <- function(values, render) {
  paste0("Vector(", paste(vapply(values, render, character(1L)), collapse = ", "), ")")
}

scala_double_vector <- function(values) scala_vector(values, scala_double)
scala_int_vector <- function(values) scala_vector(as.integer(values), as.character)
scala_bool_vector <- function(values) scala_vector(values, function(value) tolower(as.character(value)))

scala_matrix <- function(value) {
  paste0(
    "MatrixData(", nrow(value), ", ", ncol(value), ", ",
    scala_double_vector(as.numeric(t(value))), ")"
  )
}

scala_unit <- function(unit_id, unit_type, members, identifiable, selected) {
  paste0(
    "UnitExpectation(", scala_string(unit_id), ", ", scala_string(unit_type), ", ",
    scala_int_vector(members), ", ", tolower(as.character(identifiable)), ", ",
    tolower(as.character(selected)), ")"
  )
}

scala_units <- function(units, members) {
  rendered <- vapply(seq_len(nrow(units)), function(i) {
    scala_unit(
      units$unit_id[[i]],
      units$unit_type[[i]],
      members[[i]],
      units$identifiable[[i]],
      units$selected[[i]]
    )
  }, character(1L))
  paste0("Vector(\n      ", paste(rendered, collapse = ",\n      "), "\n    )")
}

scala_fixed <- vapply(fixed_cases, function(case) paste0(
  "FixedMonteCarloCase(",
  scala_string(case$name), ", ", scala_string(case$alternative), ", ",
  scala_double(case$observed), ", ", scala_double_vector(case$null), ", ",
  case$result$r, ", ", scala_double(case$result$p_value), ", ",
  scala_double(case$result$mc_se), ")"
), character(1L))

scala_sequential <- vapply(sequential_cases, function(case) paste0(
  "SequentialMonteCarloCase(",
  scala_string(case$name), ", ", scala_string("greater"), ", ",
  scala_double(case$observed), ", ", scala_double_vector(case$result$null_values), ", ",
  case$result$B_max, ", ", scala_double(case$alpha), ", ", case$result$h, ", ",
  scala_int_vector(case$result$batch_schedule), ", ", case$result$r, ", ",
  case$result$drawn, ", ", scala_string(case$result$stop_reason), ", ",
  scala_double(case$result$p_value), ", ", scala_double(case$result$mc_se), ")"
), character(1L))

scala_unit_cases <- vapply(unit_cases, function(case) paste0(
  "UnitFormationCase(\n      ",
  scala_string(case$name), ", ", scala_double_vector(case$roots), ", ",
  scala_bool_vector(case$selected), ", ",
  tolower(as.character(case$group_near_ties)), ", ", scala_double(case$tie_threshold), ",\n      ",
  scala_units(case$result, case$members), "\n    )"
), character(1L))

scala_alignment <- vapply(alignment_cases, function(case) paste0(
  "AlignmentCase(\n      ", scala_string(case$name), ", ",
  scala_matrix(case$reference), ", ", scala_matrix(case$replicate), ",\n      ",
  scala_int_vector(case$permutation), ", ", scala_matrix(case$aligned), ", ",
  scala_double(case$score), ", ", scala_double(case$margin), ", ",
  tolower(as.character(case$ambiguous)), ", ", scala_string(case$method), "\n    )"
), character(1L))

scala_angles <- vapply(angle_cases, function(case) paste0(
  "PrincipalAngleCase(", scala_string(case$name), ", ", scala_matrix(case$a), ", ",
  scala_matrix(case$b), ", ", scala_double_vector(case$angles), ")"
), character(1L))

scala_ladder_step <- function(step) paste0(
  "LadderStep(", step$step, ", ", scala_double(step$observed_stat), ", ",
  scala_double(step$p_value), ", ", scala_double(step$mc_se), ", ", step$r, ", ",
  step$B, ", ", step$drawn, ", ", step$h, ", ",
  scala_string(step$stop_reason), ", ", scala_int_vector(step$batch_schedule), ", ",
  scala_double_vector(step$null_values), ", ", tolower(as.character(step$selected)), ")"
)

scala_ladder <- vapply(ladder_specs, function(case) {
  steps <- vapply(case$result$ladder_result$step_results, scala_ladder_step, character(1L))
  y_value <- if (is.null(case$y)) "None" else paste0("Some(", scala_matrix(case$y), ")")
  paste0(
    "LadderFixture(\n      ", scala_string(case$name), ", ", scala_string(case$family), ",\n      ",
    scala_string(case$target), ",\n      ", scala_string(case$null), ", ", case$seed, "L,\n      ",
    scala_matrix(case$x), ", ", y_value, ",\n      ",
    scala_double_vector(case$result$roots_observed), ",\n      ",
    scala_units(case$result$units, attr(case$result$units, "members")), ",\n      Vector(\n        ",
    paste(steps, collapse = ",\n        "), "\n      ), ",
    case$result$ladder_result$rejected_through, ", ",
    case$result$ladder_result$last_step_tested, "\n    )"
  )
}, character(1L))

hash_entries <- paste0(
  "      ",
  vapply(seq_along(source_files), function(i) paste0(
    scala_string(source_files[[i]]), " -> ", scala_string(unname(source_hashes[[i]]))
  ), character(1L)),
  collapse = ",\n"
)

scala_source <- c(
  "package scalafim.inference",
  "",
  "/** Generated by modules/inference/fixtures/generate.R from multifer.",
  "  * Do not hand-edit: regenerate from the recorded reference checkout.",
  "  */",
  "object InferenceRReferenceFixtures:",
  "  final case class MatrixData(rows: Int, cols: Int, values: Vector[Double]):",
  "    require(rows > 0 && cols > 0)",
  "    require(values.length == rows * cols)",
  "",
  "    def apply(row: Int, col: Int): Double =",
  "      values(row * cols + col)",
  "",
  "  final case class FixedMonteCarloCase(",
  "      name: String, alternative: String, observed: Double, nullValues: Vector[Double],",
  "      exceedances: Int, pValue: Double, mcSe: Double",
  "  )",
  "  final case class SequentialMonteCarloCase(",
  "      name: String, alternative: String, observed: Double, nullValues: Vector[Double],",
  "      maxDraws: Int, alpha: Double, boundary: Int, batchSchedule: Vector[Int],",
  "      exceedances: Int, drawn: Int, stopReason: String, pValue: Double, mcSe: Double",
  "  )",
  "  final case class UnitExpectation(",
  "      id: String, kind: String, membersOneBased: Vector[Int], identifiable: Boolean, selected: Boolean",
  "  )",
  "  final case class UnitFormationCase(",
  "      name: String, roots: Vector[Double], selectedInput: Vector[Boolean],",
  "      groupNearTies: Boolean, tieThreshold: Double, units: Vector[UnitExpectation]",
  "  )",
  "  final case class AlignmentCase(",
  "      name: String, reference: MatrixData, replicate: MatrixData,",
  "      permutationOneBased: Vector[Int], aligned: MatrixData, matchScore: Double,",
  "      matchMargin: Double, ambiguous: Boolean, method: String",
  "  )",
  "  final case class PrincipalAngleCase(",
  "      name: String, a: MatrixData, b: MatrixData, anglesRadians: Vector[Double]",
  "  )",
  "  final case class LadderStep(",
  "      stepOneBased: Int, observed: Double, pValue: Double, mcSe: Double,",
  "      exceedances: Int, allocated: Int, drawn: Int, boundary: Int, stopReason: String,",
  "      batchSchedule: Vector[Int], nullValues: Vector[Double], selected: Boolean",
  "  )",
  "  final case class LadderFixture(",
  "      name: String, family: String, target: String, nullAction: String, seed: Long,",
  "      x: MatrixData, y: Option[MatrixData], roots: Vector[Double],",
  "      units: Vector[UnitExpectation], steps: Vector[LadderStep],",
  "      rejectedThrough: Int, lastStepTested: Int",
  "  )",
  "",
  paste0("  val schema: String = ", scala_string("scalafim-inference-r-v1")),
  paste0("  val multiferVersion: String = ", scala_string(package_versions[["multifer"]])),
  paste0("  val multiferCommit: String = ", scala_string(git_commit[[1L]])),
  paste0("  val multiferDirty: Boolean = ", tolower(as.character(git_dirty))),
  paste0("  val multiferSourcesDirty: Boolean = ", tolower(as.character(source_files_dirty))),
  paste0("  val rVersion: String = ", scala_string(R.version.string)),
  paste0("  val rngKind: String = ", scala_string(paste(rng, collapse = "/"))),
  "  val sourceSha256: Map[String, String] =",
  "    Map(",
  hash_entries,
  "    )",
  "",
  "  val fixedMonteCarlo: Vector[FixedMonteCarloCase] =",
  paste0("    Vector(\n      ", paste(scala_fixed, collapse = ",\n      "), "\n    )"),
  "",
  "  val sequentialMonteCarlo: Vector[SequentialMonteCarloCase] =",
  paste0("    Vector(\n      ", paste(scala_sequential, collapse = ",\n      "), "\n    )"),
  "",
  "  val unitFormation: Vector[UnitFormationCase] =",
  paste0("    Vector(\n      ", paste(scala_unit_cases, collapse = ",\n      "), "\n    )"),
  "",
  "  val alignment: Vector[AlignmentCase] =",
  paste0("    Vector(\n      ", paste(scala_alignment, collapse = ",\n      "), "\n    )"),
  "",
  "  val principalAngles: Vector[PrincipalAngleCase] =",
  paste0("    Vector(\n      ", paste(scala_angles, collapse = ",\n      "), "\n    )"),
  "",
  "  val ladders: Vector[LadderFixture] =",
  paste0("    Vector(\n      ", paste(scala_ladder, collapse = ",\n      "), "\n    )")
)

writeLines(
  scala_source,
  con = file.path(scala_dir, "InferenceRReferenceFixtures.scala"),
  useBytes = TRUE
)
