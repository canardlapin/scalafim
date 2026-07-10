#!/usr/bin/env Rscript

# Regenerate the Scala golden fixtures used by RParityCorpusSuite.
#
# From the scalafim repository root:
#   Rscript tools/r-parity/generate_fmridesign_r_parity_fixtures.R
#
# By default this loads the live R source at ~/code/fmridesign with pkgload and
# writes the design module's RParityFixtures.scala. Override those paths with:
#   FMRIDESIGN_R=/path/to/fmridesign RPARITY_OUT=/path/to/RParityFixtures.scala \
#     Rscript tools/r-parity/generate_fmridesign_r_parity_fixtures.R

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required to generate parity fixtures", call. = FALSE)
}

r_pkg <- Sys.getenv("FMRIDESIGN_R", file.path(path.expand("~"), "code", "fmridesign"))
out_file <- Sys.getenv(
  "RPARITY_OUT",
  file.path(
    "modules", "design", "shared", "src", "test", "scala",
    "scalafim", "fmri", "design", "fixtures", "RParityFixtures.scala"
  )
)

pkgload::load_all(r_pkg, quiet = TRUE)

`%||%` <- function(x, y) if (is.null(x)) y else x

scala_string <- function(x) {
  paste0("\"", gsub("\\\\", "\\\\\\\\", gsub("\"", "\\\\\"", x)), "\"")
}

scala_number <- function(x) {
  if (is.nan(x)) {
    "Double.NaN"
  } else if (is.infinite(x) && x > 0) {
    "Double.PositiveInfinity"
  } else if (is.infinite(x) && x < 0) {
    "Double.NegativeInfinity"
  } else {
    sprintf("%.9f", x)
  }
}

scala_vector <- function(xs, render, typed_empty = "Vector.empty") {
  if (length(xs) == 0) {
    typed_empty
  } else {
    paste0("Vector(", paste(vapply(xs, render, character(1)), collapse = ", "), ")")
  }
}

scala_vector_string <- function(xs) {
  scala_vector(as.character(xs), scala_string, "Vector.empty")
}

scala_vector_double <- function(xs) {
  scala_vector(as.numeric(xs), scala_number, "Vector.empty")
}

scala_vector_int <- function(xs) {
  scala_vector(as.integer(xs), function(x) as.character(as.integer(x)), "Vector.empty")
}

scala_named_int_map <- function(x) {
  if (length(x) == 0) {
    "Map.empty"
  } else {
    entries <- vapply(names(x), function(key) {
      paste0(scala_string(key), " -> ", scala_vector_int(x[[key]]))
    }, character(1))
    paste0("Map(", paste(entries, collapse = ", "), ")")
  }
}

matrix_expr <- function(mat) {
  mat <- as.matrix(mat)
  paste0(
    "MatrixFixture(rows = ", nrow(mat),
    ", cols = ", ncol(mat),
    ", columnNames = ", scala_vector_string(colnames(mat) %||% character(0)),
    ", values = ", scala_vector_double(as.vector(t(mat))),
    ")"
  )
}

contrast_items <- function(xs) {
  if (is.null(xs) || length(xs) == 0) {
    return(list())
  }

  lapply(names(xs), function(key) {
    value <- xs[[key]]
    weights <- if (is.list(value) && !is.null(value$weights)) value$weights else value
    mat <- as.matrix(weights)
    list(
      key = key,
      rows = nrow(mat),
      cols = ncol(mat),
      row_names = rownames(mat) %||% character(0),
      column_names = colnames(mat) %||% character(0),
      values = as.vector(t(mat))
    )
  })
}

contrast_expr <- function(item) {
  paste0(
    "ContrastFixture(key = ", scala_string(item$key),
    ", rows = ", item$rows,
    ", cols = ", item$cols,
    ", rowNames = ", scala_vector_string(item$row_names),
    ", columnNames = ", scala_vector_string(item$column_names),
    ", values = ", scala_vector_double(item$values),
    ")"
  )
}

scala_contrast_vector <- function(items) {
  if (length(items) == 0) {
    "Vector.empty"
  } else {
    paste0("Vector(", paste(vapply(items, contrast_expr, character(1)), collapse = ", "), ")")
  }
}

event_fixture_expr <- function(model) {
  dm <- as.matrix(design_matrix(model))
  dm0 <- design_matrix(model)
  attached <- tryCatch(contrast_weights(model), error = function(e) list())
  f_contrasts <- tryCatch(suppressWarnings(Fcontrasts(model)), error = function(e) list())

  paste0(
    "EventFixture(",
    "termKeys = ", scala_vector_string(names(terms(model))), ", ",
    "matrix = ", matrix_expr(dm), ", ",
    "rColIndices1Based = ", scala_named_int_map(attr(dm0, "col_indices") %||% list()), ", ",
    "rTermSpans1Based = ", scala_vector_int(attr(dm0, "term_spans") %||% integer(0)), ", ",
    "attachedContrasts = ", scala_contrast_vector(contrast_items(attached)), ", ",
    "fContrasts = ", scala_contrast_vector(contrast_items(f_contrasts)),
    ")"
  )
}

baseline_fixture_expr <- function(model) {
  dm <- as.matrix(design_matrix(model))
  paste0(
    "BaselineFixture(",
    "termKeys = ", scala_vector_string(names(terms(model))), ", ",
    "matrix = ", matrix_expr(dm),
    ")"
  )
}

sf_interaction <- fmrihrf::sampling_frame(blocklens = 12, TR = 1)
event_interaction_data <- data.frame(
  onset = c(1, 5, 9),
  block = 1,
  cond = factor(c("A", "B", "A")),
  x = c(1, 2, 3)
)
event_interaction_contrasts <- contrast_set(
  diff = column_contrast(pattern_A = "cond.A", pattern_B = "cond.B", name = "diff")
)
event_interaction <- event_model(
  onset ~ hrf(cond, x, name = "task", contrasts = event_interaction_contrasts),
  data = event_interaction_data,
  block = ~block,
  sampling_frame = sf_interaction
)

sf_event <- fmrihrf::sampling_frame(blocklens = 20, TR = 1)
event_spmg3_data <- data.frame(
  onset = c(1, 5, 9),
  block = 1,
  cond = factor(c("A", "A", "A"))
)
event_spmg3 <- event_model(
  onset ~ hrf(cond, basis = "spmg3", id = "task"),
  data = event_spmg3_data,
  block = ~block,
  sampling_frame = sf_event
)

event_spmg3_twocond_data <- data.frame(
  onset = c(1, 5, 9, 13),
  block = 1,
  cond = factor(c("A", "B", "A", "B"))
)
event_spmg3_twocond <- event_model(
  onset ~ hrf(cond, basis = "spmg3", id = "task"),
  data = event_spmg3_twocond_data,
  block = ~block,
  sampling_frame = sf_event
)

event_subset_data <- data.frame(
  onset = c(1, 3, 5, 7),
  block = 1,
  cond = factor(c("A", "B", "A", "B")),
  keep = c(TRUE, FALSE, TRUE, FALSE)
)
event_subset <- event_model(
  onset ~ hrf(cond, subset = keep, id = "task"),
  data = event_subset_data,
  block = ~block,
  sampling_frame = sf_event
)

event_trialwise_data <- data.frame(
  onset = c(1, 5, 9),
  block = 1
)
event_trialwise <- event_model(
  onset ~ trialwise(add_sum = TRUE),
  data = event_trialwise_data,
  block = ~block,
  sampling_frame = sf_event
)

event_weighted_data <- data.frame(
  onset = c(0, 4),
  block = 1,
  cond = factor(c("A", "B"))
)
event_weighted_data$sub_times <- I(list(c(0, 1), c(0, 2)))
event_weighted_data$sub_weights <- I(list(c(0.5, 0.5), c(0.25, 0.75)))
event_weighted <- event_model(
  onset ~ hrf(cond, hrf_fun = weighted_hrf_gen("sub_times", "sub_weights", relative = TRUE), id = "weighted"),
  data = event_weighted_data,
  block = ~block,
  sampling_frame = fmrihrf::sampling_frame(blocklens = 12, TR = 1)
)

sf_base <- fmrihrf::sampling_frame(blocklens = c(3, 2), TR = 1)
baseline_constant_runwise <- baseline_model(
  basis = "constant",
  sframe = sf_base,
  intercept = "runwise"
)

sf_nuisance <- fmrihrf::sampling_frame(blocklens = c(4, 3), TR = 1)
nuisance_1 <- matrix(c(0, 1, 2, 3, 0, 1, 2, 3, 1, 0, 1, 0), nrow = 4)
colnames(nuisance_1) <- c("dvars", "dvars_dup", "motion")
nuisance_2 <- matrix(c(1, 2, 3, 0, 0, 0), nrow = 3)
colnames(nuisance_2) <- c("motion_x", "zero_col")
baseline_nuisance_drop <- suppressWarnings(baseline_model(
  basis = "constant",
  sframe = sf_nuisance,
  intercept = "runwise",
  nuisance_list = list(nuisance_1, nuisance_2),
  nuisance_check = "drop"
))

fixtures <- c(
  "package scalafim.fmri.design.fixtures",
  "",
  "// Generated by tools/r-parity/generate_fmridesign_r_parity_fixtures.R from the live R fmridesign source.",
  "object RParityFixtures:",
  "  final case class MatrixFixture(rows: Int, cols: Int, columnNames: Vector[String], values: Vector[Double])",
  "  final case class ContrastFixture(",
  "      key: String,",
  "      rows: Int,",
  "      cols: Int,",
  "      rowNames: Vector[String],",
  "      columnNames: Vector[String],",
  "      values: Vector[Double]",
  "  )",
  "  final case class EventFixture(",
  "      termKeys: Vector[String],",
  "      matrix: MatrixFixture,",
  "      rColIndices1Based: Map[String, Vector[Int]],",
  "      rTermSpans1Based: Vector[Int],",
  "      attachedContrasts: Vector[ContrastFixture],",
  "      fContrasts: Vector[ContrastFixture]",
  "  )",
  "  final case class BaselineFixture(termKeys: Vector[String], matrix: MatrixFixture)",
  "",
  paste0("  val eventInteraction: EventFixture = ", event_fixture_expr(event_interaction)),
  "",
  paste0("  val eventSpmg3: EventFixture = ", event_fixture_expr(event_spmg3)),
  "",
  paste0("  val eventSpmg3TwoCondition: EventFixture = ", event_fixture_expr(event_spmg3_twocond)),
  "",
  paste0("  val eventSubset: EventFixture = ", event_fixture_expr(event_subset)),
  "",
  paste0("  val eventTrialwiseAddSum: EventFixture = ", event_fixture_expr(event_trialwise)),
  "",
  paste0("  val eventWeightedHrfFun: EventFixture = ", event_fixture_expr(event_weighted)),
  "",
  paste0("  val baselineConstantRunwise: BaselineFixture = ", baseline_fixture_expr(baseline_constant_runwise)),
  "",
  paste0("  val baselineNuisanceDrop: BaselineFixture = ", baseline_fixture_expr(baseline_nuisance_drop))
)

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
writeLines(fixtures, out_file)
message("wrote ", out_file)
