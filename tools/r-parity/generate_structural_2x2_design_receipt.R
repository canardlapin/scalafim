#!/usr/bin/env Rscript

# Generate the external R design receipt used by the structural S07 scenario.
#
# From the ScalaFIM repository root:
#   Rscript tools/r-parity/generate_structural_2x2_design_receipt.R
#   python tools/r-parity/finalize_structural_2x2_design_receipt.py

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required to generate the R design receipt", call. = FALSE)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to generate the R design receipt", call. = FALSE)
}

r_pkg <- Sys.getenv("FMRIDESIGN_R", file.path(path.expand("~"), "code", "fmridesign"))
hrf_pkg <- Sys.getenv("FMRIHRF_R", file.path(path.expand("~"), "code", "fmrihrf"))
out_file <- Sys.getenv(
  "STRUCTURAL_2X2_RECEIPT_OUT",
  file.path("docs", "scenarios", "fixtures", "design.structural-2x2.v1.r.json")
)
scala_out <- Sys.getenv(
  "STRUCTURAL_2X2_SCALA_OUT",
  file.path(
    "modules", "design", "shared", "src", "test", "scala",
    "scalafim", "fmri", "design", "fixtures", "Structural2x2RFixture.scala"
  )
)

pkgload::load_all(r_pkg, quiet = TRUE)

git_revision <- function(path) {
  value <- tryCatch(
    system2("git", c("-C", path, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(...) character(0)
  )
  if (length(value) == 0 || !nzchar(value[[1]])) "unresolved" else value[[1]]
}

events <- data.frame(
  onset = c(2, 12, 22, 32, 42, 52, 62, 70),
  block = 1,
  f1 = factor(c("A", "B", "A", "B", "A", "B", "A", "B"), levels = c("A", "B")),
  f2 = factor(c("X", "X", "Y", "Y", "X", "X", "Y", "Y"), levels = c("X", "Y"))
)
formula_text <- "onset ~ hrf(f1, f2, basis = \"spmg3\", id = \"task\")"
sampling_frame <- fmrihrf::sampling_frame(blocklens = 80, TR = 1)
model <- event_model(
  stats::as.formula(formula_text),
  data = events,
  block = ~block,
  sampling_frame = sampling_frame,
  precision = 0.1
)
matrix <- unname(as.matrix(design_matrix(model)))
raw_matrix <- as.matrix(design_matrix(model))
matrix <- unname(raw_matrix)
matrix_rows <- lapply(seq_len(nrow(matrix)), function(index) unname(matrix[index, ]))
design_metadata <- design_matrix(model)

fmridesign_root <- normalizePath(r_pkg, mustWork = FALSE)
source <- list(
  fmridesign_revision = git_revision(fmridesign_root),
  fmrihrf_revision = git_revision(normalizePath(hrf_pkg, mustWork = FALSE)),
  fmridesign_version = as.character(utils::packageVersion("fmridesign")),
  fmrihrf_version = as.character(utils::packageVersion("fmrihrf")),
  r_version = as.character(getRversion()),
  producer = "tools/r-parity/generate_structural_2x2_design_receipt.R",
  reference = "R fmridesign event_model/design_matrix"
)

inputs <- list(
  formula = formula_text,
  events = list(
    onset = events$onset,
    block = events$block,
    f1 = as.character(events$f1),
    f2 = as.character(events$f2),
    f1_levels = levels(events$f1),
    f2_levels = levels(events$f2)
  ),
  sampling_frame = list(blocklens = 80, tr = 1),
  precision = 0.1,
  basis = "spmg3"
)

outputs <- list(
  rows = nrow(matrix),
  cols = ncol(matrix),
  column_names = colnames(raw_matrix),
  matrix = matrix_rows,
  term_keys = names(terms(model)),
  col_indices_1_based = attr(design_metadata, "col_indices"),
  term_spans_1_based = attr(design_metadata, "term_spans")
)

scala_string <- function(value) {
  paste0("\"", gsub("\\\\", "\\\\\\\\", gsub("\"", "\\\\\"", value)), "\"")
}
scala_number <- function(value) {
  if (is.nan(value)) "Double.NaN" else if (is.infinite(value) && value > 0) "Double.PositiveInfinity" else if (is.infinite(value) && value < 0) "Double.NegativeInfinity" else sprintf("%.9f", value)
}
scala_vector <- function(values, render, empty = "Vector.empty") {
  if (length(values) == 0) empty else paste0("Vector(", paste(vapply(values, render, character(1)), collapse = ", "), ")")
}
scala_vector_string <- function(values) scala_vector(as.character(values), scala_string)
scala_vector_double <- function(values) scala_vector(as.numeric(values), scala_number)
scala_vector_int <- function(values) scala_vector(as.integer(values), function(value) as.character(as.integer(value)))
scala_named_int_map <- function(values) {
  if (length(values) == 0) "Map.empty"
  else paste0(
    "Map(",
    paste(vapply(names(values), function(key) paste0(scala_string(key), " -> ", scala_vector_int(values[[key]])), character(1)), collapse = ", "),
    ")"
  )
}
scala_matrix_fixture <- paste0(
  "RParityFixtures.MatrixFixture(rows = ", nrow(matrix), ", cols = ", ncol(matrix),
  ", columnNames = ", scala_vector_string(colnames(raw_matrix)),
  ", values = ", scala_vector_double(as.vector(t(matrix))), ")"
)
scala_event_fixture <- paste0(
  "RParityFixtures.EventFixture(termKeys = ", scala_vector_string(names(terms(model))),
  ", matrix = ", scala_matrix_fixture,
  ", rColIndices1Based = ", scala_named_int_map(attr(design_metadata, "col_indices")),
  ", rTermSpans1Based = ", scala_vector_int(attr(design_metadata, "term_spans")),
  ", attachedContrasts = Vector.empty, fContrasts = Vector.empty)"
)

receipt <- list(
  schema_version = "scalafim-r-design-fixture/v1",
  producer_command = "LC_ALL=C LANG=C Rscript tools/r-parity/generate_structural_2x2_design_receipt.R && python3 tools/r-parity/finalize_structural_2x2_design_receipt.py",
  accepted_differences = character(0),
  conventions = list(
    dtype = "float64",
    json_encoding = "sorted-key UTF-8 JSON",
    matrix_orientation = "rows=acquisition samples, columns=realized regressors",
    hrf_basis = "R fmridesign basis=spmg3",
    convolution_precision = "0.1 seconds in both R and Scala builders",
    column_order = "R fmridesign raw values are condition-major while column names are basis-major; Scala comparison transposes values to basis-major by semantic condition"
  ),
  hashes = list(inputs_sha256 = "", outputs_sha256 = ""),
  source = source
)

payload <- list(
  schema_version = "scalafim-r-design-fixture/v1",
  scenario_id = "design.structural-2x2.v1",
  inputs = inputs,
  outputs = outputs,
  source = source,
  receipt = receipt
)

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(payload, out_file, auto_unbox = TRUE, digits = 17, pretty = TRUE)
message("wrote ", out_file)

dir.create(dirname(scala_out), recursive = TRUE, showWarnings = FALSE)
writeLines(
  c(
    "package scalafim.fmri.design.fixtures",
    "",
    "// Generated by tools/r-parity/generate_structural_2x2_design_receipt.R from the live R fmridesign source.",
    "object Structural2x2RFixture:",
    paste0("  val eventStructural2x2: RParityFixtures.EventFixture = ", scala_event_fixture)
  ),
  scala_out
)
message("wrote ", scala_out)
