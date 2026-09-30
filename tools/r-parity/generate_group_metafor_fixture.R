#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args

if (!requireNamespace("metafor", quietly = TRUE)) {
  stop("metafor is required to generate the group fixture", call. = FALSE)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to generate the group fixture", call. = FALSE)
}
if (!file.exists("build.sbt")) {
  stop("run this script from the ScalaFIM repository root", call. = FALSE)
}

json_path <- file.path(
  "docs", "scenarios", "fixtures", "group.meta-analysis.v1.metafor.json"
)
scala_path <- file.path(
  "modules", "group", "shared", "src", "test", "scala", "scalafim", "fmri",
  "group", "fixtures", "GroupMetaforFixtures.scala"
)

cases <- list(
  list(
    id = "intercept_heterogeneous",
    terms = c("intercept"),
    design = matrix(1, nrow = 8, ncol = 1),
    effects = c(-0.30, 0.10, 0.25, 0.40, 0.80, 1.10, 1.40, 1.80),
    variances = c(0.04, 0.09, 0.16, 0.25, 0.36, 0.49, 0.64, 0.81)
  ),
  list(
    id = "offset_moderator",
    terms = c("intercept", "age"),
    design = cbind(
      intercept = rep(1, 10),
      age = c(42, 45, 47, 50, 54, 57, 61, 64, 68, 73)
    ),
    effects = c(0.22, 0.08, 0.31, 0.41, 0.35, 0.62, 0.79, 0.70, 1.02, 1.15),
    variances = c(0.08, 0.13, 0.05, 0.21, 0.11, 0.27, 0.09, 0.18, 0.14, 0.31)
  ),
  list(
    id = "two_moderators",
    terms = c("intercept", "dose", "site"),
    design = cbind(
      intercept = rep(1, 12),
      dose = c(-1.8, -1.3, -0.9, -0.4, -0.1, 0.2, 0.6, 0.9, 1.3, 1.7, 2.1, 2.5),
      site = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
    ),
    effects = c(-0.15, 0.34, 0.02, 0.48, 0.27, 0.61, 0.44, 0.95, 0.72, 1.18, 0.91, 1.42),
    variances = c(0.06, 0.18, 0.09, 0.22, 0.07, 0.14, 0.11, 0.29, 0.08, 0.19, 0.13, 0.25)
  )
)

policies <- list(
  list(id = "FE-z", method = "FE", test = "z"),
  list(id = "DL-z", method = "DL", test = "z"),
  list(id = "PM-z", method = "PM", test = "z"),
  list(id = "DL-mKH", method = "DL", test = "adhoc"),
  list(id = "PM-mKH", method = "PM", test = "adhoc")
)

json_array <- function(values) unname(as.list(values))

fit_policy <- function(case, policy) {
  fit <- metafor::rma.uni(
    yi = case$effects,
    vi = case$variances,
    mods = case$design,
    intercept = FALSE,
    method = policy$method,
    test = policy$test,
    control = list(tol = 1e-10)
  )
  list(
    id = policy$id,
    method = policy$method,
    test = policy$test,
    coefficients = json_array(as.numeric(fit$beta)),
    standard_errors = json_array(as.numeric(fit$se)),
    covariance_row_major = json_array(as.numeric(t(fit$vb))),
    statistics = json_array(as.numeric(fit$zval)),
    p_values = json_array(as.numeric(fit$pval)),
    tau2 = as.numeric(fit$tau2),
    fixed_q = as.numeric(fit$QE),
    residual_df = as.integer(fit$k - fit$p)
  )
}

fixture_cases <- lapply(cases, function(case) {
  list(
    id = case$id,
    subjects = nrow(case$design),
    terms = json_array(case$terms),
    design_row_major = json_array(as.numeric(t(case$design))),
    effects = json_array(case$effects),
    variances = json_array(case$variances),
    expected = lapply(policies, function(policy) fit_policy(case, policy))
  )
})

fixture <- list(
  schema_version = "scalafim-group-metafor-fixture/v1",
  generated_by = "tools/r-parity/generate_group_metafor_fixture.R",
  oracle = list(
    runtime = paste("R", getRversion()),
    package = "metafor",
    package_version = as.character(utils::packageVersion("metafor")),
    estimator_call = "metafor::rma.uni(yi, vi, mods=X, intercept=FALSE, method, test, control=list(tol=1e-10))",
    modified_knapp_hartung = "test=adhoc (Knapp-Hartung scale constrained to be at least one)"
  ),
  comparator = list(
    absolute_tolerance = 1e-8,
    relative_tolerance = 1e-8,
    p_value_tolerance = 1e-7,
    rationale = "small full-rank dense fixtures use tight coefficient/covariance tolerances; p-values retain the portable distribution implementation's documented 1e-7 approximation bound"
  ),
  cases = fixture_cases
)

json_text <- paste0(
  jsonlite::toJSON(fixture, auto_unbox = TRUE, pretty = TRUE, digits = 17),
  "\n"
)

scala_number <- function(value) {
  if (is.nan(value)) return("Double.NaN")
  if (is.infinite(value) && value > 0) return("Double.PositiveInfinity")
  if (is.infinite(value)) return("Double.NegativeInfinity")
  formatted <- format(value, digits = 17, scientific = TRUE, trim = TRUE)
  if (!grepl("[.eE]", formatted)) formatted <- paste0(formatted, ".0")
  formatted
}

scala_string <- function(value) {
  paste0("\"", gsub("\"", "\\\\\"", value, fixed = TRUE), "\"")
}

scala_vector_numbers <- function(values, indent = "") {
  paste0(indent, "Vector(", paste(vapply(values, scala_number, character(1)), collapse = ", "), ")")
}

scala_vector_strings <- function(values, indent = "") {
  paste0(indent, "Vector(", paste(vapply(values, scala_string, character(1)), collapse = ", "), ")")
}

expected_scala <- function(expected) {
  paste0(
    "      Expected(\n",
    "        policy = ", scala_string(expected$id), ",\n",
    "        coefficients = ", scala_vector_numbers(expected$coefficients), ",\n",
    "        standardErrors = ", scala_vector_numbers(expected$standard_errors), ",\n",
    "        covarianceRowMajor = ", scala_vector_numbers(expected$covariance_row_major), ",\n",
    "        statistics = ", scala_vector_numbers(expected$statistics), ",\n",
    "        pValues = ", scala_vector_numbers(expected$p_values), ",\n",
    "        tau2 = ", scala_number(expected$tau2), ",\n",
    "        fixedQ = ", scala_number(expected$fixed_q), ",\n",
    "        residualDf = ", expected$residual_df, "\n",
    "      )"
  )
}

case_scala <- function(case) {
  expected <- paste(vapply(case$expected, expected_scala, character(1)), collapse = ",\n")
  paste0(
    "    Case(\n",
    "      id = ", scala_string(case$id), ",\n",
    "      subjects = ", case$subjects, ",\n",
    "      terms = ", scala_vector_strings(case$terms), ",\n",
    "      designRowMajor = ", scala_vector_numbers(case$design_row_major), ",\n",
    "      effects = ", scala_vector_numbers(case$effects), ",\n",
    "      variances = ", scala_vector_numbers(case$variances), ",\n",
    "      expected = Vector(\n", expected, "\n      )\n",
    "    )"
  )
}

scala_cases <- paste(vapply(fixture_cases, case_scala, character(1)), collapse = ",\n")
scala_text <- paste0(
  "// Generated by tools/r-parity/generate_group_metafor_fixture.R. Do not edit by hand.\n",
  "package scalafim.fmri.group.fixtures\n\n",
  "object GroupMetaforFixtures:\n",
  "  val SchemaVersion = \"scalafim-group-metafor-fixture/v1\"\n",
  "  val Oracle = \"metafor ", as.character(utils::packageVersion("metafor")), "\"\n",
  "  val AbsoluteTolerance = 1e-8\n",
  "  val RelativeTolerance = 1e-8\n",
  "  val PValueTolerance = 1e-7\n\n",
  "  final case class Expected(\n",
  "      policy: String,\n",
  "      coefficients: Vector[Double],\n",
  "      standardErrors: Vector[Double],\n",
  "      covarianceRowMajor: Vector[Double],\n",
  "      statistics: Vector[Double],\n",
  "      pValues: Vector[Double],\n",
  "      tau2: Double,\n",
  "      fixedQ: Double,\n",
  "      residualDf: Int\n",
  "  )\n\n",
  "  final case class Case(\n",
  "      id: String,\n",
  "      subjects: Int,\n",
  "      terms: Vector[String],\n",
  "      designRowMajor: Vector[Double],\n",
  "      effects: Vector[Double],\n",
  "      variances: Vector[Double],\n",
  "      expected: Vector[Expected]\n",
  "  )\n\n",
  "  val Cases: Vector[Case] = Vector(\n",
  scala_cases, "\n",
  "  )\n"
)

write_or_check <- function(path, text) {
  if (check_only) {
    if (!file.exists(path)) stop(paste("missing generated fixture", path), call. = FALSE)
    current <- paste0(paste(readLines(path, warn = FALSE), collapse = "\n"), "\n")
    if (!identical(current, text)) stop(paste("generated fixture differs from", path), call. = FALSE)
  } else {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(sub("\n$", "", text), path, useBytes = TRUE)
  }
}

write_or_check(json_path, json_text)
write_or_check(scala_path, scala_text)

if (check_only) {
  message("group metafor fixtures are current")
} else {
  message("wrote ", json_path)
  message("wrote ", scala_path)
}
