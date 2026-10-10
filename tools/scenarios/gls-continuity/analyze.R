#!/usr/bin/env Rscript
# Retain every adverse-domain result; apply only the predeclared long-cell admission.
Sys.setenv(SCALAFIM_GLS_ANALYSIS_PROTOCOL="tools/scenarios/gls-continuity/protocol.json")
source("tools/scenarios/corrected-gls/analyze.R")
eligible <- "ar2-high-censored-voxelwise-long-adaptive-continuous"
selected <- Filter(function(r) identical(r$cell, eligible), summary$results)
selected_failures <- Filter(function(r) identical(r$cell, eligible), summary$failures)
summary$schema_version <- "scalafim-gls-continuity-qualification-summary/v1"
summary$all_domain_scientific_pass <- summary$scientific_pass
summary$admission_cell <- eligible
summary$scientific_pass <- if (profile != "confirmation") NULL else
  length(selected) == 2L && !length(selected_failures) &&
  all(vapply(selected, function(r) isTRUE(r$scientific_pass), logical(1))) &&
  !is.null(parity) && isTRUE(parity$pass)
summary$admission_scope <- "Declared long-run homogeneous AR2 cell only; short-run results remain diagnostics."
jsonlite::write_json(summary, file.path(out,"summary.json"), auto_unbox=TRUE,
  digits=16L, pretty=TRUE, na="null", null="null")
cat("Restricted long-cell admission:", if(is.null(summary$scientific_pass)) "not-admitted" else summary$scientific_pass, "\n")
