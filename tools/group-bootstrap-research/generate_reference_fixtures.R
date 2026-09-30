#!/usr/bin/env Rscript
# Independent R reference for the group bootstrap research harness
# (Mote bd-01M21BNZR9ZBRAYY9JD5WCQ8KX, declaration v2 sections 2-7).
#
# Inputs are explicit pre-drawn variate matrices written by Scala
# (inputs/bootstrap-cases.json, from ReferenceInputWriter); no seed is shared.
# Every statistic is recomputed here with metafor::rma.uni(method = "PM",
# test = "adhoc", control = list(tol = 1e-10)), the restricted null fit with an
# explicit uniroot, the Smyth (2004) moment fit written out, HC3 with
# sandwich::vcovHC(type = "HC3"), the sign-flip orbit by enumeration, and the
# Clopper-Pearson bounds with qbeta. The output is a Scala fixture that the shared
# (JVM + JS) test suites assert against.
#
# Run from the repository root:
#   Rscript tools/group-bootstrap-research/generate_reference_fixtures.R

suppressPackageStartupMessages({
  library(metafor)
  library(sandwich)
  library(jsonlite)
  library(digest)
})
if (!file.exists("build.sbt")) stop("run from the ScalaFIM repository root", call. = FALSE)

in_path <- file.path("tools", "group-bootstrap-research", "inputs", "bootstrap-cases.json")
out_path <- file.path(
  "modules", "group", "shared", "src", "test", "scala", "scalafim", "group",
  "research", "bootstrap", "BootstrapReferenceFixtures.scala"
)
input_text <- readChar(in_path, file.info(in_path)$size, useBytes = TRUE)
input_sha <- digest(input_text, algo = "sha256", serialize = FALSE)
inputs <- fromJSON(input_text, simplifyVector = FALSE)

num <- function(x) as.numeric(unlist(x))
nu_values <- function(x) vapply(x, function(v) if (identical(v, "inf")) Inf else as.numeric(v), numeric(1))
tie_band <- 1e-7

# ---- metafor PM + mKH (test = "adhoc") -------------------------------------------------
pm_fit <- function(y, v, X, j, b0) {
  fit <- tryCatch(
    suppressWarnings(rma.uni(
      yi = y, vi = v, mods = X, intercept = FALSE, method = "PM", test = "adhoc",
      control = list(tol = 1e-10)
    )),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  est <- as.numeric(fit$beta[j])
  se <- as.numeric(fit$se[j])
  list(t = (est - b0) / se, tau2 = as.numeric(fit$tau2), beta = as.numeric(fit$beta))
}

# ---- restricted null fit by explicit uniroot (declaration section 2) --------------------
# H0: beta_j = b0 for a unit contrast e_j. The null-space design is X without column j,
# the offset is b0 * X[, j]; PM solves Q0(tau2) = n - p0, tau2 = 0 if Q0(0) <= n - p0.
restricted_fit <- function(y, v, X, j, b0) {
  n <- length(y)
  Z <- X[, -j, drop = FALSE]
  p0 <- ncol(Z)
  y0 <- y - b0 * X[, j]
  wls <- function(t2) {
    w <- 1 / (v + t2)
    if (p0 == 0) {
      r <- y0
    } else {
      g <- solve(crossprod(Z, w * Z), crossprod(Z, w * y0))
      r <- as.numeric(y0 - Z %*% g)
    }
    list(Q = sum(w * r^2), r = r)
  }
  target <- n - p0
  if (wls(0)$Q <= target) {
    t2 <- 0
  } else {
    hi <- 1
    while (wls(hi)$Q > target) hi <- 2 * hi
    t2 <- uniroot(function(t) wls(t)$Q - target, c(0, hi), tol = 1e-15, maxiter = 10000)$root
  }
  if (p0 > 0) {
    check <- suppressWarnings(rma.uni(yi = y0, vi = v, mods = Z, intercept = FALSE, method = "PM",
                                      control = list(tol = 1e-10)))
    stopifnot(abs(check$tau2 - t2) <= 1e-8 * max(1, t2))
  }
  list(tau2 = t2, mean = y - wls(t2)$r)
}

# ---- Smyth (2004) moment fit, written out (limma is not used) ---------------------------
smyth_fit <- function(v, nu) {
  if (any(is.infinite(nu))) return(list(d0 = NA_real_, s0 = NA_real_))
  n <- length(v)
  e <- log(v) - digamma(nu / 2) + log(nu / 2)
  emean <- mean(e)
  evar <- sum((e - emean)^2) / (n - 1) - mean(trigamma(nu / 2))
  if (evar > 0) {
    lx <- uniroot(function(l) trigamma(exp(l)) - evar, c(-40, 40), tol = 1e-15, maxiter = 10000)$root
    d0 <- 2 * exp(lx)
    s0 <- exp(emean + digamma(d0 / 2) - log(d0 / 2))
  } else {
    d0 <- Inf
    s0 <- exp(emean)
  }
  list(d0 = d0, s0 = s0)
}

# ---- HC3 via sandwich --------------------------------------------------------------------
hc3 <- function(y, X, w, j, b0) {
  fit <- lm(y ~ X - 1, weights = w)
  V <- sandwich::vcovHC(fit, type = "HC3")
  est <- as.numeric(coef(fit)[j])
  se <- sqrt(V[j, j])
  t <- (est - b0) / se
  c(est = est, se = se, t = t, p = 2 * pt(-abs(t), df = nrow(X) - ncol(X)))
}

# ---- one bootstrap scheme on explicit variates ------------------------------------------
schemes <- c("B-plug", "B-fixV", "B-EB", "C-uncentred", "C-omit-u")

run_scheme <- function(cs, scheme, eb) {
  n <- cs$n
  B <- cs$draws
  obs <- pm_fit(cs$y, cs$v, cs$X, cs$j, cs$b0)
  stopifnot(!is.null(obs))
  if (scheme == "C-uncentred") {
    mean0 <- as.numeric(cs$X %*% obs$beta)
    t2w <- obs$tau2
  } else {
    rf <- restricted_fit(cs$y, cs$v, cs$X, cs$j, cs$b0)
    mean0 <- rf$mean
    t2w <- rf$tau2
  }
  tau_u <- if (scheme == "C-omit-u") 0 else sqrt(t2w)
  stilde <- if (scheme == "B-EB" && is.finite(eb$d0)) (eb$d0 * eb$s0 + cs$nu * cs$v) / (eb$d0 + cs$nu) else NULL
  stats <- rep(NaN, B)
  for (b in seq_len(B)) {
    idx <- ((b - 1) * n + 1):(b * n)
    sigma2 <- if (scheme == "B-EB") {
      if (is.na(eb$d0)) cs$v else if (is.infinite(eb$d0)) rep(eb$s0, n) else (eb$d0 + cs$nu) * stilde / cs$post[idx]
    } else cs$v
    ystar <- mean0 + tau_u * cs$zu[idx] + sqrt(sigma2) * cs$ze[idx]
    vstar <- if (scheme == "B-fixV") cs$v else sigma2 * cs$chi[idx]
    f <- pm_fit(ystar, vstar, cs$X, cs$j, cs$b0)
    if (!is.null(f) && is.finite(f$t)) stats[b] <- f$t
  }
  ok <- !is.nan(stats)
  list(
    scheme = scheme, stats = stats,
    exceed = sum(abs(stats[ok]) >= abs(obs$t)),
    failed = sum(!ok),
    ties = sum(abs(abs(stats[ok]) - abs(obs$t)) <= tie_band * abs(obs$t))
  )
}

cases <- lapply(inputs$cases, function(raw) {
  n <- raw$n
  p <- raw$p
  X <- matrix(num(raw$design_row_major), nrow = n, ncol = p, byrow = TRUE)
  contrast <- num(raw$contrast)
  j <- which(contrast != 0)
  stopifnot(length(j) == 1, contrast[j] == 1)
  cs <- list(
    id = raw$id, n = n, p = p, terms = unlist(raw$terms), X = X, design = num(raw$design_row_major),
    contrast = contrast, j = j, b0 = as.numeric(raw$b0), y = num(raw$y), v = num(raw$v),
    nu = nu_values(raw$nu), draws = raw$draws, zu = num(raw$zu), ze = num(raw$ze), chi = num(raw$chi),
    post = if (length(raw$post) == 0) rep(NaN, n * raw$draws) else num(raw$post)
  )
  eb <- smyth_fit(cs$v, cs$nu)
  obs <- pm_fit(cs$y, cs$v, X, j, cs$b0)
  rf <- restricted_fit(cs$y, cs$v, X, j, cs$b0)
  cs$observed <- obs$t
  cs$tau2 <- obs$tau2
  cs$restricted_tau2 <- rf$tau2
  cs$restricted_mean <- rf$mean
  cs$eb <- eb
  cs$expected <- lapply(schemes, function(s) run_scheme(cs, s, eb))
  cs$hc3_equal <- hc3(cs$y, X, rep(1, n), j, cs$b0)
  cs$hc3_inverse <- hc3(cs$y, X, 1 / cs$v, j, cs$b0)
  cs
})

# ---- sign-flip orbit (declaration section 4, section 7.3) -------------------------------
# Dyadic values, so that y_1 - b0 = 0 and |y_2 - b0| = |y_3 - b0| hold exactly in binary.
sf_y <- c(0.25, 0.75, -0.25, 1.50, 1.00, -0.375, 1.125, 1.75)
sf_v <- c(0.05, 0.11, 0.20, 0.08, 0.30, 0.15, 0.50, 0.09)
sf_b0 <- 0.25
flip <- function(g) {
  s <- ifelse(bitwAnd(g, bitwShiftL(1L, 0:7)) != 0, -1, 1)
  sf_b0 + s * (sf_y - sf_b0)
}
sign_flip_case <- function(id, failing) {
  failing_data <- lapply(failing, flip)
  stats <- vapply(0:255, function(g) {
    d <- flip(g)
    if (any(vapply(failing_data, function(f) identical(f, d), logical(1)))) return(0)
    f <- pm_fit(d, sf_v, matrix(1, 8, 1), 1, sf_b0)
    if (is.null(f) || !is.finite(f$t)) 0 else abs(f$t)
  }, numeric(1))
  # ">=" with the harness's relative tie tolerance 1e-10 (mathematically equal statistics
  # can differ in the last bits); this only raises p, so level <= alpha is kept.
  pvals <- vapply(seq_along(stats), function(g) mean(stats >= stats[g] - 1e-10 * abs(stats[g])), numeric(1))
  list(id = id, failing = failing, stats = stats, p = pvals, rejections = sum(pvals <= 0.05))
}
sign_flips <- list(sign_flip_case("literal-n8", integer(0)), sign_flip_case("literal-n8-failures", c(5L, 77L, 200L, 255L)))
for (sf in sign_flips) stopifnot(sf$rejections <= floor(0.05 * 256))

# ---- Clopper-Pearson (declaration section 6) ---------------------------------------------
R <- 20000
delta <- 6.25e-6
cp_upper <- function(k) if (k == R) 1 else qbeta(1 - delta, k + 1, R - k)
cp_lower <- function(k) if (k == 0) 0 else qbeta(delta, k, R - k + 1)
null_pass_max <- max(which(vapply(0:3000, cp_upper, numeric(1)) <= 0.065)) - 1
null_fail_min <- min(which(vapply(0:3000, cp_lower, numeric(1)) > 0.065)) - 1
failure_pass_max <- max(which(vapply(0:500, cp_upper, numeric(1)) <= 0.005)) - 1
stopifnot(null_pass_max == 1149, null_fail_min == 1456, failure_pass_max == 59)
cp_ks <- c(0, 1, 20, 59, 60, 1000, 1149, 1150, 1455, 1456, 19999, 20000)

# ---- Scala emission ---------------------------------------------------------------------
fmt <- function(x) {
  s <- formatC(x, digits = 17, format = "g")
  s[is.nan(x)] <- "NaN"
  s[is.infinite(x) & x > 0] <- "Inf"
  s[is.infinite(x) & x < 0] <- "-Inf"
  gsub(" ", "", s)
}
vec <- function(x) sprintf("parse(\"%s\")", paste(fmt(x), collapse = " "))
strs <- function(x) sprintf("Vector(%s)", paste(sprintf("\"%s\"", x), collapse = ", "))
d1 <- function(x) {
  if (is.na(x)) "Double.NaN" else if (is.infinite(x)) "Double.PositiveInfinity" else fmt(x)
}

case_scala <- function(cs) {
  expected <- vapply(cs$expected, function(e) {
    sprintf("        SchemeExpected(\"%s\", %s, %d, %d, %d)", e$scheme, vec(e$stats), e$exceed, e$failed, e$ties)
  }, character(1))
  hc <- function(h) sprintf("BaselineExpected(%s, %s, %s, %s)", fmt(h[["est"]]), fmt(h[["se"]]), fmt(h[["t"]]), fmt(h[["p"]]))
  paste0(
    "    Case(\n",
    sprintf("      id = \"%s\", n = %d, p = %d, terms = %s,\n", cs$id, cs$n, cs$p, strs(cs$terms)),
    sprintf("      designRowMajor = %s,\n", vec(cs$design)),
    sprintf("      contrast = %s, b0 = %s,\n", vec(cs$contrast), fmt(cs$b0)),
    sprintf("      y = %s,\n", vec(cs$y)),
    sprintf("      v = %s,\n", vec(cs$v)),
    sprintf("      nu = %s, draws = %d,\n", vec(cs$nu), cs$draws),
    sprintf("      zu = %s,\n", vec(cs$zu)),
    sprintf("      ze = %s,\n", vec(cs$ze)),
    sprintf("      chi = %s,\n", vec(cs$chi)),
    sprintf("      post = %s,\n", vec(cs$post)),
    sprintf("      observedStatistic = %s, tau2Hat = %s,\n", fmt(cs$observed), fmt(cs$tau2)),
    sprintf("      restrictedTau2 = %s, restrictedMean = %s,\n", fmt(cs$restricted_tau2), vec(cs$restricted_mean)),
    sprintf("      d0 = %s, s0Squared = %s,\n", d1(cs$eb$d0), d1(cs$eb$s0)),
    sprintf("      hc3Equal = %s,\n", hc(cs$hc3_equal)),
    sprintf("      hc3InverseVariance = %s,\n", hc(cs$hc3_inverse)),
    "      expected = Vector(\n", paste(expected, collapse = ",\n"), "\n      )\n    )"
  )
}

sf_scala <- function(sf) {
  sprintf(
    "    SignFlipCase(\"%s\", Vector(%s), %s, %d)",
    sf$id, paste(sf$failing, collapse = ", "), vec(sf$stats), sf$rejections
  )
}

cp_scala <- vapply(cp_ks, function(k) {
  sprintf("    ClopperPearsonCase(%d, %d, %s, %s, %s)", as.integer(k), as.integer(R), fmt(delta), fmt(cp_upper(k)), fmt(cp_lower(k)))
}, character(1))

lines <- c(
  "// Generated by tools/group-bootstrap-research/generate_reference_fixtures.R. Do not edit by hand.",
  "package scalafim.group.research.bootstrap",
  "",
  "object BootstrapReferenceFixtures:",
  sprintf("  val Oracle = \"R %s; metafor %s; sandwich %s\"", getRversion(), packageVersion("metafor"), packageVersion("sandwich")),
  sprintf("  val InputSha256 = \"%s\"", input_sha),
  "  val InputPath = \"tools/group-bootstrap-research/inputs/bootstrap-cases.json\"",
  sprintf("  val TieBand = %s", fmt(tie_band)),
  sprintf("  val NullPassMax = %d", as.integer(null_pass_max)),
  sprintf("  val NullFailMin = %d", as.integer(null_fail_min)),
  sprintf("  val FailurePassMax = %d", as.integer(failure_pass_max)),
  sprintf("  val ChiSquare49At999 = %s", fmt(qchisq(0.999, 49))),
  sprintf("  val SignFlipY = %s", vec(sf_y)),
  sprintf("  val SignFlipV = %s", vec(sf_v)),
  sprintf("  val SignFlipB0 = %s", fmt(sf_b0)),
  "",
  "  def parse(text: String): Array[Double] =",
  "    if text.isEmpty then Array.emptyDoubleArray",
  "    else",
  "      text.split(' ').map {",
  "        case \"NaN\" => Double.NaN",
  "        case \"Inf\" => Double.PositiveInfinity",
  "        case \"-Inf\" => Double.NegativeInfinity",
  "        case token => token.toDouble",
  "      }",
  "",
  "  final case class SchemeExpected(scheme: String, statistics: Array[Double], exceed: Int, failed: Int, nearTies: Int)",
  "  final case class BaselineExpected(estimate: Double, standardError: Double, statistic: Double, pValue: Double)",
  "  final case class Case(",
  "      id: String, n: Int, p: Int, terms: Vector[String], designRowMajor: Array[Double],",
  "      contrast: Array[Double], b0: Double, y: Array[Double], v: Array[Double], nu: Array[Double], draws: Int,",
  "      zu: Array[Double], ze: Array[Double], chi: Array[Double], post: Array[Double],",
  "      observedStatistic: Double, tau2Hat: Double, restrictedTau2: Double, restrictedMean: Array[Double],",
  "      d0: Double, s0Squared: Double, hc3Equal: BaselineExpected, hc3InverseVariance: BaselineExpected,",
  "      expected: Vector[SchemeExpected]",
  "  )",
  "  final case class SignFlipCase(id: String, failingPatterns: Vector[Int], statistics: Array[Double], rejections: Int)",
  "  final case class ClopperPearsonCase(k: Int, r: Int, delta: Double, upper: Double, lower: Double)",
  "",
  "  val Cases: Vector[Case] = Vector(",
  paste(vapply(cases, case_scala, character(1)), collapse = ",\n"),
  "  )",
  "",
  "  val SignFlipCases: Vector[SignFlipCase] = Vector(",
  paste(vapply(sign_flips, sf_scala, character(1)), collapse = ",\n"),
  "  )",
  "",
  "  val ClopperPearsonCases: Vector[ClopperPearsonCase] = Vector(",
  paste(cp_scala, collapse = ",\n"),
  "  )"
)
writeLines(lines, out_path)
cat(sprintf("wrote %s (inputs sha256 %s)\n", out_path, input_sha))
for (cs in cases) {
  cat(sprintf("%s: T=%.10f tau2=%.6g restricted tau2=%.6g d0=%s\n", cs$id, cs$observed, cs$tau2, cs$restricted_tau2, format(cs$eb$d0)))
  for (e in cs$expected) cat(sprintf("  %-12s k=%d f=%d ties=%d p=[%.4f, %.4f]\n", e$scheme, e$exceed, e$failed, e$ties,
                                     (1 + e$exceed) / (cs$draws + 1), (1 + e$exceed + e$failed) / (cs$draws + 1)))
}
for (sf in sign_flips) cat(sprintf("%s: rejections %d of 256 (bound 12)\n", sf$id, sf$rejections))
