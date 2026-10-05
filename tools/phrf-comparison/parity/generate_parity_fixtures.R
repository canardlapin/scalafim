#!/usr/bin/env Rscript
# S11 (PHRF comparative-evaluation pilot): comparator parity fixtures.
#
# Fits CAN, INF3 and FIR with fmrireg, LSA and LSS with fmrilss, and rLSS as an
# explicit dense penalized solve, on small deterministic designs under ONE FIXED
# global AR(1) coefficient. Writes plain CSV/JSON (17 significant digits) plus a
# sha256 manifest. Every fixture is cross-checked inside R through an independent
# path (own exact-kernel design, hand whitening, lm.fit); the script stops if any
# cross-check misses its tolerance.
#
# Usage (from anywhere):
#   Rscript tools/phrf-comparison/parity/generate_parity_fixtures.R [out_dir]
# Environment: FMR_CHECKOUT (see below).
#
# Semantics are documented in docs/verification/phrf-cmp-s11-20261001.md.

suppressMessages({
  library(fmrihrf)
  library(fmridesign)
})

script_dir <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
})
args <- commandArgs(TRUE)
out_dir <- if (length(args) >= 1) args[1] else file.path(script_dir, "fixtures")

# FMR_CHECKOUT=fmrihrf,fmridesign,fmrireg,fmrilss loads those packages from their
# ~/code checkouts with pkgload instead of the installed builds (used only to
# compare the two; the committed fixtures come from the installed builds).
checkout_pkgs <- setdiff(strsplit(Sys.getenv("FMR_CHECKOUT", ""), ",")[[1]], "")
fmrireg_source <- if (length(checkout_pkgs)) paste("checkout:", paste(checkout_pkgs, collapse = ",")) else "installed"
for (p in checkout_pkgs) suppressMessages(pkgload::load_all(path.expand(file.path("~/code", p)), quiet = TRUE))
if (!"fmrireg" %in% checkout_pkgs) suppressMessages(library(fmrireg))

# ---------------------------------------------------------------- constants ---
# Seeds: low 31 bits of stream_seed(HARNESS_ROOT = 0x7A3C91D50B44E2F1, cell, 0,
# purpose) from tools/phrf-comparison/generator/phrf_gen/seeds.py (SplitMix64).
# Computed once with that module; none is denylisted.
SEEDS <- list(
  cond  = c(design = 862739264L,  truth = 909906548L,  noise = 527002624L),
  trial = c(design = 611041696L,  truth = 1038989846L, noise = 1502733079L),
  cond_close = c(design = 1422781378L, truth = 413255651L, noise = 540723840L),
  trial_ts   = c(design = 295011566L,  truth = 995059571L, noise = 57289396L)
)
TR <- 1
START_TIME <- 0         # fmrireg default is TR/2; fixtures use 0 (scan k at k*TR)
PRECISION <- 0.1        # internal convolution grid; onsets (0.1 s grid, as the generator) and scans lie on it
FIXED_PHI <- 0.3125     # 5/16, exact in binary; the GIVEN whitening coefficient
NOISE_PHI <- 0.4        # data-generating AR(1) (deliberately != FIXED_PHI)
H <- 32L                # FIR window and bins (1 s bins), protocol 4
TAU_STEP <- 0.25
TAU <- seq(0, H, by = TAU_STEP)   # E-resp reconstruction grid (inclusive of H)

TOL_FIT <- 1e-10        # in-R cross-check tolerance (the 1e-8 gate is Scala vs fixture)
TOL_RLSS <- 1e-10

# ------------------------------------------------------------------ helpers ---
fmt <- function(x) sprintf("%.17g", x)
write_mat <- function(m, path, row_names = NULL, row_label = "row") {
  m <- as.matrix(m)
  cells <- matrix(fmt(m), nrow(m), ncol(m))
  if (!is.null(row_names)) {
    cells <- cbind(row_names, cells)
    colnames(cells) <- c(row_label, colnames(m))
  } else {
    colnames(cells) <- colnames(m)
  }
  write.table(cells, path, sep = ",", quote = FALSE, row.names = FALSE,
              col.names = TRUE)
}
write_df <- function(df, path) {
  write.table(df, path, sep = ",", quote = FALSE, row.names = FALSE, na = "")
}
relerr <- function(a, b) max(abs(a - b)) / max(max(abs(b)), .Machine$double.eps)
checks <- list()
check <- function(name, value, tol) {
  checks[[length(checks) + 1L]] <<- list(name = name, value = value, tol = tol,
                                         pass = is.finite(value) && value <= tol)
  cat(sprintf("  [%s] %-58s %.3e (tol %.0e)\n",
              if (value <= tol) "ok" else "FAIL", name, value, tol))
  if (!(value <= tol)) stop("cross-check failed: ", name, call. = FALSE)
}

# AR(1) whitening, per run (design v2.1 2.0.4: exactFirstAr1 = true is PRIMARY):
#   z_1 = sqrt(1 - phi^2) x_1 (exact_first) or x_1 (identity-first variant);
#   z_t = x_t - phi x_{t-1}  (t >= 2), restarted at every run.
whiten <- function(M, phi, run_len, exact_first = TRUE) {
  M <- as.matrix(M); out <- M; start <- 0L
  for (n in run_len) {
    i <- start + seq_len(n); m <- M[i, , drop = FALSE]; o <- m
    o[-1, ] <- m[-1, , drop = FALSE] - phi * m[-n, , drop = FALSE]
    if (exact_first) o[1, ] <- sqrt(1 - phi^2) * m[1, ]
    out[i, ] <- o; start <- start + n
  }
  out
}

# Run a fmrireg fit with the shared AR coefficient FIXED. fmri_lm() has no
# public argument for this (see receipt), so the internal estimator that feeds
# the pooled global AR(1) is replaced by a constant for the duration of the call.
with_fixed_phi <- function(phi, expr) {
  ns <- asNamespace("fmrireg"); nm <- ".estimate_shared_ar_parameters"
  orig <- get(nm, ns); unlockBinding(nm, ns)
  assign(nm, function(...) phi, envir = ns)
  on.exit({ assign(nm, orig, envir = ns); lockBinding(nm, ns) }, add = TRUE)
  force(expr)
}

make_frame <- function(Y, run_len, events) {
  obs <- fmrireg:::.frame_observations(run_len, TR)
  obs$start_time <- START_TIME
  fmridataset::fmri_frame(
    assays = list(signal = unname(Y)), observations = obs,
    space = fmridataset::index_space(ncol(Y), ids = colnames(Y)),
    tables = fmrireg:::.frame_tables(events))
}

fit_fmrireg_fixed_phi <- function(basis, events, frame, phi, exact_first = TRUE) {
  sf <- fmridataset::as_sampling_frame(frame)
  em <- event_model(onset ~ hrf(cond, basis = basis), data = events, block = ~run,
                    sampling_frame = sf, precision = PRECISION)
  bm <- baseline_model(basis = "poly", degree = 2, sframe = sf, intercept = "runwise")
  fm <- fmri_model(em, bm, dataset = frame)
  ctl <- fmri_lm_control(
    estimation = estimation_spec("joint"),
    noise = noise_spec("ar1", pooling = "global", exact_first = exact_first))
  fit <- suppressWarnings(with_fixed_phi(phi, fmri_lm(fm, control = ctl)))
  list(fit = fit, X = as.matrix(design_matrix(fm)),
       n_event = ncol(as.matrix(design_matrix(em))),
       beta = `colnames<-`(t(as.matrix(coef(fit))), fmridataset::feature_ids(frame)))   # rows = event coefficients, cols = voxels
}

# --- independent kernels (hand formulas; not fmrihrf code) -------------------
own_gam_pdf <- function(x, k, theta) ifelse(x > 0, x^(k - 1) * exp(-x / theta) /
                                              (gamma(k) * theta^k), 0)
own_spmg1 <- function(t) {
  ifelse(t < 0, 0, exp(-t) * (t^5 / 120 - t^15 / (6 * 1307674368000)))
}
own_spmg1_d <- function(t) {   # analytic d/dt of own_spmg1
  C <- 1 / (6 * 1307674368000)
  ifelse(t < 0, 0, exp(-t) * (5 * t^4 / 120 - 15 * C * t^14 - t^5 / 120 + C * t^15))
}
own_spmg1_disp <- function(t) { # SPM-sign dispersion difference, mass A1*Gamma(6)=1
  1 * (own_gam_pdf(t, 6, 1) - own_gam_pdf(t, 6 / 1.01, 1.01)) / 0.01
}
own_kernel <- function(name, t) {
  switch(name,
    can  = cbind(own_spmg1(t)),
    inf3 = cbind(own_spmg1(t), own_spmg1_d(t), own_spmg1_disp(t)),
    fir  = outer(t, seq_len(H), function(x, k) as.numeric(x >= k - 1 & x < k)))
}
SPAN <- c(can = 24, inf3 = 24, fir = H)

# Exact design for event columns (own convolution at exact lags), condition-major
# then basis (fmrireg column order), kernel support lag in [0, span] inclusive for
# SPMG (fmrihrf span convention) and [0, H) for FIR.
own_event_design <- function(events, run_len, name) {
  conds <- levels(events$cond); nb <- ncol(own_kernel(name, 0))
  n <- sum(run_len); X <- matrix(0, n, length(conds) * nb)
  colnames(X) <- as.vector(t(outer(conds, seq_len(nb), paste, sep = "_b")))
  start <- 0L
  for (r in seq_along(run_len)) {
    tt <- START_TIME + (seq_len(run_len[r]) - 1L) * TR
    for (ci in seq_along(conds)) {
      on <- events$onset[events$run == r & events$cond == conds[ci]]
      for (o in on) {
        lag <- tt - o; ok <- lag >= 0 & lag <= SPAN[[name]]
        if (name == "fir") ok <- lag >= 0 & lag < H
        K <- own_kernel(name, lag[ok])
        for (b in seq_len(nb)) X[start + which(ok), (ci - 1L) * nb + b] <-
          X[start + which(ok), (ci - 1L) * nb + b] + K[, b]
      }
    }
    start <- start + run_len[r]
  }
  X
}
own_baseline <- function(run_len) {  # per-run [1, t, t^2]; same span as fmrireg poly2 + run constant
  n <- sum(run_len); B <- matrix(0, n, 3 * length(run_len)); start <- 0L
  for (r in seq_along(run_len)) {
    t <- seq_len(run_len[r]); u <- (t - mean(t)) / run_len[r]
    B[start + t, 3 * (r - 1) + 1:3] <- cbind(1, u, u^2); start <- start + run_len[r]
  }
  B
}

# Draw sorted onsets on a grid with a minimum spacing, deterministic given seed.
draw_onsets <- function(n, lo, hi, step, min_gap) {
  # integer-tenths (or integer) construction so values are the nearest doubles to
  # their decimal; step must be 1/k for integer k
  k <- round(1 / step); grid <- seq(round(lo * k), round(hi * k)); on <- numeric(0)
  while (length(on) < n) {
    c1 <- sample(grid, 1) / k
    if (all(abs(on - c1) >= min_gap - 1e-9)) on <- c(on, c1)
  }
  sort(on)
}
ar1_noise <- function(n, phi, sd) {
  as.numeric(arima.sim(list(ar = phi), n = n, sd = sd, n.start = 100))
}


VARIANTS <- list(primary = TRUE, identity_first = FALSE)  # exact_first flag
vdir <- function(base, v) { d <- if (v == "primary") base else file.path(base, v)
  dir.create(file.path(out_dir, d), recursive = TRUE, showWarnings = FALSE); d }
P <- function(...) file.path(out_dir, ...)

# ============================================================ CONDITION CELLS ==
arms <- list(
  can  = list(label = "CAN",  basis = "spmg1"),
  inf3 = list(label = "INF3", basis = "spmg3"),
  fir  = list(label = "FIR",  basis = getHRF("fir", nbasis = H, span = H))
)
HRF_OBJ <- list(can = HRF_SPMG1, inf3 = HRF_SPMG3, fir = getHRF("fir", nbasis = H, span = H))
cat("== kernel cross-check\n")
tk <- seq(-1, 40, by = 0.125)
check("kernel SPMG1 own vs fmrihrf", relerr(own_kernel("can", tk), cbind(HRF_SPMG1(tk))), 1e-13)
check("kernel SPMG3 own vs fmrihrf", relerr(own_kernel("inf3", tk[tk <= 24]),
                                            HRF_SPMG3(tk[tk <= 24])), 1e-9)
check("kernel FIR own vs fmrihrf (H bins)", relerr(own_kernel("fir", TAU[TAU < H]),
                                                   evaluate(HRF_OBJ$fir, TAU[TAU < H])), 0)
SPMG1_PEAK <- max(HRF_SPMG1(seq(0, 32, by = 0.001)))

# cell "cond": isolated events, 0.1 s onsets, min gap 3 s.
# cell "cond_close": 10 base onsets per run (gap >= 8 s) each with a partner 0.1-0.5 s later
# (overlapping events, shared FIR bins), conditions assigned independently.
make_events <- function(cell) {
  rl <- rep(200L, 4)
  ev <- do.call(rbind, lapply(seq_along(rl), function(r) {
    if (cell == "cond") {
      on <- draw_onsets(20, 4, 150, 0.1, 3)
    } else {
      base <- draw_onsets(10, 4, 140, 0.1, 8)
      on <- sort(c(base, round((base + sample(c(0.1, 0.2, 0.3, 0.5), 10, TRUE)) * 10) / 10))
    }
    cc <- if (cell == "cond") sample(rep(c("A", "B"), 10)) else sample(c("A", "B"), 20, TRUE)
    data.frame(onset = on, cond = factor(cc, levels = c("A", "B")), run = r)
  }))
  ev$event_index <- seq_len(nrow(ev)); ev
}

run_cond_cell <- function(cell) {
  cat(sprintf("== condition cell '%s' (CAN, INF3, FIR vs fmrireg)\n", cell))
  sd <- SEEDS[[if (cell == "cond") "cond" else "cond_close"]]
  rl <- rep(200L, 4); nv <- 3L
  set.seed(sd[["design"]]); ev <- make_events(cell)
  set.seed(sd[["truth"]])
  sf_tmp <- sampling_frame(blocklens = rl, TR = TR, start_time = START_TIME)
  em_truth <- event_model(onset ~ hrf(cond, basis = "spmg3"), data = ev, block = ~run,
                          sampling_frame = sf_tmp, precision = PRECISION)
  X3 <- as.matrix(design_matrix(em_truth))
  truth_coef <- rbind(c(6, 4, 8), c(0, 1, 0), c(0, 0, 0.5), c(3, 5, 4), c(1.5, 0, 0.5), c(0, 0.5, 0))
  set.seed(sd[["noise"]])
  noise <- sapply(seq_len(nv), function(v) unlist(lapply(rl, function(n) ar1_noise(n, NOISE_PHI, 0.5))))
  Y <- X3 %*% truth_coef + noise; colnames(Y) <- paste0("v", seq_len(nv))
  frame <- make_frame(Y, rl, ev)
  d0 <- vdir(cell, "primary")
  write_df(ev[, c("event_index", "run", "onset", "cond")], P(d0, "events.csv"))
  write_mat(Y, P(d0, "Y.csv"))
  info <- list(run_len = rl, n_voxels = nv, events = nrow(ev))
  for (v in names(VARIANTS)) {
    ef <- VARIANTS[[v]]; d <- vdir(cell, v); tag <- sprintf("%s/%s", cell, v)
    Yw <- whiten(Y, FIXED_PHI, rl, ef)
    write_mat(Yw, P(d, "Y_white.csv"))
    for (nm in names(arms)) {
      r <- fit_fmrireg_fixed_phi(arms[[nm]]$basis, ev, frame, FIXED_PHI, exact_first = ef)
      if (abs(ar_parameters(r$fit)[[1]] - FIXED_PHI) > 0) stop("phi was not applied")
      X <- r$X; ne <- r$n_event; stopifnot(qr(X)$rank == ncol(X))
      Xo <- own_event_design(ev, rl, nm)
      if (v == "primary") check(sprintf("%s %s design fmrireg vs exact own (abs)", tag, nm),
                                max(abs(X[, seq_len(ne)] - Xo)), 1e-12)
      Xown <- cbind(Xo, own_baseline(rl))
      b_own <- lm.fit(whiten(Xown, FIXED_PHI, rl, ef), Yw)$coefficients[seq_len(ne), , drop = FALSE]
      check(sprintf("%s %s betas fmrireg vs lm.fit (own design, own baseline)", tag, nm),
            relerr(b_own, r$beta), TOL_FIT)
      b_own2 <- lm.fit(whiten(X, FIXED_PHI, rl, ef), Yw)$coefficients[seq_len(ne), , drop = FALSE]
      check(sprintf("%s %s betas fmrireg vs lm.fit (whitened fmrireg design)", tag, nm),
            relerr(b_own2, r$beta), TOL_FIT)
      fh <- suppressWarnings(fitted_hrf(r$fit, sample_at = TAU))$cond
      conds <- levels(ev$cond); nb <- ncol(own_kernel(nm, 0)); K <- own_kernel(nm, TAU)
      E_own <- do.call(cbind, lapply(seq_along(conds), function(ci)
        K %*% r$beta[(ci - 1) * nb + seq_len(nb), , drop = FALSE]))
      E_fm <- do.call(cbind, lapply(conds, function(cc) fh$pred[fh$design$cond == cc, , drop = FALSE]))
      check(sprintf("%s %s E-resp fitted_hrf vs own basis sum", tag, nm), relerr(E_own, E_fm), TOL_FIT)
      colnames(E_own) <- as.vector(sapply(conds, function(cc) paste0(cc, ".", colnames(Y))))
      write_mat(r$beta, P(d, sprintf("beta_%s.csv", nm)),
                row_names = colnames(X)[seq_len(ne)], row_label = "coef")
      write_mat(E_own, P(d, sprintf("eresp_%s.csv", nm)), row_names = fmt(TAU), row_label = "tau")
      if (v == "primary") {
        write_mat(X, P(d, sprintf("X_%s.csv", nm)))
        kk <- own_kernel(nm, TAU); colnames(kk) <- paste0("b", seq_len(ncol(kk)))
        write_mat(kk, P(d, sprintf("kernel_%s.csv", nm)), row_names = fmt(TAU), row_label = "tau")
      }
    }
  }
  info
}
cond_info <- list(cond = run_cond_cell("cond"), cond_close = run_cond_cell("cond_close"))

# ================================================================ TRIAL CELLS ==
# tx: TR-aligned onsets (T-TX). ts: 0.1 s onsets (T-TS).
run_trial_cell <- function(cell) {
  cat(sprintf("== trial cell '%s' (LSA, LSS vs fmrilss; rLSS vs dense solve)\n", cell))
  sd <- SEEDS[[if (cell == "tx") "trial" else "trial_ts"]]
  step <- if (cell == "tx") 1 else 0.1
  rl <- rep(150L, 3); n <- sum(rl); nv <- 4L; per_run <- 12L
  set.seed(sd[["design"]])
  trials <- do.call(rbind, lapply(seq_along(rl), function(r) {
    on <- draw_onsets(per_run, 4, 130, step, if (cell == "tx") 6 else 4)
    data.frame(onset = on, cond = factor(sample(rep(c("A", "B", "C"), per_run / 3)),
                                         levels = c("A", "B", "C")), run = r)
  }))
  trials$trial_index <- seq_len(nrow(trials)); T <- nrow(trials)
  sf_t <- sampling_frame(blocklens = rl, TR = TR, start_time = START_TIME)
  em_t <- event_model(onset ~ trialwise(basis = "spmg1"), data = trials, block = ~run,
                      sampling_frame = sf_t, precision = PRECISION)
  Xt <- as.matrix(design_matrix(em_t)); stopifnot(ncol(Xt) == T)
  Ft <- as.matrix(design_matrix(baseline_model(basis = "poly", degree = 2, sframe = sf_t, intercept = "runwise")))
  Xt_own <- sapply(seq_len(T), function(i) {
    tt <- START_TIME + (seq_len(rl[trials$run[i]]) - 1L) * TR
    col <- numeric(n); st <- sum(rl[seq_len(trials$run[i] - 1L)])
    lag <- tt - trials$onset[i]; ok <- lag >= 0 & lag <= 24
    col[st + which(ok)] <- own_spmg1(lag[ok]); col })
  check(sprintf("%s trial design fmrireg trialwise vs exact own (abs)", cell), max(abs(Xt - Xt_own)), 1e-12)
  colnames(Xt) <- sprintf("trial_%02d", seq_len(T)); colnames(Ft) <- paste0("F", seq_len(ncol(Ft)))
  set.seed(sd[["truth"]])
  cond_mean <- rbind(A = c(5, 3, 4, 6), B = c(2, 4, 1, 3), C = c(0, 1, 0, 2))
  amp <- cond_mean[as.character(trials$cond), ] + matrix(rnorm(T * nv, 0, 1.0), T, nv)
  set.seed(sd[["noise"]])
  noise <- sapply(seq_len(nv), function(v) unlist(lapply(rl, function(m) ar1_noise(m, NOISE_PHI, 0.5))))
  Y <- Xt %*% amp + noise; colnames(Y) <- paste0("v", seq_len(nv))
  d0 <- vdir(file.path("trial", cell), "primary")
  write_df(trials[, c("trial_index", "run", "onset", "cond")], P(d0, "trials.csv"))
  write_mat(Y, P(d0, "Y.csv")); write_mat(Xt, P(d0, "X_trial.csv")); write_mat(Ft, P(d0, "F.csv"))
  tid <- sprintf("trial_%02d", seq_len(T)); groups_cond <- as.integer(trials$cond)
  info <- list(run_len = rl, n_voxels = nv, trials = T)
  for (v in names(VARIANTS)) {
    ef <- VARIANTS[[v]]; d <- vdir(file.path("trial", cell), v); tag <- sprintf("%s/%s", cell, v)
    Yw <- whiten(Y, FIXED_PHI, rl, ef); Xw <- whiten(Xt, FIXED_PHI, rl, ef); Fw <- whiten(Ft, FIXED_PHI, rl, ef)
    b_lsa <- fmrilss::lsa(Yw, Xw, Z = Fw)
    # LSA independence check uses a normal-equations solve (not lm.fit, which fmrilss uses)
    A <- cbind(Xw, Fw); b_lsa_own <- solve(crossprod(A), crossprod(A, Yw))[seq_len(T), , drop = FALSE]
    check(sprintf("%s LSA fmrilss::lsa vs normal-equations solve", tag), relerr(unname(b_lsa), unname(b_lsa_own)), TOL_FIT)
    b_lss <- suppressWarnings(fmrilss::lss(Yw, Xw, Z = Fw, method = "r_optimized"))
    b_lss_naive <- suppressWarnings(fmrilss::lss(Yw, Xw, Z = Fw, method = "naive"))
    b_lss_own <- t(sapply(seq_len(T), function(i) {
      A <- cbind(Xw[, i], rowSums(Xw[, -i, drop = FALSE]), Fw)
      solve(crossprod(A), crossprod(A, Yw))[1, ] }))
    check(sprintf("%s LSS fmrilss r_optimized vs explicit loop", tag), relerr(unname(b_lss), unname(b_lss_own)), TOL_FIT)
    check(sprintf("%s LSS fmrilss naive vs explicit loop", tag), relerr(unname(b_lss_naive), unname(b_lss_own)), TOL_FIT)
    gsum <- function(groups) sapply(sort(unique(groups)), function(g) rowSums(Xw[, groups == g, drop = FALSE]))
    rlss_dense <- function(i, r, groups) {
      G <- sort(unique(groups)); D <- cbind(gsum(groups), Xw[, i], Fw)
      p <- c(rep(0, length(G)), r, rep(0, ncol(Fw)))
      beta <- solve(crossprod(D) + diag(p), crossprod(D, Yw)); gi <- match(groups[i], G)
      list(amp = beta[gi, ] + beta[length(G) + 1L, ], delta = beta[length(G) + 1L, ])
    }
    rlss_closed <- function(i, r, groups) {
      G <- sort(unique(groups)); Z <- cbind(gsum(groups), Fw); ZtZi <- solve(crossprod(Z))
      xt <- Xw[, i] - Z %*% (ZtZi %*% crossprod(Z, Xw[, i]))
      q <- sum(xt^2); delta <- as.numeric(crossprod(xt, Yw)) / (q + r)
      theta <- ZtZi %*% crossprod(Z, Yw - Xw[, i] %o% delta)
      list(amp = theta[match(groups[i], G), ] + delta, q = q)
    }
    Zc <- cbind(gsum(groups_cond), Fw)
    q_i <- colSums((Xw - Zc %*% solve(crossprod(Zc), crossprod(Zc, Xw)))^2); mean_q <- mean(q_i)
    r_mult <- c(0, 0.01, 0.1, 1, 10, 100); r_grid <- mean_q * r_mult
    edf <- function(p) mean(q_i / (q_i + p))
    rl_amp <- rl_delta <- list(); worst <- worst_q <- 0
    for (k in seq_along(r_grid)) {
      dd <- lapply(seq_len(T), rlss_dense, r = r_grid[k], groups = groups_cond)
      cl <- lapply(seq_len(T), rlss_closed, r = r_grid[k], groups = groups_cond)
      Am <- t(sapply(dd, `[[`, "amp")); worst <- max(worst, relerr(t(sapply(cl, `[[`, "amp")), Am))
      worst_q <- max(worst_q, relerr(sapply(cl, `[[`, "q"), q_i))
      rl_amp[[k]] <- Am; rl_delta[[k]] <- t(sapply(dd, `[[`, "delta"))
    }
    check(sprintf("%s rLSS closed form vs dense solve (all trials, all r)", tag), worst, TOL_RLSS)
    check(sprintf("%s rLSS q_i closed form vs residual projection", tag), worst_q, TOL_RLSS)
    g1 <- rep(1L, T)
    rl_lss_eq <- t(sapply(seq_len(T), function(i) rlss_dense(i, 0, g1)$amp))
    check(sprintf("%s rLSS single-group r=0 vs fmrilss::lss", tag), relerr(rl_lss_eq, unname(b_lss)), TOL_RLSS)
    big <- lapply(seq_len(T), rlss_dense, r = 1e12 * mean_q, groups = groups_cond)
    Zm <- cbind(gsum(groups_cond), Fw); mfit <- solve(crossprod(Zm), crossprod(Zm, Yw))[1:3, ]
    check(sprintf("%s rLSS r=1e12*mean_q vs condition-mean-only fit", tag),
          relerr(t(sapply(big, `[[`, "amp")), mfit[groups_cond, ]), 1e-9)
    p_grid <- mean_q * 10^seq(-2, 2, length.out = 9); edf_targets <- seq(0.1, 0.9, by = 0.1)
    r_for_edf <- sapply(edf_targets, function(e)
      exp(uniroot(function(lp) edf(exp(lp)) - e, c(-40, 40), tol = 1e-15)$root))
    # NOTE: this round trip is circular (inverts the function that defines it); it checks
    # the root finder only. Independence for edf comes from the closed-form q_i check above.
    check(sprintf("%s df inverse round trip (circular; root finder only)", tag),
          max(abs(sapply(r_for_edf, edf) - edf_targets)), 1e-12)
    rk <- function(lst) { M <- do.call(rbind, lst); colnames(M) <- colnames(Y); M }
    keys <- paste(rep(seq_along(r_grid) - 1L, each = T), rep(tid, length(r_grid)), sep = ":")
    write_mat(Yw, P(d, "Y_white.csv")); write_mat(Xw, P(d, "X_trial_white.csv")); write_mat(Fw, P(d, "F_white.csv"))
    write_mat(`colnames<-`(unname(b_lsa), colnames(Y)), P(d, "beta_lsa.csv"), tid, "trial")
    write_mat(`colnames<-`(unname(b_lss), colnames(Y)), P(d, "beta_lss.csv"), tid, "trial")
    write_mat(rk(rl_amp), P(d, "rlss_amp.csv"), keys, "k:trial")
    write_mat(rk(rl_delta), P(d, "rlss_delta.csv"), keys, "k:trial")
    write_mat(`colnames<-`(rl_lss_eq, colnames(Y)), P(d, "rlss_lss_equivalent.csv"), tid, "trial")
    write_df(data.frame(k = seq_along(r_grid) - 1L, r_over_mean_q = fmt(r_mult), r = fmt(r_grid),
                        edf = fmt(sapply(r_grid, edf))), P(d, "rlss_ridges.csv"))
    write_df(data.frame(trial_index = seq_len(T), cond = trials$cond, q = fmt(q_i)), P(d, "df_q.csv"))
    write_df(data.frame(p_over_mean_q = fmt(p_grid / mean_q), p = fmt(p_grid),
                        edf = fmt(sapply(p_grid, edf))), P(d, "df_edf_grid.csv"))
    write_df(data.frame(edf_target = fmt(edf_targets), r = fmt(r_for_edf)), P(d, "df_inverse.csv"))
    info$mean_q[[v]] <- fmt(mean_q)
  }
  info
}
trial_info <- list(tx = run_trial_cell("tx"), ts = run_trial_cell("ts"))

# ============================================================== META + MANIFEST
git_sha <- function(d) {
  d <- path.expand(d)
  sha <- tryCatch(system2("git", c("-C", d, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE), error = function(e) NA)
  dirty <- tryCatch(length(system2("git", c("-C", d, "status", "--porcelain", "--untracked-files=no"),
                                   stdout = TRUE, stderr = FALSE)) > 0, error = function(e) NA)
  list(head = sha[1], dirty_tracked = dirty)
}
pkg_info <- function(p) {
  d <- packageDescription(p)
  list(version = d$Version, remote_sha = d$RemoteSha %||% NA_character_,
       built = d$Built, path = system.file(package = p))
}
`%||%` <- function(a, b) if (is.null(a)) b else a
meta <- list(
  schema = "phrf-cmp-s11-parity-v1",
  generated_by = "tools/phrf-comparison/parity/generate_parity_fixtures.R",
  fmrireg_source = fmrireg_source,
  r_version = R.version.string, platform = R.version$platform,
  blas = extSoftVersion()[["BLAS"]],
  packages = lapply(setNames(nm = c("fmrireg", "fmrihrf", "fmrilss", "fmridesign",
                                     "fmridataset", "fmriAR")), pkg_info),
  checkouts = lapply(setNames(nm = c("fmrireg", "fmrihrf", "fmrilss", "fmridesign")),
                     function(p) git_sha(file.path("~/code", p))),
  seeds = SEEDS,
  seed_note = "low 31 bits of SplitMix64 stream_seed(HARNESS_ROOT=0x7A3C91D50B44E2F1, cell_id, 0, purpose); cell ids s11-parity-cond, s11-parity-trial",
  tr = TR, start_time = START_TIME, convolution_precision = PRECISION,
  fixed_phi = fmt(FIXED_PHI), noise_phi_generating = fmt(NOISE_PHI),
  whitening = "per run; PRIMARY exact_first: z_1 = sqrt(1-phi^2) x_1, z_t = x_t - phi x_(t-1) (design v2.1 2.0.4 exactFirstAr1 = true; fmrireg exact_first = TRUE). Variant identity_first (subdirectory identity_first/): z_1 = x_1 (fmrireg exact_first = FALSE). phi given, one global value",
  baseline = "fmrireg baseline_model(basis='poly', degree=2, intercept='runwise'): 3 columns per run (poly1, poly2, run constant)",
  spmg1_peak_raw = fmt(SPMG1_PEAK),
  spmg1_note = "kernels are RAW (not peak-normalised); divide SPMG1 and all SPMG3 columns by spmg1_peak_raw for a peak-normalised convention (betas scale by the same factor)",
  fir = list(H = H, bins = "1 s, piecewise constant on [k-1, k), k = 1..H; zero outside [0, H)"),
  tau_grid = list(from = 0, to = H, step = TAU_STEP),
  cells = list(cond = cond_info$cond, cond_close = cond_info$cond_close,
               trial_tx = trial_info$tx, trial_ts = trial_info$ts),
  onset_grid_s = list(cond = 0.1, cond_close = 0.1, trial_tx = 1, trial_ts = 0.1),
  event_columns = list(can = 2, inf3 = 6, fir = 2 * H),
  rlss_groups = "condition (A, B, C)", r_multipliers_of_mean_q = c(0, 0.01, 0.1, 1, 10, 100),
  relative_error_definition = "max_i |a_i - b_i| / max_i |b_i|, b = reference (fixture)",
  checks = lapply(checks, function(x) list(name = x$name, value = x$value, tol = x$tol, pass = x$pass)),
  session_info = capture.output(print(sessionInfo()))
)
jsonlite::write_json(meta, file.path(out_dir, "meta.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "null")

files <- sort(list.files(out_dir, recursive = TRUE, full.names = FALSE))
files <- files[!files %in% c("MANIFEST.sha256", "MANIFEST.json")]
sha <- vapply(files, function(f) digest::digest(file.path(out_dir, f), algo = "sha256", file = TRUE), "")
writeLines(sprintf("%s  %s", sha, files), file.path(out_dir, "MANIFEST.sha256"))
jsonlite::write_json(list(algorithm = "sha256", files = as.list(setNames(sha, files))),
                     file.path(out_dir, "MANIFEST.json"), auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("\nwrote %d fixture files to %s; all %d cross-checks passed\n",
            length(files), out_dir, length(checks)))
