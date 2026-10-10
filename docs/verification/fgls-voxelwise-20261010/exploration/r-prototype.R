# Exploratory R prototype (development stream only; R RNG, never a declared Scala stream).
args <- commandArgs(TRUE)
reps <- if (length(args) > 0) as.integer(args[1]) else 400
long <- length(args) > 1 && args[2] == "long"
seedarg <- if (length(args) > 2) as.integer(args[3]) else 1; set.seed(20261010 + seedarg)
runs <- if (long) c(192, 288) else c(96, 144)
n <- sum(runs); starts <- c(0, cumsum(runs)[-length(runs)])
phi_true <- c(0.45, -0.10); p <- 2
spm <- function(t) { # SPM canonical, matches SPMG1 approx
  a1 <- 6; a2 <- 16; b <- 1; c <- 1/6
  ifelse(t < 0, 0, dgamma(t, a1, b) - c * dgamma(t, a2, b))
}
kern <- spm(0:32); kern <- kern / max(kern) # scale irrelevant for calibration
X <- matrix(0, n, 4 + 12)
for (r in seq_along(runs)) for (loc in 0:(runs[r] - 1)) {
  row <- starts[r] + loc + 1
  for (task in 0:1) { tot <- 0; on <- 6 + task * 32
    while (on <= loc) { for (d in 0:3) { lag <- loc - on - d; if (lag >= 0 && lag < 33) tot <- tot + kern[lag + 1] }; on <- on + 64 }
    X[row, task + 1] <- tot }
  for (k in 1:6) X[row, 2 + (r - 1) * 6 + k] <- cos(pi * (2 * loc + 1) * k / (2 * runs[r]))
  X[row, 14 + r] <- 1
}
censor <- unlist(lapply(seq_along(runs), function(r) { v <- c(23, 24, 60, if (runs[r] > 97) seq(97, runs[r] - 1, by = 37)); v <- v[v < runs[r]]; v + starts[r] })) + 1
run_of <- rep(seq_along(runs), runs)
spikes <- sapply(censor, function(cr) { v <- numeric(n); v[cr] <- 1; v })
Xs <- cbind(X, spikes)

sim_noise <- function(phi) {
  out <- numeric(0)
  for (r in seq_along(runs)) { e <- rnorm(runs[r] + 256); x <- numeric(length(e))
    for (t in seq_along(e)) x[t] <- e[t] + (if (t > 1) phi[1] * x[t - 1] else 0) + (if (t > 2 && length(phi) > 1) phi[2] * x[t - 2] else 0)
    out <- c(out, tail(x, runs[r])) }
  out
}
ar_acvf <- function(phi, maxlag) { # stationary acvf, sigma2=1
  m <- maxlag + 400; psi <- numeric(m); psi[1] <- 1
  for (j in 2:m) for (k in seq_along(phi)) if (j - k >= 1) psi[j] <- psi[j] + phi[k] * psi[j - k]
  sapply(0:maxlag, function(h) sum(psi[1:(m - h)] * psi[(1 + h):m]))
}
# whitening matrix. segs: list of (start,end) 1-based inclusive with run phi
W_build <- function(phis, segs, exact) {
  W <- matrix(0, n, n)
  for (s in segs) { ph <- phis[[s$run]]; q <- length(ph); a <- s$a; b <- s$b
    first <- a
    if (exact) { g <- ar_acvf(ph, q); G <- toeplitz(g[1:q]); L <- t(chol(G)); Li <- solve(L)
      W[a:(a + q - 1), a:(a + q - 1)] <- Li; first <- a + q
    } else if (q == 1) { W[a, a] <- sqrt(1 - ph^2); first <- a + 1 }
    for (t in a:b) { if (t < first) next
      W[t, t] <- 1; for (k in 1:q) if (t - k >= a) W[t, t - k] <- -ph[k] }
  }
  W
}
run_segs <- lapply(seq_along(runs), function(r) list(a = starts[r] + 1, b = starts[r] + runs[r], run = r))
reset_segs <- {
  out <- list(); for (s in run_segs) { resets <- sort(censor[censor >= s$a & censor < s$b]) + 1; a <- s$a
    for (rs in resets) { out[[length(out) + 1]] <- list(a = a, b = rs - 1, run = s$run); a <- rs }
    out[[length(out) + 1]] <- list(a = a, b = s$b, run = s$run) }; out }
# estimation: rows kept (exclude censored), segments split at censored rows
kept <- setdiff(1:n, censor)
pairs_of <- function(h, r) { rows <- kept[run_of[kept] == r]; rows <- rows[(rows + h) %in% rows]
  # only pairs within contiguous retained segment: require no censored row between
  rows[sapply(rows, function(a) !any(censor > a & censor < a + h + 1) )] }
L <- 5
pairs <- lapply(seq_along(runs), function(r) lapply(0:L, function(h) pairs_of(h, r)))
Sk <- lapply(0:L, function(k) { S <- matrix(0, n, n); for (i in 1:n) for (j in 1:n) if (abs(i - j) == k && run_of[i] == run_of[j]) S[i, j] <- 1; S })
biasA <- function(Xd) {
  Q <- qr.Q(qr(Xd)); M <- diag(n) - Q %*% t(Q)
  lapply(seq_along(runs), function(r) { rows <- kept[run_of[kept] == r]
    C <- matrix(0, n, n); C[cbind(rows, rows)] <- 1; C[rows, rows] <- C[rows, rows] - 1 / length(rows)
    R <- C %*% M
    A <- matrix(0, L + 1, L + 1)
    for (k in 0:L) { RS <- R %*% Sk[[k + 1]] %*% t(R)
      for (h in 0:L) { a <- pairs[[r]][[h + 1]]; A[h + 1, k + 1] <- mean(RS[cbind(a, a + h)]) } }
    A })
}
A_plain <- biasA(X); A_spike <- biasA(Xs)
yw <- function(g, q) { G <- toeplitz(g[1:q]); solve(G, g[2:(q + 1)]) }
est_phi <- function(res, A, pooled) {
  gam <- lapply(seq_along(runs), function(r) { rows <- kept[run_of[kept] == r]; e <- res; e[rows] <- e[rows] - mean(e[rows])
    raw <- sapply(0:L, function(h) { a <- pairs[[r]][[h + 1]]; mean(e[a] * e[a + h]) })
    as.vector(solve(A[[r]], raw)) })
  if (pooled) { w <- sapply(seq_along(runs), function(r) length(pairs[[r]][[1]])); g <- Reduce(`+`, Map(`*`, gam, w)) / sum(w)
    ph <- yw(g, p); list(ph, ph) } else lapply(gam, yw, q = p)
}
stationary <- function(ph) all(Mod(polyroot(c(1, -ph))) > 1 + 1e-8)

gls <- function(Xd, y, W) { Z <- W %*% Xd; yw_ <- W %*% y; Zq <- qr(Z); b <- qr.coef(Zq, yw_)
  df <- n - ncol(Xd); s2 <- sum(qr.resid(Zq, yw_)^2) / df; Phi <- s2 * chol2inv(qr.R(Zq))[order(Zq$pivot), order(Zq$pivot)]
  list(b = b, Phi = Phi, s2 = s2, df = df, W = W) }

# Kenward-Roger for V = s2 * (W(phi)'W(phi))^-1 ; theta = (s2, phis...)
kr <- function(Xd, y, phis, segs, exact, pooled, s2, Lmat) {
  vinv <- function(th) { s <- th[1]; ph <- if (pooled) list(th[2:3], th[2:3]) else list(th[2:3], th[4:5])
    Wm <- W_build(ph, segs, exact); crossprod(Wm) / s }
  th <- c(s2, if (pooled) phis[[1]] else c(phis[[1]], phis[[2]])); k <- length(th)
  h <- 1e-4 * pmax(abs(th), 0.05)
  V0i <- vinv(th); V <- solve(V0i)
  D <- lapply(1:k, function(i) { e <- numeric(k); e[i] <- h[i]; (vinv(th + e) - vinv(th - e)) / (2 * h[i]) })
  D2 <- matrix(list(), k, k)
  for (i in 1:k) for (j in i:k) { ei <- numeric(k); ei[i] <- h[i]; ej <- numeric(k); ej[j] <- h[j]
    D2[[i, j]] <- (vinv(th + ei + ej) - vinv(th + ei - ej) - vinv(th - ei + ej) + vinv(th - ei - ej)) / (4 * h[i] * h[j]); D2[[j, i]] <- D2[[i, j]] }
  Phi <- solve(t(Xd) %*% V0i %*% Xd)
  P <- lapply(D, function(Di) t(Xd) %*% Di %*% Xd)
  VDX <- lapply(D, function(Di) V %*% (Di %*% Xd))
  Q <- matrix(list(), k, k); for (i in 1:k) for (j in 1:k) Q[[i, j]] <- t(Xd) %*% D[[i]] %*% VDX[[j]]
  PV <- V0i - V0i %*% Xd %*% Phi %*% t(Xd) %*% V0i
  G <- lapply(D, function(Di) PV %*% (-(V %*% Di %*% V)))
  I <- matrix(0, k, k); for (i in 1:k) for (j in 1:k) I[i, j] <- 0.5 * sum(G[[i]] * t(G[[j]]))
  Wt <- solve(I)
  U <- matrix(0, ncol(Xd), ncol(Xd))
  for (i in 1:k) for (j in 1:k) { R <- Q[[i, j]] + Q[[j, i]] - t(Xd) %*% D2[[i, j]] %*% Xd
    U <- U + Wt[i, j] * (Q[[i, j]] - P[[i]] %*% Phi %*% P[[j]] - 0.25 * R) }
  PhiA <- Phi + 2 * Phi %*% U %*% Phi
  stat <- function(Lm, beta_diff) { q <- nrow(Lm)
    Theta <- t(Lm) %*% solve(Lm %*% Phi %*% t(Lm), Lm); A1 <- A2 <- 0
    for (i in 1:k) for (j in 1:k) { ui <- Theta %*% Phi %*% P[[i]] %*% Phi; uj <- Theta %*% Phi %*% P[[j]] %*% Phi
      A1 <- A1 + Wt[i, j] * sum(diag(ui)) * sum(diag(uj)); A2 <- A2 + Wt[i, j] * sum(ui * t(uj)) }
    B <- (A1 + 6 * A2) / (2 * q); g <- ((q + 1) * A1 - (q + 4) * A2) / ((q + 2) * A2)
    c1 <- g / (3 * q + 2 * (1 - g)); c2 <- (q - g) / (3 * q + 2 * (1 - g)); c3 <- (q + 2 - g) / (3 * q + 2 * (1 - g))
    Es <- 1 / (1 - A2 / q); Vs <- (2 / q) * (1 + c1 * B) / ((1 - c2 * B)^2 * (1 - c3 * B))
    rho <- Vs / (2 * Es^2); m <- 4 + (q + 2) / (q * rho - 1); lam <- m / (Es * (m - 2))
    Fu <- as.numeric(t(beta_diff) %*% t(Lm) %*% solve(Lm %*% PhiA %*% t(Lm), Lm %*% beta_diff)) / q
    c(F = lam * Fu, m = m, lam = lam) }
  list(PhiA = PhiA, Phi = Phi, stat = stat)
}

engines <- c("known_restart", "known_exact_spike", "est_run_restart", "est_run_exact_spike", "est_pool_exact_spike",
             "kr_run_exact_spike", "kr_pool_exact_spike")
rec <- list()
L1 <- matrix(0, 1, ncol(Xs)); L1[1, 1] <- 1
L2 <- matrix(0, 2, ncol(Xs)); L2[1, 1] <- 1; L2[2, 2] <- 1
alpha <- 0.05
for (rep in 1:reps) {
  for (vox in 0:1) {
    beta <- c(if (vox == 0) c(0, 0) else c(0.75, -0.25), 0.1 * (3:16) * (vox + 1))
    y <- as.vector(X %*% beta) + sim_noise(phi_true)
    for (eng in engines) {
      exact <- grepl("exact", eng); Xd <- if (grepl("spike", eng)) Xs else X
      segs <- if (exact) run_segs else reset_segs
      phis <- if (grepl("known", eng)) list(phi_true, phi_true) else {
        res <- as.vector(qr.resid(qr(Xd), y)); est_phi(res, if (grepl("spike", eng)) A_spike else A_plain, grepl("pool", eng)) }
      if (!all(sapply(phis, stationary))) { rec[[length(rec) + 1]] <- data.frame(rep, vox, eng, ok = FALSE, est = NA, var = NA, rejT = NA, rejF = NA, cover = NA, phi1 = NA, phi2 = NA, se2 = NA, df = NA); next }
      Wm <- W_build(phis, segs, exact); f <- gls(Xd, y, Wm)
      Ld1 <- L1[, 1:ncol(Xd), drop = FALSE]; Ld2 <- L2[, 1:ncol(Xd), drop = FALSE]
      if (grepl("kr", eng)) {
        k <- kr(Xd, y, phis, segs, exact, grepl("pool", eng), f$s2)
        sT <- k$stat(Ld1, c(f$b[1] - (if (vox == 0) 0 else 0.75), rep(0, ncol(Xd) - 1)))
        sTz <- k$stat(Ld1, c(f$b[1], rep(0, ncol(Xd) - 1)))
        sF <- k$stat(Ld2, c(f$b[1:2], rep(0, ncol(Xd) - 2)))
        v <- k$PhiA[1, 1]
        rejT <- sTz["F"] > qf(1 - alpha, 1, sTz["m"]); rejF <- sF["F"] > qf(1 - alpha, 2, sF["m"])
        cover <- sT["F"] <= qf(1 - alpha, 1, sT["m"]); dfo <- sTz["m"]
      } else {
        v <- f$Phi[1, 1]; tc <- qt(1 - alpha / 2, f$df)
        rejT <- abs(f$b[1]) / sqrt(v) > tc
        Fs <- as.numeric(t(f$b[1:2]) %*% solve(f$Phi[1:2, 1:2], f$b[1:2])) / 2
        rejF <- Fs > qf(1 - alpha, 2, f$df); cover <- abs(f$b[1] - 0.75) <= tc * sqrt(v); dfo <- f$df
      }
      rec[[length(rec) + 1]] <- data.frame(rep, vox, eng, ok = TRUE, est = f$b[1], var = v, rejT, rejF, cover,
                                            phi1 = mean(sapply(phis, `[`, 1)), phi2 = mean(sapply(phis, `[`, 2)),
                                            se2 = mean(sapply(phis, function(ph) sum((ph - phi_true)^2) / 2)), df = dfo)
    }
  }
  if (rep %% 50 == 0) cat("rep", rep, "\n", file = stderr())
}
d <- do.call(rbind, rec); rownames(d) <- NULL
saveRDS(d, sprintf("proto-%s-%d-s%d.rds", if (long) "long" else "short", reps, seedarg))
summ <- do.call(rbind, lapply(engines, function(e) { a <- d[d$eng == e & d$vox == 0 & d$ok, ]; b <- d[d$eng == e & d$vox == 1 & d$ok, ]
  data.frame(engine = e, n = nrow(a), refused = sum(!d$ok[d$eng == e]), calib = mean(a$var) / var(a$est), rejT = mean(a$rejT), rejF = mean(a$rejF),
             cover = mean(b$cover), rmse = sqrt(mean(c(a$se2, b$se2))), bias1 = mean(c(a$phi1, b$phi1)) - phi_true[1],
             bias2 = mean(c(a$phi2, b$phi2)) - phi_true[2], meandf = mean(a$df)) }))
print(summ, digits = 4)
