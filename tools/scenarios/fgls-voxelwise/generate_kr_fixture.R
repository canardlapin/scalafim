# Deterministic Kenward-Roger fixture for VoxelwiseArFgls (no RNG).
# The generic KR below is validated against pbkrtest::vcovAdj / KRmodcomp on a
# linear covariance model by validate_generic_kr() (run with pbkrtest present);
# AR whitening is non-linear in phi, so the KR (1997) second-derivative term is kept.
runs <- c(20L, 30L); n <- sum(runs); starts <- c(0L, runs[1])
censor <- c(5L, 33L) # zero-based rows
t0 <- 0:(n - 1); run_of <- ifelse(t0 < runs[1], 1L, 2L)
X <- cbind(cos(0.3 * t0), sin(0.11 * t0) * (t0 / n), as.numeric(run_of == 1), as.numeric(run_of == 2))
y <- sin(0.7 * t0) + 0.3 * cos(1.3 * t0^1.1) + 0.2 * X[, 1]
ar_acvf <- function(phi, maxlag) { m <- maxlag + 400; psi <- numeric(m); psi[1] <- 1
  for (j in 2:m) for (k in seq_along(phi)) if (j - k >= 1) psi[j] <- psi[j] + phi[k] * psi[j - k]
  sapply(0:maxlag, function(h) sum(psi[1:(m - h)] * psi[(1 + h):m])) }
init_block <- function(ph, exact) { q <- length(ph)
  if (exact) return(solve(t(chol(toeplitz(ar_acvf(ph, q - 1)[1:q])))))
  B <- diag(q); if (q == 1) B[1, 1] <- sqrt(1 - ph^2) else for (i in 2:q) for (j in 1:(i - 1)) B[i, j] <- -ph[i - j]; B }
segments <- function(restart) { out <- list()
  for (r in 1:2) { a <- starts[r]; b <- starts[r] + runs[r]
    resets <- if (restart) sort(censor[censor >= a & censor < b - 1] + 1L) else integer(0)
    for (s in resets) { out[[length(out) + 1]] <- c(a, s, r); a <- s }
    out[[length(out) + 1]] <- c(a, b, r) }; out }
W_build <- function(phis, restart) { W <- matrix(0, n, n); q <- length(phis[[1]])
  for (s in segments(restart)) { a <- s[1]; b <- s[2]; ph <- phis[[s[3]]]; B <- init_block(ph, !restart)
    for (t in a:(b - 1)) { l <- t - a
      if (l < q) { for (j in 0:l) W[t + 1, a + j + 1] <- B[l + 1, j + 1] }
      else { W[t + 1, t + 1] <- 1; for (k in 1:q) W[t + 1, t - k + 1] <- -ph[k] } } }
  W }
generic_kr <- function(Xd, vinv, th) {
  k <- length(th); h <- 1e-4 * pmax(abs(th), 0.05); V0i <- vinv(th); V <- solve(V0i)
  D <- lapply(1:k, function(i) { e <- numeric(k); e[i] <- h[i]; (vinv(th + e) - vinv(th - e)) / (2 * h[i]) })
  D2 <- matrix(list(), k, k)
  for (i in 1:k) for (j in 1:k) { ei <- numeric(k); ei[i] <- h[i]; ej <- numeric(k); ej[j] <- h[j]
    D2[[i, j]] <- (vinv(th + ei + ej) - vinv(th + ei - ej) - vinv(th - ei + ej) + vinv(th - ei - ej)) / (4 * h[i] * h[j]) }
  Phi <- solve(t(Xd) %*% V0i %*% Xd); P <- lapply(D, function(Di) t(Xd) %*% Di %*% Xd)
  Q <- matrix(list(), k, k); for (i in 1:k) for (j in 1:k) Q[[i, j]] <- t(Xd) %*% D[[i]] %*% V %*% D[[j]] %*% Xd
  PV <- V0i - V0i %*% Xd %*% Phi %*% t(Xd) %*% V0i
  G <- lapply(D, function(Di) PV %*% (-(V %*% Di %*% V)))
  I <- matrix(0, k, k); for (i in 1:k) for (j in 1:k) I[i, j] <- 0.5 * sum(G[[i]] * t(G[[j]]))
  Wt <- solve(I); U <- 0
  for (i in 1:k) for (j in 1:k) { R <- Q[[i, j]] + Q[[j, i]] - t(Xd) %*% D2[[i, j]] %*% Xd
    U <- U + Wt[i, j] * (Q[[i, j]] - P[[i]] %*% Phi %*% P[[j]] - 0.25 * R) }
  list(Phi = Phi, PhiA = Phi + 2 * Phi %*% U %*% Phi, P = P, W = Wt)
}
kr_moments <- function(k, Lm) { q <- nrow(Lm); Phi <- k$Phi
  Theta <- t(Lm) %*% solve(Lm %*% Phi %*% t(Lm), Lm); A1 <- A2 <- 0; K <- length(k$P)
  for (i in 1:K) for (j in 1:K) { ui <- Theta %*% Phi %*% k$P[[i]] %*% Phi; uj <- Theta %*% Phi %*% k$P[[j]] %*% Phi
    A1 <- A1 + k$W[i, j] * sum(diag(ui)) * sum(diag(uj)); A2 <- A2 + k$W[i, j] * sum(ui * t(uj)) }
  B <- (A1 + 6 * A2) / (2 * q); g <- ((q + 1) * A1 - (q + 4) * A2) / ((q + 2) * A2)
  d <- 3 * q + 2 * (1 - g); c1 <- g / d; c2 <- (q - g) / d; c3 <- (q + 2 - g) / d
  Es <- 1 / (1 - A2 / q); Vs <- (2 / q) * (1 + c1 * B) / ((1 - c2 * B)^2 * (1 - c3 * B))
  rho <- Vs / (2 * Es^2); m <- 4 + (q + 2) / (q * rho - 1); c(df = m, scale = m / (Es * (m - 2))) }
case_values <- function(restart, pooled) {
  phis <- if (pooled) list(c(0.42, -0.12), c(0.42, -0.12)) else list(c(0.42, -0.12), c(0.30, 0.05))
  Xd <- if (restart) X else cbind(X, sapply(censor, function(cr) as.numeric(t0 == cr)))
  W <- W_build(phis, restart); Z <- W %*% Xd; zy <- W %*% y
  beta <- solve(crossprod(Z), crossprod(Z, zy)); s2 <- sum((zy - Z %*% beta)^2) / (n - ncol(Xd))
  vinv <- function(th) { ph <- if (pooled) list(th[2:3], th[2:3]) else list(th[2:3], th[4:5])
    crossprod(W_build(ph, restart)) / th[1] }
  th <- c(s2, if (pooled) phis[[1]] else c(phis[[1]], phis[[2]]))
  k <- generic_kr(Xd, vinv, th)
  L1 <- matrix(c(1, 0, rep(0, ncol(Xd) - 2)), 1); L2 <- rbind(L1, c(0, 1, rep(0, ncol(Xd) - 2)))
  list(phi = phis, sigma2 = s2, beta = beta[1:4], phiA = k$PhiA[1:4, 1:4], phiCond = k$Phi[1:4, 1:4],
       t = kr_moments(k, L1), f = kr_moments(k, L2))
}
fmt <- function(v) paste(sprintf("%.17g", v), collapse = ", ")
emit <- function(name, restart, pooled) { v <- case_values(restart, pooled)
  cat(sprintf("  val %s = KrCase(\n    restart = %s,\n    pooled = %s,\n    sigma2 = %s,\n    beta = Vector(%s),\n    conditional = Vector(%s),\n    adjusted = Vector(%s),\n    tDf = %s,\n    tScale = %s,\n    fDf = %s,\n    fScale = %s\n  )\n",
    name, tolower(restart), tolower(pooled), fmt(v$sigma2), fmt(v$beta), fmt(t(v$phiCond)), fmt(t(v$phiA)),
    fmt(v$t["df"]), fmt(v$t["scale"]), fmt(v$f["df"]), fmt(v$f["scale"]))) }
cat("// Generated by tools/scenarios/fgls-voxelwise/generate_kr_fixture.R; do not edit.\n")
emit("ContinuousPerRun", FALSE, FALSE); emit("ContinuousPooled", FALSE, TRUE)
emit("RestartPerRun", TRUE, FALSE); emit("RestartPooled", TRUE, TRUE)
