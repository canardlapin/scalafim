.libPaths(c(Sys.getenv("PBKRTEST_LIB", .libPaths()[1]), .libPaths()))
suppressMessages({library(lme4); library(pbkrtest)})
set.seed(5); g <- 12; m <- 6; n <- g*m
grp <- factor(rep(1:g, each = m)); x <- rnorm(n); x2 <- rnorm(n)
y <- 1 + 0.3*x + rnorm(g)[grp]*0.8 + rnorm(n)
fit <- lmer(y ~ x + x2 + (1|grp), REML = TRUE)
Va <- vcovAdj(fit); L <- rbind(c(0,1,0), c(0,0,1))
ref <- KRmodcomp(fit, L)
X <- model.matrix(fit); Z <- model.matrix(~ grp - 1)
vc <- as.data.frame(VarCorr(fit)); th <- c(vc$vcov[2], vc$vcov[1]) # (sigma_e^2, sigma_u^2)
generic_kr <- function(X, vinv, th, Lm, beta) {
  k <- length(th); h <- 1e-4 * pmax(abs(th), 0.05); V0i <- vinv(th); V <- solve(V0i)
  D <- lapply(1:k, function(i) { e <- numeric(k); e[i] <- h[i]; (vinv(th + e) - vinv(th - e)) / (2*h[i]) })
  D2 <- matrix(list(), k, k)
  for (i in 1:k) for (j in 1:k) { ei <- numeric(k); ei[i] <- h[i]; ej <- numeric(k); ej[j] <- h[j]
    D2[[i,j]] <- (vinv(th+ei+ej) - vinv(th+ei-ej) - vinv(th-ei+ej) + vinv(th-ei-ej))/(4*h[i]*h[j]) }
  Phi <- solve(t(X) %*% V0i %*% X); P <- lapply(D, function(Di) t(X) %*% Di %*% X)
  Q <- matrix(list(), k, k); for (i in 1:k) for (j in 1:k) Q[[i,j]] <- t(X) %*% D[[i]] %*% V %*% D[[j]] %*% X
  PV <- V0i - V0i %*% X %*% Phi %*% t(X) %*% V0i
  G <- lapply(D, function(Di) PV %*% (-(V %*% Di %*% V)))
  I <- matrix(0, k, k); for (i in 1:k) for (j in 1:k) I[i,j] <- 0.5*sum(G[[i]] * t(G[[j]]))
  Wt <- solve(I); U <- 0; Ur <- 0
  for (i in 1:k) for (j in 1:k) { R <- Q[[i,j]] + Q[[j,i]] - t(X) %*% D2[[i,j]] %*% X
    U <- U + Wt[i,j]*(Q[[i,j]] - P[[i]] %*% Phi %*% P[[j]]); Ur <- Ur + Wt[i,j]*R }
  list(PhiA = Phi + 2*Phi %*% U %*% Phi, PhiAR = Phi + 2*Phi %*% (U - Ur/4) %*% Phi, Rnorm = max(abs(Ur)), W = Wt)
}
vinv <- function(th) solve(th[1]*diag(n) + th[2]*Z %*% t(Z))
k <- generic_kr(X, vinv, th, L, fixef(fit))
cat("max |PhiA - pbkrtest| =", max(abs(k$PhiA - as.matrix(Va))), " (R-term max", k$Rnorm, ")\n")
print(as.matrix(Va)); print(k$PhiA)
print(ref)
