## ---- Parameters ----
phi_1 <- 0.6
sigma2_n <- 1 / 6
A <- 320
f <- 1.072 * 1e7
sigma2_eps <- 1

## ---- Generating Function ----
rpair <- function(X_old, t) {
  eta_new <- rnorm(n = 1,
                   mean = 0,
                   sd = sqrt(sigma2_n))
  eps_new <- rnorm(n = 1,
                   mean = 0,
                   sd = sqrt(sigma2_eps))
  X_new <- phi_1 * X_old + eta_new
  y_new <- A * cos(f * t + X_new) + eps_new
  return(c(X_new, y_new))
}

## ---- Q1 ----
X_0 <- 0
n <- 128
t <- 1
X <- c()
y <- c()
while (t <= n) {
  pair_t <- rpair(X_0, t)
  X <- c(X, pair_t[1])
  y <- c(y, pair_t[2])
  t <- t + 1
}

## ---- Q2 ----
N <- 1e4
X_SIS <- rnorm(n = N,
               mean = y[1],
               sd = sqrt(sigma2_n))
X_t <- X_SIS
W_SIS <- rbind(W_1 = rep(1, N))
t <- 2
while (t <= n) {
  X_t <- phi_1 * X_t + rnorm(n = N,
                             mean = 0,
                             sd = sqrt(sigma2_n))
  U_t <-
    pnorm(
      q = y[t],
      mean = A * cos(f * t + X_t),
      sd = sqrt(sigma2_eps)
    )
  W_t <- W_SIS[t - 1,] * U_t
  X_SIS <- rbind(X_SIS, X_t)
  W_SIS <- rbind(W_SIS, W_t)
  t <- t + 1
}