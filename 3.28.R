## ---- Parameters ----
phi_1 <- 0.6
sigma2_n <- 1 / 6
A <- 320
f <- 1.072 * 1e7
sigma2_eps <- 1

## ---- Generating Function ----
rpair <- function(X_old, t, N) {
  eta_new <- rnorm(n = N,
                   mean = 0,
                   sd = sqrt(sigma2_n))
  eps_new <- rnorm(n = N,
                   mean = 0,
                   sd = sqrt(sigma2_eps))
  X_new <- phi_1 * X_old + eta_new
  y_new <- A * cos(f * t + X_new) + eps_new
  out <- data.frame(X_t = X_new, y_t = y_new)
  return(out)
}

## ---- Q1 ----
X_0 <- 0
n <- 128
t <- 1
X <- c()
y <- c()
while (t <= n) {
  pair_t <- rpair(X_0, t, 1)
  X <- c(X, pair_t$X_t)
  y <- c(y, pair_t$y_t)
  t <- t + 1
}

## ---- Q2 ----
N <- 1e4
X_SIS <- rnorm(n = N,
               mean = y[1],
               sd = sqrt(sigma2_n))
X_t <- X_SIS
W_t <- rep(1, N)
W_SIS <- rbind(W_1 = W_t)

t <- 2
while (t <= n) {
  X_t <- rpair(X_t, t, N)$X_t
  U_t <-
    pnorm(
      q = y[t],
      mean = A * cos(f * t + X_t),
      sd = sqrt(sigma2_eps)
    )
  W_t <- W_t * U_t
  X_SIS <- rbind(X_SIS, X_t)
  W_SIS <- rbind(W_SIS, W_t)
  t <- t + 1
}

## ---- Q4 ----
X_SIS_RR <- rnorm(n = N,
                  mean = y[1],
                  sd = sqrt(sigma2_n))
X_t <- X_SIS_RR
W_t <- rep(1, N)
W_SIS_RR <- W_t[1]

t <- 2
while (t <= n) {
  X_t <- rpair(X_t, t, N)$X_t
  U_t <-
    pnorm(
      q = y[t],
      mean = A * cos(f * t + X_t),
      sd = sqrt(sigma2_eps)
    )
  W_t <- W_t * U_t
  W_t.bar <- mean(W_t)
  k_t <- floor(W_t / W_t.bar)
  N_r_t <- N - sum(k_t)
  # The resampling probability
  p.resample_t <- (W_t / W_t.bar - k_t) / N_r_t
  
  # The index of sample corresponding to its resample number for each X_t
  index.all_t <- c()
  for (k in 1:max(k_t)) {
    index.all_t <- c(index.all_t, which(k_t >= k))
    k <- k + 1
  }
  # Ensure the exception of N_r_t equals to 0 being considered
  if (N_r_t > 0) {
    index.resample_t <-
      sample(
        x = 1:N,
        size = N_r_t,
        replace = TRUE,
        prob = p.resample_t
      )
    index.all_t <- c(index.all_t, index.resample_t)
  }
  
  # Record X and W for each t
  X_SIS_RR <- rbind(X_SIS_RR, X_t)[, index.all_t]
  W_SIS_RR <- c(W_SIS_RR, W_t.bar)
  W_t <- rep(W_t.bar, N)
  t <- t + 1
}