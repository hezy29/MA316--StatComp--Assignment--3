## ---- Default ----
N <- 1e3
prep <- 1:(0.2 * N)
# set random seed
set.seed(10)

## Sample
x <- c(82, 72, 45, 34, 17)

## Distribution Function
p <- function(beta, x) {
  out <-
    (1 - beta) ^ x[2] * (1 - 2 * beta) ^ x[3] *
    beta ^ (x[4] + x[5]) * (beta > 0) * (beta < 0.5) * 1
  out
}

## Probability Function
r <- function(beta_t, y, x) {
  out <- min(1, p(y, x) / p(beta_t, x))
  out
}

## MH Algorithm
rmh <- function(x, iter) {
  beta <- runif(n = 1, min = 0, max = 0.5)
  beta.seq <- c(beta)
  for (i in 1:iter) {
    y <- runif(n = 1, min = 0, max = 0.5)
    if (y <= r(beta, y, x)) {
      beta <- y
    }
    beta.seq <- c(beta.seq, beta)
  }
  beta.seq
}

# Calculate the beta sequence
beta.seq <- rmh(x, N)

# Calculate the posterior mean estimate of beta
beta.bar <- mean(beta.seq[-prep])
beta.bar

# Calculate the standard error of beta
beta.SE <- sqrt(var(beta.seq[-prep]) / (0.8 * N))
beta.SE


## ---- Different Init ----
# Generate Different Initial Values
ini.val <- function(m = 250, n = 5) {
  out <-
    diff(c(0, sort(
      sample(
        x = 1:m,
        size = 4,
        replace = FALSE,
        prob = rep(1 / m, m)
      )
    ), m))
  return(out)
}

# Calculate the beta sequence
beta.seq_1 <- rmh(x_1 <- ini.val(), N)
beta.seq_2 <- rmh(x_2 <- ini.val(), N)
beta.seq_3 <- rmh(x_3 <- ini.val(), N)

# Calculate the posterior mean estimate of beta
beta.bar_1 <- mean(beta.seq_1[-prep])
beta.bar_2 <- mean(beta.seq_2[-prep])
beta.bar_3 <- mean(beta.seq_3[-prep])
beta.bar.all <-
  data.frame(
    Default = beta.bar,
    Rand_1 = beta.bar_1,
    Rand_2 = beta.bar_2,
    Rand_3 = beta.bar_3
  )
beta.bar.all

# Calculate the standard error of beta
beta.SE_1 <- sqrt(var(beta.seq_1[-prep]) / (0.8 * N))
beta.SE_2 <- sqrt(var(beta.seq_2[-prep]) / (0.8 * N))
beta.SE_3 <- sqrt(var(beta.seq_3[-prep]) / (0.8 * N))
beta.SE.all <- data.frame(
  Default = beta.SE,
  Rand_1 = beta.SE_1,
  Rand_2 = beta.SE_2,
  Rand_3 = beta.SE_3
)
beta.SE.all

# Plot the beta sequence
par(mfrow = c(2, 2))
plot(beta.seq, type = 'l')
plot(beta.seq_1, type = 'l')
plot(beta.seq_2, type = 'l')
plot(beta.seq_3, type = 'l')
