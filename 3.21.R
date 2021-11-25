## Sample
x <- c(82, 72, 45, 34, 17)

## Distribution Function
p <- function(beta, x) {
  out <-
    (1 - beta) ^ x[2] * (1 - 2 * beta) ^ x[3] * beta ^ (x[4] + x[5]) *
    (beta > 0) * (beta < 0.5) * 1
  out
}

## Probability Function
r <- function(beta_t, y, x) {
  out <- min(1, p(y, x) / p(beta_t, x))
  out
}

## MH Algorithm
rmh <- function(x, seed, iter) {
  set.seed(seed)
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

beta.seq <- rmh(x, 13, 1e4)

plot(beta.seq, type = 'l')
