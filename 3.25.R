library(ggplot2)

## Parameters
n <- 20
alpha <- 0.5
beta <- 0.5
N <- 1e4

## Target Distribution
ptd <- function(n, x, y, alpha, beta) {
  out <-
    choose(n, x) * y ^ (x + alpha - 1) * (1 - y) ^ (n - x + beta - 1)
  out
}

## Gibbs Sampling
x <- 0
y <- rbeta(n = 1, shape1 = alpha, shape2 = beta)
X <- c(x)
Y <- c(y)
avg_Y <- c(mean(Y))
for (i in 1:N) {
  x <- rbinom(n = 1, size = n, prob = y)
  X <- c(X, x)
  y <- rbeta(n = 1,
             shape1 = x + alpha,
             shape2 = n - x + beta)
  Y <- c(Y, y)
  avg_Y <- c(avg_Y, mean(Y))
}

## Histogram
data <- data.frame(X, Y)
ggplot(data = data, mapping = aes(x = Y)) + geom_histogram(binwidth = 0.01)

## Density of Beta(alpha,beta)
seq_x <- seq(0, 1, length = 100)
qplot(
  x = seq_x,
  y = dbeta(seq_x, shape1 = alpha, shape2 = beta),
  geom = "line"
)
