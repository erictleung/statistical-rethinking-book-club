# R code 5.1 --------------------------------------------------------------

library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce

# Standardize variables
# standardize()
d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)


# R code 5.2 --------------------------------------------------------------

sd(d$MedianAgeMarriage)
# [1] 1.24363


# R code 5.3 --------------------------------------------------------------

m5_1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d)
m5_1
# Quadratic approximate posterior distribution
#
# Formula:
# D ~ dnorm(mu, sigma)
# mu <- a + bA * A
# a ~ dnorm(0, 0.2)
# bA ~ dnorm(0, 0.5)
# sigma ~ dexp(1)
#
# Posterior means:
#            a            bA         sigma
# 1.119044e-07 -5.684029e-01  7.883249e-01
#
# Log-likelihood: -59.45


# R code 5.4 --------------------------------------------------------------

set.seed(10)

prior <- extract.prior(m5_1)  # Collect prior samples from model

mu <- link(m5_1, post = prior, data = list(A = c(-2, 2)))  # Model predictions

plot(NULL, xlim = c(-2, 2), ylim = c(-2, 2))
for (i in 1:50) {
  lines(c(-2, 2), mu[i, ], col = col.alpha("black", 0.4))
}

# Plausible regression lines implied by priors


# R code 5.5 --------------------------------------------------------------

# Get some example values to check on
A_seq <- seq(from = -3, to = 3.2, length.out = 30)

# Takes quap approximiation, samples posterior distribution, then gets mu for
# each case in the data and sample from the posterior
mu <- link(m5_1, data = list(A = A_seq))

str(mu)
# num [1:1000, 1:30] 1.87 2.38 2.02 1.46 2.24 ...

dim(mu)
# [1] 1000   30
# Here are 1000 samples of 30 samples from the posterior

mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

plot(D ~ A, data = d, col = rangi2)
lines(A_seq, mu_mean, lwd = 2)
shade(mu_PI, A_seq)

# Plotted posterior predictions


# R code 5.6 --------------------------------------------------------------

# Relationship between divorce rate and marriage rate, compared with divorce
# rate with median age of marriage

m5_2 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)
m5_2
# Quadratic approximate posterior distribution
#
# Formula:
# D ~ dnorm(mu, sigma)
# mu <- a + bM * M
# a ~ dnorm(0, 0.2)
# bM ~ dnorm(0, 0.5)
# sigma ~ dexp(1)
#
# Posterior means:
#            a           bM        sigma
# 1.359623e-05 3.500535e-01 9.102652e-01
#
# Log-likelihood: -66.7

precis(m5_2)
#       mean   sd  5.5% 94.5%
# a     0.00 0.11 -0.17  0.17
# bM    0.35 0.13  0.15  0.55
# sigma 0.91 0.09  0.77  1.05