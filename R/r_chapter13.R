library(rethinking)

# R code 13.1
data("reedfrogs")
d <- reedfrogs
str(d)

# R code 13.2

d$tank <- 1:nrow(d)

dat <- list(
  S = d$surv,
  N = d$density,
  tank = d$tank )

# approximate posterior
m13.1 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(0, 1.5)
  ), data = dat, chains = 4, log_lik = TRUE
)


# R code 13.3
m13.2 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 1.5),
    sigma ~ dexp(1)
  ), data = dat, chain = 4, log_lik = TRUE
)


# R code 13.4
compare(m13.1, m13.2)
