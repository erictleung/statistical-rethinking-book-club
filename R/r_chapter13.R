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

# WAIC
# SE
# dWAIC


# R code 13.5

# Extract Stan samples
post <- extract.samples(m13.2)

# Compute mean intercept for each tank
# Also transform to probability with logistic
d$propsurv.est <- logistic(apply(post$a, 2, mean))

# Display raw proportions surviving in each tank
plot(
  d$propsurv,
  ylim = c(0, 1),
  pch = 16,
  xaxt = "n",
  xlab = "Tank",
  ylab = "Proportion survival",
  col = rangi2
)
axis(
  1,
  at = c(1, 16, 32, 48),
  labels = c(1, 16, 32, 48)
)
