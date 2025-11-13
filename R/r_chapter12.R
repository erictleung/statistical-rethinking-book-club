library(rethinking)

# R code 12.1
pbar <- 0.5
theta <- 5
curve(
  dbeta2(x, pbar, theta),
  from = 0,
  to = 1,
  xlab = "Probability",
  ylab = "Dentisty"
)


# R code 12.2
data(UCBadmit)

d <- UCBadmit
d$grid <- ifelse(d$applicant.gender == "male", 1L, 2L)

dat <- list(A = d$admit, N = d$applications, gid = d$gid)

m12.1 <- ulam(
  alist(
    A ~ dbetabinom(N, pbar, theta),
    logit(pbar) <- a[gid],  # GID = Gender Index
    a[gid] ~ dnorm(0, 1.5),
    transpar> theta <<- phi + 2.0,
    phi ~ dexp(1)
  ), data = dat, chains = 4
)


# R code 12.3
post <- extract.samples(m12.1)
post$da <- post$a[,1] - post$a[,2]
precis(post, depth = 2)


# R code 12.4
gid <- 2