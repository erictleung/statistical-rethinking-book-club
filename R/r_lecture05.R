# Fork --------------------------------------------------------------------

# https://youtu.be/mBEA7PKDmiY?si=5N25CfB7TmObV1-Y&t=502

# Simulate 1000 trials to have common cause Z
# DAG: X <- Z -> Y
n <- 1000
Z <- rbern(n, 0.5)  # Bernoulli trials
X <- rbern(n, (1-Z)*0.1 + Z*0.9)
Y <- rbern(n, (1-Z)*0.1 + Z*0.9)

# Associated
table(X, Y)  # Y ⊥/⊥ X
cor(X, Y)
# 0.6

# Not associated, conditioned on Z
table(X, Y, Z)  # # Y ⊥⊥ X | Z
cor(X[Z == 0], Y[Z == 0])
# -0.07
cor(X[Z == 1], Y[Z == 1])
# -0.04


# Come up with new data to show fork, but in manageable visual way
cols <- c(4, 2)
N <- 300
Z <- rbern(N)
X <- rnorm(N, 2*Z - 1)
Y <- rnorm(N, 2*Z - 1)

# Plot the results of the above
plot(X, Y, col = cols[Z+1], lwd = 3)
abline(lm(Y[Z == 1] ~ X[Z == 1]), col = 2, lwd = 3)
abline(lm(Y[Z == 0] ~ X[Z == 0]), col = 2, lwd = 3)
abline(lm(Y ~ X), lwd = 3)


# Fork Example ------------------------------------------------------------

# https://youtu.be/mBEA7PKDmiY?si=QyHV1M1IkT2ySNd1&t=707

library(rethinking)
data(WaffleDivorce)

dim(WaffleDivorce)
# [1] 50 13

names(WaffleDivorce)
#  [1] "Location"          "Loc"
#  [3] "Population"        "MedianAgeMarriage"
#  [5] "Marriage"          "Marriage.SE"
#  [7] "Divorce"           "Divorce.SE"
#  [9] "WaffleHouses"      "South"
# [11] "Slaves1860"        "Population1860"
# [13] "PropSlaves1860"

# "Why do regions of the USA with higher rates of *marriage* also have higher
# rates of *divorce*?

with(
  WaffleDivorce,
  plot(
    x = MedianAgeMarriage,
    y = Divorce,
    xlab = "Median Age of Marriage",
    ylab = "Divorce Rate"
  )
)
# Southern states have higher divorce rates.

# "Fork: M <- A -> D"
# "Break the fork by *stratifying by A"


# Prior predictive simulation ---------------------------------------------

# https://youtu.be/mBEA7PKDmiY?si=zhQW7VVhP2U5-nsy&t=1231

n <- 20
a <- rnorm(n, 0, 10)
bM <- rnorm(n, 0, 10)
bA <- rnorm(n, 0, 10)
plot(
  NULL,
  xlim = c(-2, 2),
  ylim = c(-2, 2),
  xlab = "Median age of marriage (standardized)",
  ylab = "Divorce rate (standardized)"
)
Aseq <- seq(from = -3, to = 3, len = 30)
for (i in 1:n) {
  mu <- a[i] + bA[i]*Aseq
  lines(Aseq, mu, lwd = 2, col = 2)
}
