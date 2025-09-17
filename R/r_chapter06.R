# R code 6.1 --------------------------------------------------------------

set.seed(1914)

N <- 200  # Num of grant proposals
p <- 0.1  # Proportion to select

# Uncorrelated newsworthiness and truthworthiness
nw <- rnorm(N)
tw <- rnorm(N)

# Select top 10% of combined scores
s <- nw + tw  # Total score
q <- quantile(s, 1-p)  # Top 10% threshold

selected <- ifelse(s >= q, TRUE, FALSE)
cor(tw[selected], nw[selected])
# [1] -0.7680083


# R code 6.30 -------------------------------------------------------------

library(dagitty)

dag_6_2 <- dagitty("dag {
  A -> D
  A -> M -> D
  A -> M -> D
  A <- S -> M
  S -> W -> D
}")
drawdag(dag_6_2)
adjustmentSets(dag_6_2, exposure = "W", outcome = "D")
# { A, M }
# { S }
# Control for either A and M, or for S alone