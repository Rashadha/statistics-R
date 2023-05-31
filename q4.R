set.seed(1234)

# Define transition probability matrix
P <- matrix(c(0.1, 0.2, 0.7,
              0.2, 0.4, 0.4,
              0.1, 0.3, 0.6), nrow=3, ncol=3, byrow=TRUE,
            dimnames=list(c("A","B","C"),c("A","B","C")))

# Define initial state
state <- "A"

# Simulate beer purchases
n_weeks <- 10
n_sims <- 5
beer_purchases <- matrix("", nrow=n_sims, ncol=n_weeks)
for (i in 1:n_sims) {
  state <- "A"
  for (j in 1:n_weeks) {
    beer_purchases[i,j] <- state
    state <- sample(c("A", "B", "C"), size=1, prob=P[which(rownames(P) == state),])
  }
}

beer_purchases



# Calculate the probability of buying beer brand A again in the fifth week
prob <- P %*% P %*% P %*% P %*% P
prob
prob <- P["A", ] %*% P %*% P %*% P %*% P[, "A"]

# Print the result
cat("The probability of buying beer brand A in first week and again buy the same brand in fifth week is\n", prob)