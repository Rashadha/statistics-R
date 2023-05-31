states <- c("hot", "cold")

P <- matrix(c(1/4, 3/4, 
              1/3, 2/3),
            nrow = 2, ncol=2, byrow = TRUE, 
            dimnames=list(states,states))
P

# Define the emission probabilities for each state and footwear option
emissions <- matrix(c(1/2, 1/2, 0, 0.2, 0, 0.8), nrow = 2, ncol=3, byrow = TRUE,
                    dimnames = list(states, c("sandals", "flip flops", "boots")))
emissions

# Define the observed footwear sequence
observed <- c("sandals", "sandals")

# Initialize the forward probabilities for time t=1
alpha <- matrix(0, nrow = length(states), ncol = length(observed),
                dimnames = list(states, observed))
alpha

for (i in 1:length(states)) {
  alpha[i,1] <- emissions[states[i], observed[1]]/sum(emissions[states[i],])
}
alpha

# Compute the forward probabilities for time t=2
for (t in 2:length(observed)) {
  for (j in 1:length(states)) {
    alpha[j,t] <- emissions[states[j], observed[t]] %*% (P[,j] %*% alpha[,t-1])
  }
}
alpha

# Sum over all possible combinations of footwear to get the probability of John wearing sandals both today and tomorrow
prob <- sum(alpha["cold",2] * emissions["cold","sandals"] * P["cold","hot"] * emissions["hot","sandals"])
prob
