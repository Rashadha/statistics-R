# Set the initial value of S0
S0 <- 0

# Define the probability distribution of Xn
p <- c(1/2, 1/2)
x <- c(1, -1)

# Simulate the random walk
set.seed(1234) # Set seed for reproducibility
for (i in 1:5) {
  X <- sample(x, 10, replace = TRUE, prob = p) # Generate random values of Xn
  S <- cumsum(c(S0, X))[-1] # Calculate the values of Sn using cumsum() function
  print(S)
}