# Set the number of simulations
n <- 1000

# Define the function to simulate a game under service rule A
simulate_game_A <- function() {
  score_p1 <- 0
  score_p2 <- 0
  while (TRUE) {
    if (score_p1 == 2 || score_p2 == 2) {
      break
    }
    if (score_p1 == 0) {
      if (runif(1) < 0.55) {
        score_p1 <- score_p1 + 1
      } else {
        score_p2 <- score_p2 + 1
      }
    } else {
      if (runif(1) < 0.55) {
        score_p1 <- score_p1 + 1
      } else {
        score_p2 <- score_p2 + 1
        if (score_p2 == 2) {
          break
        }
        score_p1 <- 0
      }
    }
  }
  if (score_p1 == 2) {
    return("P1")
  } else {
    return("P2")
  }
}

# Define the function to simulate a game under service rule B
simulate_game_B <- function() {
  score_p1 <- 0
  score_p2 <- 0
  while (TRUE) {
    if (score_p1 == 2 || score_p2 == 2) {
      break
    }
    if (score_p1 == 0) {
      if (runif(1) < 0.55) {
        score_p1 <- score_p1 + 1
      } else {
        score_p2 <- score_p2 + 1
      }
    } else {
      if (runif(1) < 0.4) {
        score_p1 <- score_p1 + 1
      } else {
        score_p2 <- score_p2 + 1
        if (score_p2 == 2) {
          break
        }
        score_p1 <- 0
      }
    }
  }
  if (score_p1 == 2) {
    return("P1")
  } else {
    return("P2")
  }
}

# Simulate the games under service rule A
results_A <- replicate(n, simulate_game_A())
win_prob_A <- sum(results_A == "P1") / n
expected_length_A <- mean(rowSums(table(results_A)))

# Simulate the games under service rule B
results_B <- replicate(n, simulate_game_B())
win_prob_B <- sum(results_B == "P1") / n
expected_length_B <- mean(rowSums(table(results_B)))

# Print the results
cat("Winning probability under service rule A:", round(win_prob_A, 3), "\n")
cat("Expected length of a game under service rule A:", round(expected_length_A, 3), "\n")
cat("Winning probability under service rule B:", round(win_prob_B, 3), "\n")
cat("Expected length of a game under service rule B:", round(expected_length_B, 3), "\n")
