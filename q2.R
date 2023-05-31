# a)

# set the seed for reproducibility
set.seed(1234)

# simulate the game under each service rule
# (using a for loop that runs a total of 1000 iterations)

# Rule A: Server is the winner of the previous point
# create an empty vector to store the winning probability of Player 1 under Rule A
winning_prob_rule_A <- numeric(1000)
# repeat the simulation 1000 times
for (i in 1:1000){
  # initialize the score of the game to [0,0]
  score <- c(0,0)
  # simulate each point until one player reaches a total of two points
  while(max(score)<2){
    # if the Player 1's turn to serve
    if(sum(score)%%2==0){
      # determine whether Player 1 wins the point based on the given probability
      if(runif(1)<0.55){
        # if player 1 wins the point, increase their score by 1
        score[1] <- score[1]+1
      }
      # if player 2 wins the point, increase their score by 1
      else{
        score[2] <- score[2]+1
      }
    }
    # if the Player 2's turn to serve
    else{
      # determine whether Player 1 wins the point based on the given probability
      if (runif(1)<0.40){
        # if player 2 wins the point, increase their score by 1
        score[2] <- score[2]+1
      }
      # if player 1 wins the point, increase their score by 1
      else{
        score[1] <- score[1]+1
      }
    }
  }
  # store the winning probability of Player 1 under Rule A for this simulation
  winning_prob_rule_A[i] <- as.numeric(score[1]>score[2])
}



# Rule B: Server is the loser of the previous point
# create an empty vector to store the winning probability of Player 1 under Rule B
winning_prob_rule_B <- numeric(1000)
# repeat the simulation 1000 times
for (i in 1:1000) {
  # initialize the score of the game to [0,0]
  score <- c(0, 0)
  # simulate each point until one player reaches a total of two points
  while (max(score) < 2) {
    # if the Player 1's turn to serve
    if (sum(score) %% 2 == 0) {
      # determine whether Player 1 wins the point based on the given probability
      if (runif(1) < 0.55) {
        # if player 1 wins the point, increase their score by 1
        score[1] <- score[1] + 1
      } else {
        # if player 2 wins the point, increase their score by 1
        score[2] <- score[2] + 1
      }
    }
    # if the Player 2's turn to serve
    else {
      # determine whether Player 1 wins the point based on the given probability
      if (runif(1) < 0.60) {
        # if player 2 wins the point, increase their score by 1
        score[2] <- score[2] + 1
      } else {
        # if player 1 wins the point, increase their score by 1
        score[1] <- score[1] + 1
      }
    }
  }
  winning_prob_rule_B[i] <- as.numeric(score[1] > score[2])
}



# calculate the proportion of games won by player 1 under each service rule
mean(winning_prob_rule_A)
mean(winning_prob_rule_B)