library(simmer)
#set.seed(1234)

# import data
G <- read.csv("C:/Users/setup/Documents/nba_game_generator_matrix_ID19.csv", stringsAsFactors=FALSE, header = TRUE, row.names = 1)

# create exit rate vector
R <- c(1:ncol(G))
for (i in 1:ncol(G)) {
  R[i] <- abs(G[i,i])*10
}

# create transition probability matrix
P <- sweep(G, 1, R, "/")
P[P < 0] <- 0

# default state - jump ball
state <- 1

# points tally
Points_away <- c()
Points_home <- c()

# function to simulate n steps of the transition matrix
simulate_dtmc = function(steps)
{
  
  # Make x steps through the markov chain
  for (i in 1:steps)
  {
    # cat("> Dist:", paste(round(c(trans[state,]), 2)), "\n");
    newState <- sample(1:ncol(P), 1, prob=P[state,]);
    exit_time <- rexp(1, abs(R[state]));
    # check if new state resulted in points scored
    
    # if contains _MADE
    if (grepl('_MADE', rownames(P)[newState])) {
      # if contains _FT_ then add 1 point
      if (grepl('_FT_', rownames(P)[newState])) {
        if (grepl('HOME_', rownames(P)[newState])) {
          Points_home <<- c(Points_home, 1)
        } else {
          Points_away <<- c(Points_away, 1)
        }
      }
      # if contains _2PT_ then add 2 points
      if (grepl('_2PT_', rownames(P)[newState])) {
        if (grepl('HOME_', rownames(P)[newState])) {
          Points_home <<- c(Points_home, 2)
        } else {
          Points_away <<- c(Points_away, 2)
        }
      }
      # if contains _3PT_ then add 3 points
      if (grepl('_3PT_', rownames(P)[newState])) {
        if (grepl('HOME_', rownames(P)[newState])) {
          Points_home <<- c(Points_home, 3)
        } else {
          Points_away <<- c(Points_away, 3)
        }
      }
    }
    
    #cat("*", rownames(P)[state], "->", rownames(P)[newState], "in", exit_time, "seconds", "\n");
    state <<- newState
  }
  return(exit_time)
}

option.3 <- function(t) {
  # reset points tally
  Points_home <<- c()
  Points_away <<- c()
  vehicle <- trajectory() %>%
    #seize("pump", amount=1) %>%
    #log_("start timeout") %>%
    timeout(function() {
      # simulate a single step
      simulate_dtmc(1)
    }) %>%
      #log_("end timeout") %>%
    rollback(2, Inf)
    #release("pump", amount=1) 
  
  simmer() %>%
    add_resource("pump", capacity=1, queue_size=0) %>%
    add_generator("vehicle", vehicle, function() {c(runif(1), -1)}, mon = 1) %>%
    run(until=t)
}

runs = 500
totals_home <- c()
totals_away <- c()
totals <- c()
for (i in 1:runs) {
  option.3(2880)
  totals_home <- c(totals_home, sum(Points_home))
  totals_away <- c(totals_away, sum(Points_away))
  totals <- c(totals, sum(Points_away)+sum(Points_home))
}
hist(totals)
summary(totals_away)
summary(totals_home)
