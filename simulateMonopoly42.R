##  This function calculates where you will land on your next move.
moveSquare <- function(current, jailTime) {
    dice <- sample(seq(1,6), 2, replace=TRUE)

    if (current == 10 & jailTime > 0) { # in Jail
        if (dice[1] == dice[2]) {           # roll doubles!
            current <- 10 + sum(dice)       # get out of Jail
            jailTime <- 0
        } else {                            #don't move but...
            jailTime <- (jailTime + 1) %% 3 # count up jail time
        } 
    } else {
        current <- current + sum(dice)      # regular roll
    }
  return(c(current, jailTime))
}

## Helper function to avoid code replication
checkState <- function(current) {
  if (current > 40) {
    current <- current - 40
  } else if (current < 1) {
    current <- current + 40
  }
  return(current)
}

## Helper function to record landings, 
updateStateVector <- function(current, jailTime, landings) {
    if (current == 10 & jailTime > 0) {
        landings[40 + jailTime] = landings[40 + jailTime] + 1
    } else {
        landings[current] <- landings[current] + 1
    }
    
    return(landings)
}

## Move according to community chest cards
## Use 16 cards, Get Out of Jail is reserved
communityChest <- function(current, jailTime) {
  u <- runif(1)
  goto <- current # Default. Do nothing
  if (u < 1 / 16) {
    goto <- 40 # Go
  } else if (u < 2 / 16) {
      goto <- 10 # Jail
      jailTime <- 1
  }
  return( c(goto, jailTime) )
}

## Move according to chance cards
## Use 16 cards, Get Out of Jail is NOT reserved
chance <- function(current, jailTime) {
  u <- runif(1)
  goto <- current # Default. Do nothing
  if (u < 1 / 16) {
      goto <- 40 # Go
  } else if (u < 2 / 16) {
      goto <- 24 # Illinois Ave.
  } else if (u < 3 / 16) {
      goto <- 11 # St. Charles Place
  } else if (u < 4 / 16) {
      goto <- 10 # Jail
      jailTime <- 1
  } else if (u < 5 /16 ) {
      goto <- 5 # Take ride on Reading
  } else if (u < 6 / 16) {
      goto <- 39 # Boardwalk
  } else if (u < 7 / 16) {
      goto <- checkState(current - 3) # Must check, since goto maybe negative!
  } else if (u < 8 / 16) {
      if (current > 28  | current < 12) { # Advance to nearest Utility
      goto <- 12 # Water Works
    } else {
      goto <- 28 # Electricity
    }
  } else if (u < 10 / 16) { # 2 cards Advance to nearest Railroad
    if (current > 35  | current < 5) { 
      goto <- 5                         # Reading
    } else if (current > 5  & current < 15) {
      goto <- 15 #Pennsylvania
    } else if (current > 15  & current < 25) {
      goto <- 25 # B & O
    } else if (current > 25  & current < 35) {
      goto <- 35 # Short Line
    }
}
  return(c(goto, jailTime))
}

## Main, record where landings occur
mySimulateMonopoly <- function (numberOfTurns) 
{
    landings <- numeric(42) # vector to hold occupancy of states,
    # 1 is Med Ave, 40 is Go
    # 41 is in Jail for one turn, 42 is Jail for two turns 
    # 10 is Just Visiting Jail or Jail for three turns, ready to get out
    jailTime <- 0
    current <- 40           # start at Go
    for (i in 1:numberOfTurns) {
        moveState <- moveSquare(current, jailTime)
        current <- moveState[1]
        jailTime <- moveState[2]
        current <- checkState(current)
        if (current == 30) {
            current <- 10
            jailTime <- 1
        }
        if (current == 7 | current == 22 | current == 36) {
            move <- chance(current, jailTime)
            current <- move[1]
            jailTime <- move[2]
        }
        if (current == 2 | current == 17 | current == 33) {
            move <- communityChest(current, jailTime)
            current <- move[1]
            jailTime <- move[2]
        }
        landings <- updateStateVector(current, jailTime, landings) # must do update
    }
return(landings)
}

n  <- 1000000
ss  <- mySimulateMonopoly(n)/n

## NAME: simulateMonopoly42.R
## Simulate the game of Monopoly, with 42-states including Jail time
## USAGE: within R, at interactive prompt
##        source("simulateMonopoly42.R")
## after that, can run ss  <- mySimulateMonopoly(n) for given n
## REQUIRED ARGUMENTS: none
## OPTIONS: none
## DESCRIPTION: 
## Simulate the game of Monopoly, with 42-states including Jail time
## DIAGNOSTICS: non
## CONFIGURATION AND ENVIRONMENT: R
## DEPENDENCIES:  R
## INCOMPATIBILITIES: none known
## PROVENANCE: Steve Dunbar, based on \booktitle{Efficient R}, Chapter 7
## by Colin Gillespie and Robin Lovelace, 
## BUGS AND LIMITATIONS: none known
## FEATURES AND POTENTIAL IMPROVEMENTS:
## AUTHOR:  Steve Dunbar, Mon 17 Aug 2020 08:51:33 AM CDT
## VERSION: Version 1.0 
## KEYWORDS: simulate, Markov chain, steady state distribution


