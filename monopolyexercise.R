library("markovchain")

rotvec <- function(vec) vec[ c(length(vec), 1:(length(vec)-1)) ]

nStates <- 8
monopolyStates <- as.character( c(1:nStates) )

diceRoll <- matrix(0, nStates, nStates)
p1 <- c(0, 1/2, 1/2, 0, 0, 0, 0, 0)

for (i in 1:nStates) {
    diceRoll[i, ] <- p1
    p1 <- rotvec(p1)
}

Jail <- diag(nStates)
Jail[4, 4] <- 0
Jail[4, 2] <- 1                       #go from Police to Jail

Ch <- diag(nStates)
Ch[6, 7] <- 1/16                         #Nearest Railroad
Ch[6, 8] <- 1/16                         #Advance to Go
Ch[6, 1] <- 1/16                         #Reading Railroad
Ch[6, 2] <- 1/16                         #Go to Jail
Ch[6, 3] <- 1/16                         #Go back 3
Ch[6, 6] <- 11/16                         #All other cards

monopMat <- diceRoll %*% Jail %*% Ch

mcMonopoly8 <- new("markovchain", states = monopolyStates, byrow = TRUE,
                  transitionMatrix = monopMat, name = "Monopoly")
ss <- steadyStates(mcMonopoly8)
cat(ss, "\n")

## barplot(ssComb[1:nStates], names.arg=1:8,
##         main="Steady State Distribution for 8-State Monopoly",
##         xlab="State", ylab="Probability")

## NAME:  monopolyexercise.R
##  R script to simulate small Monopoly as 8-state Markov chain
## USAGE: within R, at interactive prompt
##        source("monopolyexercise.R")
## REQUIRED ARGUMENTS: none
## OPTIONS: none
## DESCRIPTION: 
## Simulate Small Monopoly as 8-state Markov chain finding the steady
## state distribution with markovchain package.
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: R with markovchain package
## DEPENDENCIES:  R with markovchain package
## INCOMPATIBILITIES: none known
## PROVENANCE: Steve Dunbar, Mon 17 Aug 2020 08:38:53 AM CDT
## BUGS AND LIMITATIONS: 
## FEATURES AND POTENTIAL IMPROVEMENTS:
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 as of Mon 17 Aug 2020 08:38:53 AM CDT
## KEYWORDS: Monopoly, Markov Chain, steady state distribution, Jail


