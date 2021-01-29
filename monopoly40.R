library("markovchain")

rotvec <- function(vec) vec[ c(length(vec), 1:(length(vec)-1)) ]

nStates <- 40
monopolyStates <- as.character( c(1:nStates) )

diceRoll <- matrix(0, nStates, nStates)
p1 <- c(0, 0,
        1/36, 2/36, 3/36, 4/36, 5/36, 6/36, 5/36, 4/36, 3/36, 2/36, 1/36,
        numeric(nStates-13)
        )
for (i in 1:nStates) {
    diceRoll[i, ] <- p1
    p1 <- rotvec(p1)
}

Jail <- diag(nStates)
Jail[30,30] <- 0
Jail[30, 10] <- 1

Ch <- diag(nStates)
chanceStates <- c(7, 22, 36)
chanceDests <- c(5, 10, 11, 24, 39, 40)
Ch[ cbind( chanceStates, chanceStates ) ] <- 6/16
Ch[ cbind( chanceStates, chanceStates-3 ) ] <- 1/16 #Go Back 3 spaces
Ch[ chanceStates, chanceDests] <- 1/16
Ch[7,12] <- 1/16; Ch[22,28] <- 1/16; Ch[36, 12] <- 1/16 #nearest utility
Ch[7,15] <- 2/16; Ch[22,25] <- 2/16; #2 Chance advance nearest Railroad,
Ch[36, 5] <- 3/16 # 3 ways to get to Reading Railroad

Cc <- diag(nStates)
chestStates <- c(2, 17, 33)
chestDests <-  c(10, 40)
Cc[ cbind(chestStates, chestStates) ] <- 14/16
Cc[chestStates, chestDests] <- 1/16

monopolyMatrix <- diceRoll %*% Jail %*% Ch %*% Cc

mcMonopoly <- new("markovchain", states = monopolyStates, byrow = TRUE,
                  transitionMatrix = monopolyMatrix, name = "Monopoly")
ss <- steadyStates(mcMonopoly)

top14ofAbbott <- (1/100)*c(5.896, 3.188, 3.114, 3.071, 3.064, 2.993, 2.919, 2.917, 2.875, 2.830, 2.811, 2.777, 2.739, 2.736)
top14ofAbbottIndices <- c(10, 24, 40, 19, 25, 5, 15, 18, 20, 21, 28, 16, 23, 11)

sstop14 <- sort(ss, decreasing=TRUE)[1:14]
sstop14Indices <- order(ss, decreasing=TRUE)[1:14]

maxDiff <- max( abs( sstop14 - top14ofAbbott) )
cat("Maximum Difference: ", maxDiff, "\n")
maxDiffIndex <- which.max( abs( sstop14 - top14ofAbbott) )
cat("Maximum Difference Index: ", maxDiffIndex, "\n")
maxRelDiff <- maxDiff/sstop14[maxDiffIndex]
cat("Relative Maximum Difference: ", maxRelDiff, "\n")

ssdf <- data.frame( sstop14Indices, sstop14)

barplot(ss, names.arg=1:40,
        main="Steady State Distribution for 40-State Monopoly",
        xlab="State", ylab="Probability")


## NAME:  monopoly40.R
##  R script to simulate Monopoly as 40-state Markov chain
## USAGE: within R, at interactive prompt
##        source("monopoly40.R")
## REQUIRED ARGUMENTS: none
## OPTIONS: none
## DESCRIPTION: 
## Simulate Monopoly as 40-state Markov chain finding the steady
## state distribution with markovchain package and comparing to
## results of Abbot and Richey
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: R with markovchain package
## DEPENDENCIES:  R with markovchain package
## INCOMPATIBILITIES: none known
## PROVENANCE: Steve Dunbar, Mon 17 Aug 2020 08:38:53 AM CDT
## BUGS AND LIMITATIONS: 
## FEATURES AND POTENTIAL IMPROVEMENTS:
## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 as of Mon 17 Aug 2020 08:38:53 AM CDT
## KEYWORDS: Monopoly, Markov Chain, steady state distribution


