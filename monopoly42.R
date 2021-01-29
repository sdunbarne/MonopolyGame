library("markovchain")

rotvec <- function(vec) vec[ c(length(vec), 1:(length(vec)-1)) ]

nStates <- 42
monopolyStates <- as.character( c(1:nStates) )

diceRoll <- matrix(0, (nStates-2), (nStates-2))
p1 <- c(0, 0,
        1/36, 2/36, 3/36, 4/36, 5/36, 6/36, 5/36, 4/36, 3/36, 2/36, 1/36,
        numeric((nStates-2)-13)
        )
for (i in 1:(nStates-2)) {                #regular states 1 through 40
    diceRoll[i, ] <- p1
    p1 <- rotvec(p1)
}
## Next, no probability of going from board state to extra Jail states 
diceRoll  <- cbind(diceRoll, matrix(0, (nStates-2), 2))
## add rows from extra Jail states
diceRoll  <- rbind(diceRoll, matrix(0, 2, nStates))
## fill transitions for rolling doubles to get out of Jail
diceRoll[41, 41] <- 0; diceRoll[41, 42] <- 5/6 #stay in jail one turn
diceRoll[41, c(12, 14, 16, 18, 20, 22)] <- 1/36 #roll doubles
diceRoll[42, 42] <- 0; diceRoll[42, 10] <- 5/6 #stay in Jail one more turn
diceRoll[42, c(12, 14, 16, 18, 20, 22)] <- 1/36 #roll doubles 

Jail <- diag(nStates)
Jail[30,30] <- 0
Jail[30, 41] <- 1                       #go from Police to Jail

Cc <- diag(nStates)
chestStates <- c(2, 17, 33)
chestDests <-  c(40, 41)
Cc[ cbind(chestStates, chestStates) ] <- 14/16
Cc[chestStates, chestDests] <- 1/16

Ch <- diag(nStates)
chanceStates <- c(7, 22, 36)
chanceDests <- c(5, 11, 24, 39, 40, 41)
Ch[ cbind( chanceStates, chanceStates ) ] <- 6/16
Ch[ cbind( chanceStates, chanceStates-3 ) ] <- 1/16 #Go Back 3 spaces
Ch[ chanceStates, chanceDests] <- 1/16
Ch[7,12] <- 1/16; Ch[22,28] <- 1/16; Ch[36, 12] <- 1/16 #nearest utility
Ch[7,15] <- 2/16; Ch[22,25] <- 2/16; #2 Chance advance nearest Railroad,
Ch[36, 5] <- 3/16 # 3 ways to get to Reading Railroad

monopMat <- diceRoll %*% Jail %*% Ch %*% Cc

mcMonopoly42 <- new("markovchain", states = monopolyStates, byrow = TRUE,
                  transitionMatrix = monopMat, name = "Monopoly")
ss <- steadyStates(mcMonopoly42)

ssComb  <- ss
ssComb[10]  <- ssComb[10] + ssComb[41] + ssComb[42]
ssCombTop14 <- sort(ssComb[1:40], decreasing=TRUE)[1:14]
ssCombTop14Indices <- order(ssComb[1:40], decreasing=TRUE)[1:14]
ssCombdf <- data.frame( ssCombTop14Indices, ssCombTop14)

top14ofAbbott <- (1/100)*c(10.800, 3.013, 2.949, 2.905, 2.845, 2.822, 2.821, 2.674, 2.669, 2.659, 2.659, 2.636, 2.628, 2.603)
top14ofAbbottIndices <- c(10, 24, 40, 25, 5, 20, 19, 18, 16, 28, 15, 12, 21 , 11)

maxDiff <- max( abs( ssCombTop14 - top14ofAbbott) )
cat("Maximum Difference: ", maxDiff, "\n")
maxDiffIndex <- which.max( abs( ssCombTop14 - top14ofAbbott) )
cat("Maximum Difference Index: ", maxDiffIndex, "\n")
maxRelDiff <- maxDiff/ssCombTop14[maxDiffIndex]
cat("Relative Maximum Difference: ", maxRelDiff, "\n")

barplot(ssComb[1:40], names.arg=1:40,
        main="Steady State Distribution for 42-State Monopoly, 10,41,42 Combined",
        xlab="State", ylab="Probability")

## NAME:  monopoly42.R
##  R script to simulate Monopoly as 42-state Markov chain
## USAGE: within R, at interactive prompt
##        source("monopoly42.R")
## REQUIRED ARGUMENTS: none
## OPTIONS: none
## DESCRIPTION: 
## Simulate Monopoly as 42-state Markov chain finding the steady
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
## KEYWORDS: Monopoly, Markov Chain, steady state distribution, Jail


