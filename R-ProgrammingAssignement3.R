setwd("C:/Users/Julien/Desktop/Mooc/Coursera - R Programming/assignments")


outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcomeDataClean <- outcomeData[, c(2,7, 11, 17, 23)]
colnames(outcomeDataClean) <- c("Hospital Name","State","heart attack", "heart failure","pneumonia")
outcomeDataClean[, 3] <- as.numeric(outcomeDataClean[, 3])
outcomeDataClean[, 4] <- as.numeric(outcomeDataClean[, 4])
outcomeDataClean[, 5] <- as.numeric(outcomeDataClean[, 5])


nrow(stateOutcomeDataClean)
stateOutcomeDataClean<- stateOutcomeDataClean [!is.na(stateOutcomeDataClean[,3]), ]
testOrdre <- stateOutcomeDataClean[order(stateOutcomeDataClean[,3],stateOutcomeDataClean[,1]),]
testOrdre$Rank <- ave( testOrdre[,3], testOrdre$State, FUN=rank )
testOrdre[5,1]

which.max(stateOutcomeDataClean[,3])
state <- "TX"
outcome <- "heart attack"
validOutcome <- c("heart attack", "heart failure","pneumonia")
best <- "bleur"
is.numeric(best)

if (!is.numeric(best) == TRUE) { print("best n'est pas numérique")}




## test allrank
outcome <- "heart attack"
num <- 5

outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

statedf <- as.data.frame(unique(outcomeData$State), row.names = NULL)
statedf <- as.data.frame(statedf[order(statedf[,1]), ])
colnames(statedf) <- c("State")

outcomeDataClean[5,2]

nrow(statedf)


statedf[5,1]

outcomeDataClean <- outcomeData[, c(2,7, 11, 17, 23)]
colnames(outcomeDataClean) <- c("Hospital Name","State","heart attack", "heart failure","pneumonia")
outcomeDataClean[, 3] <- suppressWarnings(as.numeric(outcomeDataClean[, 3]))
outcomeDataClean[, 4] <- suppressWarnings(as.numeric(outcomeDataClean[, 4]))
outcomeDataClean[, 5] <- suppressWarnings(as.numeric(outcomeDataClean[, 5]))

allOutcomeDataClean <- subset(outcomeDataClean, select = c("Hospital Name", "State", outcome))
allOutcomeDataClean <- subset(allOutcomeDataClean, complete.cases(allOutcomeDataClean[, 3]))
sortedAllOutcome <- allOutcomeDataClean[order(allOutcomeDataClean[,2], allOutcomeDataClean[,3], allOutcomeDataClean[,1]),]
dfFinal <- data.frame()

for (i in 1:nrow(statedf)) { 
  stateData <- subset(sortedAllOutcome, State == statedf[i])
  if (num == "best") { num <- 1 }
  else if (num == "worst") {  num <- nrow(stateData) }
  else if (!is.numeric(num) == TRUE) {stop ("invalid rank")}
  else { num <- num } 
  dfFinal <- rbind(dfFinal, stateData[num,1:2])
  }

## Check that state and outcome are valid

validOutcome <- c("heart attack", "heart failure","pneumonia")
if (is.na(match(outcome, validOutcome))) { stop("invalid outcome") }
## For each state, find the hospital of the given rank

statedf$maxRank <- tapply(allOutcomeDataClean[,3], allOutcomeDataClean$State, which.max)
statedf$minRank <- tapply(allOutcomeDataClean[,3], allOutcomeDataClean$State, which.min)




which.max(allOutcomeDataClean[,3], 


if (num == "best") { 
    num <- 1 
    allOutcomeDataClean$Ranking <- ave(allOutcomeDataClean[, 3], allOutcomeDataClean[, 2], FUN=rank)
    }
else if (num == "worst") { 
    num <- 1 
    allOutcomeDataClean$Ranking <- ave(-allOutcomeDataClean[, 3], allOutcomeDataClean[, 2], FUN=rank)
    }
  
else if (!is.numeric(num) == TRUE) {stop ("invalid rank")}
else { num <- num 
      allOutcomeDataClean$Ranking <- ave(allOutcomeDataClean[, 3], allOutcomeDataClean[, 2], FUN=rank)
      } 

  
  rankedOutcome <- allOutcomeDataClean[allOutcomeDataClean$Ranking == num, 1:2]
  finaldf <- merge(statedf, rankedOutcome, by = "State", all.x = TRUE, all.y = FALSE)

  
  ### PRogramming Assignemetn 3 - Rankall test
  
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeDataClean <- outcomeData[, c(2,7, 11, 17, 23)]
  colnames(outcomeDataClean) <- c("Hospital Name","State","heart attack", "heart failure","pneumonia")
  outcomeDataClean[, 3] <- suppressWarnings(as.numeric(outcomeDataClean[, 3]))
  outcomeDataClean[, 4] <- suppressWarnings(as.numeric(outcomeDataClean[, 4]))
  outcomeDataClean[, 5] <- suppressWarnings(as.numeric(outcomeDataClean[, 5]))
  
  ## Check that state and outcome are valid
  validOutcome <- c("heart attack", "heart failure","pneumonia")
  if (is.na(match(outcome, validOutcome))) { stop("invalid outcome") }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  statedf <- as.data.frame(unique(outcomeData$State), row.names = NULL)
  statedf <- as.data.frame(statedf[order(statedf[,1]), ])
  colnames(statedf) <- c("State")
  row.names(statedf) <- statedf$State
  
  allOutcomeDataClean <- subset(outcomeDataClean, select = c("Hospital Name", "State", "pneumonia"))
  allOutcomeDataClean <- subset(allOutcomeDataClean, complete.cases(allOutcomeDataClean[, 3]))
  dfFinal <- data.frame()
  
  for (i in 1:nrow(statedf)) { 
    stateData <- subset(allOutcomeDataClean, State == statedf[52,1])
    stateData <- stateData[order(stateData[,3], stateData[,1]),]
    if (num == "best") { num <- 1}
    else if (num == "worst") { num <- which.max(stateData[,3]) }
    else if (!is.numeric(num) == TRUE) {stop ("invalid rank")}
    else { num <- num } 
    dfFinal <- rbind(dfFinal, stateData[num,1:2])
  }
  
  finalDf <- merge(statedf, dfFinal, by = "State", all.x = T)
  }

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)

