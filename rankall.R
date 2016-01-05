rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeDataClean <- outcomeData[, c(2,7, 11, 17, 23)]
  colnames(outcomeDataClean) <- c("hospital","state","heart attack", "heart failure","pneumonia")
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
  colnames(statedf) <- c("state")
  row.names(statedf) <- statedf$state
  
  allOutcomeDataClean <- subset(outcomeDataClean, select = c("hospital", "state", outcome))
  allOutcomeDataClean <- subset(allOutcomeDataClean, complete.cases(allOutcomeDataClean[, 3]))
  dfFinal <- data.frame()
  
  for (i in 1:nrow(statedf)) { 
    stateData <- subset(allOutcomeDataClean, state == statedf[i,1])
    stateData <- stateData[order(stateData[,3], stateData[,1]),]
    rank <- num
    if (rank == "best") { rank <- 1}
    else if (rank == "worst") { rank <- which.max(stateData[,3]) }
    else { rank <- rank } 
    dfFinal <- rbind(dfFinal, stateData[rank,1:2])
  }
  finalDf <- merge(statedf, dfFinal, by = "state", all.x = T)
  row.names(finalDf) <- statedf$State
  finalDf
}