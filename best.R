setwd("C:/Users/Julien/Desktop/Mooc/Coursera - R Programming/assignments")

best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeDataClean <- outcomeData[, c(2,7, 11, 17, 23)]
  colnames(outcomeDataClean) <- c("Hospital Name","State","heart attack", "heart failure","pneumonia")
  ## Check that state and outcome are valid
  validOutcome <- c("heart attack", "heart failure","pneumonia")
  if (nrow(subset(outcomeData, outcomeData$State == state)) == 0) { stop("invalid state") }
  if (is.na(match(outcome, validOutcome))) { stop("invalid outcome") }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  stateOutcomeDataClean <- subset(outcomeDataClean, outcomeDataClean$State == state, 
                                  select = c("Hospital Name", "State", outcome))
  sortedOutcome <- stateOutcomeDataClean[order
                                         (suppressWarnings(as.numeric(stateOutcomeDataClean[,3])),
                                         stateOutcomeDataClean[,1]),]
  head(sortedOutcome[,1],1)
}


