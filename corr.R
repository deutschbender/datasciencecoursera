corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
}

getwd()
setwd("C:/users/Julien/Desktop/Temp/R")
?cor

testCorra <- read.csv("C:/Users/Julien/Desktop/Temp/R/specdata/001.csv")

testCorraNoNa <- subset(testCorra, complete.cases(testCorra))
cor(testCorraNoNa$sulfate, testCorraNoNa$nitrate)
summary()

filesList <- list.files("specdata", full.name= T)

corr <- function(directory, threshold = 0) {
  filesList <- list.files("specdata", full.name= T)
  id <- seq(1:length(filesList))
  completeLines <- data.frame()
  for (i in id) {
    completeLines <- rbind( completeLines, subset( read.csv ( filesList[i]), complete.cases(read.csv(filesList[i]))))
      if (nrow(completeLines) > threshold) { 
          correlation <- cor(completeLines$nitrate, completeLines$sulfate)
      } 
    else {
      NA
    }
  }
  
}

corr(specdata,300)
