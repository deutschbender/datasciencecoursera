pollutantmean <- function(directory, pollutant, id = 1:332) {
  filesList <- list.files("specdata", full.name= T)
  pollutantLines <- data.frame()
  for (i in id) {
    pollutantLines <- rbind(pollutantLines, read.csv(filesList[i]))
    
  }
  mean(pollutantLines[ , pollutant], na.rm = T)
}
