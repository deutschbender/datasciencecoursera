complete <- function(directory, id = 1:332) {
  filesList <- list.files("specdata", full.name= T)
  completeLines <- data.frame()
  for (i in id) {
    completeLines <- rbind( completeLines, subset( read.csv ( filesList[i]), complete.cases(read.csv(filesList[i]))))
  }
  df_complete <- aggregate(completeLines, by = list("ID" = completeLines$ID), length)
  dfFinal <- data.frame(ID = c(df_complete[,1]), nobs= c(df_complete[,2]))
  print(dfFinal)
}

complete(specdata, 10:5)
