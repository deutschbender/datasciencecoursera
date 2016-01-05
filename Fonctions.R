

## chargement d'un dataset

data001 <- read.csv("specdata/001.csv")

## Liste des fichiers du répertoire
filesList <- list.files("specdata", full.name = T)
filesList[2]

## Definition de la variable pollutant
pollutant <- c("sulfate")
ids <- c(2)

## Sélection des lignes des indicateurs de pollutant
pollutantLines2 <- subset(read.csv(filesList[2]), select = pollutant)
pollutantLines3 <- subset(read.csv("specdata/002.csv"), select = pollutant)

## Test de moyenne sur les subset
mean(pollutantLines2[ ,"sulfate"], na.rm = T)

pollutantLines <- rbind(pollutantLines3, pollutantLines2)


pollutantmean <- function(directory, pollutant, id = 1:332) {
  filesList <- list.files("specdata", full.name= T)
  pollutantLines <- data.frame()
    for (i in id) {
      pollutantLines <- rbind(pollutantLines, read.csv(filesList[i]))
     
    }
  mean(pollutantLines[ , pollutant], na.rm = T)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)

columnmean <- function (x, removeNA = TRUE) {
  nc <- ncol(x)
  means <- numeric(nc)
  for (i in 1:nc){
    means[i] <- mean(x[,i], na.rm = removeNA)
    
  }
  means
}

completeLines <- read.csv(filesList[2])

LinesNoNa <- subset(completeLines, is.na(completeLines$nitrate) != TRUE & is.na(completeLines$sulfate) != TRUE)

head(LinesNoNa)
aggregate(LinesNoNa, by = "ID", sum)

is.na(completeLines$sulfate)

filesList[2]
is.na(completeLines [,2:3])

nrow(is.na(completeLines [,2:3]))

complete <- function(directory, id = 1:332) {
  
  
  
  ## 'directory' is a character vector of length 1 indicating

  
  
    ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  ## paramétrate du WD
  setwd("C:/Users/Julien/desktop/Temp/R")
  getwd()
  
  ## chargement d'un dataset
  data001 <- read.csv("specdata/001.csv")
  
  ## Liste des fichiers du répertoire
  filesList <- list.files("specdata", full.name = T)
  filesList[2]
  
  ## Sélection des lignes des indicateurs de pollutant
  pollutantLines2 <- subset(read.csv(filesList[2]), select = pollutant)
  pollutantLines3 <- subset(read.csv("specdata/002.csv"), select = pollutant)
  
  ## Test de moyenne sur les subset
  mean(pollutantLines2[ ,"sulfate"], na.rm = T)
  
  ## Creation du data.frames sur les subset
  pollutantLines <- rbind(pollutantLines3, pollutantLines2)
  
  
  pollutantmean <- function(directory, pollutant, id = 1:332) {
    filesList <- list.files("specdata", full.name= T)
    pollutantLines <- data.frame()
    for (i in id) {
      pollutantLines <- rbind(pollutantLines, read.csv(filesList[i]))
      
    }
    mean(pollutantLines[ , pollutant], na.rm = T)
  }
  
  pollutantmean("specdata", "sulfate", 1:10)
  pollutantmean("specdata", "nitrate", 70:72)
  
  columnmean <- function (x, removeNA = TRUE) {
    nc <- ncol(x)
    means <- numeric(nc)
    for (i in 1:nc){
      means[i] <- mean(x[,i], na.rm = removeNA)
      
    }
    means
  }
  
  
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
  
  complete("specdata", 1)
  complete("specdata", c(2, 4, 8, 10, 12))
  complete("specdata", 30:25)
  
  
  
  df_ID <- data.frame(ID = id)
  df_ID 
  
  filesList <- list.files("specdata", full.name= T)
  id <- 30:28
  df_ID <- data.frame(ID = id, line= c(1:length(id)))
  completeLines <- data.frame()
  completeLines <- rbind( completeLines, subset( read.csv ( filesList[30]), complete.cases(read.csv(filesList[30]))))
  completeLines <- rbind( completeLines, subset( read.csv ( filesList[29]), complete.cases(read.csv(filesList[29]))))
  completeLines <- rbind( completeLines, subset( read.csv ( filesList[28]), complete.cases(read.csv(filesList[28]))))
  df_complete <- aggregate.data.frame(completeLines, by = list("ID" = completeLines$ID), length)
  df_Temp <- merge(df_ID, df_complete)
  df_Temp[ order(df_Temp[,"line"]), ]
  df_Final <- data.frame(ID = df_Temp[,"ID"], nobs= c(df_Temp[,"Date"]))
  print(df_Final)
  
  
  list("IDJoin" = c(id))
  IDJoin
  df_complete
  id
  df_ID <- data.frame(IDJoin = id, line= c(1:length(id)))
  df_ID
  
  complete2 <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    filesList <- list.files("specdata", full.name= T)
    for (i in id) {
      completeLines[i] <- subset(read.csv(filesList[i]), is.na(read.csv(filesList[i])[,"sulfate"]) != TRUE & is.na(read.csv(filesList[i])[,"sulfate"]) != TRUE)
    }
  }
  
  
  complete2("specdata", 1)
  
  completeLines <- read.csv(filesList[2])
  complete.cases(completeLines)
  
  completeLines <- subset(read.csv(filesList[2]), complete.cases(read.csv(filesList[2])))
  
  str(completeLines)
  
  LinesNoNa <- 
    head(LinesNoNa)
  tail(LinesNoNa)
  nrow(LinesNoNa)
  
  is.na(read.csv(filesList[2])[,c("sulfate","nitrate")])
  
  dfTest <- aggregate(completeLines, by = list("ID" = completeLines$ID), length)
  
  dfFinal <- data.frame(Id = c(dfTest[,1]), nobs= c(dfTest[,2]))  
  dfFinal 
  countLine <- aggregate(completeLines$Data, by = list(ID = completeLines$ID), count.fields())
  countLine
  
  
  
  
  
