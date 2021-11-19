rm(list=ls())

setwd("/Users/johnkalemkerian/Desktop/r-novice-inflammation/data/Biocomp-Exercise09")

###### Exercise 9 ######

## Write a function that takes a directory name as an argument called dir plus any 
## other arguments required to accomplish the specified task.
## The function should read data from each file in the specified directory and 
## calculate the coefficient of variation (standard deviation divided by the mean) for 
## a user specified column. These values should be returned as a vector.
## To calculate a reliable coefficient of variation we would like to have 
## 50 observations, but we also don’t want to force the user to use our high standard for the data. 
## Make your function, by default, report an error if any file has less than 
## 50 observations, but allow the user to override this behavior and only receive 
## a warning if 50 observations are not present in a file.
## For an extra credit point, add arguments and associated code to your 
## function to situations where a file doesn’t have the correct number of columns
## or the provided data includes NA.

##Function needs to: 
## 1 ## Define inputs (dir and what the column name is called)
## 2 ## If/else if function to ensure that intended columns have 50+ observations
## 3 ## Allow user to override 50+ observation cutoff
## 4 ## Read data from approved (50+ observation) files that have specified column name
## 5 ## Take read data and calculate coefficient of variation, returned in a placeholder vector
## 6 ## Return created vector with printed message

output <- function(dir, colName){
  files <- list.files(path = dir)
  outputvector <- c()
  for (i in 1:length(files)){
    file <- read.csv(file = files[i], header = TRUE, sep = ",")
    if (nrow(file) < 50){
      print("File has under 50 observations, so coefficient of variation may be unreliable")
      input <- readline(prompt = "Do you wish to continue? Enter 'Yes' or 'No'")
      if (input == "Yes"){
        standardDev <- sd(file$colName, na.rm = TRUE)
        meanX <- mean(file$colName, na.rm = TRUE)
        coefficientVar = standardDev / meanX
        vector <- c(outputvector, coefficientVar)
      }else{
        return("Not enough observations present for reliable coefficient of variation calculation")
      }
    }else{ ## By default, nrows >= 50
      standardDev <- sd(file$colName, na.rm = TRUE)
      meanX <- mean(file$colName, na.rm = TRUE)
      coefficientVar = standardDev / meanX
      vector <- c(outputvector, coefficientVar)
    }
  }
  print("Vector containing coefficients of variation for specified column name in all files in dir")
  return(outputvector)
}













