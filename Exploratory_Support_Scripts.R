# Load in needed libraries
library(pacman)
p_load(ggplot2)
p_load(dplyr)
p_load(plyr)
p_load(psych)
p_load(ggpubr)
p_load(kableExtra)
p_load(corrplot)


# Helper Method:
alreadySet <- function(tempA,tempB,currVal,corVars){
  # Get current num of rows
  currNumRows <- nrow(corVars)
  
  # Check to make sure data doesn't already exist
  tempRow1 <- data.frame(varA=tempA,varB=tempB,Correlation=currVal)
  tempRow2 <- data.frame(varA=tempB,varB=tempA,Correlation=currVal)
  # print(tempRow1)
  # print(tempRow2)
  #print(dummyDF <- match_df(corVars, tempRow1))
  
  # TRUE if exists
  if(nrow(match_df(corVars, tempRow1)) != 0){
    return(TRUE)
  }
  else if(nrow(match_df(corVars, tempRow2)) != 0){
    return(TRUE)
  }
  else{
    # New Value
    #print("New Data")
    return(FALSE)
  }
  
}


# Corlation Matrix Reader:
# Threshold for what's considered highly correlated is highCor (default is 0.8)
# Matrix to be looked at
corMatrixReader <- function(highCor=0.8, dataSet){
  # get list of column names for later and setup other lists
  varList <- colnames(dataSet)
  corVars <- data.frame(varA=character(),varB=character(),Correlation=integer())
  numRows <- dim(dataSet)[1]
  numCols <- dim(dataSet)[2]
  
  # Loop through the dataset
  for(x in 1:numRows){
    for(y in 1:numCols){
      # get Current Value and see if it's worth checking.
      currVal <- dataSet[x,y]
      if(!is.na(currVal) && currVal != 1){
        
        # Check to see if this is highly correlated
        if(currVal>=highCor){
          # These two variables are highly correlated
          tempA <- varList[x]
          tempB <- varList[y]
          
          # Make sure we haven't already stored them
          if(!alreadySet(tempA,tempB,currVal,corVars)){
            
            # Put in new values
            newRow <- c(tempA, tempB, currVal)
            corVars <- rbind(corVars, newRow)
            colnames(corVars) <-  c("varA", "varB", "Correlation")
          }
        }
      }
      
    }
  }
  # Finished building Data Frame
  corVars$varA <- as.factor(corVars$varA)
  corVars$varB <- as.factor(corVars$varB)
  corVars$Correlation <- as.numeric(corVars$Correlation)
  return(corVars)
  
}

#testCor <- corMatrixReader(0.9, dataSet =  corChems)



