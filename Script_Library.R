# Library script file -----------------------------------------
# Holds many useful functions used across the project


# -------------------------------------------------------------
# Build Test Set and Training Set:
getSets <- function(dataset, propTraining = 0.9){
  # Set up some vars for getting what we need
  propTesting <- 1- propTraining
  numRows <- nrow(dataset)
  
  # Get Rows for each dataset (Test/Training)
  trainSize <- round(numRows*propTraining)
  trainRows <- sample(numRows, trainSize)
  
  # Build datasets
  trainSet <- dataset[trainRows,]
  testSet  <- dataset[-trainRows,]
  
  # Write out to File
  curTime <- format(Sys.time(), "%b %d %Y")
  trainFileName <- paste("Training Set", propTraining, curTime, sep="_")
  trainFileName <- paste(trainFileName, "csv", sep=".")
  testFileName  <- paste("Testing Set", propTesting, curTime, sep="_")
  testFileName  <- paste(testFileName, "csv", sep=".")
  pathOut <- "C:\\Users\\aknit\\Documents\\Grad School\\Capstone\\Data\\"
  
  
  write.csv(trainSet, paste(pathOut, trainFileName, sep=""), row.names = FALSE)
  write.csv(testSet, paste(pathOut, testFileName, sep=""), row.names = FALSE)
  
}

# -------------------------------------------------------------
# Cross Validation:

cost.fn <- function(tol = 0.5, obs.response, fitted.probability) {
  
  t <- tibble(obs = obs.response, prob = fitted.probability.)
  # count number of false positives
  n.fp <- t %>%
    filter(obs == 0 & prob > tol) %>%
    nrow()
  # false negatives
  n.fn <- t %>%
    filter(obs == 1 & prob < tol) %>%
    nrow()
  return(error.rate <- (n.fp + n.fn)/nrow(t))
}


# TODO: Fix
roughPerformanceTest <- function(testSet, dependentVar, givenModel){
  
  testSet <- na.omit(testSet)
  
  # Make predictions
  probabilities <- predict(givenModel, testSet, type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
  # Prediction accuracy
  observed.classes <- testSet[, dependentVar]
  avgAcc <- mean(predicted.classes == observed.classes)
  
  return(avgAcc)
  
}





