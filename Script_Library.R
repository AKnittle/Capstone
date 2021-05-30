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

# cost.fn <- function(tol = 0.5, obs.response, fitted.probability) {
#   
#   t <- tibble(obs = obs.response, prob = fitted.probability.)
#   # count number of false positives
#   n.fp <- t %>%
#     filter(obs == 0 & prob > tol) %>%
#     nrow()
#   # false negatives
#   n.fn <- t %>%
#     filter(obs == 1 & prob < tol) %>%
#     nrow()
#   return(error.rate <- (n.fp + n.fn)/nrow(t))
# }




# ---------------------------------------------------------------------
cost.fn <- function(obs.response, fitted.probability.) {
  tol=0.5
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
# ---------------------------------------------------------------------

error.rater <- function(trainSet, model.fit, givenK=10) {
  cv.glm(trainSet, glmfit = model.fit, cost = cost.fn, K = givenK)$delta[1]
}
# ---------------------------------------------------------------------

# Confusion Matrix Builder that takes in Observed Values, Fitted Values, and Tolerance
confusionBuilder <- function(obs.response, fitted.probability, passedTolerance) {
  t <- tibble(obs = obs.response, prob = fitted.probability)
  # count number of false positives
  n.fp <- t %>%
    filter(obs == 0 & prob > passedTolerance) %>%
    nrow()
  # false negatives
  n.fn <- t %>%
    filter(obs == 1 & prob < passedTolerance) %>%
    nrow()
  # true positives
  n.tp <- t %>%
    filter(obs == 1 & prob > passedTolerance) %>%
    nrow()
  # true negatives
  n.tn <- t %>%
    filter(obs == 0 & prob < passedTolerance) %>%
    nrow()
  Observed.NO <- c(n.tn, n.fp) # True Negative, False Positive
  Observed.YES <- c(n.fn, n.tp) # False Negative, True Positive
  confusionDF <- data.frame(Observed.NO, Observed.YES, row.names = c("Predicted.NO", "Predicted.YES"))
  
  # error rate
  Error.Rate <- (n.fp + n.fn)/nrow(t)
  # false positive rate
  False.Positive.Rate <- n.fp/(n.tn+n.fp)
  # false negative rate
  False.Negative.Rate <- n.fn/(n.fn+n.tp)
  RatesDF <- data.frame(Error.Rate, False.Positive.Rate, False.Negative.Rate)
  return(list(confusionDF, RatesDF))
}


# Rough Performance Tester
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





