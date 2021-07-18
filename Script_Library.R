# Library script file -----------------------------------------
# Holds many useful functions used across the project


# -------------------------------------------------------------
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
# -------------------------------------------------------------
# Cross Validation:

# ----------------------------------------------

# Cross Validation just for LDA
# TODO: This is likely deprecated code. Refactor and use above methods
lda.k.fold.validator2 <- function(df, dependentVar, K) {
  
  # this function calculates the errors of a single fold using the fold as the holdout data
  fold.errors <- function(df, holdout.indices) {
    formulaGiven <- as.formula(paste(dependentVar, "~ ."))
    train.data <- df[-holdout.indices, ]
    holdout.data <- df[holdout.indices, ]
    fit <- lda(formulaGiven, data = train.data)
    train.predict <- predict(fit) 
    train.error <- mean(train.data[,dependentVar] != train.predict$class)
    holdout.predict <- predict(fit, newdata = holdout.data)
    holdout.error <- mean(holdout.data[,dependentVar] != holdout.predict$class)
    return(tibble(train.error = train.error, valid.error = holdout.error))
  }
  # shuffle the data and create the folds
  indices <- sample(1:nrow(df))
  # if argument K == 1 we want to do LOOCV
  if (K == 1) {
    K <- nrow(df)
  }
  folds <- cut(indices, breaks = K, labels = F)
  # set error to 0 to begin accumulation of fold error rates
  errors <- tibble()
  # iterate on the number of folds
  for (i in 1:K) {
    holdout.indices <- which(folds == i, arr.ind = T)
    folded.errors <- fold.errors(df, holdout.indices)
    errors <- bind_rows(errors, folded.errors)
  }
  errors %>%
    summarize(train.error = mean(train.error), valid.error = mean(valid.error))
}




# ---------------------------------------------------------------------

# IMPORTANT: Used for Probabilities
# Confusion Matrix Builder that takes in Observed Values, Fitted Values (probabilities), and Tolerance
# Reminder: https://i.pinimg.com/originals/18/7f/82/187f82e15145fdce5e09059eebc92b34.png
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
  False.Positive.Rate <- n.fp/(n.tn+n.fp) # Ideal is close to 0
  # false negative rate
  False.Negative.Rate <- n.fn/(n.fn+n.tp) # Ideal is close to 0
  
  # true positive rate
  True.Positive.Rate <- n.tp/(n.tp+n.fp) # Ideal is close to 1
  # true negative rate
  True.Negative.Rate <- n.tn/(n.tn+n.fn) # Ideal is close to 1
  
  RatesDF <- data.frame(Error.Rate, False.Positive.Rate, False.Negative.Rate, True.Positive.Rate, True.Negative.Rate)
  return(list(confusionDF, RatesDF))
}

# IMPORTANT: Used for that return classified values, NOT probabilities
# Confusion Matrix Builder that takes in Observed Values, Fitted Values
# Reminder: https://i.pinimg.com/originals/18/7f/82/187f82e15145fdce5e09059eebc92b34.png
rawConfusionBuilder <- function(obs.response, fitted.response) {
  t <- tibble(obs = obs.response, prob = fitted.response)
  # count number of false positives
  n.fp <- t %>%
    filter(obs == 0 & prob == 1) %>%
    nrow()
  # false negatives
  n.fn <- t %>%
    filter(obs == 1 & prob == 0) %>%
    nrow()
  # true positives
  n.tp <- t %>%
    filter(obs == 1 & prob == 1) %>%
    nrow()
  # true negatives
  n.tn <- t %>%
    filter(obs == 0 & prob == 0) %>%
    nrow()
  Observed.NO <- c(n.tn, n.fp) # True Negative, False Positive
  Observed.YES <- c(n.fn, n.tp) # False Negative, True Positive
  confusionDF <- data.frame(Observed.NO, Observed.YES, row.names = c("Predicted.NO", "Predicted.YES"))
  
  # error rate
  Error.Rate <- (n.fp + n.fn)/nrow(t)
  
  # false positive rate
  False.Positive.Rate <- n.fp/(n.tn+n.fp) # Ideal is close to 0
  # false negative rate
  False.Negative.Rate <- n.fn/(n.fn+n.tp) # Ideal is close to 0
  
  # true positive rate
  True.Positive.Rate <- n.tp/(n.tp+n.fp) # Ideal is close to 1
  # true negative rate
  True.Negative.Rate <- n.tn/(n.tn+n.fn) # Ideal is close to 1
  
  RatesDF <- data.frame(Error.Rate, False.Positive.Rate, False.Negative.Rate, True.Positive.Rate, True.Negative.Rate)
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





# -------------------------------------------------------------
# -------------------------------------------------------------
# ROC and AUC:
#https://developers.google.com/machine-learning/crash-course/classification/roc-and-auc
#https://www.youtube.com/watch?v=qcvAqAH60Yw&ab_channel=StatQuestwithJoshStarmer


# Builds a ROC result by calling the Confusion Matrix Builder
rocBuilder <- function(obs.response, fitted.probability, toleranceVec=seq(0,1,0.1)){
  
  # Initialize empty Data Frame
  rocDF <- data.frame(matrix(ncol = 4, nrow = 0))

  # Loop through tolerances
  for(tValue in toleranceVec){
    # Build new row for rocDF
    currErrorResult <- confusionBuilder(obs.response, fitted.probability, tValue)
    errorDF <- currErrorResult[[2]]
    rateVec <- c(errorDF$Error.Rate, errorDF$True.Positive.Rate, errorDF$False.Positive.Rate, tValue)
    rocDF <- rbind(rocDF, rateVec)
  }
  
  cNames <- c("Error Rate", "True Positive Rate", "False Positive Rate", "Tolerance")
  colnames(rocDF) <- cNames
  
  rocPlot <-ggplot(rocDF) +
    aes(
      x = `False Positive Rate`,
      y = `True Positive Rate`,
      colour = `Error Rate`,
      size = Tolerance
    ) +
    geom_point(shape = "circle") +
    scale_color_viridis_c(option = "plasma", direction = 1) +
    theme_minimal()
  
  ggplot(rocDF) +
    aes(
      x = `False Positive Rate`,
      y = `True Positive Rate`,
      colour = `Error Rate`
    ) +
    geom_line(size = 0.85) +
    scale_color_viridis_c(option = "plasma", direction = 1) +
    theme_minimal()
  
  return(list(rocDF, rocPlot))
}


# Returns the AUC 
simpleAUC <- function(truePositiveR, falsePositiveR){
  
  # inputs already sorted, best scores first 
  dFPR <- c(diff(falsePositiveR), 0)
  dTPR <- c(diff(truePositiveR), 0)
  return(abs(sum(truePositiveR * dFPR) + sum(dTPR * dFPR)/2))

}

# Uses libraries to build ROC and AUC... not that I don't trust my own builders
rocByLibrary <- function(model, data, dependentVar){
  
  predictedVals <- predict(model,data,type="prob")
  classes <- levels(dependentVar)
  true_values <- ifelse(dependentVar==classes,1,0)
  
  #predResultsDF <- cbind.data.frame(predictedVals, true_values)
  
  pred <- prediction(predictedVals[,1],true_values)
  perf <- performance(pred, "tpr", "fpr")
  print(plot(perf,main="ROC Curve"))
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
  
}

# testROC <- rocBuilder(train_DustData$CASESTAT, stepwiseModel1$fitted.values, toleranceVec = seq(0,1,0.001))
# testROC[[2]]
# rocDF <- testROC[[1]]
# simpleAUC(rocDF$`True Positive Rate`, rocDF$`False Positive Rate`)



















# -------------------------------------------------------------
# Risk Prediction Builder
# -------------------------------------------------------------
# https://www.publichealth.columbia.edu/research/population-health-methods/risk-prediction#Description
# -------------------------------------------------------------

# Get the proportion of the first two columns passed in
proportionGetter <-  function(x){
  denom <- x[1] + x[2]
  finalProp <- (x[2]/denom)
  return(finalProp)
}

# Get the mean of the first two columns passed in
meanGetter <-  function(x){
  meanVal <- mean(x[1],x[2])
  return(meanVal)
}

# Get the deciles for a vector of predicted values
decileBuilder <- function(predictionVec){
  
  # Builds vector of deciles based on Predicted values 
  decileVec <- as.factor(round(predictionVec, digits = 1))
  
  # Make sure we at least have the factor levels created if they're missing  
  if( !(0 %in% levels(decileVec)) ){
    levels(decileVec) <- c(0, levels(decileVec))
  }
  if( !(0.9 %in% levels(decileVec)) ){
    levels(decileVec) <- c(levels(decileVec), 0.9)
  }
  
  if( !(1 %in% levels(decileVec)) ){
    levels(decileVec) <- c(levels(decileVec), 1)
  }
  
  return(decileVec)
  
}






