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

# ---------------------------------------------------------------------

k.fold.cross.validator <- function(df, model.formula, K) {
  
  # this function calculates the MSE of a single fold using the fold as the holdout data
  fold.mses <- function(df, model.formula, holdout.indices) {
    train.data <- df[-holdout.indices, ]
    holdout.data <- df[holdout.indices, ]
    fit <- glm(model.formula, data = train.data)
    tibble(train.mse = mse(fit, train.data), valid.mse = mse(fit, holdout.data))
  }
  
  # shuffle the data and create the folds
  indices <- sample(1:nrow(df))
  # if argument K == 1 we want to do LOOCV
  if (K == 1) {
    K <- nrow(df)
  }
  folds <- cut(indices, breaks = K, labels = F)
  # convert "model.formula" from, character to a formula
  model.formula <- formula(model.formula)
  # set error to 0 to begin accumulation of fold MSEs
  mses <- tibble()
  # iterate on the number of folds
  for (i in 1:K) {
    holdout.indices <- which(folds == i, arr.ind = T)
    folded.mse <- fold.mses(df, model.formula, holdout.indices)
    mses <- mses %>%
      bind_rows(folded.mse)
  }
  mses %>%
    summarize(train.mse = mean(train.mse), valid.mse = mean(valid.mse))
  return(mses)
}

# Run k folds x number of times with a model based on a dataframe (df)
# Return DF of each Validation and Training error of a set of k-folds 
run.X.KFolds.TV.Error <- function(x, df, model, k){
  valid.errors <- c()
  train.errors <- c()
  i <- 0
  while(i < x){
    # Get back DF of K-Fold
    error.vals <- k.fold.cross.validator(df, model, k)
    # Training MSE
    mean.error.val.Train <- mean(error.vals$train.mse)
    train.errors <- c(train.errors, mean.error.val.Train)
    # Validation MSE
    mean.error.val.Valid <- mean(error.vals$valid.mse)
    valid.errors <- c(valid.errors, mean.error.val.Valid)
    i <- i + 1
  }
  total.errors <- cbind(train.errors, valid.errors)
  return(total.errors)
}



# ----------------------------------------------

# Cross Validation just for LDA
# TODO: This is likely depricated code. Refactor and use above methods
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
  
  # true positive rate
  True.Positive.Rate <- n.tp/(n.tp+n.fn)
  # true positive rate
  True.Negative.Rate <- n.tn/(n.tn+n.fp)
  
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


# testROC <- rocBuilder(train_DustData$CASESTAT, stepwiseModel1$fitted.values, toleranceVec = seq(0,1,0.001))
# testROC[[2]]
# rocDF <- testROC[[1]]
# simpleAUC(rocDF$`True Positive Rate`, rocDF$`False Positive Rate`)




