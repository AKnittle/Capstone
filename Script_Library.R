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
  confusionDF <- data.frame(Observed.NO, Observed.YES, row.names = c("Predicted NO", "Predicted YES"))
  colnames(confusionDF) <- c("Observed NO", "Observed YES")
  
  # error rate
  Error.Rate <- (n.fp + n.fn)/nrow(t)
  
  # false positive rate (Fall Out)
  False.Positive.Rate <- n.fp/(n.tn+n.fp) # Ideal is close to 0
  # false negative rate (Miss Rate)
  False.Negative.Rate <- n.fn/(n.fn+n.tp) # Ideal is close to 0
  
  # true positive rate (Sensitivity)
  True.Positive.Rate <- n.tp/(n.tp+n.fn) # Ideal is close to 1
  # true negative rate (Specificity)
  True.Negative.Rate <- n.tn/(n.tn+n.fp) # Ideal is close to 1
  
  RatesDF <- data.frame(Error.Rate, False.Positive.Rate, False.Negative.Rate, True.Positive.Rate, True.Negative.Rate)
  colnames(RatesDF) <- c("Error Rate", "False Positive Rate (Fall-Out)", "False Negative Rate (Miss Rate)", 
                         "True Positive Rate (Sensitivity)", "True Negative Rate (Specificity)")
  return(list(confusionDF, RatesDF))
}

# IMPORTANT: Used for returned classified values, NOT probabilities
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
  confusionDF <- data.frame(Observed.NO, Observed.YES, row.names = c("Predicted NO", "Predicted YES"))
  colnames(confusionDF) <- c("Observed NO", "Observed YES")
  
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
  colnames(RatesDF) <- c("Error Rate", "False Positive Rate (Fall-Out)", "False Negative Rate (Miss Rate)", 
                         "True Positive Rate (Sensitivity)", "True Negative Rate (Specificity)")
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
rocByLibrary <- function(model, data, dependentVar, ...){
  
  predictedVals <- predict(model,data, ...)
  classes <- levels(dependentVar)
  true_values <- ifelse(dependentVar==classes,1,0)
  
  # To make this more agnostic of modeling methods we need to see what
  # kinds of models we have to break up things here. Get the name of 
  # "..." and their values
  args <- list(...)
  for(i in 1:length(args)) {
    assign(x = names(args)[i], value = args[[i]])
  }
  if(!is.null(type) & type == "response"){
    roc(dependentVar ~ predictedVals, plot = TRUE, print.auc = TRUE)
    return()
  }
  
  #predResultsDF <- cbind.data.frame(predictedVals, true_values)
  pred <- prediction(predictedVals[,1],true_values)
  perf <- performance(pred, "tpr", "fpr")
  print(plot(perf,main="ROC Curve"))
  auc.perf <- performance(pred, measure = "auc")
  aucVal <- auc.perf@y.values
  return(aucVal)
  
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

# Pass in Validation DF returned from Cross Validation
plotDeciles <- function(df){
  # Get probabilities
  df$decilePredictedVals <- decileBuilder(df$holdout.rawPredict)
  
  # Make deciles
  decileBins <- seq(0,1, 0.1)
  decileProp <- data.frame()
  # Make propoortions for each decile bin
  for(i in decileBins){
    tempSet <- subset(df, decilePredictedVals==i)
    countPerDec <- table(tempSet$Response)
    tempDF <- cbind.data.frame(countPerDec[[1]], countPerDec[[2]], i)
    decileProp <- rbind(decileProp, tempDF)
  }
  colnames(decileProp) <- c("0", "1", "Decile")
  
  
  props <- apply(decileProp, 1, proportionGetter)
  decileProp$props <- props
  
  # Plot results
  decilePlot <- ggplot(decileProp) +
    geom_abline(slope = 1, linetype = 2, color="red") +
    aes(x = Decile, y = props, colour = props) + geom_line(size = 0.8) + scale_color_viridis_c(option = "viridis", direction = 1) +
    labs(x = "Deciles of Predicted Value", y = "Proportion", title = "Decile Plot") +
    theme_bw() + ylim(0, 1) + xlim(0,1)
  # Add horizontal lines to mark bin ends
  for(h in decileBins){
    decilePlot <- decilePlot + geom_vline(xintercept = h, linetype="dotted")
  }
  
  return(decilePlot)
  
}

# Builds Decile plots made of bins from quantiles
# Pass in Probabilities corresponding to their observed values 
decilesByQuantiles <- function(predictedVals, observedVals){
  # Make sure data passed in is numeric
  observedVals <- as.numeric(as.character(observedVals))
  # Get quantile bins
  quantBins <- quantile(predictedVals, probs = seq(.1, .9, by = .1))
  zoomA <- quantBins[1]
  zoomB <- quantBins[9]
  quantBins <- c(0,quantBins,1)
  # aggregate values that fell into bins
  resultDF <- cbind.data.frame(predictedVals, observedVals)
  colnames(resultDF) <- c("predicted", "observed")
  decileDF <- data.frame()
  # Loop through the bins and build the decileDF
  alpha=1
  while(alpha < length(quantBins)){
    # Get the end of the bin
    beta = alpha + 1
    # Get stats of what's between both ends of the bin
    tempDF <- resultDF %>% filter(predicted > quantBins[alpha] & predicted < quantBins[beta])
    proportion <- sum(tempDF$observed)/nrow(tempDF)
    num0 <- nrow(tempDF) - sum(tempDF$observed)
    num1 <- sum(tempDF$observed)
    # Compile results of current bin
    binDF <- cbind.data.frame(quantBins[alpha], quantBins[beta], num0, num1, proportion)
    decileDF <- rbind(binDF, decileDF)
    # Move onto the next index
    alpha = beta
  }
  colnames(decileDF) <- c("Start", "End", "0", "1", "Proportion")
  
  # Build graph of Deciles 
  decilePlot <- ggplot(decileDF) +
    geom_abline(slope = 1, linetype = 2, color="red") + aes(x = Start, y = Proportion, colour = Proportion) +
    geom_line(size = 0.8) + scale_color_viridis_c(option = "viridis", direction = 1) +
    labs(x = "Deciles of Predicted Value", y = "Proportion", title = "Decile Plot by Quantiles") +
    theme_bw() + ylim(0, 1) + xlim(0,1)
  # Add horizontal lines to mark bin ends
  for(h in decileDF$End){
    decilePlot <- decilePlot + geom_vline(xintercept = h, linetype="dotted")
  }
  
  # Build graph of Deciles (Zoomed In version)
  decilePlotZoom <- ggplot(decileDF) +
    geom_abline(slope = 1, linetype = 2, color="red") + aes(x = Start, y = Proportion, colour = Proportion) +
    geom_line(size = 0.8) + scale_color_viridis_c(option = "viridis", direction = 1) +
    labs(x = "Deciles of Predicted Value", y = "Proportion", title = "Decile Plot by Quantiles (Zoomed)") +
    theme_bw() + ylim(0, 1) + xlim(zoomA, zoomB)
  # Add horizontal lines to mark bin ends
  for(h in decileDF$End){
    decilePlotZoom <- decilePlotZoom + geom_vline(xintercept = h, linetype="dotted")
  }
  
  
  return(list(decileDF, decilePlot, decilePlotZoom))
}

# testObs <- sample(c(0,1), 100, replace = TRUE)
# testProb <-round(runif(100),4)
# testDeciles <- decilesByQuantiles(testProb, testObs)
# testDeciles


# Formatting Methods---------------

# Basically a quick wrapper for DFs 
quickDFPrint <- function(df){
  df %>%
  kbl() %>%
    kable_paper("hover", full_width = F)
}


