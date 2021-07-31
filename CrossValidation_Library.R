# Cross Validation:

# ---------------------------------------------------------------------
# The Following methods, k.fold.cross.validator and run.X.KFolds.TV.Error, 
# were methods written in the summer of 2020. I have moved them to this file
# for convenience 

# Pass in the dataframe, the formula, the model family, and the K number of folds
# NOTE: Passing in a Binomial Model gives meaningless results
k.fold.cross.validator <- function(df, model.formula, model.family, K) {
  
  # this function calculates the MSE of a single fold using the fold as the holdout data
  fold.mses <- function(df, model.formula, holdout.indices) {
    train.data <- df[-holdout.indices, ]
    holdout.data <- df[holdout.indices, ]
    fit <- glm(model.formula, data = train.data, family = model.family)
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
run.X.KFolds.TV.Error <- function(x, df, model, model.family, k){
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


# NOTE: can not take a 0% for kPerc
# Uses K as a Percentage to get folds of data, instead of a K number of folds
# This thing is a beast but here's what you need to know:
# If you want to use this with glm pass in the family appropriate for it
# otherwise for something like "random forest" pass in the family value "forest"
crossValidator.byKPercent <- function(df, response, formula, kPerc=0.10, tol=NULL, ...) {
  
  
  # ---------------------------------------------------------------------------------------
  # Inner Helper Method:
  # this function calculates the errors of a single fold using the fold as the holdout data
  fold.errors <- function(df, holdout.indices, ...) {
    
    
    # To make this more agnostic of modeling methods we need to see what
    # kinds of models we have to break up things here. Get the name of 
    # "..." and their values
    args <- list(...)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
    
    # Break up the data
    train.data <- df[-holdout.indices, ]
    holdout.data <- df[holdout.indices, ]
    
    # Initialize values for later usage
    fit <- NULL
    train.rawPredict <- NULL
    holdout.rawPredict <- NULL
    
    # Fit data on the training data and make predictions
    # Pick the model being used
    if(family == "forest"){
      # Build the Forest
      fit <- randomForest(formula, data=train.data, ntree=ntree, mtry=mtry)
      # Aggregate the error
      train.rawPredict <- fit$predicted
      holdout.rawPredict <- predict(fit, holdout.data, type="response")
      
    }else if(family == "lda"){
      # Build the LDA
      fit <- lda(lda.formula, data = train.data)
      # Aggregate the error
      train.rawPredict <- as.numeric(as.character(predict(fit, train.data)$class))
      holdout.rawPredict <- as.numeric(as.character(predict(fit, holdout.data)$class))
      
    }else if(family == "lasso"){
      # Build the Lasso with passed in best Lambda
      x <- model.matrix(formula, train.data)[,-1]
      y <- as.factor(train.data[[response]])
      
      fit <- glmnet(x,y, alpha=1,lambda=bestLambda, family="binomial", thresh = 1e-12)
      # Aggregate the error
      train.rawPredict <- predict(fit, s = bestLambda, newx = x, type="response")
      holdout.rawPredict <- predict(fit, s = bestLambda, newx = model.matrix(formula, holdout.data)[,-1], type="response")
      
    }else{
      # Build the model (Logistic Regression and others that use glm)
      fit <- glm(formula, data = train.data, ...)
      # Aggregate the error
      train.rawPredict <- fit$fitted.values
      holdout.rawPredict <- predict(fit, holdout.data, type="response")
    }
    
    train.data$train.rawPredict <- train.rawPredict
    holdout.data$holdout.rawPredict <- holdout.rawPredict
    
    # See if we need to consider tolerance
    if(!is.null(tol)){
      # See if by the this tolerance whether or not it was true; (value > tol) = TRUE
      train.tolPredict <- as.numeric(train.rawPredict > tol)
      holdout.tolPredict <- as.numeric(holdout.rawPredict > tol)
      train.error <- mean(train.data[,response] != train.tolPredict)
      holdout.error <- mean(holdout.data[,response] != holdout.tolPredict)
      
      # Aggregate Data for records and return results
      # Error Rate
      errorTib <- tibble(train.error = train.error, valid.error = holdout.error)
      
      # Training Set
      trainResultsDF <- cbind.data.frame(train.data$ID, train.data$Alpha.ID, train.data[,response],
                                         train.data$train.rawPredict, train.tolPredict)
      colnames(trainResultsDF) <- c("ID", "Alpha.ID", "Response", "train.rawPredict", "train.tolPredict")
      
      # Validation/Holdout Set
      holdoutResultsDF <- cbind.data.frame(holdout.data$ID, holdout.data$Alpha.ID, holdout.data[,response], 
                                           holdout.data$holdout.rawPredict, holdout.tolPredict)
      colnames(holdoutResultsDF) <- c("ID", "Alpha.ID", "Response", "holdout.rawPredict", "holdout.tolPredict")
      
      # Done with this Fold...
      return(list(errorTib, trainResultsDF, holdoutResultsDF))
      
    } else{
      # No tolerance to worry about
      train.error <- mean(train.data[,response] != train.rawPredict)
      holdout.error <- mean(holdout.data[,response] != holdout.rawPredict)
      
      # Aggregate Data for records and return results
      # Error Rate
      errorTib <- tibble(train.error = train.error, valid.error = holdout.error)
      
      # Training Set
      trainResultsDF <- cbind.data.frame(train.data$ID, train.data$Alpha.ID, train.data[,response],
                                         train.data$train.rawPredict)
      colnames(trainResultsDF) <- c("ID", "Alpha.ID", "Response", "train.rawPredict")
      
      # Validation/Holdout Set
      holdoutResultsDF <- cbind.data.frame(holdout.data$ID, holdout.data$Alpha.ID, holdout.data[,response], 
                                           holdout.data$holdout.rawPredict)
      colnames(holdoutResultsDF) <- c("ID", "Alpha.ID", "Response", "holdout.rawPredict")
      
      # Done with this Fold...
      return(list(errorTib, trainResultsDF, holdoutResultsDF))
    }
    
  }
  # ---------------------------------------------------------------------------------------
  
  # Make a K from the percentage of rows
  K <- floor(nrow(df)*kPerc)
  # print(K)
  
  # Make index values of rows to keep track
  #   ID: for cross validation
  #   Alpha.ID: for original row order
  ID <- sample(1:nrow(df))
  Alpha.ID <- 1:nrow(df)
  df$ID <- ID
  df$Alpha.ID <- Alpha.ID
  
  # Prepare the folds being used
  folds <- cut(ID, breaks = K, labels = F)
  
  # Initialize error to begin accumulation of fold error rates
  errors <-  data.frame()
  trainDF <- data.frame()
  validDF <- data.frame()
  # iterate on the number of folds
  for (i in 1:K) {
    holdout.indices <- which(folds == i, arr.ind = T)
    folded.results <- fold.errors(df, holdout.indices, ...) # Call Helper method to get errors and predictions
    folded.errors <- as.data.frame(folded.results[[1]])
    trainDF <- rbind(trainDF, folded.results[[2]])
    validDF <- rbind(validDF, folded.results[[3]])
    errors <-  rbind(errors, folded.errors)
  }
  # Return results and let god sort them out
  return(list(errors, df, trainDF, validDF))
}

# ---------------------------------------------------------------------
cost.fn <- function(obs.response, fitted.probability) {
  tol=0.5
  t <- tibble(obs = obs.response, prob = fitted.probability)
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
  
  cvResult <- cv.glm(trainSet, glmfit = model.fit, cost = cost.fn, K = givenK)$delta[1]
  return(cvResult)
}




# ---------------------------------------------------------------------
# Cross Validation Result Methods:
# Helper methods that help plot/organize results

# Pass in the Error DF you get back from running "crossValidator.byKPercent" (The first return value)
crossError <- function(errorsDF){
  
  # Clean up Data
  errorsDF$numFolds <- 1:nrow(errorsDF)
  colnames(errorsDF) <- c("Training Error", "Validation Error", "numFolds")
  
  # Get average errors for this cross validation
  avg.TrainCross <- mean(errorsDF$`Training Error`)
  avg.ValidCross <- mean(errorsDF$`Validation Error`)
  avg.ErrorDF <- cbind.data.frame(avg.TrainCross, avg.ValidCross)
  colnames(avg.ErrorDF) <- c("Avg. Training Error", "Avg. Validation Error")
  colnames(errorsDF) <- c("Avg. Training Error", "Avg. Validation Error")
  
  # Prep for plotting
  errorsDF$numFolds <- 1:nrow(errorsDF)
  errorsDF <- reshape2::melt(errorsDF, id.vars = c("numFolds"))
  colnames(errorsDF) <- c("numFolds", "Error.Type", "Avg.Error")
  okRows <- which(errorsDF$Error.Type == "Avg. Training Error" | errorsDF$Error.Type == "Avg. Validation Error")
  errorsDF <- errorsDF[okRows,]
  
  # Make Plots
  errorLinePlot <- ggplot(errorsDF) +
    aes(x = numFolds, y = Avg.Error, colour = Error.Type) +
    geom_line(size = 0.8) + scale_color_hue(direction = 1) +
    labs(x = "Number of Folds", y = "Error Rate",title = "Error Rate per Fold") +
    theme_bw() + ylim(0L, 1L)
  
  errorBoxPlot <- ggplot(errorsDF) +
    aes(x = "", y = Avg.Error, fill = Error.Type) +
    geom_boxplot(shape = "circle") + scale_fill_hue(direction = 1) +
    labs(x = "Error Types", y = "Error Rate", title = "Error Rate per Fold") +
    theme_bw() + ylim(0L, 1L)
  # print(errorsDF)
  
  errorPlots <- ggarrange(errorLinePlot, errorBoxPlot, nrow = 2)
  
  return(list(avg.ErrorDF, errorPlots))
}











