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

# Uses K as a Percentage to get folds of data, instead of a K number of folds
# NOTE: can not take a 0% for kPerc
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
    
    # Fit data on the training data and make predictions
    # Pick the model being used
    if(family == "forest"){
      # Build the Forest
      fit <- randomForest(formula, data=train.data, ntree=ntree, mtry=mtry)
      # Aggregate the error
      train.rawPredict <- fit$predicted
      
    }else{
      # Build the model
      fit <- glm(formula, data = train.data, ...)
      
      # Aggregate the error
      train.rawPredict <- fit$fitted.values
    }
    
    holdout.rawPredict <- predict(fit, holdout.data, type="response")
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
      errorTib <- tibble(train.error = train.error, valid.error = holdout.error)
      trainResultsDF <- cbind.data.frame(train.data$ID, train.data$Alpha.ID, train.data[,response],
                                         train.data$train.rawPredict, train.tolPredict)
      colnames(trainResultsDF) <- c("ID", "Alpha.ID", "Response", "train.rawPredict", "train.tolPredict")
      holdoutResultsDF <- cbind.data.frame(holdout.data$ID, holdout.data$Alpha.ID, holdout.data[,response], 
                                           holdout.data$holdout.rawPredict, holdout.tolPredict)
      colnames(holdoutResultsDF) <- c("ID", "Alpha.ID", "Response", "holdout.rawPredict", "holdout.tolPredict")
      return(list(errorTib, trainResultsDF, holdoutResultsDF))
      
    } else{
      # No tolerance to worry about
      train.error <- mean(train.data[,response] != train.rawPredict)
      holdout.error <- mean(holdout.data[,response] != holdout.rawPredict)
      
      # Aggregate Data for records and return results
      errorTib <- tibble(train.error = train.error, valid.error = holdout.error)
      trainResultsDF <- cbind.data.frame(train.data$ID, train.data$Alpha.ID, train.data[,response],
                                         train.data$train.rawPredict)
      colnames(trainResultsDF) <- c("ID", "Alpha.ID", "Response", "train.rawPredict")
      holdoutResultsDF <- cbind.data.frame(holdout.data$ID, holdout.data$Alpha.ID, holdout.data[,response], 
                                           holdout.data$holdout.rawPredict)
      colnames(holdoutResultsDF) <- c("ID", "Alpha.ID", "Response", "holdout.rawPredict")
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
  errors <- tibble()
  trainDF <- data.frame()
  validDF <- data.frame()
  # iterate on the number of folds
  for (i in 1:K) {
    holdout.indices <- which(folds == i, arr.ind = T)
    folded.results <- fold.errors(df, holdout.indices, ...) # Call Helper method to get errors and predictions
    folded.errors <- folded.results[[1]]
    trainDF <- rbind(trainDF, folded.results[[2]])
    validDF <- rbind(validDF, folded.results[[3]])
    errors <- bind_rows(errors, folded.errors)
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

