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

