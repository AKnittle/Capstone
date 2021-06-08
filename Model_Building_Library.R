# Library File for building specific models

# -------------------------------------------------------------
# Lasso -------------------------------------------------------
# http://www.sthda.com/english/articles/36-classification-methods-essentials/149-penalized-logistic-regression-essentials-in-r-ridge-lasso-and-elastic-net/
# -------------------------------------------------------------


# Build a model using Lasso Regression.
#   dataset = data passed in
#   dependentVar = value we want to predict
#   lambdaVals = a Vector of potential lambda values to help fit the model
#   binaryResp = Is the response variable continuous or not (default True)
#   TODO: Implement Continuous option
buildLasso <- function(dataset, dependentVar, lamVals, binaryResp=TRUE){
  
  # Build a model matrix of the model
  x <- model.matrix(as.formula(paste(dependentVar, "~ .")), dataset)[,-1]
  y <- dataset[[dependentVar]]
  
  # glmnet fits lasso regression for each value of lambda provided
  # note: if alpha=0, the glmnet fits ridge regression; alpha=1 fits lasso
  # note: glmnet will standardize each column of x so that it ranges between 0 and 1
  # note: glmnet requires x and y, not a formula
  lasso.mod=glmnet(x,y,alpha=1,lambda=lamVals, family="binomial", thresh = 1e-12)
  cv.out=cv.glmnet(x,y,alpha=1, family="binomial", thresh = 1e-12)
  # This is the best lambda value found
  bestLambda=cv.out$lambda.min
  print(bestLambda)
  
  # Calculate the Train error with the ridge regression model with the best lambda value
  pred.lasso <- predict(lasso.mod, s = bestLambda, newx = x)
  lasso.train.error <- mean((pred.lasso - dataset[[dependentVar]])^2)
  
  
  # # Plot MSE
  # msePlot <- plot(cv.out)
  # 
  # # Coefficient vs. Lambda
  # coeLamPlot <- matplot(lamVals, t(lasso.mod$beta), type='b',log='x',xlab='lambda',
  #         ylab='coefs',cex=.7)
  
  # Build final results of model
  modelResult <- list(lasso.mod, cv.out, bestLambda, pred.lasso, lasso.train.error)
  names(modelResult) <- c("lasso.mod", "cv.out", "bestLambda", "pred.lasso", "lasso.train.error")
  
  
  # print(coef(lasso.mod,s=bestLambda))
  
  return(modelResult)
}

# Test Lasso Builder
# lambda <- 10^seq(10,-2,length=30)
# tempResult <- buildLasso(train_DustData, "CASESTAT", lambda)
# tempResult$bestLambda
# coef(tempResult$lasso.mod,s=tempResult$bestLambda)



