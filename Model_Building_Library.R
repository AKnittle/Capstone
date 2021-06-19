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
  y <- as.factor(dataset[[dependentVar]])
  
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
# predicted.classes <- ifelse(tempResult$pred.lasso > 0.5, 0, 1)
# mean(predicted.classes == train_DustData$CASESTAT)
# plot(tempResult$lasso.mod, xvar = "lambda")




# -------------------------------------------------------------
# Conditional Decision Trees ----------------------------------
# https://www.youtube.com/watch?v=7VeUPuFGJHk&ab_channel=StatQuestwithJoshStarmer
# -------------------------------------------------------------

# Build a Conditional Decision Tree model
# Mostly a wrapper for ctree of the partykit library
#   dataset = data passed in
#   dependentVar = value we want to predict

buildCDTree <- function(dataset, dependentVar) {
  treeModel <- ctree(as.formula(paste(dependentVar, "~ .")), data = dataset)
  return(treeModel)
}





# -------------------------------------------------------------
# Neural Networks ----------------------------------
# https://srdas.github.io/DLBook/DeepLearningWithR.html
# -------------------------------------------------------------

# Build a Neural Network with deepnet package
#   y is the matrix (1xn) of values trying to be predicted
#   x is the matrix (mxn) of predicting variables used
#   hiddenLayerVec is the vector that describes the number of hidden nodes
#   in each layer (ie: [3] is one hidden layer of 3 nodes, [2,10,3] has 3
#   hidden layers of 2 nodes, 10 nodes, and 3 nodes)
# This really only serves as a wrapping function, but also returns the
# Neural Net and a confusion matrix for quick model evaluation
buildNetwork.deepnet <- function(y, x, hiddenLayerVec){
  # train network
  nNet <-  nn.train(x, y, hidden = hiddenLayerVec)
  predY <- nn.predict(nNet,x) # Make some predictions for confusion matrix
  
  # Make Confusion Matrix
  # NOTE: "Script_Library.R" must be sourced
  cm <- confusionBuilder(y,predY,mean(predY))
  return(list(nNet, cm))
}

# Build a Neural Network with neuralnet package
# Similar usage to other model building methods in this file
# Takes the dataset, what variable you want to predict, and the hiddenLayerVec
# which works the same way as the "buildNetwork.deepnet" function
buildNetwork.neuralnet <- function(dataset, dependentVar, hiddenLayerVec){
  # train network
  nNet <- neuralnet(as.formula(paste(dependentVar, "~ .")),data=dataset,hidden = hiddenLayerVec)
  predY <- nNet$net.result[[1]] # Make some predictions for confusion matrix

  # Make Confusion Matrix
  # NOTE: "Script_Library.R" must be sourced
  cm <- confusionBuilder(dataset[,dependentVar],predY,mean(predY))
  # plot(nNet)
  return(list(nNet, cm))
}

# -------------------------------------------------------------
# Test Runs
# buildNetwork.deepnet
# Single Hidden Layer(s):
# buildNetwork.deepnet(cancerVal, predictMatrix, c(5))[[2]]
# buildNetwork.deepnet(cancerVal, predictMatrix, c(10))[[2]]
# buildNetwork.deepnet(cancerVal, predictMatrix, c(100))[[2]]
# # =============================================================
# # =============================================================
# # Two Hidden Layer(s):
# buildNetwork.deepnet(cancerVal, predictMatrix, c(5, 5))[[2]]
# buildNetwork.deepnet(cancerVal, predictMatrix, c(10, 10))[[2]]
# buildNetwork.deepnet(cancerVal, predictMatrix, c(100, 100))[[2]]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# buildNetwork.neuralnet
# Single Hidden Layer(s):
buildNetwork.neuralnet(logDustData, "CASESTAT", c(5))[[2]]
buildNetwork.neuralnet(logDustData, "CASESTAT", c(10))[[2]]
buildNetwork.neuralnet(logDustData, "CASESTAT", c(100))[[2]]
# =============================================================
# =============================================================
# Two Hidden Layer(s):
buildNetwork.neuralnet(logDustData, "CASESTAT", c(5, 5))[[2]]
buildNetwork.neuralnet(logDustData, "CASESTAT", c(10, 10))[[2]]
buildNetwork.neuralnet(logDustData, "CASESTAT", c(100, 100))[[2]]








