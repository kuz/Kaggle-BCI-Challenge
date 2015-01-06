#
# Overfitter configuration for Bayesian Generalized Linear Models (BayesGLM)
#

# packages to use during learning (loaded into each cluster node)
packages=c('pROC', 'caret')

library('foreach')
library('doParallel')
library('parallel')
source('../functions.r')
for (pkg in packages) {
  library(pkg, character.only=T)
}

# name of the learning method
mlmethod <- 'bayesglm'

# list of parameters
parameters <- list()
parameters[['n.iter']] <- c(10, 100, 300)

# Wrapper for training a classifier
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  trcontrol <- trainControl(method='none')
  classifier <- train(class ~., data=trainingset, 'bayesglm', trControl=trcontrol, n.iter=p$n.iter)
  return(classifier)
}

# Wrapper for predicting using trained model
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- predict(classifier, newdata=validset, type='prob')$positive
  return(predicted)
}