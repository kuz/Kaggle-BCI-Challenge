#
# Overfitter configuration for Boosted Tree
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
mlmethod <- 'bstTree'

# list of parameters
parameters <- list()
parameters[['mstop']] <- c(20, 50, 100, 400)
parameters[['maxdepth']] <- c(1, 2, 5, 10, 20)
parameters[['nu']] <- c(0.001, 0.1, 1, 2, 100)

# Wrapper for training a classifier
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  tunegrid <- data.frame(mstop=p$mstop, maxdepth=p$maxdepth, nu=p$nu)
  trcontrol <- trainControl(method='none')
  classifier <- train(class ~., data=trainingset, 'bstTree', trControl=trcontrol, tuneGrid=tunegrid)
  return(classifier)
}

# Wrapper for predicting using trained model
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- predict(classifier, newdata=validset, type='prob')$positive
  return(predicted)
}