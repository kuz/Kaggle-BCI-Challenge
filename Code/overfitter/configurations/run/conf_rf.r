#
# Overfitter configuration for Regularized Random Forest (RRF)
#

# packages to use during learning (loaded into each cluster node)
packages=c('pROC', 'randomForest')

library('foreach')
library('doParallel')
library('parallel')
source('../functions.r')
for (pkg in packages) {
  library(pkg, character.only=T)
}

# name of the learning method
mlmethod <- 'rf'

# list of parameters
parameters <- list()
parameters[['ntree']] <- c(50, 200, 500)
parameters[['mtry']] <- c(10, 50, 100, 500)
parameters[['nodesize']] <- c(1, 10, 100)

# Wrapper for training a classifier
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  classifier <- randomForest(class ~ ., data=trainingset, ntree=p$ntree, mtry=p$mtry, nodesize=p$nodesize, do.trace=T)
  return(classifier)
}

# Wrapper for predicting using trained model
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- predict(classifier, newdata=validset, type='prob')[,2]
  return(predicted)
}