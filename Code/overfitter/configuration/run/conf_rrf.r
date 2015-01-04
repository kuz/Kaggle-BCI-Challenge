#
# Overfitter configuration for Regularized Random Forest (RRF)
#

# packages to use during learning (loaded into each cluster node)
packages=c('pROC', 'RRF')

library('foreach')
library('doParallel')
library('parallel')
source('../functions.r')
for (pkg in packages) {
  library(pkg, character.only=T)
}

# name of the learning method
mlmethod <- 'rrf'

# list of parameters
parameters <- list()
parameters[['ntree']] <- c(100, 500, 700)
parameters[['mtry']] <- c(5, 50, 150)
parameters[['nodesize']] <- c(1, 5, 10, 100)
parameters[['coefReg']] <- c(0.25, 0.5, 0.75)

# Wrapper for training a classifier
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  classifier <- RRF(class ~ ., data=trainingset, ntree=p$ntree, mtry=p$mtry, nodesize=p$nodesize,
                    coefReg=p$coefReg, do.trace=T)
  return(classifier)
}

# Wrapper for predicting using trained model
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- predict(classifier, newdata=validset, type='prob')[,2]
  return(predicted)
}